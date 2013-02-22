
use master
SET QUOTED_IDENTIFIER ON
GO
IF EXISTS (SELECT 1 FROM sys.objects WHERE name LIKE 'sp_ScriptFor')
       DROP PROCEDURE sp_ScriptFor
go
CREATE PROCEDURE [dbo].[sp_ScriptFor]
@Identifier NVARCHAR(776)
/**
summary:   >
This procedure returns an object buld script as a single-row, single column
result.
Unlike the built-in OBJECT_DEFINITION, it also does tables.
It copies the SMO style where possible but it uses the more intuitive
eay of representing referential constrants and includes the documentation
as comments that was, for unknown reasons, left out by microsoft.
You call it with the name of the table, either as a string, a valid table name,
or as a schema-qualified table name in a string.
Author: Phil Factor
Revision: 1.1 dealt properly with heaps
date: 20 Apr 2010
Revision: 1.2 Removed several bugs and got column-level constraints working
date: 1 Dec 2012
Revision: 1.3 Added extended properties build
date: 3 Dec 2012
example:
     - code: sp_ScriptFor 'production.product'
example:
     - code: sp_ScriptFor 'Person.businessEntityAddress'
example:
     - code: sp_Scriptfor 'person.person'
     Select * from person.person
example:
     - code: execute sp_ScriptFor TransactionHistory
example:
     - code: sp_ScriptFor 'HumanResources.uspUpdateEmployeeHireInfo'
returns:   >
single row, single column result Build_Script.
**/
 
AS
DECLARE @Script VARCHAR(MAX)
DECLARE @dbname SYSNAME --the database's name
DECLARE @PrimaryKeyAndUniqueConstraintsBuild VARCHAR(MAX)
DECLARE @IndexBuild VARCHAR(MAX)
DECLARE @Filegroup SYSNAME --of the table
DECLARE @TableObjectID int
Declare @Collation sysname --the default collation of the object
Declare @SchemaName sysname
Declare @TableName sysname


IF CHARINDEX ('.',@identifier)=0 --Add schema if none given
       SELECT top 1 @Identifier=QUOTENAME(Object_Schema_name(s.object_id))
                 +'.'+QUOTENAME(s.name)
       FROM sys.objects s WHERE s.name LIKE @identifier
 
SELECT @dbname = PARSENAME(@identifier,3)
       IF @dbname IS NULL
              SELECT @dbname = DB_NAME()
       ELSE IF @dbname <> DB_NAME()
              BEGIN
                     RAISERROR(15250,-1,-1)
                     RETURN(1)
              END
SELECT @Collation= convert(sysname,DATABASEPROPERTYEX(@dbname, 'Collation')),
       @Script=object_definition(OBJECT_ID(@Identifier))
IF @script IS NULL
  IF (SELECT TYPE FROM sys.objects
        WHERE object_id=OBJECT_ID(@Identifier))
          IN ('U','S')--if it is a table
              BEGIN
              Select top 1 @Filegroup=d.name 
			 FROM sys.filegroups d
			 INNER JOIN sys.indexes i
			   ON i.data_space_id = d.data_space_id
			WHERE i.object_id=OBJECT_ID(@Identifier)
			 AND i.index_id<2
			 Select 
			     @TableObjectID=object_id,
                 @TableName=name,
                 @SchemaName=object_schema_name(object_ID)
			from sys.tables
			WHERE sys.tables.object_id=OBJECT_ID(@Identifier)


			 
			 
              SELECT @Script='/*'+CONVERT(VARCHAR(2000),value)+'*/
'       FROM  sys.extended_properties ep
                     WHERE ep.major_ID = @TableObjectID
                     AND  minor_ID=0 AND class=1
 
SELECT @Script=COALESCE(@Script,'')+'CREATE TABLE '+quotename(@SchemaName)+'.'+Quotename(@TableName)+' (
   ' +
(SELECT QUOTENAME(c.name)+ ' '+   coalesce(UDTs.UDTName,t.name)+' '
       + CASE WHEN is_computed=1 THEN ' AS '+ --do DDL for a computed column
                     (SELECT definition FROM sys.computed_columns cc
                      WHERE cc.object_id=c.object_id AND cc.column_ID=c.column_ID)
             + CASE WHEN
                     (SELECT is_persisted 
                        FROM sys.computed_columns cc
                        WHERE cc.object_id=c.object_id
                        AND cc.column_ID=c.column_ID)
                     =1 THEN 'PERSISTED' ELSE '' END
              --we may have to put in the length         
              WHEN t.name IN ('char', 'varchar','nchar','nvarchar') THEN '('+
                 CASE WHEN c.max_length=-1 THEN 'MAX'
                      ELSE CONVERT(VARCHAR(4),
                                   CASE WHEN t.name IN ('nchar','nvarchar')
                                   THEN  c.max_length/2 ELSE c.max_length END )
                      END +')'
              WHEN t.name IN ('decimal','numeric')
                      THEN '('+ CONVERT(VARCHAR(4),c.precision)+','
                              + CONVERT(VARCHAR(4),c.Scale)+')'
                      ELSE '' END
              + CASE WHEN is_identity=1  THEN 'IDENTITY ('
                     + CONVERT(VARCHAR(8),IDENT_SEED(Object_Schema_Name(c.object_id)
                     +'.'+OBJECT_NAME(c.object_id)))+','
                     + CONVERT(VARCHAR(8),IDENT_INCR(Object_Schema_Name(c.object_id)
                     +'.'+OBJECT_NAME(c.object_id)))+')' ELSE '' END
              + CASE WHEN c.is_rowguidcol=1 THEN ' ROWGUIDCOL' ELSE '' END
              + CASE WHEN XML_collection_ID<>0 THEN --deal with object schema names
                                  '('+ CASE WHEN is_XML_Document=1
                                              THEN 'DOCUMENT ' ELSE 'CONTENT ' END
                     + COALESCE(
                         (SELECT QUOTENAME(ss.name)+'.' +QUOTENAME(sc.name)
                          FROM sys.xml_schema_collections sc
                          INNER JOIN  Sys.Schemas ss
                              ON sc.schema_ID=ss.schema_ID
                          WHERE sc.xml_collection_ID=c.XML_collection_ID)
                       ,'NULL')
                     +')' ELSE '' END
              + CASE WHEN  is_identity=1
                  THEN CASE WHEN OBJECTPROPERTY(object_id, 'IsUserTable') = 1
                            AND COLUMNPROPERTY(object_id, c.name, 'IsIDNotForRepl') = 0
                   AND OBJECTPROPERTY(object_id, 'IsMSShipped') = 0
                THEN '' ELSE ' NOT FOR REPLICATION ' END ELSE '' END
              + CASE WHEN c.collation_name IS NULL THEN ''
                 WHEN  c.collation_name<>
                          @Collation  COLLATE Latin1_General_CI_AS
                 THEN COALESCE(' COLLATE '+c.collation_name,'') ELSE '' END
              + CASE WHEN c.is_nullable=0 THEN ' NOT NULL' ELSE ' NULL' END
              + CASE WHEN c.default_object_id <>0
                 THEN ' DEFAULT '+object_Definition(c.default_object_id) ELSE '' END
              +coalesce(TheCheck,'')+  coalesce(reference,'') +'|,|'  
              + CASE WHEN ep.value IS NOT NULL
                 THEN ' /*'+CAST(value AS VARCHAR(100))+ '*/' ELSE '' END
              + CHAR(10)+'   '
              FROM sys.columns c INNER JOIN sys.types t
                     ON c.user_Type_ID=t.user_Type_ID
              LEFT OUTER JOIN sys.extended_properties ep --join to the comments
                    ON c.object_id = ep.major_ID 
                         AND c.column_ID = minor_ID AND class=1 
 			  left Outer join --get the user-defined types to show their schema
			    (Select column_ID,quotename(sys.schemas.name)+'.'+QuoteName(sys.types.name) as UDTname 
                   from sys.columns inner join sys.types
                   on sys.types.user_type_id = sys.columns.user_type_id
                   inner join sys.schemas
                   on sys.schemas.schema_id=sys.types.schema_id
                   where object_ID=@TableObjectID 
                   and sys.columns.user_type_id <> sys.columns.system_type_id
                  ) UDTs
                on UDTs.column_ID=c.column_ID 
              inner join (SELECT cr.column_ID,(select case when is_system_named=0 then ' CONSTRAINT '+quotename(name) else '' end+' CHECK '+definition 
					    FROM sys.check_constraints ccr
						where cr.column_ID = ccr.parent_column_id
						and ccr.parent_object_ID = cr.Object_ID
					   FOR XML PATH(''), TYPE).value('.', 'varchar(max)') Thecheck
					FROM sys.columns cr 
					where cr.Object_ID= @TableObjectID) TheChecks
           on theChecks.column_ID=c.column_ID
              LEFT OUTER JOIN
              (SELECT Case when is_system_named=0 then ' CONSTRAINT '+ quotename(fk.name)+' ' else '' end+' REFERENCES '
           +COALESCE(SCHEMA_NAME(foreignRef.schema_ID)+'.','')
           +OBJECT_NAME(fkc.referenced_object_id)+'('+c.name+') '--+
              + CASE WHEN delete_referential_action_desc <> 'NO_ACTION'
                                THEN 'ON DELETE '
                                   + REPLACE(delete_referential_action_desc,'_',' ')
                                                            COLLATE database_default
                                ELSE '' END
                       + CASE WHEN update_referential_action_desc <> 'NO_ACTION'
                                THEN 'ON UPDATE '
                                   + REPLACE(update_referential_action_desc,'_',' ')
                                                            COLLATE database_default
                                ELSE '' END
                       AS reference, parent_column_id
                     FROM sys.foreign_key_columns fkc 
                     INNER JOIN sys.foreign_keys fk ON constraint_object_id=fk.object_ID
                     INNER JOIN sys.objects ForeignRef ON fkc.referenced_object_id=ForeignRef.Object_ID
                     INNER JOIN sys.columns c
                     ON c.object_ID = fkc.referenced_object_id
                         AND c.column_ID = referenced_column_id
                      WHERE fk.parent_object_ID = @TableObjectID
                     AND constraint_object_ID NOT IN --include only single-column keys
                    (SELECT 1 FROM sys.foreign_key_columns multicolumn
                               WHERE multicolumn.parent_object_id =fk.parent_object_ID
                               GROUP BY constraint_object_id
                               HAVING COUNT(*)>1)) column_references
           ON  column_references.parent_column_ID=c.column_ID
        WHERE object_id = @TableObjectID
        ORDER BY c.column_ID
              FOR XML PATH(''), TYPE).value('.', 'varchar(max)')--join up all the rows!
        if (@Script is null) raiserror ('script is null',16,1)    
              SELECT @Script=LEFT(@Script,LEN(@Script)-1)
                           --take out the trailing line feed
                  
/* now we will do the indexes, unique constraints and primary key */             
		Declare @indexes table(
		name sysname, is_unique int, index_ID int primary key, [using] Varchar(255), 
		Index_type varchar(255), columns varchar(255), on_clause Varchar(255)
		)
		Insert into @indexes (name, is_unique, index_ID, [using], Index_type, columns, on_clause)
		Select 
		  i.name, i.is_unique, i.index_ID,
		  'USING XML INDEX ['+using.name+'] FOR '+secondary_type_desc
		     collate SQL_Latin1_General_CP1_CI_AS as [using],
		  case when i.is_primary_key <>0 then ' PRIMARY KEY ' 
			  else case when i.is_unique<>0 then 'UNIQUE ' else '' end end
			+CASE i.type when 1 then 'CLUSTERED' when 3 then 'XML' ELSE '' END as Index_Type, 
		  COALESCE(SUBSTRING(
			(SELECT ','+COL_NAME(ic.object_id,ic.column_id)
			   + case when is_descending_key<>0 then ' DESC' else '' end
			 FROM  sys.index_columns AS ic
			 WHERE ic.index_ID=i.index_ID AND ic.object_id=i.object_id
			 ORDER BY key_ordinal
			 FOR XML PATH(''), TYPE).value('.', 'varchar(max)'),2,2000),'?') as columns, 
			 replace('WITH ( ' + stuff(
				CASE WHEN i.is_Padded<>0 THEN ', PAD_INDEX  = ON ' ELSE '' END+
				CASE WHEN i.fill_factor not in (0,100) 
				  THEN ', FILLFACTOR  ='+convert(varchar(3),i.fill_factor)+'' ELSE '' END+
				CASE WHEN i.ignore_dup_key<>0 THEN ', IGNORE_DUP_KEY = ON' ELSE '' END+
				CASE WHEN i.allow_row_locks=0 THEN ', ALLOW_ROW_LOCKS  = OFF' ELSE '' END+
				CASE WHEN i.allow_page_locks=0 THEN ', ALLOW_PAGE_LOCKS  = OFF' ELSE ' ' END,1,1,'')
			   +')','WITH ( )','')+' ON '+coalesce(quotename(FG.name),'PRIMARY')+''
				+ CASE WHEN ep.value IS NOT NULL THEN '
		/*'+CAST(value AS VARCHAR(100))+'*/' ELSE '' END as on_clause
		 FROM sys.indexes i
         LEFT outer JOIN sys.xml_indexes
           on i.index_ID=sys.xml_indexes.index_ID
           and i.object_ID=sys.xml_indexes.object_ID
         LEFT outer JOIN sys.indexes Using 
           on using.index_ID=sys.xml_indexes.using_xml_index_id
           and using.object_ID=sys.xml_indexes.object_ID
		  LEFT outer JOIN sys.filegroups FG
			ON i.data_space_id = FG.data_space_id
		  LEFT OUTER JOIN sys.extended_properties ep
			ON i.object_id = ep.major_ID  AND i.index_ID = minor_ID AND class=7
		  WHERE i.object_ID=@TableObjectID and i.type<>0
		 Select @PrimaryKeyAndUniqueConstraintsBuild=stuff((select ',
    CONSTRAINT '+quotename(name)+index_Type+ ' ('+columns+') '
				+On_Clause from @indexes where is_unique=1 
		  FOR XML PATH(''), TYPE).value('.', 'varchar(max)'),1,1,'')  
		Select @IndexBuild=Stuff((select ',
	CREATE '+index_type+' INDEX '+quotename(name)+' ON '+quotename(@SchemaName)+'.'+Quotename(@TableName)+' ('+columns+') '
	            +coalesce([using],'')
				+On_Clause from @indexes where is_unique=0 
		FOR XML PATH(''), TYPE).value('.', 'varchar(max)'),1,1,'')   
		IF @PrimaryKeyAndUniqueConstraintsBuild IS NULL
			SELECT @Script=STUFF(@Script,--delete final comma line-terminator
							LEN(@Script)-CHARINDEX('|,|',
							REVERSE(@Script)+'|')-1,3,'')
        if (@Script is null) raiserror ('2 script is null',16,1)    

Declare @TableConstraintsBuild Varchar(MAX)
select @TableConstraintsBuild=
(Select 'ALTER TABLE '+tname+  ' WITH CHECK ADD CONSTRAINT ' 
           +name + ' CHECK ('+Replace(definition,'''','''''')+')
ALTER TABLE '+tname+' CHECK CONSTRAINT '+name+'
'
	from (select Quotename(Object_schema_name(ccr.parent_object_ID))
	           +'.'+quotename(object_name(ccr.parent_object_ID))as tname,
	      name,definition
		  FROM sys.check_constraints ccr
			where ccr.parent_object_ID = @TableObjectID
			and parent_column_ID=0)f
	FOR XML PATH(''), TYPE).value('.', 'varchar(max)')


Declare @ExtendedPropertiesBuild Varchar(MAX)
Declare @ExtendedPropertyExecute Varchar(8000)

Select @ExtendedPropertyExecute='
EXEC sys.sp_addextendedproperty N''MS_Description'', N''<value>'',
      N''SCHEMA'', N'''+object_Schema_Name(@TableObjectID)+''', N''TABLE'', N'''+object_Name(@TableObjectID)+'''' 

Select @ExtendedPropertiesBuild=
(Select Replace(@ExtendedPropertyExecute,'<value>',Replace(convert(varchar(max),value),'''',''''''))
		+Coalesce(', '''+Level2Type+''', '''+Level2Name+'''','')
from (      
	select value, null as level2type ,null as level2Name
	  from sys.extended_properties ep
						 WHERE ep.major_ID = @TableObjectID
						 AND  minor_ID=0 AND class=1
	union all 
	 
	select Value,'COLUMN',sys.columns.name
	  from sys.extended_properties ep
	  inner join sys.columns
	  on sys.columns.column_ID=minor_ID
	  and sys.columns.object_ID=major_ID
	  WHERE ep.major_ID = @TableObjectID
						 AND  class=1 AND  minor_ID>0
	union all
	Select Value,'CONSTRAINT',object_name(ep.major_ID) 
	from sys.extended_properties ep
	inner join sys.objects on object_ID=major_ID
	where parent_Object_ID=@TableObjectID
	and minor_ID=0
	union all
	Select Value,'INDEX', sys.indexes.name 
	from sys.extended_properties ep
	inner join sys.indexes on object_ID=major_ID and index_ID=minor_ID
	where major_ID=@TableObjectID
	and class=7 and minor_ID>0
	)f
	FOR XML PATH(''), TYPE).value('.', 'varchar(max)')  
         if (@Script is null) raiserror ('3 script is null',16,1)    
                                 
 SELECT @Script=REPLACE(@Script,'|,|',',')+COALESCE(@PrimaryKeyAndUniqueConstraintsBuild,'')+'
) ON '+quotename(coalesce(@FileGroup,'PRIMARY'))+' -- could be heap or a clustered table
'+coalesce('
/* The table constraints */'+@TableConstraintsBuild,'')
   + cOALESCE ('
/* and the non-clustered indexes for the table */'+@IndexBuild,'')   	
   + Coalesce('

/* and the extended properties associated with the table and its indexes */'+ @ExtendedPropertiesBuild,'')     

END
SELECT left(COALESCE(@Script,'-- could not find '''+@identifier+''' in '+DB_NAME(),'null identifier.'),8000)
    AS Build_Script
union all
SELECT substring(COALESCE(@Script,'-- could not find '''+@identifier+''' in '+DB_NAME(),'null identifier.'),8000,8000)
    AS Build_Script
    
GO
IF NOT EXISTS
  (SELECT 1 FROM sys.objects WHERE NAME = 'sp_ScriptFor' AND IS_MS_SHIPPED=1)
   EXEC sp_ms_marksystemobject 'sp_ScriptFor'
GO

