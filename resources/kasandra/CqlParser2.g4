/*
 * The MIT License (MIT)
 * 
 * Copyright (c) 2014 by Domagoj Kovačević
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Project : cql-parser; an ANTLR4 grammar for Apache Cassandra CQL  https://github.com/kdcro101cql-parser
 */

grammar CqlParser2;

root
   : cqls? MINUSMINUS? eof
   ;

cqls
   : (cql MINUSMINUS? statementSeparator | empty)* (cql (MINUSMINUS? statementSeparator)? | empty)
   ;

statementSeparator
   : SEMI
   ;

empty
   : statementSeparator
   ;

cql
   : alterKeyspace
   | alterMaterializedView
   | alterRole
   | alterTable
   | alterType
   | alterUser
   | applyBatch
   | createAggregate
   | createFunction
   | createIndex
   | createKeyspace
   | createMaterializedView
   | createRole
   | createTable
   | createTrigger
   | createType
   | createUser
   | delete
   | dropAggregate
   | dropFunction
   | dropIndex
   | dropKeyspace
   | dropMaterializedView
   | dropRole
   | dropTable
   | dropTrigger
   | dropType
   | dropUser
   | grant
   | insert
   | listPermissions
   | listRoles
   | revoke
   | select
   | truncate
   | update
   | use
   ;

revoke
   : kwRevoke priviledge kwOn resource kwFrom role
   ;

listUsers
   : kwList kwUsers
   ;

listRoles
   : kwList kwRoles (kwOf role)? kwNorecursive?
   ;

listPermissions
   : kwList priviledge (kwOn resource)? (kwOf role)?
   ;

grant
   : kwGrant priviledge kwOn resource kwTo role
   ;

priviledge
   : (kwAll | kwAllPermissions)
   | kwAlter
   | kwAuthorize
   | kwDescibe
   | kwExecute
   | kwCreate
   | kwDrop
   | kwModify
   | kwSelect
   ;

resource
   : kwAll kwFunctions
   | kwAll kwFunctions kwIn kwKeyspace keyspace
   | kwFunction (keyspace DOT)? function
   | kwAll kwKeyspaces
   | kwKeyspace keyspace
   | (kwTable)? (keyspace DOT)? table
   | kwAll kwRoles
   | kwRole role
   ;

createUser
   : kwCreate kwUser ifNotExist? user kwWith kwPassword stringLiteral (kwSuperuser | kwNosuperuser)?
   ;

createRole
   : kwCreate kwRole ifNotExist? role roleWith?
   ;

createType
   : kwCreate kwType ifNotExist? (keyspace DOT)? type syntaxBracketLr typeMemberColumnList syntaxBracketRr
   ;

typeMemberColumnList
   : column dataType (syntaxComma column dataType)*
   ;

createTrigger
   : kwCreate kwTrigger ifNotExist? (keyspace DOT)? trigger kwUsing triggerClass
   ;

createMaterializedView
   : kwCreate kwMaterialized kwView ifNotExist? (keyspace DOT)? materializedView kwAs kwSelect columnList kwFrom (keyspace DOT)? table materializedViewWhere kwPrimary kwKey syntaxBracketLr primaryKeyDefinition syntaxBracketRr (kwWith materializedViewOptions)?
   ;

materializedViewWhere
   : kwWhere columnNotNullList (kwAnd relationElements)?
   ;

columnNotNullList
   : columnNotNull (kwAnd columnNotNull)*
   ;

columnNotNull
   : column kwIs kwNot kwNull
   ;

/* materializedViewOptions
   : tableOptions
   | clusteringOrder tableOptions
   | clusteringOrder
   ; */

materializedViewOptions
   : clusteringOrder tableOptions
   | clusteringOrder
   ;

// CREATE MATERIALIZED VIEW [IF NOT EXISTS] [keyspace_name.] view_name
// AS SELECT column_list
// FROM [keyspace_name.] base_table_name
// WHERE column_name IS NOT NULL [AND column_name IS NOT NULL ...] 
//       [AND relation...] 
// PRIMARY KEY ( column_list )
// [WITH [table_properties]
//       [AND CLUSTERING ORDER BY (cluster_column_name order_option )]]
createKeyspace
   : kwCreate kwKeyspace ifNotExist? keyspace kwWith kwReplication OPERATOR_EQ syntaxBracketLc replicationList syntaxBracketRc (kwAnd durableWrites)?
   ;

createFunction
   : kwCreate orReplace? kwFunction ifNotExist? (keyspace DOT)? function syntaxBracketLr paramList? syntaxBracketRr returnMode kwReturns dataType kwLanguage language kwAs codeBlock
   ;

codeBlock
   : CODE_BLOCK
   ;

paramList
   : param (syntaxComma param)*
   ;

returnMode
   : (kwCalled | kwReturns kwNull) kwOn kwNull kwInput
   ;

createAggregate
   : kwCreate orReplace? kwAggregate ifNotExist? (keyspace DOT)? aggregate syntaxBracketLr dataType syntaxBracketRr kwSfunc function kwStype dataType kwFinalfunc function kwInitcond initCondDefinition
   ;

// paramList
// : 
initCondDefinition
   : constant
   | initCondList
   | initCondListNested
   | initCondHash
   ;

initCondHash
   : syntaxBracketLc initCondHashItem (syntaxComma initCondHashItem)* syntaxBracketRc
   ;

initCondHashItem
   : hashKey COLON initCondDefinition
   ;

initCondListNested
   : syntaxBracketLr initCondList (syntaxComma constant | initCondList)* syntaxBracketRr
   ;

initCondList
   : syntaxBracketLr constant (syntaxComma constant)* syntaxBracketRr
   ;

orReplace
   : kwOr kwReplace
   ;

alterUser
   : kwAlter kwUser user kwWith userPassword userSuperUser?
   ;

userPassword
   : kwPassword stringLiteral
   ;

userSuperUser
   : kwSuperuser
   | kwNosuperuser
   ;

alterType
   : kwAlter kwType (keyspace DOT)? type alterTypeOperation
   ;

alterTypeOperation
   : alterTypeAlterType
   | alterTypeAdd
   | alterTypeRename
   ;

alterTypeRename
   : kwRename alterTypeRenameList
   ;

alterTypeRenameList
   : alterTypeRenameItem (kwAnd alterTypeRenameItem)*
   ;

alterTypeRenameItem
   : column kwTo column
   ;

alterTypeAdd
   : kwAdd column dataType (syntaxComma column dataType)*
   ;

alterTypeAlterType
   : kwAlter column kwType dataType
   ;

alterTable
   : kwAlter kwTable (keyspace DOT)? table alterTableOperation
   ;

alterTableOperation
   : alterTableAdd
   | alterTableDropColumns
   | alterTableDropColumns
   | alterTableDropCompactStorage
   | alterTableRename
   | alterTableWith
   ;

alterTableWith
   : kwWith tableOptions
   ;

alterTableRename
   : kwRename column kwTo column
   ;

alterTableDropCompactStorage
   : kwDrop kwCompact kwStorage
   ;

alterTableDropColumns
   : kwDrop alterTableDropColumnList
   ;

alterTableDropColumnList
   : column (syntaxComma column)*
   ;

alterTableAdd
   : kwAdd alterTableColumnDefinition
   ;

alterTableColumnDefinition
   : column dataType (syntaxComma column dataType)*
   ;

alterRole
   : kwAlter kwRole role roleWith?
   ;

roleWith
   : kwWith (roleWithOptions (kwAnd roleWithOptions)*)
   ;

roleWithOptions
   : kwPassword OPERATOR_EQ stringLiteral
   | kwLogin OPERATOR_EQ booleanLiteral
   | kwSuperuser OPERATOR_EQ booleanLiteral
   | kwOptions OPERATOR_EQ optionHash
   ;

alterMaterializedView
   : kwAlter kwMaterialized kwView (keyspace DOT)? materializedView (kwWith tableOptions)?
   ;

dropUser
   : kwDrop kwUser ifExist? user
   ;

dropType
   : kwDrop kwType ifExist? (keyspace DOT)? type
   ;

dropMaterializedView
   : kwDrop kwMaterialized kwView ifExist? (keyspace DOT)? materializedView
   ;

dropAggregate
   : kwDrop kwAggregate ifExist? (keyspace DOT)? aggregate
   ;

dropFunction
   : kwDrop kwFunction ifExist? (keyspace DOT)? function
   ;

dropTrigger
   : kwDrop kwTrigger ifExist? trigger kwOn (keyspace DOT)? table
   ;

dropRole
   : kwDrop kwRole ifExist? role
   ;

dropTable
   : kwDrop kwTable ifExist? (keyspace DOT)? table
   ;

dropKeyspace
   : kwDrop kwKeyspace ifExist? keyspace
   ;

dropIndex
   : kwDrop kwIndex ifExist? (keyspace DOT)? indexName
   ;

createTable
   : kwCreate kwTable ifNotExist? (keyspace DOT)? table syntaxBracketLr columnDefinitionList syntaxBracketRr withElement?
   ;

withElement
   : kwWith (clusteringOrder tableOptions? | tableOptionItem tableOptions? )
//   : kwWith tableOptionItem tableOptions?
   ;

//clusteringOrder
//   : kwClustering kwOrder kwBy syntaxBracketLr column orderDirection? syntaxBracketRr
//   ;

clusteringOrder
   : kwClustering kwOrder kwBy syntaxBracketLr clusteringOrderKey syntaxBracketRr
   ;

clusteringOrderKey
   : column orderDirection (syntaxComma column orderDirection)*
   ;

tableOptions
   : kwAnd tableOptionItem (kwAnd tableOptionItem)*
   ;

tableOptionItem
   : tableOptionName OPERATOR_EQ tableOptionValue
   | tableOptionName OPERATOR_EQ optionHash
   ;

tableOptionName
   : OBJECT_NAME
   ;

tableOptionValue
   : stringLiteral
   | floatLiteral
   ;

optionHash
   : syntaxBracketLc optionHashItem (syntaxComma optionHashItem)* syntaxBracketRc
   ;

optionHashItem
   : optionHashKey COLON optionHashValue
   ;

optionHashKey
   : stringLiteral
   ;

optionHashValue
   : stringLiteral
   | floatLiteral
   ;

columnDefinitionList
   : (columnDefinition) (syntaxComma columnDefinition)* (syntaxComma primaryKeyElement)?
   ;

//  
columnDefinition
   : column dataType primaryKeyColumn?
//   | 'role' dataType primaryKeyColumn?
   ;

// 
primaryKeyColumn
   : kwPrimary kwKey
   ;

primaryKeyElement
   : kwPrimary kwKey syntaxBracketLr primaryKeyDefinition syntaxBracketRr
   ;

primaryKeyDefinition
   : singlePrimaryKey
   | compoundKey
   | compositeKey
   ;

singlePrimaryKey
   : column
   ;

compoundKey
   : partitionKey (syntaxComma clusteringKeyList)
   ;

compositeKey
   : syntaxBracketLr partitionKeyList syntaxBracketRr (syntaxComma clusteringKeyList)?
   ;

partitionKeyList
   : (partitionKey) (syntaxComma partitionKey)*
   ;

clusteringKeyList
   : (clusteringKey) (syntaxComma clusteringKey)*
   ;

partitionKey
   : column
   ;

clusteringKey
   : column
   ;

applyBatch
   : kwApply kwBatch
   ;

beginBatch
   : kwBegin batchType? kwBatch usingTimestampSpec?
   ;

batchType
   : kwLogged
   | kwUnlogged
   ;

alterKeyspace
   : kwAlter kwKeyspace keyspace kwWith kwReplication OPERATOR_EQ syntaxBracketLc replicationList syntaxBracketRc (kwAnd durableWrites)?
   ;

replicationList
   : (replicationListItem) (syntaxComma replicationListItem)*
   ;

replicationListItem
   : STRING_LITERAL COLON STRING_LITERAL
   | STRING_LITERAL COLON DECIMAL_LITERAL
   ;

durableWrites
   : kwDurableWrites OPERATOR_EQ booleanLiteral
   ;

use
   : kwUse keyspace
   ;

truncate
   : kwTruncate (kwTable)? (keyspace DOT)? table
   ;

createIndex
   : kwCreate kwIndex ifNotExist? indexName? kwOn (keyspace DOT)? table syntaxBracketLr indexColumnSpec syntaxBracketRr
   ;

indexName
   : OBJECT_NAME
   | stringLiteral
   ;

indexColumnSpec
   : column
   | indexKeysSpec
   | indexEntriesSSpec
   | indexFullSpec
   ;

indexKeysSpec
   : kwKeys syntaxBracketLr OBJECT_NAME syntaxBracketRr
   ;

indexEntriesSSpec
   : kwEntries syntaxBracketLr OBJECT_NAME syntaxBracketRr
   ;

indexFullSpec
   : kwFull syntaxBracketLr OBJECT_NAME syntaxBracketRr
   ;

delete
   : beginBatch? kwDelete deleteColumnList? fromSpec usingTimestampSpec? whereSpec (ifExist | ifSpec)?
   ;

deleteColumnList
   : (deleteColumnItem) (syntaxComma deleteColumnItem)*
   ;

deleteColumnItem
   : OBJECT_NAME
   | OBJECT_NAME LS_BRACKET (stringLiteral | decimalLiteral) RS_BRACKET
   ;

update
   : beginBatch? kwUpdate (keyspace DOT)? table usingTtlTimestamp? kwSet assignments whereSpec (ifExist | ifSpec)?
   ;

ifSpec
   : kwIf ifConditionList
   ;

ifConditionList
   : (ifCondition) (kwAnd ifCondition)*
   ;

ifCondition
   : OBJECT_NAME OPERATOR_EQ constant
   ;

assignments
   : (assignmentElement) (syntaxComma assignmentElement)*
   ;

assignmentElement
   : OBJECT_NAME OPERATOR_EQ (constant | assignmentMap | assignmentSet | assignmentList)
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) decimalLiteral
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) assignmentSet
   | OBJECT_NAME OPERATOR_EQ assignmentSet (PLUS | MINUS) OBJECT_NAME
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) assignmentMap
   | OBJECT_NAME OPERATOR_EQ assignmentMap (PLUS | MINUS) OBJECT_NAME
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) assignmentList
   | OBJECT_NAME OPERATOR_EQ assignmentList (PLUS | MINUS) OBJECT_NAME
   | OBJECT_NAME syntaxBracketLs decimalLiteral syntaxBracketRs OPERATOR_EQ constant
   ;

assignmentSet
   : syntaxBracketLc constant (syntaxComma constant)* syntaxBracketRc
   ;

assignmentMap
   : syntaxBracketLc (constant syntaxColon constant) (constant syntaxColon constant)* syntaxBracketRc
   ;

assignmentList
   : syntaxBracketLs constant (syntaxColon constant)* syntaxBracketRs
   ;

insert
   : (beginBatch |) kwInsert kwInto (keyspace DOT |) table insertColumnSpec insertValuesSpec (ifNotExist |) usingTtlTimestamp?
   ;

usingTtlTimestamp
   : kwUsing ttl
   | kwUsing ttl kwAnd timestamp
   | kwUsing timestamp
   | kwUsing timestamp kwAnd ttl
   ;

timestamp
   : kwTimestamp decimalLiteral
   ;

ttl
   : kwTtl decimalLiteral
   ;

usingTimestampSpec
   : kwUsing timestamp
   ;

ifNotExist
   : kwIf kwNot kwExists
   ;

ifExist
   : kwIf kwExists
   ;

insertValuesSpec
   : kwValues '(' expressionList ')'
   ;

insertColumnSpec
   : '(' columnList ')'
   ;

columnList
   : column (syntaxComma column)*
   ;

expressionList
   : (constant | assignmentMap | assignmentSet | assignmentList) (syntaxComma (constant | assignmentMap | assignmentSet | assignmentList))*
   ;

select
   : kwSelect (distinctSpec |) selectElements fromSpec whereSpec? orderSpec? limitSpec? allowFilteringSpec?
   ;

allowFilteringSpec
   : kwAllow kwFiltering
   ;

limitSpec
   : kwLimit decimalLiteral
   ;

fromSpec
   : kwFrom fromSpecElement
   ;

fromSpecElement
   : OBJECT_NAME
   | OBJECT_NAME '.' OBJECT_NAME
   ;

orderSpec
   : kwOrder kwBy orderSpecElement
   ;

orderSpecElement
   : OBJECT_NAME (kwAsc | kwDesc)?
   ;

whereSpec
   : kwWhere relationElements
   ;

distinctSpec
   : kwDistinct
   ;

selectElements
   : (star = '*' | selectElement) (syntaxComma selectElement)*
   ;

selectElement
   : OBJECT_NAME '.' '*'
   | OBJECT_NAME (kwAs OBJECT_NAME)?
   | functionCall (kwAs OBJECT_NAME)?
   ;

relationElements
   : (relationElement) (kwAnd relationElement)*
   ;

relationElement
   : OBJECT_NAME (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) constant
   | OBJECT_NAME '.' OBJECT_NAME (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) constant
   | functionCall (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) constant
   | functionCall (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) functionCall
   | OBJECT_NAME kwIn '(' functionArgs? ')'
   | relalationContainsKey
   | relalationContains
   ;

relalationContains
   : OBJECT_NAME kwContains constant
   ;

relalationContainsKey
   : OBJECT_NAME (kwContains kwKey) constant
   ;

functionCall
   : OBJECT_NAME '(' STAR ')'
   | OBJECT_NAME '(' functionArgs? ')'
   ;

functionArgs
   : (constant | OBJECT_NAME | functionCall) (syntaxComma (constant | OBJECT_NAME | functionCall))*
   ;

constant
   : UUID
   | stringLiteral
   | decimalLiteral
   | hexadecimalLiteral
   | booleanLiteral
   | kwNull
   ;

decimalLiteral
   : DECIMAL_LITERAL
   ;

floatLiteral
   : DECIMAL_LITERAL
   | FLOAT_LITERAL
   ;

stringLiteral
   : STRING_LITERAL
   ;

booleanLiteral
   : K_TRUE
   | K_FALSE
   ;

hexadecimalLiteral
   : HEXADECIMAL_LITERAL
   ;

keyspace
   : OBJECT_NAME
   | DQUOTE OBJECT_NAME DQUOTE
   ;

table
   : OBJECT_NAME
   | DQUOTE OBJECT_NAME DQUOTE
   ;

column
   : OBJECT_NAME
   | DQUOTE OBJECT_NAME DQUOTE
   | 'role'
   | 'level'
   | 'type'
   | 'date'
   ;

dataType
   : dataTypeName dataTypeDefinition?
   ;

dataTypeName
   : OBJECT_NAME
   | K_TIMESTAMP
   | K_SET
   | K_ASCII
   | K_BIGINT
   | K_BLOB
   | K_BOOLEAN
   | K_COUNTER
   | K_DATE
   | K_DECIMAL
   | K_DOUBLE
   | K_FLOAT
   | K_FROZEN
   | K_INET
   | K_INT
   | K_LIST
   | K_MAP
   | K_SMALLINT
   | K_TEXT
   | K_TIME
   | K_TIMEUUID
   | K_TINYINT
   | K_TUPLE
   | K_VARCHAR
   | K_VARINT
   | K_TIMESTAMP
   | K_UUID
   ;

dataTypeDefinition
   : syntaxBracketLa dataTypeName (syntaxComma dataTypeName)* syntaxBracketRa
   ;

orderDirection
   : kwAsc
   | kwDesc
   ;

role
   : OBJECT_NAME
   ;

trigger
   : OBJECT_NAME
   ;

triggerClass
   : stringLiteral
   ;

materializedView
   : OBJECT_NAME
   ;

type
   : OBJECT_NAME
   ;

aggregate
   : OBJECT_NAME
   ;

function
   : OBJECT_NAME
   ;

language
   : OBJECT_NAME
   ;

user
   : OBJECT_NAME
   ;

password
   : stringLiteral
   ;

hashKey
   : OBJECT_NAME
   ;

param
   : paramName dataType
   ;

paramName
   : OBJECT_NAME
   ;

kwAdd
   : K_ADD
   ;

kwAggregate
   : K_AGGREGATE
   ;

kwAll
   : K_ALL
   ;

kwAllPermissions
   : K_ALL K_PERMISSIONS
   ;

kwAllow
   : K_ALLOW
   ;

kwAlter
   : K_ALTER
   ;

kwAnd
   : K_AND
   ;

kwApply
   : K_APPLY
   ;

kwAs
   : K_AS
   ;

kwAsc
   : K_ASC
   ;

kwAuthorize
   : K_AUTHORIZE
   ;

kwBatch
   : K_BATCH
   ;

kwBegin
   : K_BEGIN
   ;

kwBy
   : K_BY
   ;

kwCalled
   : K_CALLED
   ;

kwClustering
   : K_CLUSTERING
   ;

kwCompact
   : K_COMPACT
   ;

kwContains
   : K_CONTAINS
   ;

kwCreate
   : K_CREATE
   ;

kwDelete
   : K_DELETE
   ;

kwDesc
   : K_DESC
   ;

kwDescibe
   : K_DESCRIBE
   ;

kwDistinct
   : K_DISTINCT
   ;

kwDrop
   : K_DROP
   ;

kwDurableWrites
   : K_DURABLE_WRITES
   ;

kwEntries
   : K_ENTRIES
   ;

kwExecute
   : K_EXECUTE
   ;

kwExists
   : K_EXISTS
   ;

kwFiltering
   : K_FILTERING
   ;

kwFinalfunc
   : K_FINALFUNC
   ;

kwFrom
   : K_FROM
   ;

kwFull
   : K_FULL
   ;

kwFunction
   : K_FUNCTION
   ;

kwFunctions
   : K_FUNCTIONS
   ;

kwGrant
   : K_GRANT
   ;

kwIf
   : K_IF
   ;

kwIn
   : K_IN
   ;

kwIndex
   : K_INDEX
   ;

kwInitcond
   : K_INITCOND
   ;

kwInput
   : K_INPUT
   ;

kwInsert
   : K_INSERT
   ;

kwInto
   : K_INTO
   ;

kwIs
   : K_IS
   ;

kwKey
   : K_KEY
   ;

kwKeys
   : K_KEYS
   ;

kwKeyspace
   : K_KEYSPACE
   ;

kwKeyspaces
   : K_KEYSPACES
   ;

kwLanguage
   : K_LANGUAGE
   ;

kwLimit
   : K_LIMIT
   ;

kwList
   : K_LIST
   ;

kwLogged
   : K_LOGGED
   ;

kwLogin
   : K_LOGIN
   ;

kwMaterialized
   : K_MATERIALIZED
   ;

kwModify
   : K_MODIFY
   ;

kwNosuperuser
   : K_NOSUPERUSER
   ;

kwNorecursive
   : K_NORECURSIVE
   ;

kwNot
   : K_NOT
   ;

kwNull
   : K_NULL
   ;

kwOf
   : K_OF
   ;

kwOn
   : K_ON
   ;

kwOptions
   : K_OPTIONS
   ;

kwOr
   : K_OR
   ;

kwOrder
   : K_ORDER
   ;

kwPassword
   : K_PASSWORD
   ;

kwPrimary
   : K_PRIMARY
   ;

kwRename
   : K_RENAME
   ;

kwReplace
   : K_REPLACE
   ;

kwReplication
   : K_REPLICATION
   ;

kwReturns
   : K_RETURNS
   ;

kwRole
   : K_ROLE
   ;

kwRoles
   : K_ROLES
   ;

kwSelect
   : K_SELECT
   ;

kwSet
   : K_SET
   ;

kwSfunc
   : K_SFUNC
   ;

kwStorage
   : K_STORAGE
   ;

kwStype
   : K_STYPE
   ;

kwSuperuser
   : K_SUPERUSER
   ;

kwTable
   : K_TABLE
   ;

kwTimestamp
   : K_TIMESTAMP
   ;

kwTo
   : K_TO
   ;

kwTrigger
   : K_TRIGGER
   ;

kwTruncate
   : K_TRUNCATE
   ;

kwTtl
   : K_TTL
   ;

kwType
   : K_TYPE
   ;

kwUnlogged
   : K_UNLOGGED
   ;

kwUpdate
   : K_UPDATE
   ;

kwUse
   : K_USE
   ;

kwUser
   : K_USER
   ;

kwUsers
   : K_USERS
   ;

kwUsing
   : K_USING
   ;

kwValues
   : K_VALUES
   ;

kwView
   : K_VIEW
   ;

kwWhere
   : K_WHERE
   ;

kwWith
   : K_WITH
   ;

kwRevoke
   : K_REVOKE
   ;

eof
   : EOF
   ;

// BRACKETS
// L - left
// R - right
// a - angle
// c - curly
// r - rounded
syntaxBracketLr
   : LR_BRACKET
   ;

syntaxBracketRr
   : RR_BRACKET
   ;

syntaxBracketLc
   : LC_BRACKET
   ;

syntaxBracketRc
   : RC_BRACKET
   ;

syntaxBracketLa
   : OPERATOR_LT
   ;

syntaxBracketRa
   : OPERATOR_GT
   ;

syntaxBracketLs
   : LS_BRACKET
   ;

syntaxBracketRs
   : RS_BRACKET
   ;

syntaxComma
   : COMMA
   ;

syntaxColon
   : COLON
   ;

LR_BRACKET
   : '('
   ;


RR_BRACKET
   : ')'
   ;


LC_BRACKET
   : '{'
   ;


RC_BRACKET
   : '}'
   ;


LS_BRACKET
   : '['
   ;


RS_BRACKET
   : ']'
   ;


COMMA
   : ','
   ;


SEMI
   : ';'
   ;


COLON
   : ':'
   ;


SPACE
   : [ \t\r\n] + -> channel (HIDDEN)
   ;


SPEC_MYSQL_COMMENT
   : '/*!' . +? '*/' -> channel (HIDDEN)
   ;


COMMENT_INPUT
   : '/*' .*? '*/' -> channel (HIDDEN)
   ;


LINE_COMMENT
   : (('-- ' | '#' | '//') ~ [\r\n]* ('\r'? '\n' | EOF) | '--' ('\r'? '\n' | EOF)) -> channel (HIDDEN)
   ;


DOT
   : '.'
   ;


STAR
   : '*'
   ;


DIVIDE
   : '/'
   ;


MODULE
   : '%'
   ;


PLUS
   : '+'
   ;


MINUSMINUS
   : '--'
   ;


MINUS
   : '-'
   ;


DQUOTE
   : '"'
   ;


SQUOTE
   : '\''
   ;

// Keywords

K_ADD
   : A D D | 'ADD'
   ;


K_AGGREGATE
   : A G G R E G A T E | 'AGGREGATE'
   ;


K_ALL
   : A L L | 'ALL'
   ;


K_ALLOW
   : A L L O W | 'ALLOW'
   ;


K_ALTER
   : A L T E R | 'ALTER'
   ;


K_AND
   : A N D | 'AND'
   ;


K_ANY
   : A N Y | 'ANY'
   ;


K_APPLY
   : A P P L Y | 'APPLY'
   ;


K_AS
   : A S | 'AS'
   ;


K_ASC
   : A S C | 'ASC'
   ;


K_AUTHORIZE
   : A U T H O R I Z E | 'AUTHORIZE'
   ;


K_BATCH
   : B A T C H | 'BATCH'
   ;


K_BEGIN
   : B E G I N | 'BEGIN'
   ;


K_BY
   : B Y | 'BY'
   ;


K_CALLED
   : C A L L E D | 'CALLED'
   ;


K_CLUSTERING
   : C L U S T E R I N G | 'CLUSTERING'
   ;


K_COLUMNFAMILY
   : C O L U M N F A M I L Y | 'COLUMNFAMILY'
   ;


K_COMPACT
   : C O M P A C T | 'COMPACT'
   ;


K_CONSISTENCY
   : C O N S I S T E N C Y | 'CONSISTENCY'
   ;


K_CONTAINS
   : C O N T A I N S | 'CONTAINS'
   ;


K_CREATE
   : C R E A T E | 'CREATE'
   ;


K_CUSTOM
   : C U S T O M | 'CUSTOM'
   ;


K_DELETE
   : D E L E T E | 'DELETE'
   ;


K_DESC
   : D E S C | 'DESC'
   ;


K_DESCRIBE
   : D E S C R I B E | 'DESCRIBE'
   ;


K_DISTINCT
   : D I S T I N C T | 'DISTINCT'
   ;


K_DROP
   : D R O P | 'DROP'
   ;


K_DURABLE_WRITES
   : D U R A B L E '_' W R I T E S | 'DURABLE_WRITES'
   ;


K_EACH_QUORUM
   : E A C H '_' Q U O R U M | 'EACH_QUORUM'
   ;


K_ENTRIES
   : E N T R I E S | 'ENTRIES'
   ;


K_EXECUTE
   : E X E C U T E | 'EXECUTE'
   ;


K_EXISTS
   : E X I S T S | 'EXISTS'
   ;


K_FALSE
   : F A L S E | 'FALSE'
   ;


K_FILTERING
   : F I L T E R I N G | 'FILTERING'
   ;


K_FINALFUNC
   : F I N A L F U N C | 'FINALFUNC'
   ;


K_FROM
   : F R O M | 'FROM'
   ;


K_FULL
   : F U L L | 'FULL'
   ;


K_FUNCTION
   : F U N C T I O N | 'FUNCTION'
   ;


K_FUNCTIONS
   : F U N C T I O N S | 'FUNCTIONS'
   ;


K_GRANT
   : G R A N T | 'GRANT'
   ;


K_IF
   : I F | 'IF'
   ;


K_IN
   : I N | 'IN'
   ;


K_INDEX
   : I N D E X | 'INDEX'
   ;


K_INFINITY
   : I N F I N I T Y | 'INFINITY'
   ;


K_INITCOND
   : I N I T C O N D | 'INITCOND'
   ;


K_INPUT
   : I N P U T | 'INPUT'
   ;


K_INSERT
   : I N S E R T | 'INSERT'
   ;


K_INTO
   : I N T O | 'INTO'
   ;


K_IS
   : I S | 'IS'
   ;


K_KEY
   : K E Y | 'KEY'
   ;


K_KEYS
   : K E Y S | 'KEYS'
   ;


K_KEYSPACE
   : K E Y S P A C E | 'KEYSPACE'
   ;


K_KEYSPACES
   : K E Y S P A C E S | 'KEYSPACES'
   ;


K_LANGUAGE
   : L A N G U A G E | 'LANGUAGE'
   ;


K_LEVEL
   : L E V E L | 'LEVEL'
   ;


K_LIMIT
   : L I M I T | 'LIMIT'
   ;


K_LOCAL_ONE
   : L O C A L '_' O N E | 'LOCAL_ONE'
   ;


K_LOCAL_QUORUM
   : L O C A L '_' Q U O R U M | 'LOCAL_QUORUM'
   ;


K_LOGGED
   : L O G G E D | 'LOGGED'
   ;


K_LOGIN
   : L O G I N | 'LOGIN'
   ;


K_MATERIALIZED
   : M A T E R I A L I Z E D | 'MATERIALIZED'
   ;


K_MODIFY
   : M O D I F Y | 'MODIFY'
   ;


K_NAN
   : N A N | 'NAN'
   ;


K_NORECURSIVE
   : N O R E C U R S I V E | 'NORECURSIVE'
   ;


K_NOSUPERUSER
   : N O S U P E R U S E R | 'NOSUPERUSER'
   ;


K_NOT
   : N O T | 'NOT'
   ;


K_NULL
   : N U L L | 'NULL'
   ;


K_OF
   : O F | 'OF'
   ;


K_ON
   : O N | 'ON'
   ;


K_ONE
   : O N E | 'ONE'
   ;


K_OPTIONS
   : O P T I O N S | 'OPTIONS'
   ;


K_OR
   : O R | 'OR'
   ;


K_ORDER
   : O R D E R | 'ORDER'
   ;


K_PARTITION
   : P A R T I T I O N | 'PARTITION'
   ;


K_PASSWORD
   : P A S S W O R D | 'PASSWORD'
   ;


K_PER
   : P E R | 'PER'
   ;


K_PERMISSION
   : P E R M I S S I O N | 'PERMISSION'
   ;


K_PERMISSIONS
   : P E R M I S S I O N S | 'PERMISSIONS'
   ;


K_PRIMARY
   : P R I M A R Y | 'PRIMARY'
   ;


K_QUORUM
   : Q U O R U M | 'QUORUM'
   ;


K_RENAME
   : R E N A M E | 'RENAME'
   ;


K_REPLACE
   : R E P L A C E | 'REPLACE'
   ;


K_REPLICATION
   : R E P L I C A T I O N | 'REPLICATION'
   ;


K_RETURNS
   : R E T U R N S | 'RETURNS'
   ;


K_REVOKE
   : R E V O K E | 'REVOKE'
   ;


K_ROLE
   : R O L E | 'ROLE'
   ;


K_ROLES
   : R O L E S | 'ROLES'
   ;


K_SCHEMA
   : S C H E M A | 'SCHEMA'
   ;


K_SELECT
   : S E L E C T | 'SELECT'
   ;


K_SET
   : S E T | 'SET'
   ;


K_SFUNC
   : S F U N C | 'SFUNC'
   ;


K_STATIC
   : S T A T I C | 'STATIC'
   ;


K_STORAGE
   : S T O R A G E | 'STORAGE'
   ;


K_STYPE
   : S T Y P E | 'STYPE'
   ;


K_SUPERUSER
   : S U P E R U S E R | 'SUPERUSER'
   ;


K_TABLE
   : T A B L E | 'TABLE'
   ;


K_THREE
   : T H R E E | 'THREE'
   ;


K_TIMESTAMP
   : T I M E S T A M P | 'TIMESTAMP'
   ;


K_TO
   : T O | 'TO'
   ;


K_TOKEN
   : T O K E N | 'TOKEN'
   ;


K_TRIGGER
   : T R I G G E R | 'TRIGGER'
   ;


K_TRUE
   : T R U E | 'TRUE'
   ;


K_TRUNCATE
   : T R U N C A T E | 'TRUNCATE'
   ;


K_TTL
   : T T L | 'TTL'
   ;


K_TWO
   : T W O | 'TWO'
   ;


K_TYPE
   : T Y P E | 'TYPE'
   ;


K_UNLOGGED
   : U N L O G G E D | 'UNLOGGED'
   ;


K_UPDATE
   : U P D A T E | 'UPDATE'
   ;


K_USE
   : U S E | 'USE'
   ;


K_USER
   : U S E R | 'USER'
   ;


K_USING
   : U S I N G | 'USING'
   ;


K_UUID
   : U U I D | 'UUID'
   ;


K_VALUES
   : V A L U E S | 'VALUES'
   ;


K_VIEW
   : V I E W | 'VIEW'
   ;


K_WHERE
   : W H E R E | 'WHERE'
   ;


K_WITH
   : W I T H | 'WITH'
   ;


K_WRITETIME
   : W R I T E T I M E | 'WRITETIME'
   ;


K_ASCII
   : A S C I I
   ;


K_BIGINT
   : B I G I N T
   ;


K_BLOB
   : B L O B
   ;


K_BOOLEAN
   : B O O L E A N
   ;


K_COUNTER
   : C O U N T E R
   ;


K_DATE
   : D A T E
   ;


K_DECIMAL
   : D E C I M A L
   ;


K_DOUBLE
   : D O U B L E
   ;


K_FLOAT
   : F L O A T
   ;


K_FROZEN
   : F R O Z E N
   ;


K_INET
   : I N E T
   ;


K_INT
   : I N T
   ;


K_LIST
   : L I S T
   ;


K_MAP
   : M A P
   ;


K_SMALLINT
   : S M A L L I N T
   ;


K_TEXT
   : T E X T
   ;


K_TIMEUUID
   : T I M E U U I D
   ;


K_TIME
   : T I M E
   ;


K_TINYINT
   : T I N Y I N T
   ;


K_TUPLE
   : T U P L E
   ;


K_VARCHAR
   : V A R C H A R
   ;


K_VARINT
   : V A R I N T
   ;


fragment A
   : [aA]
   ;


fragment B
   : [bB]
   ;


fragment C
   : [cC]
   ;


fragment D
   : [dD]
   ;


fragment E
   : [eE]
   ;


fragment F
   : [fF]
   ;


fragment G
   : [gG]
   ;


fragment H
   : [hH]
   ;


fragment I
   : [iI]
   ;


fragment J
   : [jJ]
   ;


fragment K
   : [kK]
   ;


fragment L
   : [lL]
   ;


fragment M
   : [mM]
   ;


fragment N
   : [nN]
   ;


fragment O
   : [oO]
   ;


fragment P
   : [pP]
   ;


fragment Q
   : [qQ]
   ;


fragment R
   : [rR]
   ;


fragment S
   : [sS]
   ;


fragment T
   : [tT]
   ;


fragment U
   : [uU]
   ;


fragment V
   : [vV]
   ;


fragment W
   : [wW]
   ;


fragment X
   : [xX]
   ;


fragment Y
   : [yY]
   ;


fragment Z
   : [zZ]
   ;


fragment CODE_BLOCK_DELIMITER
   : '$$'
   ;


fragment CODE_BLOCK_FRAG
   : '$$' (~ '$' | ('$' ~ ('$')))* '$$'
   ;


fragment HEX_4DIGIT
   : [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]
   ;


fragment OBJECT_NAME_FRAG
   : [a-zA-Z] [A-Za-z0-9_$]*
   ;


fragment SQUOTA_STRING
   : '\'' ('\\' . | '\'\'' | ~ ('\'' | '\\'))* '\''
   ;


CODE_BLOCK
   : CODE_BLOCK_FRAG
   ;


STRING_LITERAL
   : SQUOTA_STRING
   ;


DECIMAL_LITERAL
   : DEC_DIGIT +
   ;


FLOAT_LITERAL
   : (MINUS)? [0-9] + (DOT [0-9] +)?
   ;


HEXADECIMAL_LITERAL
   : 'X' '\'' (HEX_DIGIT HEX_DIGIT) + '\'' | '0X' HEX_DIGIT +
   ;


REAL_LITERAL
   : (DEC_DIGIT +)? '.' DEC_DIGIT + | DEC_DIGIT + '.' EXPONENT_NUM_PART | (DEC_DIGIT +)? '.' (DEC_DIGIT + EXPONENT_NUM_PART) | DEC_DIGIT + EXPONENT_NUM_PART
   ;


OBJECT_NAME
   : OBJECT_NAME_FRAG
   ;


UUID
   : HEX_4DIGIT HEX_4DIGIT '-' HEX_4DIGIT '-' HEX_4DIGIT '-' HEX_4DIGIT '-' HEX_4DIGIT HEX_4DIGIT HEX_4DIGIT
   ;


fragment HEX_DIGIT
   : [0-9A-F]
   ;


fragment DEC_DIGIT
   : [0-9]
   ;


fragment EXPONENT_NUM_PART
   : 'E' '-'? DEC_DIGIT +
   ;


fragment OPERATOR_EQ_FRAG
   : '='
   ;


fragment OPERATOR_LT_FRAG
   : '<'
   ;


fragment OPERATOR_GT_FRAG
   : '>'
   ;


fragment OPERATOR_GTE_FRAG
   : '>='
   ;


fragment OPERATOR_LTE_FRAG
   : '<='
   ;


OPERATOR_EQ
   : OPERATOR_EQ_FRAG
   ;


OPERATOR_LT
   : OPERATOR_LT_FRAG
   ;


OPERATOR_GT
   : OPERATOR_GT_FRAG
   ;


OPERATOR_LTE
   : OPERATOR_LTE_FRAG
   ;


OPERATOR_GTE
   : OPERATOR_GTE_FRAG
   ;


K_USERS
   : U S E R S | 'USERS'
   ;
