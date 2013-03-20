SQLDB readme file, initially by Joost van der Sluis

Since there is no real documentation about sqldb yet, this should be regarded as
the beginning of documentation for writing your own connections as well as modifying the code.

Code flow
=========

** Select statement
From the TSQLConnection point of view the following methods are called if a
select statement is used:

OPEN:
  Prepare: (is only called when prepared is false)
            - AllocateCursorHandle (only if the cursor <> nil)
            - Preparestatement
  Execute:
            - Execute
            - AddFieldDefs (only if called for the first time after a prepare)

GETNEXTPACKET: (probably called several times, of course)
            - Fetch
            - Loadfield

CLOSE:
            - FreeFieldBuffers
            - UnPrepareStatement (Only if prepare is False, thus if prepared queries
                         were not supported)
												 
UnPrepare:
            - UnPrepareStatement
            
DESTROY:
            - DeAllocateCursorHandle (Also called if the Connection is changed)
            

** Non select statement (execsql)
From the TSQLConnection point of view the following methods are called if a non-
select statement is used (execsql):

Prepare: (is only called when prepared is false)
            - AllocateCursorHandle (only if the cursor <> nil)
            - Preparestatement

Execute:
            - Execute
            - UnPrepareStatement (Only if prepare is False, thus if prepared queries
                         were not supported)

UNPREPARE:
            - UnPrepareStatement

DESTROY:
            - DeAllocateCursorHandle (Also called if the Connection is changed)


Writing your own T*Connection
=============================

** Required methods
A short description of what some methods in a TSQLConnection must do:

* Function AllocateCursorHandle : TSQLCursor; override;
This function creates and returns a TSQLcursor which can be used by any query
for the used type of database. The cursor is strictly database-dependent
It is deallocated:
- when the connection of the query changes, or
- if the query is destroyed.

* Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;
This function deallocates the TSQLCursor, and sets its value to nil.

* Function AllocateTransactionHandle : TSQLHandle; virtual; abstract;
*** to do ***

* function GetTransactionHandle(trans : TSQLHandle): pointer; virtual; abstract;
*** to do ***

* function Commit(trans : TSQLHandle) : boolean; virtual; abstract;
This function commits the statement in the context of 
transaction trans.

* function RollBack(trans : TSQLHandle) : boolean; virtual; abstract;
This function rolls back/reverts the statement in the context of 
transaction trans

* function StartdbTransaction(trans : TSQLHandle; aParams : string) : boolean; virtual; abstract;
This function starts the transaction trans.

* procedure CommitRetaining(trans : TSQLHandle); virtual; abstract;
This procedure commits the transaction tran and immediately starts the transaction again 
(or opens a new transaction with the same parameters/settings as tran).

* procedure RollBackRetaining(trans : TSQLHandle); virtual; abstract;
This procedure rolls back the transaction tran and immediately opens a
new transaction with the same parameters/settings as the original transaction.

* procedure UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string); virtual;
*** to do ***

* procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs); virtual; abstract;
*** to do ***

* function LoadField(cursor : TSQLCursor;FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean; virtual; abstract;
*** to do ***

* procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TSQLTransaction); virtual; abstract;
*** to do ***

* procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams); override;
This functions prepares the query which is given in buf.
It's only called if Prepared is True (and cursor FPrepared is False).

If the database supports prepared queries for the kind of SQL statement indicated 
in cursor.FStatementType and the prepare was successful, then cursor.FPrepared
is set to True. This keeps Prepare from being called again until UnPrepared
is called (which sets FPrepared to False).

* procedure UnPrepareStatement(cursor : TSQLCursor); virtual; abstract;
This procedure sets cursor.FPrepared to false and performs cleanup tasks to
unprepare the query statement.

* procedure FreeFldBuffers(cursor : TSQLCursor); override;
This procedure is called if a Select query is closed. This procedure is used to
handle all actions which are needed to close a select statement.

* procedure Execute(cursor: TSQLCursor;atransaction:tSQLtransaction; AParams : TParams); virtual; abstract;
Tells the database to execute the statement. No data are loaded from the database client library into the sqldb data set buffers.

* function Fetch(cursor : TSQLCursor) : boolean; virtual; abstract;
Retrieves some resultset data from the database client library and stores them in sqldb dataset buffers.

** Optional (but recommended) methods
* function GetConnectionInfo(InfoType:TConnInfoType): string; virtual;
Returns metadata information about server and client/driver type, version.

* function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; virtual;
Returns an SQL string that retrieves metadata about tables, columns, etc.
