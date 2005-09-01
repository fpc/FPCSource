SQLDB readme file, 20 Aug 2005, Joost van der Sluis

since there is no real documentation about sqldb yet, this should be regarded as
a small reminder to myself, and to others who want to write their own
connections.

From the TSQLConnection point-of-view the following methods are called if a
select-statement is used:

OPEN:
  Prepare: (is only called when prepared is false)
            - AllocateCursorHandle (only if the cursor <> nil)
            - Preparestatement
  Execute:
            - Execute
            - AddFieldDefs (only if called for the first time after a prepare)

GETNEXTPAKCET: (probably called several times, offcourse)
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
            

From the TSQLConnection point-of-view the following methods are called if a non-
select-statement is used (execsql):

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


A short description of what each method in a TSQLConnection should do:

* Function AllocateCursorHandle : TSQLCursor; override;

This function creates and returns a TSQLcursor which can be used by any query
for the used type of database. The cursor is only database-dependent, it is
deallocated when the connection of the query changes, or if the query is
destroyed.

* Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;

This function deallocates the TSQLCursor, and sets its value to nil.

* procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams); override;

This functions prepares the query which is given in buf.

It's only called if Prepared is True.
If the database supports prepared queries for the kind of sql-statement (in
cursor.FStatementType) and the prepare was successfully, then cursor.FPrepared
is set to True, so that prepare will not be called again, until UnPrepared
is called. (which sets FPrepared to False)

* procedure FreeFldBuffers(cursor : TSQLCursor); override;

This procedure is called if a Select-query is closed. This procedure is used to
handle all actions which are needed to close a select-statement.


