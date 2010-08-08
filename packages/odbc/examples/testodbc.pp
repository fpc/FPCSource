Program TestODBC;

uses odbcsql;


Const
  DBDSn : Pchar = 'FPC';
  Empty : pchar = '';
  Query : pchar = 'SELECT Id,Username,InstEmail from FPdev Order by UserName';
// Adapt to needs...
{$ifdef linux}
  UserName : pchar = 'michael';  // for mysql test.
  Password : pchar = 'geen';
{$else}
    UserName : pchar = ''; // for MS-Acces test.
    Password : pchar = '';
{$endif}

Function ODBCSuccess (Res : Integer) : Boolean;

begin
  ODBCSuccess:= (res=SQL_SUCCESS) or (res=SQL_SUCCESS_WITH_INFO);
end;

Var
  EnvHandle  : SQLHandle;
  DBHandle   : SQLHandle;
  StmtHandle : SQLHSTMT;
  ResID      : Longint;
  ResName    : Array[0..255] of char; // Matches length of field+1
  ResEmail   : Array[0..255] of char;

Procedure FreeHandles;

begin
  If assigned(StmtHAndle) then
    SQLFreeHandle(SQL_HANDLE_STMT,StmtHandle);
  If assigned(dbhandle) then
    SQLFreeHandle(SQL_HANDLE_DBC,DBHandle);
  If assigned(EnvHandle) then
    SQLFreeHandle(SQL_HANDLE_ENV,EnvHandle);
end;

Procedure DoError (Msg : String;ErrCode : Integer);

begin
  FreeHandles;
  Writeln(Msg,' Code : ',ErrCode);
  Halt(1);
end;

Procedure StartSession;

Var
  Res : Integer;

begin
  EnvHandle:=nil;
  DBHandle:=nil;
  StmtHandle:=nil;
  Res:=SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHandle);
  if Res <> SQL_SUCCESS then
    DoError('Could allocate ODBC handle',Res);
  Res:=SQLSetEnvAttr(EnvHandle,SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), 0);
  If Not ODBCSuccess(res) then
    DoError('Could not set environment',Res);
  Res:=SQLAllocHandle(SQL_HANDLE_DBC, envHandle, DBHandle);
  If res<>SQL_SUCCESS then
    DoError('Could not create database handle',res);
  Res:=SQLConnect(DBHandle,PSQLCHAR(DBDSN),SQL_NTS,
                        PSQLChar(UserName),SQL_NTS,
                        PSQLCHAR(Password),SQL_NTS);
  If Not OdbcSuccess(res) then
    DoError('Could not connect to datasource.',Res);
end;

Procedure ExecuteStatement;

Var
  Res,ErrCode : LongInt;

begin
  Res:=SQLAllocHandle(SQL_HANDLE_STMT,DBHandle,stmtHandle);
  If not ODBCSuccess(res) then
    DoError('Could not allocate statement handle.',Res);
  { Bind result buffers.
    Note that for many queries, the result is not known on beforehand,
    And must be queried with SQLPrepare, SQLNumResulCols and SQLDescribeCol
    before the statement is executed.}
  SQLBindCol(stmtHandle,1,SQL_INTEGER,SQLPointer(@ResID),4,@ErrCode);
  SQLBindCol(stmtHandle,2,SQL_CHAR,SQLPointer(@ResName),256,@ErrCode);
  SQLBindCol(stmtHandle,3,SQL_CHAR,SQLPointer(@ResEmail),256,@ErrCode);
  // Now actually do it.
  Res:=SQLExecDirect(StmtHandle,Query,SQL_NTS);
  if not ODBCSuccess(res) then
    DoError('Execute of statement failed.',Res);
end;

Procedure ShowResult;

Var
  Count,Res : Longint;

begin
  Res:=SQLFetch(StmtHandle);
  Count:=0;
  While Res<>SQL_NO_DATA do
    begin
    Inc(Count);
    Write('Record: ',Count,' : ');
    Writeln(ResId,' ',PChar(@ResName[0]),' ',Pchar(@ResEmail[0]));
    Res:=SQLFetch(StmtHandle);
    end;
end;

begin
  StartSession;
  ExecuteStatement;
  ShowResult;
  FreeHandles;
end.
