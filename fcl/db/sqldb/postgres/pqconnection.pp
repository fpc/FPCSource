unit pqconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db,postgres3;
  
type
  TPQTrans = Class(TSQLHandle)
    protected
    TransactionHandle   : PPGConn;
  end;

  TPQCursor = Class(TSQLHandle)
    protected
    Statement : string;
    tr        : Pointer;
    nFields   : integer;
    res       : PPGresult;
    BaseRes   : PPGresult;
  end;

  TPQConnection = class (TSQLConnection)
  private
    FConnectString       : string;
    FSQLDatabaseHandle   : pointer;
    function TranslateFldType(Type_Oid : integer) : TFieldType;
  protected
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    Function AllocateCursorHandle : TSQLHandle; override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure FreeStatement(cursor : TSQLHandle); override;
    procedure PrepareStatement(cursor: TSQLHandle;ATransaction : TSQLTransaction;buf : string); override;
    procedure FreeFldBuffers(cursor : TSQLHandle); override;
    procedure Execute(cursor: TSQLHandle;atransaction:tSQLtransaction); override;
    procedure AddFieldDefs(cursor: TSQLHandle; FieldDefs : TfieldDefs); override;
    function GetFieldSizes(cursor : TSQLHandle) : integer; override;
    function Fetch(cursor : TSQLHandle) : boolean; override;
    procedure LoadFieldsFromBuffer(cursor : TSQLHandle;buffer: pchar); override;
    function GetFieldData(Cursor : TSQLHandle;Field: TField; FieldDefs : TfieldDefs; Buffer: Pointer;currbuff : pchar): Boolean; override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    function StartdbTransaction(trans : TSQLHandle) : boolean; override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
  published
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

implementation

ResourceString
  SErrRollbackFailed = 'Rollback transaction failed';
  SErrCommitFailed = 'Commit transaction failed';
  SErrConnectionFailed = 'Connection to database failed';
  SErrTransactionFailed = 'Start of transacion failed';
  SErrClearSelection = 'Clear of selection failed';
  SErrExecuteFailed = 'Execution of query failed';
  SErrFieldDefsFailed = 'Can not extract field information from query';
  SErrFetchFailed = 'Fetch of data failed';
  SErrNoDatabaseName = 'Database connect string (DatabaseName) not filled in!';
  
const Oid_Text    = 25;
      Oid_Int8    = 20;
      Oid_int2    = 21;
      Oid_Int4    = 23;
      Oid_Float4  = 700;
      Oid_Float8  = 701;
      Oid_bpchar  = 1042;
      Oid_varchar = 1043;

function TPQConnection.GetTransactionHandle(trans : TSQLHandle): pointer;
begin
  Result := (trans as TPQtrans).TransactionHandle;
end;

function TPQConnection.RollBack(trans : TSQLHandle) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  result := false;

  tr := trans as TPQTrans;

  res := PQexec(tr.TransactionHandle, 'ROLLBACK');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    result := false;
    DatabaseError(SErrRollbackFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.transactionhandle) + ')',self);
    end
  else
    begin
    PQclear(res);
    PQFinish(tr.TransactionHandle);
    result := true;
    end;
end;

function TPQConnection.Commit(trans : TSQLHandle) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  result := false;

  tr := trans as TPQTrans;

  res := PQexec(tr.TransactionHandle, 'COMMIT');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    result := false;
    DatabaseError(SErrCommitFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.transactionhandle) + ')',self);
    end
  else
    begin
    PQclear(res);
    PQFinish(tr.TransactionHandle);
    result := true;
    end;
end;

function TPQConnection.StartdbTransaction(trans : TSQLHandle) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
  msg : string;
begin
  result := false;

  tr := trans as TPQTrans;

  tr.TransactionHandle := PQconnectdb(pchar(FConnectString));

  if (PQstatus(tr.TransactionHandle) = CONNECTION_BAD) then
    begin
    result := false;
    PQFinish(tr.TransactionHandle);
    DatabaseError(SErrConnectionFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.transactionhandle) + ')',self);
    end
  else
    begin
    res := PQexec(tr.TransactionHandle, 'BEGIN');
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      result := false;
      PQclear(res);
      msg := PQerrorMessage(tr.transactionhandle);
      PQFinish(tr.TransactionHandle);
      DatabaseError(sErrTransactionFailed + ' (PostgreSQL: ' + msg + ')',self);
      end
    else
      begin
      PQclear(res);
      result := true;
      end;
    end;
end;

procedure TPQConnection.RollBackRetaining(trans : TSQLHandle);
var
  res : PPGresult;
  tr  : TPQTrans;
  msg : string;
begin
  tr := trans as TPQTrans;
  res := PQexec(tr.TransactionHandle, 'ROLLBACK');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    DatabaseError(SErrRollbackFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.transactionhandle) + ')',self);
    end
  else
    begin
    PQclear(res);
    res := PQexec(tr.TransactionHandle, 'BEGIN');
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      PQclear(res);
      msg := PQerrorMessage(tr.transactionhandle);
      PQFinish(tr.TransactionHandle);
      DatabaseError(sErrTransactionFailed + ' (PostgreSQL: ' + msg + ')',self);
      end
    else
      PQclear(res);
    end;
end;

procedure TPQConnection.CommitRetaining(trans : TSQLHandle);
var
  res : PPGresult;
  tr  : TPQTrans;
  msg : string;
begin
  tr := trans as TPQTrans;
  res := PQexec(tr.TransactionHandle, 'COMMIT');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    DatabaseError(SErrCommitFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.transactionhandle) + ')',self);
    end
  else
    begin
    PQclear(res);
    res := PQexec(tr.TransactionHandle, 'BEGIN');
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      PQclear(res);
      msg := PQerrorMessage(tr.transactionhandle);
      PQFinish(tr.TransactionHandle);
      DatabaseError(sErrTransactionFailed + ' (PostgreSQL: ' + msg + ')',self);
      end
    else
      PQclear(res);
    end;
end;


procedure TPQConnection.DoInternalConnect;

var msg : string;

begin
  inherited dointernalconnect;

  if (DatabaseName = '') then
    DatabaseError(SErrNoDatabaseName,self);

  FConnectString := '';
  if (UserName <> '') then FConnectString := FConnectString + ' user=''' + UserName + '''';
  if (Password <> '') then FConnectString := FConnectString + ' password=''' + Password + '''';
  if (DatabaseName <> '') then FConnectString := FConnectString + ' dbname=''' + DatabaseName + '''';

  FSQLDatabaseHandle := PQconnectdb(pchar(FConnectString));

  if (PQstatus(FSQLDatabaseHandle) = CONNECTION_BAD) then
    begin
    msg := PQerrorMessage(FSQLDatabaseHandle);
    dointernaldisconnect;
    DatabaseError(sErrConnectionFailed + ' (PostgreSQL: ' + msg + ')',self);
    end;
end;

procedure TPQConnection.DoInternalDisconnect;
begin
  PQfinish(FSQLDatabaseHandle);
end;

function TPQConnection.TranslateFldType(Type_Oid : integer) : TFieldType;

begin
  case Type_Oid of
    Oid_varchar,Oid_bpchar : Result := ftstring;
    Oid_text               : REsult := ftmemo;
    Oid_int8               : Result := ftLargeInt;
    Oid_int4               : Result := ftInteger;
    Oid_int2               : Result := ftSmallInt;
    Oid_Float4             : Result := ftFloat;
    Oid_Float8             : Result := ftFloat;
  end;
end;

Function TPQConnection.AllocateCursorHandle : TSQLHandle;

begin
  result := TPQCursor.create;
end;

Function TPQConnection.AllocateTransactionHandle : TSQLHandle;

begin
  result := TPQTrans.create;
end;

procedure TPQConnection.PrepareStatement(cursor: TSQLHandle;ATransaction : TSQLTransaction;buf : string);

begin
  with (cursor as TPQCursor) do
    begin
    (cursor as TPQCursor).statement := buf;
    if StatementType = stselect then
      statement := 'DECLARE selectst' + name + '  BINARY CURSOR FOR ' + statement;
    end;
end;

procedure TPQConnection.FreeStatement(cursor : TSQLHandle);

begin
  with cursor as TPQCursor do
    begin
    if StatementType = stselect then
      begin
      Res := pqexec(tr,pchar('CLOSE selectst' + name));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
        pqclear(res);
        DatabaseError(SErrClearSelection + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self);
        end
      end;
    pqclear(baseres);
    pqclear(res);
    end;
end;

procedure TPQConnection.FreeFldBuffers(cursor : TSQLHandle);

begin
// Do nothing
end;

procedure TPQConnection.Execute(cursor: TSQLHandle;atransaction:tSQLtransaction);

var st : string;

begin
  with cursor as TPQCursor do
    begin
    tr := aTransaction.Handle;
//    res := pqexecParams(tr,pchar(statement),0,nil,nil,nil,nil,1);
    st := statement;
    res := pqexec(tr,pchar(st));
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      pqclear(res);
      DatabaseError(SErrExecuteFailed + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self)
      end;
    end;
end;

procedure TPQConnection.AddFieldDefs(cursor: TSQLHandle; FieldDefs : TfieldDefs);
var
  i         : integer;
  size      : integer;
  st        : string;
  fieldtype : tfieldtype;

begin
  with cursor as TPQCursor do
    begin
//    BaseRes := pqexecParams(tr,'FETCH 0 IN selectst' + pchar(name) ,0,nil,nil,nil,nil,1);
    st := 'FETCH 0 IN selectst' + pchar(name);
    BaseRes := pqexec(tr,pchar(st));
    if (PQresultStatus(BaseRes) <> PGRES_TUPLES_OK) then
      begin
      pqclear(BaseRes);
      DatabaseError(SErrFieldDefsFailed + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self)
      end;
    nFields := PQnfields(BaseRes);
    for i := 0 to nFields-1 do
      begin
      size := PQfsize(BaseRes, i);
      fieldtype := TranslateFldType(PQftype(BaseRes, i));

      if fieldtype = ftstring  then
        size := pqfmod(baseres,i)-4;

      TFieldDef.Create(FieldDefs, PQfname(BaseRes, i), fieldtype,size, False, (i + 1));
      end;
    end;
end;

function TPQConnection.GetFieldSizes(cursor : TSQLHandle) : integer;
var
  x,recsize : integer;
  size      : integer;
begin
  recsize := 0;
  {$R-}
  with cursor as TPQCursor do
    for x := 0 to PQnfields(baseres)-1 do
      begin
      size := PQfsize(baseres, x);
      if TranslateFldType(PQftype(BaseRes, x)) = ftString then
        size := pqfmod(baseres,x);
        
      if size = -1 then size := sizeof(pchar);
      Inc(recsize, size);
      end;
  {$R+}
  result := recsize;
end;

function TPQConnection.GetHandle: pointer;
begin
  Result := FSQLDatabaseHandle;
end;

function TPQConnection.Fetch(cursor : TSQLHandle) : boolean;

var st : string;

begin
  with cursor as TPQCursor do
    begin
    st := 'FETCH NEXT IN selectst' + pchar(name);
    Res := pqexec(tr,pchar(st));
    if (PQresultStatus(res) <> PGRES_TUPLES_OK) then
      begin
      pqclear(Res);
      DatabaseError(SErrfetchFailed + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self)
      end;
    Result := (PQntuples(res)<>0);
    end;
end;

procedure TPQConnection.LoadFieldsFromBuffer(cursor : TSQLHandle;buffer : pchar);
var
  x,i          : integer;

begin
  {$R-}
  with cursor as TPQCursor do for x := 0 to PQnfields(res)-1 do
    begin
    i := PQfsize(res, x);
    buffer[0] := chr(pqgetisnull(res,0,x));
    inc(buffer);

    if  i = -1 then
      begin
      i := pqgetlength(res,0,x);
      move(i,buffer^,sizeof(integer));
      inc(buffer,sizeof(integer));
      
      Move(pqgetvalue(res,0,x)^,Buffer^, i);

      inc(buffer,i);
      end
    else
      begin
      Move(pqgetvalue(res,0,x)^, Buffer^, i);
      Inc(Buffer, i);
      end;
    end;
  {$R+}
end;

function TPQConnection.GetFieldData(Cursor : TSQLHandle;Field: TField; FieldDefs : TfieldDefs; Buffer: Pointer;currbuff : pchar): Boolean;
var
  x    : longint;
  size : integer;
  tel  : byte;

begin
  Result := False;
  with cursor as TPQCursor do
    begin
    for x := 0 to Field.Fieldno-1 do
      begin
      size := PQfsize(BaseRes, x);
      inc(currbuff);
      if size = -1 then
        begin
        size := integer(CurrBuff^);
        inc(CurrBuff,sizeof(integer));
        end;
      if x < Field.Fieldno-1 then
        Inc(CurrBuff, size);
      end;

    dec(currbuff);
    if currbuff[0]<>#1 then
      begin
      inc(currbuff);
      case Field.DataType of
        ftInteger, ftSmallint, ftLargeInt,ftfloat :
          begin
          for tel := 1 to size do   // postgres returns big-endian integers
            pchar(Buffer)[tel-1] := CurrBuff[size-tel];
          end;
        ftString  :
          begin
            Move(CurrBuff^, Buffer^, size);
            PChar(Buffer + Size)^ := #0;
          end;
      end;
      Result := True;
      end
    end;
end;


end.

