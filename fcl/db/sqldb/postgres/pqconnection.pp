unit pqconnection;

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils, sqldb, db, dbconst,
{$IfDef LinkDynamically}
  postgres3dyn;
{$Else}
  postgres3;
{$EndIf}

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
    Nr        : string;
  end;

  TPQConnection = class (TSQLConnection)
  private
    FCursorCount         : word;
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
    function Fetch(cursor : TSQLHandle) : boolean; override;
    function LoadField(cursor : TSQLHandle;FieldDef : TfieldDef;buffer : pointer) : boolean; override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    function StartdbTransaction(trans : TSQLHandle) : boolean; override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    procedure UpdateIndexDefs(var IndexDefs : TIndexDefs;TableName : string); override;

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
  
const Oid_Bool     = 16;
      Oid_Text     = 25;
      Oid_Name     = 19;
      Oid_Int8     = 20;
      Oid_int2     = 21;
      Oid_Int4     = 23;
      Oid_Float4   = 700;
      Oid_Float8   = 701;
      Oid_bpchar   = 1042;
      Oid_varchar  = 1043;
      Oid_timestamp = 1114;
      oid_date      = 1082;
      oid_time      = 1083;
      oid_numeric   = 1700;


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
{$IfDef LinkDynamically}
  InitialisePostgres3;
{$EndIf}

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
{$IfDef LinkDynamically}
  ReleasePostgres3;
{$EndIf}

end;

function TPQConnection.TranslateFldType(Type_Oid : integer) : TFieldType;

begin
  case Type_Oid of
    Oid_varchar,Oid_bpchar,
    Oid_name               : Result := ftstring;
    Oid_text               : REsult := ftmemo;
    Oid_int8               : Result := ftLargeInt;
    Oid_int4               : Result := ftInteger;
    Oid_int2               : Result := ftSmallInt;
    Oid_Float4             : Result := ftFloat;
    Oid_Float8             : Result := ftFloat;
    Oid_TimeStamp          : Result := ftDateTime;
    Oid_Date               : Result := ftDate;
    Oid_Time               : Result := ftTime;
    Oid_Bool               : Result := ftBoolean;
    Oid_Numeric            : Result := ftBCD;
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
      begin
      nr := inttostr(FCursorcount);
      statement := 'DECLARE slctst' + name + nr +' BINARY CURSOR FOR ' + statement;
      inc(FCursorcount);
      end;
    end;
end;

procedure TPQConnection.FreeStatement(cursor : TSQLHandle);

begin
  with cursor as TPQCursor do
   if (PQresultStatus(res) <> PGRES_FATAL_ERROR) then //Don't try to do anything if the transaction has already encountered an error.
    begin
    if StatementType = stselect then
      begin
      Res := pqexec(tr,pchar('CLOSE slctst' + name + nr));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
        pqclear(res);
        DatabaseError(SErrClearSelection + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self)
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
      DatabaseError(SErrExecuteFailed + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self);
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
    st := pchar('FETCH 0 IN slctst' + name+nr);
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

      if (fieldtype = ftstring) and (size = -1) then
        size := pqfmod(baseres,i)-3;
      if fieldtype = ftdate  then
        size := sizeof(double);

      TFieldDef.Create(FieldDefs, PQfname(BaseRes, i), fieldtype,size, False, (i + 1));
      end;
    end;
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
    st := pchar('FETCH NEXT IN slctst' + name+nr);
    Res := pqexec(tr,pchar(st));
    if (PQresultStatus(res) <> PGRES_TUPLES_OK) then
      begin
      pqclear(Res);
      DatabaseError(SErrfetchFailed + ' (PostgreSQL: ' + PQerrorMessage(tr) + ')',self)
      end;
    Result := (PQntuples(res)<>0);
    end;
end;

function TPQConnection.LoadField(cursor : TSQLHandle;FieldDef : TfieldDef;buffer : pointer) : boolean;

var
  x,i          : integer;
  li           : Longint;
  CurrBuff     : pchar;
  tel  : byte;
  dbl  : pdouble;
  

begin
  with cursor as TPQCursor do
    begin
    for x := 0 to PQnfields(res)-1 do
      if PQfname(Res, x) = FieldDef.Name then break;

    if PQfname(Res, x) <> FieldDef.Name then
      DatabaseErrorFmt(SFieldNotFound,[FieldDef.Name],self);

    if pqgetisnull(res,0,x)=1 then
      result := false
    else
      begin
      i := PQfsize(res, x);
      CurrBuff := pqgetvalue(res,0,x);

      case FieldDef.DataType of
        ftInteger, ftSmallint, ftLargeInt,ftfloat :
          begin
          for tel := 1 to i do   // postgres returns big-endian numbers
            pchar(Buffer)[tel-1] := CurrBuff[i-tel];
          end;
        ftString  :
          begin
          li := pqgetlength(res,0,x);
          Move(CurrBuff^, Buffer^, li);
          pchar(Buffer + li)^ := #0;
          i := pqfmod(res,x)-3;
          end;
        ftdate :
          begin
          li := 0;
          for tel := 1 to i do   // postgres returns big-endian numbers
            pchar(@li)[tel-1] := CurrBuff[i-tel];
//          double(buffer^) := x + 36526; This doesn't work, please tell me what is wrong with it?
          dbl := pointer(buffer);
          dbl^ := li + 36526;
          i := sizeof(double);
          end;
        ftDateTime, fttime :
          begin
          dbl := pointer(buffer);
          dbl^ := 0;
          for tel := 1 to i do   // postgres returns big-endian numbers
            pchar(Buffer)[tel-1] := CurrBuff[i-tel];

          dbl^ := (dbl^+3.1558464E+009)/86400;  // postgres counts seconds elapsed since 1-1-2000
          end;
        ftBCD:
          begin
          // not implemented
          end;
        ftBoolean:
          pchar(buffer)[0] := CurrBuff[0]
      end;
      result := true;
      end;
    end;
end;

procedure TPQConnection.UpdateIndexDefs(var IndexDefs : TIndexDefs;TableName : string);

var qry : TSQLQuery;

begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  qry := tsqlquery.Create(nil);
  qry.transaction := Transaction;
  qry.database := Self;
  with qry do
    begin
    ReadOnly := True;
    sql.clear;

    sql.add('select '+
              'ic.relname as indexname,  '+
              'tc.relname as tablename, '+
              'ia.attname, '+
              'i.indisprimary, '+
              'i.indisunique '+
            'from '+
              'pg_attribute ta, '+
              'pg_attribute ia, '+
              'pg_class tc, '+
              'pg_class ic, '+
              'pg_index i '+
            'where '+
              '(i.indrelid = tc.oid) and '+
              '(ta.attrelid = tc.oid) and '+
              '(ia.attrelid = i.indexrelid) and '+
              '(ic.oid = i.indexrelid) and '+
              '(ta.attnum = i.indkey[ia.attnum-1]) and '+
              '(upper(tc.relname)=''' +  UpperCase(TableName) +''') '+
            'order by '+
              'ic.relname;');
    open;
    end;

  while not qry.eof do with IndexDefs.AddIndexDef do
    begin
    Name := trim(qry.fields[0].asstring);
    Fields := trim(qry.Fields[2].asstring);
    If qry.fields[3].asboolean then options := options + [ixPrimary];
    If qry.fields[4].asboolean then options := options + [ixUnique];
    qry.next;
    while (name = qry.fields[0].asstring) and (not qry.eof) do
      begin
      Fields := Fields + ';' + trim(qry.Fields[2].asstring);
      qry.next;
      end;
    end;
  qry.close;
  qry.free;
end;


end.

