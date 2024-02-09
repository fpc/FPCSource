unit PQConnection;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    Postgres database connection component

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}{$H+}

{$Define LinkDynamically}
{ $define PQDEBUG}

interface

uses
  Classes, SysUtils, sqldb, db, dbconst,bufdataset,
{$IfDef LinkDynamically}
  postgres3dyn;
{$Else}
  postgres3;
{$EndIf}

type
  TPQCursor = Class;
  TPQConnection = Class;

  { TPGHandle }
  TCheckResultAction = (craClose,craClear);
  TCheckResultActions = set of TCheckResultAction;

  TPGHandle = Class(TSQLHandle)
  strict private

    class var _HID : {$IFDEF CPU64} Int64 {$ELSE} Integer {$ENDIF};
  private
    FHandleID : {$IFDEF CPU64} Int64 {$ELSE} Integer {$ENDIF};
    FConnected: Boolean;
    FCOnnection: TPQConnection;
    FDBName : String;
    FActive : Boolean;
    FUsed: Boolean;
    function GetConnected: Boolean;
  protected
    FNativeConn: PPGConn;
    FCursorList  : TThreadList;
    Procedure RegisterCursor(Cursor : TPQCursor);
    Procedure UnRegisterCursor(Cursor : TPQCursor);
    procedure UnprepareStatement(Cursor: TPQCursor; Force: Boolean);
  Public
    Constructor Create(aConnection : TPQConnection;aDBName :string);
    Destructor Destroy; override;
    Procedure Connect;
    Procedure Disconnect;
    Procedure StartTransaction;
    Procedure RollBack;
    Procedure Commit;
    Procedure Reset;
    Function CheckConnectionStatus(doRaise : Boolean = True) : Boolean;
    Function DescribePrepared(StmtName : String): PPGresult;
    Function Exec(aSQL : String; aClearResult : Boolean; aError : String = '') : PPGresult;
    function ExecPrepared(stmtName: AnsiString; nParams:longint; paramValues:PPchar; paramLengths:Plongint;paramFormats:Plongint; aClearResult : Boolean) : PPGresult;
    procedure CheckResultError(var res: PPGresult; Actions : TCheckResultActions; const ErrMsg: string);
    Property Connection : TPQConnection Read FCOnnection;
    Property NativeConn : PPGConn Read FNativeConn;
    Property Active : Boolean Read Factive;
    Property Used : Boolean Read FUsed Write FUsed;
    Property Connected : Boolean Read GetConnected;
  end;

  // TField and TFieldDef only support a limited amount of fields.
  // TFieldBinding and TExtendedFieldType can be used to map PQ types
  // on standard fields and retain mapping info.
  TExtendedFieldType = (eftNone,eftEnum,eftCitext);

  TFieldBinding = record
    FieldDef : TSQLDBFieldDef; // FieldDef this is associated with
    Index : Integer; // Tuple index
    TypeOID : oid; // Filled with type OID if it is not standard.
    TypeName : String; // Filled with type name by GetExtendedFieldInfo
    ExtendedFieldType: TExtendedFieldType; //
  end;
  PFieldBinding = ^TFieldBinding;
  TFieldBindings = Array of TFieldBinding;

  { TPQTransactionHandle }

  TPQTransactionHandle = Class(TSQLHandle)
  strict private
    FHandle: TPGHandle;
    procedure SetHandle(AValue: TPGHandle);
  Public
    Property Handle : TPGHandle Read FHandle Write SetHandle;
  end;

  { TPQCursor }

  TPQCursor = Class(TSQLCursor)
  private
    procedure SetHandle(AValue: TPGhandle);
  protected
    Statement    : string;
    StmtName     : string;
    Fhandle       : TPGHandle;
    res          : PPGresult;
    CurTuple     : integer;
    FieldBinding : TFieldBindings;
    Function GetFieldBinding(F : TFieldDef): PFieldBinding;
   Public
    Destructor Destroy; override;
    Property Handle : TPGhandle Read FHandle Write SetHandle;
  end;

  { EPQDatabaseError }

  EPQDatabaseError = class(EDatabaseError)
    public
      SEVERITY:string;
      SQLSTATE: string;
      MESSAGE_PRIMARY:string;
      MESSAGE_DETAIL:string;
      MESSAGE_HINT:string;
      STATEMENT_POSITION:string;
      SCHEMA_NAME: string;
      TABLE_NAME: string;
      COLUMN_NAME: string;
      DATATYPE_NAME: string;
      CONSTRAINT_NAME: string;
  end;


  { TPQConnection }

  TPQConnection = class (TSQLConnection)
  private
    FHandlePool      : TThreadList;
    FCursorCount         : dword;
    FIntegerDateTimes    : boolean;
    FVerboseErrors       : Boolean;
  protected
    // Protected so they can be used by descendents.
   function GetConnectionString(const aDBName : String) : string;
    function TranslateFldType(res : PPGresult; Tuple : integer; out Size : integer; Out ATypeOID : oid) : TFieldType;
    procedure ExecuteDirectPG(const Query : String);
    Procedure GetExtendedFieldInfo(cursor: TPQCursor; Bindings : TFieldBindings);

    procedure ApplyFieldUpdate(C : TSQLCursor; P: TSQLDBParam; F: TField; UseOldValue: Boolean); override;
    Function ErrorOnUnknownType : Boolean;
    // Add connection to pool.
    procedure AddHandle(T: TPGHandle);
    function PortParamName: string; override;
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    Function AllocateCursorHandle : TSQLCursor; override;
    Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams); override;
    procedure Execute(cursor: TSQLCursor;atransaction:tSQLtransaction; AParams : TParams); override;
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs); override;
    function Fetch(cursor : TSQLCursor) : boolean; override;
    procedure UnPrepareStatement(cursor : TSQLCursor); override;
    function LoadField(cursor : TSQLCursor;FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean; override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    function StartImplicitTransaction(trans : TSQLHandle; AParams : string) : boolean; override;
    function StartDBTransaction(trans : TSQLHandle; AParams : string) : boolean; override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    procedure UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string); override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor;ATransaction : TSQLTransaction); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
    function GetNextValueSQL(const SequenceName: string; IncrementBy: Integer): string; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetConnectionInfo(InfoType:TConnInfoType): string; override;
    procedure CreateDB; override;
    procedure DropDB; override;
  published
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
    Property VerboseErrors : Boolean Read FVerboseErrors Write FVerboseErrors default true;
  end;

  { TPQConnectionDef }

  TPQConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
    Class Function DefaultLibraryName : String; override;
    Class Function LoadFunction : TLibraryLoadFunction; override;
    Class Function UnLoadFunction : TLibraryUnLoadFunction; override;
    Class Function LoadedLibraryName: string; override;
  end;

implementation

uses math, strutils, FmtBCD;

ResourceString
  SErrRollbackFailed = 'Rollback transaction failed';
  SErrCommitFailed = 'Commit transaction failed';
  SErrConnectionFailed = 'Connection to database failed';
  SErrTransactionFailed = 'Start of transacion failed';
  SErrExecuteFailed = 'Execution of query failed';
  SErrPrepareFailed = 'Preparation of query failed.';
  SErrUnPrepareFailed = 'Unpreparation of query failed.';

const Oid_Bool     = 16;
      Oid_Bytea    = 17;
      Oid_char     = 18;
      Oid_Text     = 25;
      Oid_Oid      = 26;
      Oid_Name     = 19;
      Oid_Int8     = 20;
      Oid_int2     = 21;
      Oid_Int4     = 23;
      Oid_JSON     = 114;
      Oid_Float4   = 700;
      Oid_Money    = 790;
      Oid_Float8   = 701;
      Oid_Unknown  = 705;
      Oid_MacAddr  = 829;
      Oid_Inet     = 869;
      Oid_bpchar   = 1042;
      Oid_varchar  = 1043;
      oid_date      = 1082;
      oid_time      = 1083;
      Oid_timeTZ    = 1266;
      Oid_timestamp = 1114;
      Oid_timestampTZ = 1184;
      Oid_interval  = 1186;
      oid_numeric   = 1700;
      Oid_uuid      = 2950;

{ TPQTransactionHandle }

procedure TPQTransactionHandle.SetHandle(AValue: TPGHandle);
begin
  if FHandle=AValue then Exit;
  FHandle:=AValue;
end;


{ TPGHandle }

constructor TPGHandle.Create(aConnection: TPQConnection; aDBName: string);
begin
  FDBName:=aDBName;
  FConnection:=aConnection;
  FCursorList:=TThreadList.Create;
  FCursorList.Duplicates:=dupIgnore;
  {$IFDEF CPU64}
  FHandleID:=InterlockedIncrement64(_HID);
  {$ElSE}
  FHandleID:=InterlockedIncrement(_HID);
  {$ENDIF}
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  allocating handle ');
  {$ENDIF}
end;

destructor TPGHandle.Destroy;

Var
  L : TList;
  I : integer;

begin
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  Destroying handle ');
  {$ENDIF}
  L:=FCursorList.LockList;
  try
    For I:=0 to L.Count-1 do
      TPQCursor(L[i]).Handle:=Nil;
  finally
    FCursorList.UnlockList;
  end;
  FreeAndNil(FCursorList);
  inherited Destroy;
end;

function TPGHandle.GetConnected: Boolean;
begin
  Result:=FNativeConn<>Nil;
end;

procedure TPGHandle.RegisterCursor(Cursor: TPQCursor);
begin
  if Cursor.handle=Self then
    begin
    {$IFDEF PQDEBUG}
    Writeln('>>> ',FHandleID, ' [',TThread.CurrentThread.ThreadID, '] cursor ',PtrInt(Cursor),' already registered');
    {$ENDIF}
    exit;
    end;
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  registering cursor ',PtrInt(Cursor));
  {$ENDIF}
  FCursorList.Add(Cursor);
  Cursor.Handle:=Self;
end;

procedure TPGHandle.UnRegisterCursor(Cursor: TPQCursor);

Var
  L : TList;

begin
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  unregistering cursor ',PtrInt(Cursor));
  {$ENDIF}
  Cursor.Handle:=Nil;
  FCursorList.Remove(Cursor);
  L:=FCursorList.LockList;
  try
    Used:=L.Count>0;
    {$IFDEF PQDEBUG}
    Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  unregistering cursor ',PtrInt(Cursor),'. Handle still used: ',Used);
    {$ENDIF}
  finally
    FCursorList.UnlockList;
  end;
end;

procedure TPGHandle.UnprepareStatement(Cursor: TPQCursor; Force : Boolean);
var
  SQL : String;
begin
  if Cursor.handle<>Self then
    DatabaseError('Internal error: unpreparing in different transaction!');
  if Assigned(Cursor.res) then
    PQclear(Cursor.res);
  Cursor.res:=nil;
  SQL:='deallocate '+Cursor.StmtName;
  Cursor.StmtName:='';
  if Force then
    Cursor.FPrepared := False;
  if not Cursor.FPrepared then
    Exit;
  if (PQtransactionStatus(FNativeConn) <> PQTRANS_INERROR) then
    begin
    Exec(SQL,True,SErrUnPrepareFailed);
    Cursor.FPrepared := False;
    end;
  UnregisterCursor(Cursor);
end;

procedure TPGHandle.Connect;

var
  S : AnsiString;
begin
  S:=Connection.GetConnectionString(FDBName);
  FNativeConn:=PQconnectdb(PAnsiChar(S));
  CheckConnectionStatus;
  FConnected:=True;
  S:=Connection.CharSet;
  if (S<>'') then
    PQsetClientEncoding(FNativeConn,PAnsiChar(S));
end;


{ TPQCursor }

destructor TPQCursor.Destroy;
begin
  if Assigned(Handle) then
    Handle.UnRegisterCursor(Self);
  inherited Destroy;
end;

procedure TPQCursor.SetHandle(AValue: TPGhandle);
begin
  if FHandle=AValue then Exit;
  if (FHandle<>Nil) and (aValue<>Nil) then
    begin
    {$IFDEF PQDEBUG}
    writeln('>>> ',ptrint(Self),' [',TThread.CurrentThread.ThreadID, '] Setting handle while handle still valid');
    {$endif}
    end;
  FHandle:=AValue;
end;

function TPQCursor.GetFieldBinding(F: TFieldDef): PFieldBinding;

Var
  I : Integer;

begin
  Result:=Nil;
  if (F=Nil) then exit;
  // This is an optimization: it is so for 99% of cases (FieldNo-1=array index)
  if F is TSQLDBFieldDef then
    Result:=PFieldBinding(TSQLDBFieldDef(F).SQLDBData)
  else If (FieldBinding[F.FieldNo-1].FieldDef=F) then
    Result:=@FieldBinding[F.FieldNo-1]
  else
    begin
    I:=Length(FieldBinding)-1;
    While (I>=0) and (FieldBinding[i].FieldDef<>F) do
      Dec(I);
    if I>=0 then
      Result:=@FieldBinding[i];
    end;
end;


{ TPQConnection }

constructor TPQConnection.Create(AOwner : TComponent);

begin
  inherited;
  FConnOptions := FConnOptions + [sqSupportParams, sqSupportEmptyDatabaseName, sqEscapeRepeat, sqEscapeSlash, sqImplicitTransaction,sqSupportReturning,sqSequences] - [sqCommitEndsPrepared];
  FieldNameQuoteChars:=DoubleQuotes;
  VerboseErrors:=True;
  FHandlePool:=TThreadlist.Create;
end;

destructor TPQConnection.Destroy;
begin
  // We must disconnect here. If it is done in inherited, then connection pool is gone.
  Connected:=False;
  FreeAndNil(FHandlePool);
  inherited destroy;
end;

procedure TPQConnection.CreateDB;

begin
  ExecuteDirectPG('CREATE DATABASE ' +DatabaseName);
end;

procedure TPQConnection.DropDB;

begin
  ExecuteDirectPG('DROP DATABASE ' +DatabaseName);
end;

procedure TPQConnection.ExecuteDirectPG(const Query: String);

var
  AHandle : TPGHandle;

begin
  CheckDisConnected;
{$IfDef LinkDynamically}
  InitialisePostgres3;
{$EndIf}
  aHandle:=TPGHandle.Create(Self,'template1');
  try
    aHandle.Connect;
    aHandle.Exec(Query,True,'Error executing query');
  finally
    aHandle.Free;
  end;
{$IfDef LinkDynamically}
  ReleasePostgres3;
{$EndIf}
end;

procedure TPQConnection.GetExtendedFieldInfo(cursor: TPQCursor;
  Bindings: TFieldBindings);

Var
  tt,tc,Tn,S : String;
  I,J : Integer;
  Res : PPGResult;
  toid : oid;

begin
s:='';
  For I:=0 to Length(Bindings)-1 do
    if (Bindings[i].TypeOID>0) then
      begin
      if (S<>'') then
        S:=S+', ';
      S:=S+IntToStr(Bindings[i].TypeOID);
      end;
  if (S='') then
    exit;
  S:='select oid,typname,typtype,typcategory from pg_type where oid in ('+S+') order by oid';
  Res:=Cursor.Handle.Exec(S,False,'Error getting typeinfo');
  try
    For I:=0 to PQntuples(Res)-1 do
      begin
      toid:=Strtoint(pqgetvalue(Res,i,0));
      tn:=pqgetvalue(Res,i,1);
      tt:=pqgetvalue(Res,i,2);
      tc:=pqgetvalue(Res,i,3);
      J:=length(Bindings)-1;
      while (J>= 0) do
        begin
        if (Bindings[j].TypeOID=toid) then
          Case tt of
           'e':
            Bindings[j].ExtendedFieldType:=eftEnum;
           'citext':
            Bindings[j].ExtendedFieldType:=eftCitext;
          end;
        Dec(J);
        end;
      end;
  finally
    PQClear(Res);
  end;
end;

procedure TPQConnection.ApplyFieldUpdate(C : TSQLCursor; P: TSQLDBParam; F: TField;
  UseOldValue: Boolean);
begin
  inherited ApplyFieldUpdate(C,P, F, UseOldValue);
  if (C is TPQCursor) then
    P.SQLDBData:=TPQCursor(C).GetFieldBinding(F.FieldDef);
end;

function TPQConnection.ErrorOnUnknownType: Boolean;
begin
  Result:=False;
end;

procedure TPQConnection.AddHandle(T: TPGHandle);

begin
  FHandlePool.Add(T);
end;



function TPQConnection.GetTransactionHandle(trans : TSQLHandle): pointer;
begin
  if (trans is TPQTransactionHandle) then
    Result:=Trans
  else
    DatabaseErrorFmt('Expected %s, got %s',[TPQTransactionHandle.ClassName,Trans.ClassName]);
end;

function TPQConnection.RollBack(trans : TSQLHandle) : boolean;

var
  tr  : TPGHandle;

begin
  tr := (trans as TPQTransactionHandle).Handle as TPGHandle;
  TR.RollBack;
  result := true;
end;

function TPQConnection.Commit(trans : TSQLHandle) : boolean;
var
  tr  : TPGHandle;
begin
  tr := (trans as TPQTransactionHandle).Handle;
  tr.Commit;
  Result:=True;
end;

procedure TPQConnection.RollBackRetaining(trans : TSQLHandle);
var
  tr  : TPGHandle;
begin
  tr := (trans as TPQTransactionHandle).Handle;
  TR.RollBack;
  TR.StartTransaction;
end;

procedure TPQConnection.CommitRetaining(trans : TSQLHandle);
var
  tr  : TPGHandle;
begin
  tr := (trans as TPQTransactionHandle).Handle as TPGHandle;
  TR.Commit;
  TR.StartTransaction;
end;

function TPQConnection.StartImplicitTransaction(trans : TSQLHandle; AParams : string) : boolean;
var
  i : Integer;
  T : TPGHandle;
  L : TList;

begin
  //find an unused connection in the pool
  i:=0;
  T:=Nil;
  L:=FHandlePool.LockList;
  try
    while (i<L.Count) do
      begin
      T:=TPGHandle(L[i]);
      if Not (T.Connected and T.Used) then
        break
      else
        T:=Nil;
      i:=i+1;
      end;
    // set to active now, so when we exit critical section,
    // it will be marked active and will not be found.
    if Assigned(T) then
      T.Used:=true;
  finally
    FHandlePool.UnLockList;
  end;

  if (T=Nil) then
    begin
    T:=TPGHandle.Create(Self,DatabaseName);
    T.Used:=True;
    AddHandle(T);
    end
  else
    begin
    {$IFDEF PQDEBUG}
    Writeln('>>> ',T.FHandleID,' [',TThread.CurrentThread.ThreadID, '] Reusing connection ');
    {$ENDIF}

    end;

  if (Not T.Connected) then
    T.Connect;
  (Trans as TPQTransactionHandle).handle:=T;
  Result := true;
end;

function TPQConnection.StartDBTransaction(trans: TSQLHandle; AParams: string
  ): boolean;

Var
  tr  : TPQTransactionHandle;

begin
  Result:=StartImplicitTransaction(trans, AParams);
  if Result then
    begin
    tr:= trans as TPQTransactionHandle;
    tr.Handle.StartTransaction;
    end;
end;


procedure TPQConnection.DoInternalConnect;
var
  T : TPGHandle;

begin
{$IfDef LinkDynamically}
  InitialisePostgres3;
{$EndIf}
  inherited DoInternalConnect;
  T:=TPGHandle.Create(Self,DatabaseName);
  try
    T.Connect;
    T.Used:=false;
    // This only works for pg>=8.0, so timestamps won't work with earlier versions of pg which are compiled with integer_datetimes on
    if PQparameterStatus<>nil then
      FIntegerDateTimes := PQparameterStatus(T.NativeConn,'integer_datetimes') = 'on';
  except
    T.Free;
    DoInternalDisconnect;
    raise;
  end;
  AddHandle(T);
end;

procedure TPQConnection.DoInternalDisconnect;
var
  i:integer;
  L : TList;
  T : TPGHandle;

begin
  Inherited;
  L:=FHandlePool.LockList;
  try
    for i:=0 to L.Count-1 do
      begin
      T:=TPGHandle(L[i]);
      if T.Connected then
        T.Disconnect;
      T.Free;
      end;
    L.Clear;
  finally
    FHandlePool.UnLockList;
  end;
{$IfDef LinkDynamically}
  ReleasePostgres3;
{$EndIf}
end;

function TPQConnection.GetConnectionString(const aDBName : String): string;

  Procedure MaybeAdd(aName,aValue : String);

  begin
    if aValue<>'' then
      begin
      if aName<>'' then
        Result:=Result+' '+aName+'='''+aValue+''''
      else
        Result:=result+' '+aValue;
      end;
  end;

begin
  Result:='';
  MaybeAdd('user',UserName);
  MaybeAdd('password',Password);
  MaybeAdd('host',HostName);
  MaybeAdd('dbname',aDBName);
  MaybeAdd('',Params.Text);
end;

procedure TPGHandle.Disconnect;

Var
  PG : PPGconn;

begin
  if FNativeConn=Nil then
    DatabaseError('Not connected to Postgres Server');
  PG:=FNativeConn;
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, '] ,Finishing connection');
  {$ENDIF}
  FNativeConn:=Nil;
  PQFinish(PG);
end;

procedure TPGHandle.StartTransaction;
begin
  Exec('BEGIN',True,sErrTransactionFailed);
  FActive:=True;
end;

procedure TPGHandle.RollBack;

Var
  L : TList;
  I : Integer;
  C : TPQCursor;

begin
  if not Active then
    Exit;
  // unprepare statements associated with given transaction
  L:=FCursorList.LockList;
  try
    For I:=L.Count-1 downto 0 do
      begin
      C:=TPQCursor(L[i]);
      UnprepareStatement(C,False);
      end;
    L.Clear;
  finally
    FCursorList.UnlockList;
  end;
  FActive:=False;
  Exec('ROLLBACK',True,SErrRollbackFailed);
end;

procedure TPGHandle.Commit;

begin
  Exec('COMMIT',True,SErrCommitFailed);
end;

procedure TPGHandle.Reset;
begin
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, '] : Resetting');
  {$ENDIF}
  PQReset(FNativeConn);
end;

function TPGHandle.CheckConnectionStatus(doRaise: Boolean): Boolean;

var sErr: string;

begin
  Result:=False;
  if (PQstatus(FNativeConn) <> CONNECTION_BAD) then
    Exit(True);
  sErr := PQerrorMessage(FNativeConn);
  //make connection available in pool
  Disconnect;
  if DoRaise then
    DatabaseError(sErrConnectionFailed + ' (PostgreSQL: ' + sErr + ')');
end;

function TPGHandle.DescribePrepared(StmtName: String): PPGresult;

Var
  S : AnsiString;

begin
  S:=StmtName;
  Result:=PQdescribePrepared(FNativeConn,pchar(S));
end;

function TPGHandle.Exec(aSQL: String; aClearResult: Boolean; aError: String): PPGresult;

Var
  S : UTF8String;
  Acts : TCheckResultActions;

begin
  if FNativeConn=Nil then
    DatabaseError(IntToStr(FHandleID)+': No native PQ connection available');
  CheckConnectionStatus();
  S:=aSQL;
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  exec: ',S);
  {$ENDIF}
  Result:=PQexec(FNativeConn,PAnsiChar(S));
  acts:=[];
  if aClearResult then
    include(acts,craClear);
  CheckResultError(Result,acts,aError);
end;

function TPGHandle.ExecPrepared(stmtName: AnsiString; nParams: longint;
  paramValues: PPchar; paramLengths: Plongint; paramFormats: Plongint;
  aClearResult: Boolean): PPGresult;

var
  acts : TCheckResultActions;

begin
  {$IFDEF PQDEBUG}
  Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  executr prepared ',StmtName);
  {$ENDIF}
  Result:=PQexecPrepared(NativeConn,pansichar(StmtName),nParams,ParamValues,paramlengths,paramformats,1);
  acts:=[];
  if aClearResult then
    include(acts,craClear);
  CheckResultError(Result,acts,'Error executing prepared statement '+stmtName);
end;

procedure TPGHandle.CheckResultError(var res: PPGresult; Actions: TCheckResultActions; const ErrMsg: string);

  Procedure MaybeAdd(Var S : String; Prefix,Msg : String);

  begin
    if (Msg='') then
      exit;
    S:=S+LineEnding+Prefix+': '+Msg;
  end;

var
  E: EPQDatabaseError;
  sErr: string;
  CompName: string;
  SEVERITY: string;
  SQLSTATE: string;
  MESSAGE_PRIMARY: string;
  MESSAGE_DETAIL: string;
  MESSAGE_HINT: string;
  STATEMENT_POSITION: string;
  SCHEMA_NAME: string;
  TABLE_NAME: string;
  COLUMN_NAME: string;
  DATATYPE_NAME: string;
  CONSTRAINT_NAME: string;
  P : Pchar;
  haveError : Boolean;
  lMessage : String;

begin
  lMessage:=ErrMsg;
  HaveError:=False;
  if (Res=Nil) then
    begin
    {$IFDEF PQDEBUG}
    Writeln('>>> ',FHandleID,' [',TThread.CurrentThread.ThreadID, ']  nil result');
    {$ENDIF}
    HaveError:=True;
    P:=PQerrorMessage(FNativeConn);
    If Assigned(p) then
      lMessage:=lMessage+StrPas(P);
    Reset;
    end
  else if Not (PQresultStatus(res) in [PGRES_COMMAND_OK,PGRES_TUPLES_OK]) then
    begin
    HaveError:=True;
    SEVERITY:=PQresultErrorField(res,PG_DIAG_SEVERITY);
    SQLSTATE:=PQresultErrorField(res,PG_DIAG_SQLSTATE);
    MESSAGE_PRIMARY:=PQresultErrorField(res,PG_DIAG_MESSAGE_PRIMARY);
    MESSAGE_DETAIL:=PQresultErrorField(res,PG_DIAG_MESSAGE_DETAIL);
    MESSAGE_HINT:=PQresultErrorField(res,PG_DIAG_MESSAGE_HINT);
    STATEMENT_POSITION:=PQresultErrorField(res,PG_DIAG_STATEMENT_POSITION);
    SCHEMA_NAME:=PQresultErrorField(res,PG_DIAG_SCHEMA_NAME);
    TABLE_NAME:=PQresultErrorField(res,PG_DIAG_TABLE_NAME);
    COLUMN_NAME:=PQresultErrorField(res,PG_DIAG_COLUMN_NAME);
    DATATYPE_NAME:=PQresultErrorField(res,PG_DIAG_DATATYPE_NAME);
    CONSTRAINT_NAME:=PQresultErrorField(res,PG_DIAG_CONSTRAINT_NAME);
    sErr:=PQresultErrorMessage(res);
    if Connection.VerboseErrors then
      begin
      MaybeAdd(sErr,'Severity',SEVERITY);
      MaybeAdd(sErr,'SQL State',SQLSTATE);
      MaybeAdd(sErr,'Primary Error',MESSAGE_PRIMARY);
      MaybeAdd(sErr,'Error Detail',MESSAGE_DETAIL);
      MaybeAdd(sErr,'Hint',MESSAGE_HINT);
      MaybeAdd(sErr,'Character',STATEMENT_POSITION);
      MaybeAdd(sErr,'Schema',SCHEMA_NAME);
      MaybeAdd(sErr,'Table',TABLE_NAME);
      MaybeAdd(sErr,'Column',COLUMN_NAME);
      MaybeAdd(sErr,'Data Type',DATATYPE_NAME);
      MaybeAdd(sErr,'Constraint',CONSTRAINT_NAME);
      end;
    end;
  if HaveError then
    begin
    if Assigned(Connection) then
      CompName := Connection.Name;
    if CompName='' then
      CompName:=FDBName;
    E:=EPQDatabaseError.CreateFmt('%s : %s  (PostgreSQL: %s)', [CompName, lMessage, sErr]);
    E.SEVERITY:=SEVERITY;
    E.SQLSTATE:=SQLSTATE;
    E.MESSAGE_PRIMARY:=MESSAGE_PRIMARY;
    E.MESSAGE_DETAIL:=MESSAGE_DETAIL;
    E.MESSAGE_HINT:=MESSAGE_HINT;
    E.STATEMENT_POSITION:=STATEMENT_POSITION;
    E.SCHEMA_NAME:=SCHEMA_NAME;
    E.TABLE_NAME:=TABLE_NAME;
    E.COLUMN_NAME:=COLUMN_NAME;
    E.DATATYPE_NAME:=DATATYPE_NAME;
    E.CONSTRAINT_NAME:=CONSTRAINT_NAME;

    PQclear(res);
    res:=nil;
    if craClose in Actions then
      Disconnect;
    {$IFDEF PQDEBUG}
    Writeln('>>> ',IntToStr(FHandleID)+' [',TThread.CurrentThread.ThreadID, ']  Error: ',lMessage,' - ',Serr);
    {$ENDIF}
    raise E;
    end
  else
    if craClear in Actions then
      begin
      PQClear(res);
      res:=nil;
      end;
end;

function TPQConnection.TranslateFldType(res: PPGresult; Tuple: integer; out
  Size: integer; out ATypeOID: oid): TFieldType;

const
  VARHDRSZ=sizeof(longint);
var
  li : longint;
  aoid : oid;

begin
  Size := 0;
  ATypeOID:=0;
  AOID:=PQftype(res,Tuple);
  case AOID of
    Oid_varchar,Oid_bpchar,
    Oid_name               : begin
                             Result := ftString;
                             size := PQfsize(Res, Tuple);
                             if (size = -1) then
                               begin
                               li := PQfmod(res,Tuple);
                               if li = -1 then
                                 size := dsMaxStringSize
                               else
                                 size := (li-VARHDRSZ) and $FFFF;
                               end;
                             if size > MaxSmallint then size := MaxSmallint;
                             end;
//    Oid_text               : Result := ftString;
    Oid_text,Oid_JSON      : Result := ftMemo;
    Oid_Bytea              : Result := ftBlob;
    Oid_oid                : Result := ftInteger;
    Oid_int8               : Result := ftLargeInt;
    Oid_int4               : Result := ftInteger;
    Oid_int2               : Result := ftSmallInt;
    Oid_Float4             : Result := ftFloat;
    Oid_Float8             : Result := ftFloat;
    Oid_TimeStamp,
    Oid_TimeStampTZ        : Result := ftDateTime;
    Oid_Date               : Result := ftDate;
    Oid_Interval,
    Oid_Time,
    Oid_TimeTZ             : Result := ftTime;
    Oid_Bool               : Result := ftBoolean;
    Oid_Numeric            : begin
                             Result := ftBCD;
                             li := PQfmod(res,Tuple);
                             if li = -1 then
                               size := 4 // No information about the size available, use the maximum value
                             else
                             // The precision is the high 16 bits, the scale the
                             // low 16 bits with an offset of sizeof(int32).
                               begin
                               size := (li-VARHDRSZ) and $FFFF;
                               if (size > MaxBCDScale) or ((li shr 16)-size > MaxBCDPrecision-MaxBCDScale) then
                                 Result := ftFmtBCD;
                               end;
                             end;
    Oid_Money              : Result := ftCurrency;
    Oid_char               : begin
                             Result := ftFixedChar;
                             Size := 1;
                             end;
    Oid_uuid               : begin
                             Result := ftGuid;
                             Size := 38;
                             end;
    Oid_MacAddr            : begin
                             Result := ftFixedChar;
                             Size := 17;
                             end;
    Oid_Inet               : begin
                             Result := ftString;
                             Size := 39;
                             end;
    Oid_Unknown            : Result := ftUnknown;
  else
    Result:=ftUnknown;
    ATypeOID:=AOID;
  end;
end;

function TPQConnection.AllocateCursorHandle: TSQLCursor;

begin
  result := TPQCursor.create;
end;

procedure TPQConnection.DeAllocateCursorHandle(var cursor: TSQLCursor);
begin
  FreeAndNil(cursor);
end;

function TPQConnection.AllocateTransactionHandle: TSQLHandle;

begin
  result := TPQTransactionHandle.Create;
end;

procedure TPQConnection.PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams);
                          
const TypeStrings : array[TFieldType] of string =
    (
      'Unknown',   // ftUnknown
      'text',      // ftString
      'smallint',  // ftSmallint
      'int',       // ftInteger
      'int',       // ftWord
      'bool',      // ftBoolean
      'float',     // ftFloat
      'money',     // ftCurrency
      'numeric',   // ftBCD
      'date',      // ftDate
      'time',      // ftTime
      'timestamp', // ftDateTime
      'Unknown',   // ftBytes
      'bytea',     // ftVarBytes
      'Unknown',   // ftAutoInc
      'bytea',     // ftBlob 
      'text',      // ftMemo
      'bytea',     // ftGraphic
      'text',      // ftFmtMemo
      'Unknown',   // ftParadoxOle
      'Unknown',   // ftDBaseOle
      'Unknown',   // ftTypedBinary
      'Unknown',   // ftCursor
      'char',      // ftFixedChar
      'text',      // ftWideString
      'bigint',    // ftLargeint
      'Unknown',   // ftADT
      'Unknown',   // ftArray
      'Unknown',   // ftReference
      'Unknown',   // ftDataSet
      'Unknown',   // ftOraBlob
      'Unknown',   // ftOraClob
      'Unknown',   // ftVariant
      'Unknown',   // ftInterface
      'Unknown',   // ftIDispatch
      'uuid',      // ftGuid
      'Unknown',   // ftTimeStamp
      'numeric',   // ftFMTBcd
      'Unknown',   // ftFixedWideChar
      'Unknown'   // ftWideMemo
             ,
      'Unknown',   // ftOraTimeStamp
      'Unknown',   // ftOraInterval
      'Unknown',   // ftLongWord
      'Unknown',   // ftShortint
      'Unknown',   // ftByte
      'Unknown',   // ftExtended
      'real'       // ftSingle
    );


var
  s,ts : string;
  i : integer;
  P : TParam;
  PQ : TSQLDBParam;
  PQCurs : TPQCursor;

begin
  PQCurs:=cursor as TPQCursor;
  PQCurs.FPrepared := False;
  PQCurs.FDirect := False;
  // Prior to v8 there is no support for cursors and parameters.
  // So that's not supported.
  if PQCurs.FStatementType in [stInsert,stUpdate,stDelete, stSelect] then
    begin
    PQCurs.StmtName := 'prepst'+inttostr(FCursorCount);
    InterlockedIncrement(FCursorCount);
    if PQCurs.Handle=Nil then
      (TObject(aTransaction.Handle) as TPQTransactionHandle).Handle.RegisterCursor(PQCurs);

    // Only available for pq 8.0, so don't use it...
    // Res := pqprepare(tr,'prepst'+name+nr,pchar(buf),params.Count,pchar(''));
    s := 'prepare '+PQCurs.StmtName+' ';
    if Assigned(AParams) and (AParams.Count > 0) then
      begin
      s := s + '(';
      for i := 0 to AParams.Count-1 do
        begin
        P:=AParams[i];
        If (P is TSQLDBParam) then
          PQ:=TSQLDBParam(P)
        else
          PQ:=Nil;
        TS:=TypeStrings[P.DataType];
        if (TS<>'Unknown') then
          begin
          If Assigned(PQ)
             and Assigned(PQ.SQLDBData)
             and (PFieldBinding(PQ.SQLDBData)^.ExtendedFieldType=eftEnum) then
              ts:='unknown';
          s := s + ts + ','
          end
        else
          begin
          if P.DataType = ftUnknown then
            begin
            if P.IsNull then
              s:=s+' unknown ,'
            else
              DatabaseErrorFmt(SUnknownParamFieldType,[P.Name],self)
            end
          else
            DatabaseErrorFmt(SUnsupportedParameter,[Fieldtypenames[P.DataType]],self);
          end;
        end;
      s[length(s)] := ')';
      buf := AParams.ParseSQL(buf,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psPostgreSQL);
      end;
    s := s + ' as ' + buf;
    if LogEvent(detActualSQL) then
      Log(detActualSQL,S);
    PQCurs.Res:=Nil;
    PQCurs.Res:=PQCurs.Handle.Exec(S,False,SErrPrepareFailed);
    // if statement is INSERT, UPDATE, DELETE with RETURNING clause, then
    // override the statement type derived by parsing the query.
    if (PQCurs.FStatementType in [stInsert,stUpdate,stDelete]) and (pos('RETURNING', upcase(s)) > 0) then
      begin
      PQclear(PQCurs.res);
      PQCurs.res:=PQCurs.Handle.DescribePrepared(PQCurs.StmtName);
      if (PQresultStatus(PQCurs.res) = PGRES_COMMAND_OK) and (PQnfields(PQCurs.res) > 0) then
        PQCurs.FStatementType := stSelect;
      end;
    PQCurs.FPrepared := True;
    end
  else
    begin
    if Assigned(AParams) then
      PQCurs.Statement := AParams.ParseSQL(buf,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psPostgreSQL)
    else
      PQCurs.Statement:=Buf;
    PQCurs.FDirect:=True;
    end;

end;

procedure TPQConnection.UnPrepareStatement(cursor : TSQLCursor);

Var
  C : TPQCursor;

begin
  C:=Cursor as TPQCursor;
  if Assigned(C.Handle) then
    C.Handle.UnPrepareStatement(C,ForcedClose);
end;

procedure TPQConnection.Execute(cursor: TSQLCursor;atransaction:tSQLtransaction;AParams : TParams);

var ar  : array of PAnsiChar;
    handled : boolean;
    l,i : integer;
    s   : RawByteString;
    bd : TBlobData;
    lengths,formats : array of integer;
    ParamNames,
    ParamValues : array of string;
    cash: int64;
    PQCurs : TPQCursor;

    function FormatTimeInterval(Time: TDateTime): string; // supports Time >= '24:00:00'
    var hour, minute, second, millisecond: word;
    begin
      DecodeTime(Time, hour, minute, second, millisecond);
      Result := Format('%.2d:%.2d:%.2d.%.3d',[Trunc(Time)*24+hour,minute,second,millisecond]);
    end;

begin
  ar:=[];
  ParamNames:=[];
  ParamValues:=[];
  Lengths:=[];
  Formats:=[];
  PQCurs:=cursor as TPQCursor;
  PQCurs.CurTuple:=-1;
  PQclear(PQCurs.res);
  PQCurs.Res:=Nil;
  if PQCurs.FStatementType in [stInsert,stUpdate,stDelete,stSelect] then
    begin
    if LogEvent(detParamValue) then
      LogParams(AParams);
    if Assigned(AParams) and (AParams.Count > 0) then
      begin
      l:=AParams.Count;
      setlength(ar,l);
      for i := 0 to l-1 do
        ar[i]:=nil;
      setlength(lengths,l);
      setlength(formats,l);
      try
        for i := 0 to AParams.Count -1 do if not AParams[i].IsNull then
          begin
          handled:=False;
          case AParams[i].DataType of
            ftDateTime:
              s := FormatDateTime('yyyy"-"mm"-"dd hh":"nn":"ss.zzz', AParams[i].AsDateTime);
            ftDate:
              s := FormatDateTime('yyyy"-"mm"-"dd', AParams[i].AsDateTime);
            ftTime:
              s := FormatTimeInterval(AParams[i].AsDateTime);
            ftFloat:
              Str(AParams[i].AsFloat, s);
            ftBCD:
              Str(AParams[i].AsCurrency, s);
            ftCurrency:
              begin
                cash:=NtoBE(round(AParams[i].AsCurrency*100));
                setlength(s, sizeof(cash));
                Move(cash, s[1], sizeof(cash));
              end;
            ftFmtBCD:
              s := BCDToStr(AParams[i].AsFMTBCD, FSQLFormatSettings);
            ftBlob, ftGraphic, ftVarBytes:
              begin
              Handled:=true;
              bd:= AParams[i].AsBlob;
              l:=length(BD);
              if l>0 then
                begin
                GetMem(ar[i],l+1);
                ar[i][l]:=#0;
                Move(BD[0],ar[i]^, L);
                lengths[i]:=l;
                end;
              end
            else
              s := GetAsString(AParams[i]);
          end; {case}
          {$IFDEF PQDEBUG}
          WriteLn('Setting param ',aParams[i].Name,'(',aParams[i].DataType,') to ',S);
          {$ENDIF}
          if not handled then
            begin
            l:=length(s);
            GetMem(ar[i],l+1);
            StrMove(PAnsiChar(ar[i]), PAnsiChar(s), L+1);
            lengths[i]:=L;
            end;
          if (AParams[i].DataType in [ftBlob,ftMemo,ftGraphic,ftCurrency,ftVarBytes]) then
            Formats[i]:=1
          else
            Formats[i]:=0;
          end;
        PQCurs.res := PQCurs.Handle.ExecPrepared(PQCurs.StmtName,AParams.Count,@Ar[0],@Lengths[0],@Formats[0],False);
//        PQCurs.res := PQexecPrepared(PQCurs.Handle.NativeConn,pchar(PQCurs.StmtName),AParams.Count,@Ar[0],@Lengths[0],@Formats[0],1);
      finally
        for i := 0 to AParams.Count -1 do
          FreeMem(ar[i]);
      end;
      end
    else
      PQCurs.res := PQCurs.Handle.ExecPrepared(PQCurs.StmtName,0,nil,nil,nil,False);
    end
  else
    begin
    if PQCurs.handle=Nil then
      (TObject(aTransaction.Handle) as TPQTransactionHandle).Handle.RegisterCursor(PQCurs);
    if Assigned(AParams) and (AParams.Count > 0) then
      begin
      setlength(ParamNames,AParams.Count);
      setlength(ParamValues,AParams.Count);
      for i := 0 to AParams.Count -1 do
        begin
        ParamNames[AParams.Count-i-1] := '$'+inttostr(AParams[i].index+1);
        ParamValues[AParams.Count-i-1] := GetAsSQLText(AParams[i]);
        end;
      s := stringsreplace(PQCurs.Statement,ParamNames,ParamValues,[rfReplaceAll]);
      end
    else
      s := PQCurs.Statement;
    PQCurs.Res:=PQCurs.Handle.Exec(S,False,SErrExecuteFailed);
    end;
  PQCurs.FSelectable := assigned(PQCurs.res) and (PQresultStatus(PQCurs.res)=PGRES_TUPLES_OK);
end;


procedure TPQConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs);
var
  i         : integer;
  asize     : integer;
  aoid      : oid;
  fieldtype : tfieldtype;
  nFields   : integer;
  b : Boolean;
  Q : TPQCursor;
  FD : TSQLDBFieldDef;
  FB : PFieldBinding;

begin
  B:=False;
  Q:=cursor as TPQCursor;
  with Q do
    begin
    nFields := PQnfields(Res);
    setlength(FieldBinding,nFields);
    for i := 0 to nFields-1 do
      begin
      fieldtype := TranslateFldType(Res, i, asize, aoid );
      FD := AddFieldDef(FieldDefs, i+1, PQfname(Res, i), fieldtype, asize, -1, False, False, False) as TSQLDBFieldDef;
      With FD do
        begin
        SQLDBData:=@FieldBinding[i];
        FieldBinding[i].Index:=i;
        FieldBinding[i].FieldDef:=FD;
        FieldBinding[i].TypeOID:=aOID;
        B:=B or (aOID>0);
        end;
      end;
    CurTuple := -1;
    end;
  if B then
    begin
    // get all information in 1 go.
    GetExtendedFieldInfo(Q,Q.FieldBinding);
    For I:=0 to Length(Q.FieldBinding)-1 do
      begin
      FB:=@Q.FieldBinding[i];
      if (FB^.TypeOID>0) then
        begin
        FD:=FB^.FieldDef;
        Case FB^.ExtendedFieldType of
          eftEnum :
            begin
            FD.DataType:=ftString;
            FD.Size:=64;
            //FD.Attributes:=FD.Attributes+[faReadonly];
            end;
          eftCitext:
            begin
            FD.DataType:=ftMemo;
            end
        else
          if ErrorOnUnknownType then
            DatabaseError('Unhandled field type :'+FB^.TypeName,Self);
        end;
        end;
      end;
    end;
end;

function TPQConnection.GetHandle: pointer;
var
  i:integer;
  L : TList;
  T : TPGHandle;

begin
  result:=nil;
  if not Connected then
    exit;
  //Get any handle that is (still) connected
  L:=FHandlePool.LockList;
  try
    I:=L.Count-1;
    While (I>=0) and (Result=Nil) do
      begin
      T:=TPGHandle(L[i]);
      if T.Connected and T.CheckConnectionStatus(False) then
        Result:=T;
      Dec(I);
      end;
  finally
    FHandlePool.UnLockList;
  end;
  if Result<>Nil then
     exit;
  //Nothing connected!! Reconnect
  // T is element 0 after loop
  if Not Assigned(T) then
    begin
    T:=TPGHandle.Create(Self,DatabaseName);
    AddHandle(T);
    end;
  T.Connect;
  Result:=T;
end;


function TPQConnection.Fetch(cursor : TSQLCursor) : boolean;

begin
  with cursor as TPQCursor do
    begin
    inc(CurTuple);
    Result := (PQntuples(res)>CurTuple);
    end;
end;

function TPQConnection.LoadField(cursor : TSQLCursor;FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean;

const NBASE=10000;
      DAYS_PER_MONTH=30;

type TNumericRecord = record
       Digits : SmallInt;
       Weight : SmallInt;
       Sign   : SmallInt;
       Scale  : Smallint;
     end;
     TIntervalRec = packed record
       time  : int64;
       day   : longint;
       month : longint;
     end;
     TMacAddrRec = packed record
       a, b, c, d, e, f: byte;
     end;
     TInetRec = packed record
       family : byte;
       bits   : byte;
       is_cidr: byte;
       nb     : byte;
       ipaddr : array[1..16] of byte;
     end;

var
  x,i           : integer;
  s             : string;
  li            : Longint;
  CurrBuff      : pchar;
  dbl           : pdouble;
  cur           : currency;
  NumericRecord : ^TNumericRecord;
  guid          : TGUID;
  bcd           : TBCD;
  macaddr       : ^TMacAddrRec;
  inet          : ^TInetRec;

begin
  Createblob := False;
  with cursor as TPQCursor do
    begin
    x := GetFieldBinding(FieldDef)^.Index;

    // Joost, 5 jan 2006: I disabled the following, since it's useful for
    // debugging, but it also slows things down. In principle things can only go
    // wrong when FieldDefs is changed while the dataset is opened. A user just
    // shoudn't do that. ;) (The same is done in IBConnection)
    //if PQfname(Res, x) <> FieldDef.Name then
    //  DatabaseErrorFmt(SFieldNotFound,[FieldDef.Name],self);

    if pqgetisnull(res,CurTuple,x)=1 then
      result := false
    else
      begin
      CurrBuff := pqgetvalue(res,CurTuple,x);

      result := true;

      case FieldDef.DataType of
        ftInteger, ftSmallint, ftLargeInt :
          case PQfsize(res, x) of  // postgres returns big-endian numbers
            sizeof(int64) : pint64(buffer)^ := BEtoN(pint64(CurrBuff)^); // INT8
            sizeof(integer) : pinteger(buffer)^ := BEtoN(pinteger(CurrBuff)^); // INT4
            sizeof(smallint) : psmallint(buffer)^ := BEtoN(psmallint(CurrBuff)^); // INT2
          end; {case}
        ftFloat :
          case PQfsize(res, x) of  // postgres returns big-endian numbers
            sizeof(int64) :  // FLOAT8
              pint64(buffer)^ := BEtoN(pint64(CurrBuff)^);
            sizeof(integer) :  // FLOAT4
              begin
              li := BEtoN(pinteger(CurrBuff)^);
              pdouble(buffer)^ := psingle(@li)^
              end;
          end; {case}
        ftString, ftFixedChar :
          begin
          case PQftype(res, x) of
            Oid_MacAddr:
            begin
              macaddr := Pointer(CurrBuff);
              li := FormatBuf(Buffer^, FieldDef.Size, '%.2x:%.2x:%.2x:%.2x:%.2x:%.2x', 29,
                    [macaddr^.a,macaddr^.b,macaddr^.c,macaddr^.d,macaddr^.e,macaddr^.f]);
            end;
            Oid_Inet:
            begin
              inet := Pointer(CurrBuff);
              if inet^.nb = 4 then
                li := FormatBuf(Buffer^, FieldDef.Size, '%d.%d.%d.%d', 11,
                      [inet^.ipaddr[1],inet^.ipaddr[2],inet^.ipaddr[3],inet^.ipaddr[4]])
              else if inet^.nb = 16 then
                li := FormatBuf(Buffer^, FieldDef.Size, '%x%.2x:%x%.2x:%x%.2x:%x%.2x:%x%.2x:%x%.2x:%x%.2x:%x%.2x', 55,
                      [inet^.ipaddr[1],inet^.ipaddr[2],inet^.ipaddr[3],inet^.ipaddr[4],inet^.ipaddr[5],inet^.ipaddr[6],inet^.ipaddr[7],inet^.ipaddr[8],inet^.ipaddr[9],inet^.ipaddr[10],inet^.ipaddr[11],inet^.ipaddr[12],inet^.ipaddr[13],inet^.ipaddr[14],inet^.ipaddr[15],inet^.ipaddr[16]])
              else
                li := 0;
            end
            else
            begin
              li := pqgetlength(res,curtuple,x);
              if li > FieldDef.Size*FieldDef.CharSize then li := FieldDef.Size*FieldDef.CharSize;
              Move(CurrBuff^, Buffer^, li);
            end;
          end;
          pchar(Buffer + li)^ := #0;
          end;
        ftBlob, ftMemo, ftVarBytes :
          CreateBlob := True;
        ftDate :
          begin
          dbl := pointer(buffer);
          dbl^ := BEtoN(plongint(CurrBuff)^) + 36526;
          end;
        ftDateTime, ftTime :
          begin
          dbl := pointer(buffer);
          if FIntegerDateTimes then
            dbl^ := BEtoN(pint64(CurrBuff)^) / 1000000
          else
            pint64(dbl)^ := BEtoN(pint64(CurrBuff)^);
          case PQftype(res, x) of
            Oid_Timestamp, Oid_TimestampTZ:
              dbl^ := dbl^ + 3.1558464E+009; // postgres counts seconds elapsed since 1-1-2000
            Oid_Interval:
              dbl^ := dbl^ + BEtoN(plongint(CurrBuff+ 8)^) * SecsPerDay
                           + BEtoN(plongint(CurrBuff+12)^) * SecsPerDay * DAYS_PER_MONTH;
          end;
          dbl^ := dbl^ / SecsPerDay;
          // Now convert the mathematically-correct datetime to the
          // illogical windows/delphi/fpc TDateTime:
          if (dbl^ <= 0) and (frac(dbl^) < 0) then
            dbl^ := trunc(dbl^)-2-frac(dbl^);
          end;
        ftBCD, ftFmtBCD:
          begin
          NumericRecord := pointer(CurrBuff);
          NumericRecord^.Digits := BEtoN(NumericRecord^.Digits);
          NumericRecord^.Weight := BEtoN(NumericRecord^.Weight);
          NumericRecord^.Sign := BEtoN(NumericRecord^.Sign);
          NumericRecord^.Scale := BEtoN(NumericRecord^.Scale);
          inc(pointer(currbuff),sizeof(TNumericRecord));
          if (NumericRecord^.Digits = 0) and (NumericRecord^.Scale = 0) then // = NaN, which is not supported by Currency-type, so we return NULL
            result := false
          else if FieldDef.DataType = ftBCD then
            begin
            cur := 0;
            for i := 0 to NumericRecord^.Digits-1 do
              begin
              cur := cur + beton(pword(CurrBuff)^) * intpower(NBASE, NumericRecord^.weight-i);
              inc(pointer(CurrBuff),2);
              end;
            if NumericRecord^.Sign <> 0 then cur := -cur;
            Move(Cur, Buffer^, sizeof(currency));
            end
          else //ftFmtBCD
            begin
            bcd := 0;
            for i := 0 to NumericRecord^.Digits-1 do
              begin
              BCDAdd(bcd, beton(pword(CurrBuff)^) * intpower(NBASE, NumericRecord^.weight-i), bcd);
              inc(pointer(CurrBuff),2);
              end;
            if NumericRecord^.Sign <> 0 then BCDNegate(bcd);
            Move(bcd, Buffer^, sizeof(bcd));
            end;
          end;
        ftCurrency  :
          begin
          dbl := pointer(buffer);
          dbl^ := BEtoN(PInt64(CurrBuff)^) / 100;
          end;
        ftBoolean:
          pchar(buffer)[0] := CurrBuff[0];
        ftGuid:
          begin
          Move(CurrBuff^, guid, sizeof(guid));
          guid.D1:=BEtoN(guid.D1);
          guid.D2:=BEtoN(guid.D2);
          guid.D3:=BEtoN(guid.D3);
          s:=GUIDToString(guid);
          StrPLCopy(PChar(Buffer), s, FieldDef.Size);
          end
        else
          result := false;
      end;
      end;
    end;
end;

function TPQConnection.PortParamName: string;
begin
  Result := 'port';
end;

procedure TPQConnection.UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string);

var qry : TSQLQuery;
    relname : string;

begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  if (length(TableName)>2) and (TableName[1]='"') and (TableName[length(TableName)]='"') then
    relname := QuotedStr(AnsiDequotedStr(TableName, '"'))
  else
    relname := 'lower(' + QuotedStr(TableName) + ')';  // unquoted names are stored lower case in PostgreSQL which is incompatible with the SQL standard

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
              '(tc.relname = ' + relname + ') '+
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

function TPQConnection.GetSchemaInfoSQL(SchemaType: TSchemaType;
  SchemaObjectName, SchemaPattern: string): string;

var s : string;

begin
  // select * from information_schema.tables with 
  // where table_schema [not] in ('pg_catalog','information_schema') may be better.
  // But the following should work:
  case SchemaType of
    stTables     : s := 'select '+
                          'relfilenode        as recno, '+
                          'current_database() as catalog_name, '+
                          'nspname            as schema_name, '+
                          'relname            as table_name, '+
                          '0                  as table_type '+
                        'from pg_class c '+
                          'left join pg_namespace n on c.relnamespace=n.oid '+
                        'where (relkind=''r'') and not (nspname in (''pg_catalog'',''information_schema''))' +
                        'order by relname';

    stSysTables  : s := 'select '+
                          'relfilenode        as recno, '+
                          'current_database() as catalog_name, '+
                          'nspname            as schema_name, '+
                          'relname            as table_name, '+
                          '0                  as table_type '+
                        'from pg_class c '+
                          'left join pg_namespace n on c.relnamespace=n.oid '+
                        'where (relkind=''r'') and nspname in ((''pg_catalog'',''information_schema'')) ' + // only system tables
                        'order by relname';

    stColumns    : s := 'select '+
                          'a.attnum           as recno, '+
                          'current_database() as catalog_name, '+
                          'nspname            as schema_name, '+
                          'c.relname          as table_name, '+
                          'a.attname          as column_name, '+
                          'a.attnum           as column_position, '+
                          '0                  as column_type, '+
                          'a.atttypid         as column_datatype, '+
                          't.typname          as column_typename, '+
                          '0                  as column_subtype, '+
                          '0                  as column_precision, '+
                          '0                  as column_scale, '+
                          'a.atttypmod        as column_length, '+
                          'not a.attnotnull   as column_nullable '+
                        'from pg_class c '+
                          'join pg_attribute a on c.oid=a.attrelid '+
                          'join pg_type t on t.oid=a.atttypid '+
                          'left join pg_namespace n on c.relnamespace=n.oid '+
                          // This can lead to problems when case-sensitive tablenames are used.
                        'where (a.attnum>0) and (not a.attisdropped) and (upper(c.relname)=''' + Uppercase(SchemaObjectName) + ''') '+
                        'order by a.attname';
  else
    s := inherited;
  end; {case}
  result := s;
end;

function TPQConnection.GetNextValueSQL(const SequenceName: string; IncrementBy: Integer): string;
begin
  Result := Format('SELECT nextval(''%s'')', [SequenceName]);
end;

procedure TPQConnection.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TSQLTransaction);
var
  x             : integer;
  li            : Longint;
begin
  with cursor as TPQCursor do
    begin
    x := FieldBinding[FieldDef.FieldNo-1].Index;
    li := pqgetlength(res,curtuple,x);
    ReAllocMem(ABlobBuf^.BlobBuffer^.Buffer,li);
    Move(pqgetvalue(res,CurTuple,x)^, ABlobBuf^.BlobBuffer^.Buffer^, li);
    ABlobBuf^.BlobBuffer^.Size := li;
    end;
end;

function TPQConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  if assigned(cursor) and assigned((cursor as TPQCursor).res) then
    Result := StrToIntDef(PQcmdTuples((cursor as TPQCursor).res),-1)
  else
    Result := -1;
end;

function TPQConnection.GetConnectionInfo(InfoType: TConnInfoType): string;
begin
  Result:='';
  try
    {$IFDEF LinkDynamically}
    InitialisePostgres3;
    {$ENDIF}
    case InfoType of
      citServerType:
        Result:=TPQConnectionDef.TypeName;
      citServerVersion,
      citServerVersionString:
        if Connected then
          Result:=format('%6.6d', [PQserverVersion(GetHandle)]);
      citClientName:
        Result:=TPQConnectionDef.LoadedLibraryName;
    else
      Result:=inherited GetConnectionInfo(InfoType);
    end;
  finally
    {$IFDEF LinkDynamically}
    ReleasePostgres3;
    {$ENDIF}
  end;
end;


{ TPQConnectionDef }

class function TPQConnectionDef.TypeName: String;
begin
  Result:='PostgreSQL';
end;

class function TPQConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TPQConnection;
end;

class function TPQConnectionDef.Description: String;
begin
  Result:='Connect to a PostgreSQL database directly via the client library';
end;

class function TPQConnectionDef.DefaultLibraryName: String;
begin
  {$IfDef LinkDynamically}
  Result:=pqlib;
  {$else}
  Result:='';
  {$endif}
end;

class function TPQConnectionDef.LoadFunction: TLibraryLoadFunction;
begin
  {$IfDef LinkDynamically}
  Result:=@InitialisePostgres3;
  {$else}
  Result:=Nil;
  {$endif}
end;

class function TPQConnectionDef.UnLoadFunction: TLibraryUnLoadFunction;
begin
  {$IfDef LinkDynamically}
  Result:=@ReleasePostgres3;
  {$else}
  Result:=Nil;
  {$endif}
end;

class function TPQConnectionDef.LoadedLibraryName: string;
begin
  {$IfDef LinkDynamically}
  Result:=Postgres3LoadedLibrary;
  {$else}
  Result:='';
  {$endif}
end;

initialization
  RegisterConnection(TPQConnectionDef);
finalization
  UnRegisterConnection(TPQConnectionDef);
end.
