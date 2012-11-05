unit pqconnection;

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils, sqldb, db, dbconst,bufdataset,
{$IfDef LinkDynamically}
  postgres3dyn;
{$Else}
  postgres3;
{$EndIf}

type
  TPQTrans = Class(TSQLHandle)
    protected
    PGConn        : PPGConn;
    ErrorOccured  : boolean;
  end;

  TPQCursor = Class(TSQLCursor)
    protected
    Statement    : string;
    StmtName     : string;
    tr           : TPQTrans;
    res          : PPGresult;
    CurTuple     : integer;
    FieldBinding : array of integer;
  end;

  EPQDatabaseError = class(EDatabaseError)
    public
      SEVERITY:string;
      SQLSTATE: string;
      MESSAGE_PRIMARY:string;
      MESSAGE_DETAIL:string;
      MESSAGE_HINT:string;
      STATEMENT_POSITION:string;
  end;

  { TPQConnection }

  TPQConnection = class (TSQLConnection)
  private
    FCursorCount         : word;
    FConnectString       : string;
    FSQLDatabaseHandle   : pointer;
    FIntegerDateTimes    : boolean;
    procedure CheckResultError(res: PPGresult; conn:PPGconn; ErrMsg: string);
    function GetPQDatabaseError(res : PPGresult;ErrMsg: string):EPQDatabaseError;
    function TranslateFldType(res : PPGresult; Tuple : integer; out Size : integer) : TFieldType;
    procedure ExecuteDirectPG(const Query : String);
  protected
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
    function StartdbTransaction(trans : TSQLHandle; AParams : string) : boolean; override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    procedure UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor;ATransaction : TSQLTransaction); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetConnectionInfo(InfoType:TConnInfoType): string; override;
    procedure CreateDB; override;
    procedure DropDB; override;
  published
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
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
  SErrClearSelection = 'Clear of selection failed';
  SErrExecuteFailed = 'Execution of query failed';
  SErrFieldDefsFailed = 'Can not extract field information from query';
  SErrFetchFailed = 'Fetch of data failed';
  SErrPrepareFailed = 'Preparation of query failed.';

const Oid_Bool     = 16;
      Oid_Bytea    = 17;
      Oid_char     = 18;
      Oid_Text     = 25;
      Oid_Oid      = 26;
      Oid_Name     = 19;
      Oid_Int8     = 20;
      Oid_int2     = 21;
      Oid_Int4     = 23;
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


constructor TPQConnection.Create(AOwner : TComponent);

begin
  inherited;
  FConnOptions := FConnOptions + [sqSupportParams] + [sqEscapeRepeat] + [sqEscapeSlash];
  FieldNameQuoteChars:=DoubleQuotes;
end;

procedure TPQConnection.CreateDB;

begin
  ExecuteDirectPG('CREATE DATABASE ' +DatabaseName);
end;

procedure TPQConnection.DropDB;

begin
  ExecuteDirectPG('DROP DATABASE ' +DatabaseName);
end;

procedure TPQConnection.ExecuteDirectPG(const query : string);

var ASQLDatabaseHandle    : PPGConn;
    res                   : PPGresult;
    msg                   : String;

begin
  CheckDisConnected;
{$IfDef LinkDynamically}
  InitialisePostgres3;
{$EndIf}

  FConnectString := '';
  if (UserName <> '') then FConnectString := FConnectString + ' user=''' + UserName + '''';
  if (Password <> '') then FConnectString := FConnectString + ' password=''' + Password + '''';
  if (HostName <> '') then FConnectString := FConnectString + ' host=''' + HostName + '''';
  FConnectString := FConnectString + ' dbname=''template1''';
  if (Params.Text <> '') then FConnectString := FConnectString + ' '+Params.Text;

  ASQLDatabaseHandle := PQconnectdb(pchar(FConnectString));

  if (PQstatus(ASQLDatabaseHandle) = CONNECTION_BAD) then
    begin
    msg := PQerrorMessage(ASQLDatabaseHandle);
    PQFinish(ASQLDatabaseHandle);
    DatabaseError(sErrConnectionFailed + ' (PostgreSQL: ' + Msg + ')',self);
    end;

  res := PQexec(ASQLDatabaseHandle,pchar(query));

  CheckResultError(res,ASQLDatabaseHandle,SDBCreateDropFailed);

  PQclear(res);
  PQFinish(ASQLDatabaseHandle);
{$IfDef LinkDynamically}
  ReleasePostgres3;
{$EndIf}
end;


function TPQConnection.GetTransactionHandle(trans : TSQLHandle): pointer;
begin
  Result := trans;
end;

function TPQConnection.RollBack(trans : TSQLHandle) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  result := false;

  tr := trans as TPQTrans;

  res := PQexec(tr.PGConn, 'ROLLBACK');

  CheckResultError(res,tr.PGConn,SErrRollbackFailed);

  PQclear(res);
  PQFinish(tr.PGConn);
  result := true;
end;

function TPQConnection.Commit(trans : TSQLHandle) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  result := false;

  tr := trans as TPQTrans;

  res := PQexec(tr.PGConn, 'COMMIT');
  CheckResultError(res,tr.PGConn,SErrCommitFailed);

  PQclear(res);
  PQFinish(tr.PGConn);
  result := true;
end;

function TPQConnection.StartdbTransaction(trans : TSQLHandle; AParams : string) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  tr := trans as TPQTrans;

  tr.PGConn := PQconnectdb(pchar(FConnectString));

  if (PQstatus(tr.PGConn) = CONNECTION_BAD) then
    begin
    result := false;
    PQFinish(tr.PGConn);
    DatabaseError(SErrConnectionFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self);
    end
  else
    begin
    tr.ErrorOccured := False;

    if CharSet <> '' then
      PQsetClientEncoding(tr.PGConn, pchar(CharSet));

    res := PQexec(tr.PGConn, 'BEGIN');
    CheckResultError(res,tr.PGConn,sErrTransactionFailed);

    PQclear(res);
    result := true;
    end;
end;

procedure TPQConnection.RollBackRetaining(trans : TSQLHandle);
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  tr := trans as TPQTrans;
  res := PQexec(tr.PGConn, 'ROLLBACK');
  CheckResultError(res,tr.PGConn,SErrRollbackFailed);

  PQclear(res);
  res := PQexec(tr.PGConn, 'BEGIN');
  CheckResultError(res,tr.PGConn,sErrTransactionFailed);

  PQclear(res);
end;

procedure TPQConnection.CommitRetaining(trans : TSQLHandle);
var
  res : PPGresult;
  tr  : TPQTrans;
begin
  tr := trans as TPQTrans;
  res := PQexec(tr.PGConn, 'COMMIT');
  CheckResultError(res,tr.PGConn,SErrCommitFailed);

  PQclear(res);
  res := PQexec(tr.PGConn, 'BEGIN');
  CheckResultError(res,tr.PGConn,sErrTransactionFailed);

  PQclear(res);
end;


procedure TPQConnection.DoInternalConnect;

var msg : string;

begin
{$IfDef LinkDynamically}
  InitialisePostgres3;
{$EndIf}

  inherited dointernalconnect;

  FConnectString := '';
  if (UserName <> '') then FConnectString := FConnectString + ' user=''' + UserName + '''';
  if (Password <> '') then FConnectString := FConnectString + ' password=''' + Password + '''';
  if (HostName <> '') then FConnectString := FConnectString + ' host=''' + HostName + '''';
  if (DatabaseName <> '') then FConnectString := FConnectString + ' dbname=''' + DatabaseName + '''';
  if (Params.Text <> '') then FConnectString := FConnectString + ' '+Params.Text;

  FSQLDatabaseHandle := PQconnectdb(pchar(FConnectString));

  if (PQstatus(FSQLDatabaseHandle) = CONNECTION_BAD) then
    begin
    msg := PQerrorMessage(FSQLDatabaseHandle);
    dointernaldisconnect;
    DatabaseError(sErrConnectionFailed + ' (PostgreSQL: ' + msg + ')',self);
    end;
// This only works for pg>=8.0, so timestamps won't work with earlier versions of pg which are compiled with integer_datetimes on
  if PQparameterStatus<>nil then
    FIntegerDateTimes := PQparameterStatus(FSQLDatabaseHandle,'integer_datetimes') = 'on';
end;

procedure TPQConnection.DoInternalDisconnect;
begin
  PQfinish(FSQLDatabaseHandle);
{$IfDef LinkDynamically}
  ReleasePostgres3;
{$EndIf}

end;

procedure TPQConnection.CheckResultError(res: PPGresult; conn: PPGconn;
  ErrMsg: string);
var
  E: EPQDatabaseError;

begin
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    E:=GetPQDatabaseError(res,ErrMsg);
    pqclear(res);
    if assigned(conn) then
      PQFinish(conn);
    raise E;
    end;
end;

function TPQConnection.GetPQDatabaseError(res: PPGresult; ErrMsg: string
  ): EPQDatabaseError;
var
  serr:string;
  E: EPQDatabaseError;
  CompName: string;
  SEVERITY:string;
  SQLSTATE: string;
  MESSAGE_PRIMARY:string;
  MESSAGE_DETAIL:string;
  MESSAGE_HINT:string;
  STATEMENT_POSITION:string;
begin
  SEVERITY:=PQresultErrorField(res,ord('S'));
  SQLSTATE:=PQresultErrorField(res,ord('C'));
  MESSAGE_PRIMARY:=PQresultErrorField(res,ord('M'));
  MESSAGE_DETAIL:=PQresultErrorField(res,ord('D'));
  MESSAGE_HINT:=PQresultErrorField(res,ord('H'));
  STATEMENT_POSITION:=PQresultErrorField(res,ord('P'));
  serr:=PQresultErrorMessage(res)+LineEnding+
    'Severity: '+ SEVERITY +LineEnding+
    'SQL State: '+ SQLSTATE +LineEnding+
    'Primary Error: '+ MESSAGE_PRIMARY +LineEnding+
    'Error Detail: '+ MESSAGE_DETAIL +LineEnding+
    'Hint: '+ MESSAGE_HINT +LineEnding+
    'Character: '+ STATEMENT_POSITION +LineEnding;
  if Self.Name = '' then CompName := Self.ClassName else CompName := Self.Name;
  E:=EPQDatabaseError.CreateFmt('%s : %s  (PostgreSQL: %s)', [CompName,ErrMsg, serr]);
  E.SEVERITY:=SEVERITY;
  E.SQLSTATE:=SQLSTATE;
  E.MESSAGE_PRIMARY:=MESSAGE_PRIMARY;
  E.MESSAGE_DETAIL:=MESSAGE_DETAIL;
  E.MESSAGE_HINT:=MESSAGE_HINT;
  E.STATEMENT_POSITION:=STATEMENT_POSITION;
  result:=E;
end;

function TPQConnection.TranslateFldType(res : PPGresult; Tuple : integer; out Size : integer) : TFieldType;
const VARHDRSZ=sizeof(longint);
var li : longint;
begin
  Size := 0;
  case PQftype(res,Tuple) of
    Oid_varchar,Oid_bpchar,
    Oid_name               : begin
                             Result := ftstring;
                             size := PQfsize(Res, Tuple);
                             if (size = -1) then
                               begin
                               li := PQfmod(res,Tuple);
                               if li = -1 then
                                 size := dsMaxStringSize
                               else
                                 size := (li-VARHDRSZ) and $FFFF;
                               end;
                             if size > dsMaxStringSize then size := dsMaxStringSize;
                             end;
//    Oid_text               : Result := ftstring;
    Oid_text               : Result := ftMemo;
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
    Result := ftUnknown;
  end;
end;

Function TPQConnection.AllocateCursorHandle : TSQLCursor;

begin
  result := TPQCursor.create;
end;

Procedure TPQConnection.DeAllocateCursorHandle(var cursor : TSQLCursor);

begin
  FreeAndNil(cursor);
end;

Function TPQConnection.AllocateTransactionHandle : TSQLHandle;

begin
  result := TPQTrans.create;
end;

procedure TPQConnection.PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams);
                          
const TypeStrings : array[TFieldType] of string =
    (
      'Unknown',   // ftUnknown
      'text',     // ftString
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
      'Unknown',   // ftVarBytes
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
      'Unknown'    // ftWideMemo
    );


var s : string;
    i : integer;

begin
  with (cursor as TPQCursor) do
    begin
    FPrepared := False;
    // Prior to v8 there is no support for cursors and parameters.
    // So that's not supported.
    if FStatementType in [stInsert,stUpdate,stDelete, stSelect] then
      begin
      StmtName := 'prepst'+inttostr(FCursorCount);
      inc(FCursorCount);
      tr := TPQTrans(aTransaction.Handle);
      // Only available for pq 8.0, so don't use it...
      // Res := pqprepare(tr,'prepst'+name+nr,pchar(buf),params.Count,pchar(''));
      s := 'prepare '+StmtName+' ';
      if Assigned(AParams) and (AParams.count > 0) then
        begin
        s := s + '(';
        for i := 0 to AParams.count-1 do if TypeStrings[AParams[i].DataType] <> 'Unknown' then
          s := s + TypeStrings[AParams[i].DataType] + ','
        else
          begin
          if AParams[i].DataType = ftUnknown then 
            DatabaseErrorFmt(SUnknownParamFieldType,[AParams[i].Name],self)
          else 
            DatabaseErrorFmt(SUnsupportedParameter,[Fieldtypenames[AParams[i].DataType]],self);
          end;
        s[length(s)] := ')';
        buf := AParams.ParseSQL(buf,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psPostgreSQL);
        end;
      s := s + ' as ' + buf;
      res := pqexec(tr.PGConn,pchar(s));
      CheckResultError(res,nil,SErrPrepareFailed);
      // if statement is INSERT, UPDATE, DELETE with RETURNING clause, then
      // override the statement type derrived by parsing the query.
      if (FStatementType in [stInsert,stUpdate,stDelete]) and (pos('RETURNING', upcase(s)) > 0) then
        begin
        PQclear(res);
        res := PQdescribePrepared(tr.PGConn,pchar(StmtName));
        if (PQresultStatus(res) = PGRES_COMMAND_OK) and (PQnfields(res) > 0) then
          FStatementType := stSelect;
        end;
      FPrepared := True;
      end
    else
      statement := AParams.ParseSQL(buf,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psPostgreSQL);
    end;
end;

procedure TPQConnection.UnPrepareStatement(cursor : TSQLCursor);
var
  E: EPQDatabaseError;

begin
  with (cursor as TPQCursor) do if FPrepared then
    begin
    if not tr.ErrorOccured then
      begin
      PQclear(res);
      res := pqexec(tr.PGConn,pchar('deallocate '+StmtName));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
          E:=GetPQDatabaseError(res,SErrPrepareFailed);
          pqclear(res);
          raise E;
        end
      else
        pqclear(res);
      end;
    FPrepared := False;
    end;
end;

procedure TPQConnection.Execute(cursor: TSQLCursor;atransaction:tSQLtransaction;AParams : TParams);

var ar  : array of pchar;
    l,i : integer;
    s   : string;
    lengths,formats : array of integer;
    ParamNames,
    ParamValues : array of string;
    cash: int64;
    E: EPQDatabaseError;

begin
  with cursor as TPQCursor do
    begin
    if FStatementType in [stInsert,stUpdate,stDelete,stSelect] then
      begin
      pqclear(res);
      if Assigned(AParams) and (AParams.count > 0) then
        begin
        l:=Aparams.count;
        setlength(ar,l);
        setlength(lengths,l);
        setlength(formats,l);
        for i := 0 to AParams.count -1 do if not AParams[i].IsNull then
          begin
          case AParams[i].DataType of
            ftDateTime:
              s := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AParams[i].AsDateTime);
            ftDate:
              s := FormatDateTime('yyyy-mm-dd', AParams[i].AsDateTime);
            ftTime:
              s := FormatDateTime('hh:nn:ss.zzz', AParams[i].AsDateTime);
            ftFloat, ftBCD:
              Str(AParams[i].AsFloat, s);
            ftCurrency:
              begin
                cash:=NtoBE(round(AParams[i].AsCurrency*100));
                setlength(s, sizeof(cash));
                Move(cash, s[1], sizeof(cash));
              end;
            ftFmtBCD:
              s := BCDToStr(AParams[i].AsFMTBCD, FSQLFormatSettings);
            else
              s := AParams[i].AsString;
          end; {case}
          GetMem(ar[i],length(s)+1);
          StrMove(PChar(ar[i]),Pchar(s),Length(S)+1);
          lengths[i]:=Length(s);
          if (AParams[i].DataType in [ftBlob,ftMemo,ftGraphic,ftCurrency]) then
            Formats[i]:=1
          else
            Formats[i]:=0;  
          end
        else
          FreeAndNil(ar[i]);
        res := PQexecPrepared(tr.PGConn,pchar(StmtName),Aparams.count,@Ar[0],@Lengths[0],@Formats[0],1);
        for i := 0 to AParams.count -1 do
          FreeMem(ar[i]);
        end
      else
        res := PQexecPrepared(tr.PGConn,pchar(StmtName),0,nil,nil,nil,1);
      end
    else
      begin
      tr := TPQTrans(aTransaction.Handle);

      if Assigned(AParams) and (AParams.count > 0) then
        begin
        setlength(ParamNames,AParams.Count);
        setlength(ParamValues,AParams.Count);
        for i := 0 to AParams.count -1 do
          begin
          ParamNames[AParams.count-i-1] := '$'+inttostr(AParams[i].index+1);
          ParamValues[AParams.count-i-1] := GetAsSQLText(AParams[i]);
          end;
        s := stringsreplace(statement,ParamNames,ParamValues,[rfReplaceAll]);
        end
      else
        s := Statement;
      res := pqexec(tr.PGConn,pchar(s));
      if (PQresultStatus(res) in [PGRES_COMMAND_OK,PGRES_TUPLES_OK]) then
        begin
          pqclear(res); 
          res:=nil;
        end;
      end;
    if assigned(res) and not (PQresultStatus(res) in [PGRES_COMMAND_OK,PGRES_TUPLES_OK]) then
      begin
      E:=GetPQDatabaseError(res,SErrExecuteFailed);
      pqclear(res);

      tr.ErrorOccured := True;
// Don't perform the rollback, only make it possible to do a rollback.
// The other databases also don't do this.
//      atransaction.Rollback;
      raise E;
      end;
    end;
end;


procedure TPQConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs);
var
  i         : integer;
  size      : integer;
  fieldtype : tfieldtype;
  nFields   : integer;

begin
  with cursor as TPQCursor do
    begin
    nFields := PQnfields(Res);
    setlength(FieldBinding,nFields);
    for i := 0 to nFields-1 do
      begin
      fieldtype := TranslateFldType(Res, i,size);
      with TFieldDef.Create(FieldDefs, FieldDefs.MakeNameUnique(PQfname(Res, i)), fieldtype,size, False, (i + 1)) do
        FieldBinding[FieldNo-1] := i;
      end;
    CurTuple := -1;
    end;
end;

function TPQConnection.GetHandle: pointer;
begin
  Result := FSQLDatabaseHandle;
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
  x,i,j         : integer;
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
    x := FieldBinding[FieldDef.FieldNo-1];

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
        ftInteger, ftSmallint, ftLargeInt, ftFloat :
          begin
          i := PQfsize(res, x);
          case i of               // postgres returns big-endian numbers
            sizeof(int64) : pint64(buffer)^ := BEtoN(pint64(CurrBuff)^);
            sizeof(integer) : pinteger(buffer)^ := BEtoN(pinteger(CurrBuff)^);
            sizeof(smallint) : psmallint(buffer)^ := BEtoN(psmallint(CurrBuff)^);
          else
            for j := 1 to i do
              pchar(Buffer)[j-1] := CurrBuff[i-j];
          end; {case}
          end;
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
              if li > dsMaxStringSize then li := dsMaxStringSize;
              Move(CurrBuff^, Buffer^, li);
            end;
          end;
          pchar(Buffer + li)^ := #0;
          end;
        ftBlob, ftMemo :
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

procedure TPQConnection.UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string);

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

function TPQConnection.GetSchemaInfoSQL(SchemaType: TSchemaType;
  SchemaObjectName, SchemaPattern: string): string;

var s : string;

begin
  case SchemaType of
    stTables     : s := 'select '+
                          'relfilenode              as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'relname                  as table_name, '+
                          '0                        as table_type '+
                        'from '+
                          'pg_class '+
                        'where '+
                          '(relowner > 1) and relkind=''r''' +
                        'order by relname';

    stSysTables  : s := 'select '+
                          'relfilenode              as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'relname                  as table_name, '+
                          '0                        as table_type '+
                        'from '+
                          'pg_class '+
                        'where '+
                          'relkind=''r''' +
                        'order by relname';
    stColumns    : s := 'select '+
                          'a.attnum                 as recno, '+
                          '''''                     as catalog_name, '+
                          '''''                     as schema_name, '+
                          'c.relname                as table_name, '+
                          'a.attname                as column_name, '+
                          '0                        as column_position, '+
                          '0                        as column_type, '+
                          '0                        as column_datatype, '+
                          '''''                     as column_typename, '+
                          '0                        as column_subtype, '+
                          '0                        as column_precision, '+
                          '0                        as column_scale, '+
                          'a.atttypmod              as column_length, '+
                          'not a.attnotnull         as column_nullable '+
                        'from '+
                          ' pg_class c, pg_attribute a '+
                        'WHERE '+
                        // This can lead to problems when case-sensitive tablenames are used.
                          '(c.oid=a.attrelid) and (a.attnum>0) and (not a.attisdropped) and (upper(c.relname)=''' + Uppercase(SchemaObjectName) + ''') ' +
                        'order by a.attname';
  else
    DatabaseError(SMetadataUnavailable)
  end; {case}
  result := s;
end;

procedure TPQConnection.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TSQLTransaction);
var
  x             : integer;
  li            : Longint;
begin
  with cursor as TPQCursor do
    begin
    x := FieldBinding[FieldDef.FieldNo-1];
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
          Result:=format('%6.6d', [PQserverVersion(FSQLDatabaseHandle)]);
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
