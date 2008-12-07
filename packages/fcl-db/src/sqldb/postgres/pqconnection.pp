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
    tr           : TPQTrans;
    res          : PPGresult;
    CurTuple     : integer;
    Nr           : string;
    FieldBinding : array of integer;
  end;

  { TPQConnection }

  TPQConnection = class (TSQLConnection)
  private
    FCursorCount         : word;
    FConnectString       : string;
    FSQLDatabaseHandle   : pointer;
    FIntegerDateTimes    : boolean;
    function TranslateFldType(res : PPGresult; Tuple : integer; var Size : integer) : TFieldType;
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
  end;


implementation

uses math, strutils;

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
      Oid_bpchar   = 1042;
      Oid_varchar  = 1043;
      Oid_timestamp = 1114;
      oid_date      = 1082;
      oid_time      = 1083;
      oid_numeric   = 1700;

constructor TPQConnection.Create(AOwner : TComponent);

begin
  inherited;
  FConnOptions := FConnOptions + [sqSupportParams] + [sqEscapeRepeat] + [sqEscapeSlash] + [sqQuoteFieldnames];
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

  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    msg := PQerrorMessage(ASQLDatabaseHandle);
    PQclear(res);
    PQFinish(ASQLDatabaseHandle);
    DatabaseError(SDBCreateDropFailed + ' (PostgreSQL: ' + Msg + ')',self);
    end
  else
    begin
    PQclear(res);
    PQFinish(ASQLDatabaseHandle);
    end;
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
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    result := false;
    DatabaseError(SErrRollbackFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self);
    end
  else
    begin
    PQclear(res);
    PQFinish(tr.PGConn);
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

  res := PQexec(tr.PGConn, 'COMMIT');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    result := false;
    DatabaseError(SErrCommitFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self);
    end
  else
    begin
    PQclear(res);
    PQFinish(tr.PGConn);
    result := true;
    end;
end;

function TPQConnection.StartdbTransaction(trans : TSQLHandle; AParams : string) : boolean;
var
  res : PPGresult;
  tr  : TPQTrans;
  msg : string;
begin
  result := false;

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
    res := PQexec(tr.PGConn, 'BEGIN');
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      result := false;
      PQclear(res);
      msg := PQerrorMessage(tr.PGConn);
      PQFinish(tr.PGConn);
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
  res := PQexec(tr.PGConn, 'ROLLBACK');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    DatabaseError(SErrRollbackFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self);
    end
  else
    begin
    PQclear(res);
    res := PQexec(tr.PGConn, 'BEGIN');
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      PQclear(res);
      msg := PQerrorMessage(tr.PGConn);
      PQFinish(tr.PGConn);
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
  res := PQexec(tr.PGConn, 'COMMIT');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    PQclear(res);
    DatabaseError(SErrCommitFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self);
    end
  else
    begin
    PQclear(res);
    res := PQexec(tr.PGConn, 'BEGIN');
    if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
      begin
      PQclear(res);
      msg := PQerrorMessage(tr.PGConn);
      PQFinish(tr.PGConn);
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
// This does only work for pg>=8.0, so timestamps won't work with earlier versions of pg which are compiled with integer_datetimes on
  if PQparameterStatus<>nil then
    FIntegerDatetimes := pqparameterstatus(FSQLDatabaseHandle,'integer_datetimes') = 'on';
end;

procedure TPQConnection.DoInternalDisconnect;
begin
  PQfinish(FSQLDatabaseHandle);
{$IfDef LinkDynamically}
  ReleasePostgres3;
{$EndIf}

end;

function TPQConnection.TranslateFldType(res : PPGresult; Tuple : integer; var Size : integer) : TFieldType;

begin
  Size := 0;
  case PQftype(res,Tuple) of
    Oid_varchar,Oid_bpchar,
    Oid_name               : begin
                             Result := ftstring;
                             size := PQfsize(Res, Tuple);
                             if (size = -1) then
                               begin
                               size := (PQfmod(res,Tuple)-4) and $FFFF;
                               if size = -5 then size := dsMaxStringSize;
                               end;
                             if size > dsMaxStringSize then size := dsMaxStringSize;
                             end;
//    Oid_text               : Result := ftstring;
    Oid_text               : Result := ftBlob;
    Oid_Bytea              : Result := ftBlob;
    Oid_oid                : Result := ftInteger;
    Oid_int8               : Result := ftLargeInt;
    Oid_int4               : Result := ftInteger;
    Oid_int2               : Result := ftSmallInt;
    Oid_Float4             : Result := ftFloat;
    Oid_Float8             : Result := ftFloat;
    Oid_TimeStamp          : Result := ftDateTime;
    Oid_Date               : Result := ftDate;
    Oid_Time               : Result := ftTime;
    Oid_Bool               : Result := ftBoolean;
    Oid_Numeric            : begin
                             Result := ftBCD;
                             // The precision is the high 16 bits, the scale the
                             // low 16 bits. Both with an offset of 4.
                             // In this case we need the scale:
                             size := (PQfmod(res,Tuple)-4) and $FFFF;
                             if size = -1 then
                               size := 4
                             end;
    Oid_Money              : Result := ftCurrency;
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
      'Unknown',
      'text',
      'int',
      'int',
      'int',
      'bool',
      'float',
      'numeric',
      'numeric',
      'date',
      'time',
      'timestamp',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'text',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'int',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown',
      'Unknown'
    );


var s : string;
    i : integer;

begin
  with (cursor as TPQCursor) do
    begin
    FPrepared := False;
    nr := inttostr(FCursorcount);
    inc(FCursorCount);
    // Prior to v8 there is no support for cursors and parameters.
    // So that's not supported.
    if FStatementType in [stInsert,stUpdate,stDelete, stSelect] then
      begin
      tr := TPQTrans(aTransaction.Handle);
      // Only available for pq 8.0, so don't use it...
      // Res := pqprepare(tr,'prepst'+name+nr,pchar(buf),params.Count,pchar(''));
      s := 'prepare prepst'+nr+' ';
      if Assigned(AParams) and (AParams.count > 0) then
        begin
        s := s + '(';
        for i := 0 to AParams.count-1 do if TypeStrings[AParams[i].DataType] <> 'Unknown' then
          s := s + TypeStrings[AParams[i].DataType] + ','
        else
          begin
          if AParams[i].DataType = ftUnknown then DatabaseErrorFmt(SUnknownParamFieldType,[AParams[i].Name],self)
            else DatabaseErrorFmt(SUnsupportedParameter,[Fieldtypenames[AParams[i].DataType]],self);
          end;
        s[length(s)] := ')';
        buf := AParams.ParseSQL(buf,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psPostgreSQL);
        end;
      s := s + ' as ' + buf;
      res := pqexec(tr.PGConn,pchar(s));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
        pqclear(res);
        DatabaseError(SErrPrepareFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self)
        end;
      FPrepared := True;
      end
    else
      statement := buf;
    end;
end;

procedure TPQConnection.UnPrepareStatement(cursor : TSQLCursor);

begin
  with (cursor as TPQCursor) do if FPrepared then
    begin
    if not tr.ErrorOccured then
      begin
      res := pqexec(tr.PGConn,pchar('deallocate prepst'+nr));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
        pqclear(res);
        DatabaseError(SErrPrepareFailed + ' (PostgreSQL: ' + PQerrorMessage(tr.PGConn) + ')',self)
        end;
      pqclear(res);
      end;
    FPrepared := False;
    end;
end;

procedure TPQConnection.Execute(cursor: TSQLCursor;atransaction:tSQLtransaction;AParams : TParams);

var ar  : array of pchar;
    i   : integer;
    s   : string;
    ParamNames,ParamValues : array of string;

begin
  with cursor as TPQCursor do
    begin
    if FStatementType in [stInsert,stUpdate,stDelete,stSelect] then
      begin
      if Assigned(AParams) and (AParams.count > 0) then
        begin
        setlength(ar,Aparams.count);
        for i := 0 to AParams.count -1 do if not AParams[i].IsNull then
          begin
          case AParams[i].DataType of
            ftDateTime:
              s := FormatDateTime('yyyy-mm-dd hh:nn:ss', AParams[i].AsDateTime);
            ftDate:
              s := FormatDateTime('yyyy-mm-dd', AParams[i].AsDateTime);
            ftTime:
              s := FormatDateTime('hh:nn:ss', AParams[i].AsDateTime);
            ftFloat, ftCurrency:
              Str(AParams[i].AsFloat, s);
            else
              s := AParams[i].AsString;
          end; {case}
          GetMem(ar[i],length(s)+1);
          StrMove(PChar(ar[i]),Pchar(s),Length(S)+1);
          end
        else
          FreeAndNil(ar[i]);
        res := PQexecPrepared(tr.PGConn,pchar('prepst'+nr),Aparams.count,@Ar[0],nil,nil,1);
        for i := 0 to AParams.count -1 do
          FreeMem(ar[i]);
        end
      else
        res := PQexecPrepared(tr.PGConn,pchar('prepst'+nr),0,nil,nil,nil,1);
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
      end;
    if not (PQresultStatus(res) in [PGRES_COMMAND_OK,PGRES_TUPLES_OK]) then
      begin
      s := PQerrorMessage(tr.PGConn);
      pqclear(res);

      tr.ErrorOccured := True;
// Don't perform the rollback, only make it possible to do a rollback.
// The other databases also don't do this.
//      atransaction.Rollback;
      DatabaseError(SErrExecuteFailed + ' (PostgreSQL: ' + s + ')',self);
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

type TNumericRecord = record
       Digits : SmallInt;
       Weight : SmallInt;
       Sign   : SmallInt;
       Scale  : Smallint;
     end;

var
  x,i           : integer;
  li            : Longint;
  CurrBuff      : pchar;
  tel           : byte;
  dbl           : pdouble;
  cur           : currency;
  NumericRecord : ^TNumericRecord;

begin
  Createblob := False;
  with cursor as TPQCursor do
    begin
    x := FieldBinding[FieldDef.FieldNo-1];

    // Joost, 5 jan 2006: I disabled the following, since it's usefull for
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
        ftInteger, ftSmallint, ftLargeInt,ftfloat :
          begin
          i := PQfsize(res, x);
          case i of               // postgres returns big-endian numbers
            sizeof(int64) : pint64(buffer)^ := BEtoN(pint64(CurrBuff)^);
            sizeof(integer) : pinteger(buffer)^ := BEtoN(pinteger(CurrBuff)^);
            sizeof(smallint) : psmallint(buffer)^ := BEtoN(psmallint(CurrBuff)^);
          else
            for tel := 1 to i do
              pchar(Buffer)[tel-1] := CurrBuff[i-tel];
          end; {case}
          end;
        ftString  :
          begin
          li := pqgetlength(res,curtuple,x);
          if li > dsMaxStringSize then li := dsMaxStringSize;
          Move(CurrBuff^, Buffer^, li);
          pchar(Buffer + li)^ := #0;
          end;
        ftBlob : Createblob := True;
        ftdate :
          begin
          dbl := pointer(buffer);
          dbl^ := BEtoN(plongint(CurrBuff)^) + 36526;
          end;
        ftDateTime, fttime :
          begin
          pint64(buffer)^ := BEtoN(pint64(CurrBuff)^);
          dbl := pointer(buffer);
          if FIntegerDatetimes then dbl^ := pint64(buffer)^/1000000;
          dbl^ := (dbl^+3.1558464E+009)/86400;  // postgres counts seconds elapsed since 1-1-2000
          // Now convert the mathematically-correct datetime to the
          // illogical windows/delphi/fpc TDateTime:
          if (dbl^ <= 0) and (frac(dbl^)<0) then
            dbl^ := trunc(dbl^)-2-frac(dbl^);
          end;
        ftBCD:
          begin
          NumericRecord := pointer(CurrBuff);
          NumericRecord^.Digits := BEtoN(NumericRecord^.Digits);
          NumericRecord^.Scale := BEtoN(NumericRecord^.Scale);
          NumericRecord^.Weight := BEtoN(NumericRecord^.Weight);
          inc(pointer(currbuff),sizeof(TNumericRecord));
          cur := 0;
          if (NumericRecord^.Digits = 0) and (NumericRecord^.Scale = 0) then // = NaN, which is not supported by Currency-type, so we return NULL
            result := false
          else
            begin
            for tel := 1 to NumericRecord^.Digits  do
              begin
              cur := cur + beton(pword(currbuff)^) * intpower(10000,-(tel-1)+NumericRecord^.weight);
              inc(pointer(currbuff),2);
              end;
            if BEtoN(NumericRecord^.Sign) <> 0 then cur := -cur;
            Move(Cur, Buffer^, sizeof(currency));
            end;
          end;
        ftCurrency  :
          begin
          dbl := pointer(buffer);
          dbl^ := BEtoN(PInteger(CurrBuff)^) / 100;
          end;
        ftBoolean:
          pchar(buffer)[0] := CurrBuff[0]
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

{ TPQConnectionDef }

class function TPQConnectionDef.TypeName: String;
begin
  Result:='PostGreSQL';
end;

class function TPQConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TPQConnection;
end;

class function TPQConnectionDef.Description: String;
begin
  Result:='Connect to a PostGreSQL database directly via the client library';
end;

initialization
  RegisterConnection(TPQConnectionDef);
finalization
  UnRegisterConnection(TPQConnectionDef);
end.
