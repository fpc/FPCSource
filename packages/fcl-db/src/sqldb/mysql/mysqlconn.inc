
{$mode objfpc}{$H+}
{$MACRO on}

interface

uses
  Classes, SysUtils,bufdataset,sqldb,db,dynlibs,
{$IfDef mysql50}
  mysql50dyn;
  {$DEFINE TConnectionName:=TMySQL50Connection}
  {$DEFINE TMySQLConnectionDef:=TMySQL50ConnectionDef}
  {$DEFINE TTransactionName:=TMySQL50Transaction}
  {$DEFINE TCursorName:=TMySQL50Cursor}
{$ELSE}
  {$IfDef mysql41}
    mysql41dyn;
    {$DEFINE TConnectionName:=TMySQL41Connection}
    {$DEFINE TMySQLConnectionDef:=TMySQL41ConnectionDef}
    {$DEFINE TTransactionName:=TMySQL41Transaction}
    {$DEFINE TCursorName:=TMySQL41Cursor}
  {$ELSE}
    {$IFDEF mysql4} // temporary backwards compatibility for Lazarus
      mysql40dyn;
      {$DEFINE TConnectionName:=TMySQLConnection}
      {$DEFINE TMySQLConnectionDef:=TMySQL40ConnectionDef}
      {$DEFINE TTransactionName:=TMySQLTransaction}
      {$DEFINE TCursorName:=TMySQLCursor}
    {$ELSE}
      mysql40dyn;
      {$DEFINE TConnectionName:=TMySQL40Connection}
      {$DEFINE TMySQLConnectionDef:=TMySQL40ConnectionDef}
      {$DEFINE TTransactionName:=TMySQL40Transaction}
      {$DEFINE TCursorName:=TMySQL40Cursor}
    {$EndIf}
  {$EndIf}
{$EndIf}

Const
{$IfDef mysql50}
  MySQLVersion = '5.0';
{$ELSE}
  {$IfDef mysql41}
    MySQLVersion = '4.1';
  {$ELSE}
    {$IFDEF mysql4} // temporary backwards compatibility for Lazarus
      MySQLVersion = '4.0';
    {$ELSE}
      MySQLVersion = '4.0';
    {$EndIf}
  {$EndIf}
{$EndIf}

Type
  TTransactionName = Class(TSQLHandle)
  protected
  end;

  { TCursorName }

  TCursorName = Class(TSQLCursor)
  protected
    FRes: PMYSQL_RES;                   { Record pointer }
    FNeedData : Boolean;
    FStatement : String;
    Row : MYSQL_ROW;
    RowsAffected : QWord;
    LastInsertID : QWord;
    ParamBinding : TParamBinding;
    ParamReplaceString : String;
    MapDSRowToMSQLRow  : array of integer;
  end;

  { TConnectionName }

  TConnectionName = class (TSQLConnection)
  private
    FHostInfo: String;
    FServerInfo: String;
    FMySQL : PMySQL;
    function GetClientInfo: string;
    function GetServerStatus: String;
    procedure ConnectMySQL(var HMySQL : PMySQL;H,U,P : pchar);
    procedure ExecuteDirectMySQL(const query : string);
    function EscapeString(const Str : string) : string;
  protected
    function StrToStatementType(s : string) : TStatementType; override;
    Procedure ConnectToServer; virtual;
    Procedure SelectDatabase; virtual;
    function MySQLDataType(AType: enum_field_types; ASize, ADecimals: Integer; var NewType: TFieldType; var NewSize: Integer): Boolean;
    function MySQLWriteData(AType: enum_field_types;ASize: Integer; AFieldType: TFieldType; Source, Dest: PChar; out CreateBlob : boolean): Boolean;
    // SQLConnection methods
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    function GetAsSQLText(Field : TField) : string; overload; override;
    function GetAsSQLText(Param : TParam) : string; overload; override;

    Function AllocateCursorHandle : TSQLCursor; override;
    Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams); override;
    procedure UnPrepareStatement(cursor:TSQLCursor); override;
    procedure FreeFldBuffers(cursor : TSQLCursor); override;
    procedure Execute(cursor: TSQLCursor;atransaction:tSQLtransaction;AParams : TParams); override;
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs); override;
    function Fetch(cursor : TSQLCursor) : boolean; override;
    function LoadField(cursor : TSQLCursor;FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor;ATransaction : TSQLTransaction); override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartdbTransaction(trans : TSQLHandle; AParams : string) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
    procedure UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
  Public
    constructor Create(AOwner : TComponent); override;
    procedure GetFieldNames(const TableName : string; List :  TStrings); override;
    procedure GetTableNames(List : TStrings; SystemTables : Boolean = false); override;
    procedure CreateDB; override;
    procedure DropDB; override;
    Property ServerInfo : String Read FServerInfo;
    Property HostInfo : String Read FHostInfo;
    property ClientInfo: string read GetClientInfo;
    property ServerStatus : String read GetServerStatus;
  published
    property DatabaseName;
    property HostName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property Port stored false;
    property OnLogin;
  end;

  { TMySQLConnectionDef }

  TMySQLConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;


  EMySQLError = Class(Exception);

implementation

uses dbconst,ctypes,strutils;

{ TConnectionName }

Resourcestring
  SErrServerConnectFailed = 'Server connect failed.';
  SErrDatabaseSelectFailed = 'failed to select database: %s';
  SErrDatabaseCreate = 'Failed to create database: %s';
  SErrDatabaseDrop = 'Failed to drop database: %s';
  SErrNoData = 'No data for record';
  SErrExecuting = 'Error executing query: %s';
  SErrFetchingdata = 'Error fetching row data: %s';
  SErrGettingResult = 'Error getting result set: %s';
  SErrNoQueryResult = 'No result from query.';
  SErrNotversion50 = 'TMySQL50Connection can not work with the installed MySQL client version (%s).';
  SErrNotversion41 = 'TMySQL41Connection can not work with the installed MySQL client version (%s).';
  SErrNotversion40 = 'TMySQL40Connection can not work with the installed MySQL client version (%s).';

Procedure MySQlError(R : PMySQL;Msg: String;Comp : TComponent);

Var
  MySQLMsg : String;

begin
 If (R<>Nil) then
   begin
   MySQLMsg:=Strpas(mysql_error(R));
   DatabaseErrorFmt(Msg,[MySQLMsg],Comp);
   end
 else
   DatabaseError(Msg,Comp);
end;

function TConnectionName.StrToStatementType(s : string) : TStatementType;

begin
  S:=Lowercase(s);
  if s = 'show' then exit(stSelect);
  result := inherited StrToStatementType(s);
end;


function TConnectionName.GetClientInfo: string;

begin
  // To make it possible to call this if there's no connection yet
  InitialiseMysql;
  Try  
    Result:=strpas(mysql_get_client_info());
  Finally  
    ReleaseMysql;
  end;  
end;

function TConnectionName.GetServerStatus: String;
begin
  CheckConnected;
  Result := mysql_stat(FMYSQL);
end;

procedure TConnectionName.ConnectMySQL(var HMySQL : PMySQL;H,U,P : pchar);

Var
  APort : Cardinal;

begin
  HMySQL := mysql_init(HMySQL);
  APort:=Abs(StrToIntDef(Params.Values['Port'],0));
  HMySQL:=mysql_real_connect(HMySQL,PChar(H),PChar(U),Pchar(P),Nil,APort,Nil,0);
  If (HMySQL=Nil) then
    MySQlError(Nil,SErrServerConnectFailed,Self);
end;

function TConnectionName.GetAsSQLText(Field : TField) : string;

var esc_str : pchar;

begin
  if (not assigned(field)) or field.IsNull then Result := 'Null'
  else if field.DataType = ftString then
    Result := '''' + EscapeString(field.AsString) + ''''
  else Result := inherited GetAsSqlText(field);
end;

function TConnectionName.GetAsSQLText(Param: TParam) : string;

var esc_str : pchar;

begin
  if (not assigned(param)) or param.IsNull then Result := 'Null'
  else if param.DataType in [ftString,ftBlob,ftMemo] then
    Result := '''' + EscapeString(Param.AsString) + ''''
  else Result := inherited GetAsSqlText(Param);
end;


procedure TConnectionName.ConnectToServer;

Var
  H,U,P : String;

begin
  H:=HostName;
  U:=UserName;
  P:=Password;
  ConnectMySQL(FMySQL,pchar(H),pchar(U),pchar(P));
  FServerInfo := strpas(mysql_get_server_info(FMYSQL));
  FHostInfo := strpas(mysql_get_host_info(FMYSQL));
end;

procedure TConnectionName.SelectDatabase;
begin
  if mysql_select_db(FMySQL,pchar(DatabaseName))<>0 then
    MySQLError(FMySQL,SErrDatabaseSelectFailed,Self);
end;


procedure TConnectionName.CreateDB;

begin
  ExecuteDirectMySQL('CREATE DATABASE ' +DatabaseName);
end;

procedure TConnectionName.DropDB;

begin
  ExecuteDirectMySQL('DROP DATABASE ' +DatabaseName);
end;

procedure TConnectionName.ExecuteDirectMySQL(const query : string);

var H,U,P       : String;
    AMySQL      : PMySQL;

begin
  CheckDisConnected;

  InitialiseMysql;

  H:=HostName;
  U:=UserName;
  P:=Password;
  AMySQL := nil;
  ConnectMySQL(AMySQL,pchar(H),pchar(U),pchar(P));

  if mysql_query(AMySQL,pchar(query))<>0 then
    MySQLError(AMySQL,Format(SErrExecuting,[StrPas(mysql_error(AMySQL))]),Self);

  mysql_close(AMySQL);

  ReleaseMysql;
end;

function TConnectionName.EscapeString(const Str: string): string;

var Len : integer;

begin
  SetLength(result,length(str)*2+1);
  Len := mysql_real_escape_string(FMySQL,pchar(Result),pchar(Str),length(Str));
  SetLength(result,Len);
end;

procedure TConnectionName.DoInternalConnect;
begin
  InitialiseMysql;
{$IFDEF mysql50}
  if copy(strpas(mysql_get_client_info()),1,3)<>'5.0' then
    Raise EInOutError.CreateFmt(SErrNotversion50,[strpas(mysql_get_client_info())]);
{$ELSE}
  {$IFDEF mysql41}
  if copy(strpas(mysql_get_client_info()),1,3)<>'4.1' then
    Raise EInOutError.CreateFmt(SErrNotversion41,[strpas(mysql_get_client_info())]);
  {$ELSE}
  if copy(strpas(mysql_get_client_info()),1,3)<>'4.0' then
    Raise EInOutError.CreateFmt(SErrNotversion40,[strpas(mysql_get_client_info())]);
  {$ENDIF}
{$ENDIF}
  inherited DoInternalConnect;
  ConnectToServer;
  SelectDatabase;
end;

procedure TConnectionName.DoInternalDisconnect;
begin
  inherited DoInternalDisconnect;
  mysql_close(FMySQL);
  FMySQL:=Nil;
  ReleaseMysql;
end;

function TConnectionName.GetHandle: pointer;
begin
  Result:=FMySQL;
end;

function TConnectionName.AllocateCursorHandle: TSQLCursor;
begin
  Result:=TCursorName.Create;
end;

Procedure TConnectionName.DeAllocateCursorHandle(var cursor : TSQLCursor);

begin
  FreeAndNil(cursor);
end;

function TConnectionName.AllocateTransactionHandle: TSQLHandle;
begin
//  Result:=TTransactionName.Create;
  Result := nil;
end;

procedure TConnectionName.PrepareStatement(cursor: TSQLCursor;
  ATransaction: TSQLTransaction; buf: string;AParams : TParams);
begin
//  if assigned(AParams) and (AParams.count > 0) then
//    DatabaseError('Parameters (not) yet supported for the MySQL SqlDB connection.',self);
  With Cursor as TCursorName do
    begin
    FStatement:=Buf;
    if assigned(AParams) and (AParams.count > 0) then
      FStatement := AParams.ParseSQL(FStatement,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psSimulated,paramBinding,ParamReplaceString);
    if FStatementType=stSelect then
      FNeedData:=True;
    end
end;

procedure TConnectionName.UnPrepareStatement(cursor: TSQLCursor);
begin
  // do nothing
end;

procedure TConnectionName.FreeFldBuffers(cursor: TSQLCursor);

Var
  C : TCursorName;

begin
  C:=Cursor as TCursorName;
  if c.FStatementType=stSelect then
    c.FNeedData:=False;
  If (C.FRes<>Nil) then
    begin
    Mysql_free_result(C.FRes);
    C.FRes:=Nil;
    end;
  SetLength(c.MapDSRowToMSQLRow,0);
  inherited;
end;

procedure TConnectionName.Execute(cursor: TSQLCursor;
  atransaction: tSQLtransaction;AParams : TParams);

Var
  C : TCursorName;
  i : integer;
  ParamNames,ParamValues : array of string;

begin
  C:=Cursor as TCursorName;
  If (C.FRes=Nil) then
    begin
    if Assigned(AParams) and (AParams.count > 0) then
      begin
      setlength(ParamNames,AParams.Count);
      setlength(ParamValues,AParams.Count);
      for i := 0 to AParams.count -1 do
        begin
        ParamNames[AParams.count-i-1] := C.ParamReplaceString+inttostr(AParams[i].Index+1);
        ParamValues[AParams.count-i-1] := GetAsSQLText(AParams[i]);
        end;
      // paramreplacestring kan een probleem geven bij postgres als hij niet meer gewoon $ is?
      C.FStatement := stringsreplace(C.FStatement,ParamNames,ParamValues,[rfReplaceAll]);
      end;
    if mysql_query(FMySQL,Pchar(C.FStatement))<>0 then
      MySQLError(FMYSQL,Format(SErrExecuting,[StrPas(mysql_error(FMySQL))]),Self)
    else
      begin
      C.RowsAffected := mysql_affected_rows(FMYSQL);
      C.LastInsertID := mysql_insert_id(FMYSQL);
      if C.FNeedData then
        C.FRes:=mysql_store_result(FMySQL);
      end;
    end;
end;

function TConnectionName.MySQLDataType(AType: enum_field_types; ASize, ADecimals: Integer;
   var NewType: TFieldType; var NewSize: Integer): Boolean;
begin
  Result := True;
  case AType of
    FIELD_TYPE_LONGLONG:
      begin
      NewType := ftLargeint;
      NewSize := 0;
      end;
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT:
      begin
      NewType := ftSmallint;
      NewSize := 0;
      end;
    FIELD_TYPE_LONG, FIELD_TYPE_INT24:
      begin
      NewType := ftInteger;
      NewSize := 0;
      end;
{$ifdef mysql50}
    FIELD_TYPE_NEWDECIMAL,
{$endif}
    FIELD_TYPE_DECIMAL: if ADecimals < 5 then
                          begin
                          NewType := ftBCD;
                          NewSize := ADecimals;
                          end
                        else
                          begin
                          NewType := ftFloat;
                          NewSize := 0;
                          end;
    FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      begin
      NewType := ftFloat;
      NewSize := 0;
      end;
    FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
      begin
      NewType := ftDateTime;
      NewSize := 0;
      end;
    FIELD_TYPE_DATE:
      begin
      NewType := ftDate;
      NewSize := 0;
      end;
    FIELD_TYPE_TIME:
      begin
      NewType := ftTime;
      NewSize := 0;
      end;
    FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin
      // Since mysql server version 5.0.3 string-fields with a length of more
      // then 256 characters are suported
      if ASize>dsMaxStringSize then
        begin
        NewType := ftMemo;
        NewSize := 0;
        end
      else
        begin
        NewType := ftString;
        NewSize := ASize;
        end;
      end;
    FIELD_TYPE_BLOB:
      begin
      NewType := ftBlob;
      NewSize := 0;
      end
  else
    Result := False;
  end;
end;

procedure TConnectionName.AddFieldDefs(cursor: TSQLCursor;
  FieldDefs: TfieldDefs);

var
  C : TCursorName;
  I, TF, FC: Integer;
  field: PMYSQL_FIELD;
  DFT: TFieldType;
  DFS: Integer;

begin
//  Writeln('MySQL: Adding fielddefs');
  C:=(Cursor as TCursorName);
  If (C.FRes=Nil) then
    begin
//    Writeln('res is nil');
    MySQLError(FMySQL,SErrNoQueryResult,Self);
    end;
//  Writeln('MySQL: have result');
  FC:=mysql_num_fields(C.FRes);
  SetLength(c.MapDSRowToMSQLRow,FC);

  TF := 1;
  For I:= 0 to FC-1 do
    begin
    field := mysql_fetch_field_direct(C.FRES, I);
//    Writeln('MySQL: creating fielddef ',I+1);

    if MySQLDataType(field^.ftype, field^.length, field^.decimals, DFT, DFS) then
      begin
      TFieldDef.Create(FieldDefs, FieldDefs.MakeNameUnique(field^.name), DFT, DFS, False, TF);
      c.MapDSRowToMSQLRow[TF-1] := I;
      inc(TF);
      end
    end;
//  Writeln('MySQL: Finished adding fielddefs');
end;

function TConnectionName.Fetch(cursor: TSQLCursor): boolean;

Var
  C : TCursorName;

begin
  C:=Cursor as TCursorName;
  C.Row:=MySQL_Fetch_row(C.FRes);
  Result:=(C.Row<>Nil);
end;

function TConnectionName.LoadField(cursor : TSQLCursor;
  FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean;

var
  field: PMYSQL_FIELD;
  row : MYSQL_ROW;
  C : TCursorName;

begin
//  Writeln('LoadFieldsFromBuffer');
  C:=Cursor as TCursorName;
  if C.Row=nil then
     begin
  //   Writeln('LoadFieldsFromBuffer: row=nil');
     MySQLError(FMySQL,SErrFetchingData,Self);
     end;
  Row:=C.Row;
  
  inc(Row,c.MapDSRowToMSQLRow[FieldDef.FieldNo-1]);
  field := mysql_fetch_field_direct(C.FRES, c.MapDSRowToMSQLRow[FieldDef.FieldNo-1]);

  Result := MySQLWriteData(field^.ftype, field^.length, FieldDef.DataType, Row^, Buffer, CreateBlob);
end;

procedure TConnectionName.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TSQLTransaction);
var
  row : MYSQL_ROW;
  C : TCursorName;
  li : longint;
  Lengths : pculong;
begin
  C:=Cursor as TCursorName;
  if C.Row=nil then
    MySQLError(FMySQL,SErrFetchingData,Self);
  Row:=C.Row;

  inc(Row,c.MapDSRowToMSQLRow[FieldDef.FieldNo-1]);

  Lengths := mysql_fetch_lengths(c.FRes);
  li := Lengths[c.MapDSRowToMSQLRow[FieldDef.FieldNo-1]];

  ReAllocMem(ABlobBuf^.BlobBuffer^.Buffer,li);
  Move(pchar(row^)^, ABlobBuf^.BlobBuffer^.Buffer^, li);
  ABlobBuf^.BlobBuffer^.Size := li;
end;

function InternalStrToFloat(S: string): Extended;

var
  I: Integer;
  Tmp: string;

begin
  Tmp := '';
  for I := 1 to Length(S) do
    begin
    if not (S[I] in ['0'..'9', '+', '-', 'E', 'e']) then
      Tmp := Tmp + DecimalSeparator
    else
      Tmp := Tmp + S[I];
    end;
  Result := StrToFloat(Tmp);
end;

function InternalStrToCurrency(S: string): Extended;

var
  I: Integer;
  Tmp: string;

begin
  Tmp := '';
  for I := 1 to Length(S) do
    begin
    if not (S[I] in ['0'..'9', '+', '-', 'E', 'e']) then
      Tmp := Tmp + DecimalSeparator
    else
      Tmp := Tmp + S[I];
    end;
  Result := StrToCurr(Tmp);
end;

function InternalStrToDate(S: string): TDateTime;

var
  EY, EM, ED: Word;

begin
  EY := StrToInt(Copy(S,1,4));
  EM := StrToInt(Copy(S,6,2));
  ED := StrToInt(Copy(S,9,2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result:=0
  else
    Result:=EncodeDate(EY, EM, ED);
end;

function InternalStrToDateTime(S: string): TDateTime;

var
  EY, EM, ED: Word;
  EH, EN, ES: Word;

begin
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 6, 2));
  ED := StrToInt(Copy(S, 9, 2));
  EH := StrToInt(Copy(S, 12, 2));
  EN := StrToInt(Copy(S, 15, 2));
  ES := StrToInt(Copy(S, 18, 2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result := 0
  else
    Result := EncodeDate(EY, EM, ED);
  Result := Result + EncodeTime(EH, EN, ES, 0);
end;

function InternalStrToTime(S: string): TDateTime;

var
  EH, EM, ES: Word;

begin
  EH := StrToInt(Copy(S, 1, 2));
  EM := StrToInt(Copy(S, 4, 2));
  ES := StrToInt(Copy(S, 7, 2));
  Result := EncodeTime(EH, EM, ES, 0);
end;

function InternalStrToTimeStamp(S: string): TDateTime;

var
  EY, EM, ED: Word;
  EH, EN, ES: Word;

begin
{$IFNDEF mysql40}
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 6, 2));
  ED := StrToInt(Copy(S, 9, 2));
  EH := StrToInt(Copy(S, 12, 2));
  EN := StrToInt(Copy(S, 15, 2));
  ES := StrToInt(Copy(S, 18, 2));
{$ELSE}
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 5, 2));
  ED := StrToInt(Copy(S, 7, 2));
  EH := StrToInt(Copy(S, 9, 2));
  EN := StrToInt(Copy(S, 11, 2));
  ES := StrToInt(Copy(S, 13, 2));
{$ENDIF}
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result := 0
  else
    Result := EncodeDate(EY, EM, ED);
  Result := Result + EncodeTime(EH, EN, ES, 0);;
end;

function TConnectionName.MySQLWriteData(AType: enum_field_types;ASize: Integer; AFieldType: TFieldType; Source, Dest: PChar; out CreateBlob : boolean): Boolean;

var
  VI: Integer;
  VL: LargeInt;
  VS: Smallint;
  VF: Double;
  VC: Currency;
  VD: TDateTime;
  Src : String;

begin
  Result := False;
  CreateBlob := False;
  if Source = Nil then
    exit;
  Src:=StrPas(Source);
  case AType of
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT:
      begin
      if (Src<>'') then
        VS := StrToInt(Src)
      else
        VS := 0;
      Move(VS, Dest^, SizeOf(smallint));
      end;
    FIELD_TYPE_LONG, FIELD_TYPE_INT24:
      begin
      if (Src<>'') then
        VI := StrToInt(Src)
      else
        VI := 0;
      Move(VI, Dest^, SizeOf(Integer));
      end;
    FIELD_TYPE_LONGLONG:
      begin
      if (Src<>'') then
        VL := StrToInt64(Src)
      else
        VL := 0;
      Move(VL, Dest^, SizeOf(LargeInt));
      end;
{$ifdef mysql50}
    FIELD_TYPE_NEWDECIMAL,
{$endif}      
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      if AFieldType = ftBCD then
        begin
        VC := InternalStrToCurrency(Src);
        Move(VC, Dest^, SizeOf(Currency));
        end
      else
        begin
        if Src <> '' then
          VF := InternalStrToFloat(Src)
        else
          VF := 0;
        Move(VF, Dest^, SizeOf(Double));
        end;
    FIELD_TYPE_TIMESTAMP:
      begin
      if Src <> '' then
        VD := InternalStrToTimeStamp(Src)
      else
        VD := 0;
      Move(VD, Dest^, SizeOf(TDateTime));
      end;
    FIELD_TYPE_DATETIME:
      begin
      if Src <> '' then
        VD := InternalStrToDateTime(Src)
      else
        VD := 0;
      Move(VD, Dest^, SizeOf(TDateTime));
      end;
    FIELD_TYPE_DATE:
      begin
      if Src <> '' then
        VD := InternalStrToDate(Src)
      else
        VD := 0;
      Move(VD, Dest^, SizeOf(TDateTime));
      end;
    FIELD_TYPE_TIME:
      begin
      if Src <> '' then
        VD := InternalStrToTime(Src)
      else
        VD := 0;
      Move(VD, Dest^, SizeOf(TDateTime));
      end;
    FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin
{      Write('Moving string of size ',asize,' : ');
      P:=Source;
      If (P<>nil) then
        While P[0]<>#0 do
          begin
          Write(p[0]);
          inc(p);
          end;
      Writeln;
}
      // String-fields which can contain more then dsMaxStringSize characters
      // are mapped to ftBlob fields, while their mysql-datatype is FIELD_TYPE_BLOB
      if AFieldType in [ftBlob,ftMemo] then
        CreateBlob := True
      else if Src<> '' then
        Move(Source^, Dest^, ASize)
      else
        Dest^ := #0;
      end;
    FIELD_TYPE_BLOB:
      CreateBlob := True;
  end;
  Result := True;
end;

procedure TConnectionName.UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string);

var qry : TSQLQuery;

begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  qry := tsqlquery.Create(nil);
  qry.transaction := Transaction;
  qry.database := Self;
  with qry do
    begin
    ParseSQL := False;
    sql.clear;
    sql.add('show index from ' +  TableName);
    open;
    end;
  while not qry.eof do with IndexDefs.AddIndexDef do
    begin
    Name := trim(qry.fieldbyname('Key_name').asstring);
    Fields := trim(qry.fieldbyname('Column_name').asstring);
    If Name = 'PRIMARY' then options := options + [ixPrimary];
    If qry.fieldbyname('Non_unique').asinteger = 0 then options := options + [ixUnique];
    qry.next;
    while (name = trim(qry.fieldbyname('Key_name').asstring)) and (not qry.eof) do
      begin
      Fields := Fields + ';' + trim(qry.fieldbyname('Column_name').asstring);
      qry.next;
      end;
    end;
  qry.close;
  qry.free;
end;

function TConnectionName.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  if assigned(cursor) then
    // Compile this without range-checking. RowsAffected can be -1, although
    // it's an unsigned integer. (small joke from the mysql-guys)
    // Without range-checking this goes ok. If Range is turned on, this results
    // in range-check errors.
    Result := (cursor as TCursorName).RowsAffected
  else
    Result := -1;
end;

constructor TConnectionName.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnOptions := FConnOptions + [sqEscapeRepeat] + [sqEscapeSlash];
  FMySQL := Nil;
end;

procedure TConnectionName.GetFieldNames(const TableName: string; List: TStrings);
begin
  GetDBInfo(stColumns,TableName,'field',List);
end;

procedure TConnectionName.GetTableNames(List: TStrings; SystemTables: Boolean);
begin
  GetDBInfo(stTables,'','tables_in_'+DatabaseName,List)
end;

function TConnectionName.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
  Result:=Nil;
end;

function TConnectionName.Commit(trans: TSQLHandle): boolean;
begin
  // Do nothing.
end;

function TConnectionName.RollBack(trans: TSQLHandle): boolean;
begin
  // Do nothing
end;

function TConnectionName.StartdbTransaction(trans: TSQLHandle; AParams : string): boolean;
begin
  // Do nothing
end;

procedure TConnectionName.CommitRetaining(trans: TSQLHandle);
begin
  // Do nothing
end;

procedure TConnectionName.RollBackRetaining(trans: TSQLHandle);
begin
  // Do nothing
end;

function TConnectionName.GetSchemaInfoSQL(SchemaType: TSchemaType;
  SchemaObjectName, SchemaPattern: string): string;

begin
  case SchemaType of
    stTables     : result := 'show tables';
    stColumns    : result := 'show columns from ' + EscapeString(SchemaObjectName);
  else
    DatabaseError(SMetadataUnavailable)
  end; {case}
end;

{ TMySQLConnectionDef }

class function TMySQLConnectionDef.TypeName: String;
begin
  Result:='MySQL '+MySQLVersion;
end;

class function TMySQLConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TConnectionName;
end;

class function TMySQLConnectionDef.Description: String;
begin
  Result:='Connect to a MySQL '+MySQLVersion+'database directly via the client library';
end;

initialization
  RegisterConnection(TMySQLConnectionDef);
finalization
  UnRegisterConnection(TMySQLConnectionDef);
end.
