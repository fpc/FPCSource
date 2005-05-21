unit MySQLDB4;

{$H+}

interface

uses
  SysUtils, Classes, db, mysql4,mysql4_com;

type
  PMySQLDatasetBookmark = ^TMySQLDatasetBookmark;
  TMySQLDatasetBookmark = record
                          BookmarkData: Integer;
                          BookmarkFlag: TBookmarkFlag;
                          end;

  Pinteger = ^Integer;

  TMySQLDatabase = class(TDatabase)
  Private
    FMYSQL: PMYSQL;
    FServerInfo: string;
    FHostInfo: string;
    function GetHostName: String;
    Function GetUserName : String;
    procedure SetHostName(const AValue: String);
    Procedure SetUserName (Value : String);
    Procedure SetPassword (Value : String);
    Function GetPassword : String;
    Function GetClientInfo : String;
  Protected
    Procedure ConnectToServer;
    Procedure SelectDatabase;
    Procedure DoInternalConnect; override;
    Procedure DoInternalDisConnect; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    function  GetServerStatus: string;
  Public
    Procedure CreateDatabase;
    Procedure DropDatabase;
    Property ServerInfo : String Read FServerInfo;
    Property HostInfo : String Read FHostInfo;
    property ClientInfo: string read GetClientInfo;
    property ServerStatus : String read GetServerStatus;
  Published
    Property UserName : String Read GetUserName Write SetUserName;
    Property HostName : String Read GetHostName Write SetHostName;
    Property Password : String Read GetPassword Write SetPassword;
  end;

  TMySQLDataset = class(TDBDataSet)
  private
    FSQL: TStrings;
    FRecordSize: Integer;
    FBufferSize: Integer;
    // MySQL data
    FMYSQLRES: PMYSQL_RES;
    FCurrentRecord: Integer;              { Record pointer }
    FAffectedRows: QWord;
    FLastInsertID: Integer;
    FLoadingFieldDefs: Boolean;

    procedure DoClose;
    procedure DoQuery;
    procedure DoGetResult;

    procedure CalculateSizes;
    procedure LoadBufferFromData(Buffer: PChar);
  protected
    Function FMySQL : PMySQL;
    procedure SetSQL(const Value: TStrings);
    function InternalStrToFloat(S: string): Extended;
    function InternalStrToDate(S: string): TDateTime;
    function InternalStrToTime(S: string): TDateTime;
    function InternalStrToDateTime(S: string): TDateTime;
    function InternalStrToTimeStamp(S: string): TDateTime;

    function MySQLFieldToFieldType(AType: enum_field_types; ASize: Integer;
         var NewType: TFieldType; var NewSize: Integer): Boolean;
    function MySQLDataSize(AType: enum_field_types; ASize: Integer): Integer;
    function MySQLWriteFieldData(AType: enum_field_types; ASize: Integer; Source: PChar;
       Dest: PChar): Integer;


    function GetCanModify: Boolean; override;
    { Mandatory overrides }
    // Record buffer methods:
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    // Bookmark methods:
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    // Navigational methods:
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // Editing methods:
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    // Misc methods:
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    { Optional overrides }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecSQL;

    // TDataset method
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    property AffectedRows: QWord read FAffectedRows;
    property LastInsertID: Integer read FLastInsertID;
  published
    property Active;
    property Database;
    property SQL: TStrings read FSQL write SetSQL;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;
  end;

  EMySQLError = Class(Exception);

implementation

Resourcestring
  SErrServerConnectFailed = 'Server connect failed.';
  SErrDatabaseSelectFailed = 'failed to select database: %s';
  SErrDatabaseCreate = 'Failed to create database: %s';
  SErrDatabaseDrop = 'Failed to drop database: %s';
  SErrNoData = 'No data for record';
  SErrExecuting = 'Error executing query: %s';
  SErrFetchingdata = 'Error fetching row data: %s';
  SErrGettingResult = 'Error getting result set: %s';

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

{ TMySQLDataset }

constructor TMySQLDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FBufferSize := 0;
  FRecordSize := 0;
  FCurrentRecord := -1;
  FLoadingFieldDefs := False;
  FAffectedRows := 0;
  FLastInsertID := -1;
  FMYSQLRES := nil;
end;

destructor TMySQLDataset.Destroy;
begin
  Close;
  FSQL.Free;
  inherited destroy;
end;

function TMySQLDataset.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(FBufferSize);
end;

procedure TMySQLDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  If (@Buffer<>nil) then
    FreeMem(Buffer);
end;

procedure TMySQLDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PMySQLDatasetBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TMySQLDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result:=PMySQLDatasetBookmark(Buffer + FRecordSize)^.BookmarkFlag;
end;

function TMySQLDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

var
  I, FC: Integer;
  fld: PMYSQL_FIELD;
  CurBuf: PChar;

begin
  Result := False;
  CurBuf := ActiveBuffer;
  FC := mysql_num_fields(FMYSQLRES);
  for I := 0 to FC-1 do
    begin
    fld := mysql_fetch_field_direct(FMYSQLRES, I);
    if Field.FieldName = fld^.name then
      begin
      Move(CurBuf^, PChar(Buffer)^, MySQLDataSize(fld^.ftype, fld^.length));
      if Field.DataType in [ftString{, ftWideString}] then
        begin
        Result := PChar(buffer)^ <> #0;
        if Result then
          // Terminate string (necessary for enum fields)
          PChar(buffer)[fld^.length] := #0;
        end
      else
        Result := True;
      break;
      end
    else
      Inc(CurBuf, MySQLDataSize(fld^.ftype, fld^.length));
    end;
end;

function TMySQLDataset.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurrentRecord=-1) and (RecordCount > 0) then
    Result:=1
  else
    Result:=FCurrentRecord+1;
end;

function TMySQLDataset.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  if RecordCount < 1 then
    Result := grEOF
  else
    begin
    Result := grOk;
    case GetMode of
      gmPrior:
        if FCurrentRecord <= 0 then
          begin
          Result := grBOF;
          FCurrentRecord := -1;
          end
        else
          Dec(FCurrentRecord);
      gmCurrent:
        if (FCurrentRecord<0) or (FCurrentRecord>=RecordCount) then
          Result := grError;
      gmNext:
        if FCurrentRecord>=RecordCount-1 then
          Result := grEOF
        else
          Inc(FCurrentRecord);
     end;
     if (Result=grOK) then
       begin
       LoadBufferFromData(Buffer);
       with PMySQLDatasetBookmark(Buffer + FRecordSize)^ do
         begin
         BookmarkData := FCurrentRecord;
         BookmarkFlag := bfCurrent;
         end;
       end
     else
       if (Result=grError) and (DoCheck) then
         DatabaseError(SerrNoData,Self);
     end;
end;

function TMySQLDataset.GetRecordCount: Integer;
begin
  Result:=mysql_num_rows(FMYSQLRES);
end;

function TMySQLDataset.GetRecordSize: Word;
begin
  Result:=FRecordSize;
end;

procedure TMySQLDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
begin

end;

procedure TMySQLDataset.InternalClose;
begin
  FCurrentRecord := -1;
  DoClose;
  if DefaultFields then
    DestroyFields;
end;

procedure TMySQLDataset.InternalDelete;
begin

end;

procedure TMySQLDataset.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TMySQLDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecord := PInteger(ABookmark)^;
end;

procedure TMySQLDataset.InternalHandleException;
begin
  //     Application.HandleException(self);
end;

procedure TMySQLDataset.InternalInitFieldDefs;

var
  I, FC: Integer;
  field: PMYSQL_FIELD;
  DFT: TFieldType;
  DFS: Integer;
  WasClosed: Boolean;

begin
  if FLoadingFieldDefs then Exit;
  FLoadingFieldDefs := True;
  try
    WasClosed := not IsCursorOpen;
    if WasClosed then
      begin
      DoQuery;
      DoGetResult;
      end;
    try
      FieldDefs.Clear;
      FC := mysql_num_fields(FMYSQLRES);
      for I := 0 to FC-1 do
        begin
        field := mysql_fetch_field_direct(FMYSQLRES, I);
        if MySQLFieldToFieldType(field^.ftype, field^.length, DFT, DFS) then
            TFieldDef.Create(FieldDefs, field^.name, DFT, DFS, False, I+1);
        end;
    finally
      if WasClosed then
        DoClose;
    end;
  finally
    FLoadingFieldDefs := False;
  end;
end;

procedure TMySQLDataset.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FBufferSize, 0);
end;

procedure TMySQLDataset.InternalLast;
begin
  FCurrentRecord := RecordCount;
end;

procedure TMySQLDataset.InternalOpen;
begin
  CheckDatabase;
  FMYSQLRES := nil;
  try
    DoQuery;
    DoGetResult;
    FCurrentRecord := -1;
    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    CalculateSizes;
    BindFields(True);
  except
    DoClose;
    raise;
  end;
  BookMarkSize:=SizeOf(Longint);
end;

procedure TMySQLDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentRecord := PMySQLDatasetBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TMySQLDataset.IsCursorOpen: Boolean;
begin
  Result:=(FMYSQLRES<>nil);
end;

procedure TMySQLDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PMySQLDatasetBookmark(Buffer + FRecordSize)^.BookmarkData := PInteger(Data)^;
end;

procedure TMySQLDataset.SetBookmarkFlag(Buffer: PChar;
  Value: TBookmarkFlag);
begin
  PMySQLDatasetBookmark(Buffer + FRecordSize)^.BookmarkFlag := Value;
end;

procedure TMySQLDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin

end;

procedure TMySQLDataset.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= RecordCount-1) then
    begin
    FCurrentRecord := Value-1;
    Resync([]);
    end;
end;

procedure TMySQLDataset.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
  FieldDefs.Clear;
end;

procedure TMySQLDataset.ExecSQL;
begin
  try
    DoQuery;
  finally
    DoClose;
  end;
end;


procedure TMySQLDataset.InternalPost;
begin

end;

function TMySQLDataset.MySQLFieldToFieldType(AType: enum_field_types; ASize: Integer;
   var NewType: TFieldType; var NewSize: Integer): Boolean;
begin
  Result := True;
  case AType of
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT, FIELD_TYPE_LONG, FIELD_TYPE_LONGLONG,
    FIELD_TYPE_INT24:
      begin
      NewType := ftInteger;
      NewSize := 0;
      end;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
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
      NewType := ftString;
      NewSize := ASize;
      end;
  else
    Result := False;
  end;
end;

procedure TMySQLDataset.CalculateSizes;
var
  I, FC: Integer;
  field: PMYSQL_FIELD;
begin
  FRecordSize := 0;
  FC := mysql_num_fields(FMYSQLRES);
  for I := 0 to FC-1 do
    begin
    field := mysql_fetch_field_direct(FMYSQLRES, I);
    FRecordSize := FRecordSize + MySQLDataSize(field^.ftype, field^.length);
    end;
  FBufferSize := FRecordSize + SizeOf(TMySQLDatasetBookmark);
end;

procedure TMySQLDataset.LoadBufferFromData(Buffer: PChar);

var
  I, FC, CT: Integer;
  field: PMYSQL_FIELD;
  row: TMYSQL_ROW;

begin
  mysql_data_seek(FMYSQLRES, FCurrentRecord);
  row := mysql_fetch_row(FMYSQLRES);
  if row = nil then
     MySQLError(FMySQL,SErrFetchingData,Self);
  FC := mysql_num_fields(FMYSQLRES);
  for I := 0 to FC-1 do
    begin
    field := mysql_fetch_field_direct(FMYSQLRES, I);
    CT := MySQLWriteFieldData(field^.ftype, field^.length, row^, Buffer);
    Inc(Buffer, CT);
    Inc(row);
    end;
end;


function TMySQLDataset.MySQLDataSize(AType: enum_field_types;
  ASize: Integer): Integer;
begin
  Result := 0;
  case AType of
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT, FIELD_TYPE_LONG, FIELD_TYPE_LONGLONG,
    FIELD_TYPE_INT24:
      begin
      Result := SizeOf(Integer);
      end;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      begin
      Result := SizeOf(Double);
      end;
    FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME:
      begin
      Result := SizeOf(TDateTime);
      end;
    FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin
      Result := ASize;
      end;
  end;
end;

function TMySQLDataset.MySQLWriteFieldData(AType: enum_field_types;
  ASize: Integer; Source, Dest: PChar): Integer;

var
  VI: Integer;
  VF: Double;
  VD: TDateTime;

begin
  Result := 0;
  case AType of
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT, FIELD_TYPE_LONG, FIELD_TYPE_LONGLONG,
    FIELD_TYPE_INT24:
      begin
      Result := SizeOf(Integer);
      if Source <> '' then
        VI := StrToInt(Source)
      else
        VI := 0;
      Move(VI, Dest^, Result);
      end;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      begin
      Result := SizeOf(Double);
      if Source <> '' then
        VF := InternalStrToFloat(Source)
      else
        VF := 0;
      Move(VF, Dest^, Result);
      end;
    FIELD_TYPE_TIMESTAMP:
      begin
      Result := SizeOf(TDateTime);
      if Source <> '' then
        VD := InternalStrToTimeStamp(Source)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_DATETIME:
      begin
      Result := SizeOf(TDateTime);
      if Source <> '' then
        VD := InternalStrToDateTime(Source)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_DATE:
      begin
      Result := SizeOf(TDateTime);
      if Source <> '' then
        VD := InternalStrToDate(Source)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_TIME:
      begin
      Result := SizeOf(TDateTime);
      if Source <> '' then
        VD := InternalStrToTime(Source)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin
      Result := ASize;
      if Source <> '' then
        Move(Source^, Dest^, Result)
      else
        Dest^ := #0;
      end;
  end;
end;

function TMySQLDataset.InternalStrToFloat(S: string): Extended;

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

function TMySQLDataset.InternalStrToDate(S: string): TDateTime;

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

function TMySQLDataset.InternalStrToDateTime(S: string): TDateTime;

var
  EY, EM, ED: Word;
  EH, EN, ES: Word;

begin
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 6, 2));
  ED := StrToInt(Copy(S, 9, 2));
  EH := StrToInt(Copy(S, 11, 2));
  EN := StrToInt(Copy(S, 14, 2));
  ES := StrToInt(Copy(S, 17, 2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result := 0
  else
    Result := EncodeDate(EY, EM, ED);
  Result := Result + EncodeTime(EH, EN, ES, 0);
end;

function TMySQLDataset.InternalStrToTime(S: string): TDateTime;

var
  EH, EM, ES: Word;

begin
  EH := StrToInt(Copy(S, 1, 2));
  EM := StrToInt(Copy(S, 4, 2));
  ES := StrToInt(Copy(S, 7, 2));
  Result := EncodeTime(EH, EM, ES, 0);
end;

function TMySQLDataset.InternalStrToTimeStamp(S: string): TDateTime;

var
  EY, EM, ED: Word;
  EH, EN, ES: Word;

begin
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 5, 2));
  ED := StrToInt(Copy(S, 7, 2));
  EH := StrToInt(Copy(S, 9, 2));
  EN := StrToInt(Copy(S, 11, 2));
  ES := StrToInt(Copy(S, 13, 2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result := 0
  else
    Result := EncodeDate(EY, EM, ED);
  Result := Result + EncodeTime(EH, EN, ES, 0);;
end;

procedure TMySQLDataset.DoClose;
begin
  try
    if FMYSQLRES <> nil then
       mysql_free_result(FMYSQLRES);
   finally
      FMYSQLRES := nil;
   end;
end;

procedure TMySQLDataset.DoQuery;
var
  Query: PChar;

begin
  Query := FSQL.GetText;
  try
    if mysql_query(FMySQL,Query) <> 0 then
      MySQLError(FMYSQL,SErrExecuting,Self);
  finally
    StrDispose(Query);
  end;
  FAffectedRows := mysql_affected_rows(FMYSQL);
  FLastInsertID := mysql_insert_id(FMYSQL);
end;

function TMySQLDataset.GetCanModify: Boolean;
begin
  Result := False;
end;

procedure TMySQLDataset.DoGetResult;
begin
  FMYSQLRES := mysql_store_result(FMYSQL);
  if (FMYSQLRES=nil) then
     MySQLError(FMYSQL,SErrGettingResult,Self);
  FAffectedRows := mysql_affected_rows(FMYSQL);
end;

function TMySQLDataset.FMySQL: PMySQL;
begin
  Result:=(Database as TMySQLDatabase).FMySQL;
end;

{ TMySQLDatabase }

function TMySQLDatabase.GetUserName: String;
begin
  result:=Params.values['UserName'];
end;

function TMySQLDatabase.GetHostName: String;
begin
  Result:=Params.Values['HostName'];
end;

procedure TMySQLDatabase.SetHostName(const AValue: String);
begin
  Params.Values['HostName']:=AValue;
end;

procedure TMySQLDatabase.SetUserName(Value: String);
begin
  Params.Values['UserName']:=Value;
end;

procedure TMySQLDatabase.SetPassword(Value: String);
begin
  Params.Values['Password']:=Value;
end;

function TMySQLDatabase.GetPassword: String;
begin
  Result:=Params.Values['Password'];
end;

function TMySQLDatabase.GetClientInfo: String;
begin
  Result:=strpas(mysql_get_client_info);
end;

procedure TMySQLDatabase.ConnectToServer;
Var
  H,U,P : String;

begin
  if (FMySQL=Nil) then
    New(FMySQL);
  H:=HostName;
  U:=UserName;
  P:=Password;
  mysql_init(FMySQL);
  FMySQL:=mysql_real_connect(FMySQL,PChar(H),PChar(U),Pchar(P),Nil,0,Nil,0);
  If (FMySQL=Nil) then
    MySQlError(Nil,SErrServerConnectFailed,Self);
  FServerInfo := strpas(mysql_get_server_info(FMYSQL));
  FHostInfo := strpas(mysql_get_host_info(FMYSQL));

end;

procedure TMySQLDatabase.SelectDatabase;
begin
  if mysql_select_db(FMySQL,pchar(DatabaseName))<>0 then
     MySQLError(FMySQL,SErrDatabaseSelectFailed,Self);
end;

procedure TMySQLDatabase.DoInternalConnect;
begin
  if (FMySQL<>nil) then
    DoInternalDisconnect;
  ConnectToServer;
  SelectDatabase;
end;

procedure TMySQLDatabase.DoInternalDisConnect;
begin
  mysql_close(FMySQL);
  FMySQL:=Nil;
  FServerInfo:='';
  FHostInfo:='';
end;

procedure TMySQLDatabase.StartTransaction;
begin
  // Nothing yet
end;

procedure TMySQLDatabase.EndTransaction;
begin
  // Nothing yet
end;

procedure TMySQLDatabase.CreateDatabase;

Var
  Disconnect : Boolean;

begin
  Disconnect:=(FMySQL=Nil);
  if Disconnect then
    ConnectToServer;
  try
    {if mysql_create_db(FMySQL,Pchar(DatabaseName))<>0 then
      MySQLError(FMySQL,SErrDatabaseCreate,Self);}
  Finally
    If Disconnect then
      DoInternalDisconnect;
  end;
end;

procedure TMySQLDatabase.DropDatabase;

Var
  Disconnect : Boolean;

begin
  Disconnect:=(FMySQL=Nil);
  if Disconnect then
    ConnectToServer;
  If (FMySQL=Nil) then
    ConnectToServer;
  try
{
    if mysql_drop_db(FMySQL,Pchar(DatabaseName))<>0 then
      MySQLError(FMySQL,SErrDatabaseDrop,Self);
}
  Finally
    If Disconnect then
      DoInternalDisconnect;
  end;
end;

function TMySQLDatabase.GetServerStatus: string;
begin
  CheckConnected;
  Result := mysql_stat(FMYSQL);
end;

end.
