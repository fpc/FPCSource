unit fpodbc;

{$mode objfpc}
{$h+}

interface

uses odbcsql,SysUtils,Classes;

Type
  TDSNTypes = (dtUser,dtSystem,dtBoth);
  TODBCParamType  = (ptUnknown,ptInput,ptInputOutput,ptResult,ptOutput,ptRetVal);
  TODBCParamTypes = Set of TODBCParamType;

  TODBCObject = Class(TComponent)
  Private
    FHandle : SQLHandle;
    FHandleType : SQLSmallint;
    Function GetHandle : SQLHandle;
    function GetHandleAllocated: Boolean;
    function GetExtendedErrorInfo: String;
  Protected
    Function CreateHandle : SQLHandle; Virtual;
    Function ParentHandle : SQLHandle; Virtual;
    Procedure FreeHandle;
    Function CheckODBC(Res : Integer;Msg : String) : Integer;
  Public
    Destructor Destroy; override;
    Property Handle : SQLHandle Read GetHandle;
    Property HandleAllocated : Boolean Read GetHandleAllocated;
  end;

  TODBCEnvironment = Class(TODBCObject)
  Private
    FODBCBehaviour : Integer;
    procedure SetODBCbehaviour(const Value: Integer);
    function GetNullTerminate: Boolean;
    procedure SetNullTerminate(const Value: Boolean);
  protected
    function CreateHandle: SQLHandle; override;
    Procedure SetIntAttribute(Const Attr,Value : Integer);
    Procedure SetStringAttribute(Const Attr: Integer; Value : String);
    Function GetIntAttribute(Const Attr : Integer) : Integer;
    Function GetStringAttribute(Const Attr : Integer) : String;
  Public
    Constructor Create(Aowner : TComponent);override;
    Function GetDriverNames(List : Tstrings) : Integer;
    Function GetDataSourceNames(List : Tstrings; Types : TDSNTypes;Descriptions : Boolean) : Integer;
    function GetDriverOptions(Driver: String; Options: TStrings): Integer;
    Property ODBCBehaviour : Integer Read FODBCBehaviour Write SetODBCbehaviour;
    Property NullTerminateStrings : Boolean Read GetNullTerminate Write SetNullTerminate;
  end;

  TConnectionBrowseEvent = Procedure (Sender : TObject;InParams,OutParams : Tstrings) of Object;

  TODBCConnection = Class(TODBCObject)
  Private
    FActive : Boolean;
    FDriverParams : TStrings;
    FDSN,
    FDriverName,
    FUserName,
    FPassword : String;
    FEnvironMent : TODBCEnvironment;
    FOnBrowseConnection : TConnectionBrowseEvent;
    FWindowHandle : integer;
    FDriverCOmpletion: SQLUSmallInt;
    function GetDriverName: String;
    function GetDriverParams: TStrings;
    procedure SetActive(const Value: Boolean);
    procedure SetDriverName(const Value: String);
    procedure SetDriverParams(const Value: TStrings);
    procedure SetDSN(const Value: String);
    function GetEnvironment: TODBCEnvironMent;
    procedure SetEnvironment(const Value: TODBCEnvironMent);
  Protected
    procedure ConnectToDriver;
    procedure ConnectToDSN;
    Procedure ConnectBrowsing;
    Function ParentHandle : SQLHandle; override;
    Procedure CheckActive;
    Procedure CheckInActive;
  Public
    Constructor Create(Aowner : TComponent);override;
    Destructor Destroy; override;
    Procedure Connect;
    Procedure Disconnect;
    Procedure GetTableNames(S : TStrings; SystemTables : Boolean);
    Procedure GetFieldNames(TableName : String; S : TStrings);
    Procedure GetPrimaryKeyFields(TableName : String; S : TStrings);
    procedure GetProcedureNames(S : TStrings);
    procedure GetProcedureParams(ProcName : String;ParamTypes : TODBCParamTypes; S : TStrings);
    Property DSN : String Read FDSN Write SetDSN;
    Property DriverName : String Read GetDriverName Write SetDriverName;
    Property DriverCompletion : SQLUSmallInt Read FDriverCOmpletion Write FDriverCompletion;
    Property DriverParams : TStrings Read GetDriverParams Write SetDriverParams;
    Property Active : Boolean Read FActive Write SetActive;
    Property Environment : TODBCEnvironMent Read GetEnvironment Write SetEnvironment;
    Property UserName : String Read FUserName Write FUserName;
    Property Password  : string Read FPassword Write FPassword;
    Property OnBrowseConnection : TConnectionBrowseEvent Read FonBrowseConnection Write FOnBrowseConnection;
    Property WindowHandle : integer Read FWindowHandle Write FWindowHandle;
  end;

  TODBCStatement = Class;

  TODBCFieldList = Class(TCollection)
  Private
    FStatement : TODBCStatement;
  Public
    Constructor Create(Statement : TODBCStatement);
  end;

  {
    TODBCStatement allocates 1 big data buffer. For each bound field
    two things are allocated in the buffer:
    - Size of fetched data as filled in by fetch.
    - data. (may be zero for blobs etc)
    The FBuffOffset contains the offset in the buffer of the size field.
    Data immediatly follows the size.
  }

  TODBCField = Class(TCollectionItem)
  Private
    FDecimalDigits,
    FPosition : SQLSmallInt;
    FName : String;
    FSize : SQLUInteger;       // Declared size, as returned by DescribeCol
    FNullable : Boolean;
    FDataType : SQLSmallInt;   // Declared type, as returned by DescribeCol
    FBuffOffSet : SQLInteger;  // Offset in data buffer.
    FBuffer : Pointer;         // Pointer to data.
    FBufSize : SQLInteger;     // Allocated buffer size.
    FBufType : SQLSmallInt;    // Allocated buffer type
    function GetAsString: String;
    function GetData : PChar;
    Function GetIsNull : Boolean;
    Function GetAsInteger : Integer;
    Function GetAsBoolean : Boolean;
    Function GetAsDouble : Double;
    Function GetAsDateTime : TDateTime;
  Public
    Property Position : SQLSmallint Read FPosition;
    Property Name : String read FName;
    Property DataType : SQLSmallInt read FDatatype;
    Property Size : SQLUinteger read FSize;
    property DecimalDigits : SQLSmallInt read FDecimalDigits;
    Property Nullable : Boolean Read FNullable;
    Property Data : Pchar Read GetData;
    Property BufType : SQLSmallInt Read FBufType;
    Property BufSize : SQLInteger Read FBufSize;
    Property IsNull : Boolean Read GetIsNull;
    Property AsString : String Read GetAsString;
    Property AsInteger : Integer Read GetAsInteger;
    Property AsBoolean : Boolean Read GetAsBoolean;
    Property AsDouble : Double Read GetAsDouble;
    Property AsDateTime : TDateTime Read GetAsDateTime;
  end;

  TODBCStatement = Class(TODBCObject)
  Private
    FBOF,FEOF : Boolean;
    FConnection: TODBCConnection;
    FFields : TODBCFieldList;
    FBuffer : Pointer;
  Protected
    Function ParentHandle : SQLHandle; override;
    procedure SetConnection(const Value: TODBCConnection);
    procedure AllocBuffers;
  Public
    Constructor Create(Aowner : TComponent);override;
    Destructor Destroy; override;
    Procedure BindFields(RestrictList : TStrings);virtual;
    Procedure ClearFields;virtual;
    Function Fetch : Boolean;
    Property Connection : TODBCConnection Read FConnection Write SetConnection;
    Property BOF : Boolean read FBOF;
    Property EOF : Boolean read FEOF;
    Property Fields : TODBCFieldList Read FFields;
  end;

  TODBCTableList = Class(TODBCStatement)
  Public
    Procedure GetTableNames(S : TStrings; SystemTables : Boolean);
  end;

  TODBCFieldNamesList = Class(TODBCStatement)
  Public
    Procedure GetFieldNames(TableName : String;S : TStrings);
  end;

  TODBCPrimaryKeyFieldsList = Class(TODBCStatement)
  Public
    Procedure GetPrimaryKeyFields(TableName : String;S : TStrings);
  end;

  TODBCProcedureList = Class(TODBCStatement)
  Public
    Procedure GetProcedureList(S : TStrings);
  end;

  TODBCProcedureParams = Class(TODBCStatement)
    Procedure GetProcedureParams(ProcName: String; ParamTypes: TODBCParamTypes; S: TStrings);
  end;

  TStatementState = (ssInactive,ssPrepared,ssBound,ssOpen);

  TODBCSQLStatement = Class(TODBCStatement)
  Private
    FSQL : TStrings;
    FState : TStatementState;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  Protected
    procedure FreeStatement(Option: SQLUSMALLINT);
    procedure ExecuteDirect;
    procedure ExecutePrepared;
    Procedure SetSQL(const Value: TStrings);
  Public
    Constructor Create(Aowner : TComponent);override;
    Destructor Destroy; override;
    procedure Prepare;
    procedure Unprepare;
    Procedure BindFields(RestrictList : TStrings);override;
    procedure ExecSQL;
    Procedure Open;
    Procedure Close;
    procedure GetFieldList(List: TStrings);
    Property Active : Boolean Read GetActive Write SetActive;
    Property SQL : TStrings Read FSQL Write SetSQL;
  end;

  EODBCError = Class(Exception);

Const
  ODBCParamTypeNames : Array [TODBCParamType] of string
                     = ('Unknown','Input','Input/Output','Result','Output','RetVal');

Function DefaultEnvironment : TODBCEnvironment;

implementation

{ TODBCObject }

resourcestring
  SErrUnexpected = 'Unexpected ODBC error:';
  SErrEnvironmentHandle = 'Cannot allocate environment handle:';
  SErrInvalidBehaviour = 'Invalid value for ODBC behaviour: %d';
  SErrNotConnected = 'Operation invalid when not connected.';
  SErrConnected = 'Operation invalid when connected.';
  SNeedDSNOrDriver = 'Cannot connect with empty DSN and driver names.';
  SErrGettingDataSources = 'Error getting datasources:';
  SErrGettingDriverNames = 'Error getting driver names:';
  SErrGettingDriverOptions = 'Error getting driver options:';
  SErrSettingEnvAttribute = 'Error setting environment attribute:';
  SErrGettingEnvAttribute = 'Error Getting environment attribute:';
  SErrBrowseConnecting = 'Error connecting to datasource via browse:';
  SErrDSNConnect = 'Error connecting to DSN:';
  SErrDriverConnect = 'Error connecting to driver:';
  SErrDisconnecting = 'Error disconnecting:';
  SErrNoConnectionForStatement = 'Missing connection for statement.';
  SErrNoSQLStatement = 'Missing SQL statement.';
  SErrPreparing = 'Error preparing statement:';
  SErrGettingTableNames = 'Error getting table names:';
  SErrFetchingData = 'Error fetching data:';
  SErrFieldNames = 'Error getting field names:';
  SErrPrimaryKeys = 'Error getting primary key names:';
  SErrProcedureNames = 'Error getting procedure names:';
  SErrExecuting = 'Error while executing statement:';
  SErrExecutingPrepared = 'Error while executing prepared statement:';
  SErrNotPrepared = 'Statement is not prepared';
  SErrNotInactive = 'Statement is already prepared or executed.';
  SErrStatementActive = 'A statement is still active';
  SErrColumnCount = 'Error retrieving cilumn count:';
  SErrColDescription = 'Error retrieving column description';
  SErrInvalidConversion = 'invalid type conversion';
  SErrBindCol = 'Error binding column';
Const
  ODBCSuccess = [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO];

Procedure ODBCError (Msg : String);

begin
  Raise EODBCError.Create(Msg);
end;

Procedure ODBCErrorFmt (Fmt : String;Args : Array of const);

begin
  Raise EODBCError.CreateFmt(Fmt,Args);
end;

Function CheckODBC(Res : Integer;Msg : String) : Integer;

begin
  Result:=Res;
  if not Res in [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO] then
    begin
    If MSG='' then
      MSG:=SErrUnexpected;
    ODBCErrorFmt(msg,[res]);
    end;
end;

function TODBCObject.CheckODBC(Res: Integer; Msg: String): Integer;

Var S : String;

begin
  Result:=Res;
  if not Res in [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO] then
    begin
    If MSG='' then
      MSG:=SErrUnexpected;
    S:=GetExtendedErrorInfo;
    If S<>'' then
      Msg:=Msg+LineEnding+S;
    ODBCError(msg);
    end;
end;

function TODBCObject.GetExtendedErrorInfo : String;

Var
  Res : SQLreturn;
  I,MsgLen : SQLSmallInt;
  SQLState : Array[0..6] of Char;
  NativeError : SQLInteger;
  MSg : Array[0..SQL_MAX_MESSAGE_LENGTH] of Char;
  SState,SMsg : String;

begin
   I:=0;
   Result:='';
   Repeat
     Inc(i);
     Res:=SQLGetDiagRec(FhandleType, FHandle, i, SqlState, NativeError,
            Msg, sizeof(Msg), MsgLen);
     If Res<>SQL_NO_DATA then
       begin
       SState:=SQLState;
       SMsg:=Msg;
       If Length(Result)>0 then
         Result:=Result+LineEnding;
       Result:=Result+Format('[%s] : %s (%d)',[SState,SMsg,NativeError]);
       end;
   Until (Res=SQL_NO_DATA);
end;




function TODBCObject.CreateHandle: SQLHandle;
begin
{$ifdef debug}
  Writeln(Classname,': Creating handle of type ',FHAndleType,' and parent ',ParentHandle);
{$endif}
  CheckODBC(SQLAllocHandle(FHandleType,ParentHandle,FHandle),SErrEnvironmentHandle);
  Result:=FHandle;
end;


destructor TODBCObject.Destroy;
begin
  If FHandle<>0 then
    FreeHandle;
  inherited;
end;

procedure TODBCObject.FreeHandle;
begin
  If FHandle<>0 then
    begin
    SQLFreeHandle(FHandleType,FHandle);
    FHandle:=0;
    end;
end;

function TODBCObject.GetHandle: SQLHandle;
begin
  If FHandle=0 then
    CreateHandle;
  Result:=FHandle;
end;

function TODBCObject.GetHandleAllocated: Boolean;
begin
  Result:=(FHandle<>0)
end;

function TODBCObject.ParentHandle: SQLHandle;
begin
  Result:=SQL_NULL_HANDLE;
end;

{ TODBCEnvironment }

constructor TODBCEnvironment.Create(Aowner: TComponent);
begin
  FHandleType:=SQL_HANDLE_ENV;
  inherited;
end;

function TODBCEnvironment.CreateHandle: SQLHandle;
begin
  Result:=Inherited CreateHandle;
  ODBCbehaviour:=SQL_OV_ODBC3;
end;

function TODBCEnvironment.GetDataSourceNames(List: Tstrings;
  Types: TDSNTypes;Descriptions : Boolean): Integer;

var
  DSNName,
  DSNDesc: array[0..SQL_MAX_OPTION_STRING_LENGTH] of Char;
  lenn,lend : SQLSmallInt;
  Dir : SQLSmallInt;
  Sn,SD : String;

begin
  Case Types of
    dtSystem : Dir:=SQL_FETCH_FIRST_SYSTEM;
    dtUser : Dir:=SQL_FETCH_FIRST_USER;
    dtBoth : Dir:=SQL_FETCH_FIRST;
  end;
  List.Clear;
  CheckODBC(SQLDatasources(Handle, Dir,
                           DSNName,SQL_MAX_OPTION_STRING_LENGTH, @lenn,
                           DSNDesc,SQL_MAX_OPTION_STRING_LENGTH, @lend),SErrGettingDataSources);
  Repeat
    If Not Descriptions then
      List.Add(DSNName)
    else
      begin
      SN:=DSNName;
      SD:=DSNDesc;
      List.Add(SN+'='+SD);
      end;
  Until Not (SQLDataSources(Handle, SQL_FETCH_NEXT,
                            DSNName, SQL_MAX_OPTION_STRING_LENGTH, @lenn,
                            DSNDesc,SQL_MAX_OPTION_STRING_LENGTH, @lend) in ODBCSuccess);
  Result:=List.Count;
end;

function TODBCEnvironment.GetDriverNames(List : Tstrings): Integer;

Var
  DriverName: array[0..SQL_MAX_OPTION_STRING_LENGTH] of Char;
  len : SQLSmallInt;

begin
  List.Clear;
  CheckODBC(SQLDrivers(Handle, SQL_FETCH_FIRST, DriverName,
        SQL_MAX_OPTION_STRING_LENGTH, @len, Nil,0,Nil),SErrGettingDriverNames);
  Repeat
    List.Add(DriverName);
  Until Not (SQLDrivers(Handle, SQL_FETCH_NEXT, DriverName,
        SQL_MAX_OPTION_STRING_LENGTH, @len, Nil,0,Nil) in ODBCSuccess);
  Result:=List.Count;
end;

function TODBCEnvironment.GetDriverOptions(Driver : String;Options: Tstrings): Integer;

Var
  DriverName,
  DriverOptions: array[0..SQL_MAX_OPTION_STRING_LENGTH] of Char;
  lenn,leno : SQLSmallInt;
  Found : Boolean;
  P : PChar;
  S : string;

begin
  CheckODBC(SQLDrivers(Handle, SQL_FETCH_FIRST, DriverName,
        SQL_MAX_OPTION_STRING_LENGTH, @lenn, DriverOptions,
        SQL_MAX_OPTION_STRING_LENGTH,@Leno),SErrGettingDriverOptions);
  Result:=0;
  Options.Clear;
  Repeat
    Found:=CompareText(Driver,DriverName)=0;
    If Found then
      begin
      P:=@DriverOptions[0];
      While P[0]<>#0 do
        begin
        S:=StrPas(P);
        options.Add(S);
        Inc(P,Length(S)+1);
        end;
      end;
  Until Not (SQLDrivers(Handle, SQL_FETCH_NEXT, DriverName,
        SQL_MAX_OPTION_STRING_LENGTH, @lenn, DriverOptions,
        SQL_MAX_OPTION_STRING_LENGTH,@Leno) in ODBCSuccess) or Found;
  Result:=Options.Count;
end;

function TODBCEnvironment.GetIntAttribute(const Attr: Integer): Integer;
begin
  CheckODBC(SQLSetEnvAttr(Handle,Attr,SQLPointer(@result),0),SErrSettingEnvAttribute);
end;

function TODBCEnvironment.GetNullTerminate: Boolean;
begin
  Result:=(GetIntAttribute(SQL_ATTR_OUTPUT_NTS)=SQL_TRUE);
end;

function TODBCEnvironment.GetStringAttribute(const Attr: Integer): String;

Var
  OldLen,Len: Integer;

begin
  OldLen:=0;
  Repeat
    Inc(OldLen,255);
    SetLength(Result,OldLen);
    CheckODBC(SQLGetEnvAttr(Handle,Attr,SQLPointer(@result),OldLen,@Len),SErrGettingEnvAttribute);
  until (Len<=OldLen);
  SetLength(Result,Len);
end;

procedure TODBCEnvironment.SetIntAttribute(const Attr, Value: Integer);
begin
  CheckODBC(SQLSetEnvAttr(Handle,Attr,SQLPointer(Value),0),SErrSettingEnvAttribute);
end;

procedure TODBCEnvironment.SetNullTerminate(const Value: Boolean);
begin
  If Value then
    SetIntAttribute(SQL_ATTR_OUTPUT_NTS,SQL_TRUE)
  else
    SetIntAttribute(SQL_ATTR_OUTPUT_NTS,SQL_FALSE);
end;

procedure TODBCEnvironment.SetODBCbehaviour(const Value: Integer);
begin
  If (Value<>FODBCBehaviour) then
    begin
    If Not (Value in [SQL_OV_ODBC3,SQL_OV_ODBC2]) Then
      ODBCErrorFmt(SErrInvalidBehaviour,[Value]);
    SetIntAttribute(SQL_ATTR_ODBC_VERSION,Value);
    FODBCBehaviour := Value;
    end;
end;

procedure TODBCEnvironment.SetStringAttribute(const Attr: Integer;
  Value: String);
begin
  CheckODBC(SQLSetEnvAttr(Handle,Attr,SQLPointer(Value),Length(Value)),SErrSettingEnvAttribute);
end;

{ TODBCConnection }

procedure TODBCConnection.CheckActive;
begin
  If Not FActive then
    ODBCError(SErrNotConnected);
end;

procedure TODBCConnection.CheckInActive;
begin
  If FActive then
    ODBCError(SErrConnected);
end;

procedure TODBCConnection.Connect;
begin
  If Not FActive then
    begin
    If Assigned (FonBrowseConnection) then
      ConnectBrowsing
    else If (FDSN<>'') then
      ConnectToDSN
    else if FDriverName<>'' then
      ConnectToDriver
    else
      ODBCError(SNeedDSNOrDriver);
    FActive:=True;
    end;
end;

Function ListToBuf(List : Tstrings; Buf : PChar; Sep : Char; MaxLen : Integer) : Boolean;

Var
  P : PChar;
  S : String;
  I,Len : Integer;

begin
  P:=Buf;
  I:=0;
  Result:=True;
  While Result and (I<List.Count) do
    begin
    S:=List[i];
    If I<List.Count-1 then
      S:=S+Sep;
    Len:=Length(S);
    Result:=(Longint(P-Buf)+Len)<=MaxLen;
    If Result then
      begin
      Move(S[1],P^,Len);
      Inc(P,Len);
      end;
    Inc(i);
    end;
  P[0]:=#0;
end;

Function BufToList(Buf : PChar;MaxLen : Integer;List : Tstrings;Sep : Char) : Integer;

Var
  S : String;
  P : PChar;
  Totlen,Len : Integer;

begin
  List.Clear;
  Result:=0;
  P:=Buf;
  TotLen:=0;
  While (P[0]<>#0) or (totlen<Maxlen) do
    begin
    Len:=0;
    While Not (P[len] in [Sep,#0]) do
      Inc(len);
    SetLength(S,Len);
    List.Add(S);
    Move(P[0],S[1],Len);
    Inc(P,Len);
    If P[0]<>#0 then
      Inc(P,1);
    inc(Totlen,Len+1);
    end;
  Result:=List.Count;
end;


Procedure TODBCConnection.ConnectBrowsing;

Var
  Inlist,OutList : TStringList;
  InStr,
  OutStr: Array[0..SQL_MAX_OPTION_STRING_LENGTH] of Char;
  i,Res : Integer;
  olen : SQLSmallint;

begin
  InList:=TStringList.Create;
  OutList:=TstringList.Create;
  try
    If FDSN<>'' then
      InList.Add('DSN='+FDSN)
    else If FDriverName<>'' then
      begin
      Inlist.Add('DRIVER='+FDriverName);
      For I:=0 to DriverParams.Count-1 do
        Inlist.Add(DriverParams[i]);
      end;
    Repeat
      ListToBuf(Inlist,Instr,';',SQL_MAX_OPTION_STRING_LENGTH);
      Res:=SQLBrowseConnect(Handle,Instr,SQL_NTS,Outstr,SQL_MAX_OPTION_STRING_LENGTH,Olen);
      If RES=SQL_NEED_DATA then
        begin
        OutList.Clear;
        BufToList(OutStr,Olen,OutList,';');
        FOnBrowseConnection(Self,InList,OutList);
        end
    Until (Res<>SQL_NEED_DATA);
    CheckODBC(Res,SErrBrowseConnecting);
  Finally
    Outlist.free;
    InList.Free;
  end;
end;

Procedure TODBCConnection.ConnectToDSN;
begin
  CheckODBC(SQLConnect(Handle,PSQLChar(FDSN),SQL_NTS,
                    PSQLChar(FUserName),SQL_NTS,
                    PSQLChar(FPassword),SQL_NTS),SErrDSNConnect);
end;


Procedure TODBCConnection.ConnectToDriver;

Var
  Instr,
  OutStr :  Array[0..SQL_MAX_OPTION_STRING_LENGTH] of Char;
  OLen : SQLSmallint;
  InList : TStringList;

begin
  InList:=TStringList.Create;
  Try
    Inlist.Assign(DriverParams);
    Inlist.Insert(0,'DRIVER={'+DRIVERNAME+'}');
    ListToBuf(Inlist,InStr,';',SQL_MAX_OPTION_STRING_LENGTH);
  Finally
    Inlist.Free;
  end;
  CheckODBC(SQLDriverConnect(Handle,FWindowHandle,
               Instr,SQL_NTS,
               OutStr,SQL_MAX_OPTION_STRING_LENGTH,
               Olen,FDriverCompletion),SErrDriverConnect);
end;

constructor TODBCConnection.Create(Aowner: TComponent);
begin
  inherited;
  FHandleType:=SQL_HANDLE_DBC;
  FDriverParams:=TStringList.Create;
  FDriverCompletion:=SQL_DRIVER_NOPROMPT;
end;

destructor TODBCConnection.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TODBCConnection.Disconnect;
begin
  If FActive then
    begin
    CheckODBC(SQLDisconnect(Handle),SErrDisconnecting);
    Factive:=False;
    end;
end;

function TODBCConnection.GetDriverName: String;
begin
  Result:=FDriverName;
end;

function TODBCConnection.GetDriverParams: TStrings;
begin
  Result:=FDriverParams;
end;

function TODBCConnection.GetEnvironment: TODBCEnvironMent;
begin
  If FEnvironment=Nil then
    result:=DefaultEnvironment
  else
    Result:=FEnvironment;
end;

procedure TODBCConnection.SetActive(const Value: Boolean);
begin
  If Value then
    Connect
  else
    Disconnect;
end;

procedure TODBCConnection.SetDriverName(const Value: String);
begin
  CheckInactive;
  FDSN:='';
  If CompareText(FDriverName,Value)<>0 then
    begin
    FDriverName:=Value;
    FDriverParams.Clear;
    end;
end;

procedure TODBCConnection.SetDriverParams(const Value: TStrings);
begin
  CheckInactive;
  FDriverParams.Assign(Value);
end;

procedure TODBCConnection.SetDSN(const Value: String);
begin
  CheckInactive;
  FDSN := Value;
end;

procedure TODBCConnection.SetEnvironment(const Value: TODBCEnvironMent);
begin
  CheckInactive;
  If (Value<>Environment) then // !! may be defaultenvironment...
    begin
    If HandleAllocated then
      FreeHandle;
    FEnvironment:=Value
    end;
end;


function TODBCConnection.ParentHandle: SQLHandle;
begin
  Result:=Environment.Handle
end;

Const
  DefEnv : Pointer = Nil;

Function DefaultEnvironment : TODBCEnvironment;

begin
  If DefEnv=Nil then
    DefEnv:=TODBCEnvironment.Create(Nil);
  Result:=TODBCEnvironment(DefEnv);
end;

procedure TODBCConnection.GetTableNames(S: TStrings;
  SystemTables: Boolean);
begin
  With TODBCTableList.Create(Self) do
    try
      GetTableNames(S,SystemTables);
    finally
      Free;
    end;
end;

procedure TODBCConnection.GetFieldNames(TableName: String; S: TStrings);
begin
  With TODBCFieldNamesList.Create(Self) do
    try
      GetFieldNames(TableName,S);
    finally
      Free;
    end;
end;

procedure TODBCConnection.GetPrimaryKeyFields(TableName: String;
  S: TStrings);
begin
  With TODBCPrimaryKeyFieldsList.Create(Self) do
    try
      GetPrimaryKeyFields(TableName,S);
    finally
      Free;
    end;
end;

procedure TODBCConnection.GetProcedureNames(S: TStrings);
begin
  With TODBCProcedureList.Create(Self) do
    try
      GetProcedureList(S);
    Finally
      Free;
    end;
end;

procedure TODBCConnection.GetProcedureParams(ProcName: String;
  ParamTypes: TODBCParamTypes; S: TStrings);
begin
  With TODBCProcedureParams.Create(Self) do
    Try
      GetProcedureParams(ProcName,Paramtypes,S);
    finally
      Free;
    end;
end;

{ TODBCStatement }

Type
  TODBCFieldBufRec = Record
    T{ype}    : SQlSmallint;
    B{ufsize} : SQLInteger;
    {Buftyp}e : SQLSmallint;
  end;

Const
  BufDescrCount = 26;
  BufDescr : Array[1..BufDescrCount] of TODBCFieldBufRec =
  { Type                Bufsize              Buftype }
  (
  (T:SQL_CHAR          ;b:-1                  ;e: SQL_CHAR),
  (T:SQL_NUMERIC       ;b:sizeof(SQLDouble)   ;e: SQL_DOUBLE),
  (T:SQL_DECIMAL       ;b:sizeof(SQLDouble)   ;e: SQL_DOUBLE),
  (T:SQL_INTEGER       ;b:sizeof(SQLInteger)  ;e: SQL_INTEGER),
  (T:SQL_SMALLINT      ;b:sizeof(SQLSmallInt) ;e: SQL_SMALLINT),
  (T:SQL_FLOAT         ;b:sizeof(SQLDOUBLE)   ;e: SQL_DOUBLE),
  (T:SQL_REAL          ;b:sizeof(SQLDOUBLE)   ;e: SQL_DOUBLE),
  (T:SQL_DOUBLE        ;b:Sizeof(SQLDOUBLE)   ;e: SQL_DOUBLE),
  (T:SQL_DATE          ;b:Sizeof(SQL_DATE_STRUCT) ;e: SQL_DATE),
  (T:SQL_TIME          ;b:sizeof(SQL_TIME_STRUCT) ;e: SQL_TIME),
  (T:SQL_TIMESTAMP     ;b:sizeof(SQL_TIMESTAMP_STRUCT) ;e: SQL_TIMESTAMP),
  (T:SQL_VARCHAR       ;b:-1                  ;e: SQL_CHAR),
  (T:SQL_UNKNOWN_TYPE  ;b:0                   ;e: SQL_UNKNOWN_TYPE),
  (T:SQL_LONGVARCHAR   ;b:-1                  ;e: SQL_CHAR),
  (T:SQL_BINARY        ;b:-2                  ;e: SQL_BINARY),
  (T:SQL_VARBINARY     ;b:-2                  ;e: SQL_BINARY),
  (T:SQL_LONGVARBINARY ;b:-2                  ;e: SQL_BINARY),
  (T:SQL_BIGINT        ;b:sizeOf(SQLDOUBLE)   ;e: SQL_DOUBLE),
  (T:SQL_TINYINT       ;b:Sizeof(SQLSMALLINT) ;e: SQL_SMALLINT),
  (T:SQL_BIT           ;b:sizeof(SQL_CHAR)    ;e: SQL_BIT),
  (T:SQL_WCHAR         ;b:-1                  ;e: SQL_CHAR),
  (T:SQL_WVARCHAR      ;b:-1                  ;e: SQL_CHAR),
  (T:SQL_WLONGVARCHAR  ;b:-1                  ;e: SQL_CHAR),
  (T:SQL_TYPE_DATE     ;b:sizeof(SQL_DATE_STRUCT) ;e: SQL_TYPE_DATE),
  (T:SQL_TYPE_TIME     ;b:sizeof(SQL_TIME_STRUCT) ;e: SQL_TYPE_TIME),
  (T:SQL_TYPE_TIMESTAMP;b:sizeof(SQL_TIMESTAMP_STRUCT) ;e: SQL_TYPE_TIMESTAMP)
  );
{  // template
  (T: ;b: ;e: ),
}

Function GetColSizeBufType (Coltype: SQLSmallint;
                             Var BufSize : SQLInteger;
                             Var BufType : SQLSmallInt) : Boolean;
Var
  I : Integer;

begin
  I:=0;
  BufSize:=0;
  BufType:=0;
  While (I<=BufDescrCount) and (BufDescr[i].t<>Coltype) do
    Inc(i);
  Result:=(i<=BufDescrCount);
  If Result then
    begin
    BufSize:=BufDescr[i].b;
    BufType:=BufDescr[i].e;
    end;
end;


procedure TODBCStatement.BindFields(RestrictList : TStrings);

Var
  Count: SQLSmallInt;
  CName : Array[0..SQL_NAME_LEN] of Char;
  CSize : SQLUINTEGER;
  CDataType,CDecimals,CNullable,CNameLen: SQLSmallInt;
  I : integer;

begin
  CheckODBC(SQLNumResultCols(Handle,Count),SErrColumnCount);
  For I:=1 to Count do
    begin
    CheckODBC(SQLDescribeCol(Handle,i,CName,SQL_NAME_LEN,CNameLen,
                             CdataType,CSize, CDecimals,CNullable)
              ,SErrColDescription);
    If Not Assigned(RestrictList) or (RestrictList.IndexOf(Cname)<>-1) then
      With FFields.Add as TODBCField do
        begin
        FPosition:=I;
        FName:=Cname;
        FDataType:=CDataType;
        FSize:=CSize;
        FDecimalDigits:=CDecimals;
        FNullable:=(CNullable=SQL_TRUE);
        GetColsizeBufType(FDataType,FBufSize,FBufType);
        If FBufSize=-1 then
          FBufSize:=FSize;
        end;
    end;
  AllocBuffers;
  For I:=0 to Count-1 do
    With FFields.Items[i] as TODBCField do
      CheckODBC(SQLBindCol(Handle,FPosition,FBufType,GetData,FBufSize,FBuffer+FBuffOffset)
               ,SErrBindCol);

end;

procedure TODBCStatement.ClearFields;
begin
  FFields.Clear;
end;

constructor TODBCStatement.Create(Aowner: TComponent);
begin
  FHandleType:=SQL_HANDLE_STMT;
  inherited;
  If AOwner is TODBCConnection then
    Connection:=TODBCConnection(Aowner);
  FFields:=TODBCFieldList.Create(Self);
end;

function TODBCStatement.ParentHandle: SQLHandle;
begin
   If (Connection=Nil) then
     ODBCError(SErrNoConnectionForStatement);
   Result:=Connection.Handle;
end;

procedure TODBCStatement.SetConnection(const Value: TODBCConnection);
begin
  If Value<>FConnection then
    begin
    If HandleAllocated then
      FreeHandle;
    FConnection := Value;
    end;
end;

Function TODBCStatement.fetch : Boolean;

Var
  res : SQLReturn;

begin
  Res:=SQLFetch(Handle);
  Result:=(Res=SQL_SUCCESS);
  If Not Result and (Res<>SQL_NO_DATA) then
    CheckODBC(Res,SErrFetchingData);
  FBof:=False;
  If (Res=SQL_NO_DATA) then
    FEOF:=True;
end;

destructor TODBCStatement.Destroy;
begin
  FFields.Free;
  inherited;
end;

{ TODBCSQLStatement }

procedure TODBCSQLStatement.GetFieldList(List : TStrings);

Var
  Count: SQLSmallInt;
  CName : Array[0..SQL_NAME_LEN] of Char;
  CSize : SQLUINTEGER;
  CDataType,CDecimals,CNullable,CNameLen: SQLSmallInt;
  I : integer;

begin
  if Not (FState in [ssPrepared,ssBound,ssOpen]) then
    ODBCError(SErrNotPrepared);
  List.Clear;
  CheckODBC(SQLNumResultCols(Handle,Count),SErrColumnCount);
  For I:=1 to Count do
    begin
    CheckODBC(SQLDescribeCol(Handle,i,CName,SQL_NAME_LEN,CNameLen,
                             CdataType,CSize, CDecimals,CNullable)
              ,SErrColDescription);
    List.Add(CName);
    end;
end;


procedure TODBCSQLStatement.Unprepare;

begin
  Case FState of
    ssBound,ssOpen :
              begin
              ClearFields;
              FreeStatement(SQL_CLOSE);
              end;
    ssPrepared : begin
                 FreeStatement(SQL_CLOSE);
                 end;
  end;
  FState:=ssInactive;
end;

procedure TODBCSQLStatement.FreeStatement(Option : SQLUSMALLINT);

begin
  SQLFreeStmt(Handle,SQL_CLOSE);
end;

procedure TODBCSQLStatement.Close;
begin
  if FState<>ssInactive then
    begin
    Unprepare;
    FreeStatement(SQL_CLOSE);
    FState:=ssInactive;
    end;
end;

constructor TODBCSQLStatement.Create(Aowner: TComponent);
begin
  inherited;
  FSQL:=TStringList.Create;
end;

destructor TODBCSQLStatement.Destroy;
begin
  if FState=ssOpen then
    Close
  else If FState<>ssInactive then
    Unprepare;
  FSQL.Free;
  inherited;
end;

procedure TODBCSQLStatement.ExecSQL;
begin
  Case FState of
    ssPrepared,ssBound : ExecutePrepared;
    ssInactive : ExecuteDirect;
  else
    Raise Exception.Create(SErrStatementActive)
  end;
end;

procedure TODBCSQLStatement.ExecuteDirect;

Var
  S : String;

begin
  if FState<>ssInactive then
    ODBCError(SErrStatementActive);
  S:=SQL.Text;
  CheckODBC(SQLExecDirect(Handle,PChar(S),SQL_NTS),SErrExecuting);
end;

procedure TODBCSQLStatement.ExecutePrepared;
begin
  if Not (FState in [ssPrepared,ssBound]) then
    ODBCError(SErrNotPrepared);
  CheckODBC(SQLExecute(Handle),SErrExecutingPrepared);
end;

function TODBCSQLStatement.GetActive: Boolean;
begin
  Result:=(FState=ssOpen);
end;

procedure TODBCSQLStatement.Open;
begin
  if (FState<>ssOpen) then
    begin
    Writeln('Preparing');
    If FState=ssInactive then
      Prepare;
    Writeln('Bind fields');
    if FState=ssPrepared then
      BindFields(Nil);
    Writeln('Executing');
    ExecSQL;
    Writeln('Fetching');
    If FState=ssBound then
      Fetch;
    FState:=ssOpen;
    FBOF:=True;
    end;
end;

procedure TODBCSQLStatement.Prepare;

Var
  S : String;

begin
  If FState<>ssInactive then
    ODBCError(SErrNotInactive);
  If (FSQL.Count=0) then
    ODBCError(SErrNoSQLStatement);
  S:=FSQL.Text;
  CheckODBC(SQLPrepare(Handle,PChar(S),SQL_NTS),SErrPreparing);
  FState:=ssPrepared;
end;

procedure TODBCSQLStatement.SetActive(const Value: Boolean);
begin
  If Value then
    Open
  else
    Close;
end;

procedure TODBCSQLStatement.SetSQL(const Value: TStrings);

begin
  FSQL.Assign(Value);
end;


procedure TODBCSQLStatement.BindFields(RestrictList: TStrings);
begin
  inherited;
  FState:=ssBound;
end;


procedure TODBCStatement.AllocBuffers;

Var
  I,TotalSize,AddSize : Integer;

begin
  TotalSize:=0;
  For i:=0 to FFields.Count-1 do
    With (FFields.Items[i] as TODBCField) do
        begin
        AddSize:=FBufSize;
        If FBufSize=-2 then // Blob.
          AddSize:=0
        else if FBufSize=-1 then
          AddSize:=FSize+1; // some Char variant.
        // Store offset temporarily in FData
        FBuffOffset:=TotalSize;
        Inc(TotalSize,AddSize+SizeOf(SQLinteger));
        end;
  FBuffer:=GetMem(TotalSize);
  TotalSize:=0;
  For i:=0 to FFields.Count-1 do
    With (FFields.Items[i] as TODBCField) do
      FBuffer:=Self.FBuffer;
end;

{ TODBCTableList }

procedure TODBCTableList.GetTableNames(S: TStrings; SystemTables : Boolean);

var
  TName,
  TType: array[0..SQL_NAME_LEN+1] of char;
  NL,TL: SQLINTEGER;
  Res: SQLRETURN;

begin
  S.Clear;
  Res:=CheckODBC(SQLTables(handle, nil,0,nil,0,nil,0,nil,0),SErrGettingTableNames);
  if Res=SQL_SUCCESS then
    begin
    // Must bind by colno, because names changed between ODBC 2.0 and 3.0 !!
    SQLBindCol(handle,3,SQL_CHAR,@TName,SQL_NAME_LEN,@NL);
    SQLBindCol(handle,4,SQL_CHAR,@TType,SQL_NAME_LEN,@TL);
    While Fetch do
      if SystemTables or (CompareText(TType,'SYSTEM TABLE')<>0) then
         S.Add(TName);
    end;
end;

{ TODBCFieldNamesList }

procedure TODBCFieldNamesList.GetFieldNames(TableName: String;
  S: TStrings);

var
  FName : array[0..SQL_NAME_LEN+1] of char;
  NF : SQLINTEGER;
  Res: SQLRETURN;

begin
  S.Clear;
  Res:=CheckODBC(SQLColumns(handle, nil, 0, nil, 0, pchar(TableName), SQL_NTS, nil, 0),SErrFieldNames);
  if Res=SQL_SUCCESS then
    begin
    SQLBindCol(handle, 4, SQL_CHAR, @FNAme, SQL_NAME_LEN, @NF);
    While Fetch do
       S.Add(FName);
    end;
end;

{ TODBCPrimaryKeyFieldsList }

procedure TODBCPrimaryKeyFieldsList.GetPrimaryKeyFields(TableName: String;
  S: TStrings);
var
  FName : array[0..SQL_NAME_LEN+1] of char;
  NF : SQLINTEGER;
  Res: SQLRETURN;

begin
  S.Clear;
  Res:=CheckODBC(SQLPrimaryKeys(handle, nil, 0, nil, 0, pchar(TableName), SQL_NTS),SErrPrimaryKeys);
  if Res=SQL_SUCCESS then
    begin
    SQLBindCol(handle, 4, SQL_CHAR, @FNAme, SQL_NAME_LEN, @NF);
    While Fetch do
       S.Add(FName);
    end;

end;

{ TODBCProcedureList }

procedure TODBCProcedureList.GetProcedureList(S: TStrings);

var
  PName : array[0..SQL_NAME_LEN+1] of char;
  NP : SQLINTEGER;
  Res: SQLRETURN;

begin
  S.Clear;
  Res:=CheckODBC(SQLProcedures(handle, nil, 0, nil, 0, Nil, 0),SErrProcedureNames);
  if Res=SQL_SUCCESS then
    begin
    SQLBindCol(handle, 3, SQL_CHAR, @PNAme, SQL_NAME_LEN, @NP);
    While Fetch do
      S.Add(PName);
    end;

end;

{ TODBCProcedureParams }

procedure TODBCProcedureParams.GetProcedureParams(ProcName: String;
  ParamTypes: TODBCParamTypes; S: TStrings);

var
  PName : array[0..SQL_NAME_LEN+1] of char;
  NP,NT : SQLINTEGER;
  Ptype : SQLSmallInt;
  Res: SQLRETURN;

begin
  S.Clear;
  Res:=CheckODBC(SQLProcedureColumns(handle, nil, 0, nil, 0, PChar(ProcName),SQL_NTS,Nil, 0),SErrProcedureNames);
  if Res=SQL_SUCCESS then
    begin
    SQLBindCol(handle, 4, SQL_CHAR, @PName, SQL_NAME_LEN, @NP);
    SQLBindCol(handle, 5, SQL_SMALLINT, @PType, SizeOf(SQLSmallInt), @NT);
    While Fetch do
      begin
      If TODBCParamType(PType) in ParamTypes then
        S.Add(PName);
      end;
    end;
end;

{ TODBCFieldList }

constructor TODBCFieldList.Create(Statement: TODBCStatement);
begin
  FStatement:=Statement;
  Inherited Create(TODBCField);
end;

{ TODBCField }

function TODBCField.GetAsString: String;
begin
  If IsNull then
    Result:=''
  else
    Case FBufType of
      SQL_Smallint : Result:=IntToStr(PSQLSmallInt(Data)^);
      SQL_Integer  : Result:=IntToStr(PSQLINTEGER(Data)^);
      SQL_BIT      : Result:=IntToStr(PByte(Data)^);
      SQL_CHAR     : Result:=StrPas(Data);
      SQL_DOUBLE   : Result:=FloatToStr(GetAsDouble);
      SQL_DATE     : result:=DateToStr(AsDateTime);
      SQL_TIME     : Result:=TimeToStr(AsDateTime);
      SQL_TIMESTAMP : result:=datetimeToStr(AsDateTime);
      SQL_TYPE_DATE  : result:=dateToStr(AsDateTime);
      SQL_TYPE_TIMESTAMP : result:=datetimeToStr(AsDateTime);
      SQL_TYPE_TIME : Result:=TimeToStr(AsDateTime);
    else
      ODBCError(SErrInvalidConversion)
    end;
end;

function TODBCField.GetData : Pchar;

begin
  Result:=FBuffer+FBuffOffset+SizeOf(SQLinteger);
end;

function TODBCField.GetIsNull : boolean;

begin
  Result:=PSQLinteger(FBuffer+FBuffOffset)^=SQL_NULL_DATA;
end;

Function TODBCField.GetAsInteger : Integer;

begin
  If IsNull then
    Result:=0
  else
    Case FBufType of
      SQL_Smallint : Result:=PSQLSmallInt(Data)^;
      SQL_Integer  : Result:=PSQLINTEGER(Data)^;
      SQL_BIT      : Result:=PByte(Data)^;
      SQL_CHAR     : Result:=StrToInt(GetAsString);
      SQL_DOUBLE   : Result:=Round(GetAsDouble);
      SQL_DATE,
      SQL_TIME,
      SQL_TIMESTAMP,
      SQL_TYPE_DATE,
      SQL_TYPE_TIMESTAMP,
      SQL_TYPE_TIME : Result:=Round(AsDateTime);
    else
      ODBCError(SErrInvalidConversion)
    end;
end;

Function TODBCField.GetAsBoolean : Boolean;

begin
  If IsNull then
    Result:=False
  else
    Case FBufType of
      SQL_Smallint : Result:=PSQLSmallInt(Data)^=0;
      SQL_Integer  : Result:=PSQLINTEGER(Data)^=0;
      SQL_BIT      : Result:=PBYTE(Data)^=0;
      SQL_CHAR     : Result:=(StrToInt(GetAsString)=0);
      SQL_DOUBLE   : Result:=Round(GetAsDouble)=0;
      SQL_DATE,
      SQL_TIME,
      SQL_TIMESTAMP,
      SQL_TYPE_DATE,
      SQL_TYPE_TIMESTAMP,
      SQL_TYPE_TIME : Result:=Round(AsDateTime)=0;
    else
      ODBCError(SErrInvalidConversion)
    end;
end;

Function TODBCField.GetAsDouble : Double;

begin
  If IsNull then
    Result:=0
  else
    Case FBufType of
      SQL_Smallint : Result:=PSQLSmallInt(Data)^;
      SQL_Integer  : Result:=PSQLINTEGER(Data)^;
      SQL_BIT      : Result:=PBYTE(Data)^;
      SQL_CHAR     : Result:=StrToFloat(GetAsString);
      SQL_DOUBLE   : Result:=PSQLDOUBLE(GetData)^;
      SQL_DATE,
      SQL_TIME,
      SQL_TIMESTAMP,
      SQL_TYPE_DATE,
      SQL_TYPE_TIMESTAMP,
      SQL_TYPE_TIME : Result:=AsDateTime;
    else
      ODBCError(SErrInvalidConversion)
    end;
end;

{
function DateStructToDateTime( b:PSQL_DATE_STRUCT):TDateTime;
function DateTimeToDateStruct( b:TDateTime):SQL_DATE_STRUCT;
procedure DateTime2TimeStampStruct( var Value:SQL_TIMESTAMP_STRUCT; b:TDateTime);
}
Function TODBCField.GetAsDateTime : TDateTime;

begin
  If IsNull then
    Result:=0
  else
    Case FBufType of
      SQL_Smallint : Result:=PSQLSmallInt(Data)^;
      SQL_Integer  : Result:=PSQLINTEGER(Data)^;
      SQL_BIT      : Result:=PBYTE(Data)^;
      SQL_CHAR     : Result:=StrToInt(GetAsString);
      SQL_DOUBLE   : Result:=PSQLDOUBLE(GetData)^;
      SQL_DATE     : Result:=DateStructToDateTime(PSQL_DATE_STRUCT(Data));
      SQL_TIME     : Result:=TimeStructToDateTime(PSQL_TIME_STRUCT(Data));
      SQL_TIMESTAMP : Result:=TimeStampStructToDateTime(PSQL_TIMESTAMP_STRUCT(Data));
      SQL_TYPE_DATE : Result:=DateStructToDateTime(PSQL_DATE_STRUCT(Data));
      SQL_TYPE_TIMESTAMP :  Result:=TimeStampStructToDateTime(PSQL_TIMESTAMP_STRUCT(Data));
      SQL_TYPE_TIME : Result:=TimeStructToDateTime(PSQL_TIME_STRUCT(Data));
    else
      ODBCError(SErrInvalidConversion)
    end;
end;

Finalization
  If Assigned(DefEnv) then
    TODBCEnvironment(DefEnv).Free;
end.
