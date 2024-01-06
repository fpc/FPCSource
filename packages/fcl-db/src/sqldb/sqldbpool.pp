unit sqldbpool;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, pqconnection, syncobjs, contnrs;

const
  DefaultDisconnectTimeOut = 10*60; // Number of seconds before connection is considered old and is discarded.

type
  TPoolLogEvent = procedure(Sender : TObject; Const Msg : string) of object;

  ESQLDBPool = Class(EDatabaseError);

  { TSQLConnectionDef }

  { TSQLDBConnectionDef }

  TSQLDBConnectionDef = Class(TCollectionItem)
  private
    FConnectionClass: TSQLConnectionClass;
    FConnectionType: String;
    FDatabaseName: UTF8String;
    FEnabled: Boolean;
    FHostName: UTF8String;
    FName: UTF8String;
    FParams: TStrings;
    FPassword: UTF8string;
    FRole: UTF8String;
    FUserName: UTF8String;
    FKey : UTF8String;
    FCharSet : UTF8String;
    procedure DoChange(Sender: TObject);
    function GetPort: Word;
    procedure SetCharSet(AValue: UTF8String);
    procedure SetConnectionType(AValue: String);
    procedure SetDatabaseName(AValue: UTF8String);
    procedure SetHostName(AValue: UTF8String);
    procedure SetParams(AValue: TStrings);
    procedure SetPassword(AValue: UTF8string);
    procedure SetPort(AValue: Word);
    procedure SetRole(AValue: UTF8String);
    procedure SetUserName(AValue: UTF8String);
  Protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ClearKey;
    function GetName : UTF8String; virtual;
    Function GetDisplayName: string; override;
    function CreateKey : String; virtual;
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    Property ConnectionClass : TSQLConnectionClass Read FConnectionClass Write FConnectionClass;
    function GetDescription(Full: Boolean=False): string;
    Function ToString: string; override;
  Published
    // TSQLConnector type
    Property ConnectionType : String read FConnectionType write SetConnectionType;
    // Name for this connection
    Property Name : UTF8String read GetName write FName;
    // Database database name
    Property DatabaseName : UTF8String read FDatabaseName write SetDatabaseName;
    // Database hostname
    Property HostName : UTF8String read FHostName write SetHostName;
    // Database username
    Property UserName : UTF8String read FUserName write SetUserName;
    // Database role
    Property Role :  UTF8String read FRole write SetRole;
    // Database user password
    Property Password : UTF8string read FPassword write SetPassword;
    // Other parameters
    Property Params : TStrings Read FParams Write SetParams;
    // Stored in Params.
    // Database character set
    Property CharSet : UTF8String Read FCharSet Write SetCharSet;
    // Port
    Property Port : Word Read GetPort Write SetPort;
    // Allow this connection to be used ?
    Property Enabled : Boolean Read FEnabled Write FEnabled default true;
  end;

  { TConnectionPoolData }

  TConnectionPoolData = Class(TObject)
  private
    FConnection: TSQLConnection;
    FLastUsed: TDateTime;
    FLocked: Boolean;
  Public
    Constructor Create(aConnection : TSQLConnection; aLocked : Boolean = true);
    Destructor Destroy; override;
    Procedure Lock;
    Procedure Unlock;
    Procedure FreeConnection;
    Property Connection : TSQLConnection Read FConnection;
    Property LastUsed : TDateTime Read FLastUsed Write FLastUsed;
    Property Locked : Boolean Read FLocked;
  end;

  { TSQLConnectionHelper }

  TSQLConnectionHelper = class helper for TSQLConnection
    Function GetDescription(Full : Boolean) : string;
  end;

  { TConnectionList }

  TConnectionList = Class (TFPObjectList)
  Private
    FonLog: TPoolLogEvent;
    FDisconnectTimeout: Integer;
    FLock : TCriticalSection;
  Protected
    Procedure Dolog(Const Msg : String);
    Procedure DoLog(Const Fmt : String; Args : Array of const);
    Function DoDisconnectOld(aTimeOut : Integer = -1) : Integer; virtual;
    function CreatePoolData(aConnection : TSQLConnection; aLocked : Boolean = True) : TConnectionPoolData;
  Public
    Constructor Create; reintroduce;
    Destructor Destroy; override;
    Procedure DisconnectAll;
    Function DisconnectOld(aTimeOut : Integer = -1) : Integer;
    function AddConnection (aConnection : TSQLConnection; aLocked : Boolean = True) : TConnectionPoolData;
    Function PopConnection : TSQLConnection;
    Function UnlockConnection(aConnection : TSQLConnection) : boolean;
    Property DisconnectTimeout : Integer Read FDisconnectTimeout Write FDisconnectTimeout;
    Property OnLog : TPoolLogEvent Read FonLog Write FOnLog;
  end;

  { TSQLDBConnectionPool }

  TSQLDBConnectionPool = class(TComponent)
  private
    FonLog: TPoolLogEvent;
    FPool : TFPObjectHashTable;
    FLock : TCriticalSection;
    procedure DisconnectAll;
    function GetCount: longword;
  protected
    Function CreateList : TConnectionList; virtual;
    Procedure Dolog(Const Msg : String);
    Procedure DoLog(Const Fmt : String; Args : Array of const);
    procedure Lock;
    procedure Unlock;
    function CreateKey(aDef : TSQLDBConnectionDef) : String; virtual;
    function CreateDef: TSQLDBConnectionDef;
    function DoFindConnection(const aConnectionDef: TSQLDBConnectionDef): TSQLConnection; virtual;
    procedure DoDisconnect(Item: TObject; const Key: string; var Continue: Boolean);
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    function CountConnections(aClass : TSQLConnectionClass; const aDatabaseName,aHostName,aUserName,aPassword: string; aParams:TStrings = nil):Integer;
    function CountConnections(aInstance : TSQLConnection):Integer;
    function CountConnections(aDef : TSQLDBConnectionDef):Integer;
    Function CountAllConnections : Integer;
    function FindConnection(aClass : TSQLConnectionClass; const aDatabaseName,aHostName,aUserName,aPassword: string; aParams:TStrings = nil):TSQLConnection;
    function FindConnection(const aConnectionDef : TSQLDBConnectionDef):TSQLConnection;
    procedure AddConnection(aConnection: TSQLConnection; aLocked: Boolean=True);
    function ReleaseConnection(aConnection: TSQLConnection) : Boolean;
    Property OnLog : TPoolLogEvent Read FonLog Write FOnLog;
  end;


  { TTypedConnectionPool }

  Generic TTypedConnectionPool<T: TSQLConnection> = class(TSQLDBConnectionPool)
  public
    function FindConnection(const aDatabaseName:string; const aHostName:string; const aUserName:string; const aPassword:string; aParams:TStrings=nil):T; overload;
  end;


  { TSQLDBConnectionDefList }

  TSQLDBConnectionDefList = Class(TOwnedCollection)
  private
    function GetD(aIndex : Integer): TSQLDBConnectionDef;
    procedure SetD(aIndex : Integer; AValue: TSQLDBConnectionDef);
  Public
    Function IndexOf(const aName : UTF8String) : Integer;
    Function Find(const aName : UTF8String) : TSQLDBConnectionDef;
    Function Get(const aName : UTF8String) : TSQLDBConnectionDef;
    Property Definitions[aIndex : Integer] : TSQLDBConnectionDef Read GetD Write SetD; default;
  end;

  { TSQLDBConnectionmanager }

  TSQLDBConnectionmanager = Class(TComponent)
  private
    FConnectionOwner: TComponent;
    FDefinitions: TSQLDBConnectionDefList;
    FMaxDBConnections: Word;
    FMaxTotalConnections: Cardinal;
    FOnLog: TPoolLogEvent;
    FPool : TSQLDBConnectionPool;
    FMyPool : TSQLDBConnectionPool;
    FLogEvents : TDBEventTypes;
    procedure SetConnectionOwner(AValue: TComponent);
    procedure SetDefinitions(AValue: TSQLDBConnectionDefList);
    procedure SetOnLog(AValue: TPoolLogEvent);
    procedure SetPool(AValue: TSQLDBConnectionPool);
  Protected
    Procedure DoLog(const Msg : String);
    Procedure DoLog(const Fmt : String; const aArgs : Array of const);

    function NewConnectionAllowed(aDef: TSQLDBConnectionDef; out aReason: string): Boolean; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function CreatePool : TSQLDBConnectionPool; virtual;
    Function CreateDefinitionList : TSQLDBConnectionDefList; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Function CreateConnection(const aDef : TSQLDBConnectionDef; addToPool : Boolean) : TSQLConnection;
    Function CreateConnection(const aName : string; addToPool : Boolean) : TSQLConnection;
    Function GetConnection(const aDef : TSQLDBConnectionDef) : TSQLConnection;
    Function GetConnection(const aName : string) : TSQLConnection;
    Function ReleaseConnection(aConnection : TSQLConnection) : Boolean;
  Published
    Property Pool : TSQLDBConnectionPool Read FPool Write SetPool;
    Property Definitions : TSQLDBConnectionDefList Read FDefinitions Write SetDefinitions;
    Property MaxDBConnections : Word Read FMaxDBConnections Write FMaxDBConnections;
    Property MaxTotalConnections : Cardinal Read FMaxTotalConnections Write FMaxTotalConnections;
    Property ConnectionOwner : TComponent Read FConnectionOwner Write SetConnectionOwner;
    Property OnLog : TPoolLogEvent Read FOnLog Write SetOnLog;
    Property LogEvents : TDBEventTypes Read FLogEvents Write FLogEvents;
  end;


implementation

uses typinfo, dateutils;

Resourcestring
  SFindingConnection = 'Finding Connection (%s)';
  SFoundConnection = 'Found Connection (%s) : %x';
  SNoSuchConnection = 'No such Connection (%s)';
  SErrorDisconnecting = 'Error %s disconnecting connections : %s';
  SCreatingNewConnection = 'Creating new connection for connection definition (%s)';
  STimeoutReached = 'Timeout (%d>%d) reached, freeing connection (%s)';
  SReleasingConnections = 'Releasing connections (%s) (Current count: %d)';
  SErrCannotCreateNewConnection = 'Cannot create new connection for (%s): %s';
  SErrMaxNumberOfDefConnections = 'Max number of connections (%d) for this connection (%s) is reached';
  SErrMaxTotalConnectionReached = 'Max total number of connections (%d) is reached';
  SErrFreeingConnection = 'Error %s freeing connection %d : %s';

{ TSQLConnectionHelper }

function TSQLConnectionHelper.GetDescription(Full: Boolean): string;

  Procedure AddTo(const aName,aValue : String);
  begin
    if aValue='' then
      exit;
    if Result<>'' then
      Result:=Result+', ';
    Result:=Result+aName+': '+aValue;
  end;

var
  aPort : integer;

begin
  Result:='';
  AddTo('Name',Name);
  AddTo('Host',HostName);
  AddTo('Database',DatabaseName);
  AddTo('User',Username);
  AddTo('Charset',CharSet);
  if IsPublishedProp(Self,'Port') then
    if PropIsType(Self,'Port',tkInteger) then
      begin
      aPort:=GetOrdProp(Self,'Port');
      if aPort>0 then
        AddTo('Port',IntToStr(aPort));
      end;
  if Full then
    begin
    AddTo('Password',Password);
    if Params.Count>0 then
      AddTo('Params',Params.CommaText);
    end;
end;

{ TSQLDBConnectionDefList }

function TSQLDBConnectionDefList.GetD(aIndex : Integer): TSQLDBConnectionDef;
begin
  Result:=TSQLDBConnectionDef(Items[aIndex])
end;

procedure TSQLDBConnectionDefList.SetD(aIndex : Integer; AValue: TSQLDBConnectionDef
  );
begin
  Items[aIndex]:=aValue;
end;

function TSQLDBConnectionDefList.IndexOf(const aName: UTF8String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aName,GetD(Result).Name) do
    Dec(Result);
end;

function TSQLDBConnectionDefList.Find(const aName: UTF8String): TSQLDBConnectionDef;

Var
  Idx : Integer;

begin
  Result:=Nil;
  Idx:=IndexOf(aName);
  if Idx<>-1 then
    Result:=GetD(Idx);
end;

function TSQLDBConnectionDefList.Get(const aName: UTF8String): TSQLDBConnectionDef;
begin
  Result:=Find(aName);
  if Result=Nil then
end;

{ TSQLDBConnectionDef }


procedure TSQLDBConnectionDef.DoChange(Sender: TObject);
begin
  ClearKey;
end;

procedure  TSQLDBConnectionDef.ClearKey;

begin
  FKey:='';
end;

function TSQLDBConnectionDef.GetName: UTF8String;
begin
  Result:=FName;
end;

function TSQLDBConnectionDef.GetPort: Word;
begin
  Result:=StrToIntDef(FParams.Values['port'],0);
end;

procedure TSQLDBConnectionDef.SetCharSet(AValue: UTF8String);
begin
  FCharSet :=aValue;
  ClearKey;
end;

procedure TSQLDBConnectionDef.SetConnectionType(AValue: String);

Var
  Def : TConnectionDef;

begin
  if FConnectionType=AValue then Exit;
  FConnectionType:=AValue;
  if FConnectionType<>'' then
    begin
    Def:=GetConnectionDef(aValue);
    if Def<>Nil then
      ConnectionClass:=Def.ConnectionClass
    else
      ConnectionClass:=TSQLConnector;
    end
  else
    ConnectionClass:=Nil;
end;


procedure TSQLDBConnectionDef.SetDatabaseName(AValue: UTF8String);
begin
  if FDatabaseName=AValue then Exit;
  FDatabaseName:=AValue;
  ClearKey;
end;

procedure TSQLDBConnectionDef.SetHostName(AValue: UTF8String);
begin
  if FHostName=AValue then Exit;
  FHostName:=AValue;
  ClearKey;
end;


procedure TSQLDBConnectionDef.SetParams(AValue: TStrings);
begin
  FParams.Assign(aValue);
  ClearKey;
end;

procedure TSQLDBConnectionDef.SetPassword(AValue: UTF8string);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
  ClearKey;
end;


procedure TSQLDBConnectionDef.SetPort(AValue: Word);
begin
  if aValue=0 then
    FParams.Values['port']:=''
  else
    FParams.Values['port']:=IntToStr(aValue)
end;

procedure TSQLDBConnectionDef.SetRole(AValue: UTF8String);
begin
  if FRole=AValue then Exit;
  FRole:=AValue;
  ClearKey;
end;

procedure TSQLDBConnectionDef.SetUserName(AValue: UTF8String);
begin
  if FUserName=AValue then Exit;
  FUserName:=AValue;
  ClearKey;
end;

procedure TSQLDBConnectionDef.AssignTo(Dest: TPersistent);

var
  Conn : TSQLConnection absolute Dest;

begin
  if Dest is TSQLDBConnectionDef then
    Dest.Assign(Self)
  else if Dest is TSQLConnection then
    begin
    Conn.DatabaseName := FDatabaseName;
    Conn.HostName := FHostName;
    Conn.Password := FPassword;
    Conn.UserName := FUserName;
    Conn.Role:=FRole;
    Conn.CharSet:=FCharSet;
    Conn.Params.Assign(Self.Params);
    if Conn is TSQLConnector then
      TSQLConnector(Conn).ConnectorType:=Self.ConnectionType;
    end
  else
    inherited AssignTo(Dest);
end;

function TSQLDBConnectionDef.GetDisplayName: string;
begin
  Result:=Name;
end;

function TSQLDBConnectionDef.CreateKey: String;

Var
  S : TStringList;
  N : String;
begin
  if FKey<>'' then
    Exit(FKey);
  if Assigned(ConnectionClass) then
    N:=ConnectionClass.ClassName
  else
    N:=TSQLConnector.ClassName+'.'+ConnectionType;
  Result:=N
         +'#@'+HostName
         +'#@'+DatabaseName
         +'#@'+UserName
         +'#@'+Password
         +'#@'+Role
         +'#@'+CharSet;
  If Assigned(Params) then
    begin
    // Canonicalize
    S:=TStringList.Create;
    try
      S.Sorted:=true;
      S.AddStrings(Params);
      Result:=Result+'#@'+S.Text;
    finally
      S.Free;
    end;
    end;
  FKey:=Result;
end;

constructor TSQLDBConnectionDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams:=TStringList.Create;
  TStringList(FParams).OnChange:=@DoChange;
  FEnabled:=True;
end;

destructor TSQLDBConnectionDef.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TSQLDBConnectionDef.Assign(Source: TPersistent);

Var
  Def : TSQLDBConnectionDef absolute source;
  Conn : TSQLConnection absolute source;

begin
  if Source is TSQLDBConnectionDef then
    begin
    FConnectionType:=Def.ConnectionType;
    FDatabaseName:=Def.DatabaseName;
    FHostName:=Def.HostName;
    FPassword:=Def.Password;
    FUserName:=Def.UserName;
    FName:=Def.Name;
    FCharSet:=Def.CharSet;
    FParams.Assign(Def.Params);
    FEnabled:=Def.Enabled;
    ClearKey;
    end
  else if Source is TSQLConnection then
    begin
    if Conn is TSQLConnector then
      FConnectionType:=TSQLConnector(Conn).ConnectorType
    else
      FConnectionClass:=TSQLConnectionClass(Conn.ClassType);
    FDatabaseName:=Conn.DatabaseName;
    FHostName:=Conn.HostName;
    FPassword:=Conn.Password;
    FUserName:=Conn.UserName;
    FName:='';
    FCharSet:=Conn.CharSet;
    FParams.Assign(Conn.Params);
    FEnabled:=Def.Enabled;
    ClearKey;
    end
  else
    inherited Assign(Source);
end;

function TSQLDBConnectionDef.GetDescription(Full : Boolean = False) : string;

  Procedure AddTo(const aName,aValue : String);
  begin
    if aValue='' then
      exit;
    if Result<>'' then
      Result:=Result+', ';
    Result:=Result+aName+': '+aValue;
end;

begin
  Result:='';
  AddTo('Name',Name);
  AddTo('Host',HostName);
  AddTo('Database',DatabaseName);
  AddTo('User',Username);
  AddTo('Charset',CharSet);
  if Port>0 then
    AddTo('Port',IntToStr(Port));
  if Full then
    begin
    AddTo('Password',Password);
    if Params.Count>0 then
      AddTo('Params',Params.CommaText);
    end;
end;

function TSQLDBConnectionDef.ToString: string;

begin
  Result:=GetDescription;
end;

{ TSQLDBConnectionmanager }

procedure TSQLDBConnectionmanager.SetConnectionOwner(AValue: TComponent);
begin
  if FConnectionOwner=AValue then Exit;
  if Assigned(FConnectionOwner) then
    FConnectionOwner.RemoveFreeNotification(Self);
  FConnectionOwner:=AValue;
  if Assigned(FConnectionOwner) then
    FConnectionOwner.FreeNotification(Self);
end;

procedure TSQLDBConnectionmanager.SetDefinitions(AValue: TSQLDBConnectionDefList);
begin
  if FDefinitions=AValue then
    Exit;
  FDefinitions.Assign(AValue);
end;

procedure TSQLDBConnectionmanager.SetOnLog(AValue: TPoolLogEvent);
begin
  if FOnLog=AValue then Exit;
  FOnLog:=AValue;
  if Assigned(FMyPool) then
    FMyPool.OnLog:=aValue;
end;

procedure TSQLDBConnectionmanager.SetPool(AValue: TSQLDBConnectionPool);
begin
  if FPool=AValue then Exit;
  FPool:=AValue;
  if (FPool=Nil) then
    FPool:=FMyPool;
end;

procedure TSQLDBConnectionmanager.DoLog(const Msg: String);
begin
  if Assigned(OnLog) then
    OnLog(Self,Msg);
end;

procedure TSQLDBConnectionmanager.DoLog(const Fmt: String;
  const aArgs: array of const);
begin
  DoLog(Format(Fmt,aArgs));
end;

procedure TSQLDBConnectionmanager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) then
    if (aComponent=FConnectionOwner) then
      FConnectionOwner:=Nil
    else
      begin
      if (aComponent=FMyPool) then
        FMyPool:=Nil;
      if (aComponent=FPool) then
        FPool:=Nil;
      end;
  inherited Notification(AComponent, Operation);
end;

constructor TSQLDBConnectionmanager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMyPool:=CreatePool;
  FMyPool.SetSubComponent(True);
  FDefinitions:=CreateDefinitionList;
  FPool:=FMyPool;
  FLogEvents:=LogAllEvents;
end;

destructor TSQLDBConnectionmanager.Destroy;
begin
  FreeAndNil(FPool);
  FreeAndNil(FDefinitions);
  inherited Destroy;
end;

function TSQLDBConnectionmanager.CreateConnection(const aDef: TSQLDBConnectionDef; addToPool: Boolean): TSQLConnection;

var
  C : TSQLConnectionClass;

begin
  C:=aDef.ConnectionClass;
  if (C=Nil) and (aDef.ConnectionType<>'') then
    C:=TSQLConnector;
  With aDef do
    DoLog(SCreatingNewConnection, [GetDescription]);
  Result:=C.Create(Self.ConnectionOwner);
  try
    aDef.AssignTo(Result);
    Result.LogEvents:=Self.LogEvents;
    Result.Transaction:=TSQLTransaction.Create(Result);
  except
    Result.Free;
    Raise;
  end;
  if AddToPool then
    Pool.AddConnection(Result);
end;

function TSQLDBConnectionmanager.CreatePool : TSQLDBConnectionPool;

begin
  Result:=TSQLDBConnectionPool.Create(Self);
end;

function TSQLDBConnectionmanager.CreateDefinitionList: TSQLDBConnectionDefList;
begin
  Result:=TSQLDBConnectionDefList.Create(Self,TSQLDBConnectionDef);
end;


function TSQLDBConnectionmanager.CreateConnection(const aName: string;
  addToPool: Boolean): TSQLConnection;
begin
  Result:=CreateConnection(Definitions.Get(aName),addToPool);
end;

function TSQLDBConnectionmanager.NewConnectionAllowed(aDef: TSQLDBConnectionDef; out aReason: string): Boolean;

Var
  N: Integer;

begin
  Result:=True;
  if (MaxDBConnections>0) then
    begin
    N:=FPool.CountConnections(aDef);
    if (N>MaxDBConnections) then
      AReason:=Format(SErrMaxNumberOfDefConnections, [MaxDBConnections, aDef.GetDescription(False)]);
    end;
  if (MaxTotalConnections>0) then
    begin
    N:=FPool.CountAllConnections;
    if (N>MaxDBConnections) then
      aReason:=Format(SErrMaxTotalConnectionReached, [MaxDBConnections]);
    end;
  Result:=aReason='';
end;

function TSQLDBConnectionmanager.GetConnection(const aDef: TSQLDBConnectionDef ): TSQLConnection;

Var
  aReason,aErr : String;

begin
  Result:=FPool.FindConnection(aDef);
  if Result=Nil then
    begin
    if Not NewConnectionAllowed(aDef,aReason) then
      begin
      aErr:=Format(SErrCannotCreateNewConnection, [aDef.GetDescription, aReason]);
      DoLog(aErr);
      Raise ESQLDBPool.Create(aErr);
      end;
    Result:=CreateConnection(aDef,True);
    end;
end;

function TSQLDBConnectionmanager.GetConnection(const aName: string
  ): TSQLConnection;
begin
  Result:=GetConnection(Definitions.Get(aName));
end;

function TSQLDBConnectionmanager.ReleaseConnection(aConnection: TSQLConnection
  ): Boolean;

begin
  Result:=FPool.ReleaseConnection(aConnection);
end;

{ TConnectionPoolData }

constructor TConnectionPoolData.Create(aConnection: TSQLConnection; aLocked : Boolean = true);
begin
  FConnection:=aConnection;
  LastUsed:=Now;
  Flocked:=aLocked;
end;

destructor TConnectionPoolData.Destroy;
begin
  inherited Destroy;
end;

procedure TConnectionPoolData.Lock;
begin
  FLocked:=True;
  FLastUsed:=Now;
end;

procedure TConnectionPoolData.Unlock;
begin
  FLocked:=False;
  FLastUsed:=Now;
end;

procedure TConnectionPoolData.FreeConnection;

Var
  TR : TSQLTransaction;

begin
  try
    TR:=Connection.Transaction;
    Connection.Transaction:=Nil;
    TR.Free;
    Connection.Connected:=False;
  finally
    FreeAndNil(FConnection);
  end;
end;

{ TTypedConnectionPool }

function TTypedConnectionPool.FindConnection(const aDatabaseName: string;
  const aHostName: string; const aUserName: string; const aPassword: string;
  aParams: TStrings): T;
begin
  Result:=T(Inherited FindConnection(T,aDatabaseName,aHostName,aUserName,aPassword,aParams));
end;

{ TPQConnPool }

(*
generic function TTypedConnectionPool<T : TSQLConnection>.FindConnection(const aDatabaseName: string; const aHostName: string;
  const aUserName: string; const aPassword: string; aParams: TStrings = Nil): T;
begin
  result:=T(FindConnection(T,aDatabaseName,aHostName,aPassword,aUserName,aParams));
end;
*)

{ TConnectionList }

constructor TConnectionList.Create;
begin
  Inherited Create;
  FLock:=TCriticalSection.Create;
  FDisconnectTimeout:=DefaultDisconnectTimeout;
end;

destructor TConnectionList.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TConnectionList.DisconnectAll;

Var
  I : integer;
  CD : TConnectionPoolData;

begin
  FLock.Enter;
  try
    For I:=Count-1 downto 0 do
    begin
    CD:=TConnectionPoolData(Items[i]);
    if (not CD.Locked) then
      begin
      CD.FreeConnection;
      Delete(I);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TConnectionList.Dolog(const Msg: String);
begin
  If Assigned(OnLog) then
    OnLog(Self,Msg);
end;

procedure TConnectionList.DoLog(const Fmt: String; Args: array of const);
begin
  DoLog(Format(Fmt,args));
end;

function TConnectionList.DoDisconnectOld(aTimeOut: Integer = -1): Integer;

Var
  secs,I : integer;

  CD : TConnectionPoolData;
  N : TDateTime;

begin
  Result:=0;
  N:=Now;
  if aTimeout<0 then
    aTimeout:=FDisconnectTimeout;
  for I:=Count-1 downto 0 do
    begin
    CD:=TConnectionPoolData(Items[i]);
    Secs:=SecondsBetween(N,CD.LastUsed);
    if (not CD.Locked) and (Secs>aTimeout) then
      begin
      With CD.Connection do
        DoLog(STimeoutReached, [Secs, aTimeout, GetDescription(False)]);
      try
        CD.FreeConnection;
      except
        on E : Exception do
          DoLog(SErrFreeingConnection, [E.ClassName, I, E.Message]);
      end;
      Delete(I);
      Inc(Result);
      end;
    end;
end;

function TConnectionList.CreatePoolData(aConnection: TSQLConnection;
  aLocked: Boolean): TConnectionPoolData;
begin
  Result:=TConnectionPoolData.Create(aConnection,aLocked);
end;

function TConnectionList.DisconnectOld(aTimeOut: Integer): Integer;

begin
  FLock.Enter;
  try
    Result:=DoDisconnectOld(aTimeout);
  finally
    FLock.Leave;
  end;
end;

function TConnectionList.AddConnection(aConnection: TSQLConnection; aLocked: Boolean
  ): TConnectionPoolData;

begin
  FLock.Enter;
  try
    Result:=CreatePoolData(aConnection,aLocked);
    Add(Result);
  finally
    FLock.Leave;
  end;
end;

function TConnectionList.PopConnection: TSQLConnection;

Var
  i : integer;
  CD : TConnectionPoolData;

begin
  Result:=nil;
  FLock.Enter;
  try
    DoDisconnectOld;
    I:=0;
    While (Result=Nil) and (I<Count) do
      begin
      CD:=TConnectionPoolData(Items[i]);
      if not CD.Locked then
        begin
        CD.Lock;
        Result:=CD.Connection;
        end;
      Inc(I);
      end;
  finally
    Flock.Leave;
  end;
end;

function TConnectionList.UnlockConnection(aConnection: TSQLConnection): boolean;

Var
  I : Integer;
  Data : TConnectionPoolData;
begin
  Result:=False;
  FLock.Enter;
  try
    I:=Count-1;
    Data:=Nil;
    While (Data=Nil) and (I>=0) do
      begin
      Data:=TConnectionPoolData(Items[i]);
      if Data.Connection<>aConnection then
        Data:=Nil;
      Dec(i);
      end;
    if Assigned(Data) then
      begin
      Data.Unlock;
      Result:=True;
      end;
  finally
    FLock.Leave;
  end;
end;


{ TSQLDBConnectionPool }

function TSQLDBConnectionPool.GetCount: longword;
begin
  Result:=FPool.Count;
end;

function TSQLDBConnectionPool.CreateList: TConnectionList;
begin
  Result:=TConnectionList.Create;
end;

procedure TSQLDBConnectionPool.Dolog(const Msg: String);
begin
  If Assigned(OnLog) then
    OnLog(Self,Msg);
end;

procedure TSQLDBConnectionPool.DoLog(const Fmt: String; Args: array of const);
begin
  DoLog(Format(Fmt,args));
end;

procedure TSQLDBConnectionPool.Lock;
begin
  Flock.Enter;
end;

procedure TSQLDBConnectionPool.Unlock;
begin
  Flock.Leave;
end;

function TSQLDBConnectionPool.CreateKey(aDef: TSQLDBConnectionDef): String;

begin
  Result:=aDef.CreateKey;
end;


function TSQLDBConnectionPool.CreateDef : TSQLDBConnectionDef;

begin
  Result:=TSQLDBConnectionDef.Create(Nil);
end;

function TSQLDBConnectionPool.FindConnection(aClass : TSQLConnectionClass; const aDatabaseName, aHostName,
  aUserName, aPassword: string; aParams: TStrings): TSQLConnection;

Var
  Def : TSQLDBConnectionDef;

begin
  Result:=nil;
  Def:=CreateDef;
  try
    Def.ConnectionClass:=aClass;
    Def.DatabaseName:=aDatabaseName;
    Def.HostName:=aHostName;
    Def.UserName:=aUserName;
    Def.Password:=aPassword;
    if Assigned(aParams) then
      Def.Params:=aParams;
    Result:=FindConnection(Def);
  finally
    Def.Free;
  end;
end;

function TSQLDBConnectionPool.FindConnection(const aConnectionDef: TSQLDBConnectionDef): TSQLConnection;
Var
  N : String;

begin
  Result:=nil;
  with aConnectionDef do
    begin
    N:=ConnectionType;
    if (N='') and Assigned(ConnectionClass) then
      N:=ConnectionClass.ClassName;
    DoLog(SFindingConnection,[GetDescription]);
    Result:=DoFindConnection(aConnectionDef);
    If (Result=Nil) then
      DoLog(SNoSuchConnection,[GetDescription])
    else
      DoLog(SFoundConnection,[GetDescription, PtrInt(Result)])
    end;
end;

function TSQLDBConnectionPool.DoFindConnection(const aConnectionDef: TSQLDBConnectionDef): TSQLConnection;

Var
  Key : String;
  L : TConnectionList;

begin
  Result:=Nil;
  Key:=CreateKey(aConnectionDef);
  Lock;
  try
    L:=TConnectionList(FPool.Items[Key]);
    if L=Nil then
      Exit;
    Result:=L.PopConnection;
  finally
    Unlock;
  end;
end;


(*
result:=TSQLConnection(FPool[key]);
  if result=nil then
    begin
    result:=CreateConn(AOwner);
    result.HostName:=GetFirstNonNull(sHostName,FHostName);
    // Force local connection
    if result.HostName=MyServerName then
      Result.HostName:='';
    result.DatabaseName:=GetFirstNonNull(sDatabaseName,FDatabaseName);
    result.UserName:=GetFirstNonNull(sUserName,FUserName);
    result.Password:=GetFirstNonNull(sPassword,FPassword);
    result.Params:=GetFirstNonNull(ssParams,FParams);
    result.CharSet:='UTF8';
    if not CreateDisconnected then
      Result.Open;
    FPool.Add(key,result);
    end;
end;
*)

procedure TSQLDBConnectionPool.DoDisconnect(Item: TObject; const Key: string;
  var Continue: Boolean);

Var
  L : TConnectionList absolute item;

begin
  Continue:=True;
  try
    L.DisconnectOld();
  except
    on E : Exception do
      DoLog(SErrorDisconnecting,[E.ClassName,E.Message]);
  end;
end;

procedure TSQLDBConnectionPool.DisconnectAll;
begin
  Lock;
  try
    FPool.Iterate(@DoDisconnect);
  finally
    UnLock;
  end;
end;

destructor TSQLDBConnectionPool.Destroy;
begin
  FLock.Free;
  FPool.Destroy;
  inherited Destroy;
end;

function TSQLDBConnectionPool.CountConnections(aClass: TSQLConnectionClass;
  const aDatabaseName, aHostName, aUserName, aPassword: string;
  aParams: TStrings): Integer;

Var
  Def : TSQLDBConnectionDef;

begin
  Result:=0;
  Def:=CreateDef;
  try
    Def.ConnectionClass:=aClass;
    Def.DatabaseName:=aDatabaseName;
    Def.HostName:=aHostName;
    Def.UserName:=aUserName;
    Def.Password:=aPassword;
    if Assigned(aParams) then
      Def.Params:=aParams;
    Result:=CountConnections(Def);
  finally
    Def.Free;
  end;
end;

function TSQLDBConnectionPool.CountConnections(aInstance: TSQLConnection): Integer;
begin
  With aInstance do
    Result:=CountConnections(TSQLConnectionClass(ClassType),DatabaseName,HostName,UserName,Password,Params);
end;

function TSQLDBConnectionPool.CountConnections(aDef: TSQLDBConnectionDef): Integer;
Var
  Key : String;
  L : TConnectionList;
begin
  Result:=0;
  Key:=CreateKey(aDef);
  Lock;
  try
    L:=TConnectionList(FPool.Items[Key]);
    if L<>Nil then
      Result:=L.Count;
  finally
    UnLock;
  end;
end;
Type

  { TConnectionCounter }

  TConnectionCounter = Class(TObject)
  private
    FCount : Integer;
  Public
    Procedure DoCount(Item: TObject; const Key: string; var Continue: Boolean);
    Property Count : Integer Read FCount;
  end;

{ TConnectionCounter }

procedure TConnectionCounter.DoCount(Item: TObject; const Key: string; var Continue: Boolean);
begin
  FCount:=FCount+(Item as TConnectionList).Count;
  Continue:=True;
end;

function TSQLDBConnectionPool.CountAllConnections: Integer;

var
  Counter : TConnectionCounter;

begin
  Counter:=Nil;
  Lock;
  try
    Counter:=TConnectionCounter.Create;
    FPool.Iterate(@Counter.DoCount);
    Result:=Counter.Count;
  finally
    Unlock;
    Counter.Free;
  end;
end;

procedure TSQLDBConnectionPool.AddConnection(aConnection: TSQLConnection; aLocked : Boolean = True);

Var
  Key : String;
  L : TConnectionList;
  aDef: TSQLDBConnectionDef;
begin
  aDef:=Nil;
  Lock;
  try
    aDef:=CreateDef;
    aDef.Assign(aConnection);
    Key:=CreateKey(aDef);
    L:=TConnectionList(FPool.Items[Key]);
    if L=Nil then
      begin
      L:=CreateList;
      L.FonLog:=Self.OnLog;
      FPool.Add(Key,L);
      end;
    L.AddConnection(aConnection,aLocked);
  finally
    Unlock;
    aDef.Free;
  end;
end;

function TSQLDBConnectionPool.ReleaseConnection(aConnection: TSQLConnection): Boolean;

Var
  Key : String;
  L : TConnectionList;
  aDef: TSQLDBConnectionDef;
begin
  Result:=False;
  aDef:=Nil;
  Lock;
  try
    aDef:=CreateDef;
    aDef.Assign(aConnection);
    Key:=CreateKey(aDef);
    L:=TConnectionList(FPool.Items[Key]);
    if Assigned(L)  then
      begin
      With aConnection do
        DoLog(SReleasingConnections, [GetDescription(False), L.Count]);
      Result:=L.UnlockConnection(aConnection);
      end;
  finally
    Unlock;
    aDef.Free;
  end;
end;

constructor TSQLDBConnectionPool.Create(aOwner: TComponent);
begin
  FPool:=TFPObjectHashTable.Create(True);
  FLock:=TCriticalSection.Create;
end;

end.

