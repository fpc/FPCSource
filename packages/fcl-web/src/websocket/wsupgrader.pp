unit wsupgrader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpprotocol, httpdefs, fphttpserver, fpwebsocket, fpcustwsserver;

Type

  { TCustomWebsocketUpgrader }
  TAllowUpgradeEvent = Procedure(Sender : TObject; aRequest : TRequest; var aAllow : Boolean) of object;

  TCustomWebsocketUpgrader = Class(TCustomWSServer)
  private
    FActive: Boolean;
    FOnAllowUpgrade: TAllowUpgradeEvent;
    FStrictProtocolCheck: Boolean;
    FUpgradeName: String;
    FWebServer: TFPCustomHttpServer;
    FHost: String;
    function GetHandshakeRequest(aRequest: TFPHTTPConnectionRequest): TWSHandShakeRequest;
    function GetUpgradeName: String;
    procedure SetHost(const AValue: String);
    procedure SetUpgradeName(const AValue: String);
    procedure SetWebServer(AValue: TFPCustomHttpServer);
  Protected
    // Override from custom server
    procedure SetActive(const aValue: Boolean); override;
    function GetActive: Boolean; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Start upgrader: register, create connection handler
    procedure StartUpgrader;
    // End upgrader: unregister, free connection handler
    procedure StopUpgrader;
    // Check callback for upgrader mechanism
    procedure DoCheck(aRequest: TFPHTTPConnectionRequest; var aHandlesUpgrade: Boolean); virtual;
    // Upgrade callback for upgrader mechanism
    procedure DoUpgrade(aConnection: TFPHTTPConnection; aRequest: TFPHTTPConnectionRequest); virtual;
    // Webserver that we must register with
    Property WebServer : TFPCustomHttpServer Read FWebServer Write SetWebServer;
    // If set, only this resource will be acceped for upgrade.
    Property Host : String Read FHost Write SetHost;
    // Name to use when registering upgrade mechanism. Defaults to Name.
    Property UpgradeName : String Read GetUpgradeName Write SetUpgradeName;
    // Check also Host and Sec-Websocket-Version
    Property StrictProtocolCheck : Boolean Read FStrictProtocolCheck Write FStrictProtocolCheck;
    // Called when upgrade request is processed. allow is initialized with check for websocket upgrade.
    Property OnAllowUpgrade : TAllowUpgradeEvent Read FOnAllowUpgrade Write FOnAllowUpgrade;
  Public
    Destructor Destroy; override;
  end;

  TWebsocketUpgrader = class(TCustomWebsocketUpgrader)
  Published
    Property Active; // Registers, unregisters
    Property WebServer;
    Property Host;
    Property Resource;
    Property StrictProtocolCheck;
    Property ThreadMode;
    Property WebSocketVersion;
    Property MessageWaitTime;
    Property Options;
    Property OnAllow;
    property OnMessageReceived;
    property OnDisconnect;
    property OnControlReceived;
    Property OnError;
    Property OutgoingFrameMask;
    Property OnAllowUpgrade;
  end;


implementation

Resourcestring
  SErrWebserverNotAssigned = 'Webserver not assigned';
  SErrNoUpgradeName = 'Upgradename not set. Set UpgradeName or Name';

{ TCustomWebsocketUpgrader }

Function TCustomWebsocketUpgrader.GetActive : Boolean;

begin
  Result:=FActive;
end;

procedure TCustomWebsocketUpgrader.SetActive(const AValue: Boolean);
begin
  if FActive=AValue then Exit;
  If not Assigned(Webserver) then
    Raise EWebsocket.Create(SErrWebserverNotAssigned);
  If (UpgradeName='') then
    Raise EWebsocket.Create(SErrNoUpgradeName);
  if aValue then
    StartUpgrader
  else
    StopUpgrader;
  FActive:=AValue;
end;

Procedure TCustomWebsocketUpgrader.StartUpgrader;

begin
  StartConnectionHandler;
  Webserver.RegisterUpdateHandler(UpgradeName,@DoCheck,@DoUpgrade)
end;

Procedure TCustomWebsocketUpgrader.StopUpgrader;

begin
  Webserver.UnRegisterUpdateHandler(UpgradeName);
  ConnectionHandler.CloseConnections;
  WaitForConnections(10);
  FreeConnectionHandler;
end;

procedure TCustomWebsocketUpgrader.SetHost(const AValue: String);
begin
  if Host=AValue then Exit;
  CheckInactive;
  Host:=AValue;
end;


function TCustomWebsocketUpgrader.GetUpgradeName: String;
begin
  Result:=FUpgradeName;
  if Result='' then
    Result:=Name;
end;

procedure TCustomWebsocketUpgrader.DoCheck(aRequest: TFPHTTPConnectionRequest; var aHandlesUpgrade: Boolean);

Var
  aKey,aVersion : String;

begin
  aKey:=aRequest.GetFieldByName(SSecWebsocketKey);
  aVersion:=aRequest.GetFieldByName(SSecWebsocketVersion);
  // Connection: Upgrade is already checked before we get here
  aHandlesUpgrade:=SameText(aRequest.Method,'GET')
                   and SameText(aRequest.GetHeader(hhUpgrade),'WebSocket')
                   and (aKey<>'');
  if Host<>'' then
    aHandlesUpgrade:=aHandlesUpgrade and SameText(aRequest.GetHeader(hhHost),Host);
  if Resource<>'' then
    aHandlesUpgrade:=aHandlesUpgrade and aRequest.PathInfo.StartsWith(Resource,True);
  if StrictProtocolCheck and aHandlesUpgrade then
    aHandlesUpgrade:=((Host<>'') or (aRequest.GetHeader(hhHost)<>''))    // Check also Host present
                     and (aVersion<>'');  // and Sec-Websocket-Version
  if Assigned(OnAllowUpgrade) then
    OnAllowUpgrade(Self,aRequest,aHandlesUpgrade);
end;

Function TCustomWebsocketUpgrader.GetHandshakeRequest(aRequest: TFPHTTPConnectionRequest) : TWSHandShakeRequest;

Var
  aHeaders : TStrings;
  H : THeader;
  N,V : String;
  I : Integer;

begin
  Result:=Nil;
  aHeaders:=TStringList.Create;
  try
    aHeaders.NameValueSeparator:=':';
    for H:=Succ(Low(THeader)) to High(Theader) do
      begin
      V:=aRequest.GetHeader(H);
      if V<>'' then
        aHeaders.Add(HTTPHeaderNames[H]+': '+V);
      end;
    For I:=0 to aRequest.CustomHeaders.Count-1 do
      begin
      aRequest.CustomHeaders.GetNameValue(I,N,V);
      V:=Trim(V);
      if (N<>'') and (V<>'') then
        aHeaders.Add(N+': '+V);
      end;
    Result:=TWSHandShakeRequest.Create(aRequest.PathInfo,aHeaders);
  Finally
    aHeaders.Free;
  end;
end;

procedure TCustomWebsocketUpgrader.DoUpgrade(aConnection: TFPHTTPConnection; aRequest: TFPHTTPConnectionRequest);

Var
  aHandShake : TWSHandShakeRequest;
  aConn : TWSServerConnection;

begin
  aHandShake:=GetHandshakeRequest(aRequest);
  try
    aConn:=CreateWebsocketConnection(aConnection.Socket,Options);
    aConn.OnControl:=@DoControlReceived;
    aConn.OnMessageReceived:=@DoMessageReceived;
    aConn.OnDisconnect:=@DoDisconnect;
    aConn.OnHandshake:=OnConnectionHandshake;
    aConn.DoHandshake(aHandshake);
    Connections.Add(aConn);
    ConnectionHandler.HandleConnection(aConn,False);
  finally
    aHandshake.Free;
  end;
end;

destructor TCustomWebsocketUpgrader.Destroy;
begin
  FActive:=False;
  inherited Destroy;
end;


procedure TCustomWebsocketUpgrader.SetUpgradeName(const AValue: String);
begin
  if aValue=GetUpgradeName then
    exit;
  CheckInactive;
  FUpgradeName:=aValue;
end;

procedure TCustomWebsocketUpgrader.SetWebServer(AValue: TFPCustomHttpServer);
begin
  if FWebServer=AValue then Exit;
  CheckInactive;
  if Assigned(FWebServer) then
    FWebServer.RemoveFreeNotification(Self);
  FWebServer:=AValue;
  if Assigned(FWebServer) then
    FWebServer.FreeNotification(Self);
end;

procedure TCustomWebsocketUpgrader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FWebServer) then
    FWebServer:=Nil;
end;


end.

