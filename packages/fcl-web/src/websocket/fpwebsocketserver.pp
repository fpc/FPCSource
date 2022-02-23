{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 - by the Free Pascal development team

    Standalone websocket server implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit fpwebsocketserver;

interface

uses
  Classes, SysUtils, ssockets, sslbase, fpwebsocket, fpcustwsserver, fpThreadPool;

type
  TWebSocketServer = Class;

  { TWebSocketServer }
  TAcceptIdleEvent = Procedure (sender : TObject; var aCheckMessages : Boolean) of object;

  TWebSocketServer = Class(TCustomWSServer)
  private
    FAcceptIdleTimeout: Cardinal;
    FActive: Boolean;
    FAfterSocketHandlerCreated: TWSSocketHandlerCreatedEvent;
    FCertificateData: TCertificateData;
    FHost: String;
    FMessageWaitTime: Cardinal;
    FOnAcceptIdle: TAcceptIdleEvent;
    FOnGetSocketHandler: TWSGetSocketHandlerEvent;
    FPort: Word;
    FQueueSize: Word;
    FServer: TInetServer;
    FThreadedAccept: Boolean;
    FUseSSL: Boolean;
    procedure SetAcceptIdleTimeout(AValue: Cardinal);
    procedure SetCertificateData(AValue: TCertificateData);
    procedure SetHost(const AValue: String);
    procedure SetPort(AValue: Word);
    procedure SetQueueSize(AValue: Word);
    procedure SetUseSSL(AValue: Boolean);
    function GetSocketHandler(const UseSSL: Boolean): TSocketHandler;
  Protected
    procedure SetThreadMode(AValue: TWSThreadMode); override;
    Function GetActive : Boolean; override;
    Procedure SetActive(const aValue : Boolean); override;
    // Socket server startup, socket handler
    Procedure FreeServerSocket; virtual;
    Procedure StartAccepting; virtual;
    procedure SetupSocket; virtual;
    procedure StartServerSocket; virtual;
    procedure CreateServerSocket; virtual;
    function CreateSSLSocketHandler: TSocketHandler; virtual;
    // Socket server callbacks
    procedure DoConnect(Sender: TObject; Data: TSocketStream); virtual;
    procedure DoCreateClientHandler(Sender: TObject; out AHandler: TSocketHandler); virtual;
    procedure DoAllowConnect(Sender: TObject; ASocket: Longint; var Allow: Boolean); virtual;
    procedure DoAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction); virtual;
    procedure DoAcceptIdle(Sender: TObject); virtual;
    Property Server : TInetServer Read FServer;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    // if ThreadedAccept = False, this call will not return till server is stopped.
    Procedure StartServer;
    // Stop server
    Procedure StopServer;
    // Check for incoming messages
    Procedure CheckIncomingMessages;
  Published
    Property Active : Boolean read FActive write SetActive;
    Property MessageWaitTime;
    Property Options;
    Property Port: Word Read FPort Write SetPort default 8080;
    Property Host: String Read FHost Write SetHost; // default '0.0.0.0'
    Property Resource;
    Property WebSocketVersion;
    property OnConnect;
    Property OnAllow;
    Property OutgoingFrameMask;
    property OnMessageReceived;
    property OnDisconnect;
    property OnControlReceived;
    Property ThreadMode;
    // use SSL ?
    Property UseSSL : Boolean Read FUseSSL Write SetUseSSL;
    // Properties to use when doing SSL handshake
    Property CertificateData  : TCertificateData Read FCertificateData Write SetCertificateData;
    // Called to create socket handler. If not set, or Nil is returned, a standard socket handler is created.
    Property OnGetSocketHandler : TWSGetSocketHandlerEvent Read FOnGetSocketHandler Write FOnGetSocketHandler;
    // Called after create socket handler was created, with the created socket handler.
    Property AfterSocketHandlerCreate : TWSSocketHandlerCreatedEvent Read FAfterSocketHandlerCreated Write FAfterSocketHandlerCreated;
    // If >0, when no new connection appeared after timeout, OnAcceptIdle is called.
    Property AcceptIdleTimeout : Cardinal Read FAcceptIdleTimeout Write SetAcceptIdleTimeout;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read FQueueSize Write SetQueueSize Default 5;
    // Run Accept in thread ?  If true, calling StartServer or setting Active to true will return at once.
    Property ThreadedAccept : Boolean Read FThreadedAccept Write FThreadedAccept;
    // Run when accept is idle. AcceptIdleTimeout. Note that this is run in the accept thread.
    Property OnAcceptIdle : TAcceptIdleEvent Read FOnAcceptIdle Write FOnAcceptIdle;
  end;

implementation

uses
  sslsockets;

Type
  { TAcceptThread }
  TAcceptThread = Class(TThread)
  Private
    FServer : TWebSocketServer;
  Public
    Constructor Create(aServer : TWebSocketServer);
    Procedure Execute; override;
  end;


{ TAcceptThread }

constructor TAcceptThread.Create(aServer: TWebSocketServer);
begin
  FreeOnTerminate:=True;
  FServer:=aServer;
  Inherited Create(False);
end;

procedure TAcceptThread.Execute;

begin
  FServer.StartAccepting;
end;


{ TWebSocketServer }

procedure TWebSocketServer.SetUseSSL(AValue: Boolean);
begin
  if FUseSSL=AValue then Exit;
  CheckInactive;
  FUseSSL:=AValue;
end;


procedure TWebSocketServer.SetPort(AValue: Word);
begin
  if FPort=AValue then Exit;
  CheckInactive;
  FPort:=AValue;
end;

procedure TWebSocketServer.SetQueueSize(AValue: Word);
begin
  if FQueueSize=AValue then Exit;
  FQueueSize:=AValue;
end;


function TWebSocketServer.CreateSSLSocketHandler : TSocketHandler;

Var
  S : TSSLSocketHandler;
  CK : TCertAndKey;

begin
  S:=TSSLSocketHandler.GetDefaultHandler;
  try
    // We must create the certificate once in our global copy of CertificateData !
    if CertificateData.NeedCertificateData then
      begin
      S.CertGenerator.HostName:=CertificateData.Hostname;
      CK:=S.CertGenerator.CreateCertificateAndKey;
      CertificateData.Certificate.Value:=CK.Certificate;
      CertificateData.PrivateKey.Value:=CK.PrivateKey;
      end;
    S.CertificateData:=Self.CertificateData;
    Result:=S;
  except
    S.free;
    Raise;
  end;
end;


function TWebSocketServer.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;

begin
  Result:=Nil;
  if Assigned(FonGetSocketHandler) then
    FOnGetSocketHandler(Self,UseSSL,Result);
  if (Result=Nil) then
    If UseSSL then
      Result:=CreateSSLSocketHandler
    else
      Result:=TSocketHandler.Create;
  if Assigned(FAfterSocketHandlerCreated) then
    FAfterSocketHandlerCreated(Self,Result);
end;

procedure TWebSocketServer.DoCreateClientHandler(Sender: TObject; out AHandler: TSocketHandler);
begin
  AHandler:=GetSocketHandler(UseSSL);
end;


procedure TWebSocketServer.DoConnect(Sender: TObject; Data: TSocketStream);

Var
  Con : TWSServerConnection;

begin
  Con:=CreateWebsocketConnection(Data,Options);
  Con.OnControl:=@DoControlReceived;
  Con.OnMessageReceived:=@DoMessageReceived;
  Con.OnDisconnect:=@DoDisconnect;
  Con.OnHandshake:=OnConnectionHandshake;
  if Not AllowConnection(Con) then
    Con.Free
  else
    begin
    Connections.Add(Con);
    ConnectionHandler.HandleConnection(Con,True);

    if Assigned(OnConnect) then
      OnConnect(Self,Con);
    end;
end;

procedure TWebSocketServer.DoAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  HandleError(Nil,E);
  ErrorAction:=aeaStop;
end;

procedure TWebSocketServer.DoAcceptIdle(Sender: TObject);

Var
  doCheckMessages : Boolean;

begin
  doCheckMessages:=True;
  if Assigned(OnAcceptIdle) then
    OnAcceptIdle(Self,doCheckMessages);
  if doCheckMessages then
    ConnectionHandler.CheckIncomingMessages;
end;

procedure TWebSocketServer.DoAllowConnect(Sender: TObject; ASocket: Longint; var Allow: Boolean);
begin
  Allow:=True;
end;

procedure TWebSocketServer.SetAcceptIdleTimeout(AValue: Cardinal);
begin
  if FAcceptIdleTimeout=AValue then Exit;
  CheckInactive;
  FAcceptIdleTimeout:=AValue;
end;

procedure TWebSocketServer.SetCertificateData(AValue: TCertificateData);
begin
  if FCertificateData=AValue then Exit;
  CheckInactive;
  FCertificateData.Assign(AValue);
end;

procedure TWebSocketServer.SetHost(const AValue: String);
begin
  if FHost=AValue then Exit;
  CheckInactive;
  FHost:=AValue;
end;

function TWebSocketServer.GetActive: Boolean;
begin
  Result:=Assigned(FServer);
end;

procedure TWebSocketServer.SetActive(const aValue: Boolean);
begin
  if AValue=GetActive then exit;
  if AValue then
    StartServer
  else
    StopServer;
end;

procedure TWebSocketServer.SetThreadMode(AValue: TWSThreadMode);
begin
  inherited SetThreadMode(AValue);
  If (ThreadMode<>wtmThread) and (AcceptIdleTimeout=0) then
    AcceptIdleTimeout:=DefaultAcceptTimeout;
end;

procedure TWebSocketServer.FreeServerSocket;
begin
  FreeAndNil(FServer);
  FreeConnectionHandler;
end;

procedure TWebSocketServer.CreateServerSocket;

begin
  FServer:=TInetServer.Create(FHost,FPort);
  FServer.OnCreateClientSocketHandler:=@DoCreateClientHandler;
  FServer.MaxConnections:=-1;
  FServer.OnConnectQuery:=@DoAllowConnect;
  FServer.OnConnect:=@DoConnect;
  FServer.OnAcceptError:=@DoAcceptError;
  FServer.OnIdle:=@DoAcceptIdle;
  FServer.AcceptIdleTimeOut:=AcceptIdleTimeout;
end;

procedure TWebSocketServer.SetupSocket;

begin
  FServer.QueueSize:=Self.QueueSize;
  FServer.ReuseAddress:=true;
end;


procedure TWebSocketServer.StartServerSocket;
begin
  FServer.Bind;
  FServer.Listen;
  if ThreadedAccept then
    TAcceptThread.Create(Self)
  else
    begin
    StartAccepting;
    StopServer;
    end;
end;

procedure TWebSocketServer.StartAccepting;

begin
  FActive:=True;
  FServer.StartAccepting;
end;

procedure TWebSocketServer.StartServer;

begin
  StartConnectionHandler;
  CreateServerSocket;
  SetupSocket;
  StartServerSocket;
end;

procedure TWebSocketServer.StopServer;
begin
  FActive:=False;
  FServer.StopAccepting(True);
  ConnectionHandler.CloseConnections;
  WaitForConnections(10);
  FreeServerSocket;
end;

procedure TWebSocketServer.CheckIncomingMessages;
begin
  if Assigned(ConnectionHandler) then
    ConnectionHandler.CheckIncomingMessages;
end;

constructor TWebSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHost:='0.0.0.0';
  FPort:=8080;
  FQueueSize:=5;
  FMessageWaitTime:=DefaultWaitTime;
end;


destructor TWebSocketServer.Destroy;
begin
  Active:=False;
  FreeServerSocket;
  inherited Destroy;
end;



end.
