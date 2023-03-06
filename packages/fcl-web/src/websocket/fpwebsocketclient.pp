{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 - by the Free Pascal development team

    Websocket client implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode ObjFPC}{$H+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwebsocketclient;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, FpWeb.WebSocket.Protocol, System.Net.Ssockets, System.Net.Sslsockets;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils, classes, fpwebsocket, ssockets, sslsockets;
{$ENDIF FPC_DOTTEDUNITS}

Type
  EWebSocketClient = Class(EWebSocket);

  TWSClientHandShakeEvent = Procedure(Sender : TObject; aHeaders : TStrings) of Object;
  TWSClientHandShakeResponseEvent = Procedure(Sender : TObject; aResponse : TWSHandShakeResponse; Var aAllow : Boolean) of Object;
  TWSErrorEvent = Procedure (Sender : TObject; E : Exception) of object;

  { TWSMessagePump }

  TWSMessagePump = Class (TComponent)
  private
    FInterval:Integer;
    FList: TThreadList;
    FReads: TSocketStreamArray;
    FExceptions : TSocketStreamArray;
    FOnError: TWSErrorEvent;
    procedure SetInterval(AValue: Integer);
  Protected
    function WaitForData: Boolean;
    Function CheckConnections : Boolean; virtual;
    Procedure ReadConnections;
    Property List : TThreadList Read FList;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure AddClient(aConnection : TWSClientConnection);
    Procedure RemoveClient(aConnection : TWSClientConnection);
    Procedure Execute; virtual; abstract;
    Procedure Terminate; virtual; abstract;
    Property Interval : Integer Read FInterval Write SetInterval;
    Property OnError : TWSErrorEvent Read FOnError Write FOnError;
  End;

  // Default message driver, works with thread that checks sockets for available data

  TWSThreadMessagePump = Class(TWSMessagePump)
  Private
    FThread : TThread;
    Procedure ThreadTerminated(Sender : TObject);
  Protected
    Type
      TMessageDriverThread = Class(TThread)
      Public
        FPump : TWSThreadMessagePump;
        Constructor Create(aPump : TWSThreadMessagePump; aTerminate : TNotifyEvent);
        Procedure Execute;override;
      End;
  Public
    Procedure Execute; override;
    Procedure Terminate; override;
  End;

  TCustomWebsocketClient = class;

  { TWebSocketClientConnection }

  TWebSocketClientConnection = class(TWSClientConnection)
  protected
    Procedure DoDisconnect; override;
    function GetClient: TCustomWebsocketClient; virtual;
  Public
    Property WebsocketClient : TCustomWebsocketClient Read GetClient;
  end;

  { TCustomWebsocketClient }

  TCustomWebsocketClient = Class(TComponent)
  private
    FOutGoingFrameMask: Integer;
    FPort: Integer;
    FActive: Boolean;
    FLoadActive : Boolean;
    FHostName: String;
    FUseSSL: Boolean;
    FResource: string;
    FConnectTimeout: Integer;
    FOptions: TWSOptions;
    FSocket : TInetSocket;
    FTransport : TWSClientTransport;
    FCheckTimeOut: Integer;
    FAutoCheckMessages: Boolean;
    FHandShake : TWSHandShakeRequest;
    FMessagePump: TWSMessagePump; // Do not free
    FHandshakeResponse: TWSHandShakeResponse;
    FOnSendHandShake: TWSClientHandshakeEvent;
    FOnHandshakeResponse: TWSClientHandshakeResponseEvent;
    FConnection: TWebSocketClientConnection;
    FOnMessageReceived: TWSMessageEvent;
    FOnControl: TWSControlEvent;
    FOnDisconnect: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    procedure FreeConnectionObjects;
    procedure SetActive(const Value: Boolean);
    procedure SetHostName(const Value: String);
    procedure SetMessagePump(AValue: TWSMessagePump);
    procedure SetPort(const Value: Integer);
    procedure SetUseSSL(const Value: Boolean);
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetResource(const Value: string);
    procedure SetCheckTimeOut(const Value: Integer);
    procedure SetOptions(const Value: TWSOptions);
    procedure SetAutoCheckMessages(const Value: Boolean);
    procedure SendHeaders(aHeaders: TStrings);
    procedure ConnectionDisconnected(Sender: TObject);
  Protected
    Procedure CheckInactive;
    Procedure Loaded; override;
    function CreateClientConnection(aTransport : TWSClientTransport): TWebSocketClientConnection; virtual;
    procedure MessageReceived(Sender: TObject; const aMessage : TWSMessage);
    Procedure ControlReceived(Sender: TObject; aType : TFrameType; const aData: TBytes);virtual;
    function CheckHandShakeResponse(aHeaders: TStrings): Boolean; virtual;
    function CreateHandShakeRequest: TWSHandShakeRequest; virtual;
    function CreateHandshakeResponse(aHeaders: TStrings): TWSHandShakeResponse; virtual;
    procedure SendHandShakeRequest; virtual;
    function ReadHandShakeResponse: Boolean; virtual;
    Function DoHandShake: Boolean;
    Property Transport: TWSClientTransport Read FTransport;
  Public
    Property Connection: TWebSocketClientConnection Read FConnection;
  Public
    Destructor Destroy; override;
    // Check for incoming messages
    Function CheckIncoming : TIncomingResult;
    // Connect and perform handshake
    Procedure Connect;
    // Disconnect from server.
    Procedure Disconnect(SendClose : boolean = true);
    // Send a ping message
    Procedure Ping(aMessage: UTF8String);
    // Send a pong message
    Procedure Pong(aMessage: UTF8String);
    // Send raw data (ftBinary)
    Procedure SendData(aBytes : TBytes);
    // Send a string message
    Procedure SendMessage(Const aMessage : String);
  Public
    // Connect/Disconnect
    Property Active : Boolean Read FActive Write SetActive;
    // Check for message timeout
    Property CheckTimeOut : Integer Read FCheckTimeOut Write SetCheckTimeOut;
    // Timeout for connect
    Property ConnectTimeout : Integer Read FConnectTimeout Write SetConnectTimeout;
    // Host to connect to
    Property HostName : String Read FHostName Write SetHostName;
    // Message driver
    Property MessagePump : TWSMessagePump Read FMessagePump Write SetMessagePump;
    // Options
    Property Options : TWSOptions Read FOptions Write SetOptions;
    // Mask to use for outgoing frames
    Property OutGoingFrameMask : Integer Read FOutGoingFrameMask Write FOutGoingFrameMask;
    // Port to connect to
    Property Port : Integer Read FPort Write SetPort;
    // Path/Document in HTTP URL for GET request
    Property Resource : string Read FResource Write SetResource;
    // User SSL when connecting
    Property UseSSL : Boolean Read FUseSSL Write SetUseSSL;
    // Events
    // Called when handshake is about to be sent
    Property OnSendHandShake : TWSClientHandshakeEvent Read FOnSendHandShake Write FOnSendHandshake;
    // Called when handshake response is received
    Property OnHandshakeResponse : TWSClientHandshakeResponseEvent Read FOnHandshakeResponse Write FOnHandshakeResponse;
    // Called when a text message is received.
    property OnMessageReceived: TWSMessageEvent read FOnMessageReceived write FOnMessageReceived;
    // Called when a connection is disconnected.
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    // Called when a connection is established
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    // Called when a control message is received.
    property OnControl: TWSControlEvent read FOnControl write FOnControl;
  End;

  TWebsocketClient = Class(TCustomWebsocketClient)
  Published
    Property HostName;
    Property Port;
    Property CheckTimeOut;
    Property ConnectTimeout;
    Property MessagePump;
    Property Options;
    Property Resource;
    Property UseSSL;
    Property OnSendHandShake;
    Property OnHandshakeResponse;
    property OnMessageReceived;
    property OnDisconnect;
    property OnConnect;
    property OnControl;
    Property OutGoingFrameMask;
  End;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Sha1;
{$ELSE FPC_DOTTEDUNITS}
uses sha1;
{$ENDIF FPC_DOTTEDUNITS}

{ TWebSocketClientConnection }

procedure TWebSocketClientConnection.DoDisconnect;
begin
  If Assigned(WebSocketClient) then
    WebSocketClient.ConnectionDisconnected(Self);
end;

function TWebSocketClientConnection.GetClient: TCustomWebsocketClient;

begin
  Result:=Owner as TCustomWebsocketClient;
end;


{ TCustomWebsocketClient }

procedure TCustomWebsocketClient.CheckInactive;
begin
  If Active then
    Raise EWebSocketClient.Create(SErrConnectionActive);
end;

Function TCustomWebsocketClient.CheckIncoming : TIncomingResult;

begin
  If Not Active then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  if Not Connection.HandshakeCompleted then
    Raise EWebSocketClient.Create(SErrHandshakeInComplete);
  Result:=Connection.CheckIncoming(CheckTimeout);
  if (Result=irClose) then
    begin
    Disconnect(False);
    end;
end;

procedure TCustomWebsocketClient.ControlReceived(Sender: TObject; aType : TFrameType; const aData: TBytes);
begin
  If Assigned(FOnControl) then
    FOnControl(Sender, aType, aData);
end;

function TCustomWebsocketClient.CreateClientConnection(aTransport: TWSClientTRansport): TWebsocketClientConnection;

begin
  Result:=TWebSocketClientConnection.Create(Self,aTransport,FOptions);
end;

procedure TCustomWebsocketClient.ConnectionDisconnected(Sender : TObject);

begin
  FActive:=False;
  If Assigned(MessagePump) then
    MessagePump.RemoveClient(FConnection);
  If Assigned(OnDisconnect) then
    OnDisconnect(FConnection);
  // We cannot free the connection here, because it still needs to call it's own OnDisconnect.
end;

procedure TCustomWebsocketClient.Connect;
var
  SSLHandler: TSSLSocketHandler;
begin
  If Active then
    Exit;
  // Safety: Free any dangling objects before recreating
  FreeConnectionObjects;
  SSLHandler := nil;
  if UseSSL then
  begin
    SSLHandler := TSSLSocketHandler.GetDefaultHandler;
    SSLHandler.VerifyPeerCert := False;
  end;
  FSocket:=TInetSocket.Create(HostName,Port,ConnectTimeout, SSLHandler);
  FTransport:=TWSClientTransport.Create(FSocket);
  FConnection:=CreateClientConnection(FTransport);
  FConnection.OnMessageReceived:=@MessageReceived;
  FConnection.OnControl:=@ControlReceived;
  // RFC states we MUST use a mask.
  if OutGoingFrameMask=0 then
    OutGoingFrameMask:=1+Random(MaxInt-1);
  FConnection.OutgoingFrameMask:=Self.OutGoingFrameMask;
  if UseSSL then
    FSocket.Connect;
  FActive:=True;
  if not DoHandShake then
    Disconnect(False)
  else
    begin
    If Assigned(MessagePump) then
      MessagePump.AddClient(FConnection);
    if Assigned(OnConnect) then
      OnConnect(Self);
    end;
end;


destructor TCustomWebsocketClient.Destroy;
begin
  DisConnect(False);
  FreeAndNil(FHandShake);
  FreeAndNil(FHandshakeResponse);
  FreeConnectionObjects;
  Inherited;
end;


Function TCustomWebsocketClient.CreateHandShakeRequest : TWSHandShakeRequest;

begin
  Result:=TWSHandShakeRequest.Create('',Nil);
end;

procedure TCustomWebsocketClient.SendData(aBytes: TBytes);

begin
  Connection.Send(aBytes);
end;

procedure TCustomWebsocketClient.SendHeaders(aHeaders : TStrings);

Var
  S : String;
  B : TBytes;

begin
  for S in AHeaders do
    begin
    B:=TEncoding.UTF8.GetAnsiBytes(S+#13#10);
    Connection.Transport.WriteBytes(B,Length(B));
    end;
  B:=TEncoding.UTF8.GetAnsiBytes(#13#10);
  Connection.Transport.WriteBytes(B,Length(B));
end;

procedure TCustomWebsocketClient.SendHandShakeRequest;

Var
  aRequest : TWSHandShakeRequest;
  aHeaders : TStrings;
begin
  aHeaders:=Nil;
  FreeAndNil(FHandShake);
  aRequest:=CreateHandShakeRequest;
  try
    aRequest.Host:=HostName;
    aRequest.Port:=Port;
    aRequest.Resource:=Resource;
    aHeaders:=TStringList.Create;
    aHeaders.NameValueSeparator:=':';
    aRequest.ToStrings(aHeaders);
    if Assigned(FOnSendHandshake) then
      FOnSendHandshake(self,aHeaders);
    // Do not use FClient.WriteHeader, it messes up the strings !
    SendHeaders(aHeaders);
    FHandShake:=aRequest;
  finally
    aHeaders.Free;
    if FhandShake<>aRequest then
      aRequest.Free;
  end;
end;

procedure TCustomWebsocketClient.SendMessage(const aMessage: String);
begin
  Connection.Send(aMessage);
end;

Function TCustomWebsocketClient.CreateHandshakeResponse(aHeaders : TStrings) : TWSHandShakeResponse;

begin
  Result:=TWSHandShakeResponse.Create('',aHeaders);
end;

Function TCustomWebsocketClient.CheckHandShakeResponse(aHeaders : TStrings) : Boolean;

Var 
  K : String;
  {%H-}hash : TSHA1Digest;
  B : TBytes;

begin
  B:=[];
  FreeAndNil(FHandshakeResponse);
  FHandshakeResponse:=CreateHandshakeResponse(aHeaders);
  k := Trim(FHandshake.Key) + SSecWebSocketGUID;
  hash:=SHA1String(k);
  SetLength(B,SizeOf(hash));
  Move(hash[0],B[0],SizeOf(hash));
  k:=EncodeBytesBase64(B);
  Result:=SameText(K,FHandshakeResponse.Accept)
          and SameText(FHandshakeResponse.Upgrade,'websocket');
end;

Function TCustomWebsocketClient.ReadHandShakeResponse : Boolean;

Var
  S : String;
  aHeaders : TStrings;

begin
  Result:=False;
  aHeaders:=TStringList.Create;
  Try
    aHeaders.NameValueSeparator:=':';
    Repeat
      S:=Connection.Transport.ReadLn;
      aHeaders.Add(S);
    Until (S='');
    Result:=CheckHandShakeResponse(aHeaders);
    if Result and Assigned(FOnHandshakeResponse) then
       FOnHandshakeResponse(Self,FHandShakeResponse,Result);
    if Result then
      FConnection.HandshakeResponse:=FHandShakeResponse
  Finally
    aHeaders.Free;
  End;
end;

Function TCustomWebsocketClient.DoHandShake : Boolean;

begin
  SendHandShakeRequest;
  Result:=ReadHandShakeResponse;
end;

procedure TCustomWebsocketClient.Loaded;
begin
  inherited;
  if FLoadActive then
    Connect;
end;

procedure TCustomWebsocketClient.MessageReceived(Sender: TObject; const aMessage : TWSMessage) ;
begin
  if Assigned(OnMessageReceived) and (TWSClientConnection(Sender).HandshakeCompleted) then
    OnMessageReceived(Self, AMessage);
end;

procedure TCustomWebsocketClient.Ping(aMessage: UTF8String);
begin
  FConnection.Send(ftPing,TEncoding.UTF8.GetAnsiBytes(aMessage));
end;

procedure TCustomWebsocketClient.Pong(aMessage: UTF8String);
begin
  FConnection.Send(ftPong,TEncoding.UTF8.GetAnsiBytes(aMessage));
end;

procedure TCustomWebsocketClient.FreeConnectionObjects;

begin
  FreeAndNil(FConnection);
  FTransport:=nil; // FTransport is freed in TWSClientConnection.Destroy
  FSocket:=nil; // FSocket is freed in TWSClientTransport.Destroy
end;

procedure TCustomWebsocketClient.Disconnect(SendClose : boolean = true);

begin
  if Not Active then
    Exit;
  if SendClose and (Connection.CloseState <> csClosed) then
    Connection.Close('');
  if Assigned(MessagePump) then
    MessagePump.RemoveClient(Connection);
  If Assigned(OnDisconnect) then
    OnDisconnect(Self);
  FreeConnectionObjects;
  FActive:=False;
end;

procedure TCustomWebsocketClient.SetActive(const Value: Boolean);
begin
  FLoadActive := Value;
  if (csDesigning in ComponentState) then
    exit;
  if Value then
    Connect
  else
    Disconnect;
end;

procedure TCustomWebsocketClient.SetAutoCheckMessages(const Value: Boolean);
begin
  CheckInactive;
  FAutoCheckMessages := Value;
end;

procedure TCustomWebsocketClient.SetCheckTimeOut(const Value: Integer);
begin
  CheckInactive;
  FCheckTimeOut := Value;
end;

procedure TCustomWebsocketClient.SetConnectTimeout(const Value: Integer);
begin
  CheckInactive;
  FConnectTimeout := Value;
end;

procedure TCustomWebsocketClient.SetHostName(const Value: String);
begin
  CheckInactive;
  FHostName := Value;
end;

procedure TCustomWebsocketClient.SetMessagePump(AValue: TWSMessagePump);
begin
  if FMessagePump=AValue then Exit;
  If Assigned(FMessagePump) then
    FMessagePump.RemoveFreeNotification(Self);
  FMessagePump:=AValue;
  If Assigned(FMessagePump) then
    FMessagePump.FreeNotification(Self);
end;

procedure TCustomWebsocketClient.SetOptions(const Value: TWSOptions);
begin
  CheckInactive;
  FOptions := Value;
end;

procedure TCustomWebsocketClient.SetPort(const Value: Integer);
begin
  CheckInactive;
  FPort := Value;
end;

procedure TCustomWebsocketClient.SetResource(const Value: string);
begin
  CheckInactive;
  FResource := Value;
end;

procedure TCustomWebsocketClient.SetUseSSL(const Value: Boolean);
begin
  CheckInactive;
  FUseSSL := Value;
end;


{ TTMSClientWebSocketConnection }



{ TWSMessagePump }

procedure TWSMessagePump.AddClient(aConnection: TWSClientConnection);
begin
  List.Add(aConnection);
end;

procedure TWSMessagePump.RemoveClient(aConnection: TWSClientConnection);
begin
  FList.Remove(aConnection);
end;

procedure TWSMessagePump.SetInterval(AValue: Integer);
begin
  if FInterval=AValue then Exit;
  FInterval:=AValue;
end;

Function TWSMessagePump.WaitForData : Boolean;

Var
  dummy1,dummy2 : TSocketStreamArray;

begin
  Dummy1:=Nil;
  Dummy2:=Nil;
  Result:=False;
  // FReadSet was populated by checkconnections
  SetLength(FExceptions,0);
  if Length(FReads)=0 then
    begin
    TThread.Sleep(FInterval);
    end
  else
    begin
    try
      // We take the first ont in the list.
      Result := FReadS[0].Select(FReads,dummy1,dummy2,FInterval);
    except
      Result := False;
    end;
    end;
end;

function TWSMessagePump.CheckConnections: Boolean;

Var
  aList : TList;
  aClient: TWSClientConnection;
  aTrans : TWSClientTransport;
  I,aLen : Integer;

begin
  Result:=False;
  aList := List.LockList;
  try
    aLen:=0;
    SetLength(FReads,aList.Count);
    for I := 0 to aList.Count - 1 do
      begin
      aClient := TWSClientConnection(aList.Items[I]);
      if assigned(aClient) then
        aTrans:=aClient.ClientTransport
      else
        aTrans:=Nil;
      if (aTrans<>nil) then
        begin
        // There is already data
        FReads[aLen]:=aTrans.Socket;
        Inc(aLen);
        end;
      end;
  finally
    List.UnlockList;
  end;
  if Not Result then
    Result:=WaitForData;
end;

constructor TWSMessagePump.Create(aOwner : TComponent);
begin
  FList:=TThreadList.Create;
  FReads:=[];
  FExceptions:=[];
  Finterval:=50;
end;

destructor TWSMessagePump.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TWSMessagePump.ReadConnections;

Var
  aList : TList;
  aClient: TWSClientConnection;
  I : Integer;

begin
  try
    aList := List.LockList;
    try
      FReads:=[];
      for I := 0 to aList.Count - 1 do
        begin
        aClient:= TWSClientConnection(aList.Items[I]);
        if assigned(aClient.Transport) then
           aClient.CheckIncoming(1);
        end;
    finally
      List.UnlockList;
    end;
  except
    on E: Exception do
      if Assigned(OnError) then
        OnError(Self,E);
  end;
end;


{ TWSThreadMessagePump }

procedure TWSThreadMessagePump.Execute;
begin
  FThread:=TMessageDriverThread.Create(Self,@ThreadTerminated);
end;

procedure TWSThreadMessagePump.ThreadTerminated(Sender: TObject);
begin
  FThread:=Nil;
end;

procedure TWSThreadMessagePump.Terminate;
begin
  FThread.Terminate;
  if Assigned(FThread) then
    FThread.WaitFor;
end;

{ TWSThreadMessagePump.TMessageDriverThread }

constructor TWSThreadMessagePump.TMessageDriverThread.Create(aPump: TWSThreadMessagePump; aTerminate : TNotifyEvent);

begin
  FPump:=aPump;
  OnTerminate:=aTerminate;
  Inherited Create(False);
end;

procedure TWSThreadMessagePump.TMessageDriverThread.Execute;

begin
  While Not Terminated do
    if FPump.CheckConnections then
      FPump.ReadConnections
    else
      TThread.Sleep(FPump.Interval);
end;

end.
