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
unit FpWebSocketClient;
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
    FCore: TObject;
    FEntries: TThreadList;
    FList: TThreadList;
    FRegistryLock: TRTLCriticalSection;
    FReads: TSocketStreamArray;
    FExceptions : TSocketStreamArray;
    FOnError: TWSErrorEvent;
    function FindEntry(aConnection: TWSClientConnection): TObject;
    procedure RemoveEntry(aEntry: TObject);
    procedure StartEntry(aEntry: TObject; aRunGeneration: LongInt);
    procedure EntryEnded(aEntry: TObject; aError: Exception);
    procedure ReportError(aError: Exception);
    procedure ClearEntries;
    procedure SetInterval(AValue: Integer);
  Protected
    Procedure InterruptConnections;
    function WaitForData: Boolean;
    Function CheckConnections : Boolean; virtual;
    Procedure ReadConnections;
    Property List : TThreadList Read FList;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Register a connection created by TCustomWebsocketClient. Its internal
    // session supplies the lifetime lease required by the threaded reader.
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
    FLifecycleLock : TRTLCriticalSection;
    function TryFinalize(aTimeoutMs: Integer;
      aInterruptReaders: Boolean): Boolean;
    procedure RequestStop;
  Protected
    Type
      TMessageDriverThread = Class(TThread)
      Public
        FPump : TWSThreadMessagePump;
        FRunGeneration : LongInt;
        Constructor Create(aPump : TWSThreadMessagePump;
          aTerminate : TNotifyEvent);
        Procedure Execute;override;
      End;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Execute; override;
    Procedure Terminate; override;
  End;

  TCustomWebsocketClient = class;

  { TWebSocketClientConnection }

  TWebSocketClientConnection = class(TWSClientConnection)
  private
    FClientSession: TObject;
    procedure SetClientSession(aSession: TObject);
  protected
    Procedure DoDisconnect; override;
    function GetClient: TCustomWebsocketClient; virtual;
  Public
    procedure Send(aFrame: TWSFrame); overload; override;
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
    FMaxFramePayloadSize: QWord;
    FMaxMessagePayloadSize: QWord;
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
    FStateLock: TRTLCriticalSection;
    FDestroying: Boolean;
    FNextGeneration: QWord;
    FConnectingGeneration: QWord;
    FOwnerGate: TObject;
    FSession: TObject;
    function GetActive: Boolean;
    function GetConnection: TWebSocketClientConnection;
    function AcquireCurrentSession: TObject;
    function AcquireHandshakeSession: TObject;
    function IsCurrentSession(aSession: TObject): Boolean;
    procedure DisconnectSession(aSession: TObject; SendClose: Boolean;
      aEventSender: TObject);
    function DetachCurrentSession(aSession: TObject): Boolean;
    procedure SessionMessageReceived(aSession: TObject;
      const aMessage: TWSMessage);
    procedure SessionControlReceived(aSession: TObject; aEventSender: TObject;
      aType: TFrameType; const aData: TBytes);
    procedure SessionDisconnected(aSession: TObject; aEventSender: TObject;
      aPump: TWSMessagePump);
    procedure SessionConnected(aSession: TObject);
    procedure ReportCallbackError(aPump: TWSMessagePump; E: Exception);
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
    procedure SetMaxFramePayloadSize(const Value: QWord);
    procedure SetMaxMessagePayloadSize(const Value: QWord);
    procedure SetAutoCheckMessages(const Value: Boolean);
    procedure SendHeaders(aHeaders: TStrings);
    procedure ConnectionDisconnected(Sender: TObject);
  Protected
    Procedure CheckInactive;
    Procedure Loaded; override;
    Procedure Notification(aComponent : TComponent;
      Operation : TOperation); override;
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
    Property Connection: TWebSocketClientConnection Read GetConnection;
  Public
    Constructor Create(aOwner : TComponent); override;
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
    Property Active : Boolean Read GetActive Write SetActive;
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
    // Maximum accepted frame payload, 0 means unlimited.
    Property MaxFramePayloadSize : QWord Read FMaxFramePayloadSize
      Write SetMaxFramePayloadSize;
    // Maximum accepted reassembled message payload, 0 means unlimited.
    Property MaxMessagePayloadSize : QWord Read FMaxMessagePayloadSize
      Write SetMaxMessagePayloadSize;
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
    Property MaxFramePayloadSize;
    Property MaxMessagePayloadSize;
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

{ Internal design overview

  The public client/component model is retained, but connections are managed
  internally as generations:

  * TWSClientSession represents one successful or in-progress Connect.  It
    owns the connection, transport, socket and handshake objects.  The client,
    pump registration, reader and temporary API operations hold counted
    leases, so an old generation can finish without touching a reconnect.
  * TWSMessagePumpEntry is the stable registry identity.  Removal is by entry
    or connection identity, never by an index retained across a callback.
  * TWSPumpCore outlives the component while workers drain.  Workers acquire a
    short owner admission before calling pump code, so pump destruction cannot
    race EntryEnded or OnError.
  * TWSClientOwnerGate protects all callbacks into TCustomWebsocketClient.
    Destruction closes admission and waits for admitted callbacks, servicing
    CheckSynchronize on the main thread while it waits.

  The threaded pump uses one blocking reader per registered session.  Its
  driver still calls the protected CheckConnections/ReadConnections pipeline,
  but ReadConnections schedules readers rather than performing a competing
  read.  This prevents one incomplete frame from starving healthy siblings.

  Terminal notification first acquires callback admission, then claims the
  disconnect once, marks the exact session closing, updates client-visible
  state, removes its registry entry with durable read cancellation, and
  finally invokes OnDisconnect. Application callbacks run without the
  registry, state or transport write locks held.

  Disconnect and reconnect are supported from message, control and disconnect
  callbacks, including methods reached through TThread.Synchronize.  Directly
  freeing a client or pump from its own callback is rejected: Pascal cannot
  safely return from that callback through an already destroyed object.

  TWSMessagePump.AddClient accepts the public base parameter for compatibility
  but requires a TWebSocketClientConnection carrying this unit's managed
  session.  A raw TWSClientConnection has no lifetime lease and therefore
  cannot be read safely while an unrelated owner may destroy it.

  See README.md in this directory for the full invariants, shutdown algorithm,
  compatibility decisions and regression-test recipe. }

Const
  WSClientSessionOpen = 0;
  WSClientSessionClosing = 1;
  WSClientSessionClosed = 2;
  WSPumpEntryRegistered = 0;
  WSPumpEntryRemoved = 1;
  WSMaxHandshakeHeaderLineBytes = 8192;
  WSMaxHandshakeHeaderLines = 256;
  WSMaxHandshakeHeaderBytes = 65536;

Resourcestring
  SErrClientSessionClosing = 'WebSocket client session is closing';
  SErrFreeFromCallback = 'A websocket client cannot be freed from its own callback';
  SErrHandshakeHeaderLineTooLong = 'WebSocket handshake response header line is too long';
  SErrHandshakeHeadersTooLarge = 'WebSocket handshake response headers are too large';

Type
  TWSPumpCore = Class;
  TWSMessagePumpEntry = Class;
  TWSClientOwnerGate = Class;
  PWSOwnerGateToken = ^TWSOwnerGateToken;
  TWSOwnerGateToken = Record
    Gate : TWSClientOwnerGate;
    Previous : PWSOwnerGateToken;
  end;

  { TWSClientOwnerGate

    This object, rather than a connection method pointer, protects callbacks
    into the component.  It is shared by every generation of a client and may
    therefore outlive the component itself. }

  TWSClientOwnerGate = Class
  Private
    FReferenceCount : LongInt;
    FLock : TRTLCriticalSection;
    FNoCallbacks : PRTLEvent;
    FClient : TCustomWebsocketClient;
    FAdmissionOpen : Boolean;
    FCallbackCount : LongInt;
  Public
    Constructor Create(aClient : TCustomWebsocketClient);
    Destructor Destroy; override;
    Procedure AddReference;
    Procedure ReleaseReference;
    Function TryEnter(out aClient : TCustomWebsocketClient;
      out aToken : TWSOwnerGateToken) : Boolean;
    Procedure Leave(var aToken : TWSOwnerGateToken);
    Procedure CloseAdmission;
    Procedure WaitForQuiescence;
    Function HasCallbacks : Boolean;
    Function IsCurrentCallback : Boolean;
  end;

  { TWSClientSession

    One instance represents exactly one Connect generation.  It owns the
    concrete connection (and through it the transport and socket). }

  TWSClientSession = Class
  Private
    FReferenceCount : LongInt;
    FState : LongInt;
    FDisconnectClaimed : LongInt;
    FGeneration : QWord;
    FOwnerGate : TWSClientOwnerGate;
    FConnection : TWebSocketClientConnection;
    FHandshakeRequest : TWSHandShakeRequest;
    FHandshakeResponse : TWSHandShakeResponse;
    Procedure MessageReceived(Sender : TObject; const aMessage : TWSMessage);
    Procedure ControlReceived(Sender : TObject; aType : TFrameType;
      const aData : TBytes);
  Public
    Constructor Create(aOwnerGate : TWSClientOwnerGate;
      aGeneration : QWord);
    Destructor Destroy; override;
    Procedure AddReference;
    Procedure ReleaseReference;
    Procedure AttachConnection(aConnection : TWebSocketClientConnection);
    Procedure SetHandshakeRequest(aRequest : TWSHandShakeRequest);
    Procedure SetHandshakeResponse(aResponse : TWSHandShakeResponse);
    Function TakeHandshakeResponse : TWSHandShakeResponse;
    Function IsOpen : Boolean;
    Function BeginClosing : Boolean;
    Procedure CancelReads;
    Procedure InterruptRead;
    Procedure ConnectionRequestedDisconnect(aEventSender : TObject);
    Procedure NotifyDisconnected(aPump : TWSMessagePump;
      aEventSender : TObject);
    Procedure NotifyConnected;
    Property Connection : TWebSocketClientConnection Read FConnection;
    Property Generation : QWord Read FGeneration;
    Property HandshakeRequest : TWSHandShakeRequest Read FHandshakeRequest;
  end;

  { Stable pump registration.  Registry, worker and temporary users each own
    a reference.  The entry owns one session reference. }

  TWSMessagePumpEntry = Class
  Private
    FReferenceCount : LongInt;
    FState : LongInt;
    FWorkerRunning : LongInt;
    FSession : TWSClientSession;
    FConnection : TWSClientConnection;
  Public
    Constructor Create(aSession : TWSClientSession;
      aConnection : TWSClientConnection);
    Destructor Destroy; override;
    Procedure AddReference;
    Procedure ReleaseReference;
    Function MarkRemoved : Boolean;
    Function IsRegistered : Boolean;
    Function TryStartWorker : Boolean;
    Procedure WorkerStopped;
    Function WorkerRunning : Boolean;
    Procedure CancelReads;
    Procedure InterruptRead;
    Property Connection : TWSClientConnection Read FConnection;
    Property Session : TWSClientSession Read FSession;
  end;

  { Refcounted state used by reader workers.  A worker never retains the
    component pointer directly; it obtains a short owner admission here. }

  TWSPumpCore = Class
  Private
    FReferenceCount : LongInt;
    FLock : TRTLCriticalSection;
    FNoWorkers : PRTLEvent;
    FNoOwnerUsers : PRTLEvent;
    FPump : TWSMessagePump;
    FOwnerOpen : Boolean;
    FOwnerUsers : LongInt;
    FWorkerCount : LongInt;
    FRunning : Boolean;
    FRunGeneration : LongInt;
    FInterval : Integer;
  Public
    Constructor Create(aPump : TWSMessagePump; aInterval : Integer);
    Destructor Destroy; override;
    Procedure AddReference;
    Procedure ReleaseReference;
    Function BeginRun(out aGeneration : LongInt) : Boolean;
    Procedure RequestStop;
    Function IsRunning(aGeneration : LongInt) : Boolean;
    Function CurrentGeneration : LongInt;
    Procedure WorkerStarting;
    Procedure WorkerDone;
    Function WorkerCount : LongInt;
    Function WaitWorkers(aTimeoutMs : Integer) : Boolean;
    Function TryEnterPump(out aPump : TWSMessagePump) : Boolean;
    Procedure LeavePump;
    Procedure CloseOwner;
    Procedure WaitOwnerUsers;
    Procedure SetInterval(aValue : Integer);
    Function GetInterval : Integer;
  end;

  TWSClientReaderThread = Class(TThread)
  Private
    FCore : TWSPumpCore;
    FEntry : TWSMessagePumpEntry;
    FRunGeneration : LongInt;
    Procedure FinishWithError(aError : Exception);
  Public
    Constructor Create(aCore : TWSPumpCore; aEntry : TWSMessagePumpEntry;
      aRunGeneration : LongInt);
    Procedure AbandonBeforeStart;
    Procedure Execute; override;
  end;

ThreadVar
  CurrentWSPumpCore : TWSPumpCore;
  CurrentWSOwnerGateToken : PWSOwnerGateToken;
  CurrentWSHandshakeClient : TCustomWebsocketClient;
  CurrentWSHandshakeSession : TWSClientSession;

procedure ReleaseClientSession(aSession: TWSClientSession;
  aPump: TWSMessagePump);
begin
  if not Assigned(aSession) then
    Exit;
  try
    aSession.ReleaseReference;
  except
    on E : Exception do
      if Assigned(aPump) then
        aPump.ReportError(E);
  end;
end;

{ TWSClientOwnerGate }

constructor TWSClientOwnerGate.Create(aClient: TCustomWebsocketClient);
begin
  inherited Create;
  FReferenceCount:=1;
  InitCriticalSection(FLock);
  FNoCallbacks:=RTLEventCreate;
  RTLEventSetEvent(FNoCallbacks);
  FClient:=aClient;
  FAdmissionOpen:=True;
end;

destructor TWSClientOwnerGate.Destroy;
begin
  RTLEventDestroy(FNoCallbacks);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TWSClientOwnerGate.AddReference;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TWSClientOwnerGate.ReleaseReference;
begin
  if InterlockedDecrement(FReferenceCount)=0 then
    Free;
end;

function TWSClientOwnerGate.TryEnter(
  out aClient: TCustomWebsocketClient;
  out aToken: TWSOwnerGateToken): Boolean;
begin
  aClient:=Nil;
  aToken.Gate:=Nil;
  aToken.Previous:=Nil;
  EnterCriticalSection(FLock);
  try
    Result:=FAdmissionOpen and Assigned(FClient);
    if Result then
      begin
      if FCallbackCount=0 then
        RTLEventResetEvent(FNoCallbacks);
      Inc(FCallbackCount);
      aClient:=FClient;
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
  if Result then
    begin
    aToken.Gate:=Self;
    aToken.Previous:=CurrentWSOwnerGateToken;
    CurrentWSOwnerGateToken:=@aToken;
    end;
end;

procedure TWSClientOwnerGate.Leave(var aToken: TWSOwnerGateToken);
begin
  EnterCriticalSection(FLock);
  try
    Dec(FCallbackCount);
    if FCallbackCount=0 then
      RTLEventSetEvent(FNoCallbacks);
  finally
    LeaveCriticalSection(FLock);
  end;
  if CurrentWSOwnerGateToken=@aToken then
    CurrentWSOwnerGateToken:=aToken.Previous;
  aToken.Gate:=Nil;
  aToken.Previous:=Nil;
end;

procedure TWSClientOwnerGate.CloseAdmission;
begin
  EnterCriticalSection(FLock);
  try
    FAdmissionOpen:=False;
    FClient:=Nil;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSClientOwnerGate.WaitForQuiescence;
var
  Pending : LongInt;
begin
  repeat
    EnterCriticalSection(FLock);
    try
      Pending:=FCallbackCount;
    finally
      LeaveCriticalSection(FLock);
    end;
    if Pending=0 then
      Exit;
    if TThread.CurrentThread.ThreadID=MainThreadID then
      CheckSynchronize(0);
    RTLEventWaitFor(FNoCallbacks,1);
  until False;
end;

function TWSClientOwnerGate.HasCallbacks: Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result:=FCallbackCount<>0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TWSClientOwnerGate.IsCurrentCallback: Boolean;
var
  Token : PWSOwnerGateToken;
begin
  Token:=CurrentWSOwnerGateToken;
  while Assigned(Token) and (Token^.Gate<>Self) do
    Token:=Token^.Previous;
  Result:=Assigned(Token);
end;

{ TWSClientSession }

constructor TWSClientSession.Create(aOwnerGate: TWSClientOwnerGate;
  aGeneration: QWord);
begin
  inherited Create;
  FReferenceCount:=1;
  FState:=WSClientSessionOpen;
  FGeneration:=aGeneration;
  FOwnerGate:=aOwnerGate;
  FOwnerGate.AddReference;
end;

destructor TWSClientSession.Destroy;
var
  ConnectionToFree : TWebSocketClientConnection;
begin
  InterlockedExchange(FState,WSClientSessionClosed);
  ConnectionToFree:=FConnection;
  FConnection:=Nil;
  if Assigned(ConnectionToFree) then
    begin
    ConnectionToFree.SetClientSession(Nil);
    ConnectionToFree.OnMessageReceived:=Nil;
    ConnectionToFree.OnControl:=Nil;
    ConnectionToFree.HandshakeResponse:=Nil;
    end;
  try
    try
      FreeAndNil(FHandshakeResponse);
    finally
      try
        FreeAndNil(FHandshakeRequest);
      finally
        if Assigned(ConnectionToFree) then
          begin
          ConnectionToFree.Free;
          end;
      end;
    end;
  finally
    try
      FOwnerGate.ReleaseReference;
    finally
      inherited Destroy;
    end;
  end;
end;

procedure TWSClientSession.SetHandshakeRequest(
  aRequest: TWSHandShakeRequest);
begin
  if FHandshakeRequest=aRequest then
    Exit;
  FreeAndNil(FHandshakeRequest);
  FHandshakeRequest:=aRequest;
end;

procedure TWSClientSession.SetHandshakeResponse(
  aResponse: TWSHandShakeResponse);
begin
  if FHandshakeResponse=aResponse then
    Exit;
  FreeAndNil(FHandshakeResponse);
  FHandshakeResponse:=aResponse;
end;

function TWSClientSession.TakeHandshakeResponse: TWSHandShakeResponse;
begin
  Result:=FHandshakeResponse;
  FHandshakeResponse:=Nil;
end;

procedure TWSClientSession.AddReference;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TWSClientSession.ReleaseReference;
begin
  if InterlockedDecrement(FReferenceCount)=0 then
    Free;
end;

procedure TWSClientSession.AttachConnection(
  aConnection: TWebSocketClientConnection);
begin
  if Assigned(FConnection) then
    Raise EWebSocketClient.Create('A websocket session already has a connection');
  FConnection:=aConnection;
  FConnection.SetClientSession(Self);
  FConnection.OnMessageReceived:=@MessageReceived;
  FConnection.OnControl:=@ControlReceived;
end;

function TWSClientSession.IsOpen: Boolean;
begin
  Result:=InterlockedCompareExchange(FState,WSClientSessionOpen,
    WSClientSessionOpen)=WSClientSessionOpen;
end;

function TWSClientSession.BeginClosing: Boolean;
begin
  Result:=InterlockedCompareExchange(FState,WSClientSessionClosing,
    WSClientSessionOpen)=WSClientSessionOpen;
end;

procedure TWSClientSession.CancelReads;
begin
  if Assigned(FConnection) and Assigned(FConnection.ClientTransport) then
    FConnection.ClientTransport.CancelReads;
end;

procedure TWSClientSession.InterruptRead;
begin
  if Assigned(FConnection) and Assigned(FConnection.ClientTransport) then
    FConnection.ClientTransport.InterruptRead;
end;

procedure TWSClientSession.MessageReceived(Sender: TObject;
  const aMessage: TWSMessage);
var
  aClient : TCustomWebsocketClient;
  GateToken : TWSOwnerGateToken;
begin
  if not IsOpen then
    Exit;
  if not FOwnerGate.TryEnter(aClient,GateToken) then
    Exit;
  try
    if IsOpen then
      aClient.SessionMessageReceived(Self,aMessage);
  finally
    FOwnerGate.Leave(GateToken);
  end;
end;

procedure TWSClientSession.ControlReceived(Sender: TObject; aType: TFrameType;
  const aData: TBytes);
var
  aClient : TCustomWebsocketClient;
  GateToken : TWSOwnerGateToken;
begin
  if not IsOpen then
    Exit;
  if not FOwnerGate.TryEnter(aClient,GateToken) then
    Exit;
  try
    if IsOpen then
      aClient.SessionControlReceived(Self,Sender,aType,aData);
  finally
    FOwnerGate.Leave(GateToken);
  end;
end;

procedure TWSClientSession.ConnectionRequestedDisconnect(aEventSender: TObject);
begin
  AddReference;
  try
    NotifyDisconnected(Nil,aEventSender);
  finally
    ReleaseClientSession(Self,Nil);
  end;
end;

procedure TWSClientSession.NotifyDisconnected(aPump: TWSMessagePump;
  aEventSender: TObject);
var
  aClient : TCustomWebsocketClient;
  GateToken : TWSOwnerGateToken;
begin
  { Admission is deliberately acquired before the once-only claim.  Closing
    a client can then either wait for this admitted notification or claim and
    deliver the notification itself; it can never be silently lost between
    the claim and a closing callback gate. }
  if not FOwnerGate.TryEnter(aClient,GateToken) then
    Exit;
  try
    if InterlockedCompareExchange(FDisconnectClaimed,1,0)=0 then
      aClient.SessionDisconnected(Self,aEventSender,aPump);
  finally
    FOwnerGate.Leave(GateToken);
  end;
end;

procedure TWSClientSession.NotifyConnected;
var
  aClient : TCustomWebsocketClient;
  GateToken : TWSOwnerGateToken;
begin
  if not FOwnerGate.TryEnter(aClient,GateToken) then
    Exit;
  try
    if IsOpen then
      aClient.SessionConnected(Self);
  finally
    FOwnerGate.Leave(GateToken);
  end;
end;

{ TWSMessagePumpEntry }

constructor TWSMessagePumpEntry.Create(aSession: TWSClientSession;
  aConnection: TWSClientConnection);
begin
  inherited Create;
  FReferenceCount:=1;
  FState:=WSPumpEntryRegistered;
  FSession:=aSession;
  FSession.AddReference;
  FConnection:=aConnection;
end;

destructor TWSMessagePumpEntry.Destroy;
begin
  try
    FSession.ReleaseReference;
  finally
    inherited Destroy;
  end;
end;

procedure TWSMessagePumpEntry.AddReference;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TWSMessagePumpEntry.ReleaseReference;
begin
  if InterlockedDecrement(FReferenceCount)=0 then
    Free;
end;

function TWSMessagePumpEntry.MarkRemoved: Boolean;
begin
  Result:=InterlockedCompareExchange(FState,WSPumpEntryRemoved,
    WSPumpEntryRegistered)=WSPumpEntryRegistered;
end;

function TWSMessagePumpEntry.IsRegistered: Boolean;
begin
  Result:=InterlockedCompareExchange(FState,WSPumpEntryRegistered,
    WSPumpEntryRegistered)=WSPumpEntryRegistered;
end;

function TWSMessagePumpEntry.TryStartWorker: Boolean;
begin
  Result:=IsRegistered and FSession.IsOpen and
    (InterlockedCompareExchange(FWorkerRunning,1,0)=0);
  if Result and ((not IsRegistered) or (not FSession.IsOpen)) then
    begin
    InterlockedExchange(FWorkerRunning,0);
    Result:=False;
    end;
end;

procedure TWSMessagePumpEntry.WorkerStopped;
begin
  InterlockedExchange(FWorkerRunning,0);
end;

function TWSMessagePumpEntry.WorkerRunning: Boolean;
begin
  Result:=InterlockedCompareExchange(FWorkerRunning,0,0)<>0;
end;

procedure TWSMessagePumpEntry.CancelReads;
begin
  FSession.CancelReads;
end;

procedure TWSMessagePumpEntry.InterruptRead;
begin
  FSession.InterruptRead;
end;

{ TWSPumpCore }

constructor TWSPumpCore.Create(aPump: TWSMessagePump; aInterval: Integer);
begin
  inherited Create;
  FReferenceCount:=1;
  InitCriticalSection(FLock);
  FNoWorkers:=RTLEventCreate;
  FNoOwnerUsers:=RTLEventCreate;
  RTLEventSetEvent(FNoWorkers);
  RTLEventSetEvent(FNoOwnerUsers);
  FPump:=aPump;
  FOwnerOpen:=True;
  FInterval:=aInterval;
end;

destructor TWSPumpCore.Destroy;
begin
  RTLEventDestroy(FNoOwnerUsers);
  RTLEventDestroy(FNoWorkers);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TWSPumpCore.AddReference;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TWSPumpCore.ReleaseReference;
begin
  if InterlockedDecrement(FReferenceCount)=0 then
    Free;
end;

function TWSPumpCore.BeginRun(out aGeneration: LongInt): Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result:=(not FRunning) and (FWorkerCount=0) and FOwnerOpen;
    if Result then
      begin
      Inc(FRunGeneration);
      if FRunGeneration=0 then
        Inc(FRunGeneration);
      FRunning:=True;
      end;
    aGeneration:=FRunGeneration;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSPumpCore.RequestStop;
begin
  EnterCriticalSection(FLock);
  try
    FRunning:=False;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TWSPumpCore.IsRunning(aGeneration: LongInt): Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result:=FRunning and (FRunGeneration=aGeneration);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TWSPumpCore.CurrentGeneration: LongInt;
begin
  EnterCriticalSection(FLock);
  try
    Result:=FRunGeneration;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSPumpCore.WorkerStarting;
begin
  EnterCriticalSection(FLock);
  try
    if FWorkerCount=0 then
      RTLEventResetEvent(FNoWorkers);
    Inc(FWorkerCount);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSPumpCore.WorkerDone;
begin
  EnterCriticalSection(FLock);
  try
    Dec(FWorkerCount);
    if FWorkerCount=0 then
      RTLEventSetEvent(FNoWorkers);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TWSPumpCore.WorkerCount: LongInt;
begin
  EnterCriticalSection(FLock);
  try
    Result:=FWorkerCount;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TWSPumpCore.WaitWorkers(aTimeoutMs: Integer): Boolean;
var
  Started : QWord;
begin
  Started:=TThread.GetTickCount64;
  repeat
    Result:=WorkerCount=0;
    if Result then
      Exit;
    if (aTimeoutMs>=0) and
       ((TThread.GetTickCount64-Started)>=QWord(aTimeoutMs)) then
      Exit(False);
    if TThread.CurrentThread.ThreadID=MainThreadID then
      CheckSynchronize(0);
    RTLEventWaitFor(FNoWorkers,1);
  until False;
end;

function TWSPumpCore.TryEnterPump(out aPump: TWSMessagePump): Boolean;
begin
  aPump:=Nil;
  EnterCriticalSection(FLock);
  try
    Result:=FOwnerOpen and Assigned(FPump);
    if Result then
      begin
      if FOwnerUsers=0 then
        RTLEventResetEvent(FNoOwnerUsers);
      Inc(FOwnerUsers);
      aPump:=FPump;
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSPumpCore.LeavePump;
begin
  EnterCriticalSection(FLock);
  try
    Dec(FOwnerUsers);
    if FOwnerUsers=0 then
      RTLEventSetEvent(FNoOwnerUsers);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSPumpCore.CloseOwner;
begin
  EnterCriticalSection(FLock);
  try
    FOwnerOpen:=False;
    FPump:=Nil;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TWSPumpCore.WaitOwnerUsers;
var
  Pending : LongInt;
begin
  repeat
    EnterCriticalSection(FLock);
    try
      Pending:=FOwnerUsers;
    finally
      LeaveCriticalSection(FLock);
    end;
    if Pending=0 then
      Exit;
    if TThread.CurrentThread.ThreadID=MainThreadID then
      CheckSynchronize(0);
    RTLEventWaitFor(FNoOwnerUsers,1);
  until False;
end;

procedure TWSPumpCore.SetInterval(aValue: Integer);
begin
  EnterCriticalSection(FLock);
  try
    FInterval:=aValue;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TWSPumpCore.GetInterval: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result:=FInterval;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TWSClientReaderThread }

constructor TWSClientReaderThread.Create(aCore: TWSPumpCore;
  aEntry: TWSMessagePumpEntry; aRunGeneration: LongInt);
begin
  inherited Create(True);
  FCore:=aCore;
  FCore.AddReference;
  FEntry:=aEntry;
  FEntry.AddReference;
  FRunGeneration:=aRunGeneration;
  FCore.WorkerStarting;
  FreeOnTerminate:=True;
end;

procedure TWSClientReaderThread.AbandonBeforeStart;
var
  aPump : TWSMessagePump;
  PreviousCore : TWSPumpCore;
begin
  PreviousCore:=CurrentWSPumpCore;
  CurrentWSPumpCore:=FCore;
  try
    FEntry.WorkerStopped;
    try
      try
        FEntry.ReleaseReference;
      except
        on E : Exception do
          if FCore.TryEnterPump(aPump) then
            try
              aPump.ReportError(E);
            finally
              FCore.LeavePump;
            end;
      end;
    finally
      FCore.WorkerDone;
    end;
  finally
    try
      FCore.ReleaseReference;
    finally
      CurrentWSPumpCore:=PreviousCore;
      FEntry:=Nil;
      FCore:=Nil;
    end;
  end;
end;

procedure TWSClientReaderThread.FinishWithError(aError: Exception);
var
  aPump : TWSMessagePump;
begin
  if FCore.TryEnterPump(aPump) then
    try
      aPump.EntryEnded(FEntry,aError);
    finally
      FCore.LeavePump;
    end;
end;

procedure TWSClientReaderThread.Execute;
var
  IncomingResult : TIncomingResult;
  FinishedConnection : Boolean;
  aPump : TWSMessagePump;
  PreviousCore : TWSPumpCore;
begin
  PreviousCore:=CurrentWSPumpCore;
  CurrentWSPumpCore:=FCore;
  try
    FinishedConnection:=False;
    while FCore.IsRunning(FRunGeneration) and FEntry.IsRegistered and
          FEntry.Session.IsOpen do
      begin
      try
        IncomingResult:=FEntry.Connection.CheckIncoming(FCore.GetInterval);
        if IncomingResult=irClose then
          begin
          FinishWithError(Nil);
          FinishedConnection:=True;
          Break;
          end;
      except
        on E : EWSReadInterrupted do
          begin
          { An interruption observed by the exact reader invalidates a
            partially consumed frame.  Removal/notification is idempotent. }
          FinishWithError(Nil);
          FinishedConnection:=True;
          Break;
          end;
        on E : Exception do
          begin
          FinishWithError(E);
          FinishedConnection:=True;
          Break;
          end;
      end;
      end;
    { Avoid a warning in compilers which do not optimize the loop flag. }
    if FinishedConnection then
      ;
  finally
    try
      FEntry.WorkerStopped;
      try
        try
          FEntry.ReleaseReference;
        except
          on E : Exception do
            begin
            if FCore.TryEnterPump(aPump) then
              try
                aPump.ReportError(E);
              finally
                FCore.LeavePump;
              end;
            end;
        end;
      finally
        FCore.WorkerDone;
      end;
    finally
      try
        FCore.ReleaseReference;
      finally
        CurrentWSPumpCore:=PreviousCore;
      end;
    end;
  end;
end;

{ TWebSocketClientConnection }

procedure TWebSocketClientConnection.DoDisconnect;
begin
  if Assigned(FClientSession) then
    TWSClientSession(FClientSession).ConnectionRequestedDisconnect(Self);
end;

procedure TWebSocketClientConnection.SetClientSession(aSession: TObject);
begin
  FClientSession:=aSession;
end;

procedure TWebSocketClientConnection.Send(aFrame: TWSFrame);
begin
  if Assigned(FClientSession) and
     (not TWSClientSession(FClientSession).IsOpen) then
    Raise EWebSocketClient.Create(SErrClientSessionClosing);
  if not HandshakeCompleted then
    Raise EWebSocketClient.Create(SErrHandshakeInComplete);
  inherited Send(aFrame);
end;

function TWebSocketClientConnection.GetClient: TCustomWebsocketClient;

begin
  Result:=Owner as TCustomWebsocketClient;
end;


{ TCustomWebsocketClient }

constructor TCustomWebsocketClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FStateLock);
  FMaxFramePayloadSize:=DefaultMaxFramePayloadSize;
  FMaxMessagePayloadSize:=DefaultMaxMessagePayloadSize;
  FOwnerGate:=TWSClientOwnerGate.Create(Self);
end;

function TCustomWebsocketClient.GetActive: Boolean;
begin
  EnterCriticalSection(FStateLock);
  try
    Result:=FActive;
  finally
    LeaveCriticalSection(FStateLock);
  end;
end;

function TCustomWebsocketClient.GetConnection: TWebSocketClientConnection;
begin
  EnterCriticalSection(FStateLock);
  try
    Result:=FConnection;
  finally
    LeaveCriticalSection(FStateLock);
  end;
end;

function TCustomWebsocketClient.AcquireCurrentSession: TObject;
begin
  EnterCriticalSection(FStateLock);
  try
    Result:=FSession;
    if Assigned(Result) then
      TWSClientSession(Result).AddReference;
  finally
    LeaveCriticalSection(FStateLock);
  end;
end;

function TCustomWebsocketClient.AcquireHandshakeSession: TObject;
begin
  if CurrentWSHandshakeClient=Self then
    Result:=CurrentWSHandshakeSession
  else
    Result:=Nil;
  if Assigned(Result) then
    TWSClientSession(Result).AddReference
  else
    Result:=AcquireCurrentSession;
end;

function TCustomWebsocketClient.IsCurrentSession(aSession: TObject): Boolean;
begin
  EnterCriticalSection(FStateLock);
  try
    Result:=(FSession=aSession) and (not FDestroying);
  finally
    LeaveCriticalSection(FStateLock);
  end;
end;

procedure TCustomWebsocketClient.DisconnectSession(aSession: TObject;
  SendClose: Boolean; aEventSender: TObject);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Session:=TWSClientSession(aSession);
  if not Assigned(Session) then
    Exit;
  EnterCriticalSection(FStateLock);
  try
    Pump:=FMessagePump;
  finally
    LeaveCriticalSection(FStateLock);
  end;
  if SendClose and Session.IsOpen and Session.Connection.HandshakeCompleted and
     (Session.Connection.CloseState<>csClosed) then
    try
      Session.Connection.Close('');
    except
      on E : Exception do
        if Assigned(Pump) then
          Pump.ReportError(E);
    end;
  Session.NotifyDisconnected(Pump,aEventSender);
end;

function TCustomWebsocketClient.DetachCurrentSession(aSession: TObject): Boolean;
begin
  EnterCriticalSection(FStateLock);
  try
    Result:=Assigned(FSession) and
      ((aSession=Nil) or (FSession=aSession));
    if Result then
      begin
      FSession:=Nil;
      FConnection:=Nil;
      FTransport:=Nil;
      FSocket:=Nil;
      FActive:=False;
      end;
  finally
    LeaveCriticalSection(FStateLock);
  end;
end;

procedure TCustomWebsocketClient.SessionMessageReceived(aSession: TObject;
  const aMessage: TWSMessage);
var
  aHandler : TWSMessageEvent;
begin
  aHandler:=FOnMessageReceived;
  if Assigned(aHandler) then
    aHandler(Self,aMessage);
  { Do not access Self after user code. Disconnect/reconnect are supported;
    direct Free from the callback is rejected by Destroy because a Pascal
    destructor cannot safely return into this method. }
end;

procedure TCustomWebsocketClient.SessionControlReceived(aSession: TObject;
  aEventSender: TObject; aType: TFrameType; const aData: TBytes);
var
  aHandler : TWSControlEvent;
begin
  aHandler:=FOnControl;
  if Assigned(aHandler) then
    aHandler(aEventSender,aType,aData);
  { As above, callback-time disconnect/reconnect are supported, not a direct
    destruction of the callback target. }
end;

procedure TCustomWebsocketClient.ReportCallbackError(aPump: TWSMessagePump;
  E: Exception);
begin
  if Assigned(aPump) then
    aPump.ReportError(E);
end;

procedure TCustomWebsocketClient.SessionDisconnected(aSession: TObject;
  aEventSender: TObject; aPump: TWSMessagePump);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
  Handler : TNotifyEvent;
  ReleaseClientReference : Boolean;
begin
  Session:=TWSClientSession(aSession);
  Pump:=aPump;
  Session.BeginClosing;
  EnterCriticalSection(FStateLock);
  try
    if FConnectingGeneration=Session.Generation then
      FConnectingGeneration:=0;
    ReleaseClientReference:=FSession=Session;
    if ReleaseClientReference then
      begin
      FSession:=Nil;
      FConnection:=Nil;
      FTransport:=Nil;
      FSocket:=Nil;
      FActive:=False;
      if not Assigned(Pump) then
        Pump:=FMessagePump;
      end;
    Handler:=FOnDisconnect;
  finally
    LeaveCriticalSection(FStateLock);
  end;

  { Logical removal precedes terminal cancellation and the application
    notification.  Neither registry nor component-state locks cross user
    code. }
  if Assigned(Pump) then
    Pump.RemoveClient(Session.Connection);
  Session.CancelReads;
  if Assigned(Handler) then
    try
      Handler(aEventSender);
    except
      on E : Exception do
        if Assigned(Pump) then
          Pump.ReportError(E);
    end;
  if ReleaseClientReference then
    try
      Session.ReleaseReference;
    except
      on E : Exception do
        if Assigned(Pump) then
          Pump.ReportError(E);
    end;
  { Do not access Self after the handler. }
end;

procedure TCustomWebsocketClient.SessionConnected(aSession: TObject);
var
  Handler : TNotifyEvent;
  IsCurrent : Boolean;
begin
  EnterCriticalSection(FStateLock);
  try
    IsCurrent:=(FSession=aSession) and FActive and (not FDestroying);
    Handler:=FOnConnect;
  finally
    LeaveCriticalSection(FStateLock);
  end;
  if IsCurrent and Assigned(Handler) then
    Handler(Self);
  { Do not access Self after the handler. }
end;

procedure TCustomWebsocketClient.CheckInactive;
var
  IsBusy : Boolean;
begin
  EnterCriticalSection(FStateLock);
  try
    IsBusy:=FActive or Assigned(FSession) or
      (FConnectingGeneration<>0) or FDestroying;
  finally
    LeaveCriticalSection(FStateLock);
  end;
  If IsBusy then
    Raise EWebSocketClient.Create(SErrConnectionActive);
end;

Function TCustomWebsocketClient.CheckIncoming : TIncomingResult;
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    if not Session.Connection.HandshakeCompleted then
      Raise EWebSocketClient.Create(SErrHandshakeInComplete);
    try
      Result:=Session.Connection.CheckIncoming(CheckTimeout);
      if Result=irClose then
        Session.NotifyDisconnected(Pump,Session.Connection);
    except
      { Manual polling gets the same terminal lifetime transition as the
        threaded pump.  The original exception still reaches the caller. }
      on E : Exception do
        begin
        Session.NotifyDisconnected(Pump,Session.Connection);
        raise;
        end;
    end;
  finally
    ReleaseClientSession(Session,Pump);
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
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Exit;
  try
    Session.ConnectionRequestedDisconnect(Sender);
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.Connect;
var
  SSLHandler: TSSLSocketHandler;
  NewSocket : TInetSocket;
  NewTransport : TWSClientTransport;
  NewConnection : TWebSocketClientConnection;
  Session : TWSClientSession;
  Gate : TWSClientOwnerGate;
  Generation : QWord;
  Pump : TWSMessagePump;
  Published : Boolean;
  HandshakeOK : Boolean;
  FinishConnection : Boolean;
  CallbackClient : TCustomWebsocketClient;
  ConnectToken : TWSOwnerGateToken;
  PreviousHandshakeClient : TCustomWebsocketClient;
  PreviousHandshakeSession : TWSClientSession;
begin
  EnterCriticalSection(FStateLock);
  try
    if FDestroying then
      Raise EWebSocketClient.Create(SErrConnectionInActive);
    if Assigned(FSession) or FActive or (FConnectingGeneration<>0) then
      Exit;
    Inc(FNextGeneration);
    if FNextGeneration=0 then
      Inc(FNextGeneration);
    Generation:=FNextGeneration;
    FConnectingGeneration:=Generation;
    Gate:=TWSClientOwnerGate(FOwnerGate);
    Gate.AddReference; // protects the handoff into the first session lease
    if not Gate.TryEnter(CallbackClient,ConnectToken) then
      begin
      FConnectingGeneration:=0;
      Gate.ReleaseReference;
      Raise EWebSocketClient.Create(SErrConnectionInActive);
      end;
  finally
    LeaveCriticalSection(FStateLock);
  end;

  NewSocket:=Nil;
  NewTransport:=Nil;
  NewConnection:=Nil;
  Session:=Nil;
  Published:=False;
  Pump:=Nil;
  SSLHandler := nil;
  try
    Session:=TWSClientSession.Create(Gate,Generation);
    if UseSSL then
      begin
      SSLHandler := TSSLSocketHandler.GetDefaultHandler;
      SSLHandler.VerifyPeerCert := False;
      end;
    NewSocket:=TInetSocket.Create(HostName,Port,ConnectTimeout,SSLHandler);
    NewTransport:=TWSClientTransport.Create(NewSocket);
    NewSocket:=Nil; // owned by NewTransport
    NewConnection:=CreateClientConnection(NewTransport);
    if not Assigned(NewConnection) then
      Raise EWebSocketClient.Create(SErrConnectionInActive);
    NewTransport:=Nil; // owned by NewConnection
    Session.AttachConnection(NewConnection);
    NewConnection:=Nil; // owned by Session

    if OutGoingFrameMask=0 then
      OutGoingFrameMask:=1+Random(MaxInt-1);
    Session.Connection.OutgoingFrameMask:=OutGoingFrameMask;
    Session.Connection.MaxFramePayloadSize:=FMaxFramePayloadSize;
    Session.Connection.MaxMessagePayloadSize:=FMaxMessagePayloadSize;
    if UseSSL then
      TInetSocket(Session.Connection.ClientTransport.Socket).Connect;

    EnterCriticalSection(FStateLock);
    try
      if FDestroying or Assigned(FSession) or
         (FConnectingGeneration<>Generation) then
        Raise EWebSocketClient.Create(SErrConnectionActive);
      Session.AddReference; // current-client ownership
      FSession:=Session;
      FConnection:=Session.Connection;
      FTransport:=Session.Connection.ClientTransport;
      FSocket:=FTransport.Socket as TInetSocket;
      FActive:=True;
      Pump:=FMessagePump;
      Published:=True;
    finally
      LeaveCriticalSection(FStateLock);
    end;

    try
      PreviousHandshakeClient:=CurrentWSHandshakeClient;
      PreviousHandshakeSession:=CurrentWSHandshakeSession;
      CurrentWSHandshakeClient:=Self;
      CurrentWSHandshakeSession:=Session;
      try
        HandshakeOK:=DoHandShake;
      finally
        CurrentWSHandshakeSession:=PreviousHandshakeSession;
        CurrentWSHandshakeClient:=PreviousHandshakeClient;
      end;
      if (not HandshakeOK) or (not IsCurrentSession(Session)) or
         (not Session.IsOpen) then
        begin
        DisconnectSession(Session,False,Self);
        Exit;
        end;

      EnterCriticalSection(FStateLock);
      try
        FinishConnection:=(FSession=Session) and Session.IsOpen and
          (not FDestroying);
        if FinishConnection then
          Pump:=FMessagePump;
      finally
        LeaveCriticalSection(FStateLock);
      end;
      if not FinishConnection then
        begin
        DisconnectSession(Session,False,Self);
        Exit;
        end;

      if Assigned(Pump) then
        Pump.AddClient(Session.Connection);

      FinishConnection:=IsCurrentSession(Session) and Session.IsOpen;
      EnterCriticalSection(FStateLock);
      try
        FinishConnection:=FinishConnection and (FSession=Session) and
          (not FDestroying);
        if FinishConnection and (FConnectingGeneration=Generation) then
          FConnectingGeneration:=0;
      finally
        LeaveCriticalSection(FStateLock);
      end;
      if not FinishConnection then
        begin
        if Assigned(Pump) then
          Pump.RemoveClient(Session.Connection);
        DisconnectSession(Session,False,Self);
        Exit;
        end;
      Session.NotifyConnected;
    except
      DisconnectSession(Session,False,Self);
      raise;
    end;
  finally
    try
      EnterCriticalSection(FStateLock);
      try
        if FConnectingGeneration=Generation then
          FConnectingGeneration:=0;
      finally
        LeaveCriticalSection(FStateLock);
      end;
      try
        if not Published then
          begin
          try
            NewConnection.Free;
          finally
            try
              NewTransport.Free;
            finally
              NewSocket.Free;
            end;
          end;
          end;
      finally
        ReleaseClientSession(Session,Pump);
      end;
    finally
      try
        Gate.Leave(ConnectToken);
      finally
        Gate.ReleaseReference;
      end;
    end;
  end;
end;


destructor TCustomWebsocketClient.Destroy;
var
  Session : TWSClientSession;
  Gate : TWSClientOwnerGate;
  Pump : TWSMessagePump;
begin
  Session:=Nil;
  Gate:=TWSClientOwnerGate(FOwnerGate);
  if Gate.IsCurrentCallback then
    Raise EWebSocketClient.Create(SErrFreeFromCallback);
  EnterCriticalSection(FStateLock);
  try
    FDestroying:=True;
    if Assigned(FSession) then
      begin
      Session:=TWSClientSession(FSession);
      FSession:=Nil;
      end;
    FConnection:=Nil;
    FTransport:=Nil;
    FSocket:=Nil;
    FActive:=False;
    Pump:=FMessagePump;
  finally
    LeaveCriticalSection(FStateLock);
  end;
  try
    if Assigned(Session) then
      begin
      Session.BeginClosing;
      if Assigned(Pump) then
        Pump.RemoveClient(Session.Connection);
      Session.CancelReads;
      Session.NotifyDisconnected(Pump,Self);
      end;
    Gate.CloseAdmission;
    Gate.WaitForQuiescence;
    if Assigned(Session) then
      try
        Session.ReleaseReference;
      except
        on E : Exception do
          if Assigned(Pump) then
            Pump.ReportError(E);
      end;
    FreeAndNil(FHandShake);
    FreeAndNil(FHandshakeResponse);
    if Assigned(FMessagePump) then
      FMessagePump.RemoveFreeNotification(Self);
    FMessagePump:=Nil;
    Gate.ReleaseReference;
    FOwnerGate:=Nil;
  finally
    DoneCriticalSection(FStateLock);
    inherited Destroy;
  end;
end;


Function TCustomWebsocketClient.CreateHandShakeRequest : TWSHandShakeRequest;

begin
  Result:=TWSHandShakeRequest.Create('',Nil);
end;

procedure TCustomWebsocketClient.SendData(aBytes: TBytes);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    Session.Connection.Send(aBytes);
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.SendHeaders(aHeaders : TStrings);

Var
  HeaderBlock : String;
  B : TBytes;
  Session : TWSClientSession;
  Pump : TWSMessagePump;
  I : Integer;

begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireHandshakeSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    if (not Session.IsOpen) or (not IsCurrentSession(Session)) then
      Exit;
    HeaderBlock:='';
    for I:=0 to aHeaders.Count-1 do
      HeaderBlock:=HeaderBlock+aHeaders[I]+#13#10;
    HeaderBlock:=HeaderBlock+#13#10;
    B:=TEncoding.UTF8.GetAnsiBytes(HeaderBlock);
    { One write-all call keeps a websocket frame from being interleaved into
      the HTTP upgrade request. }
    Session.Connection.Transport.WriteBuffer(B);
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.SendHandShakeRequest;

Var
  aRequest : TWSHandShakeRequest;
  aHeaders : TStrings;
  Session : TWSClientSession;
  Pump : TWSMessagePump;
  CallbackClient : TCustomWebsocketClient;
  Handler : TWSClientHandshakeEvent;
  GateToken : TWSOwnerGateToken;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireHandshakeSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  aHeaders:=Nil;
  aRequest:=Nil;
  try
    if not Session.FOwnerGate.TryEnter(CallbackClient,GateToken) then
      Exit;
    try
    if (not Session.IsOpen) or
       (not CallbackClient.IsCurrentSession(Session)) then
      Exit;
    aRequest:=CallbackClient.CreateHandShakeRequest;
    if not Assigned(aRequest) then
      Raise EWebSocketClient.Create(SErrHandshakeInComplete);
    if (not Session.IsOpen) or
       (not CallbackClient.IsCurrentSession(Session)) then
      Exit;
    aRequest.Host:=HostName;
    aRequest.Port:=Port;
    aRequest.Resource:=Resource;
    aHeaders:=TStringList.Create;
    aHeaders.NameValueSeparator:=':';
    aRequest.ToStrings(aHeaders);
    Session.SetHandshakeRequest(aRequest);
    aRequest:=Nil;
    Handler:=CallbackClient.FOnSendHandshake;
    if Assigned(Handler) then
      Handler(CallbackClient,aHeaders);
    if (not Session.IsOpen) or
       (not CallbackClient.IsCurrentSession(Session)) then
      Exit;
    // Do not use FClient.WriteHeader, it messes up the strings !
    CallbackClient.SendHeaders(aHeaders);
    finally
      Session.FOwnerGate.Leave(GateToken);
    end;
  finally
    aHeaders.Free;
    aRequest.Free;
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.SendMessage(const aMessage: String);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    Session.Connection.Send(aMessage);
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

Function TCustomWebsocketClient.CreateHandshakeResponse(aHeaders : TStrings) : TWSHandShakeResponse;

begin
  Result:=TWSHandShakeResponse.Create('',aHeaders);
end;

Function TCustomWebsocketClient.CheckHandShakeResponse(aHeaders : TStrings) : Boolean;

  Function ParseStatusLine(const aLine : String;
    out aHTTPVersion : String; out aStatusCode : Integer;
    out aStatusText : String) : Boolean;
  Var
    P : Integer;
    ProtocolPart,
    Rest,
    CodePart : String;
  begin
    Result:=False;
    aHTTPVersion:='';
    aStatusCode:=0;
    aStatusText:='';
    Rest:=Trim(aLine);
    P:=Pos(' ',Rest);
    if P=0 then
      Exit;
    ProtocolPart:=Copy(Rest,1,P-1);
    if (Length(ProtocolPart)<5) or
       (CompareText(Copy(ProtocolPart,1,5),'HTTP/')<>0) then
      Exit;
    aHTTPVersion:=Copy(ProtocolPart,6,MaxInt);
    if aHTTPVersion='' then
      Exit;
    Rest:=Trim(Copy(Rest,P+1,MaxInt));
    P:=Pos(' ',Rest);
    if P=0 then
      begin
      CodePart:=Rest;
      aStatusText:='';
      end
    else
      begin
      CodePart:=Copy(Rest,1,P-1);
      aStatusText:=Trim(Copy(Rest,P+1,MaxInt));
      end;
    Result:=TryStrToInt(CodePart,aStatusCode);
  end;

  Function HasHeaderToken(const aValue,aToken : String) : Boolean;
  Var
    P : Integer;
    Remaining,
    ValuePart : String;
  begin
    Remaining:=aValue;
    repeat
      P:=Pos(',',Remaining);
      if P=0 then
        begin
        ValuePart:=Trim(Remaining);
        Remaining:='';
        end
      else
        begin
        ValuePart:=Trim(Copy(Remaining,1,P-1));
        Delete(Remaining,1,P);
        end;
      if SameText(ValuePart,aToken) then
        Exit(True);
    until Remaining='';
    Result:=False;
  end;

Var
  K : String;
  {%H-}hash : TSHA1Digest;
  B : TBytes;
  Session : TWSClientSession;
  Pump : TWSMessagePump;
  Response : TWSHandShakeResponse;
  ValidStatus : Boolean;
  HTTPVersion,
  StatusText : String;
  StatusCode : Integer;

begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireHandshakeSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  B:=[];
  Response:=Nil;
  try
    if not Assigned(Session.HandshakeRequest) then
      Raise EWebSocketClient.Create(SErrHandshakeInComplete);
    ValidStatus:=False;
    if aHeaders.Count>0 then
      ValidStatus:=ParseStatusLine(aHeaders[0],HTTPVersion,StatusCode,
        StatusText) and (StatusCode=101);
    Response:=CreateHandshakeResponse(aHeaders);
    if not Assigned(Response) then
      Raise EWebSocketClient.Create(SErrHandshakeInComplete);
    Response.HTTPVersion:=HTTPVersion;
    Response.StatusCode:=StatusCode;
    Response.StatusText:=StatusText;
    k := Trim(Session.HandshakeRequest.Key) + SSecWebSocketGUID;
    hash:=SHA1String(k);
    SetLength(B,SizeOf(hash));
    Move(hash[0],B[0],SizeOf(hash));
    k:=EncodeBytesBase64(B);
    { Sec-WebSocket-Accept is base64 and therefore case-sensitive. }
    Result:=(K=Response.Accept)
            and SameText(Response.Upgrade,'websocket')
            and HasHeaderToken(Response.Connection,'Upgrade')
            and ValidStatus;
    Session.SetHandshakeResponse(Response);
    Response:=Nil;
  finally
    Response.Free;
    ReleaseClientSession(Session,Pump);
  end;
end;

Function TCustomWebsocketClient.ReadHandShakeResponse : Boolean;

  Function ParseResponseStatus(const aLine : String;
    out aHTTPVersion : String; out aStatusCode : Integer;
    out aStatusText : String) : Boolean;
  Var
    P : Integer;
    ProtocolPart,
    Rest,
    CodePart : String;
  begin
    Result:=False;
    aHTTPVersion:='';
    aStatusCode:=0;
    aStatusText:='';
    Rest:=Trim(aLine);
    P:=Pos(' ',Rest);
    if P=0 then
      Exit;
    ProtocolPart:=Copy(Rest,1,P-1);
    if (Length(ProtocolPart)<6) or
       (CompareText(Copy(ProtocolPart,1,5),'HTTP/')<>0) then
      Exit;
    aHTTPVersion:=Copy(ProtocolPart,6,MaxInt);
    Rest:=Trim(Copy(Rest,P+1,MaxInt));
    P:=Pos(' ',Rest);
    if P=0 then
      CodePart:=Rest
    else
      begin
      CodePart:=Copy(Rest,1,P-1);
      aStatusText:=Trim(Copy(Rest,P+1,MaxInt));
      end;
    Result:=(aHTTPVersion<>'') and TryStrToInt(CodePart,aStatusCode);
  end;

Var
  S : String;
  aHeaders : TStrings;
  aResponse : TWSHandShakeResponse;
  ResponseToTransfer : TWSHandShakeResponse;
  aHandler : TWSClientHandshakeResponseEvent;
  Session : TWSClientSession;
  Pump : TWSMessagePump;
  CallbackClient : TCustomWebsocketClient;
  GateToken : TWSOwnerGateToken;
  HeaderBytes : SizeInt;
  HeaderLines : Integer;
  LineBytes : SizeInt;
  HTTPVersion,
  StatusText : String;
  StatusCode : Integer;
  ValidStatus : Boolean;

begin
  Result:=False;
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireHandshakeSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  aHeaders:=TStringList.Create;
  ResponseToTransfer:=Nil;
  HeaderBytes:=0;
  HeaderLines:=0;
  HTTPVersion:='';
  StatusText:='';
  StatusCode:=0;
  Try
    if not Session.FOwnerGate.TryEnter(CallbackClient,GateToken) then
      Exit;
    try
    if (not Session.IsOpen) or
       (not CallbackClient.IsCurrentSession(Session)) then
      Exit;
    aHeaders.NameValueSeparator:=':';
    Repeat
      S:=Session.Connection.Transport.ReadLn;
      LineBytes:=Length(S);
      if LineBytes>WSMaxHandshakeHeaderLineBytes then
        Raise EWSHandShake.Create(SErrHandshakeHeaderLineTooLong);
      Inc(HeaderLines);
      if (HeaderLines>WSMaxHandshakeHeaderLines) or
         (LineBytes>WSMaxHandshakeHeaderBytes-HeaderBytes-2) then
        Raise EWSHandShake.Create(SErrHandshakeHeadersTooLarge);
      Inc(HeaderBytes,LineBytes+2);
      aHeaders.Add(S);
    Until (S='');
    ValidStatus:=(aHeaders.Count>0) and
      ParseResponseStatus(aHeaders[0],HTTPVersion,StatusCode,StatusText) and
      (StatusCode=101);
    Result:=ValidStatus and CallbackClient.CheckHandShakeResponse(aHeaders);
    if (not Session.IsOpen) or
       (not CallbackClient.IsCurrentSession(Session)) then
      begin
      Result:=False;
      Exit;
      end;
    if Result then
      begin
      ResponseToTransfer:=Session.TakeHandshakeResponse;
      if not Assigned(ResponseToTransfer) then
        ResponseToTransfer:=CallbackClient.CreateHandshakeResponse(aHeaders);
      if (not Assigned(ResponseToTransfer)) or (not Session.IsOpen) or
         (not CallbackClient.IsCurrentSession(Session)) then
        begin
        Result:=False;
        Exit;
        end;
      { Even an override which implements its own header checks exposes the
        status line actually received, rather than constructor defaults. }
      ResponseToTransfer.HTTPVersion:=HTTPVersion;
      ResponseToTransfer.StatusCode:=StatusCode;
      ResponseToTransfer.StatusText:=StatusText;
      { Keep the original public non-owning HandshakeResponse contract.
        The generation session owns this response for exactly as long as the
        connection may expose it. }
      Session.SetHandshakeResponse(ResponseToTransfer);
      ResponseToTransfer:=Nil;
      aResponse:=Session.FHandshakeResponse;
      Session.Connection.HandshakeResponse:=aResponse;
      aHandler:=CallbackClient.FOnHandshakeResponse;
      if Assigned(aHandler) then
        aHandler(CallbackClient,aResponse,Result);
      end;
    finally
      Session.FOwnerGate.Leave(GateToken);
    end;
  Finally
    ResponseToTransfer.Free;
    aHeaders.Free;
    ReleaseClientSession(Session,Pump);
  End;
end;

Function TCustomWebsocketClient.DoHandShake : Boolean;
var
  Session : TWSClientSession;
  PreviousSession : TWSClientSession;
  PreviousClient : TCustomWebsocketClient;
  Pump : TWSMessagePump;
  CallbackClient : TCustomWebsocketClient;
  GateToken : TWSOwnerGateToken;
begin
  Result:=False;
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireHandshakeSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    if not Session.FOwnerGate.TryEnter(CallbackClient,GateToken) then
      Exit;
    try
      PreviousSession:=CurrentWSHandshakeSession;
      PreviousClient:=CurrentWSHandshakeClient;
      CurrentWSHandshakeClient:=CallbackClient;
      CurrentWSHandshakeSession:=Session;
      try
        if (not Session.IsOpen) or
           (not CallbackClient.IsCurrentSession(Session)) then
          Exit;
        CallbackClient.SendHandShakeRequest;
        if (not Session.IsOpen) or
           (not CallbackClient.IsCurrentSession(Session)) then
          Exit;
        Result:=CallbackClient.ReadHandShakeResponse;
        Result:=Result and Session.IsOpen and
          CallbackClient.IsCurrentSession(Session);
      finally
        CurrentWSHandshakeSession:=PreviousSession;
        CurrentWSHandshakeClient:=PreviousClient;
      end;
    finally
      Session.FOwnerGate.Leave(GateToken);
    end;
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.Loaded;
begin
  inherited;
  if FLoadActive then
    Connect;
end;

procedure TCustomWebsocketClient.Notification(aComponent : TComponent;
  Operation : TOperation);
begin
  inherited Notification(aComponent,Operation);
  if Operation=opRemove then
    begin
    EnterCriticalSection(FStateLock);
    try
      if aComponent=FMessagePump then
        FMessagePump:=Nil;
    finally
      LeaveCriticalSection(FStateLock);
    end;
    end;
end;

procedure TCustomWebsocketClient.MessageReceived(Sender: TObject; const aMessage : TWSMessage) ;
begin
  if Assigned(OnMessageReceived) and (TWSClientConnection(Sender).HandshakeCompleted) then
    OnMessageReceived(Self, AMessage);
end;

procedure TCustomWebsocketClient.Ping(aMessage: UTF8String);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    Session.Connection.Send(ftPing,TEncoding.UTF8.GetAnsiBytes(aMessage));
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.Pong(aMessage: UTF8String);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create(SErrConnectionInActive);
  try
    Session.Connection.Send(ftPong,TEncoding.UTF8.GetAnsiBytes(aMessage));
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.FreeConnectionObjects;
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Exit;
  try
    Disconnect(False);
  finally
    ReleaseClientSession(Session,Pump);
  end;
end;

procedure TCustomWebsocketClient.Disconnect(SendClose : boolean = true);
var
  Session : TWSClientSession;
  Pump : TWSMessagePump;
begin
  Pump:=MessagePump;
  Session:=TWSClientSession(AcquireCurrentSession);
  if not Assigned(Session) then
    Exit;
  try
    { SessionDisconnected transfers/releases the client's ownership.  This
      local reference keeps the session alive through the whole operation. }
    DisconnectSession(Session,SendClose,Self);
  finally
    ReleaseClientSession(Session,Pump);
  end;
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
  if Active or TWSClientOwnerGate(FOwnerGate).HasCallbacks then
    Raise EWebSocketClient.Create(SErrConnectionActive);
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

procedure TCustomWebsocketClient.SetMaxFramePayloadSize(const Value: QWord);
begin
  CheckInactive;
  FMaxFramePayloadSize:=Value;
end;

procedure TCustomWebsocketClient.SetMaxMessagePayloadSize(const Value: QWord);
begin
  CheckInactive;
  FMaxMessagePayloadSize:=Value;
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
var
  Session : TWSClientSession;
  Entry : TWSMessagePumpEntry;
  Entries : TList;
  I : Integer;
begin
  { A raw TWSClientConnection has no reference/lifetime protocol. Accepting
    one here would let its external owner free it while CheckIncoming runs.
    Preserve the public entry point, but fail before registration unless the
    connection carries the managed session used by this client unit. }
  if not (aConnection is TWebSocketClientConnection) then
    Raise EWebSocketClient.Create('The message pump requires a client session connection');
  Session:=TWSClientSession(TWebSocketClientConnection(aConnection).FClientSession);
  if not Assigned(Session) then
    Raise EWebSocketClient.Create('The websocket connection has no client session');

  Entry:=Nil;
  EnterCriticalSection(FRegistryLock);
  try
    Entries:=FEntries.LockList;
    try
      for I:=0 to Entries.Count-1 do
        if TWSMessagePumpEntry(Entries[I]).Connection=aConnection then
          Exit;
      Entry:=TWSMessagePumpEntry.Create(Session,aConnection);
      Entries.Add(Entry);
      Entry.AddReference; // registry lease; initial reference stays local
    finally
      FEntries.UnlockList;
    end;
    try
      FList.Add(aConnection); // protected compatibility mirror
    except
      Entries:=FEntries.LockList;
      try
        Entries.Remove(Entry);
        Entry.MarkRemoved;
      finally
        FEntries.UnlockList;
      end;
      try
        Entry.ReleaseReference; // registry lease
      except
        on Exception do
          ; // preserve the original AddClient failure
      end;
      try
        Entry.ReleaseReference; // local lease
      except
        on Exception do
          ; // preserve the original AddClient failure
      end;
      Entry:=Nil;
      raise;
    end;
  finally
    LeaveCriticalSection(FRegistryLock);
  end;
  try
    { The default thread pump schedules this entry through its virtual
      CheckConnections/ReadConnections pipeline.  That retains the protected
      extension hook without introducing a second socket reader. }
    if (not Entry.IsRegistered) or (not Session.IsOpen) then
      RemoveEntry(Entry);
  finally
    try
      Entry.ReleaseReference; // local lease
    except
      on E : Exception do
        ReportError(E);
    end;
  end;
end;

procedure TWSMessagePump.RemoveClient(aConnection: TWSClientConnection);
var
  Entry : TWSMessagePumpEntry;
begin
  Entry:=TWSMessagePumpEntry(FindEntry(aConnection));
  if not Assigned(Entry) then
    begin
    FList.Remove(aConnection);
    Exit;
    end;
  try
    RemoveEntry(Entry);
  finally
    try
      Entry.ReleaseReference;
    except
      on E : Exception do
        ReportError(E);
    end;
  end;
end;

function TWSMessagePump.FindEntry(aConnection: TWSClientConnection): TObject;
var
  Entries : TList;
  Entry : TWSMessagePumpEntry;
  I : Integer;
begin
  Result:=Nil;
  EnterCriticalSection(FRegistryLock);
  try
    Entries:=FEntries.LockList;
    try
      for I:=0 to Entries.Count-1 do
        begin
        Entry:=TWSMessagePumpEntry(Entries[I]);
        if Entry.Connection=aConnection then
          begin
          Entry.AddReference;
          Result:=Entry;
          Exit;
          end;
        end;
    finally
      FEntries.UnlockList;
    end;
  finally
    LeaveCriticalSection(FRegistryLock);
  end;
end;

procedure TWSMessagePump.RemoveEntry(aEntry: TObject);
var
  Entry : TWSMessagePumpEntry;
  Entries : TList;
  RemovedRegistryReference : Boolean;
begin
  Entry:=TWSMessagePumpEntry(aEntry);
  RemovedRegistryReference:=False;
  EnterCriticalSection(FRegistryLock);
  try
    Entries:=FEntries.LockList;
    try
      if Entry.MarkRemoved then
        begin
        if Entries.Remove(Entry)>=0 then
          RemovedRegistryReference:=True;
        end;
    finally
      FEntries.UnlockList;
    end;
    FList.Remove(Entry.Connection);
  finally
    LeaveCriticalSection(FRegistryLock);
  end;
  { This is terminal cancellation for this exact session.  It remains set
    between exact-read chunks and therefore has no interrupt gap. }
  Entry.CancelReads;
  if RemovedRegistryReference then
    try
      Entry.ReleaseReference;
    except
      on E : Exception do
        ReportError(E);
    end;
end;

procedure TWSMessagePump.StartEntry(aEntry: TObject;
  aRunGeneration: LongInt);
var
  Entry : TWSMessagePumpEntry;
  Reader : TWSClientReaderThread;
begin
  if not (Self is TWSThreadMessagePump) then
    Exit;
  if not TWSPumpCore(FCore).IsRunning(aRunGeneration) then
    Exit;
  Entry:=TWSMessagePumpEntry(aEntry);
  if not Entry.TryStartWorker then
    Exit;
  Reader:=Nil;
  try
    Reader:=TWSClientReaderThread.Create(TWSPumpCore(FCore),Entry,
      aRunGeneration);
    Reader.Start;
  except
    if Assigned(Reader) then
      begin
      Reader.FreeOnTerminate:=False;
      Reader.AbandonBeforeStart;
      Reader.Free;
      end
    else
      Entry.WorkerStopped;
    raise;
  end;
end;

procedure TWSMessagePump.EntryEnded(aEntry: TObject; aError: Exception);
var
  Entry : TWSMessagePumpEntry;
begin
  Entry:=TWSMessagePumpEntry(aEntry);
  Entry.Session.BeginClosing;
  RemoveEntry(Entry);
  if Assigned(aError) then
    ReportError(aError);
  { Notification is the tail operation: it may re-enter or attempt to free
    its pump. }
  Entry.Session.NotifyDisconnected(Self,Entry.Connection);
end;

procedure TWSMessagePump.ReportError(aError: Exception);
var
  Handler : TWSErrorEvent;
begin
  Handler:=FOnError;
  if Assigned(Handler) then
    try
      Handler(Self,aError);
    except
      { An error reporter must not terminate a reader or skip its releases. }
    end;
end;

procedure TWSMessagePump.ClearEntries;
var
  Entries : TList;
  LocalEntries : TList;
  Entry : TWSMessagePumpEntry;
  I : Integer;
begin
  LocalEntries:=TList.Create;
  try
    EnterCriticalSection(FRegistryLock);
    try
      Entries:=FEntries.LockList;
      try
        for I:=0 to Entries.Count-1 do
          begin
          Entry:=TWSMessagePumpEntry(Entries[I]);
          Entry.MarkRemoved;
          LocalEntries.Add(Entry);
          end;
        Entries.Clear;
      finally
        FEntries.UnlockList;
      end;
      FList.Clear;
    finally
      LeaveCriticalSection(FRegistryLock);
    end;
    for I:=0 to LocalEntries.Count-1 do
      begin
      Entry:=TWSMessagePumpEntry(LocalEntries[I]);
      try
        Entry.ReleaseReference;
      except
        on E : Exception do
          ReportError(E);
      end;
      end;
  finally
    LocalEntries.Free;
  end;
end;

procedure TWSMessagePump.SetInterval(AValue: Integer);
begin
  if FInterval=AValue then Exit;
  FInterval:=AValue;
  TWSPumpCore(FCore).SetInterval(aValue);
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
  Entries : TList;

begin
  Entries:=FEntries.LockList;
  try
    Result:=Entries.Count<>0;
  finally
    FEntries.UnlockList;
  end;
  if not Result then
    TThread.Sleep(FInterval);
end;

constructor TWSMessagePump.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FRegistryLock);
  FEntries:=TThreadList.Create;
  FList:=TThreadList.Create;
  FReads:=[];
  FExceptions:=[];
  Finterval:=25;
  FCore:=TWSPumpCore.Create(Self,FInterval);
end;

destructor TWSMessagePump.Destroy;
begin
  TWSPumpCore(FCore).RequestStop;
  TWSPumpCore(FCore).WaitWorkers(-1);
  ClearEntries;
  TWSPumpCore(FCore).CloseOwner;
  TWSPumpCore(FCore).WaitOwnerUsers;
  TWSPumpCore(FCore).ReleaseReference;
  FCore:=Nil;
  FreeAndNil(FEntries);
  FreeAndNil(FList);
  DoneCriticalSection(FRegistryLock);
  inherited;
end;

procedure TWSMessagePump.InterruptConnections;
Var
  Entries : TList;
  Snapshot : TList;
  Entry : TWSMessagePumpEntry;
  I : Integer;

begin
  Snapshot:=TList.Create;
  try
    Entries:=FEntries.LockList;
    try
      for I:=0 to Entries.Count-1 do
        begin
        Entry:=TWSMessagePumpEntry(Entries[I]);
        Entry.AddReference;
        Snapshot.Add(Entry);
        end;
    finally
      FEntries.UnlockList;
    end;
    for I:=0 to Snapshot.Count-1 do
      begin
      Entry:=TWSMessagePumpEntry(Snapshot[I]);
      try
        try
          if Entry.WorkerRunning then
            Entry.InterruptRead;
        except
          on E : Exception do
            ReportError(E);
        end;
      finally
        try
          Entry.ReleaseReference;
        except
          on E : Exception do
            ReportError(E);
        end;
      end;
      end;
  finally
    Snapshot.Free;
  end;
end;

procedure TWSMessagePump.ReadConnections;
Var
  Entries : TList;
  Snapshot : TList;
  Entry : TWSMessagePumpEntry;
  IncomingResult: TIncomingResult;
  RunGeneration : LongInt;
  I : Integer;

begin
  if Self is TWSThreadMessagePump then
    begin
    { Persistent per-session readers are the sole consumers in the default
      threaded pump.  The legacy protected pipeline schedules them here. }
    Snapshot:=TList.Create;
    try
      Entries:=FEntries.LockList;
      try
        for I:=0 to Entries.Count-1 do
          begin
          Entry:=TWSMessagePumpEntry(Entries[I]);
          Entry.AddReference;
          Snapshot.Add(Entry);
          end;
      finally
        FEntries.UnlockList;
      end;
      RunGeneration:=TWSPumpCore(FCore).CurrentGeneration;
      for I:=0 to Snapshot.Count-1 do
        begin
        Entry:=TWSMessagePumpEntry(Snapshot[I]);
        try
          try
            StartEntry(Entry,RunGeneration);
          except
            on E : Exception do
              EntryEnded(Entry,E);
          end;
        finally
          try
            Entry.ReleaseReference;
          except
            on E : Exception do
              ReportError(E);
          end;
        end;
        end;
    finally
      Snapshot.Free;
    end;
    Exit;
    end;

  Snapshot:=TList.Create;
  try
    Entries:=FEntries.LockList;
    try
      for I:=0 to Entries.Count-1 do
        begin
        Entry:=TWSMessagePumpEntry(Entries[I]);
        Entry.AddReference;
        Snapshot.Add(Entry);
        end;
    finally
      FEntries.UnlockList;
    end;
    for I:=0 to Snapshot.Count-1 do
      begin
      Entry:=TWSMessagePumpEntry(Snapshot[I]);
      try
        if Entry.IsRegistered and Entry.Session.IsOpen then
          try
            IncomingResult:=Entry.Connection.CheckIncoming(0);
            if IncomingResult=irClose then
              EntryEnded(Entry,Nil);
          except
            on E : EWSReadInterrupted do
              EntryEnded(Entry,Nil);
            on E : Exception do
              EntryEnded(Entry,E);
          end;
      finally
        try
          Entry.ReleaseReference;
        except
          on E : Exception do
            ReportError(E);
        end;
      end;
      end;
  finally
    Snapshot.Free;
  end;
end;


{ TWSThreadMessagePump }

constructor TWSThreadMessagePump.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FLifecycleLock);
end;

procedure TWSThreadMessagePump.Execute;
var
  RunGeneration : LongInt;
  DriverThread : TMessageDriverThread;
begin
  { Reap only a stopped generation.  A harmless second Execute must never
    interrupt the readers of the generation which is already running. }
  RunGeneration:=TWSPumpCore(FCore).CurrentGeneration;
  if not TWSPumpCore(FCore).IsRunning(RunGeneration) then
    TryFinalize(0,True);
  EnterCriticalSection(FLifecycleLock);
  try
    if Assigned(FThread) then
      Exit;
    if not TWSPumpCore(FCore).BeginRun(RunGeneration) then
      Exit;
    DriverThread:=TMessageDriverThread.Create(Self,Nil);
    DriverThread.FRunGeneration:=RunGeneration;
    FThread:=DriverThread;
    try
      DriverThread.Start;
    except
      FThread:=Nil;
      DriverThread.Free;
      TWSPumpCore(FCore).RequestStop;
      raise;
    end;
  finally
    LeaveCriticalSection(FLifecycleLock);
  end;
end;

destructor TWSThreadMessagePump.Destroy;
begin
  if CurrentWSPumpCore=TWSPumpCore(FCore) then
    Raise EWebSocketClient.Create('A websocket message pump cannot be freed from its own callback');
  RequestStop;
  while not TryFinalize(100,True) do
    ;
  DoneCriticalSection(FLifecycleLock);
  inherited Destroy;
end;

procedure TWSThreadMessagePump.RequestStop;
var
  DriverThread : TThread;
begin
  TWSPumpCore(FCore).RequestStop;
  EnterCriticalSection(FLifecycleLock);
  try
    DriverThread:=FThread;
    if Assigned(DriverThread) then
      DriverThread.Terminate;
  finally
    LeaveCriticalSection(FLifecycleLock);
  end;
end;

function TWSThreadMessagePump.TryFinalize(aTimeoutMs: Integer;
  aInterruptReaders: Boolean): Boolean;
var
  Started : QWord;
  DriverThread : TThread;
  DriverFinished : Boolean;
  TimedOut : Boolean;
begin
  Result:=False;
  if CurrentWSPumpCore=TWSPumpCore(FCore) then
    Exit;
  Started:=TThread.GetTickCount64;
  repeat
    if aInterruptReaders then
      InterruptConnections;
    EnterCriticalSection(FLifecycleLock);
    try
      DriverThread:=FThread;
      DriverFinished:=(not Assigned(DriverThread)) or DriverThread.Finished;
    finally
      LeaveCriticalSection(FLifecycleLock);
    end;
    Result:=(TWSPumpCore(FCore).WorkerCount=0) and
      DriverFinished;
    if Result then
      Break;
    TimedOut:=(aTimeoutMs>=0) and
      ((TThread.GetTickCount64-Started)>=QWord(aTimeoutMs));
    if TimedOut then
      Exit(False);
    if TThread.CurrentThread.ThreadID=MainThreadID then
      CheckSynchronize(0);
    TThread.Sleep(1);
  until False;

  EnterCriticalSection(FLifecycleLock);
  try
    DriverThread:=FThread;
    if Assigned(DriverThread) and DriverThread.Finished then
      begin
      DriverThread.WaitFor;
      FThread:=Nil;
      DriverThread.Free;
      end;
  finally
    LeaveCriticalSection(FLifecycleLock);
  end;
end;

procedure TWSThreadMessagePump.Terminate;
Const
  MinStopGraceMs = 100;
  MaxStopGraceMs = 1000;
Var
  GraceMs : QWord;
begin
  RequestStop;
  { A callback running on one of this pump's reader threads may request a
    stop, but it must not wait for itself. }
  if CurrentWSPumpCore=TWSPumpCore(FCore) then
    Exit;

  { Preserve the previous ability to stop and restart a healthy pump without
    disconnecting its clients. Normally the bounded polling loop exits within
    two intervals. Interrupt sockets only when that graceful stop fails. }
  if Interval>0 then
    GraceMs:=QWord(Interval)*2+10
  else
    GraceMs:=MinStopGraceMs;
  if GraceMs<MinStopGraceMs then
    GraceMs:=MinStopGraceMs
  else if GraceMs>MaxStopGraceMs then
    GraceMs:=MaxStopGraceMs;

  { Bounded finalization is essential when this call is itself executing in
    a method which the reader synchronized to the calling thread.  On timeout
    the thread objects and core are retained; a later Terminate, Execute or
    destructor reaps them after the callback unwinds. }
  if TryFinalize(Integer(GraceMs),False) then
    Exit;
  { A worker which did not leave within the normal read timeout is inside an
    exact read.  Targeted, repeated interruption now forces only those
    readers out; this second wait remains bounded for synchronized callers. }
  TryFinalize(Integer(GraceMs),True);
end;

{ TWSThreadMessagePump.TMessageDriverThread }

constructor TWSThreadMessagePump.TMessageDriverThread.Create(
  aPump: TWSThreadMessagePump; aTerminate: TNotifyEvent);

begin
  FPump:=aPump;
  OnTerminate:=aTerminate;
  Inherited Create(True);
  FreeOnTerminate:=False;
end;

procedure TWSThreadMessagePump.TMessageDriverThread.Execute;
var
  PreviousCore : TWSPumpCore;
begin
  PreviousCore:=CurrentWSPumpCore;
  CurrentWSPumpCore:=TWSPumpCore(FPump.FCore);
  try
    while (not Terminated) and
          TWSPumpCore(FPump.FCore).IsRunning(FRunGeneration) do
      begin
      try
        if FPump.CheckConnections then
          FPump.ReadConnections;
      except
        on E : Exception do
          FPump.ReportError(E);
      end;
      if (not Terminated) and
         TWSPumpCore(FPump.FCore).IsRunning(FRunGeneration) then
        if FPump.Interval>0 then
          TThread.Sleep(FPump.Interval)
        else
          TThread.Sleep(1);
      end;
  finally
    CurrentWSPumpCore:=PreviousCore;
  end;
end;

end.
