{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 - by the Free Pascal development team

    Abstract websocket server implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpcustwsserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ssockets, fpthreadpool, fpwebsocket;

Const
  DefaultAcceptTimeout = 50;
  DefaultWaitTime = 0;


Type
  TCustomWSServer = Class;
  EWebsocketServer = Class(EWebsocket);

  TWSSendToFilter = Procedure (AConnection: TWSServerConnection; var aAllow : Boolean) of object;
  TWSAllowConnectionEvent = procedure(Sender: TObject; AConnection: TWSServerConnection; Var aAllow : Boolean) of object;
  TWSConnectEvent = procedure(Sender: TObject; AConnection: TWSServerConnection) of object;
  TWSGetSocketHandlerEvent = Procedure (Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler) of object;
  TWSSocketHandlerCreatedEvent = Procedure (Sender : TObject; AHandler : TSocketHandler) of object;
  TConnectionIterator = Procedure (aConnection : TWSServerConnection; var aContinue : boolean) of object;
  TWSErrorEvent = Procedure(Sender : TObject; aConnection : TWSServerConnection; aError : Exception) of object;

  { TWSConnectionList }

  TWSConnectionList = Class(TThreadList)
    Function ForEach(aIterator : TConnectionIterator) : Boolean;
    Function FindConnectionById(aID : String) : TWSConnection;
  end;


  TWSThreadMode = (wtmNone,wtmThread,wtmThreadPool);

  { TWSServerConnectionHandler }

  TWSServerConnectionHandler = Class(TObject)
  Private
    FServer : TCustomWSServer;
    FWaitTime: Integer;
    function GetList: TWSConnectionList;
  Protected
    Type
      TErrorHandler = Procedure (aConnection : TWSServerConnection; E : Exception) of object;
    // Default handler for iterator which checks & reads 1 incoming message
    procedure DoCheckConnectionRequests(aConnection: TWSServerConnection; var aContinue: boolean); virtual;
    // Will pass on to the server
    Procedure RemoveConnection(aConnection : TWSServerConnection); virtual;
    // Will pass on exception and connection to the server
    Procedure HandleError(aConnection : TWSServerConnection; E : Exception);
  Public
    Constructor Create(aServer : TCustomWSServer); virtual;
    Destructor Destroy; override;
    // loop over all connections, calling aIterator
    Procedure Foreach(aIterator : TConnectionIterator);
    // Close all connections
    Procedure CloseConnections; virtual;
    // Check for requests, and handle them.
    Procedure CheckIncomingMessages; virtual;
    // New connection
    Procedure HandleConnection(aConnection : TWSServerConnection; DoHandshake : Boolean); virtual; abstract;
    // Our server
    Property Server : TCustomWSServer Read FServer;
    // Shortcut to server list of connections
    Property List : TWSConnectionList Read GetList;
    // Time (in milliseconds) to wait for incoming connection requests
    Property WaitTime : Integer Read FWaitTime Write FWaitTime;
  end;


  { TWSSimpleConnectionHandler }

  TWSSimpleConnectionHandler = Class(TWSServerConnectionHandler)
  private
  Public
    Procedure HandleConnection(aConnection : TWSServerConnection; DoHandshake : Boolean); override;
  end;


  { TWSThreadedConnectionHandler }

  TWSThreadedConnectionHandler = Class(TWSServerConnectionHandler)
  private
    procedure ConnectionDone(Sender: TObject);
  public
    Type
      { TWSConnectionThread }
      TWSConnectionThread = Class(TThread)
      private
        FConnection: TWSServerConnection;
        FOnDone : TNotifyEvent;
        FDoHandshake : Boolean;
      Public
        Constructor CreateConnection(AConnection : TWSServerConnection; aOnConnectionDone : TNotifyEvent; DoHandShake : Boolean); virtual;
        Procedure Execute; override;
        Property Connection : TWSServerConnection Read FConnection;
      end;
  Public
    procedure CheckIncomingMessages; override;
    Procedure HandleConnection(aConnection : TWSServerConnection; DoHandshake : Boolean); override;
  end;

  { TWSPooledConnectionHandler }

  TWSPooledConnectionHandler = Class(TWSServerConnectionHandler)
  Private
    FPool : TFPCustomSimpleThreadPool;
    FBusy : TThreadList;
  Protected
    Type
       { THandleRequestTask }
       THandleRequestTask = Class(TThreadPoolTask)
       private
         FConnection: TWSServerConnection;
         FDoHandshake : Boolean;
       Protected
         procedure DoExecute; override;
       Public
         Constructor Create(aConnection : TWSServerConnection; aOnConnectionDone : TNotifyEvent; aOnError : TErrorHandler; aDoHandshake : Boolean);
         Property Connection : TWSServerConnection Read FConnection;
       end;
    function IsBusy(aConnection: TWSServerConnection): Boolean;
    procedure ConnectionDone(Sender: TObject); virtual;
    procedure ScheduleRequest(aConnection: TWSServerConnection; DoHandShake : Boolean);virtual;
    procedure CheckRequest(aConnection: TWSServerConnection; var aContinue : Boolean);virtual;
  Public
    Procedure CloseConnections; override;
    procedure CheckIncomingMessages; override;
    Constructor Create(aServer : TCustomWSServer); override;
    Procedure HandleConnection(aConnection : TWSServerConnection; DoHandshake : Boolean); override;
    function CreatePool : TFPCustomSimpleThreadPool;
    Property Pool : TFPCustomSimpleThreadPool Read FPool;
  end;

  { TCustomWSServer }

  TCustomWSServer = class(TComponent)
  private
    FConnections: TWSConnectionList;
    FMessageWaitTime: Cardinal;
    FOnConnect: TWSConnectEvent;
    FOnError: TWSErrorEvent;
    FOnMessageReceived: TWSMessageEvent;
    FOnControl : TWSControlEvent;
    FOnDisconnect: TNotifyEvent;
    FOptions: TWSOptions;
    FOutgoingFrameMask: Integer;
    FWebSocketVersion: Integer;
    FOnAllow: TWSAllowConnectionEvent;
    FResource: string;
    FConnectionHandler : TWSServerConnectionHandler;
    FThreadMode: TWSThreadmode;
    FOnConnectionHandshake: TWSConnectionHandshakeEvent;
    function GetActiveConnectionCount: Integer;
    procedure SetOptions(const Value: TWSOptions);
    procedure SetResource(AValue: string);
  protected
    // Virtual so it can be overriden;
    procedure SetThreadMode(AValue: TWSThreadMode); virtual;
    // Called when a connection is disconnected
    Procedure DoDisconnect(Sender : TObject);  virtual;
    // Free connection handler
    Procedure FreeConnectionHandler; virtual;
    // Create connection handler if it is not set yet
    procedure StartConnectionHandler; virtual;
    // Handle error
    procedure HandleError(aConnection : TWSServerConnection; E : Exception); virtual;
    // Disconnect connection when asked, remove from list, free connection
    procedure RemoveConnection(AConnection: TWSServerConnection; aDoDisconnect: Boolean); virtual;
    // Close connection socket
    Procedure CloseConnectionSocket(aConnection :TWSServerConnection; var aContinue : boolean); virtual;
    Procedure CheckInactive;
    // Must be implemented by descendents
    procedure SetActive(const Value: Boolean); virtual;
    function GetActive: Boolean; virtual;
    Function CreateConnectionHandler : TWSServerConnectionHandler; virtual;
    // Allow-all TWSSendToFilter
    procedure DoAllowAll(aConnection: TWSServerConnection; var aAllow: Boolean);
    // Create connection list
    function CreateConnections: TWSConnectionList; virtual;
    // Create TWSServerConnection based on incoming socket stream
    function CreateWebsocketConnection(aStream : TSocketStream; aOptions: TWSOptions): TWSServerConnection; virtual;
    // Allow the connection ?
    function AllowConnection(AConnection: TWSServerConnection) : Boolean; virtual;
    // Event handlers for messages : Call OnMessageReceived
    procedure DoMessageReceived(Sender: TObject; const aMessage : TWSMessage); virtual;
    // Event handlers for control frames : Call OnControlReceived
    Procedure DoControlReceived(Sender: TObject; aType : TFrameType; const aData: TBytes);virtual;
    // Wait for connections to close
    function WaitForConnections(aMaxAttempts : Integer = 10) : Boolean; virtual;
    // Active or not ?
    Property Active : Boolean Read GetActive Write SetActive;
    // Read only access to connection handler
    property ConnectionHandler : TWSServerConnectionHandler Read FConnectionHandler;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    // Broadcast text data to all connections
    procedure BroadcastFrame(aFrame : TWSFrame); virtual;
    // Broadcast text data to all connections
    procedure BroadcastMessage(AMessage: string); virtual;
    // Broadcast binary data to all connections
    procedure BroadcastData(AData: TBytes); virtual;
    // Send frame to connections, calling aSelector to see whether frame must be sent to a particular connection
    procedure SendFrameTo(aFrame : TWSFrame; aSelector : TWSSendToFilter); virtual;
    // Send message to connections, calling aSelector to see whether data must be sent to a particular connection
    procedure SendMessageTo(AMessage: string; aSelector : TWSSendToFilter); virtual;
    // Send binary data to connections, calling aSelector to see whether data must be sent to a particular connection
    procedure SendDataTo(AData: TBytes; aSelector : TWSSendToFilter); virtual;
    // Do something for all connections. Stop when iterator indicates to stop
    Procedure Foreach(aIterator : TConnectionIterator);
    // Connection list
    property Connections: TWSConnectionList read FConnections;
    // Count of connections
    property ConnectionCount : Integer Read GetActiveConnectionCount;
  protected
    // Websocket version to use
    Property WebSocketVersion : Integer Read FWebSocketVersion Write FWebSocketVersion Default DefaultWebSocketVersion;
    // Wait time when checking for new messages
    Property MessageWaitTime : Cardinal Read FMessageWaitTime Write FMessageWaitTime;
    // Options regarding WebSocket Protocol
    Property Options : TWSOptions Read FOptions Write SetOptions;
    // Resource: when set, the request must match this.
    Property Resource : string Read FResource Write SetResource;
    // Thread mode
    Property ThreadMode : TWSThreadMode Read FThreadMode write SetThreadMode;
    // Called when a new connection is made.
    property OnConnect: TWSConnectEvent read FOnConnect write FOnConnect;
    // Called when handshake was received, use this to disallow connection
    Property OnAllow : TWSAllowConnectionEvent Read FOnAllow Write FOnAllow;
    // Called when a text message is received.
    property OnMessageReceived: TWSMessageEvent read FOnMessageReceived write FOnMessageReceived;
    // Called when a connection is disconnected. Sender is TCustomWebsocketClient
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    // Called when a control message is received.
    property OnControlReceived: TWSControlEvent read FOnControl write FOnControl;
    // Called when a websocket handshake is performed; use this to tune the response headers.
    Property OnConnectionHandshake : TWSConnectionHandshakeEvent Read FOnConnectionHandshake Write FOnConnectionHandshake;
    // Called when unhandled exceptions occur
    Property OnError : TWSErrorEvent Read FOnError Write FOnError;
    // Mask to use when sending frames. Set to nonzero value to send masked frames.
    Property OutgoingFrameMask : Integer Read FOutgoingFrameMask Write FOutgoingFrameMask;
  end;

implementation

{ TWSConnectionList }

function TWSConnectionList.ForEach(aIterator: TConnectionIterator): Boolean;
Var
  L : TList;
  I : Integer;

begin
  Result:=True;
  L:=LockList;
  try
    I:=0;
    While Result and (I<L.Count) do
      begin
      aIterator(TWSServerConnection(L[i]),Result);
      Inc(I);
      end;
  finally
    UnlockList;
  end;
end;

function TWSConnectionList.FindConnectionById(aID: String): TWSConnection;
Var
  L : TList;
  I : Integer;

begin
  Result:=Nil;
  L:=LockList;
  try
    I:=0;
    While (Result=Nil) and (I<L.Count) do
      begin
      Result:=TWSServerConnection(L[I]);
      if Result.ConnectionID<>aID then
        Result:=Nil;
      Inc(I);
      end;
  finally
    UnlockList;
  end;
end;

{ TCustomWSServer }

procedure TCustomWSServer.DoMessageReceived(Sender: TObject; const aMessage: TWSMessage);
begin
  if Assigned(OnMessageReceived) and (TWSConnection(Sender).HandshakeCompleted) then
    OnMessageReceived(Sender, AMessage);
end;

procedure TCustomWSServer.DoControlReceived(Sender: TObject; aType: TFrameType; const aData: TBytes);
begin
  if Assigned(OnControlReceived) and (TWSConnection(Sender).HandshakeCompleted) then
    OnControlReceived(Sender, AType,aData);
end;

function TCustomWSServer.AllowConnection(AConnection: TWSServerConnection) : Boolean;

begin
  Result:=True;
  if Assigned(FonAllow) then
    OnAllow(Self,aConnection,Result);
end;

function TCustomWSServer.WaitForConnections(aMaxAttempts: Integer): Boolean;

Var
  aLastCount,ACount : Integer;

begin
  ACount:=0;
  aLastCount:=ConnectionCount;
  While (ConnectionCount>0) and (aCount<aMaxAttempts) do
    begin
    Sleep(100);
    if (ConnectionCount=aLastCount) then
      Inc(ACount)
    else
      aLastCount:=ConnectionCount;
    end;
  Result:=aLastCount=0;
end;

procedure TCustomWSServer.BroadcastData(AData: TBytes);

begin
  SendDataTo(aData,@DoAllowAll);
end;

Procedure TCustomWSServer.DoAllowAll(aConnection :TWSServerConnection; var aAllow : Boolean);

begin
  aAllow:=Assigned(AConnection);
end;

function TCustomWSServer.CreateConnections: TWSConnectionList;
begin
  Result:=TWSConnectionList.Create;
end;

procedure TCustomWSServer.BroadcastMessage(AMessage: string);

begin
  SendMessageTo(aMessage,@DoAllowAll);
end;

constructor TCustomWSServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWebSocketVersion:=DefaultWebSocketVersion;
  FConnections:=CreateConnections;
end;

destructor TCustomWSServer.Destroy;
begin
  FreeAndNil(FConnections);
  inherited;
end;

procedure TCustomWSServer.BroadcastFrame(aFrame: TWSFrame);
begin
  SendFrameTo(aFrame,@DoAllowAll);
end;

function TCustomWSServer.CreateWebsocketConnection(aStream: TSocketStream; aOptions: TWSOptions): TWSServerConnection;

Var
  aTransport: TWSServerTransport;

begin
  aTransport:=TWSServerTransport.Create(aStream);
  Result:=TWSServerConnection.Create(Self,aTransport,aOptions);
  Result.OutgoingFrameMask:=Self.OutgoingFrameMask;
end;

procedure TCustomWSServer.SendDataTo(AData: TBytes; aSelector: TWSSendToFilter);

  Function DoAllow(Conn : TWSServerConnection) : Boolean;
  begin
    Result:=Conn.HandshakeCompleted;
    if Result then
      aSelector(Conn,Result);
  end;

var
  Connection: TWSServerConnection;
  L : TList;
  I : integer;

begin
  L:=Connections.LockList;
  try
    For I:=0 to L.Count-1 do
      begin
      Connection:=TWSServerConnection(L[i]);
      if DoAllow(Connection) then
        Connection.Send(ftBinary,aData);
      end;
  finally
    Connections.UnlockList;
  end;
end;

procedure TCustomWSServer.Foreach(aIterator: TConnectionIterator);
Var
  L : TList;
  aContinue : Boolean;
  I : Integer;

begin
  aContinue:=True;
  L:=Connections.LockList;
  try
    For I:=L.Count-1 downto 0 do
      if aContinue then
        aIterator(TWSServerConnection(L[i]),aContinue);
  finally
    Connections.UnlockList;
  end;
end;

procedure TCustomWSServer.SendFrameTo(aFrame: TWSFrame; aSelector: TWSSendToFilter);

  Function DoAllow(Conn : TWSServerConnection) : Boolean;

  begin
    Result:=Conn.HandshakeCompleted;
    if Result then
      aSelector(Conn,Result);
  end;

var
  Connection: TWSServerConnection;
  L : TList;
  I : integer;

begin
  // Create the message only once.
  L:=Connections.Locklist;
  try
    For I:=0 to L.Count-1 do
      begin
      Connection:=TWSServerConnection(L[i]);
      if DoAllow(Connection) then
        Connection.Send(aFrame);
      end;
  finally
    Connections.UnlockList;
  end;
end;

procedure TCustomWSServer.SendMessageTo(AMessage: string; aSelector: TWSSendToFilter);

  Function DoAllow(Conn : TWSServerConnection) : Boolean;
  begin
    Result:=Conn.HandshakeCompleted;
    if Result then
      aSelector(Conn,Result);
  end;

var
  Connection: TWSServerConnection;
  L : TList;
  I : integer;

begin
  // Create the message only once.
  L:=Connections.Locklist;
  try
    For I:=0 to L.Count-1 do
      begin
      Connection:=TWSServerConnection(L[i]);
      if DoAllow(Connection) then
        Connection.Send(aMessage);
      end;
  finally
    Connections.UnlockList;
  end;
end;

function TCustomWSServer.GetActive: Boolean;
begin
  Result:=False;
end;

function TCustomWSServer.CreateConnectionHandler: TWSServerConnectionHandler;
begin
  Case ThreadMode of
    wtmNone : Result:=TWSSimpleConnectionHandler.Create(Self);
    wtmThread : Result:=TWSThreadedConnectionHandler.Create(Self);
    wtmThreadPool : Result:=TWSPooledConnectionHandler.Create(Self);
  end;
end;

procedure TCustomWSServer.SetActive(const Value: Boolean);
begin
  // Do nothing
end;

procedure TCustomWSServer.SetOptions(const Value: TWSOptions);
begin
  if (FOptions = Value) then exit;
  CheckInactive;
  FOptions := Value;
end;

procedure TCustomWSServer.SetResource(AValue: string);
begin
  if FResource=AValue then Exit;
  CheckInactive;
  FResource:=AValue;
end;

procedure TCustomWSServer.SetThreadMode(AValue: TWSThreadMode);

begin
  if FThreadMode=AValue then Exit;
  CheckInactive;
  FThreadMode:=AValue;
  FreeAndNil(FConnectionHandler);
end;

procedure TCustomWSServer.DoDisconnect(Sender: TObject);
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(Sender);
end;

procedure TCustomWSServer.FreeConnectionHandler;
begin
  FreeAndNil(FConnectionHandler);
end;

procedure TCustomWSServer.StartConnectionHandler;

begin
  if (ConnectionHandler=nil) then
    FConnectionHandler:=CreateConnectionHandler;
  FConnectionHandler.WaitTime:=MessageWaitTime;
end;

procedure TCustomWSServer.HandleError(aConnection: TWSServerConnection; E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self,aConnection,E);
end;

function TCustomWSServer.GetActiveConnectionCount: Integer;

Var
  L : TList;
begin
  L:=Connections.LockList;
  try
    Result:=L.Count;
  finally
    Connections.UnlockList;
  end;
end;

procedure TCustomWSServer.CloseConnectionSocket(aConnection: TWSServerConnection; var aContinue: boolean);
begin
  if aConnection.ServerTransport<>nil then
    aConnection.ServerTransport.CloseSocket;
  aContinue:=True;
end;

procedure TCustomWSServer.RemoveConnection(AConnection: TWSServerConnection;aDoDisconnect: Boolean);
begin
  if aDoDisconnect then
    try
      aConnection.Disconnect;
    except
      on E : Exception do
       HandleError(aConnection,E);
    end;
  DoDisconnect(aConnection);
  Connections.Remove(aConnection);
  aConnection.Free;
end;

procedure TCustomWSServer.CheckInactive;
begin
  if Active then
    Raise EWebsocketServer.Create(SErrServerActive);
end;

{ TWSServerConnectionHandler }

function TWSServerConnectionHandler.GetList: TWSConnectionList;
begin
  Result:=FServer.Connections;
end;

procedure TWSServerConnectionHandler.DoCheckConnectionRequests(aConnection: TWSServerConnection; var aContinue: boolean);
begin
  aConnection.CheckIncoming(WaitTime,True);
  aContinue:=True;
end;

procedure TWSServerConnectionHandler.RemoveConnection(aConnection: TWSServerConnection);
begin
  FServer.RemoveConnection(aConnection,True);
end;

procedure TWSServerConnectionHandler.HandleError(aConnection : TWSServerConnection; E: Exception);
begin
  if Assigned(FServer) then
    FServer.HandleError(aConnection,E);
end;

constructor TWSServerConnectionHandler.Create(aServer: TCustomWSServer);
begin
  FServer:=aServer;
  FWaitTime:=DefaultWaitTime;
end;

destructor TWSServerConnectionHandler.Destroy;
begin
  FServer:=Nil;
  inherited Destroy;
end;

procedure TWSServerConnectionHandler.Foreach(aIterator: TConnectionIterator);
begin
  FServer.Foreach(aIterator);
end;

procedure TWSServerConnectionHandler.CloseConnections;
begin
  Foreach(@FServer.CloseConnectionSocket);
end;

procedure TWSServerConnectionHandler.CheckIncomingMessages;
begin
  Foreach(@DoCheckConnectionRequests);
end;


{ TWSSimpleConnectionHandler }

procedure TWSSimpleConnectionHandler.HandleConnection(aConnection: TWSServerConnection; DoHandshake : Boolean);
begin
  if DoHandShake then
    aConnection.PerformHandShake;
  aConnection.CheckIncoming(WaitTime);
end;

{ TWSThreadedConnectionHandler.TWSConnectionThread }

constructor TWSThreadedConnectionHandler.TWSConnectionThread.CreateConnection(AConnection: TWSServerConnection; aOnConnectionDone : TNotifyEvent; DoHandShake : Boolean);
begin
  FOnDone:=aOnConnectionDone;
  FConnection:=AConnection;
  FDoHandshake:=DoHandshake;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure TWSThreadedConnectionHandler.TWSConnectionThread.Execute;

begin
  try
    // Always handle first request
    if FDoHandshake then
      begin
      Connection.PerformHandshake;
      if not Connection.HandshakeResponseSent then
        Terminate;
      end;
    While not Terminated do
      if Connection.CheckIncoming(10)=irClose then
      begin
        // answer for client about close connection
        if not (Connection.CloseState = csClosed) then
          Connection.Close('', CLOSE_NORMAL_CLOSURE);
        Terminate;
      end;
  except
    Raise;
   //  on E : Exception do
      // Server.HandleUnexpectedError(E);
  end;
  If Assigned(FOnDone) then
    FOnDone(Connection);
end;

{ TWSThreadedConnectionHandler }

procedure TWSThreadedConnectionHandler.CheckIncomingMessages;
begin
  // Do nothing
end;

procedure TWSThreadedConnectionHandler.ConnectionDone(Sender: TObject);
begin
  RemoveConnection(Sender as TWSServerConnection);
end;

procedure TWSThreadedConnectionHandler.HandleConnection(aConnection: TWSServerConnection; DoHandshake: Boolean);
begin
  TWSConnectionThread.CreateConnection(aConnection,@ConnectionDone,DoHandShake);
end;

{ TWSPooledConnectionHandler.THandleRequestTask }

constructor TWSPooledConnectionHandler.THandleRequestTask.Create(aConnection: TWSServerConnection; aOnConnectionDone: TNotifyEvent; aOnError: TErrorHandler; aDoHandshake : Boolean);

begin
  Inherited Create;
  // So it is removed from the busy list.
  DoneOnException:=True;
  OnDone:=aOnConnectionDone;
  FConnection:=aConnection;
  FDoHandshake:=aDoHandshake;
end;

procedure TWSPooledConnectionHandler.THandleRequestTask.DoExecute;
begin
  if FDoHandshake then
    Connection.PerformHandshake;
  Connection.ReadMessage;
end;

{ TWSPooledConnectionHandler }

procedure TWSPooledConnectionHandler.CheckIncomingMessages;

begin
  // First schedule what is already there..
  FPool.CheckQueuedTasks;
  // Now maybe add new ones
  Foreach(@CheckRequest);
end;

constructor TWSPooledConnectionHandler.Create(aServer: TCustomWSServer);
begin
  inherited Create(aServer);
  FPool:=CreatePool;
  FBusy:=TThreadList.Create;
end;

Function TWSPooledConnectionHandler.IsBusy(aConnection : TWSServerConnection) : Boolean;

Var
  L : TList;

begin
  L:=FBusy.LockList;
  try
    Result:=L.IndexOf(aConnection)<>-1;
  finally
    FBusy.UnlockList;
  end;
end;

procedure TWSPooledConnectionHandler.ScheduleRequest(aConnection: TWSServerConnection; DoHandShake : Boolean);

begin
  // So we don't schedule it again while it is waiting to be handled.
  if not IsBusy(aConnection) then
    begin
    FBusy.Add(aConnection);
    FPool.AddTask(THandleRequestTask.Create(aConnection,@ConnectionDone,@HandleError,DoHandshake));
    end;
end;

procedure TWSPooledConnectionHandler.CheckRequest(aConnection: TWSServerConnection; var aContinue: Boolean);
begin
  if Server.Active then
    Case aConnection.CheckIncoming(WaitTime,False) of
      irWaiting : ScheduleRequest(aConnection,False);
      irClose :  RemoveConnection(aConnection);
    else
      // nothing
    end;
//  if Server.Active and aConnection.AllowNewRequest and aConnection.RequestPending then
//
end;

procedure TWSPooledConnectionHandler.CloseConnections;
begin
  FPool.CancelQueuedTasks;
  inherited CloseConnections;
end;

procedure TWSPooledConnectionHandler.ConnectionDone(Sender: TObject);

var
  aTask : THandleRequestTask absolute Sender;
  aConn : TWSServerConnection;

begin
  aConn:=aTask.Connection;
  FBusy.Remove(aConn);
  if aConn.CheckIncoming(10)=irClose then
    RemoveConnection(aConn);
end;

procedure TWSPooledConnectionHandler.HandleConnection(aConnection: TWSServerConnection; DoHandshake : Boolean);
begin
  ScheduleRequest(aConnection,DoHandshake);
end;

function TWSPooledConnectionHandler.CreatePool: TFPCustomSimpleThreadPool;

Var
  P : TFPSimpleThreadPool;

begin
  P:=TFPSimpleThreadPool.Create;
  P.AddTimeout:=30;
  Result:=P;
end;



end.

