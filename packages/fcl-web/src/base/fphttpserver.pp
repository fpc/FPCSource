{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2011- by the Free Pascal development team
    
    Simple HTTP server component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fphttpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, sslbase, sslsockets, ssockets, resolve, httpdefs, httpprotocol,
  fpthreadpool;

Const
  ReadBufLen = 4096;
  DefaultKeepConnectionIdleTimeout = 50; // Ms

Type
  TFPHTTPConnection = Class;
  TFPHTTPConnectionThread = Class;
  TFPCustomHttpServer = Class;
  TRequestErrorHandler = Procedure (Sender : TObject; E : Exception) of object;
  TGetSocketHandlerEvent = Procedure (Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler) of object;
  TSocketHandlerCreatedEvent = Procedure (Sender : TObject; AHandler : TSocketHandler) of object;

  { TFPHTTPConnectionRequest }

  TFPHTTPConnectionRequest = Class(TRequest)
  private
    FConnection: TFPHTTPConnection;
  protected
    Procedure InitRequestVars; override;
  published
    Property Connection : TFPHTTPConnection Read FConnection;
  end;

  { TFPHTTPConnectionResponse }

  TFPHTTPConnectionResponse = Class(TResponse)
  private
    FConnection: TFPHTTPConnection;
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
    Property Connection : TFPHTTPConnection Read FConnection;
  end;


  { TFPHTTPConnection }

  TFPHTTPConnection = Class(TObject)
  private
    Class var _ConnectionCount : {$IFDEF CPU64}QWord{$ELSE}Cardinal{$ENDIF};
    procedure ParseStartLine(Request: TFPHTTPConnectionRequest;
      AStartLine: String);
  private
    FBusy: Boolean;
    FConnectionID: String;
    FEmptyDetected: Boolean;
    FIsUpgraded: Boolean;
    FOnRequestError: TRequestErrorHandler;
    FOnUnexpectedError: TRequestErrorHandler;
    FServer: TFPCustomHTTPServer;
    FSocket: TSocketStream;
    FIsSocketSetup : Boolean;
    FBuffer : Ansistring;
    FKeepAlive : Boolean;
    function GetKeepConnections: Boolean;
    function GetKeepConnectionTimeout: Integer;
    function GetKeepConnectionIdleTimeout: Integer;
    procedure InterPretHeader(ARequest: TFPHTTPConnectionRequest; const AHeader: String);
    function ReadString: String;
    Function GetLookupHostNames : Boolean;
  Protected
    // Allocate the ID for this connection.
    Procedure AllocateConnectionID;
    // Read the request content
    procedure ReadRequestContent(ARequest: TFPHTTPConnectionRequest); virtual;
    // Allow descendents to handle unknown headers
    procedure UnknownHeader({%H-}ARequest: TFPHTTPConnectionRequest; const {%H-}AHeader: String); virtual;
    // Handle request error, calls OnRequestError
    procedure HandleRequestError(E : Exception); virtual;
    // Handle unexpected error, calls OnUnexpectedError
    procedure HandleUnexpectedError(E : Exception); virtual;
    // Setup socket
    Procedure SetupSocket; virtual;
    // Mark connection as busy with request
    Procedure SetBusy;
    // Actually handle request
    procedure DoHandleRequest; virtual;
    // Called when KeepConnection is idle.
    procedure DoKeepConnectionIdle; virtual;
    // Read request headers
    Function ReadRequestHeaders : TFPHTTPConnectionRequest;
    // Check if we have keep-alive and no errors occurred
    Function AllowNewRequest : Boolean;
    // Check if there is a new request pending, i.e. there is data.
    Function RequestPending : Boolean;
    // True if we're handling a request. Needed to be able to schedule properly.
    Property Busy : Boolean Read FBusy;
    // If empty detected, we should not proceed
    Property EmptyDetected : Boolean Read FEmptyDetected;
    // The server supports HTTP 1.1 connection: keep-alive
    Property KeepConnections : Boolean read GetKeepConnections;
    // Idle time-out for keep-alive: after how many ms should the connection fire the OnKeepConnectionIdle event
    Property KeepConnectionIdleTimeout: Integer read GetKeepConnectionIdleTimeout;
    // Time-out for keep-alive: how many ms should the server keep the connection alive after a request has been handled.
    //   After this timeout the keep-alive connection is forcefully closed.
    Property KeepConnectionTimeout: Integer read GetKeepConnectionTimeout;
  Public
    Type
      TConnectionIDAllocator = Procedure(out aID : String) of object;
    class var IDAllocator : TConnectionIDAllocator;
  Public
    Constructor Create(AServer : TFPCustomHTTPServer; ASocket : TSocketStream);
    Destructor Destroy; override;
    // Handle 1 request: Set up socket if needed, Read request, dispatch, return response.
    Procedure HandleRequest;
    // Unique ID per new connection
    Property ConnectionID : String Read FConnectionID;
    // The socket used by this connection
    Property Socket : TSocketStream Read FSocket;
    // The server that created this connection
    Property Server : TFPCustomHTTPServer Read FServer;
    // Handler to call when an error occurs.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
    // Called when an unexpected error occurs outside the request.
    Property OnUnexpectedError : TRequestErrorHandler Read FOnUnexpectedError Write FOnUnexpectedError;
    // Look up host names to map IP -> hostname ?
    Property LookupHostNames : Boolean Read GetLookupHostNames;
    // is the current connection set up for KeepAlive?
    Property KeepAlive: Boolean read FKeepAlive;
    // Is the current connection upgraded ?
    Property IsUpgraded : Boolean Read FIsUpgraded;
  end;

  { TFPHTTPConnectionThread }
  TFPHTTPConnectionThread = Class(TThread)
  private
    FConnection: TFPHTTPConnection;
    FOnDone : TNotifyEvent;
  Public
    Constructor CreateConnection(AConnection : TFPHTTPConnection; aOnConnectionDone : TNotifyEvent); virtual;
    Procedure Execute; override;
    Property Connection : TFPHTTPConnection Read FConnection;
  end;

  { TFPHttpServer }
  THTTPServerRequestHandler = Procedure (Sender: TObject;
      Var ARequest: TFPHTTPConnectionRequest;
      Var AResponse : TFPHTTPConnectionResponse) of object;

  { TFPCustomHttpServer }
  TThreadMode = (tmNone,tmThread,tmThreadPool);

  { TFPHTTPServerConnectionHandler }

  TFPHTTPServerConnectionHandler = Class(TObject)
  Private
    FServer : TFPCustomHttpServer;
  Protected
    Procedure RemoveConnection(aConnection :TFPHTTPConnection); virtual; abstract;
  Public
    Constructor Create(aServer : TFPCustomHttpServer); virtual;
    Destructor Destroy; override;
    Procedure CloseSockets; virtual; abstract;
    Procedure CheckRequests; virtual; abstract;
    Function WaitForRequests(MaxAttempts : Integer = 10) : Boolean; virtual;
    Function GetActiveConnectionCount : Integer; virtual; abstract;
    Procedure HandleConnection(aConnection : TFPHTTPConnection); virtual; abstract;
    Property Server : TFPCustomHttpServer Read FServer;
  end;

  TConnectionList = Class(TThreadList)
  end;

  { TFPHTTPServerConnectionListHandler }

  TFPHTTPServerConnectionListHandler = Class(TFPHTTPServerConnectionHandler)
  Private
    FList: TConnectionList;
  Protected
    Type
      TConnectionIterator = Procedure (aConnection :TFPHTTPConnection; var aContinue : boolean) of object;
    Function CreateList : TConnectionList;
    Procedure CloseConnectionSocket(aConnection :TFPHTTPConnection; var {%H-}aContinue : boolean);
    Procedure Foreach(aIterator : TConnectionIterator);
    Procedure RemoveConnection(aConnection :TFPHTTPConnection); override;
  Public
    Constructor Create(aServer : TFPCustomHTTPServer); override;
    Destructor Destroy; override;
    Procedure HandleConnection(aConnection : TFPHTTPConnection); override;
    Procedure CloseSockets; override;
    Function GetActiveConnectionCount : Integer; override;
    Property List : TConnectionList Read FList;
  end;


  { TFPSimpleConnectionHandler }

  TFPSimpleConnectionHandler = Class(TFPHTTPServerConnectionHandler)
    FConnection : TFPHTTPConnection;
  Protected
    Procedure RemoveConnection(aConnection :TFPHTTPConnection); override;
  Public
    procedure CheckRequests; override;
    Procedure HandleConnection(aConnection : TFPHTTPConnection); override;
    Function GetActiveConnectionCount : Integer; override;
    Procedure CloseSockets; override;
    Property Connection : TFPHTTPConnection Read FConnection;
  end;

  { TFPThreadedConnectionHandler }

  TFPThreadedConnectionHandler = Class(TFPHTTPServerConnectionListHandler)
  private
    procedure ConnectionDone(Sender: TObject);
  Public
    procedure CheckRequests; override;
    Procedure HandleConnection(aConnection : TFPHTTPConnection); override;
  end;

  { TFPPooledConnectionHandler }

  TFPPooledConnectionHandler = Class(TFPHTTPServerConnectionListHandler)
  Private
    FPool : TFPCustomSimpleThreadPool;
  Protected
    Type

       { THandleRequestTask }

       THandleRequestTask = Class(TThreadPoolTask)
       private
         FConnection: TFPHTTPConnection;
         FOnDone: TNotifyEvent;
       Protected
         procedure DoExecute; override;
       Public
         Constructor Create(aConnection : TFPHTTPConnection; aOnConnectionDone : TNotifyEvent);
         Property Connection : TFPHTTPConnection Read FConnection;
         Property OnDone : TNotifyEvent Read FOnDone;
       end;
    procedure ConnectionDone(Sender: TObject); virtual;
    procedure ScheduleRequest(aConnection: TFPHTTPConnection); virtual;
    procedure CheckRequest(aConnection: TFPHTTPConnection; var {%H-}aContinue: Boolean); virtual;
  Public
    Procedure CloseSockets; override;
    procedure CheckRequests; override;
    Constructor Create(aServer : TFPCustomHttpServer); override;
    Destructor Destroy; override;
    Procedure HandleConnection(aConnection : TFPHTTPConnection); override;
    function CreatePool : TFPCustomSimpleThreadPool;
    Property Pool : TFPCustomSimpleThreadPool Read FPool;
  end;

  // List of server connection handlers TFPHTTPServerConnectionHandler


  THandlesUpgradeEvent = procedure(aRequest : TFPHTTPConnectionRequest; var aHandlesUpgrade : Boolean) of object;
  TUpgradeConnectionEvent = procedure(aConnection : TFPHTTPConnection; aRequest : TFPHTTPConnectionRequest) of object;

  { TUpgradeHandlerItem }

  TUpgradeHandlerItem = Class(TCollectionItem)
  private
    FName: String;
    FOnHandlesUpgrade: THandlesUpgradeEvent;
    FOnUpgrade: TUpgradeConnectionEvent;
  Public
    Property Name : String Read FName Write FName;
    Property OnHandleUpgrade : THandlesUpgradeEvent Read FOnHandlesUpgrade Write FOnHandlesUpgrade;
    Property OnUpgrade : TUpgradeConnectionEvent Read FOnUpgrade Write FOnUpgrade;
  end;

  { TUpgradeHandlerList }

  TUpgradeHandlerList = Class(TCollection)
  private
    function GetHandlerItem(aIndex : Integer): TUpgradeHandlerItem;
  Public
    Function IndexOfName(const aName : String) : Integer;
    Function HandlerByName(const aName : String) : TUpgradeHandlerItem;
    Function AddHandler(const aName : String; aOnCheck : THandlesUpgradeEvent; aOnUpgrade : TUpgradeConnectionEvent) : TUpgradeHandlerItem; virtual;
    Property Handler[aIndex :Integer] : TUpgradeHandlerItem Read GetHandlerItem; default;
  end;

  { TConnectionList }

  THTTPLogEvent = Procedure (aSender : TObject; aType: TEventType; Const Msg : String) of object;
  // Events in the lifetime of a request that are logged
  THTTPLogMoment = (hlmStartSocket,hlmCloseSocket,hlmConnect,hlmNoHTTPProtocol, hlmEmptyRequest, hlmRequestStart,hlmHeaders,hlmRequestDone,hlmUpgrade,hlmDisconnect,hlmError);
  THTTPLogMoments = set of THTTPLogMoment;

  TFPCustomHttpServer = Class(TComponent)
  Private
    FAcceptIdleTimeout: Cardinal;
    FAdminMail: string;
    FAdminName: string;
    FAfterSocketHandlerCreated: TSocketHandlerCreatedEvent;
    FCertificateData: TCertificateData;
    FKeepConnections: Boolean;
    FKeepConnectionIdleTimeout: Integer;
    FKeepConnectionTimeout: Integer;
    FLogMoments: THTTPLogMoments;
    FOnAcceptIdle: TNotifyEvent;
    FOnKeepConnectionIdle: TNotifyEvent;
    FOnAllowConnect: TConnectQuery;
    FOnGetSocketHandler: TGetSocketHandlerEvent;
    FOnLog: THTTPLogEvent;
    FOnRequest: THTTPServerRequestHandler;
    FOnRequestError: TRequestErrorHandler;
    FOnUnexpectedError: TRequestErrorHandler;
    FAddress: string;
    FPort: Word;
    FQueueSize: Word;
    FServer : TInetServer;
    FLoadActivate : Boolean;
    FServerBanner: string;
    FLookupHostNames : Boolean;
    FTreadMode: TThreadMode;
    FUseSSL: Boolean;
    FConnectionHandler : TFPHTTPServerConnectionHandler;
    FUpdateHandlers : TUpgradeHandlerList;
    procedure DoCreateClientHandler(Sender: TObject; out AHandler: TSocketHandler);
    function GetActive: Boolean;
    function GetConnectionCount: Integer;
    function GetHasUpdateHandlers: Boolean;
    function GetHostName: string;
    function GetThreaded: Boolean;
    function GetUpdateHandlers: TUpgradeHandlerList;
    procedure SetAcceptIdleTimeout(AValue: Cardinal);
    procedure SetActive(const AValue: Boolean);
    procedure SetCertificateData(AValue: TCertificateData);
    procedure SetHostName(const AValue: string);
    procedure SetIdle(const AValue: TNotifyEvent);
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
    procedure SetThreadMode(AValue: TThreadMode);
    procedure SetupSocket;
    procedure SetupConnectionHandler;
  Protected
    // Should given event type be logged ?
    function CanLog(aMoment: THTTPLogMoment): Boolean; inline;
    Procedure DoLog(aType : TEventType; const Msg : String); overload;
    procedure DoLog(aMoment: THTTPLogMoment; const Msg: String); overload;
    Procedure DoLog(aMoment : THTTPLogMoment; const Fmt : String; Const Args : Array of const); overload;
    Function CheckUpgrade(aConnection : TFPHTTPConnection; aRequest : TFPHTTPConnectionRequest) : Boolean;
    // Override this to create Descendent
    Function CreateUpgradeHandlerList : TUpgradeHandlerList; virtual;
    // Override this to create descendent
    function CreateSSLSocketHandler: TSocketHandler; virtual;
    // Override this to create descendent
    Function CreateCertificateData : TCertificateData; virtual;
    // Override this to create descendent
    Function GetSocketHandler(Const UseSSL : Boolean) : TSocketHandler;  virtual;
    // Override these to create descendents of the request/response instead.
    Function CreateRequest : TFPHTTPConnectionRequest; virtual;
    Function CreateResponse(ARequest : TFPHTTPConnectionRequest) : TFPHTTPConnectionResponse; virtual;
    Procedure InitRequest({%H-}ARequest : TFPHTTPConnectionRequest); virtual;
    Procedure InitResponse({%H-}AResponse : TFPHTTPConnectionResponse); virtual;
    // Called on accept errors
    procedure DoAcceptError(Sender: TObject; {%H-}ASocket: Longint; {%H-}E: Exception;  var ErrorAction: TAcceptErrorAction); virtual;
    // Called when accept is idle. Will check for new requests.
    procedure DoAcceptIdle(Sender: TObject); virtual;
    // Called when KeepConnection is idle.
    procedure DoKeepConnectionIdle(Sender: TObject); virtual;
    // Create a connection handling object.
    function CreateConnection(Data : TSocketStream) : TFPHTTPConnection; virtual;
    // Create a connection handler object depending on threadmode
    Function CreateConnectionHandler : TFPHTTPServerConnectionHandler; virtual;
    // Check if server is inactive
    Procedure CheckInactive; virtual;
    // Called by TInetServer when a new connection is accepted.
    Procedure DoConnect(Sender : TObject; Data : TSocketStream); virtual;
    // Create and configure TInetServer
    Procedure CreateServerSocket; virtual;
    // Start server socket
    procedure StartServerSocket; virtual;
    // Stop server stocket
    procedure StopServerSocket; virtual;
    // free server socket instance
    Procedure FreeServerSocket; virtual;
    // Handle request. This calls OnRequest. It can be overridden by descendants to provide standard handling.
    procedure HandleRequest(Var ARequest: TFPHTTPConnectionRequest;
                            Var AResponse : TFPHTTPConnectionResponse); virtual;
    // Called when a connection encounters an unexpected error. Will call OnRequestError when set.
    procedure HandleRequestError(Sender: TObject; E: Exception); virtual;
    // Called when a connection encounters an error outside the request. Will call OnUnexpectedError when set.
    procedure HandleUnexpectedError(Sender: TObject; E : Exception); virtual;
    // Connection Handler
    Property Connectionhandler : TFPHTTPServerConnectionHandler Read FConnectionHandler;
    // Connection count. Convenience shortcut for Connectionhandler.GetActiveConnectionCount;
    Property ConnectionCount : Integer Read GetConnectionCount;
    // Upgrade handlers. Created on demand
    Property UpdateHandlers : TUpgradeHandlerList Read GetUpdateHandlers;
    // Has update handlers
    Property HasUpdateHandlers : Boolean Read GetHasUpdateHandlers;
  Public
    Type
       TLogMomentEventTypes = Array [THTTPLogMoment] of TEventType;
    Class var
      // How to report certain events
      LogMomentEventTypes : TLogMomentEventTypes;

  public
    class constructor init;
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function RegisterUpdateHandler(Const aName : string; const OnCheck : THandlesUpgradeEvent; const OnUpgrade : TUpgradeConnectionEvent) : TUpgradeHandlerItem;
    Procedure UnRegisterUpdateHandler(Const aName : string);
  protected
    // Set to true to start listening.
    Property Active : Boolean Read GetActive Write SetActive Default false;
    // Address to listen on.
    Property Address : string Read FAddress Write SetAddress;
    // Port to listen on.
    Property Port : Word Read FPort Write SetPort Default 80;
    // Set to true if you want to support HTTP 1.1 connection: keep-alive - only available for threaded server
    Property KeepConnections: Boolean read FKeepConnections write FKeepConnections;
    // Idle time-out for keep-alive: after how many ms should the connection fire the OnKeepConnectionIdle event
    Property KeepConnectionIdleTimeout: Integer read FKeepConnectionIdleTimeout write FKeepConnectionIdleTimeout;
    // Time-out for keep-alive: how many ms should the server keep the connection alive after a request has been handled.
    //   After this timeout the keep-alive connection is forcefully closed.
    Property KeepConnectionTimeout: Integer read FKeepConnectionTimeout write FKeepConnectionTimeout;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read FQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read FOnAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read GetThreaded Write SetThreaded; deprecated 'Use ThreadMode instead';
    // ThreadMode: none, threading, threadpool.
    property ThreadMode : TThreadMode read FTreadMode Write SetThreadMode;
    // Called to handle the request. If Threaded=True, it is called in a the connection thread.
    Property OnRequest : THTTPServerRequestHandler Read FOnRequest Write FOnRequest;
    // Called when an unexpected error occurs during handling of the request. Sender is the TFPHTTPConnection.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
    // Called when an unexpected error occurs outside the request. Sender is either the TFPHTTPConnection or TFPCustomHttpServer
    Property OnUnexpectedError : TRequestErrorHandler Read FOnUnexpectedError Write FOnUnexpectedError;
    // Called when there are no connections waiting.
    Property OnAcceptIdle : TNotifyEvent Read FOnAcceptIdle Write SetIdle;
    // Called when there are no requests waiting in a keep-alive connection.
    Property OnKeepConnectionIdle : TNotifyEvent Read FOnKeepConnectionIdle Write FOnKeepConnectionIdle;
    // If >0, when no new connection appeared after timeout, OnAcceptIdle is called.
    Property AcceptIdleTimeout : Cardinal Read FAcceptIdleTimeout Write SetAcceptIdleTimeout;
  published
    //aditional server information
    property AdminMail: string read FAdminMail write FAdminMail;
    property AdminName: string read FAdminName write FAdminName;
    property ServerBanner: string read FServerBanner write FServerBanner;
    Property LookupHostNames : Boolean Read FLookupHostNames Write FLookupHostNames;
    // You need to set this if you want to use SSL
    property HostName : string Read GetHostName Write SetHostName; deprecated 'Use certificatedata instead';
    // Properties to use when doing SSL handshake
    Property CertificateData  : TCertificateData Read FCertificateData Write SetCertificateData;
    // Set to true if you want to use SSL
    Property UseSSL : Boolean Read FUseSSL Write FUseSSL;
    // Set this to filter events
    Property LogMoments : THTTPLogMoments Read FLogMoments Write FLogMoments;
    // Called to create socket handler. If not set, or Nil is returned, a standard socket handler is created.
    Property OnGetSocketHandler : TGetSocketHandlerEvent Read FOnGetSocketHandler Write FOnGetSocketHandler;
    // Called after create socket handler was created, with the created socket handler.
    Property AfterSocketHandlerCreate : TSocketHandlerCreatedEvent Read FAfterSocketHandlerCreated Write FAfterSocketHandlerCreated;
    // Called to log events
    Property OnLog : THTTPLogEvent Read FOnLog Write FOnLog;
  end;

  TFPHttpServer = Class(TFPCustomHttpServer)
  Published
    Property Address;
    Property Active;
    Property Port;
    Property QueueSize;
    Property OnAllowConnect;
    property Threaded;
    property ThreadMode;
    Property OnRequest;
    Property OnRequestError;
    Property OnAcceptIdle;
    Property AcceptIdleTimeout;
    Property KeepConnections;
    Property KeepConnectionIdleTimeout;
    Property KeepConnectionTimeout;
    Property OnKeepConnectionIdle;
    Property OnUnexpectedError;
    Property LogMoments;
    Property OnLog;
  end;

  EHTTPServer = Class(EHTTP);

  Function GetStatusCode (ACode: Integer) : String; deprecated 'Use GetHTTPStatusText from unit httpprotocol';

Const
  AllLogMoments = [Low(THTTPLogMoment)..High(THTTPLogMoment)];

implementation


resourcestring
  SErrSocketActive    =  'Operation not allowed while server is active';
  SErrReadingSocket   = 'Error reading data from the socket';
  SErrMissingProtocol = 'Missing HTTP protocol version in request "%s"';
  SErrDuplicateUpgradeHandler = 'Duplicate upgrade handler';
  SUpgradingConnection = 'connection "%s" is upgraded to %s for request: %s';
  SErrAcceptingNewConnection = 'Accepting new connection (%s) from %s';
  SErrorDuringRequest = 'Exception %s during request handling: %s';
  SStartSocket = 'Creating socket on port %d';
  SClosingConnection = 'Closing connection (%s) to %s';
  SStopSocket = 'Closing socket on port %d';
  SRequestDone = 'Finished handling request %s';
  SRequestStart = 'Start handling request %s';
  SErrLogMissingProtocol = 'Missing HTTP protocol version in first line "%s" for request';
  SWarnEmptyRequest = 'Empty request detected.';

{ TFPHTTPConnectionRequest }

Function GetStatusCode (ACode: Integer) : String;

begin
  Result := GetHTTPStatusText(ACode);
end;

Function GetHostNameByAddress(const AnAddress: String): String;
var
  Resolver: THostResolver;
begin
  Result := '';
  if AnAddress = '' then exit;
  Resolver := THostResolver.Create(nil);
  try
    if Resolver.AddressLookup(AnAddress) then
      Result := Resolver.ResolvedName
  finally
    FreeAndNil(Resolver);
  end;
end;

{ TUpgradeHandlerList }

function TUpgradeHandlerList.GetHandlerItem(aIndex : Integer): TUpgradeHandlerItem;
begin
  Result:=TUpgradeHandlerItem(Items[aIndex]);
end;

function TUpgradeHandlerList.IndexOfName(const aName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(aName,GetHandlerItem(Result).Name) do
    Dec(Result);
end;

function TUpgradeHandlerList.HandlerByName(const aName: String): TUpgradeHandlerItem;

Var
  Idx : integer;

begin
  Idx:=IndexOfName(aName);
  if (Idx=-1) then
    Result:=Nil
  else
    Result:=GetHandlerItem(Idx);
end;

function TUpgradeHandlerList.AddHandler(const aName: String;
  aOnCheck: THandlesUpgradeEvent; aOnUpgrade: TUpgradeConnectionEvent
  ): TUpgradeHandlerItem;
begin
  if IndexOfName(aName)<>-1 then
    Raise EHTTPServer.CreateFmt(SErrDuplicateUpgradeHandler,[aName]);
  Result:=Add as TUpgradeHandlerItem;
  Result.Name:=aName;
  Result.OnHandleUpgrade:=aOnCheck;
  Result.OnUpgrade:=aOnUpgrade;
end;

{ TFPPooledConnectionHandler.THandleRequestTask }

constructor TFPPooledConnectionHandler.THandleRequestTask.Create(aConnection: TFPHTTPConnection; aOnConnectionDone : TNotifyEvent);
begin
  FOnDone:=aOnConnectionDone;
  FConnection:=aConnection;
end;

procedure TFPPooledConnectionHandler.THandleRequestTask.DoExecute;
begin
  Try
    Connection.HandleRequest;
    if Assigned(FOnDone) then
      FOnDone(Connection);
  except
    On E : Exception do
      Connection.HandleUnexpectedError(E);
  end;
end;

{ TFPPooledConnectionHandler }

procedure TFPPooledConnectionHandler.CheckRequests;

begin
  // First schedule what is already there..
  FPool.CheckQueuedTasks;
  // Now maybe add
  Foreach(@CheckRequest);
end;

constructor TFPPooledConnectionHandler.Create(aServer: TFPCustomHttpServer);
begin
  inherited Create(aServer);
  FPool:=CreatePool;
end;

destructor TFPPooledConnectionHandler.Destroy;
begin
  FreeAndNil(FPool);
  inherited Destroy;
end;

procedure TFPPooledConnectionHandler.ScheduleRequest(aConnection: TFPHTTPConnection);

begin
  // So we don't schedule it again while it is waiting to be handled.
  aConnection.SetBusy;
  FPool.AddTask(THandleRequestTask.Create(aConnection,@ConnectionDone));
end;

procedure TFPPooledConnectionHandler.CheckRequest(aConnection: TFPHTTPConnection; var aContinue: Boolean);
begin
  if Server.Active and aConnection.AllowNewRequest and aConnection.RequestPending then
    ScheduleRequest(aConnection);
end;

procedure TFPPooledConnectionHandler.CloseSockets;
begin
  FPool.CancelQueuedTasks;
  inherited CloseSockets;
end;

procedure TFPPooledConnectionHandler.ConnectionDone(Sender: TObject);

var
  aConn : TFPHTTPConnection;

begin
  aConn:=Sender as TFPHTTPConnection;
  if Not aConn.AllowNewRequest then
    RemoveConnection(aConn);
end;

procedure TFPPooledConnectionHandler.HandleConnection(aConnection: TFPHTTPConnection);
begin
  Inherited;
  ScheduleRequest(aConnection);
end;

function TFPPooledConnectionHandler.CreatePool: TFPCustomSimpleThreadPool;

Var
  P : TFPSimpleThreadPool;

begin
  P:=TFPSimpleThreadPool.Create;
  //P.AddWaitInterval:=10;
  P.AddTimeout:=30;
  Result:=P;
end;

{ TFPHTTPServerConnectionListHandler }

function TFPHTTPServerConnectionListHandler.CreateList: TConnectionList;
begin
  Result:=TConnectionList.Create;
end;

destructor TFPHTTPServerConnectionListHandler.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TFPHTTPServerConnectionListHandler.CloseConnectionSocket(aConnection: TFPHTTPConnection; var aContinue: boolean);
begin
  if Not aConnection.IsUpgraded then
    begin
    sockets.CloseSocket(aConnection.Socket.Handle);
    aConnection.FKeepAlive:=False; // to exit the keep-alive loop for hanging sockets
    end;
end;

procedure TFPHTTPServerConnectionListHandler.Foreach(aIterator: TConnectionIterator);
Var
  L : TList;
  aContinue : Boolean;
  I : Integer;

begin
  aContinue:=True;
  L:=FList.LockList;
  try
    For I:=L.Count-1 downto 0 do
      if aContinue then
        aIterator(TFPHTTPConnection(L[i]),aContinue);
  finally
    FList.UnlockList;
  end;
end;

procedure TFPHTTPServerConnectionListHandler.RemoveConnection(aConnection: TFPHTTPConnection);
begin
  Flist.Remove(aConnection);
  aConnection.Free;
end;

constructor TFPHTTPServerConnectionListHandler.Create(aServer: TFPCustomHTTPServer);
begin
  inherited Create(aServer);
  FList:=CreateList;
end;

procedure TFPHTTPServerConnectionListHandler.HandleConnection(aConnection: TFPHTTPConnection);
begin
  FList.Add(aConnection);
end;

procedure TFPHTTPServerConnectionListHandler.CloseSockets;
begin
  Foreach(@CloseConnectionSocket);
end;

function TFPHTTPServerConnectionListHandler.GetActiveConnectionCount: Integer;

Var
  L : TList;
begin
  L:=FList.LockList;
  try
    Result:=L.Count;
  finally
    FList.UnlockList;
  end;
end;

{ TFPThreadedConnectionHandler }

procedure TFPThreadedConnectionHandler.ConnectionDone(Sender: TObject);
begin
  RemoveConnection(Sender as TFPHTTPConnection);
end;

procedure TFPThreadedConnectionHandler.CheckRequests;
begin
  // Do nothing
end;

procedure TFPThreadedConnectionHandler.HandleConnection(aConnection: TFPHTTPConnection);
begin
  Inherited; // Adds to list
  TFPHTTPConnectionThread.CreateConnection(aConnection,@ConnectionDone);
end;


{ TFPSimpleConnectionHandler }

function TFPSimpleConnectionHandler.GetActiveConnectionCount: Integer;
begin
  Result:=Ord(Assigned(FConnection));
end;

procedure TFPSimpleConnectionHandler.RemoveConnection(aConnection: TFPHTTPConnection);
begin
  if aConnection=FConnection then
    FConnection:=Nil;
  aConnection.Free;
end;

procedure TFPSimpleConnectionHandler.CheckRequests;
begin
  // Do nothing
end;

procedure TFPSimpleConnectionHandler.HandleConnection(aConnection: TFPHTTPConnection);
begin
  FConnection:=AConnection;
  try
    FConnection.HandleRequest;
  finally
    RemoveConnection(aConnection);
  end;
end;

procedure TFPSimpleConnectionHandler.CloseSockets;
begin
  if Assigned(FConnection) then
    sockets.CloseSocket(FConnection.Socket.Handle);
end;


{ TFPHTTPServerConnectionHandler }

constructor TFPHTTPServerConnectionHandler.Create(aServer: TFPCustomHttpServer);
begin
  FServer:=aServer;
end;

destructor TFPHTTPServerConnectionHandler.Destroy;
begin
  FServer:=Nil;
  inherited Destroy;
end;

Function TFPHTTPServerConnectionHandler.WaitForRequests(MaxAttempts: Integer = 10) : Boolean;

Var
  aLastCount,ACount : Integer;

begin
  ACount:=0;
  aLastCount:=GetActiveConnectionCount;
  While (GetActiveConnectionCount>0) and (aCount<MaxAttempts) do
    begin
    Sleep(100);
    if (GetActiveConnectionCount=aLastCount) then
      Inc(ACount)
    else
      aLastCount:=GetActiveConnectionCount;
    end;
  Result:=aLastCount=0;
end;




procedure TFPHTTPConnectionRequest.InitRequestVars;
Var
  P : Integer;
  S : String;
begin
  S:=URL;
  P:=Pos('?',S);
  if (P<>0) then
    SetHTTPVariable(hvQuery,Copy(S,P+1,Length(S)-P));
  if Assigned(FConnection) and FConnection.LookupHostNames then
    SetHTTPVariable(hvRemoteHost,GetHostNameByAddress(RemoteAddress));
  inherited InitRequestVars;
end;

Function SocketAddrToString(ASocketAddr: TSockAddr): String;
begin
  if ASocketAddr.sa_family = AF_INET then
    Result := NetAddrToStr(ASocketAddr.sin_addr)
  else // no ipv6 support yet
    Result := '';
end;



procedure TFPHTTPConnectionResponse.DoSendHeaders(Headers: TStrings);

Var
  S : UTF8String;
  I : Integer;
begin
  if Connection.IsUpgraded then
    exit;
  S:=Format('HTTP/1.1 %3d %s'#13#10,[Code,GetHTTPStatusText(Code)]);
  For I:=0 to Headers.Count-1 do
    S:=S+UTF8Encode(Headers[i]+#13#10);
  // Last line in headers is empty.
  Connection.Socket.WriteBuffer(S[1],Length(S));
end;

procedure TFPHTTPConnectionResponse.DoSendContent;
begin
  if Connection.IsUpgraded then
    exit;
  If Assigned(ContentStream) and (ContentStream.Size>0) then
    Connection.Socket.CopyFrom(ContentStream,0)
  else
    if Length(Content)>0 then
      Connection.Socket.WriteBuffer(Content[1],Length(Content));
end;

{ TFPHTTPConnection }

function TFPHTTPConnection.ReadString : String;

  Procedure FillBuffer;

  Var
    R : Integer;

  begin
    SetLength(FBuffer,ReadBufLen);
    r:=FSocket.Read(FBuffer[1],ReadBufLen);
    If r<0 then
      Raise EHTTPServer.Create(SErrReadingSocket);
    if (r<ReadBuflen) then
      SetLength(FBuffer,r);
  end;

Var
  CheckLF,Done : Boolean;
  P,L : integer;

begin
  Result:='';
  Done:=False;
  CheckLF:=False;
  Repeat
    if Length(FBuffer)=0 then
      FillBuffer;
    if Length(FBuffer)=0 then
      Done:=True
    else if CheckLF then
      begin
      If (FBuffer[1]<>#10) then
        Result:=Result+#13
      else
        begin
        Delete(FBuffer,1,1);
        Done:=True;
        end;
      CheckLF:=False;  
      end;
    if not Done then
      begin
      P:=Pos(#13#10,FBuffer);
      If P=0 then
        begin
        L:=Length(FBuffer);
        CheckLF:=FBuffer[L]=#13;
        if CheckLF then
          Result:=Result+Copy(FBuffer,1,L-1)
        else
          Result:=Result+FBuffer;
        FBuffer:='';
        end
      else
        begin
        Result:=Result+Copy(FBuffer,1,P-1);
        Delete(FBuffer,1,P+1);
        Done:=True;
        end;
      end;
  until Done;
end;

procedure TFPHTTPConnection.UnknownHeader(ARequest: TFPHTTPConnectionRequest;
  const AHeader: String);
begin
  // Do nothing
end;

procedure TFPHTTPConnection.HandleRequestError(E: Exception);
begin
  If Assigned(FOnRequestError) then
    try
      FOnRequestError(Self,E);
    except
      On E : exception do
        HandleUnexpectedError(E);
    end
  else if Assigned(Server) and Server.CanLog(hlmError) then
    Server.DoLog(hlmError,SErrorDuringRequest,[E.ClassName,E.Message]);
end;

procedure TFPHTTPConnection.HandleUnexpectedError(E: Exception);
begin
  If Assigned(FOnUnexpectedError) then
    FOnUnexpectedError(Self,E)
  else if Assigned(Server) and Server.CanLog(hlmError) then
     Server.DoLog(hlmError,SErrorDuringRequest,[E.ClassName,E.Message]);
end;

procedure TFPHTTPConnection.SetupSocket;
begin
{$if defined(FreeBSD) or defined(Linux)}
  FSocket.ReadFlags:=MSG_NOSIGNAL;
  FSocket.WriteFlags:=MSG_NOSIGNAL;
{$endif}
  FIsSocketSetup:=True;
end;

procedure TFPHTTPConnection.SetBusy;
begin
  FBusy:=True;
end;

procedure TFPHTTPConnection.InterPretHeader(ARequest: TFPHTTPConnectionRequest;
  const AHeader: String);

Var
  P : Integer;
  N,V : String;

begin
  V:=AHeader;
  P:=Pos(':',V);
  if (P=0) then
    begin
    UnknownHeader(ARequest,Aheader);
    Exit;
    end;
  N:=Copy(V,1,P-1);
  if SameText(N,'Upgrade') then
    V:=V;
  Delete(V,1,P);
  V:=Trim(V);
  ARequest.SetFieldByName(N,V);
end;

procedure TFPHTTPConnection.ParseStartLine(Request : TFPHTTPConnectionRequest;
  AStartLine : String);

  Function GetNextWord(Var S : String) : string;

  Var
    P : Integer;

  begin
    P:=Pos(' ',S);
    If (P=0) then
      P:=Length(S)+1;
    Result:=Copy(S,1,P-1);
    Delete(S,1,P);
  end;

Var
  S : String;
  I : Integer;
  
begin
  if aStartLine='' then 
    exit;
  Request.Method:=GetNextWord(AStartLine);
  Request.URL:=GetNextWord(AStartLine);
  S:=Request.URL;
  I:=Pos('?',S);
  if (I>0) then
    S:=Copy(S,1,I-1);
  If (Length(S)>1) and (S[1]<>'/') then
    S:='/'+S
  else if S='/' then 
    S:='';
  Request.PathInfo:=S;
  S:=GetNextWord(AStartLine);
  If Assigned(Server) and Server.CanLog(hlmRequestStart) then
    Server.DoLog(hlmRequestStart,SRequestStart,[Request.ToString]);
  If (S<>'') and (Pos('HTTP/',S)<>1) then
    begin
    If Assigned(Server) and Server.CanLog(hlmNoHTTPProtocol) then
      Server.DoLog(hlmNoHTTPProtocol,SErrLogMissingProtocol,[aStartLine,Request.ToString]);
    Raise EHTTPServer.CreateFmtHelp(SErrMissingProtocol,[AStartLine],400);
    end;
  Delete(S,1,5);
  Request.ProtocolVersion:=trim(S);
end;

procedure TFPHTTPConnection.ReadRequestContent(
  ARequest: TFPHTTPConnectionRequest);

Var
  P,L,R : integer;
  S : String;

begin
  S:='';
  L:=ARequest.ContentLength;
  If (L>0) then
    begin
    SetLength(S,L);
    P:=Length(FBuffer);
    if (P>0) then
      begin
      if P>L then
        P:=L;
      Move(FBuffer[1],S[1],P);
      FBuffer:='';
      L:=L-P;
      end;
    P:=P+1;
    R:=1;
    While (L>0) and (R>0) do
      begin
      R:=FSocket.Read(S[p],L);
      If R<0 then
        Raise EHTTPServer.Create(SErrReadingSocket);
      if (R>0) then
        begin
        P:=P+R;
        L:=L-R;
        end;
      end;  
    end;
  ARequest.InitContent(S);
end;

function TFPHTTPConnection.ReadRequestHeaders: TFPHTTPConnectionRequest;

Var
  StartLine,S : String;
begin
  Result:=Nil;
  StartLine:=ReadString;
  if StartLine='' then
    exit;
  Result:=Server.CreateRequest;
  try
    Server.InitRequest(Result);
    Result.FConnection:=Self;
    ParseStartLine(Result,StartLine);
    Repeat
      S:=ReadString;
      if (S<>'') then
        InterPretHeader(Result,S);
    Until (S='');
    Result.RemoteAddress := SocketAddrToString(FSocket.RemoteAddress);
    Result.ServerPort := FServer.Port;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TFPHTTPConnection.AllowNewRequest: Boolean;
begin
  Result:=not (Busy or IsUpgraded or EmptyDetected);
  Result:=Result and KeepConnections and KeepAlive and (Socket.LastError=0) ;
end;

function TFPHTTPConnection.RequestPending: Boolean;
begin
  Result:=(Not IsUpgraded) and Socket.CanRead(KeepConnectionIdleTimeout);
end;

constructor TFPHTTPConnection.Create(AServer: TFPCustomHTTPServer;
  ASocket: TSocketStream);
begin
  FIsUpgraded:=False;
  FIsSocketSetup:=False;
  FSocket:=ASocket;
  FServer:=AServer;
  AllocateConnectionID;
end;

destructor TFPHTTPConnection.Destroy;
begin
  If Assigned(FServer) and FServer.CanLog(hlmDisConnect) then
    FServer.DoLog(hlmDisconnect,SClosingConnection,[Self.ConnectionID,HostAddrToStr(FSocket.RemoteAddress.sin_addr)]);
  FreeAndNil(FSocket);

  Inherited;
end;

function TFPHTTPConnection.GetLookupHostNames: Boolean;

begin
  if Assigned(FServer) then
    Result:=FServer.LookupHostNames
  else
    Result:=False;  
end;

procedure TFPHTTPConnection.AllocateConnectionID;

begin
  if Assigned(IDAllocator) then
    IDAllocator(FConnectionID);
  if FConnectionID='' then
{$IFDEF CPU64}
    FConnectionID:=IntToStr(InterlockedIncrement64(_ConnectionCount));
{$ELSE}
    FConnectionID:=IntToStr(InterlockedIncrement(_ConnectionCount));
{$ENDIF}
end;

procedure TFPHTTPConnection.DoHandleRequest;

Var
  Req : TFPHTTPConnectionRequest;
  Resp : TFPHTTPConnectionResponse;

begin
  if IsUpgraded then
    exit;
  // Read headers.
  Resp:=Nil;
  Req:=ReadRequestHeaders;
  If Req=Nil then
    begin
    If Assigned(Server) and Server.CanLog(hlmEmptyRequest) then
      Server.DoLog(hlmEmptyRequest,SWarnEmptyRequest);
    FKeepAlive:=False;
    FEmptyDetected:=True;
    exit;
    end;
  try
    If Assigned(Server) and Server.CanLog(hlmHeaders) then
      Server.DoLog(hlmHeaders,Req.ToString);
    //set port
    Req.ServerPort := Server.Port;
    // Read content, if any
    If Req.ContentLength>0 then
      ReadRequestContent(Req);
    Req.InitRequestVars;
    If Server.CheckUpgrade(Self,Req) then
      begin
      FSocket:=Nil; // Must have been taken over by upgrader
      FIsUpgraded:=True;
      Exit;
      end;
    if KeepConnections then
      begin
      // Read out keep-alive
      FKeepAlive:=Req.HttpVersion='1.1'; // keep-alive is default on HTTP 1.1
      if SameText(Req.GetHeader(hhConnection),'close') then
        FKeepAlive:=False
      else if SameText(Req.GetHeader(hhConnection),'keep-alive') then
        FKeepAlive:=True;
      end;
    // Create Response
    Resp:= Server.CreateResponse(Req);
    Server.InitResponse(Resp);
    // We set the header here now. User can override it when needed.
    if FKeepAlive and (Req.HttpVersion='1.0') and not Resp.HeaderIsSet(hhConnection) then
      Resp.SetHeader(hhConnection,'keep-alive');
    Resp.FConnection:=Self;
    // And dispatch
    if Server.Active then
      Server.HandleRequest(Req,Resp);
    if Assigned(Resp) and (not Resp.ContentSent) then
      Resp.SendContent;
    If Assigned(Server) and Server.CanLog(hlmRequestDone) then
      begin
      if Assigned(Resp) then
        Server.DoLog(hlmRequestDone,SRequestDone,[Resp.ToString])
      else
        Server.DoLog(hlmRequestDone,SRequestDone,['Response was freed']);
      end;
  Finally
    FreeAndNil(Resp);
    FreeAndNil(Req);
  end;
end;

procedure TFPHTTPConnection.DoKeepConnectionIdle;
begin
  if Assigned(FServer) then
    FServer.DoKeepConnectionIdle(Self);
end;

function TFPHTTPConnection.GetKeepConnections: Boolean;
begin
  if Assigned(FServer) then
    Result := FServer.KeepConnections
  else
    Result := False;
end;

function TFPHTTPConnection.GetKeepConnectionIdleTimeout: Integer;
begin
  if Assigned(FServer) then
    Result := FServer.KeepConnectionIdleTimeout
  else
    Result := 0;
  if Result=0 then
    Result := KeepConnectionTimeout; // when there is KeepConnectionTimeout set, limit KeepConnectionIdleTimeout with its value
end;

function TFPHTTPConnection.GetKeepConnectionTimeout: Integer;
begin
  if Assigned(FServer) then
    Result := FServer.KeepConnectionTimeout
  else
    Result := 0;
end;

procedure TFPHTTPConnection.HandleRequest;


begin
  FBusy:=True;
  Try
    if not FIsSocketSetup then
      SetupSocket;
    DoHandleRequest;
  Except
    On E : Exception do
      begin
      FKeepAlive:=False; // don't keep alive connections that failed
      HandleRequestError(E);
      end;
  end;
  FBusy:=False;
end;

{ TFPHTTPConnectionThread }

constructor TFPHTTPConnectionThread.CreateConnection(AConnection: TFPHTTPConnection; aOnConnectionDone : TNotifyEvent);
begin
  FOnDone:=aOnConnectionDone;
  FConnection:=AConnection;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;


procedure TFPHTTPConnectionThread.Execute;

var
  AttemptsLeft: Integer;
begin
  try
    // Always handle first request
    Connection.HandleRequest;
    if (Connection.KeepConnectionIdleTimeout>0) and (Connection.KeepConnectionTimeout>0) then
      AttemptsLeft := Connection.KeepConnectionTimeout div Connection.KeepConnectionIdleTimeout
    else
      AttemptsLeft := -1; // infinitely
    While not Terminated and Connection.AllowNewRequest and (AttemptsLeft<>0) do
      begin
      if Connection.RequestPending then
        Connection.HandleRequest
      else // KeepConnectionIdleTimeout was reached without a new request -> idle
        begin
        if AttemptsLeft>0 then
          Dec(AttemptsLeft);
        if AttemptsLeft<>0 then
          Connection.DoKeepConnectionIdle;
        end;
      end;
  except
    on E : Exception do
      Connection.HandleUnexpectedError(E);
  end;
  If Assigned(FOnDone) then
    FOnDone(Connection);
end;

{ TFPCustomHttpServer }

procedure TFPCustomHttpServer.HandleRequestError(Sender: TObject; E: Exception);
begin
  if CanLog(hlmError) then
     DoLog(hlmError,SErrorDuringRequest,[E.ClassName,E.Message]);
  If Assigned(FOnRequestError) then
    try
      FOnRequestError(Sender,E);
    except
      HandleUnexpectedError(Self, E);
    end
end;

procedure TFPCustomHttpServer.DoAcceptError(Sender: TObject; ASocket: Longint;
  E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  If Not Active then
    ErrorAction:=AEAStop
  else
    ErrorAction:=AEARaise
end;

function TFPCustomHttpServer.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result:=FLoadActivate
  else
    Result:=Assigned(FServer);
end;

function TFPCustomHttpServer.GetConnectionCount: Integer;
begin
  if Assigned(FConnectionHandler) then
    Result:=FConnectionHandler.GetActiveConnectionCount
  else
    Result:=0;
end;

function TFPCustomHttpServer.GetHasUpdateHandlers: Boolean;
begin
  Result:=FUpdateHandlers<>Nil;
end;

procedure TFPCustomHttpServer.DoCreateClientHandler(Sender: TObject; out AHandler: TSocketHandler);
begin
  AHandler:=GetSocketHandler(UseSSL);
end;

procedure TFPCustomHttpServer.DoAcceptIdle(Sender: TObject);
begin
  if Assigned(OnAcceptIdle) then
    OnAcceptIdle(Sender);
  try
    // Allow the connection handler to check for requests
    FConnectionHandler.CheckRequests;
  except
    On E : Exception do
      HandleUnexpectedError(Self, E);
  end;
end;

procedure TFPCustomHttpServer.DoKeepConnectionIdle(Sender: TObject);
begin
  if Assigned(OnKeepConnectionIdle) then
    OnKeepConnectionIdle(Sender);
end;

function TFPCustomHttpServer.GetHostName: string;
begin
  Result:=FCertificateData.HostName;
end;

function TFPCustomHttpServer.GetThreaded: Boolean;
begin
  Result:=ThreadMode=tmThread;
end;

function TFPCustomHttpServer.GetUpdateHandlers: TUpgradeHandlerList;
begin
  If FUpdateHandlers=Nil then
    FUpdateHandlers:=CreateUpgradeHandlerList;
  Result:=FUpdateHandlers;
end;

procedure TFPCustomHttpServer.SetAcceptIdleTimeout(AValue: Cardinal);
begin
  if FAcceptIdleTimeout=AValue then Exit;
  FAcceptIdleTimeout:=AValue;
  If Assigned(FServer) then
    FServer.AcceptIdleTimeOut:=AValue;
end;

procedure TFPCustomHttpServer.StopServerSocket;
begin
  if CanLog(hlmCloseSocket) then
    DoLog(hlmCloseSocket,SStopSocket,[Port]);
  FServer.StopAccepting(False);
end;

procedure TFPCustomHttpServer.SetActive(const AValue: Boolean);
begin
  If AValue=GetActive then exit;
  FLoadActivate:=AValue;
  if not (csDesigning in Componentstate) then
    if AValue then
      begin
      if (FConnectionHandler=Nil) then
        SetupConnectionHandler;
      CreateServerSocket;
      SetupSocket;
      try
        StartServerSocket;
      finally
        FreeServerSocket;
      end;
      end
    else
      StopServerSocket;
end;

procedure TFPCustomHttpServer.SetCertificateData(AValue: TCertificateData);
begin
  if FCertificateData=AValue then Exit;
  FCertificateData:=AValue;
end;

procedure TFPCustomHttpServer.SetHostName(const AValue: string);
begin
  FCertificateData.HostName:=aValue;
end;

procedure TFPCustomHttpServer.SetIdle(const AValue: TNotifyEvent);
begin
  FOnAcceptIdle:=AValue;
  if Assigned(FServer) then
    FServer.OnIdle:=AValue;
end;

procedure TFPCustomHttpServer.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  if FOnAllowConnect=AValue then exit;
  CheckInactive;
  FOnAllowConnect:=AValue;
end;

procedure TFPCustomHttpServer.SetAddress(const AValue: string);
begin
  if FAddress=AValue then exit;
  CheckInactive;
  FAddress:=AValue;
end;

procedure TFPCustomHttpServer.SetPort(const AValue: Word);
begin
  if FPort=AValue then exit;
  CheckInactive;
  FPort:=AValue;
end;

procedure TFPCustomHttpServer.SetQueueSize(const AValue: Word);
begin
  if FQueueSize=AValue then exit;
  CheckInactive;
  FQueueSize:=AValue;
end;

procedure TFPCustomHttpServer.SetThreaded(const AValue: Boolean);

Const
  Modes : Array[Boolean] of TThreadMode = (tmNone,tmThread);
begin
  if GetThreaded=AValue then exit;
  ThreadMode:=Modes[aValue];
end;

procedure TFPCustomHttpServer.SetThreadMode(AValue: TThreadMode);
begin
  if FTreadMode=AValue then Exit;
  CheckInactive;
  FTreadMode:=AValue;
  SetupConnectionHandler;
end;

function TFPCustomHttpServer.CreateRequest: TFPHTTPConnectionRequest;
begin
  Result:=TFPHTTPConnectionRequest.Create;
end;

function TFPCustomHttpServer.CreateResponse(ARequest : TFPHTTPConnectionRequest): TFPHTTPConnectionResponse;
begin
  Result:=TFPHTTPConnectionResponse.Create(ARequest);
end;

procedure TFPCustomHttpServer.InitRequest(ARequest: TFPHTTPConnectionRequest);
begin

end;

procedure TFPCustomHttpServer.InitResponse(AResponse: TFPHTTPConnectionResponse
  );
begin

end;

function TFPCustomHttpServer.CreateConnection(Data: TSocketStream): TFPHTTPConnection;
begin
  Result:=TFPHTTPConnection.Create(Self,Data);
end;

function TFPCustomHttpServer.CreateConnectionHandler: TFPHTTPServerConnectionHandler;
begin
  case ThreadMode of
    tmNone : Result:=TFPSimpleConnectionHandler.Create(Self);
    tmThread : Result:=TFPThreadedConnectionHandler.Create(Self);
    tmThreadPool : Result:=TFPPooledConnectionHandler.Create(Self);
  end;
end;

procedure TFPCustomHttpServer.CheckInactive;
begin
  If GetActive then
    Raise EHTTPServer.Create(SErrSocketActive);
end;

procedure TFPCustomHttpServer.DoConnect(Sender: TObject; Data: TSocketStream);

Var
  Con : TFPHTTPConnection;

begin
  Con:=CreateConnection(Data);
  Con.FServer:=Self;
  Con.OnRequestError:=@HandleRequestError;
  Con.OnUnexpectedError:=@HandleUnexpectedError;
  If CanLog(hlmConnect) then
    DoLog(hlmConnect,SErrAcceptingNewConnection,[Con.ConnectionID, HostAddrToStr(Data.RemoteAddress.sin_addr)]);
  FConnectionHandler.HandleConnection(Con);
end;

procedure TFPCustomHttpServer.SetupSocket;

begin
  FServer.QueueSize:=Self.QueueSize;
  FServer.ReuseAddress:=true;
end;

procedure TFPCustomHttpServer.SetupConnectionHandler;
begin
  if Assigned(FConnectionHandler) then
    FreeAndNil(FConnectionHandler);
  FConnectionHandler:=CreateConnectionHandler();
end;

function TFPCustomHttpServer.CanLog(aMoment: THTTPLogMoment): Boolean;
begin
  Result:=aMoment in FLogMoments;
end;

procedure TFPCustomHttpServer.DoLog(aType: TEventType; const Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self,aType,Msg);
end;

procedure TFPCustomHttpServer.DoLog(aMoment: THTTPLogMoment; const Msg: String);
begin
  If CanLog(aMoment) then
    DoLog(LogMomentEventTypes[aMoment],Msg)
end;

procedure TFPCustomHttpServer.DoLog(aMoment: THTTPLogMoment; const Fmt: String;
  const Args: array of const);
begin
  if CanLog(aMoment) then
    DoLog(aMoment,Format(Fmt,Args));
end;

function TFPCustomHttpServer.CheckUpgrade(aConnection: TFPHTTPConnection; aRequest: TFPHTTPConnectionRequest): Boolean;

Var
  I : Integer;
  Handler : TUpgradeHandlerItem;

begin
  Result:=HasUpdateHandlers;
  If Result then
    begin
    Result:=False;
    If Pos('upgrade',LowerCase(aRequest.GetHeader(hhConnection)))=0 then
      Exit;
    If (aRequest.GetHeader(hhUpgrade)='') then
      Exit;
    I:=0;
    While (I<UpdateHandlers.Count) and not Result do
      begin
      Handler:=UpdateHandlers[i];
      Handler.OnHandleUpgrade(aRequest,Result);
      Inc(I);
      end;
   If Result then
     begin
     if CanLog(hlmUpgrade) then
       DoLog(hlmUpgrade,SUpgradingConnection,[aConnection.ConnectionID,aRequest.GetHeader(hhUpgrade),aRequest.ToString]);
     Handler.OnUpgrade(aConnection,aRequest);
     end;
   end;
end;

function TFPCustomHttpServer.CreateUpgradeHandlerList: TUpgradeHandlerList;
begin
  Result:=TUpgradeHandlerList.Create(TUpgradeHandlerItem);
end;

procedure TFPCustomHttpServer.HandleUnexpectedError(Sender: TObject; E: Exception);
begin
  if CanLog(hlmError) then
    DoLog(hlmError,SErrorDuringRequest,[E.ClassName,E.Message]);
  If Assigned(FOnUnexpectedError) then
    FOnUnexpectedError(Sender,E);
end;

class constructor TFPCustomHttpServer.init;

Const
  aDefaults : TLogMomentEventTypes =
     (etInfo,etInfo,etInfo,etDebug,etWarning,etInfo,etInfo,etInfo,etInfo,etInfo,etError) ;

begin
  LogMomentEventTypes:=aDefaults;
end;

procedure TFPCustomHttpServer.CreateServerSocket;

begin
  if FAddress='' then
    FServer:=TInetServer.Create(FPort)
  else
    FServer:=TInetServer.Create(FAddress,FPort);
  FServer.OnCreateClientSocketHandler:=@DoCreateClientHandler;
  FServer.MaxConnections:=-1;
  FServer.OnConnectQuery:=OnAllowConnect;
  FServer.OnConnect:=@DOConnect;
  FServer.OnAcceptError:=@DoAcceptError;
  FServer.OnIdle:=@DoAcceptIdle;
  FServer.AcceptIdleTimeOut:=AcceptIdleTimeout;
end;

procedure TFPCustomHttpServer.StartServerSocket;
begin
  if CanLog(hlmStartSocket) then
    DoLog(hlmStartSocket,SStartSocket,[Port]);
  FServer.Bind;
  FServer.Listen;
  FServer.StartAccepting;
end;

procedure TFPCustomHttpServer.FreeServerSocket;
begin
  FreeAndNil(FServer);
end;

procedure TFPCustomHttpServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  If Assigned(FOnRequest) then
    FonRequest(Self,ARequest,AResponse);
end;

constructor TFPCustomHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:=80;
  FQueueSize:=5;
  FServerBanner := 'FreePascal';
  FCertificateData:=CreateCertificateData;
  FKeepConnections:=False;
  FKeepConnectionIdleTimeout:=DefaultKeepConnectionIdleTimeout;
end;


function TFPCustomHttpServer.CreateCertificateData: TCertificateData;
begin
  Result:=TCertificateData.Create;
end;

function TFPCustomHttpServer.CreateSSLSocketHandler : TSocketHandler;

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

function TFPCustomHttpServer.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;

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

destructor TFPCustomHttpServer.Destroy;

begin
  Active:=False;
  if (GetConnectionCount>0) then
  begin
    FConnectionHandler.WaitForRequests;
    // first wait for open requests to finish and get closed automatically
    // Force close
    FConnectionHandler.CloseSockets;
    // all requests must be destroyed - wait infinitely
    FConnectionHandler.WaitForRequests(High(Integer));
  end;
  FreeAndNil(FConnectionHandler);
  FreeAndNil(FUpdateHandlers);
  FreeAndNil(FCertificateData);
  inherited Destroy;
end;

function TFPCustomHttpServer.RegisterUpdateHandler(const aName: string;
  const OnCheck: THandlesUpgradeEvent; const OnUpgrade: TUpgradeConnectionEvent
  ): TUpgradeHandlerItem;
begin
  Result:=UpdateHandlers.AddHandler(aName,OnCheck,OnUpgrade);
end;

procedure TFPCustomHttpServer.UnRegisterUpdateHandler(const aName: string);

Var
  Idx : Integer;

begin
  With UpdateHandlers do
    begin
    Idx:=IndexOfName(aName);
    if Idx<>-1 then
      Delete(Idx);
    end;
end;

end.

