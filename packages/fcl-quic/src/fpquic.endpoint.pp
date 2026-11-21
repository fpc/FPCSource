{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    UDP endpoint for QUIC: a worker thread drives TQuicConnection objects; blocking client, server and stream API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Endpoint;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, System.Math, System.SyncObjs, System.Contnrs, System.Net.Sockets,
  System.Net.FPSockets, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets, FpQuic.TransportParams,
  FpQuic.OpenSSL, FpQuic.Connection, FpQuic.Udp;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, Math, SyncObjs, contnrs, sockets, fpsockets, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets,
  FpQuic.TransportParams, FpQuic.OpenSSL, FpQuic.Connection, FpQuic.Udp;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Error raised by the blocking stream API.
  EQuicStreamError = class(Exception)
  private
    FIsReset: Boolean;
    FResetCode: QWord;
  public
    // Creates the error for a reset of the stream by the peer with the application error aCode.
    constructor CreateReset(aCode: QWord);
    // True when the peer reset the stream.
    property IsReset: Boolean read FIsReset;
    // Application error code of the reset.
    property ResetCode: QWord read FResetCode;
  end;

  TQuicEndpoint = class;
  TQuicStreamAdapter = class;

  // Event storing or loading the session ticket of aKey; an empty aTicket on load means none.
  TQuicTicketEvent = procedure(Sender: TObject; const aKey: String; var aTicket: TBytes) of object;

  // Thread-safe in-memory session tickets of QUIC clients, keyed by server; share one cache between clients.
  TQuicSessionCache = class
  private
    FLock: TCriticalSection;
    FKeys: array of String;
    FTickets: array of TBytes;
    FOnStore: TQuicTicketEvent;
    FOnLoad: TQuicTicketEvent;
  public
    // Creates an empty cache.
    constructor Create;
    // Frees the cache and its tickets.
    destructor Destroy; override;
    // Remembers aTicket as the latest ticket of aKey and calls OnStore.
    procedure Store(const aKey: String; const aTicket: TBytes);
    // Returns the ticket of aKey; calls OnLoad when none is in memory. Empty when there is none.
    function Load(const aKey: String): TBytes;
    // Called after a ticket was stored in memory, to keep it elsewhere.
    property OnStore: TQuicTicketEvent read FOnStore write FOnStore;
    // Called when no ticket of a key is in memory, to fetch it from elsewhere.
    property OnLoad: TQuicTicketEvent read FOnLoad write FOnLoad;
  end;

  // A connection of an endpoint; methods are thread-safe and block up to the given timeout in milliseconds.
  TQuicEndpointConnection = class(TObject, IQuicConnectionEvents)
  private
    FEndpoint: TQuicEndpoint;
    FConnection: TQuicConnection;
    FTLS: TQuicOpenSSLTLS;
    FAddress: TNetworkAddress;
    FPort: Word;
    FAccepted: Boolean;
    FQueued: Boolean;
    FTicketKey: String;
    FRoutes: array of TBytes;
    FInHandshake: Boolean;
    function GetState: TQuicConnectionState;
    function GetEarlyDataAccepted: Boolean;
    procedure QueueAccept;
    procedure EndHandshake;
    procedure AddRoute(const aCid: TBytes);
    procedure RemoveRoute(aIndex: Integer);
    procedure UpdateRoutes;
    procedure RemoveRoutes;
  protected
    // IQuicConnectionEvents
    procedure QuicHandshakeComplete(aConnection: TQuicConnection);
    procedure QuicStreamData(aConnection: TQuicConnection; aStreamId: QWord);
    procedure QuicConnectionClosed(aConnection: TQuicConnection);
    procedure QuicSessionTicket(aConnection: TQuicConnection; const aTicket: TBytes);
  public
    // Creates the connection object; aOriginalDcid and aRetryScid are used for server connections only.
    constructor Create(aEndpoint: TQuicEndpoint; const aAddress: TNetworkAddress; aPort: Word;
      const aOriginalDcid: TBytes; const aRetryScid: TBytes = nil);
    // Closes the connection if needed and removes it from its endpoint.
    destructor Destroy; override;
    // Waits until the handshake completed; False on timeout or failure.
    function WaitHandshake(aTimeout: Integer): Boolean;
    // Opens a stream; nil when the stream limit is reached.
    function OpenStream(aBidi: Boolean = True): TQuicStreamAdapter;
    // Waits for a stream opened by the peer; nil on timeout or when the connection closed.
    function AcceptStream(aTimeout: Integer): TQuicStreamAdapter;
    // Closes the connection with an application error code.
    procedure Close(aErrorCode: QWord = 0; const aReason: String = '');
    // Waits until the connection is closed; False on timeout.
    function WaitClosed(aTimeout: Integer): Boolean;
    // Negotiated application protocol.
    function Alpn: AnsiString;
    // Connection state.
    property State: TQuicConnectionState read GetState;
    // True once the handshake completed and the server accepted 0-RTT data.
    property EarlyDataAccepted: Boolean read GetEarlyDataAccepted;
    // Underlying sans-I/O connection; lock the endpoint before using it directly.
    property Connection: TQuicConnection read FConnection;
    // Peer address.
    property Address: TNetworkAddress read FAddress;
    // Peer UDP port.
    property Port: Word read FPort;
  end;

  // Blocking stream on top of a QUIC stream; the caller owns it.
  TQuicStreamAdapter = class(TStream)
  private
    FConnection: TQuicEndpointConnection;
    FId: QWord;
    FTimeout: Integer;
    function GetIsEarlyData: Boolean;
  public
    // Wraps stream aId of aConnection.
    constructor Create(aConnection: TQuicEndpointConnection; aId: QWord);
    // Reads up to Count bytes; blocks until data arrives; 0 at the end of the stream.
    // Raises EQuicStreamError on reset, connection loss or timeout.
    function Read(var Buffer; Count: Longint): Longint; override;
    // Queues Count bytes; blocks while too much data is unacknowledged. Raises EQuicStreamError when closed.
    function Write(const Buffer; Count: Longint): Longint; override;
    // Ends the sending direction with a FIN.
    procedure Finish;
    // Abandons the sending direction with RESET_STREAM.
    procedure Reset(aErrorCode: QWord);
    // Stream identifier.
    property StreamId: QWord read FId;
    // Timeout of blocking operations in milliseconds.
    property Timeout: Integer read FTimeout write FTimeout;
    // True when the peer opened the stream with 0-RTT data.
    property IsEarlyData: Boolean read GetIsEarlyData;
  end;

  // UDP socket with a worker thread serving QUIC connections.
  TQuicEndpoint = class
  private
    FSocket: TFPSocket;
    FOpen: Boolean;
    FLock: TCriticalSection;
    FSignal: PRTLEvent;
    FThread: TThread;
    FFactory: TQuicOpenSSLCipherFactory;
    FContext: TQuicOpenSSLContext;
    FTLSConfig: TQuicTLSConfig;
    FParams: TQuicTransportParams;
    FIsServer: Boolean;
    FConnections: TFPList;
    FAccept: TFPList;
    FRoutes: TFPDataHashTable;
    FRetryKey: TBytes;
    FHandshakes: Integer;
    FMaxConnections: Integer;
    FMaxHandshakes: Integer;
    FRetryThreshold: Integer;
    FGso: Boolean;
    function GetPort: Word;
    procedure SendDatagram(const aAddress: TNetworkAddress; aPort: Word; aData: PByte; aLength: Integer);
    procedure AbortAll(const aReason: String);
    procedure SendBatch(aConnection: TQuicEndpointConnection; const aBatch: TBytes; aSegmentSize: Integer);
    function GetConnectionCount: Integer;
    function RetryMac(const aData, aScid: TBytes; const aAddress: TNetworkAddress; aPort: Word): TBytes;
    function CheckRetryToken(const aToken, aScid: TBytes; const aAddress: TNetworkAddress; aPort: Word;
      out aOriginalDcid: TBytes): Boolean;
    procedure SendRetry(const aHeader: TQuicPacketHeader; const aAddress: TNetworkAddress; aPort: Word);
    procedure AcceptInitial(const aHeader: TQuicPacketHeader; const aData: TBytes; const aAddress: TNetworkAddress;
      aPort: Word);
    procedure Deliver(aConnection: TQuicEndpointConnection; const aData: TBytes);
  protected
    // Opens the UDP socket on aAddress:aPort and starts the worker thread; IPv6 sockets also accept IPv4.
    procedure Open(const aAddress: String; aPort: Word);
    // Receives datagrams for up to aWait milliseconds and handles timers. When it raises an exception, the worker
    // thread closes all connections with that error and continues.
    procedure Poll(aWait: Integer); virtual;
    // Routes a received datagram to its connection.
    procedure RouteDatagram(const aData: TBytes; const aAddress: TNetworkAddress; aPort: Word);
  public
    // Creates an endpoint; raises EQuicOpenSSL when the TLS configuration is invalid.
    constructor Create(aIsServer: Boolean; const aTLS: TQuicTLSConfig; const aParams: TQuicTransportParams);
    // Stops the worker thread and frees all connections that were not handed to the caller.
    destructor Destroy; override;
    // Acquires the endpoint lock.
    procedure Lock;
    // Releases the endpoint lock.
    procedure Unlock;
    // Sends all datagrams aConnection has ready; call with the lock held.
    procedure Flush(aConnection: TQuicEndpointConnection);
    // Waits for the worker thread to signal progress, at most a few milliseconds.
    procedure WaitSignal;
    // Current time in microseconds.
    function Now: QWord;
    // Local UDP port.
    property Port: Word read GetPort;
    // Number of connections of the endpoint.
    property ConnectionCount: Integer read GetConnectionCount;
  end;

  // QUIC client with one connection.
  TQuicClient = class(TQuicEndpoint)
  private
    FConnection: TQuicEndpointConnection;
    FSessionCache: TQuicSessionCache;
  public
    // Creates a client with the given TLS settings and transport parameters.
    constructor Create(const aTLS: TQuicTLSConfig; const aParams: TQuicTransportParams);
    // Frees the connection and the endpoint.
    destructor Destroy; override;
    // Connects to an IPv4 or IPv6 address literal; nil when the handshake fails or times out.
    // With aEarly and a ticket allowing 0-RTT in SessionCache, returns before the handshake completes:
    // streams opened then send 0-RTT data.
    function Connect(const aAddress: String; aPort: Word; aTimeout: Integer;
      aEarly: Boolean = False): TQuicEndpointConnection;
    // Session tickets used to resume connections, owned by the caller; nil for full handshakes only.
    property SessionCache: TQuicSessionCache read FSessionCache write FSessionCache;
  end;

  // QUIC server accepting connections on one UDP socket.
  TQuicServer = class(TQuicEndpoint)
  public
    // Creates a server with the given TLS settings and transport parameters.
    constructor Create(const aTLS: TQuicTLSConfig; const aParams: TQuicTransportParams);
    // Largest number of connections; new clients beyond it are ignored. 0 means no limit.
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    // Largest number of connections whose handshake is not complete; new clients beyond it are ignored.
    // 0 means no limit.
    property MaxHandshakes: Integer read FMaxHandshakes write FMaxHandshakes;
    // Number of incomplete handshakes from which new clients must first answer a Retry packet.
    // 0 sends a Retry to every client; a negative value never sends one.
    property RetryThreshold: Integer read FRetryThreshold write FRetryThreshold;
    // Starts serving on aAddress (IPv4 or IPv6 literal; '::' for both) and aPort (0: any free port).
    procedure Listen(const aAddress: String; aPort: Word);
    // Waits for a connection whose handshake completed or that has 0-RTT stream data; the caller owns it.
    // nil on timeout.
    function Accept(aTimeout: Integer): TQuicEndpointConnection;
  end;

// Transport parameters suitable for most applications.
function QuicDefaultEndpointParams: TQuicTransportParams;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  FpQuic.Crypto;
{$ELSE FPC_DOTTEDUNITS}
uses
  FpQuic.Crypto;
{$ENDIF FPC_DOTTEDUNITS}

const
  ReceiveBufferSize = 65536;
  MaxBufferedSend = 1048576;
  PollInterval = 20;
  RetryTokenLifetime = 10000;
  RetryMacLength = 16;
  RouteTableSize = 4093;

type
  // Worker thread of an endpoint.
  TQuicEndpointThread = class(TThread)
  private
    FEndpoint: TQuicEndpoint;
  protected
    procedure Execute; override;
  public
    // Creates and starts the thread polling aEndpoint.
    constructor Create(aEndpoint: TQuicEndpoint);
  end;

function QuicDefaultEndpointParams: TQuicTransportParams;

begin
  Result:=QuicDefaultTransportParams;
  Result.MaxIdleTimeout:=30000;
  Result.InitialMaxData:=4194304;
  Result.InitialMaxStreamDataBidiLocal:=1048576;
  Result.InitialMaxStreamDataBidiRemote:=1048576;
  Result.InitialMaxStreamDataUni:=1048576;
  Result.InitialMaxStreamsBidi:=100;
  Result.InitialMaxStreamsUni:=100;
end;


// Returns the end of a timeout of aTimeout milliseconds.
function Deadline(aTimeout: Integer): QWord;

begin
  Result:=GetTickCount64+QWord(aTimeout);
end;


// Key of connection ID aCid in the routing table.
function RouteKey(const aCid: TBytes): AnsiString;

begin
  SetString(Result,PAnsiChar(aCid),Length(aCid));
end;


{ EQuicStreamError }

constructor EQuicStreamError.CreateReset(aCode: QWord);

begin
  inherited Create('Stream reset by the peer with error '+IntToStr(aCode));
  FIsReset:=True;
  FResetCode:=aCode;
end;


{ TQuicSessionCache }

constructor TQuicSessionCache.Create;

begin
  FLock:=TCriticalSection.Create;
end;


destructor TQuicSessionCache.Destroy;

begin
  FLock.Free;
  inherited Destroy;
end;


procedure TQuicSessionCache.Store(const aKey: String; const aTicket: TBytes);

var
  lTicket: TBytes;
  I: Integer;

begin
  FLock.Enter;
  try
    I:=High(FKeys);
    while (I>=0) and (FKeys[I]<>aKey) do
      Dec(I);
    if I<0 then
      begin
      Insert(aKey,FKeys,Length(FKeys));
      Insert(aTicket,FTickets,Length(FTickets));
      end
    else
      FTickets[I]:=aTicket;
  finally
    FLock.Leave;
  end;
  lTicket:=aTicket;
  if Assigned(FOnStore) then
    FOnStore(Self,aKey,lTicket);
end;


function TQuicSessionCache.Load(const aKey: String): TBytes;

var
  I: Integer;

begin
  Result:=nil;
  FLock.Enter;
  try
    for I:=0 to High(FKeys) do
      if FKeys[I]=aKey then
        Exit(FTickets[I]);
  finally
    FLock.Leave;
  end;
  if Assigned(FOnLoad) then
    FOnLoad(Self,aKey,Result);
end;


{ TQuicEndpointThread }

constructor TQuicEndpointThread.Create(aEndpoint: TQuicEndpoint);

begin
  FEndpoint:=aEndpoint;
  inherited Create(False);
end;


procedure TQuicEndpointThread.Execute;

begin
  while not Terminated do
    try
      FEndpoint.Poll(PollInterval);
    except
      on E: Exception do
        FEndpoint.AbortAll(E.Message);
    end;
end;


{ TQuicEndpointConnection }

constructor TQuicEndpointConnection.Create(aEndpoint: TQuicEndpoint; const aAddress: TNetworkAddress; aPort: Word;
  const aOriginalDcid: TBytes; const aRetryScid: TBytes);

var
  lConfig: TQuicConnectionConfig;

begin
  FEndpoint:=aEndpoint;
  FAddress:=aAddress;
  FPort:=aPort;
  FTLS:=TQuicOpenSSLTLS.Create(aEndpoint.FContext);
  lConfig:=Default(TQuicConnectionConfig);
  lConfig.IsServer:=aEndpoint.FIsServer;
  lConfig.TLS:=FTLS;
  lConfig.CipherFactory:=aEndpoint.FFactory;
  lConfig.Params:=aEndpoint.FParams;
  lConfig.Events:=Self;
  if (not aEndpoint.FIsServer) and (TQuicClient(aEndpoint).FSessionCache<>nil) then
    begin
    FTicketKey:=aEndpoint.FTLSConfig.ServerName+'|'+aAddress.Address+'|'+IntToStr(aPort);
    lConfig.Ticket:=TQuicClient(aEndpoint).FSessionCache.Load(FTicketKey);
    end;
  if aEndpoint.FIsServer then
    begin
    FConnection:=TQuicConnection.CreateServer(lConfig,aOriginalDcid,aEndpoint.Now,aRetryScid);
    FInHandshake:=True;
    Inc(aEndpoint.FHandshakes);
    end
  else
    FConnection:=TQuicConnection.CreateClient(lConfig,aEndpoint.Now);
  aEndpoint.FConnections.Add(Self);
end;


destructor TQuicEndpointConnection.Destroy;

begin
  FEndpoint.Lock;
  try
    if FConnection.State<qcsClosing then
      begin
      FConnection.Close(0,'',FEndpoint.Now);
      FEndpoint.Flush(Self);
      end;
    EndHandshake;
    RemoveRoutes;
    FEndpoint.FConnections.Remove(Self);
    FEndpoint.FAccept.Remove(Self);
    FConnection.Free;
    FTLS.Free;
  finally
    FEndpoint.Unlock;
  end;
  inherited Destroy;
end;


function TQuicEndpointConnection.GetState: TQuicConnectionState;

begin
  FEndpoint.Lock;
  try
    Result:=FConnection.State;
  finally
    FEndpoint.Unlock;
  end;
end;


function TQuicEndpointConnection.GetEarlyDataAccepted: Boolean;

begin
  FEndpoint.Lock;
  try
    Result:=FConnection.EarlyDataAccepted;
  finally
    FEndpoint.Unlock;
  end;
end;


procedure TQuicEndpointConnection.QueueAccept;

begin
  if FEndpoint.FIsServer and not FQueued then
    begin
    FQueued:=True;
    FEndpoint.FAccept.Add(Self);
    end;
end;


procedure TQuicEndpointConnection.EndHandshake;

begin
  if not FInHandshake then
    Exit;
  FInHandshake:=False;
  Dec(FEndpoint.FHandshakes);
end;


procedure TQuicEndpointConnection.AddRoute(const aCid: TBytes);

begin
  FEndpoint.FRoutes.Add(RouteKey(aCid),Self);
  Insert(aCid,FRoutes,Length(FRoutes));
end;


procedure TQuicEndpointConnection.RemoveRoute(aIndex: Integer);

begin
  FEndpoint.FRoutes.Delete(RouteKey(FRoutes[aIndex]));
  Delete(FRoutes,aIndex,1);
end;


procedure TQuicEndpointConnection.UpdateRoutes;

var
  lCids: TQuicLocalCidArray;
  lSame: Boolean;
  I: Integer;

begin
  lCids:=FConnection.LocalCids;
  lSame:=Length(FRoutes)=Length(lCids)+1;
  I:=0;
  while lSame and (I<Length(lCids)) do
    begin
    lSame:=QuicBytesEqual(FRoutes[I+1],lCids[I].Cid);
    Inc(I);
    end;
  if lSame then
    Exit;
  for I:=High(FRoutes) downto 1 do
    RemoveRoute(I);
  for I:=0 to High(lCids) do
    AddRoute(lCids[I].Cid);
end;


procedure TQuicEndpointConnection.RemoveRoutes;

var
  I: Integer;

begin
  for I:=High(FRoutes) downto 0 do
    RemoveRoute(I);
end;


procedure TQuicEndpointConnection.QuicHandshakeComplete(aConnection: TQuicConnection);

begin
  EndHandshake;
  QueueAccept;
end;


procedure TQuicEndpointConnection.QuicStreamData(aConnection: TQuicConnection; aStreamId: QWord);

begin
  if aConnection.StreamIsEarly(aStreamId) then
    QueueAccept;
  RTLEventSetEvent(FEndpoint.FSignal);
end;


procedure TQuicEndpointConnection.QuicSessionTicket(aConnection: TQuicConnection; const aTicket: TBytes);

begin
  if FTicketKey<>'' then
    TQuicClient(FEndpoint).FSessionCache.Store(FTicketKey,aTicket);
end;


procedure TQuicEndpointConnection.QuicConnectionClosed(aConnection: TQuicConnection);

begin
  RTLEventSetEvent(FEndpoint.FSignal);
end;


function TQuicEndpointConnection.WaitHandshake(aTimeout: Integer): Boolean;

var
  lEnd: QWord;
  lDone: Boolean;

begin
  lEnd:=Deadline(aTimeout);
  repeat
    FEndpoint.Lock;
    try
      Result:=FConnection.HandshakeComplete and (FConnection.State=qcsEstablished);
      lDone:=Result or (FConnection.State>=qcsClosing);
    finally
      FEndpoint.Unlock;
    end;
    if lDone then
      Exit;
    FEndpoint.WaitSignal;
  until GetTickCount64>=lEnd;
end;


function TQuicEndpointConnection.OpenStream(aBidi: Boolean): TQuicStreamAdapter;

var
  lId: QWord;
  lOk: Boolean;

begin
  FEndpoint.Lock;
  try
    lOk:=FConnection.OpenStream(aBidi,lId);
  finally
    FEndpoint.Unlock;
  end;
  if lOk then
    Result:=TQuicStreamAdapter.Create(Self,lId)
  else
    Result:=nil;
end;


function TQuicEndpointConnection.AcceptStream(aTimeout: Integer): TQuicStreamAdapter;

var
  lEnd, lId: QWord;
  lFound, lClosed: Boolean;

begin
  lEnd:=Deadline(aTimeout);
  repeat
    FEndpoint.Lock;
    try
      lFound:=FConnection.AcceptStream(lId);
      lClosed:=FConnection.State>=qcsClosing;
    finally
      FEndpoint.Unlock;
    end;
    if lFound then
      Exit(TQuicStreamAdapter.Create(Self,lId));
    if lClosed then
      Break;
    FEndpoint.WaitSignal;
  until GetTickCount64>=lEnd;
  Result:=nil;
end;


procedure TQuicEndpointConnection.Close(aErrorCode: QWord; const aReason: String);

begin
  FEndpoint.Lock;
  try
    FConnection.Close(aErrorCode,aReason,FEndpoint.Now);
    FEndpoint.Flush(Self);
  finally
    FEndpoint.Unlock;
  end;
end;


function TQuicEndpointConnection.WaitClosed(aTimeout: Integer): Boolean;

var
  lEnd: QWord;

begin
  lEnd:=Deadline(aTimeout);
  repeat
    Result:=State=qcsClosed;
    if Result then
      Exit;
    FEndpoint.WaitSignal;
  until GetTickCount64>=lEnd;
end;


function TQuicEndpointConnection.Alpn: AnsiString;

begin
  FEndpoint.Lock;
  try
    Result:=FConnection.Alpn;
  finally
    FEndpoint.Unlock;
  end;
end;


{ TQuicStreamAdapter }

constructor TQuicStreamAdapter.Create(aConnection: TQuicEndpointConnection; aId: QWord);

begin
  FConnection:=aConnection;
  FId:=aId;
  FTimeout:=30000;
end;


function TQuicStreamAdapter.GetIsEarlyData: Boolean;

begin
  FConnection.FEndpoint.Lock;
  try
    Result:=FConnection.FConnection.StreamIsEarly(FId);
  finally
    FConnection.FEndpoint.Unlock;
  end;
end;


function TQuicStreamAdapter.Read(var Buffer; Count: Longint): Longint;

var
  lEndpoint: TQuicEndpoint;
  lConnection: TQuicConnection;
  lEnd, lCode: QWord;
  lReset, lClosed: Boolean;

begin
  lEndpoint:=FConnection.FEndpoint;
  lConnection:=FConnection.FConnection;
  lEnd:=Deadline(FTimeout);
  repeat
    lEndpoint.Lock;
    try
      Result:=lConnection.ReadStream(FId,@Buffer,Count);
      lEndpoint.Flush(FConnection);
      lReset:=(Result=0) and lConnection.StreamReset(FId,lCode);
      lClosed:=(Result=0) and (lConnection.State>=qcsClosing);
      if not lReset and not lClosed and ((Result>0) or lConnection.StreamFinished(FId)) then
        Exit;
    finally
      lEndpoint.Unlock;
    end;
    if lReset then
      raise EQuicStreamError.CreateReset(lCode);
    if lClosed then
      raise EQuicStreamError.Create('Connection closed');
    lEndpoint.WaitSignal;
  until GetTickCount64>=lEnd;
  raise EQuicStreamError.Create('Read timeout');
end;


function TQuicStreamAdapter.Write(const Buffer; Count: Longint): Longint;

var
  lEndpoint: TQuicEndpoint;
  lConnection: TQuicConnection;
  lEnd: QWord;
  lOk, lFull: Boolean;

begin
  lEndpoint:=FConnection.FEndpoint;
  lConnection:=FConnection.FConnection;
  lEndpoint.Lock;
  try
    lOk:=lConnection.WriteStream(FId,QuicBytesOf(@Buffer,Count));
    lEndpoint.Flush(FConnection);
  finally
    lEndpoint.Unlock;
  end;
  if not lOk then
    raise EQuicStreamError.Create('Cannot write to the stream');
  Result:=Count;
  lEnd:=Deadline(FTimeout);
  repeat
    lEndpoint.Lock;
    try
      lFull:=(lConnection.StreamBuffered(FId)>MaxBufferedSend) and (lConnection.State=qcsEstablished);
    finally
      lEndpoint.Unlock;
    end;
    if not lFull then
      Exit;
    lEndpoint.WaitSignal;
  until GetTickCount64>=lEnd;
  raise EQuicStreamError.Create('Write timeout');
end;


procedure TQuicStreamAdapter.Finish;

var
  lEndpoint: TQuicEndpoint;

begin
  lEndpoint:=FConnection.FEndpoint;
  lEndpoint.Lock;
  try
    FConnection.FConnection.WriteStream(FId,nil,True);
    lEndpoint.Flush(FConnection);
  finally
    lEndpoint.Unlock;
  end;
end;


procedure TQuicStreamAdapter.Reset(aErrorCode: QWord);

var
  lEndpoint: TQuicEndpoint;

begin
  lEndpoint:=FConnection.FEndpoint;
  lEndpoint.Lock;
  try
    FConnection.FConnection.ResetStream(FId,aErrorCode);
    lEndpoint.Flush(FConnection);
  finally
    lEndpoint.Unlock;
  end;
end;


{ TQuicEndpoint }

constructor TQuicEndpoint.Create(aIsServer: Boolean; const aTLS: TQuicTLSConfig; const aParams: TQuicTransportParams);

begin
  FIsServer:=aIsServer;
  FParams:=aParams;
  FTLSConfig:=aTLS;
  FLock:=TCriticalSection.Create;
  FSignal:=RTLEventCreate;
  FConnections:=TFPList.Create;
  FAccept:=TFPList.Create;
  FRoutes:=TFPDataHashTable.CreateWith(RouteTableSize,@RSHash);
  FRetryKey:=QuicRandomBytes(32);
  FFactory:=TQuicOpenSSLCipherFactory.Create;
  FContext:=TQuicOpenSSLContext.Create(aIsServer,aTLS);
end;


destructor TQuicEndpoint.Destroy;

begin
  if FThread<>nil then
    begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
    end;
  while FConnections.Count>0 do
    TQuicEndpointConnection(FConnections[0]).Free;
  if FOpen then
    CloseSocket(FSocket);
  FAccept.Free;
  FConnections.Free;
  FRoutes.Free;
  FContext.Free;
  FFactory.Free;
  QuicWipe(FRetryKey);
  RTLEventDestroy(FSignal);
  FLock.Free;
  inherited Destroy;
end;


procedure TQuicEndpoint.Lock;

begin
  FLock.Enter;
end;


procedure TQuicEndpoint.Unlock;

begin
  FLock.Leave;
end;


function TQuicEndpoint.Now: QWord;

begin
  Result:=GetTickCount64*1000;
end;


function TQuicEndpoint.GetConnectionCount: Integer;

begin
  Lock;
  try
    Result:=FConnections.Count;
  finally
    Unlock;
  end;
end;


function TQuicEndpoint.GetPort: Word;

begin
  Result:=LocalEndpoint(FSocket).Second;
end;


procedure TQuicEndpoint.Open(const aAddress: String; aPort: Word);

begin
  if isIPv4Address(aAddress) then
    FSocket:=UDPSocket(stIPv4)
  else
    FSocket:=UDPSocket(stIPDualStack);
  FOpen:=True;
  Bind(FSocket,aAddress,aPort);
  SetNonBlocking(FSocket,True);
  FGso:=QuicGsoAvailable(FSocket);
  FThread:=TQuicEndpointThread.Create(Self);
end;


procedure TQuicEndpoint.WaitSignal;

begin
  RTLEventWaitFor(FSignal,PollInterval);
end;


// Sends one datagram; socket errors are ignored.
procedure TQuicEndpoint.SendDatagram(const aAddress: TNetworkAddress; aPort: Word; aData: PByte; aLength: Integer);

begin
  try
    SendTo(FSocket,aAddress,aPort,aData,aLength);
  except
    on ESocketCodeError do
      ;
  end;
end;


// Closes every connection at once with aReason and wakes the threads waiting for them.
procedure TQuicEndpoint.AbortAll(const aReason: String);

var
  I: Integer;

begin
  Lock;
  try
    for I:=0 to FConnections.Count-1 do
      TQuicEndpointConnection(FConnections[I]).FConnection.Abort(aReason);
  finally
    Unlock;
  end;
  RTLEventSetEvent(FSignal);
end;


procedure TQuicEndpoint.SendBatch(aConnection: TQuicEndpointConnection; const aBatch: TBytes; aSegmentSize: Integer);

var
  lOffset, lSize: Integer;
  lMultiple: Boolean;

begin
  lMultiple:=Length(aBatch)>aSegmentSize;
  if lMultiple and FGso then
    FGso:=QuicSendSegments(FSocket,aConnection.FAddress,aConnection.FPort,PByte(aBatch),Length(aBatch),aSegmentSize);
  if lMultiple and FGso then
    Exit;
  lOffset:=0;
  while lOffset<Length(aBatch) do
    begin
    lSize:=Min(aSegmentSize,Length(aBatch)-lOffset);
    SendDatagram(aConnection.FAddress,aConnection.FPort,@aBatch[lOffset],lSize);
    Inc(lOffset,lSize);
    end;
end;


procedure TQuicEndpoint.Flush(aConnection: TQuicEndpointConnection);

var
  lData, lBatch: TBytes;
  lSegmentSize, lCount, lMax: Integer;

begin
  lBatch:=nil;
  lCount:=0;
  lSegmentSize:=0;
  lMax:=0;
  while aConnection.FConnection.NextDatagram(Now,lData) do
    begin
    if (lCount>0) and ((Length(lData)>lSegmentSize) or (lCount>=lMax)) then
      begin
      SendBatch(aConnection,lBatch,lSegmentSize);
      lBatch:=nil;
      lCount:=0;
      end;
    if lCount=0 then
      begin
      lSegmentSize:=Length(lData);
      lMax:=Min(QuicMaxSegments,QuicMaxSegmentBytes div lSegmentSize);
      end;
    lBatch:=Concat(lBatch,lData);
    Inc(lCount);
    if Length(lData)<lSegmentSize then
      begin
      SendBatch(aConnection,lBatch,lSegmentSize);
      lBatch:=nil;
      lCount:=0;
      end;
    end;
  if lCount>0 then
    SendBatch(aConnection,lBatch,lSegmentSize);
  if FIsServer then
    aConnection.UpdateRoutes;
end;


function TQuicEndpoint.RetryMac(const aData, aScid: TBytes; const aAddress: TNetworkAddress; aPort: Word): TBytes;

var
  lWriter: TQuicWriter;

begin
  lWriter.Init;
  lWriter.WriteBytes(aData);
  lWriter.WriteVarBytes(aScid);
  lWriter.WriteVarBytes(TEncoding.UTF8.GetBytes(aAddress.Address));
  lWriter.WriteUInt16(aPort);
  Result:=Copy(QuicHkdfExtract(qcsAes128Gcm,FRetryKey,lWriter.ToBytes),0,RetryMacLength);
end;


function TQuicEndpoint.CheckRetryToken(const aToken, aScid: TBytes; const aAddress: TNetworkAddress; aPort: Word;
  out aOriginalDcid: TBytes): Boolean;

var
  lData: TBytes;
  lReader: TQuicReader;
  lExpiry: QWord;

begin
  aOriginalDcid:=nil;
  Result:=False;
  if Length(aToken)<=RetryMacLength then
    Exit;
  lData:=Copy(aToken,0,Length(aToken)-RetryMacLength);
  if not QuicBytesEqualConstantTime(RetryMac(lData,aScid,aAddress,aPort),Copy(aToken,Length(lData),RetryMacLength)) then
    Exit;
  lReader.InitBytes(lData);
  lExpiry:=lReader.ReadVarInt;
  aOriginalDcid:=lReader.ReadVarBytes;
  Result:=lExpiry>=GetTickCount64;
end;


procedure TQuicEndpoint.SendRetry(const aHeader: TQuicPacketHeader; const aAddress: TNetworkAddress; aPort: Word);

var
  lScid, lData, lPacket: TBytes;
  lWriter: TQuicWriter;

begin
  lScid:=QuicRandomBytes(QUIC_LOCAL_CID_LENGTH);
  lWriter.Init;
  lWriter.WriteVarInt(GetTickCount64+RetryTokenLifetime);
  lWriter.WriteVarBytes(aHeader.DestCid);
  lData:=lWriter.ToBytes;
  lPacket:=QuicBuildRetry(aHeader.SrcCid,lScid,Concat(lData,RetryMac(lData,lScid,aAddress,aPort)));
  lPacket:=Concat(lPacket,QuicRetryIntegrityTag(FFactory,aHeader.DestCid,lPacket));
  SendDatagram(aAddress,aPort,PByte(lPacket),Length(lPacket));
end;


procedure TQuicEndpoint.AcceptInitial(const aHeader: TQuicPacketHeader; const aData: TBytes;
  const aAddress: TNetworkAddress; aPort: Word);

var
  lConnection: TQuicEndpointConnection;
  lOriginalDcid, lRetryScid: TBytes;

begin
  if ((FMaxConnections>0) and (FConnections.Count>=FMaxConnections))
     or ((FMaxHandshakes>0) and (FHandshakes>=FMaxHandshakes)) then
    Exit;
  lOriginalDcid:=aHeader.DestCid;
  lRetryScid:=nil;
  if Length(aHeader.Token)>0 then
    begin
    if not CheckRetryToken(aHeader.Token,aHeader.DestCid,aAddress,aPort,lOriginalDcid) then
      Exit;
    lRetryScid:=aHeader.DestCid;
    end
  else if (FRetryThreshold>=0) and (FHandshakes>=FRetryThreshold) then
    begin
    SendRetry(aHeader,aAddress,aPort);
    Exit;
    end;
  lConnection:=TQuicEndpointConnection.Create(Self,aAddress,aPort,lOriginalDcid,lRetryScid);
  lConnection.AddRoute(aHeader.DestCid);
  lConnection.FConnection.ReceiveDatagram(aData,Now);
  if not lConnection.FConnection.InitialReceived then
    begin
    lConnection.FConnection.Close(0,'',Now);
    lConnection.Free;
    end
  else
    Flush(lConnection);
end;


procedure TQuicEndpoint.Deliver(aConnection: TQuicEndpointConnection; const aData: TBytes);

begin
  aConnection.FConnection.ReceiveDatagram(aData,Now);
  Flush(aConnection);
end;


procedure TQuicEndpoint.RouteDatagram(const aData: TBytes; const aAddress: TNetworkAddress; aPort: Word);

var
  lHeader: TQuicPacketHeader;
  lConnection: TQuicEndpointConnection;
  lVN: TBytes;

begin
  if not QuicParseHeader(PByte(aData),Length(aData),QUIC_LOCAL_CID_LENGTH,lHeader) then
    Exit;
  if not FIsServer then
    begin
    if FConnections.Count>0 then
      Deliver(TQuicEndpointConnection(FConnections[0]),aData);
    Exit;
    end;
  lConnection:=TQuicEndpointConnection(FRoutes[RouteKey(lHeader.DestCid)]);
  if lConnection<>nil then
    Deliver(lConnection,aData)
  else if Length(aData)<QUIC_MIN_INITIAL_DATAGRAM then
    Exit
  else if lHeader.PacketType=qptUnsupported then
    begin
    lVN:=QuicBuildVersionNegotiation(lHeader.SrcCid,lHeader.DestCid,[QUIC_VERSION_1]);
    SendDatagram(aAddress,aPort,PByte(lVN),Length(lVN));
    end
  else if lHeader.PacketType=qptInitial then
    AcceptInitial(lHeader,aData,aAddress,aPort);
end;


procedure TQuicEndpoint.Poll(aWait: Integer);

var
  lBuffer: TBytes;
  lAddr: TAddressUnion;
  lAddrLength: TSockLen;
  lSize: SizeInt;
  lAddress: TNetworkAddress;
  lPort: Word;
  lConnection: TQuicEndpointConnection;
  I: Integer;

begin
  lBuffer:=nil;
  SetLength(lBuffer,ReceiveBufferSize);
  if DataAvailable(FSocket,aWait) then
    repeat
      lAddrLength:=SizeOf(lAddr);
      lSize:=fprecvfrom(FSocket.FD,PByte(lBuffer),ReceiveBufferSize,0,psockaddr(@lAddr),@lAddrLength);
      if lSize<=0 then
        Break;
      ReadAddr(lAddr,FSocket.SocketType=stIPDualStack,lAddress,lPort);
      Lock;
      try
        RouteDatagram(Copy(lBuffer,0,lSize),lAddress,lPort);
      finally
        Unlock;
      end;
    until False;
  Lock;
  try
    for I:=FConnections.Count-1 downto 0 do
      begin
      lConnection:=TQuicEndpointConnection(FConnections[I]);
      if lConnection.FConnection.NextTimeout<=Now then
        lConnection.FConnection.HandleTimeout(Now);
      Flush(lConnection);
      if FIsServer and (lConnection.FConnection.State=qcsClosed) and not lConnection.FAccepted then
        lConnection.Free;
      end;
  finally
    Unlock;
  end;
  RTLEventSetEvent(FSignal);
end;


{ TQuicClient }

constructor TQuicClient.Create(const aTLS: TQuicTLSConfig; const aParams: TQuicTransportParams);

begin
  inherited Create(False,aTLS,aParams);
end;


destructor TQuicClient.Destroy;

begin
  if FThread<>nil then
    FThread.Terminate;
  FConnection.Free;
  inherited Destroy;
end;


function TQuicClient.Connect(const aAddress: String; aPort: Word; aTimeout: Integer;
  aEarly: Boolean): TQuicEndpointConnection;

var
  lEarly: Boolean;

begin
  if isIPv4Address(aAddress) then
    Open('0.0.0.0',0)
  else
    Open('::',0);
  Lock;
  try
    FConnection:=TQuicEndpointConnection.Create(Self,aAddress,aPort,nil);
    Flush(FConnection);
    lEarly:=aEarly and FConnection.FConnection.CanSendEarly;
  finally
    Unlock;
  end;
  if lEarly or FConnection.WaitHandshake(aTimeout) then
    Result:=FConnection
  else
    Result:=nil;
end;


{ TQuicServer }

constructor TQuicServer.Create(const aTLS: TQuicTLSConfig; const aParams: TQuicTransportParams);

begin
  inherited Create(True,aTLS,aParams);
  FMaxConnections:=10000;
  FMaxHandshakes:=1000;
  FRetryThreshold:=100;
end;


procedure TQuicServer.Listen(const aAddress: String; aPort: Word);

begin
  Open(aAddress,aPort);
end;


function TQuicServer.Accept(aTimeout: Integer): TQuicEndpointConnection;

var
  lEnd: QWord;

begin
  lEnd:=Deadline(aTimeout);
  repeat
    Lock;
    try
      if FAccept.Count>0 then
        begin
        Result:=TQuicEndpointConnection(FAccept[0]);
        FAccept.Delete(0);
        Result.FAccepted:=True;
        Exit;
        end;
    finally
      Unlock;
    end;
    WaitSignal;
  until GetTickCount64>=lEnd;
  Result:=nil;
end;


end.
