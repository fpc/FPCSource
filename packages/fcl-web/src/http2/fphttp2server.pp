{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2026 - by the Free Pascal development team

    HTTP/2 opt-in server component

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fphttp2server;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Net.Ssockets, System.Hash.Base64, FpWeb.Http.Defs, FpWeb.Http.Server,
  FpWeb.Http2.Connection, FpWeb.Http2.Request, FpWeb.Http2.Consts, FpWeb.Http2.Frames, FpWeb.Uhpack;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, ssockets, base64, httpdefs, fphttpserver, fphttp2connection, fphttp2request, fphttp2consts, fphttp2frames, uhpack;
{$ENDIF FPC_DOTTEDUNITS}

Type
  EHTTP2Handler = class(Exception);

  {  Concurrency switch.
     h2tmThread (the default) runs theconnection's whole frame-read + dispatch loop on its OWN thread, serialized per connection.
     h2tmNone runs the connection INLINE on the calling thread (blocks until the connection closes) - (used by the unittests).
     h2tmThreadPool is RESERVED for a future per-stream-worker switch }
  TH2ThreadMode = (h2tmNone, h2tmThread, h2tmThreadPool);

  { Ingress variants:
     amPriorKnowledge: the connection performs the normal preface exchange over the seeded+socket input.
     amUpgradeH2C: the decoded HTTP2-Settings are injected as the initial peer SETTINGS via ApplyUpgradeSettings
     amALPN (TLS-ALPN ingress): like amPriorKnowledge it has a nil request and no settings }
  TH2AdoptMode = (amPriorKnowledge, amUpgradeH2C, amALPN);

  TCustomHTTP2Handler = class;

  // Iterator for the locked connection-list walk (mirror of the WebSocket
  // TConnectionIterator shape).
  TH2ConnectionIterator = procedure(aConn: TH2Connection; var aContinue: Boolean) of object;

  // Fired when a connection's run loop ends with an exception 
  TH2ErrorEvent = procedure(Sender: TObject; aConn: TH2Connection; aError: Exception) of object;

  { TH2ConnectionList - thread-safe live-connection registry.}

  TH2ConnectionList = class
  private
    FList: TFPList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TH2Connection);
    procedure Remove(Item: TH2Connection);
    procedure Clear;
    function Count: Integer;
    function LockList: TFPList;
    procedure UnlockList;
    function ForEach(aIterator: TH2ConnectionIterator): Boolean;
  end;

  { TH2ConnectionThread - runs ONE connection's frame loop on its own thread (the default). }

  TH2ConnectionThread = class(TThread)
  private
    FHandler: TCustomHTTP2Handler;
    FConnection: TH2Connection;
    FSocket: TStream;
    FOnDone: TNotifyEvent;
  public
    constructor CreateConnection(aHandler: TCustomHTTP2Handler;
      aConnection: TH2Connection; aSocket: TStream; const aOnDone: TNotifyEvent); virtual;
    procedure Execute; override;
    // Drop the connection+socket references once ConnectionDone has freed them
    procedure DetachConnection;
    property Connection: TH2Connection read FConnection;
    property Socket: TStream read FSocket;
  end;

  { TCustomHTTP2Handler - the opt-in HTTP/2 handler component. }

  TCustomHTTP2Handler = class(TComponent)
  private
    FActive: Boolean;
    FWebServer: TFPCustomHttpServer;
    FThreadMode: TH2ThreadMode;
    FConnections: TH2ConnectionList;
    // Worker-thread registry for the deterministic join.
    FThreads: TThreadList;
    FDispatcher: TH2Dispatcher;
    FOnError: TH2ErrorEvent;
    // The five configurable DoS limits, defaulted from DEFAULT_H2_LIMITS
    FMaxConcurrentStreams: Cardinal;
    FMaxFrameSize: Cardinal;
    FMaxHeaderListSize: Cardinal;
    FMaxContinuationBytes: Cardinal;
    FHPACKTableLimit: Cardinal;
    // The three flood/rapid-reset budgets, same lifecycle as the five limits above.
    FMaxResetStreams: Cardinal;
    FMaxPingFlood: Cardinal;
    FMaxSettingsFlood: Cardinal;
    // The WebServer.CertificateData.ALPNProtocols value captured at activation
    FSavedALPN: string;
    procedure SetWebServer(AValue: TFPCustomHttpServer);
    procedure SetThreadMode(AValue: TH2ThreadMode);
    procedure SetMaxConcurrentStreams(AValue: Cardinal);
    procedure SetMaxFrameSize(AValue: Cardinal);
    procedure SetMaxHeaderListSize(AValue: Cardinal);
    procedure SetMaxContinuationBytes(AValue: Cardinal);
    procedure SetHPACKTableLimit(AValue: Cardinal);
    procedure SetMaxResetStreams(AValue: Cardinal);
    procedure SetMaxPingFlood(AValue: Cardinal);
    procedure SetMaxSettingsFlood(AValue: Cardinal);
  protected
    procedure SetActive(const AValue: Boolean); virtual;
    function GetActive: Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Raise EHTTP2Handler if Active (used by the property setters).
    procedure CheckInactive;
    // Pass an unhandled connection-loop exception to OnError (best-effort log).
    procedure HandleError(aConn: TH2Connection; E: Exception);
    // The shared run body for inline (h2tmNone) and the connection thread:
    procedure RunConnection(aConn: TH2Connection);
    // Remove aConn from the list and free it
    procedure ConnectionDone(aConn: TH2Connection; aSocket: TStream; aFreeSocket: Boolean);
    // OnDone target for a TH2ConnectionThread (recovers connection + socket).
    procedure ThreadConnectionDone(Sender: TObject);
    // Graceful deactivate close walk
    procedure CloseConnections;
    // Deterministic worker join (Active:=False)
    procedure JoinThreads;
    // Close a connection's transport fd WITHOUT freeing the stream object
    procedure CloseTransport(aSocket: TStream); virtual;
    // Lingering close. Makes sure the error-path GOAWAY has been written
    procedure LingeringClose(aSocket: TStream);
    // fphttpserver prior-knowledge adopt callback.
    procedure DoAdoptPriorKnowledge(aConnection: TFPHTTPConnection;
      {%H-}aRequest: TFPHTTPConnectionRequest);
    // The single registered adopt callback, serving BOTH ingress shapes through the one FOnAdoptHTTP2 event.
    procedure DoAdoptHTTP2(aConnection: TFPHTTPConnection;
      aRequest: TFPHTTPConnectionRequest);
    // The ALPN branch: detection fired BEFORE any HTTP/1.1 parse
    procedure DoAdoptALPN(aConnection: TFPHTTPConnection;
      {%H-}aRequest: TFPHTTPConnectionRequest);
    // h2c Upgrade detection through the EXISTING CheckUpgrade
    procedure DoCheckH2CUpgrade(aRequest: TFPHTTPConnectionRequest; var aHandlesUpgrade: Boolean);
    // TUpgradeConnectionEvent takeover: write the raw `101 Switching Protocols` directly to the socket 
    procedure DoUpgradeH2C(aConnection: TFPHTTPConnection; aRequest: TFPHTTPConnectionRequest);
    // WebServer that owns the dispatch pipeline. Free-notification keeps the
    // reference from dangling.
    property WebServer: TFPCustomHttpServer read FWebServer write SetWebServer;
    // Concurrency switch; default h2tmThread (the connection's own thread).
    property ThreadMode: TH2ThreadMode read FThreadMode write SetThreadMode;
    // DoS limits (safe defaults from DEFAULT_H2_LIMITS; settable only
    // while not Active). Each is read into every adopted TH2Connection.
    property MaxConcurrentStreams: Cardinal read FMaxConcurrentStreams write SetMaxConcurrentStreams;
    property MaxFrameSize: Cardinal read FMaxFrameSize write SetMaxFrameSize;
    property MaxHeaderListSize: Cardinal read FMaxHeaderListSize write SetMaxHeaderListSize;
    property MaxContinuationBytes: Cardinal read FMaxContinuationBytes write SetMaxContinuationBytes;
    property HPACKTableLimit: Cardinal read FHPACKTableLimit write SetHPACKTableLimit;
    // Flood/rapid-reset budgets (safe defaults; settable only
    // while not Active). Each is read into every adopted TH2Connection.
    property MaxResetStreams: Cardinal read FMaxResetStreams write SetMaxResetStreams;
    property MaxPingFlood: Cardinal read FMaxPingFlood write SetMaxPingFlood;
    property MaxSettingsFlood: Cardinal read FMaxSettingsFlood write SetMaxSettingsFlood;
    // Active:=True validates WebServer; Active:=False drains the connections,
    // unregisters, and frees the dispatcher.
    property Active: Boolean read GetActive write SetActive;
    // Called when a connection's run loop ends with an exception.
    property OnError: TH2ErrorEvent read FOnError write FOnError;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Entry point: build a TH2Connection over aSocket, seed the residual
    // pre-read bytes, wire the completed-stream dispatch to Server.HandleRequest,
    procedure AdoptConnection(aSocket: TStream; aMode: TH2AdoptMode;
      aInitialRequest: TRequest; const aBufferedBytes: TBytes;
      const aClientSettings: TH2SettingsArray = nil);
    // Poll Count until it reaches 0 or the attempt budget is spent.
    function WaitForConnections(aMaxAttempts: Integer): Boolean;
    // Number of live connections (locked read).
    function ConnectionCount: Integer;
  end;

  { TFPHTTP2Handler - publishes the surface (mirror of TWebsocketUpgrader). }

  TFPHTTP2Handler = class(TCustomHTTP2Handler)
  published
    property Active;
    property WebServer;
    property ThreadMode;
    property OnError;
    // Published DoS limits (safe defaults; settable only while inactive).
    property MaxConcurrentStreams;
    property MaxFrameSize;
    property MaxHeaderListSize;
    property MaxContinuationBytes;
    property HPACKTableLimit;
    property MaxResetStreams;
    property MaxPingFlood;
    property MaxSettingsFlood;
  end;

const
  { RFC 7540 §3.2 - the exact `101 Switching Protocols` response the h2c handler
   writes RAW to the socket before handing off }
  H2C_101_RESPONSE =
    'HTTP/1.1 101 Switching Protocols'#13#10 +
    'Connection: Upgrade'#13#10 +
    'Upgrade: h2c'#13#10 +
    #13#10;

  // The ALPN offer the handler installs into CertificateData.ALPNProtocols
  // on Active (only when the list is empty): `h2` preferred, `http/1.1` fallback.
  H2_ALPN_PROTOCOLS = 'h2,http/1.1';

// Stateless base64url HTTP2-Settings decoder (RFC 7540 §3.2.1 / RFC 4648 §5)
function TryDecodeHTTP2Settings(aRequest: TFPHTTPConnectionRequest; out aSettings: TH2SettingsArray): Boolean;

implementation

resourcestring
  SErrWebserverNotAssigned = 'Webserver not assigned';
  SErrHandlerNotActive = 'HTTP/2 handler is not active';
  SErrHandlerActive = 'Operation not allowed while the HTTP/2 handler is active';
  SErrPriorKnowledgeNeedsNilRequest = 'amPriorKnowledge requires aInitialRequest = nil';
  SErrALPNNeedsNilRequest = 'amALPN requires aInitialRequest = nil';

function TryDecodeHTTP2Settings(aRequest: TFPHTTPConnectionRequest; out aSettings: TH2SettingsArray): Boolean;
var
  Raw, Std, Decoded: AnsiString;
  PadDeficit, I: Integer;
  Payload: TBytesStream;
  Hdr: TH2FrameHeader;
begin
  Result := False;
  aSettings := nil;
  // RFC 7540 §3.2.1: a server MUST NOT upgrade when HTTP2-Settings is absent.
  Raw := aRequest.GetFieldByName('HTTP2-Settings');
  if Raw = '' then
    Exit;
  // Validate the RFC 4648 §5 base64url alphabet up front: any character outside
  // [A-Za-z0-9-_] (padding '=' tolerated) means a non-base64 value -> no upgrade.
 
  for I := 1 to Length(Raw) do
    if not (Raw[I] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '=']) then
      Exit;
  // base64url -> standard base64: translate the two differing alphabet chars,
  // then restore the padding base64url omits (pad up to a multiple of 4).
  Std := Raw;
  for I := 1 to Length(Std) do
    case Std[I] of
      '-': Std[I] := '+';
      '_': Std[I] := '/';
    end;
  PadDeficit := Length(Std) mod 4;
  if PadDeficit > 0 then
    Std := Std + StringOfChar('=', 4 - PadDeficit);
  Decoded := DecodeStringBase64(Std, False);  // MIME-tolerant
  // The decoded bytes are a SETTINGS frame PAYLOAD (id/value pairs), not a framed
  // frame: it must be a non-negative multiple of 6, else malformed -> no upgrade.
  if (Length(Decoded) mod 6) <> 0 then
    Exit;
  Payload := TBytesStream.Create;
  try
    if Length(Decoded) > 0 then
      Payload.WriteBuffer(Decoded[1], Length(Decoded));   // AnsiString is 1-based
    Payload.Position := 0;
    // Reuse the wire parser (and its Len mod 6 guard) over a synthesized header
    // rather than hand-rolling the id/value walk.
    Hdr.FrameType := Ord(ftSettings);
    Hdr.Flags := 0;
    Hdr.StreamID := 0;
    Hdr.Len := Cardinal(Length(Decoded));
    try
      aSettings := ReadSettingsFrame(Payload, Hdr);
      Result := True;
    except
      on EH2FrameError do
      begin
        aSettings := nil;
        Result := False;
      end;
    end;
  finally
    Payload.Free;
  end;
end;

{ TH2ConnectionList }

constructor TH2ConnectionList.Create;
begin
  FList := TFPList.Create;
  InitCriticalSection(FLock);
end;

destructor TH2ConnectionList.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DoneCriticalSection(FLock);
  end;
end;

procedure TH2ConnectionList.Add(Item: TH2Connection);
begin
  LockList;
  try
    if FList.IndexOf(Item) = -1 then
      FList.Add(Item);
  finally
    UnlockList;
  end;
end;

procedure TH2ConnectionList.Remove(Item: TH2Connection);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TH2ConnectionList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TH2ConnectionList.Count: Integer;
begin
  LockList;
  try
    Result := FList.Count;
  finally
    UnlockList;
  end;
end;

function TH2ConnectionList.LockList: TFPList;
begin
  Result := FList;
  System.EnterCriticalSection(FLock);
end;

procedure TH2ConnectionList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

function TH2ConnectionList.ForEach(aIterator: TH2ConnectionIterator): Boolean;
var
  L: TFPList;
  I: Integer;
begin
  Result := True;
  L := LockList;
  try
    I := 0;
    while Result and (I < L.Count) do
    begin
      aIterator(TH2Connection(L[I]), Result);
      Inc(I);
    end;
  finally
    UnlockList;
  end;
end;

{ TH2ConnectionThread }

constructor TH2ConnectionThread.CreateConnection(aHandler: TCustomHTTP2Handler;
  aConnection: TH2Connection; aSocket: TStream; const aOnDone: TNotifyEvent);
begin
  FHandler := aHandler;
  FConnection := aConnection;
  FSocket := aSocket;
  FOnDone := aOnDone;
  // FreeOnTerminate := False so the handler can deterministically join
  // (WaitFor) each worker at teardown and own its Free
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TH2ConnectionThread.Execute;
begin
  try
    FHandler.RunConnection(FConnection);
  finally
    // Hands the connection + socket back to the handler for removal + free.
    if Assigned(FOnDone) then
      FOnDone(Self);
  end;
end;

procedure TH2ConnectionThread.DetachConnection;
begin
  // The handler has taken ownership of (and freed) the connection + socket
  FConnection := nil;
  FSocket := nil;
end;

{ TCustomHTTP2Handler }

constructor TCustomHTTP2Handler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnections := TH2ConnectionList.Create;
  // Worker-thread registry for the deterministic teardown join.
  FThreads := TThreadList.Create;
  // Default: each adopted connection runs on its own thread.
  FThreadMode := h2tmThread;
  // Start from the canonical safe defaults (single source of truth in
  // fphttp2consts.pp). The operator may override any of these while not Active.
  FMaxConcurrentStreams := DEFAULT_H2_LIMITS.MaxConcurrentStreams;
  FMaxFrameSize         := DEFAULT_H2_LIMITS.MaxFrameSize;
  FMaxHeaderListSize    := DEFAULT_H2_LIMITS.MaxHeaderListSize;
  FMaxContinuationBytes := DEFAULT_H2_LIMITS.MaxContinuationBytes;
  FHPACKTableLimit      := DEFAULT_H2_LIMITS.HPACKTableLimit;
  FMaxResetStreams      := DEFAULT_H2_LIMITS.MaxResetStreams;
  FMaxPingFlood         := DEFAULT_H2_LIMITS.MaxPingFlood;
  FMaxSettingsFlood     := DEFAULT_H2_LIMITS.MaxSettingsFlood;
end;

destructor TCustomHTTP2Handler.Destroy;
begin
  // Active := False has already joined every worker thread, so no
  // thread can touch FConnections after this returns.
  Active := False;
  FreeAndNil(FConnections);
  // FThreads is empty after the join loop cleared it; free the (now-empty) registry.
  FreeAndNil(FThreads);
  inherited Destroy;
end;

function TCustomHTTP2Handler.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TCustomHTTP2Handler.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  if AValue then
  begin
    if not Assigned(FWebServer) then
      raise EHTTP2Handler.Create(SErrWebserverNotAssigned);
    // The single shared dispatcher.
    FDispatcher := TH2Dispatcher.Create(FWebServer);
    FActive := True;
    { Register the ingress callback last: the server routes
      both PRI * HTTP/2.0 start lines AND TLS-ALPN-negotiated 'h2' connections to
      DoAdoptHTTP2, which dispatches to the prior-knowledge or the ALPN arm. }
    FWebServer.RegisterHTTP2Handler(@DoAdoptHTTP2);
    // The complementary ingress: the header-based `Upgrade: h2c`
    FWebServer.RegisterUpdateHandler(H2C_UPGRADE_NAME, @DoCheckH2CUpgrade, @DoUpgradeH2C);
    // Advertise h2 via ALPN so a browser/curl can select it/
    FSavedALPN := FWebServer.CertificateData.ALPNProtocols;
    if FWebServer.CertificateData.ALPNProtocols = '' then
      FWebServer.CertificateData.ALPNProtocols := H2_ALPN_PROTOCOLS;
  end
  else
  begin
    { Unregister ingress FIRST so no new connection is adopted while we drain.
      FWebServer may already be nil if the server was freed first (free-notify).}
    if Assigned(FWebServer) then
    begin
      FWebServer.UnRegisterHTTP2Handler;
      // Unregister the h2c update handler too
      FWebServer.UnRegisterUpdateHandler(H2C_UPGRADE_NAME);
      // Restore the ALPN list to its pre-activation value.
      FWebServer.CertificateData.ALPNProtocols := FSavedALPN;
    end;
    // Graceful GOAWAY + socket-fd close per live connection.
    CloseConnections;
    // Deterministically JOIN every worker
    JoinThreads;
    // Safety net: the bounded poll. After the join ConnectionCount is 0, so this returns at once.
    WaitForConnections(10);
    FreeAndNil(FDispatcher);
    FActive := False;
  end;
end;

procedure TCustomHTTP2Handler.JoinThreads;
var
  L: TList;
  I: Integer;
  Snapshot: array of TH2ConnectionThread;
begin
  { Snapshot under the registry lock - copy the live thread references into a local
    array, then release the lock BEFORE the (potentially blocking) WaitFor calls so
    a worker's OnDone is never blocked on the registry lock while we wait on it. }
  L := FThreads.LockList;
  try
    SetLength(Snapshot, L.Count);
    for I := 0 to L.Count - 1 do
      Snapshot[I] := TH2ConnectionThread(L[I]);
  finally
    FThreads.UnlockList;
  end;
  { Deterministic join: WaitFor blocks until the worker's Execute fully returns
   (OnDone already ran), then the handler owns the Free (no self-free, no double-
   free - ConnectionDone owns conn+socket, the handler owns the thread object). }
  for I := 0 to High(Snapshot) do
  begin
    Snapshot[I].WaitFor;
    Snapshot[I].Free;
  end;
  // Every tracked worker is joined and freed; empty the registry.
  FThreads.Clear;
end;

procedure TCustomHTTP2Handler.SetWebServer(AValue: TFPCustomHttpServer);
begin
  if FWebServer = AValue then Exit;
  CheckInactive;
  if Assigned(FWebServer) then
    FWebServer.RemoveFreeNotification(Self);
  FWebServer := AValue;
  if Assigned(FWebServer) then
    FWebServer.FreeNotification(Self);
end;

procedure TCustomHTTP2Handler.SetThreadMode(AValue: TH2ThreadMode);
begin
  if FThreadMode = AValue then Exit;
  CheckInactive;
  FThreadMode := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxConcurrentStreams(AValue: Cardinal);
begin
  if FMaxConcurrentStreams = AValue then Exit;
  CheckInactive;
  FMaxConcurrentStreams := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxFrameSize(AValue: Cardinal);
begin
  if FMaxFrameSize = AValue then Exit;
  CheckInactive;
  FMaxFrameSize := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxHeaderListSize(AValue: Cardinal);
begin
  if FMaxHeaderListSize = AValue then Exit;
  CheckInactive;
  FMaxHeaderListSize := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxContinuationBytes(AValue: Cardinal);
begin
  if FMaxContinuationBytes = AValue then Exit;
  CheckInactive;
  FMaxContinuationBytes := AValue;
end;

procedure TCustomHTTP2Handler.SetHPACKTableLimit(AValue: Cardinal);
begin
  if FHPACKTableLimit = AValue then Exit;
  CheckInactive;
  FHPACKTableLimit := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxResetStreams(AValue: Cardinal);
begin
  if FMaxResetStreams = AValue then Exit;
  CheckInactive;
  FMaxResetStreams := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxPingFlood(AValue: Cardinal);
begin
  if FMaxPingFlood = AValue then Exit;
  CheckInactive;
  FMaxPingFlood := AValue;
end;

procedure TCustomHTTP2Handler.SetMaxSettingsFlood(AValue: Cardinal);
begin
  if FMaxSettingsFlood = AValue then Exit;
  CheckInactive;
  FMaxSettingsFlood := AValue;
end;

procedure TCustomHTTP2Handler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FWebServer) then
    FWebServer := Nil;
end;

procedure TCustomHTTP2Handler.CheckInactive;
begin
  if FActive then
    raise EHTTP2Handler.Create(SErrHandlerActive);
end;

procedure TCustomHTTP2Handler.HandleError(aConn: TH2Connection; E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self, aConn, E);
end;

procedure TCustomHTTP2Handler.RunConnection(aConn: TH2Connection);
begin
  try
    aConn.Start;                            // raises EH2ConnectionError on bad preface
    while not aConn.Closing do
      if not aConn.HandleNextFrame then     // False => GOAWAY emitted, finished
        Break;
  except
    // A finite/closed transport surfaces a short read as EReadError when the frame loop reads past the end
    // the expected clean termination, not a failure.
    on E: EStreamError do
      ; // normal loop exit
    on E: EH2ConnectionError do
      HandleError(aConn, E);                // bad preface etc. - log via OnError
    on E: Exception do
      HandleError(aConn, E);
  end;
end;

procedure TCustomHTTP2Handler.ConnectionDone(aConn: TH2Connection;
  aSocket: TStream; aFreeSocket: Boolean);
begin
  FConnections.Remove(aConn);
  aConn.Free;
  // The TH2Connection never owns/frees its transport, so the handler closes the socket in thread modes.
  // Linger-drain before the close so an orderly FIN is sent instead of a RST. 
  if aFreeSocket and Assigned(aSocket) then
  begin
    LingeringClose(aSocket);
    aSocket.Free;
  end;
end;

procedure TCustomHTTP2Handler.ThreadConnectionDone(Sender: TObject);
var
  T: TH2ConnectionThread;
  Conn: TH2Connection;
  Sock: TStream;
begin
  T := Sender as TH2ConnectionThread;
  // Detach the connection+socket from the thread underthe FThreads lock, before freeing them.
  FThreads.LockList;
  try
    Conn := T.Connection;
    Sock := T.Socket;
    T.DetachConnection;
  finally
    FThreads.UnlockList;
  end;
  // Free outside the lock (ConnectionDone takes the FConnections lock and frees
  // conn+socket); the references are already detached so the walk can't reach them.
  ConnectionDone(Conn, Sock, True);
end;

procedure TCustomHTTP2Handler.CloseTransport(aSocket: TStream);
begin
  // Close the fd so a worker blocked in ReadBuffer returns (or raises EStreamError),
  // letting its run loop observe Closing/transport-end and exit.
  // Closing the TSocketStream does NOT free the object - ConnectionDone frees it later
  if aSocket is TSocketStream then
    TSocketStream(aSocket).Close;
end;

procedure TCustomHTTP2Handler.LingeringClose(aSocket: TStream);
const
  // An errored peer FINs promptly after reading our GOAWAY; keep the linger short
  // and bounded so a misbehaving peer cannot stall the worker's teardown.
  DRAIN_TIMEOUT_MS = 250;
  DRAIN_MAX_ITERS  = 16;
var
  Sock: TSocketStream;
  Iters, N: Integer;
  Scratch: array[0..4095] of Byte;
begin
  if not (aSocket is TSocketStream) then
    Exit;                                  // in-memory test transport: no-op
  Sock := TSocketStream(aSocket);
  Iters := 0;
  // Drain until the peer half-closes (Read<=0 => its FIN/EOF) or nothing more
  // arrives within the timeout. This empties the receive queue so the subsequent
  // close() is a clean FIN that carries the already-written GOAWAY to the peer.
  try
    while (Iters < DRAIN_MAX_ITERS) and Sock.CanRead(DRAIN_TIMEOUT_MS) do
    begin
      Inc(Iters);
      N := Sock.Read(Scratch, SizeOf(Scratch));
      if N <= 0 then
        Break;
    end;
  except
    on E: Exception do
      ; // swallow - draining is advisory; proceed to close
  end;
end;

procedure TCustomHTTP2Handler.CloseConnections;
var
  L: TList;
  I: Integer;
  T: TH2ConnectionThread;
begin
  // Walk the worker registry (it pairs connection↔socket) under its lock. For each
  // live worker: emit a graceful GOAWAY (BeginShutdown, idempotent) while the worker
  // is still blocked in its read, THEN close the fd to unblock it. 
  L := FThreads.LockList;
  try
    for I := 0 to L.Count - 1 do
    begin
      T := TH2ConnectionThread(L[I]);
      if Assigned(T.Connection) then
      begin
        T.Connection.BeginShutdown(ecNoError);
        CloseTransport(T.Socket);
      end;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TCustomHTTP2Handler.AdoptConnection(aSocket: TStream;
  aMode: TH2AdoptMode; aInitialRequest: TRequest; const aBufferedBytes: TBytes;
  const aClientSettings: TH2SettingsArray = nil);
var
  Conn: TH2Connection;
  Limits: TH2Limits;
  InitHeaders: THPackHeaderTextList;
  InitBody: TBytes;
begin
  if not FActive then
    raise EHTTP2Handler.Create(SErrHandlerNotActive);
  // amPriorKnowledge requires a nil initial request
  // amUpgradeH2C PERMITS a non-nil aInitialRequest (the upgraded request), consumed below
  if (aMode = amPriorKnowledge) and Assigned(aInitialRequest) then
    raise EHTTP2Handler.Create(SErrPriorKnowledgeNeedsNilRequest);
  if (aMode = amALPN) and Assigned(aInitialRequest) then
    raise EHTTP2Handler.Create(SErrALPNNeedsNilRequest);

  // Snapshot the handler's configured limits into the connection at
  // construction (read once; the connection never reaches back into the handler).
  Limits.MaxConcurrentStreams := FMaxConcurrentStreams;
  Limits.MaxFrameSize         := FMaxFrameSize;
  Limits.MaxHeaderListSize    := FMaxHeaderListSize;
  Limits.MaxContinuationBytes := FMaxContinuationBytes;
  Limits.HPACKTableLimit      := FHPACKTableLimit;
  Limits.MaxResetStreams      := FMaxResetStreams;
  Limits.MaxPingFlood         := FMaxPingFlood;
  Limits.MaxSettingsFlood     := FMaxSettingsFlood;
  Conn := TH2Connection.Create(aSocket, Limits);
  // For h2c, the decoded HTTP2-Settings become the connection's
  // initial peer SETTINGS (RFC 7540 §3.2.1: the 101 is their implicit ACK).
  if Length(aClientSettings) > 0 then
    Conn.ApplyUpgradeSettings(aClientSettings);
  // For h2c, convert the upgraded request to an HTTP/2 header list + body
  if (aMode = amUpgradeH2C) and Assigned(aInitialRequest) then
  begin
    InitHeaders := H2HeadersFromHTTPRequest(aInitialRequest, InitBody);
    Conn.SetInitialRequest(InitHeaders, InitBody);
  end;
  // Residual handoff - MUST happen before the connection starts.
  Conn.Seed(aBufferedBytes);
  // Wire the completed-stream dispatch to the UNMODIFIED Server.HandleRequest
  // pipeline.
  Conn.OnRequestReady := @FDispatcher.HandleRequestReady;
  FConnections.Add(Conn);

  if FThreadMode = h2tmNone then
  begin
    // Inline: blocks until the connection closes. The caller keeps the socket.
    RunConnection(Conn);
    ConnectionDone(Conn, aSocket, False);
  end
  else
    // h2tmThread / h2tmThreadPool:
    // run on the connection's own thread, which frees the connection + socket on exit.
    FThreads.Add(TH2ConnectionThread.CreateConnection(Self, Conn, aSocket,
      @ThreadConnectionDone));
end;

procedure TCustomHTTP2Handler.DoAdoptPriorKnowledge(aConnection: TFPHTTPConnection;
  aRequest: TFPHTTPConnectionRequest);
const
  // Bytes 0..17 of H2_CLIENT_PREFACE = 'PRI * HTTP/2.0'#13#10#13#10 - the part the
  // HTTP/1.1 start-line + blank-line read already consumed off the socket.
  ConsumedPrefaceLen = 18;
var
  Seed, Residual: TBytes;
begin
  // aRequest is unused: the detection predicate already proved the start line was the
  // canonical PRI * HTTP/2.0 preface.
  Residual := aConnection.ResidualBytes;
  SetLength(Seed, ConsumedPrefaceLen + Length(Residual));
  Move(H2_CLIENT_PREFACE[0], Seed[0], ConsumedPrefaceLen);
  if Length(Residual) > 0 then
    Move(Residual[0], Seed[ConsumedPrefaceLen], Length(Residual));
  // aInitialRequest is nil for amPriorKnowledge.
  AdoptConnection(aConnection.Socket, amPriorKnowledge, nil, Seed);
end;

procedure TCustomHTTP2Handler.DoAdoptHTTP2(aConnection: TFPHTTPConnection;
  aRequest: TFPHTTPConnectionRequest);
begin
  // One adopt seam, two cleartext-vs-TLS ingress shapes.
  // ALPN (TLS) fires with a nil request and a negotiated 'h2';
  // Prior-knowledge (cleartext) fires with the parsed PRI request and '' negotiated. 
  if aConnection.NegotiatedProtocol = 'h2' then
    DoAdoptALPN(aConnection, aRequest)
  else
    DoAdoptPriorKnowledge(aConnection, aRequest);
end;

procedure TCustomHTTP2Handler.DoAdoptALPN(aConnection: TFPHTTPConnection;
  {%H-}aRequest: TFPHTTPConnectionRequest);
begin
  // ALPN: detection fired BEFORE any HTTP/1.1 parse.
  // so nothing off the HTTP/2 stream was consumed at this point
  AdoptConnection(aConnection.Socket, amALPN, nil, aConnection.ResidualBytes);
end;

procedure TCustomHTTP2Handler.DoCheckH2CUpgrade(aRequest: TFPHTTPConnectionRequest;
  var aHandlesUpgrade: Boolean);
var
  Dummy: TH2SettingsArray;
begin
  // CheckUpgrade has already verified Connection:upgrade + a non-empty Upgrade
  // header before we fire. The zero-impact gate: handle ONLY when the Upgrade
  // token is exactly `h2c` AND the request carries a single, base64url-decodable,
  // length-multiple-of-6 HTTP2-Settings (RFC 7540 §3.2.1). Everything else returns
  // False and falls through to normal HTTP/1.1 handling (NOT an error). Any method
  // may upgrade per RFC, so we deliberately do not gate on Method.
  aHandlesUpgrade := SameText(aRequest.Upgrade, H2C_UPGRADE_NAME)
                     and TryDecodeHTTP2Settings(aRequest, Dummy);
end;

procedure TCustomHTTP2Handler.DoUpgradeH2C(aConnection: TFPHTTPConnection;
  aRequest: TFPHTTPConnectionRequest);
var
  Settings: TH2SettingsArray;
  Seed: TBytes;
begin
  // (a) Write the 101 RAW to the socket.
  aConnection.Socket.WriteBuffer(H2C_101_RESPONSE[1], Length(H2C_101_RESPONSE));
  // (b) Decode HTTP2-Settings - DoCheckH2CUpgrade already proved this succeeds.
  if not TryDecodeHTTP2Settings(aRequest, Settings) then
    Settings := nil;
  // (c) Seed: replay any bytes the HTTP/1.1 layer already buffered past the
  //     blank-line terminator. Unlike prior-knowledge there is NO 18-byte
  //     preface to reconstruct
  Seed := aConnection.ResidualBytes;
  // (d) Adopt in h2c mode
  AdoptConnection(aConnection.Socket, amUpgradeH2C, aRequest, Seed, Settings);
end;

function TCustomHTTP2Handler.ConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TCustomHTTP2Handler.WaitForConnections(aMaxAttempts: Integer): Boolean;
var
  aLastCount, aCount: Integer;
begin
  aCount := 0;
  aLastCount := ConnectionCount;
  while (ConnectionCount > 0) and (aCount < aMaxAttempts) do
  begin
    Sleep(100);
    if ConnectionCount = aLastCount then
      Inc(aCount)
    else
      aLastCount := ConnectionCount;
  end;
  Result := aLastCount = 0;
end;

end.
