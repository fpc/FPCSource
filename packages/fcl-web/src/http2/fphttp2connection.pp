{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2026 - by the Free Pascal development team

    HTTP/2 connection and stream handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fphttp2connection;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Contnrs, FpWeb.Http2.Consts, FpWeb.Http2.Frames, FpWeb.Uhpack;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Contnrs, fphttp2consts, fphttp2frames, uhpack;
{$ENDIF FPC_DOTTEDUNITS}

type
  { An  EH2ConnectionError is connection-fatal:
    HandleNextFrame converts it to GOAWAY(FLastProcessedPeerStreamID)+close,
    The graceful lifecycle path emits a NO_ERROR GOAWAY }
  EH2ConnectionError = class(Exception)
  public
    ErrorCode: TH2ErrorCode;
    constructor Create(aCode: TH2ErrorCode; const aMsg: string = '');
  end;

  { stream-scoped error (RST_STREAM). raised by the stream state machine:
    recv on a half-closed-remote/closed stream, or DATA onan unopened id)
    Results in STREAM_CLOSED }
  EH2StreamError = class(Exception)
  public
    ErrorCode: TH2ErrorCode;
    StreamID: Cardinal;
    constructor Create(aCode: TH2ErrorCode; aStreamID: Cardinal; const aMsg: string = '');
  end;

  // RFC 9113 §5.1 per-stream lifecycle. These are connection-LAYER states
  TH2StreamState = (ssIdle, ssOpen, ssHalfClosedLocal, ssHalfClosedRemote, ssClosed);

  TH2Connection = class;

  { Fired ONCE per stream by the demux when the peer finishes its side (END_STREAM)
    The bridge to the request pipeline lives in fphttp2request.pp }
  TH2RequestEvent = procedure(aConn: TH2Connection; aStreamID: Cardinal;
    aHeaders: THPackHeaderTextList; const aBody: TBytes) of object;

  { TH2Stream - tracks one stream's id + RFC 9113 §5.1 lifecycle state.
    The owning TH2Connection creates it in ssIdle and drives the RECEIVE-side
    transitions from its demux (ProcessNextFrame).
    The SEND-side transition is called when the server emits its response }

  TH2Stream = class
  private
    FID: Cardinal;
    FState: TH2StreamState;
    // §6.9 per-stream SEND window 
    FSendWindow: Int64;
    // The decoded request headers Kept until the request is dispatched.
    FHeaders: THPackHeaderTextList;
    FBody: TBytes;
    FDispatched: Boolean;
    // §6.9.1 pending-send buffer
    FPendingData: TBytes;
    FPendingEndStream: Boolean;
    FHasPending: Boolean;
  public
    constructor Create(aID: Cardinal; aInitialSendWindow: Int64);
    destructor Destroy; override;
    // Append a (de-padded) DATA payload to the body buffer
    procedure AppendBody(const aBytes: TBytes);
    // Receive-side transitions (server view; peer = client).
    // aEndStream is the frame's END_STREAM flag.
    procedure RecvHeaders(aEndStream: Boolean);
    procedure RecvData(aEndStream: Boolean);
    procedure RecvRSTStream;
    procedure SentEndStream;
    // §5.1 - we emitted RST_STREAM for this stream: it is closed locally
    // regardless of prior state, symmetric with RecvRSTStream for a peer-sent
    // reset. A later frame on it is then handled as a closed stream.
    procedure SentRSTStream;
    // §6.9 grow this stream's send window by a peer WINDOW_UPDATE increment.
    // aIncrement=0 -> PROTOCOL_ERROR; pushing past H2_MAX_WINDOW_SIZE ->
    // FLOW_CONTROL_ERROR (both stream-scoped). Comparison is done in Int64 BEFORE
    // committing so it cannot wrap (§6.9.1).
    procedure IncreaseSendWindow(aIncrement: Cardinal);
    // Debit aCount octets from the send window. Called ONLY by SendData, which has
    // already clamped aCount to the available window, so it never goes negative here.
    procedure ConsumeSendWindow(aCount: Cardinal);
    // §6.9.2 - apply a SIGNED retroactive delta to the send window when
    // the peer changes SETTINGS_INITIAL_WINDOW_SIZE mid-connection. The result MAY
    // be negative and is retained (a sender MUST track a negative window and not
    // send until a later WINDOW_UPDATE makes it positive). Int64, no clamp.
    procedure AdjustSendWindow(aDelta: Int64);
    property ID: Cardinal read FID;
    property State: TH2StreamState read FState;
    // Read-only send-window exposure for tests/dispatch (§6.9).
    property SendWindow: Int64 read FSendWindow;
    // The stream's persistent decoded request headers (created in the ctor; the
    // demux decodes each HEADERS/CONTINUATION block into it).
    property Headers: THPackHeaderTextList read FHeaders;
    // The accumulated (de-padded) request body bytes.
    property Body: TBytes read FBody;
    // Set once the completed request has been dispatched (one-shot guard).
    property Dispatched: Boolean read FDispatched write FDispatched;
    // §6.9.1 - TRUE while this stream still owes egress (buffered DATA and/or an
    // unsent END_STREAM). ResumeSends visits only streams where this holds.
    property HasPending: Boolean read FHasPending;
  end;

  { TH2Connection - owns a transport TStream and performs the RFC 9113 §3.4
    connection-preface / SETTINGS exchange. It references no socket unit: it
    works purely against the abstract TStream (the component passes the real
    TSocketStream, tests pass an in-memory duplex stream). Every outbound frame
    flows through the single WriteLock-protected SendFrame egress. Seed() injects
    pre-read bytes consumed before the transport. }

  TH2Connection = class
  private
    FTransport: TStream;
    FWriteLock: TRTLCriticalSection;
    FSeed: TBytes;
    FSeedPos: Integer;
    FRemoteSettings: TH2SettingsArray;
    FStarted: Boolean;
    // h2c (§3.2.1) - the upgraded HTTP/1.1 request
    FInitialHeaders: THPackHeaderTextList;
    FInitialBody: TBytes;
    FHasInitialRequest: Boolean;
    // The connection's SINGLE HPACK contexts.
    FEncoder: THPackEncoder;
    FDecoder: THPackDecoder;
    // The five DoS bounds, read ONCE at construction.
    FLimits: TH2Limits;
    // Per-connection FLOOD/RAPID-RESET counters.
    FResetCount: Cardinal;
    FPingCount: Cardinal;
    FSettingsCount: Cardinal;
    // The seed-then-transport input shim - it always holds a TH2InputStream.
    FInput: TStream;
    // Connection-scoped stream registry (id -> TH2Stream), keyed by IntToStr(id),
    // mirroring the WebSocket TFPHashList idiom but WITHOUT a lock: it is touched
    // only on the single-threaded frame-read path..
    FStreams: TFPHashList;
    // Highest client-initiated (odd) stream id opened so far.
    FLastPeerStreamID: Cardinal;
    // §6.8 GOAWAY last-stream-id
    FLastProcessedPeerStreamID: Cardinal;
    // Set TRUE by HandleNextFrame after it emits a GOAWAY
    FClosing: Boolean;
    // §6.9 connection-level SEND window
    FSendWindow: Int64;
    // §6.9.2 - the LIVE peer SETTINGS_INITIAL_WINDOW_SIZE currently in force.
    FPeerInitialWindow: Cardinal;
    // The completed-request notification.
    FOnRequestReady: TH2RequestEvent;
    // Scan FRemoteSettings for the peer's advertised SETTINGS_INITIAL_WINDOW_SIZE, defaults to : H2_DEFAULT_INITIAL_WINDOW_SIZE.
    function PeerInitialWindowSize: Cardinal;
    // §6.9 grow the CONNECTION send window by a stream-0 WINDOW_UPDATE increment.
    procedure IncreaseConnSendWindow(aIncrement: Cardinal);
    // Int64 minimum - shared by SendData and FlushStream (avoids a Math dependency).
    function MinI64(a, b: Int64): Int64;
    // §6.9.1 - drain aStream's pending-send buffer as far as the current connection+stream windows allow
    function FlushStream(aStream: TH2Stream): Integer;
    // §6.9.1 - after any event that grows a usable window
    procedure ResumeSends;
    // Read source: drain the seed buffer first, then the transport.
    procedure ReadFull(var aBuffer; aCount: Integer);
    // Serialize a SETTINGS frame to TBytes via the codec (into a TBytesStream), ready to be emitted through SendFrame.
    function SerializeSettings(const aSettings: TH2SettingsArray; aFlags: Byte): TBytes;
    // Consume and discard aLen payload octets (parse-and-ignore frames).
    procedure DiscardPayload(aLen: Cardinal);
    // Validate a NEW client-initiated stream id: odd + strictly greater than FLastPeerStreamID; on success create the stream.
    function OpenStream(aStreamID: Cardinal): TH2Stream;
    // §5.1.2 - count streams that count against SETTINGS_MAX_CONCURRENT_STREAMS:
    function ActiveStreamCount: Integer;
    // Route a stream-0 (connection-control) frame. DATA/HEADERS/RST_STREAM on stream 0 are illegal -> EH2ConnectionError.
    procedure ProcessConnectionFrame(const aHeader: TH2FrameHeader);
    // Route a stream-id>0 frame to its TH2Stream, driving the §5.1 transition.
    procedure ProcessStreamFrame(const aHeader: TH2FrameHeader);
    // §8.1.2.6 - when a request is fully received (END_STREAM)
    procedure ValidateContentLength(aStream: TH2Stream);
    // Dispatch trigger: when aStream's peer side is finished - fire it exactly once with the stream's retained headers + body. 
    procedure MaybeDispatch(aStream: TH2Stream);
    // A fully-received request is real work and credits back all three flood budgets
    procedure CreditProgress;
    // Control/error-frame emit helpers
    procedure SendPingAck(const aOpaque: TBytes);
    procedure SendRSTStream(aStreamID: Cardinal; aErrorCode: TH2ErrorCode);
    procedure SendGoAway(aLastStreamID: Cardinal; aErrorCode: TH2ErrorCode);
  public
    constructor Create(aTransport: TStream); overload;
    constructor Create(aTransport: TStream; const aLimits: TH2Limits); overload;
    destructor Destroy; override;
    // Inject pre-read residual bytes. MUST be called before Start: the read path drains these before touching the transport.
    procedure Seed(const aBytes: TBytes);
    // h2c - inject the base64url-decoded HTTP2-Settings as the initial peer SETTINGS. MUST be called before Start
    procedure ApplyUpgradeSettings(const aSettings: TH2SettingsArray);
    // h2c - store the converted upgraded request as the connection-owned snapshot synthesized into stream 1 at the tail of Start. MUST be called before Start
    procedure SetInitialRequest(aHeaders: THPackHeaderTextList; const aBody: TBytes);
    // Perform the server/client preface + SETTINGS exchange (RFC 9113 §3.4/§6.5).
    procedure Start;
    // The SOLE write egress: acquire WriteLock, write the whole frame, release.
    procedure SendFrame(const aFrameBytes: TBytes);
    // Decode a COMPLETE header-block. The decoder's dynamic table is updated as a side effect.
    procedure DecodeHeaderBlock(const aFragment: TBytes; aHeaders: THPackHeaderTextList);
    // Encode aHeaders through the connection's single encoder and emit a HEADERS frame for aStreamID
    procedure SendHeaders(aStreamID: Cardinal; aHeaders: THPackHeaderTextList; aFlags: Byte = 0);
    // The only flow-controlled DATA egress - the DATA analogue of SendHeaders. 
    function SendData(aStreamID: Cardinal; const aData: TBytes; aEndStream: Boolean): Integer;
    // Return the tracked stream for aStreamID, or nil if none is registered.
    function FindStream(aStreamID: Cardinal): TH2Stream;
    // §6.9 connection-level SEND window, read-only for tests/dispatch.
    property SendWindow: Int64 read FSendWindow;
    // Read ONE frame from the seed-then-transport input and route it.
    procedure ProcessNextFrame;
    // The SINGLE catch site that wraps ProcessNextFrame and converts the two connection-layer exception scopes into wire frames
    function HandleNextFrame: Boolean;
    // §6.8 graceful-shutdown egress. 
    procedure BeginShutdown(aErrorCode: TH2ErrorCode = ecNoError);
    property Transport: TStream read FTransport;
    property RemoteSettings: TH2SettingsArray read FRemoteSettings;
    // The connection's immutable limit set (read-only, for tests).
    property Limits: TH2Limits read FLimits;
    // Fired once per stream when the request is complete.
    property OnRequestReady: TH2RequestEvent read FOnRequestReady write FOnRequestReady;
    // Highest client-initiated stream id opened (0 if none) - for GOAWAY.
    property LastPeerStreamID: Cardinal read FLastPeerStreamID;
    // §6.8 GOAWAY last-stream-id source (highest processed peer stream).
    property LastProcessedPeerStreamID: Cardinal read FLastProcessedPeerStreamID;
    // TRUE once a connection-scoped violation has caused a GOAWAY to be emitted by HandleNextFrame.
    property Closing: Boolean read FClosing;
  end;

implementation

{ A TStream that exposes the connection's seed-then-transport read source as a
  single stream, so the codec Read*/ReadFrameHeader calls read uniformly. Only
  Read is meaningful; Write/Seek are not supported (this shim is read-only). }

type
  TH2InputStream = class(TStream)
  private
    FOwner: TH2Connection;
  public
    constructor Create(aOwner: TH2Connection);
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ EH2ConnectionError }

constructor EH2ConnectionError.Create(aCode: TH2ErrorCode; const aMsg: string = '');
begin
  inherited Create(aMsg);
  ErrorCode := aCode;
end;

{ EH2StreamError }

constructor EH2StreamError.Create(aCode: TH2ErrorCode; aStreamID: Cardinal;
  const aMsg: string = '');
begin
  inherited Create(aMsg);
  ErrorCode := aCode;
  StreamID := aStreamID;
end;

{ TH2Stream }

constructor TH2Stream.Create(aID: Cardinal; aInitialSendWindow: Int64);
begin
  inherited Create;
  FID := aID;
  FState := ssIdle;   // every stream starts idle (§5.1)
  FSendWindow := aInitialSendWindow;
  FHeaders := THPackHeaderTextList.Create;
  FHasPending := False;
  FPendingEndStream := False;
end;

destructor TH2Stream.Destroy;
begin
  FHeaders.Free;
  inherited Destroy;
end;

procedure TH2Stream.AppendBody(const aBytes: TBytes);
var
  OldLen, AddLen: Integer;
begin
  AddLen := Length(aBytes);
  if AddLen = 0 then
    Exit;
  OldLen := Length(FBody);
  // A body that would exceed H2_DEFAULT_MAX_BODY_SIZE is a stream-scoped REFUSED_STREAM. 
  if Int64(OldLen) + Int64(AddLen) > H2_DEFAULT_MAX_BODY_SIZE then
    raise EH2StreamError.Create(ecRefusedStream, FID,
      'request body exceeds H2_DEFAULT_MAX_BODY_SIZE');
  SetLength(FBody, OldLen + AddLen);
  Move(aBytes[0], FBody[OldLen], AddLen);   // 0-based TBytes copy
end;

procedure TH2Stream.RecvHeaders(aEndStream: Boolean);
begin
  // §5.1 receive HEADERS (peer = client). On an idle stream this is the open;
  // on an open stream it is trailers. END_STREAM half-closes the remote side.
  case FState of
    ssIdle:
      if aEndStream then
        FState := ssHalfClosedRemote
      else
        FState := ssOpen;
    ssOpen:
      if aEndStream then
        FState := ssHalfClosedRemote;        // trailers with END_STREAM; else stays open
    ssHalfClosedLocal:
      if aEndStream then
        FState := ssClosed;                  // else stays half-closed (local)
    ssHalfClosedRemote:
      // Peer ended its side but the stream is not yet fully closed:
      raise EH2StreamError.Create(ecStreamClosed, FID,
        'HEADERS received on half-closed(remote) stream');
    ssClosed:
      // A fully-closed stream receiving HEADERS is a CONNECTION error STREAM_CLOSED (§5.1)
      raise EH2ConnectionError.Create(ecStreamClosed,
        Format('HEADERS received on closed stream %d (§5.1)', [FID]));
  end;
end;

procedure TH2Stream.RecvData(aEndStream: Boolean);
begin
  // §5.1 receive DATA (peer = client).
  case FState of
    ssOpen:
      if aEndStream then
        FState := ssHalfClosedRemote;
    ssHalfClosedLocal:
      if aEndStream then
        FState := ssClosed;
    ssIdle, ssHalfClosedRemote, ssClosed:
      raise EH2StreamError.Create(ecStreamClosed, FID,
        'DATA received on idle/half-closed(remote)/closed stream');
  end;
end;

procedure TH2Stream.RecvRSTStream;
begin
  // RST_STREAM closes the stream from any active state;
  FState := ssClosed;
end;

procedure TH2Stream.SentEndStream;
begin
  // Send-side END_STREAM, driven by SendData and the response adapter.
  case FState of
    ssOpen:
      FState := ssHalfClosedLocal;
    ssHalfClosedRemote:
      FState := ssClosed;
  end;
end;

procedure TH2Stream.SentRSTStream;
begin
  // We emitted RST_STREAM for this stream (§5.1/§5.4.2):
  FState := ssClosed;
end;

procedure TH2Stream.IncreaseSendWindow(aIncrement: Cardinal);
begin
  // §6.9 - a zero increment is a PROTOCOL_ERROR (stream-scoped).
  if aIncrement = 0 then
    raise EH2StreamError.Create(ecProtocolError, FID,
      'WINDOW_UPDATE with zero increment');
  // §6.9.1 - the window may not exceed 2^31-1.
  if FSendWindow + Int64(aIncrement) > H2_MAX_WINDOW_SIZE then
    raise EH2StreamError.Create(ecFlowControlError, FID,
      'WINDOW_UPDATE overflows the stream flow-control window');
  Inc(FSendWindow, aIncrement);
end;

procedure TH2Stream.ConsumeSendWindow(aCount: Cardinal);
begin
  // FlushStream has already clamped aCount to min(conn, stream) window
  Dec(FSendWindow, aCount);
end;

procedure TH2Stream.AdjustSendWindow(aDelta: Int64);
begin
  // §6.9.2 - signed retroactive adjustment;
  Inc(FSendWindow, aDelta);
end;

{ TH2InputStream }

constructor TH2InputStream.Create(aOwner: TH2Connection);
begin
  inherited Create;
  FOwner := aOwner;
end;

function TH2InputStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  // Delegate to the owner's seed-then-transport drain
  if Count <= 0 then
    Exit(0);
  FOwner.ReadFull(Buffer, Count);
  Result := Count;
end;

function TH2InputStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  // Read-only shim: writes always go straight to FTransport under WriteLock.
  raise EH2ConnectionError.Create(ecInternalError, 'TH2InputStream is read-only');
end;

function TH2InputStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // The input source is a forward-only byte stream (seed then socket).
  raise EH2ConnectionError.Create(ecInternalError, 'TH2InputStream is not seekable');
end;

{ TH2Connection }

constructor TH2Connection.Create(aTransport: TStream);
begin
  // Delegate to the limits-aware ctor with the safe defaults so there is one
  // construction path and the parameterless form is just "default limits".
  Create(aTransport, DEFAULT_H2_LIMITS);
end;

constructor TH2Connection.Create(aTransport: TStream; const aLimits: TH2Limits);
begin
  inherited Create;
  FTransport := aTransport;
  FLimits := aLimits;
  InitCriticalSection(FWriteLock);
  FEncoder := THPackEncoder.Create;
  FDecoder := THPackDecoder.Create(FLimits.MaxHeaderListSize, FLimits.HPACKTableLimit);
  FInput := TH2InputStream.Create(Self);
  FStreams := TFPHashList.Create;
  FLastPeerStreamID := 0;
  FLastProcessedPeerStreamID := 0;
  FResetCount := 0;
  FPingCount := 0;
  FSettingsCount := 0;
  FSendWindow := H2_DEFAULT_INITIAL_WINDOW_SIZE;
  FInitialHeaders := nil;
  FHasInitialRequest := False;
end;

destructor TH2Connection.Destroy;
var
  I: Integer;
begin
  // The connection owns every TH2Stream it created
  if Assigned(FStreams) then
  begin
    for I := 0 to FStreams.Count - 1 do
      TH2Stream(FStreams[I]).Free;
    FreeAndNil(FStreams);
  end;
  FreeAndNil(FInput);
  FreeAndNil(FInitialHeaders);
  FreeAndNil(FEncoder);
  FreeAndNil(FDecoder);
  DoneCriticalSection(FWriteLock);
  inherited Destroy;
end;

procedure TH2Connection.Seed(const aBytes: TBytes);
var
  OldLen, AddLen: Integer;
begin
  if FStarted then
    raise EH2ConnectionError.Create(ecInternalError, 'Seed must be called before Start');
  AddLen := Length(aBytes);
  if AddLen = 0 then
    Exit;
  OldLen := Length(FSeed);
  SetLength(FSeed, OldLen + AddLen);
  Move(aBytes[0], FSeed[OldLen], AddLen);
end;

procedure TH2Connection.ApplyUpgradeSettings(const aSettings: TH2SettingsArray);
begin
  // Must precede Start, exactly like Seed
  if FStarted then
    raise EH2ConnectionError.Create(ecInternalError,
      'ApplyUpgradeSettings must be called before Start');
  // Front-load the h2c client's settings as the initial peer baseline
  FRemoteSettings := aSettings;
  FPeerInitialWindow := PeerInitialWindowSize;
end;

procedure TH2Connection.SetInitialRequest(aHeaders: THPackHeaderTextList;
  const aBody: TBytes);
begin
  // Pre-Start; guarded exactly like Seed/ApplyUpgradeSettings:
  if FStarted then
    raise EH2ConnectionError.Create(ecInternalError,
      'SetInitialRequest must be called before Start');
  // Take ownership of the header list (freed in Destroy) and copy the body snapshot.
  FInitialHeaders := aHeaders;
  FInitialBody := Copy(aBody);
  FHasInitialRequest := True;
end;

procedure TH2Connection.ReadFull(var aBuffer; aCount: Integer);
var
  Dest: PByte;
  FromSeed: Integer;
begin
  if aCount <= 0 then
    Exit;
  Dest := @aBuffer;
  // Drain any remaining seeded bytes first 
  if FSeedPos < Length(FSeed) then
  begin
    FromSeed := Length(FSeed) - FSeedPos;
    if FromSeed > aCount then
      FromSeed := aCount;
    Move(FSeed[FSeedPos], Dest^, FromSeed);
    Inc(FSeedPos, FromSeed);
    Inc(Dest, FromSeed);
    Dec(aCount, FromSeed);
  end;
  // read the remainder from the transport 
  if aCount > 0 then
    FTransport.ReadBuffer(Dest^, aCount);
end;

function TH2Connection.SerializeSettings(const aSettings: TH2SettingsArray;
  aFlags: Byte): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WriteSettingsFrame(S, aSettings, aFlags);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

procedure TH2Connection.SendFrame(const aFrameBytes: TBytes);
begin
  // The ONLY place in this unit that writes to the transport.
  EnterCriticalSection(FWriteLock);
  try
    if Length(aFrameBytes) > 0 then
      FTransport.WriteBuffer(aFrameBytes[0], Length(aFrameBytes));
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

procedure TH2Connection.SendPingAck(const aOpaque: TBytes);
var
  S: TBytesStream;
begin
  // §6.7 - reflect the 8 opaque octets back with the ACK flag set
  S := TBytesStream.Create;
  try
    WritePingFrame(S, aOpaque, H2_FLAG_ACK);
    SendFrame(Copy(S.Bytes, 0, S.Size));
  finally
    S.Free;
  end;
end;

procedure TH2Connection.SendRSTStream(aStreamID: Cardinal; aErrorCode: TH2ErrorCode);
var
  S: TBytesStream;
begin
  // §6.4 - terminate one stream while the connection survives.
  //  ONLY called from HandleNextFrame 
  S := TBytesStream.Create;
  try
    WriteRSTStreamFrame(S, aStreamID, Cardinal(Ord(aErrorCode)));
    SendFrame(Copy(S.Bytes, 0, S.Size));
  finally
    S.Free;
  end;
end;

procedure TH2Connection.SendGoAway(aLastStreamID: Cardinal; aErrorCode: TH2ErrorCode);
var
  S: TBytesStream;
begin
  // §6.8 - tell the peer the highest stream id we processed and why.
  S := TBytesStream.Create;
  try
    WriteGoAwayFrame(S, aLastStreamID, Cardinal(Ord(aErrorCode)), nil);
    SendFrame(Copy(S.Bytes, 0, S.Size));
  finally
    S.Free;
  end;
end;

procedure TH2Connection.BeginShutdown(aErrorCode: TH2ErrorCode = ecNoError);
begin
  // Idempotent: if a GOAWAY already went out - never a second GOAWAY.
  if FClosing then
    Exit;
  SendGoAway(FLastProcessedPeerStreamID, aErrorCode);
  FClosing := True;
end;

function TH2Connection.HandleNextFrame: Boolean;
var
  S: TH2Stream;
begin
  // The only catch site, driven by the `while not Closing do HandleNextFrame` loop.
  Result := True;
  try
    ProcessNextFrame;
  except
    on E: EH2StreamError do
    begin
      SendRSTStream(E.StreamID, E.ErrorCode);
      // §5.1 - the stream we just RST'd is closed locally.
      S := FindStream(E.StreamID);
      if Assigned(S) then
        S.SentRSTStream;
    end;
    on E: EH2ConnectionError do
    begin
      if not FClosing then
        SendGoAway(FLastProcessedPeerStreamID, E.ErrorCode);
      FClosing := True;
      Result := False;
    end;
  end;
end;

procedure TH2Connection.DecodeHeaderBlock(const aFragment: TBytes;
  aHeaders: THPackHeaderTextList);
var
  S: TBytesStream;
  I: Integer;
begin
  // The decoder reads from a TStream and uses Size-Position to know how much of the block remains
  S := TBytesStream.Create(aFragment);
  try
    try
      FDecoder.Decode(S);
    except
      // RFC 9113 §6.5.2: an HPACK processing failure is a connection error (COMPRESSION_ERROR). 
      on E: THPACKException do
        raise EH2ConnectionError.Create(ecCompressionError, 'HPACK decode: ' + E.Message);
    end;
    // RFC 7541 - a COMPLETE header block that ends part-way through a field representation is a decoding error.
    if not FDecoder.AtHeaderBoundary then
      raise EH2ConnectionError.Create(ecCompressionError,
        'incomplete HPACK header block (ended mid-representation)');
    // A truncated list is a connection COMPRESSION_ERROR (§6.5.2).
    if FDecoder.EndHeaderBlockTruncated then
      raise EH2ConnectionError.Create(ecCompressionError,
        'header list exceeds MaxHeaderListSize');
    // Copy this block's decoded pairs out to the caller, then clear only the output accumulator
    for I := 0 to FDecoder.DecodedHeaders.Count - 1 do
      aHeaders.Add(FDecoder.DecodedHeaders[I]^.HeaderName,
        FDecoder.DecodedHeaders[I]^.HeaderValue,
        FDecoder.DecodedHeaders[I]^.IsSensitive);
    FDecoder.DecodedHeaders.Clear;
  finally
    S.Free;
  end;
end;

procedure TH2Connection.SendHeaders(aStreamID: Cardinal;
  aHeaders: THPackHeaderTextList; aFlags: Byte = 0);
var
  Frag: TBytesStream;
  FragBytes: TBytes;
  I: Integer;
begin
  // The central correctness invariant: HPACK-encode and write the HEADERS frame as one atomic step under FWriteLock.
  EnterCriticalSection(FWriteLock);
  try
    Frag := TBytesStream.Create;
    try
      try
        for I := 0 to aHeaders.Count - 1 do
          FEncoder.EncodeHeader(Frag, aHeaders[I]^.HeaderName,
            aHeaders[I]^.HeaderValue, False);   // encode INSIDE the lock
      except
        on E: THPACKException do
          raise EH2ConnectionError.Create(ecCompressionError, 'HPACK encode: ' + E.Message);
      end;
      FragBytes := Copy(Frag.Bytes, 0, Frag.Size);
      // Single complete block: END_HEADERS is always set (no CONTINUATION split on the send side). aFlags may add END_STREAM.
      WriteHeadersFrame(FTransport, aStreamID, FragBytes,
        aFlags or H2_FLAG_END_HEADERS);          // write INSIDE the same lock
    finally
      Frag.Free;
    end;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

function TH2Connection.MinI64(a, b: Int64): Int64;
begin
  // Int64 minimum - avoids pulling Math in; shared by SendData and FlushStream.
  if a < b then
    Result := a
  else
    Result := b;
end;

function TH2Connection.FlushStream(aStream: TH2Stream): Integer;
var
  Avail, Chunk: Int64;
  Flags: Byte;
  ChunkBytes, FrameBytes: TBytes;
  Tmp: TBytesStream;
  Last: Boolean;
begin
  // §6.9.1 windowed drain of aStream's pending-send buffer - the single output method
  Result := 0;
  if not aStream.FHasPending then
    Exit;   // nothing owed

  // Empty-body END_STREAM (§6.9.1): a zero-length DATA frame with END_STREAM is exempt from flow control and always emitted
  if (Length(aStream.FPendingData) = 0) and aStream.FPendingEndStream then
  begin
    Tmp := TBytesStream.Create;
    try
      WriteDataFrame(Tmp, aStream.ID, nil, H2_FLAG_END_STREAM);
      FrameBytes := Copy(Tmp.Bytes, 0, Tmp.Size);
    finally
      Tmp.Free;
    end;
    SendFrame(FrameBytes);
    aStream.SentEndStream;
    aStream.FPendingEndStream := False;
    aStream.FHasPending := False;
    Exit;
  end;

  { Drain while bytes remain AND both windows allow at least one octet.
    Note:
    A window <= 0 (e.g. a retroactive SETTINGS decrease, §6.9.2) sends nothing
    and the buffer is retained - the §6.9.2 "MUST NOT send until positive" }
  while (Length(aStream.FPendingData) > 0) and
        (MinI64(FSendWindow, aStream.SendWindow) > 0) do
  begin
    Avail := MinI64(FSendWindow, aStream.SendWindow);
    // No frame exceeds either send window nor the 16384 default max frame size.
    Chunk := MinI64(MinI64(Avail, H2_DEFAULT_MAX_FRAME_SIZE),
      Length(aStream.FPendingData));
    // END_STREAM only when this chunk drains the buffer AND an end-stream is owed.
    Last := aStream.FPendingEndStream and (Chunk = Length(aStream.FPendingData));
    if Last then
      Flags := H2_FLAG_END_STREAM
    else
      Flags := 0;
    // 0-based TBytes slice
    ChunkBytes := Copy(aStream.FPendingData, 0, Chunk);
    Tmp := TBytesStream.Create;
    try
      WriteDataFrame(Tmp, aStream.ID, ChunkBytes, Flags);
      FrameBytes := Copy(Tmp.Bytes, 0, Tmp.Size);
    finally
      Tmp.Free;
    end;
    SendFrame(FrameBytes);
    // Debit BOTH windows by every octet placed in the frame (§6.9.1).
    aStream.ConsumeSendWindow(Cardinal(Chunk));
    Dec(FSendWindow, Chunk);
    // Trim the sent prefix off the pending buffer (0-based TBytes slice).
    aStream.FPendingData := Copy(aStream.FPendingData, Chunk,
      Length(aStream.FPendingData) - Chunk);
    if Last then
    begin
      aStream.SentEndStream;
      aStream.FPendingEndStream := False;
    end;
    Inc(Result, Chunk);
  end;

  // Clear the owes-egress flag once the buffer is empty AND no END_STREAM is owed
  if (Length(aStream.FPendingData) = 0) and not aStream.FPendingEndStream then
    aStream.FHasPending := False;
end;

function TH2Connection.SendData(aStreamID: Cardinal; const aData: TBytes;
  aEndStream: Boolean): Integer;
var
  S: TH2Stream;
  OldLen, AddLen: Integer;
begin
  S := FindStream(aStreamID);
  if S = nil then
    raise EH2ConnectionError.Create(ecInternalError, 'SendData on unknown stream');

  // Append aData to the stream's pending-send buffer.
  AddLen := Length(aData);
  if AddLen > 0 then
  begin
    OldLen := Length(S.FPendingData);
    SetLength(S.FPendingData, OldLen + AddLen);
    Move(aData[0], S.FPendingData[OldLen], AddLen);
  end;
  S.FPendingEndStream := S.FPendingEndStream or aEndStream;
  if (Length(S.FPendingData) > 0) or S.FPendingEndStream then
    S.FHasPending := True;

  // Initial drain attempt.
  Result := FlushStream(S);
end;

procedure TH2Connection.ResumeSends;
var
  I, J: Integer;
  Ids: array of Cardinal;
  Pivot: Cardinal;
  S: TH2Stream;
begin
  // §6.9.1 - after a window grant, do a FlushStream over every pending stream
  // in ASCENDING stream-id order. No lock, SendFrame takes WriteLock per frame.
  SetLength(Ids, FStreams.Count);
  for I := 0 to FStreams.Count - 1 do
    Ids[I] := TH2Stream(FStreams[I]).ID;
  // Insertion sort ascending (stream counts are small - no pruning).
  for I := 1 to High(Ids) do
  begin
    Pivot := Ids[I];
    J := I - 1;
    while (J >= 0) and (Ids[J] > Pivot) do
    begin
      Ids[J + 1] := Ids[J];
      Dec(J);
    end;
    Ids[J + 1] := Pivot;
  end;
  for I := 0 to High(Ids) do
  begin
    S := FindStream(Ids[I]);
    if Assigned(S) and S.HasPending then
      FlushStream(S);
  end;
end;

procedure TH2Connection.Start;
var
  Magic: array[0..23] of Byte;
  I: Integer;
  Hdr: TH2FrameHeader;
  ServerSettings: TH2SettingsArray;
  S: TH2Stream;
begin
  if FStarted then
    Exit;
  FStarted := True;

  { Server connection preface - a SETTINGS frame on stream 0.
    sent as the FIRST frame, before consuming any client bytes (§3.4);
    non-ACK.
    We advertise the limits we enforce so the peer self-limits and our rejects
    are RFC-legitimate (you may only reject what you advertised, §4.2/§6.5.2) }
  SetLength(ServerSettings, 3);
  ServerSettings[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  ServerSettings[0].Value := FLimits.MaxConcurrentStreams;
  ServerSettings[1].Identifier := H2_SETTINGS_MAX_HEADER_LIST_SIZE;
  ServerSettings[1].Value := FLimits.MaxHeaderListSize;
  ServerSettings[2].Identifier := H2_SETTINGS_HEADER_TABLE_SIZE;
  ServerSettings[2].Value := FLimits.HPACKTableLimit;
  if FLimits.MaxFrameSize <> H2_DEFAULT_MAX_FRAME_SIZE then
  begin
    SetLength(ServerSettings, 4);
    ServerSettings[3].Identifier := H2_SETTINGS_MAX_FRAME_SIZE;
    ServerSettings[3].Value := FLimits.MaxFrameSize;
  end;
  SendFrame(SerializeSettings(ServerSettings, 0));

  // Client connection preface - exactly the 24-octet magic (§3.4)
  try
    ReadFull(Magic, SizeOf(Magic));
  except
    on E: EStreamError do
      raise EH2ConnectionError.Create(ecProtocolError,
        'short read on client connection preface');
  end;
  for I := 0 to High(Magic) do
    if Magic[I] <> H2_CLIENT_PREFACE[I] then
      raise EH2ConnectionError.Create(ecProtocolError,
        'invalid client connection preface');

  // Client SETTINGS - the magic is immediately followed by a SETTINGS frame (§3.4).
  try
    Hdr := ReadFrameHeader(FInput);
    if Hdr.FrameType <> Ord(ftSettings) then
      raise EH2ConnectionError.Create(ecProtocolError,
        'expected SETTINGS frame after client preface');
    if Hdr.StreamID <> 0 then
      raise EH2ConnectionError.Create(ecProtocolError,
        'client SETTINGS must be on stream 0');
    if (Hdr.Flags and H2_FLAG_ACK) <> 0 then
      raise EH2ConnectionError.Create(ecProtocolError,
        'first client SETTINGS must not be an ACK');
    FRemoteSettings := ReadSettingsFrame(FInput, Hdr);
  except
    on E: EH2FrameError do
      raise EH2ConnectionError.Create(E.ErrorCode, 'bad client SETTINGS: ' + E.Message);
    on E: EStreamError do
      raise EH2ConnectionError.Create(ecProtocolError,
        'short read on client SETTINGS: ' + E.Message);
  end;

  // §6.9.2 - record the bootstrap peer SETTINGS_INITIAL_WINDOW_SIZE as the live current value.
  FPeerInitialWindow := PeerInitialWindowSize;

  // 4) ACK the client's SETTINGS (§6.5.3):
  SendFrame(SerializeSettings(nil, H2_FLAG_ACK));

  // 5) h2c (§3.2.1) - synthesize the upgraded HTTP/1.1 request as stream 1
  //    and dispatch it through the EXISTING path. 
  if FHasInitialRequest then
  begin
    S := OpenStream(1);
    for I := 0 to FInitialHeaders.Count - 1 do
      S.Headers.Add(FInitialHeaders[I]^.HeaderName, FInitialHeaders[I]^.HeaderValue);
    if Length(FInitialBody) > 0 then
      S.AppendBody(FInitialBody);
    S.RecvHeaders(True);
    FLastProcessedPeerStreamID := 1;
    MaybeDispatch(S);
  end;
end;

procedure TH2Connection.DiscardPayload(aLen: Cardinal);
var
  Buf: array[0..1023] of Byte;
  N: Integer;
begin
  // Read through the SAME seed-then-transport drain FInput uses, so the cursor
  // stays aligned for the next frame even when the payload is ignored.
  while aLen > 0 do
  begin
    if aLen > Cardinal(SizeOf(Buf)) then
      N := SizeOf(Buf)
    else
      N := Integer(aLen);
    ReadFull(Buf, N);
    Dec(aLen, Cardinal(N));
  end;
end;

function TH2Connection.FindStream(aStreamID: Cardinal): TH2Stream;
begin
  // Cast to Int64 so the IntToStr overload is unambiguous for a Cardinal.
  Result := TH2Stream(FStreams.Find(IntToStr(Int64(aStreamID))));
end;

function TH2Connection.OpenStream(aStreamID: Cardinal): TH2Stream;
begin
  // §5.1.1 client-initiated id validation. These are connection-FATAL PROTOCOL_ERRORs (not stream errors);
  if (aStreamID and 1) = 0 then
    raise EH2ConnectionError.Create(ecProtocolError,
      Format('even client stream id %d (server-id space)', [aStreamID]));
  // A single comparison rejects BOTH a reused id and a non-monotonic/lower id:
  // a lower idle stream is implicitly closed once a higher one opens (§5.1.1).
  if aStreamID <= FLastPeerStreamID then
    raise EH2ConnectionError.Create(ecProtocolError,
      Format('non-monotonic/reused client stream id %d (last opened %d)',
        [aStreamID, FLastPeerStreamID]));
  // §6.8 - once a GOAWAY has gone out we MUST NOT process NEW client streams.
  if FClosing then
    raise EH2StreamError.Create(ecRefusedStream, aStreamID,
      'new stream after GOAWAY (connection closing, §6.8)');
  // Seed the new stream's send window with the LIVE peer initial window (§6.9.2)
  Result := TH2Stream.Create(aStreamID, FPeerInitialWindow);
  FLastPeerStreamID := aStreamID;
  FStreams.Add(IntToStr(Int64(aStreamID)), Result);
end;

function TH2Connection.ActiveStreamCount: Integer;
var
  I: Integer;
begin
  // §5.1.2 - only ssOpen/ssHalfClosedLocal/ssHalfClosedRemote count toward MaxConcurrentStreams
  Result := 0;
  for I := 0 to FStreams.Count - 1 do
    if TH2Stream(FStreams[I]).State in
       [ssOpen, ssHalfClosedLocal, ssHalfClosedRemote] then
      Inc(Result);
end;

function TH2Connection.PeerInitialWindowSize: Cardinal;
var
  I: Integer;
begin
  // §6.9.2 - the peer's SETTINGS_INITIAL_WINDOW_SIZE is the receive window it advertises for new streams, default to 65535
  for I := 0 to High(FRemoteSettings) do
    if FRemoteSettings[I].Identifier = H2_SETTINGS_INITIAL_WINDOW_SIZE then
      Exit(FRemoteSettings[I].Value);
  Result := H2_DEFAULT_INITIAL_WINDOW_SIZE;
end;

procedure TH2Connection.IncreaseConnSendWindow(aIncrement: Cardinal);
begin
  // §6.9 - connection-scoped counterparts of TH2Stream.IncreaseSendWindow.
  if aIncrement = 0 then
    raise EH2ConnectionError.Create(ecProtocolError,
      'connection WINDOW_UPDATE with zero increment');
  if FSendWindow + Int64(aIncrement) > H2_MAX_WINDOW_SIZE then
    raise EH2ConnectionError.Create(ecFlowControlError,
      'connection WINDOW_UPDATE overflows the flow-control window');
  Inc(FSendWindow, aIncrement);
end;

procedure TH2Connection.ProcessConnectionFrame(const aHeader: TH2FrameHeader);
var
  LastStreamID, ErrorCode: Cardinal;
  Opaque: TBytes;
  Settings: TH2SettingsArray;
  NewIW: Cardinal;
  Delta: Int64;
  I: Integer;
  HasIW: Boolean;
begin
  // Stream 0 = connection control. PING is answered here (§6.7)
  case aHeader.FrameType of
    Ord(ftData), Ord(ftHeaders), Ord(ftRSTStream):
      raise EH2ConnectionError.Create(ecProtocolError,
        Format('frame type %d illegal on stream 0', [aHeader.FrameType]));
    Ord(ftSettings):
      begin
        // §6.5 - CAPTURE the parsed settings
        Settings := ReadSettingsFrame(FInput, aHeader);
        // SETTINGS flood budget - §6.5. Count only NON-ACK SETTINGS
        if (aHeader.Flags and H2_FLAG_ACK) = 0 then
        begin
          Inc(FSettingsCount);
          if FSettingsCount > FLimits.MaxSettingsFlood then
            raise EH2ConnectionError.Create(ecEnhanceYourCalm,
              Format('SETTINGS flood: %d > MaxSettingsFlood %d', [FSettingsCount, FLimits.MaxSettingsFlood]));
          // §6.5.2 - validate every defined setting's value before applying any
          for I := 0 to High(Settings) do
            if Settings[I].Identifier = H2_SETTINGS_ENABLE_PUSH then
            begin
              if Settings[I].Value > 1 then
                raise EH2ConnectionError.Create(ecProtocolError,
                  Format('SETTINGS_ENABLE_PUSH invalid value %d (§6.5.2)', [Settings[I].Value]));
            end
            else if Settings[I].Identifier = H2_SETTINGS_MAX_FRAME_SIZE then
            begin
              if (Settings[I].Value < H2_DEFAULT_MAX_FRAME_SIZE)
                 or (Settings[I].Value > H2_MAX_ALLOWED_FRAME_SIZE) then
                raise EH2ConnectionError.Create(ecProtocolError,
                  Format('SETTINGS_MAX_FRAME_SIZE out of range %d (§6.5.2)', [Settings[I].Value]));
            end;
          // §6.9.2 - apply ONLY SETTINGS_INITIAL_WINDOW_SIZE
          // (§6.5.3 requires the ACK, not full application).
          HasIW := False;
          NewIW := 0;
          for I := 0 to High(Settings) do
            if Settings[I].Identifier = H2_SETTINGS_INITIAL_WINDOW_SIZE then
            begin
              HasIW := True;
              NewIW := Settings[I].Value;
            end;
          if HasIW then
          begin
            // §6.5.2 - a value above 2^31-1 is a connection FLOW_CONTROL_ERROR
            if NewIW > H2_MAX_WINDOW_SIZE then
              raise EH2ConnectionError.Create(ecFlowControlError,
                'SETTINGS_INITIAL_WINDOW_SIZE above 2^31-1 (§6.5.2)');
            // §6.9.2 - adjust EVERY tracked stream's send window (any state) by the signed delta against the value currently in force
            Delta := Int64(NewIW) - Int64(FPeerInitialWindow);
            for I := 0 to FStreams.Count - 1 do
              TH2Stream(FStreams[I]).AdjustSendWindow(Delta);
            FPeerInitialWindow := NewIW;
            // An INCREASE may lift a stalled (or negative) window positive -> drain
            // the buffered sends (§6.9.1 resume). A decrease never unblocks.
            if Delta > 0 then
              ResumeSends;
          end;
          // §6.5.3 - a non-ACK post-bootstrap SETTINGS MUST beacknowledged with an empty-payload SETTINGS carrying the ACK flag
          SendFrame(SerializeSettings(nil, H2_FLAG_ACK));
        end;
      end;
    Ord(ftPing):
      begin
        // §6.7 - read the 8 opaque octets (the codec enforces Len=8). 
        Opaque := ReadPingFrame(FInput, aHeader);
        if (aHeader.Flags and H2_FLAG_ACK) = 0 then
        begin
          Inc(FPingCount);
          if FPingCount > FLimits.MaxPingFlood then
            raise EH2ConnectionError.Create(ecEnhanceYourCalm,
              Format('PING flood: %d > MaxPingFlood %d', [FPingCount, FLimits.MaxPingFlood]));
          SendPingAck(Opaque);
        end;
      end;
    Ord(ftWindowUpdate):
      // §6.9 - a stream-0 WINDOW_UPDATE grows the CONNECTION send window.
      // §6.9.1 - a connection WINDOW_UPDATE can unblock any/all stalled streams.
      begin
        IncreaseConnSendWindow(ReadWindowUpdateFrame(FInput, aHeader));
        ResumeSends;
      end;
    Ord(ftGoAway):
      ReadGoAwayFrame(FInput, aHeader, LastStreamID, ErrorCode);  // parse-and-ignore
  else
    // PRIORITY not yet handled.
    DiscardPayload(aHeader.Len);
  end;
end;

procedure TH2Connection.ValidateContentLength(aStream: TH2Stream);
var
  I, Code: Integer;
  CL: Int64;
  Found: Boolean;
begin
  // §8.1.2.6 - a request whose content-length disagrees with the sum of its DATA payloads is malformed.
  Found := False;
  CL := 0;
  for I := 0 to aStream.Headers.Count - 1 do
    if LowerCase(aStream.Headers[I]^.HeaderName) = 'content-length' then
    begin
      if Found then
        raise EH2StreamError.Create(ecProtocolError, aStream.ID,
          'duplicate content-length header (§8.1.2.6)');
      Found := True;
      Val(aStream.Headers[I]^.HeaderValue, CL, Code);
      if (Code <> 0) or (CL < 0) then
        raise EH2StreamError.Create(ecProtocolError, aStream.ID,
          'malformed content-length header (§8.1.2.6)');
    end;
  if Found and (CL <> Length(aStream.Body)) then
    raise EH2StreamError.Create(ecProtocolError, aStream.ID,
      Format('content-length %d <> DATA length %d (§8.1.2.6)', [CL, Length(aStream.Body)]));
end;

procedure TH2Connection.MaybeDispatch(aStream: TH2Stream);
begin
  // Fire ONCE, only when the peer has finished its side (END_STREAM applied).
  if aStream.Dispatched or not Assigned(FOnRequestReady) then
    Exit;
  if aStream.State in [ssHalfClosedRemote, ssClosed] then
  begin
    aStream.Dispatched := True;
    FOnRequestReady(Self, aStream.ID, aStream.Headers, aStream.Body);
  end;
end;

procedure TH2Connection.CreditProgress;
begin
  // A fully-received request is real work and credits back all three flood budgets.
  FResetCount := 0;
  FPingCount := 0;
  FSettingsCount := 0;
end;

procedure TH2Connection.ProcessStreamFrame(const aHeader: TH2FrameHeader);
var
  S: TH2Stream;
  FirstFrag, Frag, Payload: TBytes;
  WInc: Cardinal;
  EndStream, IsNew: Boolean;
  Trailers: THPackHeaderTextList;   // scratch decode target for a trailing HEADERS
  K: Integer;
begin
  case aHeader.FrameType of
    Ord(ftHeaders):
      begin
        // Read the first fragment, then reassemble any CONTINUATION frames (§6.10) into one complete header block
        FirstFrag := ReadHeadersFrame(FInput, aHeader);
        Frag := AssembleHeaderBlock(FInput, aHeader, FirstFrag,
          FLimits.MaxContinuationBytes, FLimits.MaxFrameSize);
        // Resolve the stream: a new id is validated + opened; an existing
        // one is trailers/continuation of an open stream.
        S := FindStream(aHeader.StreamID);
        IsNew := S = nil;
        if IsNew then
          S := OpenStream(aHeader.StreamID);
        EndStream := (aHeader.Flags and H2_FLAG_END_STREAM) <> 0;
        // A HEADERS on an already-OPEN stream is a TRAILING header block - trailers (§8.1).
        if (not IsNew) and (S.State = ssOpen) then
        begin
          Trailers := THPackHeaderTextList.Create;
          try
            DecodeHeaderBlock(Frag, Trailers);
            // §8.1 - a trailing HEADERS block MUST carry END_STREAM. one without it is a stream PROTOCOL_ERROR (8.1.1).
            if not EndStream then
              raise EH2StreamError.Create(ecProtocolError, aHeader.StreamID,
                'trailers (second HEADERS) without END_STREAM (§8.1)');
            // §8.1.2.1 - trailers MUST NOT contain pseudo-header fields (8.1.2.1.3).
            for K := 0 to Trailers.Count - 1 do
              if (Length(Trailers[K]^.HeaderName) > 0) and (Trailers[K]^.HeaderName[1] = ':') then
                raise EH2StreamError.Create(ecProtocolError, aHeader.StreamID,
                  'pseudo-header field in trailers (§8.1.2.1)');
          finally
            Trailers.Free;
          end;
          // Valid trailers end the client side: half-close and dispatch the buffered request
          S.RecvHeaders(True);
          ValidateContentLength(S);
          MaybeDispatch(S);
          CreditProgress;
        end
        else
        begin
          // Initial HEADERS - opens the stream (or rides END_STREAM to half-closed).
          DecodeHeaderBlock(Frag, S.Headers);
          // §5.1.2 MaxConcurrentStreams enforcement: a NEW stream that would push the active count above the limit is REFUSED_STREAM
          if IsNew and (Cardinal(ActiveStreamCount) >= FLimits.MaxConcurrentStreams) then
            raise EH2StreamError.Create(ecRefusedStream, aHeader.StreamID,
              Format('stream %d refused: active streams >= MaxConcurrentStreams %d',
                [aHeader.StreamID, FLimits.MaxConcurrentStreams]));
          // Apply the §5.1 receive transition (END_STREAM => half-closed remote),
          // then dispatch a body-less request whose END_STREAM rode on the HEADERS.
          S.RecvHeaders(EndStream);
          if EndStream then
          begin
            ValidateContentLength(S);   // §8.1.2.6, before dispatch
            MaybeDispatch(S);
            // A fully-received request is real work.
            CreditProgress;
          end;
        end;
      end;
    Ord(ftData):
      begin
        Payload := ReadDataFrame(FInput, aHeader);   // de-padded body bytes
        S := FindStream(aHeader.StreamID);
        if S = nil then
        begin
          // §5.1: a frame other than HEADERS/PRIORITY on an IDLE stream is a CONNECTION PROTOCOL_ERROR.
          // DATA on an already-closed lower id is the stream-scoped STREAM_CLOSED of §6.1.
          if aHeader.StreamID > FLastPeerStreamID then
            raise EH2ConnectionError.Create(ecProtocolError,
              Format('DATA on idle stream %d (no HEADERS opened it)', [aHeader.StreamID]))
          else
            raise EH2StreamError.Create(ecStreamClosed, aHeader.StreamID,
              'DATA on closed stream');
        end;
        // Retain the body (bounded) instead of discarding it, then apply the
        // §5.1 transition and dispatch on the final (END_STREAM) DATA frame.
        S.AppendBody(Payload);
        EndStream := (aHeader.Flags and H2_FLAG_END_STREAM) <> 0;
        S.RecvData(EndStream);
        if EndStream then
        begin
          ValidateContentLength(S);   // §8.1.2.6, before dispatch
          MaybeDispatch(S);
          CreditProgress;   // request fully received credits the flood budgets
        end;
      end;
    Ord(ftRSTStream):
      begin
        ReadRSTStreamFrame(FInput, aHeader);  // consume; error code unused
        S := FindStream(aHeader.StreamID);
        // §5.1: RST_STREAM on an IDLE stream (never opened) is a connection PROTOCOL_ERROR
        if (S = nil) and (aHeader.StreamID > FLastPeerStreamID) then
          raise EH2ConnectionError.Create(ecProtocolError,
            Format('RST_STREAM on idle stream %d (§5.1)', [aHeader.StreamID]));
        if Assigned(S) then
          S.RecvRSTStream;
        // Rapid-reset accounting - CVE-2023-44487.
        // Count ONLY a reset of an in-flight stream (opened-but-not-dispatched)
        if Assigned(S) and not S.Dispatched then
        begin
          Inc(FResetCount);
          if FResetCount > FLimits.MaxResetStreams then
            raise EH2ConnectionError.Create(ecEnhanceYourCalm,
              Format('rapid-reset budget exceeded: %d resets > MaxResetStreams %d (CVE-2023-44487)',
                [FResetCount, FLimits.MaxResetStreams]));
        end;
      end;
    Ord(ftWindowUpdate):
      begin
        // §6.9 - a stream WINDOW_UPDATE grows THAT stream's send window.
        WInc := ReadWindowUpdateFrame(FInput, aHeader);
        S := FindStream(aHeader.StreamID);
        // §5.1: WINDOW_UPDATE on an IDLE stream (never opened) is a connection PROTOCOL_ERROR; 
        if (S = nil) and (aHeader.StreamID > FLastPeerStreamID) then
          raise EH2ConnectionError.Create(ecProtocolError,
            Format('WINDOW_UPDATE on idle stream %d (§5.1)', [aHeader.StreamID]));
        if Assigned(S) then
        begin
          S.IncreaseSendWindow(WInc);
          // §6.9.1 - a stream WINDOW_UPDATE can unblock that streams buffered sends.
          ResumeSends;
        end;
      end;
    Ord(ftPriority):
      DiscardPayload(aHeader.Len);             // PRIORITY is a declared non-goal
    Ord(ftContinuation):
      // A CONTINUATION only ever appears INSIDE AssembleHeaderBlock;
      // standalone here is a protocol violation (§6.10).
      raise EH2ConnectionError.Create(ecProtocolError,
        'unexpected standalone CONTINUATION frame');
    Ord(ftSettings), Ord(ftPing), Ord(ftGoAway):
      // §6.5/§6.7/§6.8 - SETTINGS/PING/GOAWAY are connection-control frames that MUST be carried on stream 0;
      // on a non-zero stream they are a connection PROTOCOL_ERROR.
      raise EH2ConnectionError.Create(ecProtocolError,
        Format('connection-control frame type %d illegal on stream %d',
          [aHeader.FrameType, aHeader.StreamID]));
    Ord(ftPushPromise):
      // §8.4 - a client cannot push, so a server MUST treat the receipt of a PUSH_PROMISE frame as a connection PROTOCOL_ERROR.
      raise EH2ConnectionError.Create(ecProtocolError,
        'PUSH_PROMISE received by server (push is not enabled)');
  else
    // Unknown / other frame types on a stream: discard to stay byte-aligned.
    DiscardPayload(aHeader.Len);
  end;
end;

procedure TH2Connection.ProcessNextFrame;
var
  Hdr: TH2FrameHeader;
begin
  // Read ONE frame and route it. 
  try
    Hdr := ReadFrameHeader(FInput);
    // Negotiated SETTINGS_MAX_FRAME_SIZE enforcement (§4.2). 
    if Hdr.Len > FLimits.MaxFrameSize then
    begin
      if (Hdr.StreamID = 0)
         or (Hdr.FrameType in [Ord(ftHeaders), Ord(ftContinuation),
                               Ord(ftSettings), Ord(ftPushPromise)]) then
        raise EH2ConnectionError.Create(ecFrameSizeError,
          Format('frame length %d exceeds MaxFrameSize %d (connection scope)',
            [Hdr.Len, FLimits.MaxFrameSize]))
      else
      begin
        DiscardPayload(Hdr.Len);
        raise EH2StreamError.Create(ecFrameSizeError, Hdr.StreamID,
          Format('frame length %d exceeds MaxFrameSize %d (stream scope)',
            [Hdr.Len, FLimits.MaxFrameSize]));
      end;
    end;
    if Hdr.StreamID = 0 then
      ProcessConnectionFrame(Hdr)
    else
    begin
      ProcessStreamFrame(Hdr);
      // §6.8 - a peer-initiated (odd) stream whose frame we processed without raising advances the GOAWAY last-stream-id.
      if ((Hdr.StreamID and 1) = 1) and (Hdr.StreamID > FLastProcessedPeerStreamID) then
        FLastProcessedPeerStreamID := Hdr.StreamID;
    end;
  except
    on E: EH2FrameError do
      raise EH2ConnectionError.Create(E.ErrorCode, 'frame error: ' + E.Message);
  end;
end;

end.
