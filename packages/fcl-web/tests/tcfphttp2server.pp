unit tcfphttp2server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, base64, fpcunit, testregistry,
  httpdefs, httpprotocol, fphttpserver,
  fphttp2connection, fphttp2frames, fphttp2consts, fphttp2request, fphttp2server,
  uhpack;

type

  // In-memory duplex TStream (independent copy of the tcfphttp2request idiom):
  // Read pops from a preloaded input vector; Write appends to an output buffer.
  TDuplexStream = class(TStream)
  private
    FInput: TBytes;
    FInputPos: Integer;
    FOutput: TMemoryStream;
  public
    constructor Create(const aInput: TBytes);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function OutputBytes: TBytes;
  end;

  { Story 2.6 - a blocking, closeable in-memory transport (NFR7, no real socket).
    Read serves the preloaded input, then BLOCKS on an event until Close is called
    (after which Read returns 0, surfacing transport-end). Write appends to the
    output buffer. It signals FReachedBlock the first time it blocks so a test can
    wait until the worker is parked in the frame-read loop (Start finished) before
    deactivating. The event WaitFor is bounded (safety net) so a regression where
    CloseTransport never unblocks the worker fails SLOWLY rather than hanging the
    suite forever. }

  TBlockingDuplexStream = class(TStream)
  private
    FInput: TBytes;
    FInputPos: Integer;
    FOutput: TMemoryStream;
    FUnblock: TSimpleEvent;       // signaled by Close
    FReachedBlock: TSimpleEvent;  // set when Read first blocks (worker parked)
    FClosed: Boolean;
  public
    constructor Create(const aInput: TBytes);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    // Close the transport: subsequent (and any blocked) Read returns 0.
    procedure Close;
    // Block until the worker has parked in the frame-read loop (or timeout).
    function WaitUntilBlocked(aTimeoutMs: Cardinal): Boolean;
    function OutputBytes: TBytes;
  end;

  { Story 2.6 - handler subclass whose CloseTransport unblocks the in-memory mock
    (no real socket to close). It also snapshots the mock's output at close time
    (BEFORE ConnectionDone frees the mock object) so a test can assert the graceful
    GOAWAY was emitted. }

  TTestableH2Handler = class(TFPHTTP2Handler)
  private
    FMock: TBlockingDuplexStream;
    FCapturedOutput: TBytes;
  protected
    procedure CloseTransport(aSocket: TStream); override;
  public
    property Mock: TBlockingDuplexStream read FMock write FMock;
    // True if the captured transport output contains a GOAWAY frame.
    function CapturedOutputHasGoAway: Boolean;
  end;

  { Story 3.1 - surfaces the protected h2c upgrade predicate so the socket-free
    test can drive it with a hand-built request. }
  THTTP2HandlerProbe = class(TFPHTTP2Handler)
  public
    function ProbeCheckH2C(aRequest: TFPHTTPConnectionRequest): Boolean;
  end;

  { Story 4.6 - surfaces the protected ALPN ingress detector so the socket-free
    NFR1-gate tests can drive it with a hand-built (socketless) connection. }
  THTTP2ServerProbe = class(TFPCustomHttpServer)
  public
    function ProbeCheckHTTP2ALPN(aConnection: TFPHTTPConnection): Boolean;
  end;

  TTestHTTP2Handler = class(TTestCase)
  private
    // --- captured state from the wired OnRequest handler ---
    FSeenMethod, FSeenPath: string;
    FSeenCount: Integer;
    // Story 3.2 - extra dispatch-time captures for the h2c stream-1 cases: the body
    // (AC#9c), the :authority-mapped Host, and selected regular headers (AC#9b).
    FSeenContent, FSeenAuthority: string;
    FSeenXTest, FSeenConnection, FSeenUpgrade, FSeenH2Settings: string;
    // --- byte / framing helpers (independent copies of tcfphttp2request's) ---
    function Bytes(const aBytes: array of Byte): TBytes;
    function Concat(const aHead, aTail: TBytes): TBytes;
    function ClientPreface: TBytes;
    function SettingsFrameBytes(const aSettings: TH2SettingsArray; aFlags: Byte = 0): TBytes;
    function ClientSettings: TBytes;
    function ValidClientInput: TBytes;
    function HeadersFrameBytes(aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0): TBytes;
    function EncodeBlock(aList: THPackHeaderTextList): TBytes;
    function RequestHeaders(const aMethod, aPath: string): THPackHeaderTextList;
    // A GET request HEADERS frame (stream 1, END_STREAM) for aPath.
    function GetRequestFrame(const aPath: string): TBytes;
    // Output positioned just past the two bootstrap SETTINGS frames Start emits.
    function OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
    // Decode a HEADERS fragment with a fresh connection's decoder.
    function DecodeStatus(const aFragment: TBytes): string;
    // --- Story 3.1 h2c helpers (socket-free) ---
    // The id/value SETTINGS payload (the framed-frame minus its 9-byte header).
    function SettingsPayload(const aSettings: TH2SettingsArray): TBytes;
    // base64-encode bytes; aURL => base64url alphabet with padding stripped.
    function B64(const aBytes: TBytes; aURL: Boolean): string;
    // A request carrying `Upgrade: aUpgrade` and (optionally) HTTP2-Settings.
    function H2CRequest(const aUpgrade, aSettingsValue: string;
      aWithSettings: Boolean): TFPHTTPConnectionRequest;
    // OnRequest handler wired through the server pipeline.
    procedure HandleTestRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  protected
    procedure SetUp; override;
  published
    procedure TestEndToEndDispatchThroughAdoptConnection;   // (a)
    procedure TestSeedBytesAreHonored;                      // (b)
    procedure TestPriorKnowledgeReconstruction;             // (b') 18-byte split
    procedure TestConnectionRegisteredThenRemoved;          // (c)
    procedure TestActiveWithoutWebServerRaises;             // (d)
    procedure TestAdoptConnectionWhenInactiveRaises;        // (d)
    procedure TestSetThreadModeWhileActiveRaises;           // (d)
    procedure TestSetWebServerWhileActiveRaises;            // (d)
    // Story 2.6 lifecycle/teardown
    procedure TestInlineDeactivateAfterFiniteAdopt;         // (e)
    procedure TestThreadModeDeterministicJoinNoLeak;        // (f)
    // Story 3.1 h2c upgrade - socket-free unit cases (AC#9)
    procedure TestTryDecodeHTTP2SettingsRoundTrip;          // (a)
    procedure TestTryDecodeHTTP2SettingsRejectsBad;         // (a)
    procedure TestDoCheckH2CUpgradePredicate;               // (b)
    procedure Test101SwitchingResponseBytes;                // (c)
    procedure TestApplyUpgradeSettings;                     // (d)
    // Story 3.2 h2c initial-request -> stream-1 end-to-end (AC#9)
    procedure TestH2CUpgradeDispatchEndToEnd;               // (a)
    procedure TestH2CUpgradeHeaderCleaning;                 // (b)
    procedure TestH2CUpgradePostBodyBecomesStream1Body;     // (c)
    // Story 4.6 TLS-ALPN ingress - socket-free NFR1/NFR3 gate cases (AC#2/#5/#1)
    procedure TestNegotiatedProtocolEmptyWithoutSSL;        // (a) AC#2
    procedure TestCheckHTTP2ALPNFalseWithoutHandler;        // (b) AC#5 no-handler
    procedure TestCheckHTTP2ALPNFalseWhenNotH2;             // (c) AC#5 non-h2 gate
    procedure TestActivateAdvertisesALPNAndRestores;        // (d) AC#1 advertise/restore
    procedure TestActivateHonorsUserALPNList;               // (e) AC#1 honor-user-list
  end;

implementation

{ TDuplexStream }

constructor TDuplexStream.Create(const aInput: TBytes);
begin
  inherited Create;
  FInput := aInput;
  FInputPos := 0;
  FOutput := TMemoryStream.Create;
end;

destructor TDuplexStream.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

function TDuplexStream.Read(var Buffer; Count: LongInt): LongInt;
var
  Avail: Integer;
begin
  Avail := Length(FInput) - FInputPos;
  if Avail <= 0 then
    Exit(0);
  if Count < Avail then
    Result := Count
  else
    Result := Avail;
  Move(FInput[FInputPos], Buffer, Result);
  Inc(FInputPos, Result);
end;

function TDuplexStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := FOutput.Write(Buffer, Count);
end;

function TDuplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;   // not seekable; the connection never seeks the transport
end;

function TDuplexStream.OutputBytes: TBytes;
begin
  SetLength(Result, FOutput.Size);
  if FOutput.Size > 0 then
    Move(FOutput.Memory^, Result[0], FOutput.Size);
end;

{ TBlockingDuplexStream }

const
  // Safety net only - on the correct path Close unblocks Read at once. A regression
  // that never unblocks the worker fails after this delay instead of hanging.
  BLOCKING_READ_SAFETY_MS = 10000;

constructor TBlockingDuplexStream.Create(const aInput: TBytes);
begin
  inherited Create;
  FInput := aInput;
  FInputPos := 0;
  FOutput := TMemoryStream.Create;
  FUnblock := TSimpleEvent.Create;
  FReachedBlock := TSimpleEvent.Create;
  FClosed := False;
end;

destructor TBlockingDuplexStream.Destroy;
begin
  FUnblock.Free;
  FReachedBlock.Free;
  FOutput.Free;
  inherited Destroy;
end;

function TBlockingDuplexStream.Read(var Buffer; Count: LongInt): LongInt;
var
  Avail: Integer;
begin
  // Serve any remaining preloaded input first (preface + SETTINGS during Start).
  Avail := Length(FInput) - FInputPos;
  if Avail > 0 then
  begin
    if Count < Avail then
      Result := Count
    else
      Result := Avail;
    Move(FInput[FInputPos], Buffer, Result);
    Inc(FInputPos, Result);
    Exit;
  end;
  if FClosed then
    Exit(0);
  // Out of input and still open: the worker is parked here waiting for the next
  // frame. Announce it, then block until Close (or the safety timeout) wakes us.
  FReachedBlock.SetEvent;
  FUnblock.WaitFor(BLOCKING_READ_SAFETY_MS);
  // Whether closed or merely timed out, return 0 → the frame loop sees transport
  // end and exits cleanly (EStreamError → RunConnection's clean arm).
  Result := 0;
end;

function TBlockingDuplexStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := FOutput.Write(Buffer, Count);
end;

function TBlockingDuplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;   // not seekable
end;

procedure TBlockingDuplexStream.Close;
begin
  FClosed := True;
  FUnblock.SetEvent;
end;

function TBlockingDuplexStream.WaitUntilBlocked(aTimeoutMs: Cardinal): Boolean;
begin
  Result := FReachedBlock.WaitFor(aTimeoutMs) = wrSignaled;
end;

function TBlockingDuplexStream.OutputBytes: TBytes;
begin
  SetLength(Result, FOutput.Size);
  if FOutput.Size > 0 then
    Move(FOutput.Memory^, Result[0], FOutput.Size);
end;

{ TTestableH2Handler }

procedure TTestableH2Handler.CloseTransport(aSocket: TStream);
begin
  // The mock is not a TSocketStream, so the production CloseTransport would no-op.
  // Snapshot its output (the graceful GOAWAY is already written by the preceding
  // BeginShutdown) BEFORE unblocking - ConnectionDone frees the object afterwards.
  if (aSocket <> nil) and (aSocket = FMock) then
  begin
    FCapturedOutput := FMock.OutputBytes;
    FMock.Close;
  end
  else
    inherited CloseTransport(aSocket);
end;

function TTestableH2Handler.CapturedOutputHasGoAway: Boolean;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  Result := False;
  S := TBytesStream.Create;
  try
    if Length(FCapturedOutput) > 0 then
      S.WriteBuffer(FCapturedOutput[0], Length(FCapturedOutput));
    S.Position := 0;
    while (S.Size - S.Position) >= 9 do   // 9 = frame header size
    begin
      H := ReadFrameHeader(S);
      if H.FrameType = Ord(ftGoAway) then
        Exit(True);
      S.Position := S.Position + H.Len;   // skip payload
    end;
  finally
    S.Free;
  end;
end;

{ helpers }

procedure TTestHTTP2Handler.SetUp;
begin
  inherited SetUp;
  FSeenMethod := '';
  FSeenPath := '';
  FSeenCount := 0;
  FSeenContent := '';
  FSeenAuthority := '';
  FSeenXTest := '';
  FSeenConnection := '';
  FSeenUpgrade := '';
  FSeenH2Settings := '';
end;

function TTestHTTP2Handler.Bytes(const aBytes: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(aBytes));
  for I := 0 to High(aBytes) do
    Result[I] := aBytes[I];
end;

function TTestHTTP2Handler.Concat(const aHead, aTail: TBytes): TBytes;
begin
  SetLength(Result, Length(aHead) + Length(aTail));
  if Length(aHead) > 0 then
    Move(aHead[0], Result[0], Length(aHead));
  if Length(aTail) > 0 then
    Move(aTail[0], Result[Length(aHead)], Length(aTail));
end;

function TTestHTTP2Handler.ClientPreface: TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(H2_CLIENT_PREFACE));
  for I := 0 to High(H2_CLIENT_PREFACE) do
    Result[I] := H2_CLIENT_PREFACE[I];
end;

function TTestHTTP2Handler.SettingsFrameBytes(const aSettings: TH2SettingsArray;
  aFlags: Byte = 0): TBytes;
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

function TTestHTTP2Handler.ClientSettings: TBytes;
var
  Settings: TH2SettingsArray;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  Settings[0].Value := 100;
  Result := SettingsFrameBytes(Settings, 0);
end;

function TTestHTTP2Handler.ValidClientInput: TBytes;
begin
  Result := Concat(ClientPreface, ClientSettings);
end;

function TTestHTTP2Handler.HeadersFrameBytes(aStreamID: Cardinal;
  const aFragment: TBytes; aFlags: Byte = 0): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WriteHeadersFrame(S, aStreamID, aFragment, aFlags or H2_FLAG_END_HEADERS);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Handler.EncodeBlock(aList: THPackHeaderTextList): TBytes;
var
  Enc: THPackEncoder;
  S: TBytesStream;
  I: Integer;
begin
  Enc := THPackEncoder.Create;
  S := TBytesStream.Create;
  try
    for I := 0 to aList.Count - 1 do
      Enc.EncodeHeader(S, aList[I]^.HeaderName, aList[I]^.HeaderValue, False);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
    Enc.Free;
  end;
end;

function TTestHTTP2Handler.RequestHeaders(const aMethod, aPath: string): THPackHeaderTextList;
begin
  Result := THPackHeaderTextList.Create;
  Result.Add(H2_PSEUDO_METHOD, aMethod);
  Result.Add(H2_PSEUDO_SCHEME, 'http');
  Result.Add(H2_PSEUDO_AUTHORITY, 'example.com');
  Result.Add(H2_PSEUDO_PATH, aPath);
end;

function TTestHTTP2Handler.GetRequestFrame(const aPath: string): TBytes;
var
  List: THPackHeaderTextList;
begin
  List := RequestHeaders('GET', aPath);
  try
    Result := HeadersFrameBytes(1, EncodeBlock(List), H2_FLAG_END_STREAM);
  finally
    List.Free;
  end;
end;

function TTestHTTP2Handler.OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
var
  H: TH2FrameHeader;
  Outp: TBytes;
begin
  // Snapshot the output ONCE and guard the empty case before indexing [0].
  Outp := aDup.OutputBytes;
  Result := TBytesStream.Create;
  if Length(Outp) > 0 then
    Result.WriteBuffer(Outp[0], Length(Outp));
  Result.Position := 0;
  // Skip the server SETTINGS + the SETTINGS ACK that Start emits.
  H := ReadFrameHeader(Result);
  ReadSettingsFrame(Result, H);
  H := ReadFrameHeader(Result);
  ReadSettingsFrame(Result, H);
end;

function TTestHTTP2Handler.DecodeStatus(const aFragment: TBytes): string;
var
  Dup2: TDuplexStream;
  Conn2: TH2Connection;
  Decoded: THPackHeaderTextList;
begin
  Result := '';
  Dup2 := TDuplexStream.Create(nil);
  try
    Conn2 := TH2Connection.Create(Dup2);
    try
      Decoded := THPackHeaderTextList.Create;
      try
        Conn2.DecodeHeaderBlock(aFragment, Decoded);
        AssertTrue('decoded at least one header', Decoded.Count >= 1);
        AssertEquals(':status is first', H2_PSEUDO_STATUS, Decoded[0]^.HeaderName);
        Result := Decoded[0]^.HeaderValue;
      finally
        Decoded.Free;
      end;
    finally
      Conn2.Free;
    end;
  finally
    Dup2.Free;
  end;
end;

procedure TTestHTTP2Handler.HandleTestRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  Inc(FSeenCount);
  FSeenMethod := ARequest.Method;
  FSeenPath := ARequest.PathInfo;
  // Story 3.2 - capture body + selected headers at dispatch time (the request is
  // freed right after this returns). :authority surfaces as Host; the dropped
  // connection-specific / upgrade-machinery headers must read back as ''.
  FSeenContent := ARequest.Content;
  FSeenAuthority := ARequest.Host;
  FSeenXTest := ARequest.GetFieldByName('x-test');
  FSeenConnection := ARequest.GetFieldByName('connection');
  FSeenUpgrade := ARequest.GetFieldByName('upgrade');
  FSeenH2Settings := ARequest.GetFieldByName('http2-settings');
  AResponse.Code := 200;
  AResponse.Content := 'Hi there';
end;

{ (a) End-to-end through AdoptConnection: a bare TFPCustomHttpServer whose
  OnRequest sets Code/Content, a TFPHTTP2Handler with WebServer set and
  ThreadMode := h2tmNone, Active := True, then AdoptConnection over an in-memory
  duplex stream. The inline run drains the finite input, the handler saw the
  mapped request, and the framed response carries HEADERS(:status 200) on
  stream 1 - proving create+seed+wire+dispatch+frame all happen INSIDE
  AdoptConnection. }

procedure TTestHTTP2Handler.TestEndToEndDispatchThroughAdoptConnection;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag: TBytes;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Dup := TDuplexStream.Create(Concat(ValidClientInput, GetRequestFrame('/index')));
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    Handler.AdoptConnection(Dup, amPriorKnowledge, nil, nil);

    // The handler saw the mapped request fields.
    AssertEquals('OnRequest fired exactly once', 1, FSeenCount);
    AssertEquals('handler saw mapped method', 'GET', FSeenMethod);
    AssertEquals('handler saw mapped path', '/index', FSeenPath);

    // The framed HTTP/2 response is in the output: HEADERS(:status 200) first.
    Outp := OutputAfterBootstrap(Dup);
    try
      H := ReadFrameHeader(Outp);
      AssertEquals('response frame is HEADERS', Ord(ftHeaders), H.FrameType);
      AssertEquals('response HEADERS on stream 1', 1, Integer(H.StreamID));
      Frag := ReadHeadersFrame(Outp, H);
      AssertEquals('decoded :status is 200', '200', DecodeStatus(Frag));
    finally
      Outp.Free;
    end;
  finally
    // h2tmNone: the test owns and frees Dup (the handler did not free the socket).
    Handler.Free;
    Server.Free;
    Dup.Free;
  end;
end;

{ (b) Seed is honored (FR0): the client preface is passed as aBufferedBytes and
  the SETTINGS+HEADERS as the transport input. The same successful dispatch
  proves Seed bytes are consumed before the transport. }

procedure TTestHTTP2Handler.TestSeedBytesAreHonored;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag: TBytes;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  // Preface in the seed; SETTINGS + HEADERS on the transport.
  Dup := TDuplexStream.Create(Concat(ClientSettings, GetRequestFrame('/seeded')));
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    Handler.AdoptConnection(Dup, amPriorKnowledge, nil, ClientPreface);

    AssertEquals('OnRequest fired exactly once', 1, FSeenCount);
    AssertEquals('handler saw mapped method', 'GET', FSeenMethod);
    AssertEquals('handler saw seeded path', '/seeded', FSeenPath);

    Outp := OutputAfterBootstrap(Dup);
    try
      H := ReadFrameHeader(Outp);
      AssertEquals('response frame is HEADERS', Ord(ftHeaders), H.FrameType);
      Frag := ReadHeadersFrame(Outp, H);
      AssertEquals('decoded :status is 200', '200', DecodeStatus(Frag));
    finally
      Outp.Free;
    end;
  finally
    Handler.Free;
    Server.Free;
    Dup.Free;
  end;
end;

{ (b') Prior-knowledge FR0 reconstruction (Story 1.11 AC#8): proves the EXACT
  hand-off the server seam produces. The HTTP/1.1 layer consumes the first 18
  octets of the preface (start line + blank line), so DoAdoptPriorKnowledge
  rebuilds the seed as H2_CLIENT_PREFACE[0..17] + residual. This test splits the
  input at that 18-byte boundary: H2_CLIENT_PREFACE[0..17] is passed as
  aBufferedBytes (the reconstructed consumed head) and the tail 'SM'CRLFCRLF
  (bytes 18..23) + client SETTINGS + a GET HEADERS(stream 1, END_STREAM) is the
  transport input. Start must accept the rejoined 24-octet preface, dispatch the
  request through the shared OnRequest, and emit HEADERS(:status 200) on stream 1. }

procedure TTestHTTP2Handler.TestPriorKnowledgeReconstruction;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag: TBytes;
  Preface, ConsumedHead, PrefaceTail, TransportInput: TBytes;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Preface := ClientPreface;
  // Split exactly where ReadRequestHeaders leaves off: bytes 0..17 are the
  // consumed start line + blank line; bytes 18..23 ('SM'CRLFCRLF) remain.
  ConsumedHead := Copy(Preface, 0, 18);
  PrefaceTail := Copy(Preface, 18, Length(Preface) - 18);
  TransportInput := Concat(Concat(PrefaceTail, ClientSettings), GetRequestFrame('/pk'));
  Dup := TDuplexStream.Create(TransportInput);
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    // Seed = reconstructed consumed head; transport = preface tail + frames.
    Handler.AdoptConnection(Dup, amPriorKnowledge, nil, ConsumedHead);

    // No preface error (the reconstruction validated) AND the request dispatched.
    AssertEquals('OnRequest fired exactly once', 1, FSeenCount);
    AssertEquals('handler saw mapped method', 'GET', FSeenMethod);
    AssertEquals('handler saw reconstructed-preface path', '/pk', FSeenPath);

    Outp := OutputAfterBootstrap(Dup);
    try
      H := ReadFrameHeader(Outp);
      AssertEquals('response frame is HEADERS', Ord(ftHeaders), H.FrameType);
      AssertEquals('response HEADERS on stream 1', 1, Integer(H.StreamID));
      Frag := ReadHeadersFrame(Outp, H);
      AssertEquals('decoded :status is 200', '200', DecodeStatus(Frag));
    finally
      Outp.Free;
    end;
  finally
    Handler.Free;
    Server.Free;
    Dup.Free;
  end;
end;

{ (c) The connection is added to the list and removed+freed on loop exit: after
  the inline run completes, ConnectionCount is back to 0. }

procedure TTestHTTP2Handler.TestConnectionRegisteredThenRemoved;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Dup := TDuplexStream.Create(Concat(ValidClientInput, GetRequestFrame('/index')));
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    AssertEquals('no live connections before adopt', 0, Handler.ConnectionCount);
    Handler.AdoptConnection(Dup, amPriorKnowledge, nil, nil);
    AssertEquals('connection removed after inline run', 0, Handler.ConnectionCount);
  finally
    Handler.Free;
    Server.Free;
    Dup.Free;
  end;
end;

{ (d) Guard rails. }

procedure TTestHTTP2Handler.TestActiveWithoutWebServerRaises;
var
  Handler: TFPHTTP2Handler;
  Raised: Boolean;
begin
  Handler := TFPHTTP2Handler.Create(nil);
  try
    Raised := False;
    try
      Handler.Active := True;
    except
      on E: EHTTP2Handler do
        Raised := True;
    end;
    AssertTrue('Active:=True with WebServer=nil raises EHTTP2Handler', Raised);
    AssertFalse('handler not active after the failed activation', Handler.Active);
  finally
    Handler.Free;
  end;
end;

procedure TTestHTTP2Handler.TestAdoptConnectionWhenInactiveRaises;
var
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Raised: Boolean;
begin
  Handler := TFPHTTP2Handler.Create(nil);
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Raised := False;
    try
      Handler.AdoptConnection(Dup, amPriorKnowledge, nil, nil);
    except
      on E: EHTTP2Handler do
        Raised := True;
    end;
    AssertTrue('AdoptConnection while inactive raises EHTTP2Handler', Raised);
  finally
    Handler.Free;
    Dup.Free;
  end;
end;

procedure TTestHTTP2Handler.TestSetThreadModeWhileActiveRaises;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Raised: Boolean;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  try
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;
    Raised := False;
    try
      Handler.ThreadMode := h2tmThread;
    except
      on E: EHTTP2Handler do
        Raised := True;
    end;
    AssertTrue('setting ThreadMode while Active raises EHTTP2Handler', Raised);
    Handler.Active := False;
  finally
    Handler.Free;
    Server.Free;
  end;
end;

procedure TTestHTTP2Handler.TestSetWebServerWhileActiveRaises;
var
  Server, Server2: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Raised: Boolean;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Server2 := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  try
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;
    Raised := False;
    try
      Handler.WebServer := Server2;
    except
      on E: EHTTP2Handler do
        Raised := True;
    end;
    AssertTrue('setting WebServer while Active raises EHTTP2Handler', Raised);
    Handler.Active := False;
  finally
    Handler.Free;
    Server.Free;
    Server2.Free;
  end;
end;

{ (e) Story 2.6 - inline (h2tmNone) teardown regression. After a finite inline
  adopt drains the connection, Active:=False must succeed cleanly (the SetActive
  rework - CloseConnections over the empty registry, the join loop over no
  threads, the dispatcher free - must not disturb the inline path) with
  ConnectionCount still 0 and no exception. }

procedure TTestHTTP2Handler.TestInlineDeactivateAfterFiniteAdopt;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Dup := TDuplexStream.Create(Concat(ValidClientInput, GetRequestFrame('/x')));
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    Handler.AdoptConnection(Dup, amPriorKnowledge, nil, nil);
    AssertEquals('inline run drained the connection', 0, Handler.ConnectionCount);

    Handler.Active := False;   // the SetActive(False) rework must not break inline
    AssertEquals('still 0 connections after deactivate', 0, Handler.ConnectionCount);
    AssertFalse('handler inactive after deactivate', Handler.Active);
  finally
    // h2tmNone: the test owns Dup (the handler did not free the socket).
    Handler.Free;
    Server.Free;
    Dup.Free;
  end;
end;

{ (f) Story 2.6 - the core AC: a deterministic worker-thread JOIN with NO leak.
  Adopt ONE connection in h2tmThread over a blocking, closeable in-memory mock
  (valid preface+SETTINGS, then it blocks). Once the worker has parked in the
  frame-read loop, ConnectionCount=1. Active:=False sends a graceful GOAWAY,
  closes the mock (CloseTransport override) to UNBLOCK the worker, and WaitFors
  the thread - which would hang forever if the worker were orphaned or never
  unblocked. On return ConnectionCount=0 (the worker's ConnectionDone removed +
  freed it) and a GOAWAY is present in the captured transport output. }

procedure TTestHTTP2Handler.TestThreadModeDeterministicJoinNoLeak;
var
  Server: TFPCustomHttpServer;
  Handler: TTestableH2Handler;
  Mock: TBlockingDuplexStream;
begin
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TTestableH2Handler.Create(nil);
  Mock := TBlockingDuplexStream.Create(ValidClientInput);
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmThread;
    Handler.Mock := Mock;          // CloseTransport closes THIS mock
    Handler.Active := True;

    Handler.AdoptConnection(Mock, amPriorKnowledge, nil, nil);
    // Wait until the worker finished Start and is blocked in the frame read, so
    // the connection is established and bootstrap output is written.
    AssertTrue('worker reached the blocking frame read',
      Mock.WaitUntilBlocked(5000));
    AssertEquals('one live connection while the worker is blocked',
      1, Handler.ConnectionCount);

    // The deterministic proof: deactivate must JOIN the worker and return.
    Handler.Active := False;
    AssertEquals('worker joined; no leaked connection', 0, Handler.ConnectionCount);
    AssertFalse('handler inactive after deactivate', Handler.Active);
    AssertTrue('graceful GOAWAY present in transport output',
      Handler.CapturedOutputHasGoAway);
  finally
    // Thread mode: ConnectionDone already freed the mock - do NOT free it here.
    Handler.Free;
    Server.Free;
  end;
end;

{ THTTP2HandlerProbe }

function THTTP2HandlerProbe.ProbeCheckH2C(aRequest: TFPHTTPConnectionRequest): Boolean;
begin
  Result := False;
  DoCheckH2CUpgrade(aRequest, Result);
end;

{ THTTP2ServerProbe }

function THTTP2ServerProbe.ProbeCheckHTTP2ALPN(aConnection: TFPHTTPConnection): Boolean;
begin
  Result := CheckHTTP2ALPN(aConnection);
end;

{ --- Story 3.1 h2c helpers --- }

function TTestHTTP2Handler.SettingsPayload(const aSettings: TH2SettingsArray): TBytes;
var
  Frame: TBytes;
begin
  // SettingsFrameBytes writes a full SETTINGS frame; the payload (id/value pairs)
  // is everything past the fixed 9-byte frame header.
  Frame := SettingsFrameBytes(aSettings, 0);
  Result := Copy(Frame, 9, Length(Frame) - 9);
end;

function TTestHTTP2Handler.B64(const aBytes: TBytes; aURL: Boolean): string;
var
  Raw: AnsiString;
  I: Integer;
begin
  SetLength(Raw, Length(aBytes));
  if Length(aBytes) > 0 then
    Move(aBytes[0], Raw[1], Length(aBytes));   // TBytes 0-based -> AnsiString 1-based
  Result := EncodeStringBase64(Raw);           // standard base64, padded
  if aURL then
  begin
    for I := 1 to Length(Result) do
      case Result[I] of
        '+': Result[I] := '-';
        '/': Result[I] := '_';
      end;
    // base64url omits padding (RFC 4648 §5)
    while (Result <> '') and (Result[Length(Result)] = '=') do
      SetLength(Result, Length(Result) - 1);
  end;
end;

function TTestHTTP2Handler.H2CRequest(const aUpgrade, aSettingsValue: string;
  aWithSettings: Boolean): TFPHTTPConnectionRequest;
begin
  Result := TFPHTTPConnectionRequest.Create;
  Result.Upgrade := aUpgrade;
  if aWithSettings then
    Result.SetFieldByName('HTTP2-Settings', aSettingsValue);
end;

{ (a) base64url HTTP2-Settings round-trips to the exact TH2SettingsArray, and a
  standard padded base64 value of the same payload decodes identically. }

procedure TTestHTTP2Handler.TestTryDecodeHTTP2SettingsRoundTrip;
var
  Settings, Decoded: TH2SettingsArray;
  Payload: TBytes;
  Req: TFPHTTPConnectionRequest;
begin
  SetLength(Settings, 2);
  Settings[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  Settings[0].Value := 100;
  Settings[1].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[1].Value := 65535;
  Payload := SettingsPayload(Settings);

  // base64url (no padding) decodes to the exact pairs.
  Req := H2CRequest('h2c', B64(Payload, True), True);
  try
    AssertTrue('base64url HTTP2-Settings decodes', TryDecodeHTTP2Settings(Req, Decoded));
    AssertEquals('two settings decoded', 2, Length(Decoded));
    AssertEquals('id[0] MAX_CONCURRENT_STREAMS',
      Integer(H2_SETTINGS_MAX_CONCURRENT_STREAMS), Integer(Decoded[0].Identifier));
    AssertEquals('value[0]', 100, Integer(Decoded[0].Value));
    AssertEquals('id[1] INITIAL_WINDOW_SIZE',
      Integer(H2_SETTINGS_INITIAL_WINDOW_SIZE), Integer(Decoded[1].Identifier));
    AssertEquals('value[1]', 65535, Integer(Decoded[1].Value));
  finally
    Req.Free;
  end;

  // The SAME payload as standard padded base64 decodes identically (alphabet +
  // padding tolerance).
  Req := H2CRequest('h2c', B64(Payload, False), True);
  try
    AssertTrue('padded standard base64 also decodes', TryDecodeHTTP2Settings(Req, Decoded));
    AssertEquals('same count from padded base64', 2, Length(Decoded));
    AssertEquals('same id[0] from padded base64',
      Integer(H2_SETTINGS_MAX_CONCURRENT_STREAMS), Integer(Decoded[0].Identifier));
    AssertEquals('same value[1] from padded base64', 65535, Integer(Decoded[1].Value));
  finally
    Req.Free;
  end;
end;

{ (a) reject paths: absent, length-not-multiple-of-6, and non-base64 values all
  return False (no upgrade - never an error, per AC#2/RFC 7540 §3.2.1). }

procedure TTestHTTP2Handler.TestTryDecodeHTTP2SettingsRejectsBad;
var
  Decoded: TH2SettingsArray;
  Req: TFPHTTPConnectionRequest;
begin
  // Absent header.
  Req := H2CRequest('h2c', '', False);
  try
    AssertFalse('absent HTTP2-Settings rejected', TryDecodeHTTP2Settings(Req, Decoded));
  finally
    Req.Free;
  end;

  // 4 bytes -> length not a multiple of 6.
  Req := H2CRequest('h2c', B64(Bytes([1, 2, 3, 4]), True), True);
  try
    AssertFalse('len mod 6 <> 0 rejected', TryDecodeHTTP2Settings(Req, Decoded));
  finally
    Req.Free;
  end;

  // Non-base64url characters ('!') -> rejected by the alphabet guard.
  Req := H2CRequest('h2c', 'not_base64!!', True);
  try
    AssertFalse('non-base64 value rejected', TryDecodeHTTP2Settings(Req, Decoded));
  finally
    Req.Free;
  end;
end;

{ (b) DoCheckH2CUpgrade: True only for `Upgrade: h2c` + a valid HTTP2-Settings;
  False for a different token, an absent header, or a malformed value. }

procedure TTestHTTP2Handler.TestDoCheckH2CUpgradePredicate;
var
  Probe: THTTP2HandlerProbe;
  Settings: TH2SettingsArray;
  ValidValue: string;
  Req: TFPHTTPConnectionRequest;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[0].Value := 65535;
  ValidValue := B64(SettingsPayload(Settings), True);

  Probe := THTTP2HandlerProbe.Create(nil);
  try
    // h2c + valid settings -> handled.
    Req := H2CRequest('h2c', ValidValue, True);
    try
      AssertTrue('h2c + valid settings is handled', Probe.ProbeCheckH2C(Req));
    finally
      Req.Free;
    end;

    // Different Upgrade token -> not handled.
    Req := H2CRequest('websocket', ValidValue, True);
    try
      AssertFalse('non-h2c Upgrade token not handled', Probe.ProbeCheckH2C(Req));
    finally
      Req.Free;
    end;

    // h2c but HTTP2-Settings absent -> not handled.
    Req := H2CRequest('h2c', '', False);
    try
      AssertFalse('h2c without HTTP2-Settings not handled', Probe.ProbeCheckH2C(Req));
    finally
      Req.Free;
    end;

    // h2c but malformed HTTP2-Settings (len mod 6 <> 0) -> not handled.
    Req := H2CRequest('h2c', B64(Bytes([1, 2, 3, 4]), True), True);
    try
      AssertFalse('h2c with malformed HTTP2-Settings not handled', Probe.ProbeCheckH2C(Req));
    finally
      Req.Free;
    end;
  finally
    Probe.Free;
  end;
end;

{ (c) The exact 101 Switching Protocols byte sequence the handler writes. }

procedure TTestHTTP2Handler.Test101SwitchingResponseBytes;
const
  Expected =
    'HTTP/1.1 101 Switching Protocols'#13#10 +
    'Connection: Upgrade'#13#10 +
    'Upgrade: h2c'#13#10 +
    #13#10;
begin
  AssertEquals('exact 101 switching-protocols response', Expected, H2C_101_RESPONSE);
end;

{ (d) ApplyUpgradeSettings injects the decoded settings pre-Start; calling it
  after Start raises EH2ConnectionError. }

procedure TTestHTTP2Handler.TestApplyUpgradeSettings;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Settings: TH2SettingsArray;
  Raised: Boolean;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[0].Value := 12345;

  // Apply on a fresh (un-started) connection -> RemoteSettings reflects it.
  Dup := TDuplexStream.Create(nil);
  Conn := TH2Connection.Create(Dup);
  try
    Conn.ApplyUpgradeSettings(Settings);
    AssertEquals('one peer setting applied', 1, Length(Conn.RemoteSettings));
    AssertEquals('applied identifier',
      Integer(H2_SETTINGS_INITIAL_WINDOW_SIZE), Integer(Conn.RemoteSettings[0].Identifier));
    AssertEquals('applied value', 12345, Integer(Conn.RemoteSettings[0].Value));
  finally
    Conn.Free;
    Dup.Free;
  end;

  // After Start the injection point is closed -> raises.
  Dup := TDuplexStream.Create(ValidClientInput);
  Conn := TH2Connection.Create(Dup);
  try
    Conn.Start;
    Raised := False;
    try
      Conn.ApplyUpgradeSettings(Settings);
    except
      on E: EH2ConnectionError do
        Raised := True;
    end;
    AssertTrue('ApplyUpgradeSettings after Start raises EH2ConnectionError', Raised);
  finally
    Conn.Free;
    Dup.Free;
  end;
end;

{ Story 3.2 (a) - h2c initial-request -> stream-1 end-to-end dispatch. Contrasts with
  TestPriorKnowledgeReconstruction: the transport carries the client's post-101
  connection preface (magic + SETTINGS) with NO wire HEADERS frame. Stream 1 is
  SYNTHESIZED from the upgraded request handed to AdoptConnection(amUpgradeH2C, Req).
  Asserts the handler saw the REQUEST's method/path/authority, and the framed response
  is HEADERS(:status 200) then DATA, both on stream 1 - after the server preface. }

procedure TTestHTTP2Handler.TestH2CUpgradeDispatchEndToEnd;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Req: TFPHTTPConnectionRequest;
  Settings: TH2SettingsArray;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag: TBytes;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[0].Value := 65535;

  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Req := H2CRequest('h2c', B64(SettingsPayload(Settings), True), True);
  Req.Method := 'GET';
  Req.URL := '/h2c';
  Req.Host := 'example.com';
  // Transport = ONLY the post-101 connection preface (magic + SETTINGS). No HEADERS
  // frame - the request becomes stream 1 by synthesis, not off the wire.
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    Handler.AdoptConnection(Dup, amUpgradeH2C, Req, nil, Settings);

    // Dispatched request fields came from the REQUEST (proving stream-1 synthesis).
    AssertEquals('OnRequest fired exactly once', 1, FSeenCount);
    AssertEquals('handler saw request method', 'GET', FSeenMethod);
    AssertEquals('handler saw request path', '/h2c', FSeenPath);
    AssertEquals('handler saw :authority as Host', 'example.com', FSeenAuthority);

    // Wire order after the two bootstrap SETTINGS: HEADERS(:status 200) then DATA,
    // both on stream 1.
    Outp := OutputAfterBootstrap(Dup);
    try
      H := ReadFrameHeader(Outp);
      AssertEquals('response frame is HEADERS', Ord(ftHeaders), H.FrameType);
      AssertEquals('response HEADERS on stream 1', 1, Integer(H.StreamID));
      Frag := ReadHeadersFrame(Outp, H);
      AssertEquals('decoded :status is 200', '200', DecodeStatus(Frag));
      H := ReadFrameHeader(Outp);
      AssertEquals('response DATA frame follows', Ord(ftData), H.FrameType);
      AssertEquals('response DATA on stream 1', 1, Integer(H.StreamID));
    finally
      Outp.Free;
    end;
  finally
    // h2tmNone: the test owns Dup and Req (the snapshot was captured synchronously
    // inside AdoptConnection, so the request is safe to free here).
    Handler.Free;
    Server.Free;
    Dup.Free;
    Req.Free;
  end;
end;

{ Story 3.2 (b) - §8 header cleaning across the upgrade boundary. The request carries
  a mixed-case custom header plus the upgrade machinery (Connection / Upgrade /
  HTTP2-Settings) and a Host. After synthesis+dispatch: x-test survives LOWERCASED;
  the connection-specific / upgrade-machinery headers are dropped (read back as '');
  :authority equals the original Host. }

procedure TTestHTTP2Handler.TestH2CUpgradeHeaderCleaning;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Req: TFPHTTPConnectionRequest;
  Settings: TH2SettingsArray;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[0].Value := 65535;

  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  // H2CRequest sets Upgrade:=h2c and the HTTP2-Settings header; add a mixed-case
  // custom header and a Connection header - all upgrade machinery must NOT survive.
  Req := H2CRequest('h2c', B64(SettingsPayload(Settings), True), True);
  Req.Method := 'GET';
  Req.URL := '/clean';
  Req.Host := 'example.com';
  Req.SetFieldByName('X-Test', 'hi');
  Req.SetFieldByName('Connection', 'Upgrade');
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    Handler.AdoptConnection(Dup, amUpgradeH2C, Req, nil, Settings);

    AssertEquals('OnRequest fired exactly once', 1, FSeenCount);
    AssertEquals('x-test survives lowercased with its value', 'hi', FSeenXTest);
    AssertEquals('connection dropped (§8.2.2)', '', FSeenConnection);
    AssertEquals('upgrade dropped (§8.2.2)', '', FSeenUpgrade);
    AssertEquals('http2-settings dropped (upgrade token)', '', FSeenH2Settings);
    AssertEquals(':authority mapped to Host (not a duplicate host header)',
      'example.com', FSeenAuthority);
  finally
    Handler.Free;
    Server.Free;
    Dup.Free;
    Req.Free;
  end;
end;

{ Story 3.2 (c) - the upgraded request's body (AC#5) becomes stream 1's body and
  reaches the handler. A POST upgrade with Content set; the dispatched request's
  Content equals the original (and the FR0 residual seed stayed empty/separate). }

procedure TTestHTTP2Handler.TestH2CUpgradePostBodyBecomesStream1Body;
const
  BodyText = 'the-request-body-bytes';
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
  Dup: TDuplexStream;
  Req: TFPHTTPConnectionRequest;
  Settings: TH2SettingsArray;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[0].Value := 65535;

  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Req := H2CRequest('h2c', B64(SettingsPayload(Settings), True), True);
  Req.Method := 'POST';
  Req.URL := '/submit';
  Req.Host := 'example.com';
  Req.Content := BodyText;
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Server.OnRequest := @HandleTestRequest;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;

    Handler.AdoptConnection(Dup, amUpgradeH2C, Req, nil, Settings);

    AssertEquals('OnRequest fired exactly once', 1, FSeenCount);
    AssertEquals('handler saw POST method', 'POST', FSeenMethod);
    AssertEquals('request body became stream-1 body', BodyText, FSeenContent);
  finally
    Handler.Free;
    Server.Free;
    Dup.Free;
    Req.Free;
  end;
end;

{ --- Story 4.6 TLS-ALPN ingress - socket-free NFR1/NFR3 gate cases ---

  A live ALPN negotiation needs a real TLS handshake (integration-only, see
  tests/integrationtests/http2-tls-alpn.md). The socket-free coverage here is the
  zero-impact gate: a connection with no SSL handler never reports h2, a server with
  no handler never adopts, the non-h2 gate holds, and activate/deactivate advertises
  then restores the ALPN list (NFR1). Connections are constructed with a nil socket
  (TFPHTTPConnection over no TSocketStream) - NegotiatedProtocol's not-Assigned guard
  returns '' and the destructor never dereferences the socket while logging is off. }

procedure TTestHTTP2Handler.TestNegotiatedProtocolEmptyWithoutSSL;
var
  Server: TFPCustomHttpServer;
  Conn: TFPHTTPConnection;
begin
  // AC#2: a cleartext / non-OpenSSL connection (here: no socket handler at all)
  // reports '' - it never appears to have negotiated h2 (NFR3).
  Server := TFPCustomHttpServer.Create(nil);
  Conn := TFPHTTPConnection.Create(Server, nil);
  try
    AssertEquals('no SSL handler => NegotiatedProtocol is empty', '', Conn.NegotiatedProtocol);
  finally
    Conn.Free;
    Server.Free;
  end;
end;

procedure TTestHTTP2Handler.TestCheckHTTP2ALPNFalseWithoutHandler;
var
  Server: THTTP2ServerProbe;
  Conn: TFPHTTPConnection;
begin
  // AC#5: with NO HTTP/2 handler registered, CheckHTTP2ALPN returns False and the
  // connection falls through to the unchanged HTTP/1.1 path (NFR1).
  Server := THTTP2ServerProbe.Create(nil);
  Conn := TFPHTTPConnection.Create(Server, nil);
  try
    AssertFalse('no handler => CheckHTTP2ALPN is False', Server.ProbeCheckHTTP2ALPN(Conn));
  finally
    Conn.Free;
    Server.Free;
  end;
end;

procedure TTestHTTP2Handler.TestCheckHTTP2ALPNFalseWhenNotH2;
var
  Server: THTTP2ServerProbe;
  Handler: TFPHTTP2Handler;
  Conn: TFPHTTPConnection;
begin
  // AC#5: a registered handler is necessary but NOT sufficient - the connection must
  // also have negotiated 'h2'. Here NegotiatedProtocol is '' (no SSL handler), so the
  // gate holds: CheckHTTP2ALPN returns False and fires no adopt (FR16).
  Server := THTTP2ServerProbe.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  Conn := TFPHTTPConnection.Create(Server, nil);
  try
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;
    Handler.Active := True;   // registers the adopt callback on the server

    AssertFalse('handler active but non-h2 => CheckHTTP2ALPN is False',
      Server.ProbeCheckHTTP2ALPN(Conn));

    Handler.Active := False;
  finally
    Conn.Free;
    Handler.Free;
    Server.Free;
  end;
end;

procedure TTestHTTP2Handler.TestActivateAdvertisesALPNAndRestores;
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
begin
  // AC#1: an empty ALPN list is set to 'h2,http/1.1' on activate, then restored to
  // its pre-activation (empty) value on deactivate - byte-for-byte clean (NFR1).
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  try
    AssertEquals('ALPN list starts empty', '', Server.CertificateData.ALPNProtocols);
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;

    Handler.Active := True;
    AssertEquals('activate advertises h2', 'h2,http/1.1', Server.CertificateData.ALPNProtocols);

    Handler.Active := False;
    AssertEquals('deactivate restores empty list', '', Server.CertificateData.ALPNProtocols);
  finally
    Handler.Free;
    Server.Free;
  end;
end;

procedure TTestHTTP2Handler.TestActivateHonorsUserALPNList;
const
  UserList = 'h2,http/1.1,myproto';
var
  Server: TFPCustomHttpServer;
  Handler: TFPHTTP2Handler;
begin
  // AC#1: a user-supplied (non-empty) ALPN list is NOT clobbered on activate and is
  // restored verbatim on deactivate.
  Server := TFPCustomHttpServer.Create(nil);
  Handler := TFPHTTP2Handler.Create(nil);
  try
    Server.CertificateData.ALPNProtocols := UserList;
    Handler.WebServer := Server;
    Handler.ThreadMode := h2tmNone;

    Handler.Active := True;
    AssertEquals('user ALPN list is honored unchanged', UserList,
      Server.CertificateData.ALPNProtocols);

    Handler.Active := False;
    AssertEquals('deactivate restores the user list', UserList,
      Server.CertificateData.ALPNProtocols);
  finally
    Handler.Free;
    Server.Free;
  end;
end;

initialization

  RegisterTest(TTestHTTP2Handler);
end.
