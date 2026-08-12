unit tcfphttp2request;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  httpdefs, httpprotocol, fphttpserver,
  fphttp2connection, fphttp2frames, fphttp2consts, fphttp2request, uhpack;

type

  // In-memory duplex TStream (independent copy of the tcfphttp2connection idiom):
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

  TTestHTTP2Request = class(TTestCase)
  private
    // --- captured state for the end-to-end / event cases ---
    FSeenMethod, FSeenPath, FSeenQuery, FSeenHost, FSeenCustom: string;
    FSeenBody: TBytes;
    FReadyCount: Integer;
    FReadyStreamID: Cardinal;
    FReadyBody: TBytes;
    FReadyHeaders: TStringList;
    // Set True at entry of HandleTestRequest - the end-to-end rejection case asserts
    // it stays False
    FHandlerCalled: Boolean;
    // --- byte / framing helpers (independent copies) ---
    function Bytes(const aBytes: array of Byte): TBytes;
    function Concat(const aHead, aTail: TBytes): TBytes;
    function ClientPreface: TBytes;
    function SettingsFrameBytes(const aSettings: TH2SettingsArray; aFlags: Byte = 0): TBytes;
    function ValidClientInput: TBytes;
    function HeadersFrameBytes(aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0): TBytes;
    function DataFrameBytes(aStreamID: Cardinal; const aData: TBytes; aFlags: Byte = 0): TBytes;
    // Encode a header list to an HPACK fragment with a FRESH (client) encoder.
    function EncodeBlock(aList: THPackHeaderTextList): TBytes;
    // A minimal request header list (:method/:scheme/:authority/:path).
    function RequestHeaders(const aMethod, aPath: string): THPackHeaderTextList;
    // Output positioned just past the two bootstrap SETTINGS frames Start emits.
    function OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
    procedure AssertSameBytes(const aMsg: string; const aExpected, aActual: TBytes);
    // Event sink matching TH2RequestEvent (case (d)).
    procedure CaptureRequestReady(aConn: TH2Connection; aStreamID: Cardinal;
      aHeaders: THPackHeaderTextList; const aBody: TBytes);
    // OnRequest handler for the end-to-end case (e).
    procedure HandleTestRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    // A valid request header list (:method/:scheme/:authority/:path), as a fresh
    // list the caller owns and frees - the base for the per-construct mutations.
    function ValidRequestList: THPackHeaderTextList;
    // Run ValidateH2RequestHeaders(aList, 1) and return True iff it raised
    // EH2StreamError with ErrorCode = ecProtocolError and StreamID = 1.
    function ExpectReject(aList: THPackHeaderTextList): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRequestMapsPseudoHeadersAndBody;          // (a)
    procedure TestResponseHeadersAreHpackWithStatusFirst;   // (b)
    procedure TestResponseContentEmitsEndStreamData;        // (c)
    procedure TestResponseEmptyBodyEmitsZeroLengthEndStream;// (c2)
    procedure TestOnRequestReadyFiresHeadersOnly;           // (d)
    procedure TestOnRequestReadyFiresHeadersPlusData;       // (d2)
    procedure TestEndToEndDispatchThroughServer;            // (e)
    // §8 malformed-request rejection (direct, 8a) ---
    procedure TestRejectConnectionSpecificHeader;           // #2
    procedure TestRejectTransferEncodingHeader;             // #2
    procedure TestRejectTENotTrailers;                      // #3
    procedure TestRejectPseudoHeaderAfterRegular;           // #4
    procedure TestRejectUppercaseFieldName;                 // #5
    procedure TestRejectEmptyPath;                          // #6a
    procedure TestRejectNonOriginPath;                      // #6a
    procedure TestRejectAuthorityUserinfo;                  // #6b
    procedure TestRejectUnknownPseudoHeader;                // #6c
    procedure TestRejectBadScheme;                          // #6d
    procedure TestRejectDuplicatePseudoHeader;              // #6e
    procedure TestRejectMissingMethod;                      // #6f
    procedure TestRejectMissingScheme;                      // #6f
    procedure TestRejectMissingPath;                        // #6f
    procedure TestValidRequestPassesValidation;             // #7
    // End-to-end rejection (8b) ---
    procedure TestMalformedRequestRSTStreamNotDispatched;   // #8b
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

{ helpers }

procedure TTestHTTP2Request.SetUp;
begin
  inherited SetUp;
  FSeenMethod := '';
  FSeenPath := '';
  FSeenQuery := '';
  FSeenHost := '';
  FSeenCustom := '';
  SetLength(FSeenBody, 0);
  FReadyCount := 0;
  FReadyStreamID := 0;
  SetLength(FReadyBody, 0);
  FReadyHeaders := TStringList.Create;
  FHandlerCalled := False;
end;

procedure TTestHTTP2Request.TearDown;
begin
  FreeAndNil(FReadyHeaders);
  inherited TearDown;
end;

function TTestHTTP2Request.Bytes(const aBytes: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(aBytes));
  for I := 0 to High(aBytes) do
    Result[I] := aBytes[I];
end;

function TTestHTTP2Request.Concat(const aHead, aTail: TBytes): TBytes;
begin
  SetLength(Result, Length(aHead) + Length(aTail));
  if Length(aHead) > 0 then
    Move(aHead[0], Result[0], Length(aHead));
  if Length(aTail) > 0 then
    Move(aTail[0], Result[Length(aHead)], Length(aTail));
end;

function TTestHTTP2Request.ClientPreface: TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(H2_CLIENT_PREFACE));
  for I := 0 to High(H2_CLIENT_PREFACE) do
    Result[I] := H2_CLIENT_PREFACE[I];
end;

function TTestHTTP2Request.SettingsFrameBytes(const aSettings: TH2SettingsArray;
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

function TTestHTTP2Request.ValidClientInput: TBytes;
var
  Settings: TH2SettingsArray;
begin
  SetLength(Settings, 1);
  Settings[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  Settings[0].Value := 100;
  Result := Concat(ClientPreface, SettingsFrameBytes(Settings, 0));
end;

function TTestHTTP2Request.HeadersFrameBytes(aStreamID: Cardinal;
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

function TTestHTTP2Request.DataFrameBytes(aStreamID: Cardinal;
  const aData: TBytes; aFlags: Byte = 0): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WriteDataFrame(S, aStreamID, aData, aFlags);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Request.EncodeBlock(aList: THPackHeaderTextList): TBytes;
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

function TTestHTTP2Request.RequestHeaders(const aMethod, aPath: string): THPackHeaderTextList;
begin
  Result := THPackHeaderTextList.Create;
  Result.Add(H2_PSEUDO_METHOD, aMethod);
  Result.Add(H2_PSEUDO_SCHEME, 'http');
  Result.Add(H2_PSEUDO_AUTHORITY, 'example.com');
  Result.Add(H2_PSEUDO_PATH, aPath);
end;

function TTestHTTP2Request.OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
var
  H: TH2FrameHeader;
  Outp: TBytes;
begin
  // Snapshot the output ONCE (OutputBytes allocates a fresh copy per call) and
  // guard the empty case before indexing [0].
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

procedure TTestHTTP2Request.AssertSameBytes(const aMsg: string;
  const aExpected, aActual: TBytes);
var
  I: Integer;
begin
  AssertEquals(aMsg + ' length', Length(aExpected), Length(aActual));
  for I := 0 to High(aExpected) do
    AssertEquals(aMsg + ' byte ' + IntToStr(I), aExpected[I], aActual[I]);
end;

procedure TTestHTTP2Request.CaptureRequestReady(aConn: TH2Connection;
  aStreamID: Cardinal; aHeaders: THPackHeaderTextList; const aBody: TBytes);
var
  I: Integer;
begin
  Inc(FReadyCount);
  FReadyStreamID := aStreamID;
  FReadyBody := Copy(aBody, 0, Length(aBody));
  FReadyHeaders.Clear;
  for I := 0 to aHeaders.Count - 1 do
    FReadyHeaders.Values[aHeaders[I]^.HeaderName] := aHeaders[I]^.HeaderValue;
end;

procedure TTestHTTP2Request.HandleTestRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  FHandlerCalled := True;
  FSeenMethod := ARequest.Method;
  FSeenPath := ARequest.PathInfo;
  FSeenQuery := ARequest.QueryString;
  FSeenHost := ARequest.Host;
  FSeenCustom := ARequest.GetFieldByName('x-custom');
  FSeenBody := Copy(ARequest.ContentBytes, 0, Length(ARequest.ContentBytes));
  AResponse.Code := 200;
  AResponse.Content := 'Hi there';
end;

{ (a) TH2Request maps pseudo-headers + regular header + body, and InitRequestVars
  splits the query. }

procedure TTestHTTP2Request.TestRequestMapsPseudoHeadersAndBody;
var
  List: THPackHeaderTextList;
  Req: TH2Request;
  Body: TBytes;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'POST');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/foo?q=1');
    List.Add('content-type', 'text/plain');
    List.Add('x-custom', 'hello');
    Body := Bytes([Ord('h'), Ord('i')]);
    Req := TH2Request.CreateFromH2(List, Body);
    try
      AssertEquals('method mapped from :method', 'POST', Req.Method);
      AssertEquals('URL mapped from :path (origin-form incl query)', '/foo?q=1', Req.URL);
      AssertEquals('PathInfo derived from :path', '/foo', Req.PathInfo);
      AssertEquals('QueryString split by InitRequestVars', 'q=1', Req.QueryString);
      AssertEquals('host mapped from :authority', 'example.com', Req.Host);
      AssertEquals('known regular header routed to slot', 'text/plain', Req.ContentType);
      AssertEquals('unknown regular header kept as custom', 'hello',
        Req.GetFieldByName('x-custom'));
      AssertEquals('HttpVersion forced to 2.0', '2.0', Req.HttpVersion);
      AssertEquals('body buffered into ContentBytes', 2, Length(Req.ContentBytes));
      AssertEquals('body byte 0', Ord('h'), Req.ContentBytes[0]);
      AssertEquals('body byte 1', Ord('i'), Req.ContentBytes[1]);
    finally
      Req.Free;
    end;
  finally
    List.Free;
  end;
end;

{ (b) DoSendHeaders (driven via SendHeaders) emits exactly one HEADERS frame whose
  decoded block has :status first = Code, response headers lowercased, the §8.2.2
  connection-specific headers absent, and END_STREAM unset. }

procedure TTestHTTP2Request.TestResponseHeadersAreHpackWithStatusFirst;
var
  Dup, Dup2: TDuplexStream;
  Conn, Conn2: TH2Connection;
  Resp: TH2Response;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag: TBytes;
  Decoded: THPackHeaderTextList;
  I: Integer;
  HasConnection, HasTE: Boolean;
begin
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Resp := TH2Response.CreateH2(nil, Conn, 1);
      try
        Resp.Code := 404;
        Resp.ContentType := 'text/plain';
        Resp.SetCustomHeader('X-Test', 'Yes');
        Resp.SetCustomHeader('Connection', 'close');         // §8.2.2 forbidden
        Resp.SetCustomHeader('Transfer-Encoding', 'chunked'); // §8.2.2 forbidden
        Resp.SendHeaders;
      finally
        Resp.Free;
      end;

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('response frame is HEADERS', Ord(ftHeaders), H.FrameType);
        AssertEquals('HEADERS on stream 1', 1, Integer(H.StreamID));
        AssertTrue('END_HEADERS set', (H.Flags and H2_FLAG_END_HEADERS) <> 0);
        AssertEquals('END_STREAM NOT set on HEADERS', 0, H.Flags and H2_FLAG_END_STREAM);
        Frag := ReadHeadersFrame(Outp, H);
        AssertEquals('exactly one HEADERS frame emitted', Outp.Size, Outp.Position);
      finally
        Outp.Free;
      end;

      // Decode the emitted block with a fresh connection's decoder.
      Dup2 := TDuplexStream.Create(nil);
      try
        Conn2 := TH2Connection.Create(Dup2);
        try
          Decoded := THPackHeaderTextList.Create;
          try
            Conn2.DecodeHeaderBlock(Frag, Decoded);
            AssertTrue('at least :status + content-type + x-test', Decoded.Count >= 3);
            AssertEquals(':status is first', H2_PSEUDO_STATUS, Decoded[0]^.HeaderName);
            AssertEquals(':status carries Code', '404', Decoded[0]^.HeaderValue);
            HasConnection := False;
            HasTE := False;
            for I := 0 to Decoded.Count - 1 do
            begin
              // every (non-pseudo) field name must be lowercase
              if Decoded[I]^.HeaderName[1] <> ':' then
                AssertEquals('field name is lowercased',
                  LowerCase(Decoded[I]^.HeaderName), Decoded[I]^.HeaderName);
              if Decoded[I]^.HeaderName = 'connection' then HasConnection := True;
              if Decoded[I]^.HeaderName = 'transfer-encoding' then HasTE := True;
            end;
            AssertFalse('Connection header stripped (§8.2.2)', HasConnection);
            AssertFalse('Transfer-Encoding header stripped (§8.2.2)', HasTE);
          finally
            Decoded.Free;
          end;
        finally
          Conn2.Free;
        end;
      finally
        Dup2.Free;
      end;
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ (c) DoSendContent emits the body as DATA with END_STREAM on the last frame. The
  full SendContent path (HEADERS then DATA) is driven; we assert the DATA frame. }

procedure TTestHTTP2Request.TestResponseContentEmitsEndStreamData;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Resp: TH2Response;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Payload, Expected: TBytes;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, EncodeBlock(RequestHeaders('GET', '/')), H2_FLAG_END_STREAM)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // open + complete stream 1 (no OnRequestReady wired)
      Resp := TH2Response.CreateH2(nil, Conn, 1);
      try
        Resp.Code := 200;
        Resp.Content := 'hello body';
        Resp.SendContent;      // SendHeaders -> DoSendHeaders, then DoSendContent
      finally
        Resp.Free;
      end;

      Outp := OutputAfterBootstrap(Dup);
      try
        // First the HEADERS frame (no END_STREAM)...
        H := ReadFrameHeader(Outp);
        AssertEquals('first emitted frame is HEADERS', Ord(ftHeaders), H.FrameType);
        AssertEquals('HEADERS has no END_STREAM', 0, H.Flags and H2_FLAG_END_STREAM);
        ReadHeadersFrame(Outp, H);
        // ...then the DATA frame carrying the body with END_STREAM last.
        H := ReadFrameHeader(Outp);
        AssertEquals('second emitted frame is DATA', Ord(ftData), H.FrameType);
        AssertEquals('DATA on stream 1', 1, Integer(H.StreamID));
        AssertTrue('DATA carries END_STREAM', (H.Flags and H2_FLAG_END_STREAM) <> 0);
        Payload := ReadDataFrame(Outp, H);
        Expected := Bytes([Ord('h'), Ord('e'), Ord('l'), Ord('l'), Ord('o'), Ord(' '),
          Ord('b'), Ord('o'), Ord('d'), Ord('y')]);
        AssertSameBytes('DATA payload equals Content', Expected, Payload);
        AssertEquals('no further frames', Outp.Size, Outp.Position);
      finally
        Outp.Free;
      end;
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ (c2) A no-body response still closes the stream: a single zero-length END_STREAM
  DATA frame follows the HEADERS. }

procedure TTestHTTP2Request.TestResponseEmptyBodyEmitsZeroLengthEndStream;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Resp: TH2Response;
  Outp: TBytesStream;
  H: TH2FrameHeader;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, EncodeBlock(RequestHeaders('GET', '/')), H2_FLAG_END_STREAM)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;
      Resp := TH2Response.CreateH2(nil, Conn, 1);
      try
        Resp.Code := 204;
        Resp.Content := '';
        Resp.SendContent;
      finally
        Resp.Free;
      end;

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('first emitted frame is HEADERS', Ord(ftHeaders), H.FrameType);
        ReadHeadersFrame(Outp, H);
        H := ReadFrameHeader(Outp);
        AssertEquals('second emitted frame is DATA', Ord(ftData), H.FrameType);
        AssertEquals('zero-length DATA', 0, Integer(H.Len));
        AssertTrue('zero-length DATA carries END_STREAM',
          (H.Flags and H2_FLAG_END_STREAM) <> 0);
        ReadDataFrame(Outp, H);
        AssertEquals('no further frames', Outp.Size, Outp.Position);
      finally
        Outp.Free;
      end;
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ (d) OnRequestReady fires once for a HEADERS-only request (END_STREAM on HEADERS):
  correct stream id, headers present, empty body. }

procedure TTestHTTP2Request.TestOnRequestReadyFiresHeadersOnly;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  List: THPackHeaderTextList;
begin
  List := RequestHeaders('GET', '/index');
  try
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      HeadersFrameBytes(1, EncodeBlock(List), H2_FLAG_END_STREAM)));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Conn.OnRequestReady := @CaptureRequestReady;
        Conn.Start;
        Conn.ProcessNextFrame;   // HEADERS(END_STREAM) -> dispatch
        AssertEquals('OnRequestReady fired exactly once', 1, FReadyCount);
        AssertEquals('dispatched stream id', 1, Integer(FReadyStreamID));
        AssertEquals('empty body for HEADERS-only request', 0, Length(FReadyBody));
        AssertEquals('decoded :method', 'GET', FReadyHeaders.Values[H2_PSEUDO_METHOD]);
        AssertEquals('decoded :path', '/index', FReadyHeaders.Values[H2_PSEUDO_PATH]);
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  finally
    List.Free;
  end;
end;

{ (d2) OnRequestReady fires once for a HEADERS+DATA request, on the final DATA
  frame (END_STREAM), with the body buffered. }

procedure TTestHTTP2Request.TestOnRequestReadyFiresHeadersPlusData;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  List: THPackHeaderTextList;
  Body: TBytes;
begin
  Body := Bytes([Ord('a'), Ord('b'), Ord('c')]);
  List := RequestHeaders('POST', '/submit');
  try
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      Concat(HeadersFrameBytes(1, EncodeBlock(List)),
             DataFrameBytes(1, Body, H2_FLAG_END_STREAM))));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Conn.OnRequestReady := @CaptureRequestReady;
        Conn.Start;
        Conn.ProcessNextFrame;   // HEADERS (no END_STREAM) -> no dispatch yet
        AssertEquals('no dispatch before END_STREAM', 0, FReadyCount);
        Conn.ProcessNextFrame;   // DATA(END_STREAM) -> dispatch
        AssertEquals('OnRequestReady fired exactly once', 1, FReadyCount);
        AssertEquals('dispatched stream id', 1, Integer(FReadyStreamID));
        AssertSameBytes('buffered body', Body, FReadyBody);
        AssertEquals('decoded :method', 'POST', FReadyHeaders.Values[H2_PSEUDO_METHOD]);
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  finally
    List.Free;
  end;
end;

{ (e) End-to-end: a bare TFPCustomHttpServer whose OnRequest sets Code/Content is
  wired through TH2Dispatcher. Feeding a complete request + ProcessNextFrame
  produces the framed HTTP/2 response and the handler saw the mapped fields. }

procedure TTestHTTP2Request.TestEndToEndDispatchThroughServer;
var
  Dup, Dup2: TDuplexStream;
  Conn, Conn2: TH2Connection;
  Server: TFPCustomHttpServer;
  Dispatcher: TH2Dispatcher;
  List: THPackHeaderTextList;
  Body: TBytes;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag, Payload, Expected: TBytes;
  Decoded: THPackHeaderTextList;
begin
  Body := Bytes([Ord('x'), Ord('y')]);
  List := RequestHeaders('PUT', '/api/items?id=7');
  List.Add('x-custom', 'abc');
  try
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      Concat(HeadersFrameBytes(1, EncodeBlock(List)),
             DataFrameBytes(1, Body, H2_FLAG_END_STREAM))));
    Server := TFPCustomHttpServer.Create(nil);
    Dispatcher := TH2Dispatcher.Create(Server);
    try
      Server.OnRequest := @HandleTestRequest;
      Conn := TH2Connection.Create(Dup);
      try
        Conn.OnRequestReady := @Dispatcher.HandleRequestReady;
        Conn.Start;
        Conn.ProcessNextFrame;   // HEADERS (no END_STREAM)
        Conn.ProcessNextFrame;   // DATA(END_STREAM) -> dispatch -> handler -> response

        // The handler saw the mapped request fields.
        AssertEquals('handler saw mapped method', 'PUT', FSeenMethod);
        AssertEquals('handler saw mapped path', '/api/items', FSeenPath);
        AssertEquals('handler saw split query', 'id=7', FSeenQuery);
        AssertEquals('handler saw mapped host', 'example.com', FSeenHost);
        AssertEquals('handler saw custom header', 'abc', FSeenCustom);
        AssertSameBytes('handler saw buffered body', Body, FSeenBody);

        // The framed HTTP/2 response is in the output: HEADERS(:status 200) + DATA.
        Outp := OutputAfterBootstrap(Dup);
        try
          H := ReadFrameHeader(Outp);
          AssertEquals('response HEADERS frame', Ord(ftHeaders), H.FrameType);
          AssertEquals('response HEADERS on stream 1', 1, Integer(H.StreamID));
          AssertEquals('response HEADERS no END_STREAM', 0, H.Flags and H2_FLAG_END_STREAM);
          Frag := ReadHeadersFrame(Outp, H);
          H := ReadFrameHeader(Outp);
          AssertEquals('response DATA frame', Ord(ftData), H.FrameType);
          AssertTrue('response DATA END_STREAM', (H.Flags and H2_FLAG_END_STREAM) <> 0);
          Payload := ReadDataFrame(Outp, H);
          Expected := Bytes([Ord('H'), Ord('i'), Ord(' '), Ord('t'), Ord('h'),
            Ord('e'), Ord('r'), Ord('e')]);
          AssertSameBytes('response body', Expected, Payload);
        finally
          Outp.Free;
        end;

        // Decode the response HEADERS: :status first = 200.
        Dup2 := TDuplexStream.Create(nil);
        try
          Conn2 := TH2Connection.Create(Dup2);
          try
            Decoded := THPackHeaderTextList.Create;
            try
              Conn2.DecodeHeaderBlock(Frag, Decoded);
              AssertEquals(':status is first', H2_PSEUDO_STATUS, Decoded[0]^.HeaderName);
              AssertEquals(':status is 200', '200', Decoded[0]^.HeaderValue);
            finally
              Decoded.Free;
            end;
          finally
            Conn2.Free;
          end;
        finally
          Dup2.Free;
        end;
      finally
        Conn.Free;
      end;
    finally
      Dispatcher.Free;
      Server.Free;
      Dup.Free;
    end;
  finally
    List.Free;
  end;
end;

{ §8 malformed-request rejection }

function TTestHTTP2Request.ValidRequestList: THPackHeaderTextList;
begin
  // :method / :scheme / :authority / :path, in that (pseudo-first) order.
  Result := THPackHeaderTextList.Create;
  Result.Add(H2_PSEUDO_METHOD, 'GET');
  Result.Add(H2_PSEUDO_SCHEME, 'http');
  Result.Add(H2_PSEUDO_AUTHORITY, 'example.com');
  Result.Add(H2_PSEUDO_PATH, '/');
end;

function TTestHTTP2Request.ExpectReject(aList: THPackHeaderTextList): Boolean;
var
  Raised: Boolean;
  Code: TH2ErrorCode;
  SID: Cardinal;
begin
  Raised := False;
  Code := ecNoError;
  SID := 0;
  try
    ValidateH2RequestHeaders(aList, 1);
  except
    on E: EH2StreamError do
    begin
      Raised := True;
      Code := E.ErrorCode;
      SID := E.StreamID;
    end;
  end;
  Result := Raised and (Code = ecProtocolError) and (SID = 1);
end;

{ #2 - connection-specific request header (§8.2.2). }

procedure TTestHTTP2Request.TestRejectConnectionSpecificHeader;
var
  List: THPackHeaderTextList;
begin
  List := ValidRequestList;
  try
    List.Add('connection', 'keep-alive');
    AssertTrue('connection header -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #2 - transfer-encoding is connection-specific (§8.2.2). }

procedure TTestHTTP2Request.TestRejectTransferEncodingHeader;
var
  List: THPackHeaderTextList;
begin
  List := ValidRequestList;
  try
    List.Add('transfer-encoding', 'chunked');
    AssertTrue('transfer-encoding header -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #3 - TE may only carry 'trailers' (§8.2.2). 'te: trailers' MUST pass. }

procedure TTestHTTP2Request.TestRejectTENotTrailers;
var
  List: THPackHeaderTextList;
begin
  List := ValidRequestList;
  try
    List.Add('te', 'gzip');
    AssertTrue('te<>trailers -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
  // Positive: te: trailers is the one legal TE in HTTP/2 - must not raise.
  List := ValidRequestList;
  try
    List.Add('te', 'trailers');
    try
      ValidateH2RequestHeaders(List, 1);
    except
      on E: EH2StreamError do Fail('te: trailers must pass: ' + E.Message);
    end;
  finally
    List.Free;
  end;
end;

{ #4 - a pseudo-header after a regular header is illegal (§8.3.1). }

procedure TTestHTTP2Request.TestRejectPseudoHeaderAfterRegular;
var
  List: THPackHeaderTextList;
begin
  // Order a regular header BEFORE a pseudo-header.
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add('x-a', '1');                 // regular header first
    List.Add(H2_PSEUDO_SCHEME, 'http');   // pseudo after regular -> illegal
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/');
    AssertTrue('pseudo-after-regular -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #5 - uppercase byte in a field name (§8.2.1). }

procedure TTestHTTP2Request.TestRejectUppercaseFieldName;
var
  List: THPackHeaderTextList;
begin
  List := ValidRequestList;
  try
    List.Add('X-Bad', '1');               // uppercase X / B
    AssertTrue('uppercase field name -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6a - empty :path (§8.3.1). }

procedure TTestHTTP2Request.TestRejectEmptyPath;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '');
    AssertTrue('empty :path -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6a - non-origin-form :path (no leading '/', not '*') (§8.3.1). '/' and '*' pass. }

procedure TTestHTTP2Request.TestRejectNonOriginPath;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, 'foo');      // no leading '/'
    AssertTrue('non-origin :path -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
  // Positive: a '/'-rooted path passes.
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/');
    try
      ValidateH2RequestHeaders(List, 1);
    except
      on E: EH2StreamError do Fail(':path = "/" must pass: ' + E.Message);
    end;
  finally
    List.Free;
  end;
  // Positive: the asterisk-form passes.
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'OPTIONS');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '*');
    try
      ValidateH2RequestHeaders(List, 1);
    except
      on E: EH2StreamError do Fail(':path = "*" must pass: ' + E.Message);
    end;
  finally
    List.Free;
  end;
end;

{ #6b - :authority with userinfo ('@') (§8.3.1). }

procedure TTestHTTP2Request.TestRejectAuthorityUserinfo;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'user@example.com');
    List.Add(H2_PSEUDO_PATH, '/');
    AssertTrue(':authority userinfo -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6c - unknown / response pseudo-header in a request (§8.3.1). }

procedure TTestHTTP2Request.TestRejectUnknownPseudoHeader;
var
  List: THPackHeaderTextList;
begin
  // An undefined pseudo-header.
  List := ValidRequestList;
  try
    List.Add(':x-bad', '1');
    AssertTrue('unknown pseudo-header -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
  // A response pseudo-header (:status) has no place in a request.
  List := ValidRequestList;
  try
    List.Add(H2_PSEUDO_STATUS, '200');
    AssertTrue(':status in request -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6d - :scheme not in http, https (§8.3.1). }

procedure TTestHTTP2Request.TestRejectBadScheme;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'ftp');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/');
    AssertTrue('bad :scheme -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6e - duplicate pseudo-header (§8.3.1). }

procedure TTestHTTP2Request.TestRejectDuplicatePseudoHeader;
var
  List: THPackHeaderTextList;
begin
  List := ValidRequestList;
  try
    List.Add(H2_PSEUDO_METHOD, 'POST');   // second :method
    AssertTrue('duplicate :method -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
  // Two EMPTY :authority headers are still a duplicate (presence, not value).
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, '');
    List.Add(H2_PSEUDO_AUTHORITY, '');    // duplicate, both empty
    List.Add(H2_PSEUDO_PATH, '/');
    AssertTrue('duplicate empty :authority -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6f - missing :method (§8.3.1). }

procedure TTestHTTP2Request.TestRejectMissingMethod;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/');
    AssertTrue('missing :method -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6f - missing :scheme (§8.3.1). }

procedure TTestHTTP2Request.TestRejectMissingScheme;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/');
    AssertTrue('missing :scheme -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #6f - missing :path (§8.3.1). }

procedure TTestHTTP2Request.TestRejectMissingPath;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'http');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    AssertTrue('missing :path -> PROTOCOL_ERROR', ExpectReject(List));
  finally
    List.Free;
  end;
end;

{ #7 - a well-formed request (regular headers after the pseudo-block, te: trailers,
  a ?query in :path) returns WITHOUT raising. }

procedure TTestHTTP2Request.TestValidRequestPassesValidation;
var
  List: THPackHeaderTextList;
begin
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_METHOD, 'GET');
    List.Add(H2_PSEUDO_SCHEME, 'https');
    List.Add(H2_PSEUDO_AUTHORITY, 'example.com');
    List.Add(H2_PSEUDO_PATH, '/search?q=1');
    List.Add('content-type', 'text/plain');   // regular AFTER the pseudo-block
    List.Add('te', 'trailers');                // the one legal TE
    try
      ValidateH2RequestHeaders(List, 1);
    except
      on E: EH2StreamError do
        Fail('valid request must not be rejected: ' + E.Message);
    end;
  finally
    List.Free;
  end;
end;

{ #8b - end-to-end: a malformed request (connection header) is RST_STREAM'd at the
  HandleNextFrame catch site, the connection survives, and HandleRequest is never
  invoked . }

procedure TTestHTTP2Request.TestMalformedRequestRSTStreamNotDispatched;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Server: TFPCustomHttpServer;
  Dispatcher: TH2Dispatcher;
  List: THPackHeaderTextList;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Code: Cardinal;
begin
  List := RequestHeaders('GET', '/');
  List.Add('connection', 'keep-alive');   // §8.2.2 forbidden -> rejection
  try
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      HeadersFrameBytes(1, EncodeBlock(List), H2_FLAG_END_STREAM)));
    Server := TFPCustomHttpServer.Create(nil);
    Dispatcher := TH2Dispatcher.Create(Server);
    try
      Server.OnRequest := @HandleTestRequest;
      Conn := TH2Connection.Create(Dup);
      try
        Conn.OnRequestReady := @Dispatcher.HandleRequestReady;
        Conn.Start;
        // HandleNextFrame (NOT ProcessNextFrame) - the catch site is here.
        AssertTrue('connection survives the rejection', Conn.HandleNextFrame);
        AssertFalse('connection not Closing (stream error, not connection error)',
          Conn.Closing);
        AssertFalse('HandleRequest not invoked', FHandlerCalled);

        Outp := OutputAfterBootstrap(Dup);
        try
          H := ReadFrameHeader(Outp);
          AssertEquals('emitted frame is RST_STREAM', Ord(ftRSTStream), H.FrameType);
          AssertEquals('RST_STREAM on stream 1', 1, Integer(H.StreamID));
          Code := ReadRSTStreamFrame(Outp, H);
          AssertEquals('RST_STREAM carries PROTOCOL_ERROR',
            Cardinal(Ord(ecProtocolError)), Code);
        finally
          Outp.Free;
        end;
      finally
        Conn.Free;
      end;
    finally
      Dispatcher.Free;
      Server.Free;
      Dup.Free;
    end;
  finally
    List.Free;
  end;
end;

initialization

  RegisterTest(TTestHTTP2Request);
end.
