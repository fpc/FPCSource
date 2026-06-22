unit tcfphttp2connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fphttp2connection, fphttp2frames, fphttp2consts, uhpack;

type

  { In-memory duplex TStream: Read pops from a preloaded input byte vector
    (read cursor); Write appends to an internal output buffer. This is the
    "a TStream" the connection is constructed over - socket-free, deterministic.
    A single seekable stream cannot serve as both source and sink (writes would
     overwrite unread input), so the two directions are kept separate. }
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
    // Captured output bytes, for assertions.
    function OutputBytes: TBytes;
  end;

  TTestHTTP2Connection = class(TTestCase)
  private
    // Build a TBytes from a byte literal.
    function Bytes(const aBytes: array of Byte): TBytes;
    // The 24-octet client connection preface as TBytes.
    function ClientPreface: TBytes;
    // Serialize a SETTINGS frame to TBytes (via the codec) for use as wire input.
    function SettingsFrameBytes(const aSettings: TH2SettingsArray; aFlags: Byte = 0): TBytes;
    // Concatenate two byte vectors.
    function Concat(const aHead, aTail: TBytes): TBytes;
    // A single-pair client SETTINGS array (MAX_CONCURRENT_STREAMS = 100).
    function ValidClientSettings: TH2SettingsArray;
    // A valid client side: preface + a single-pair client SETTINGS frame.
    function ValidClientInput: TBytes;
    // Serialize a HEADERS frame (END_HEADERS always set, so the demux needs no
    // CONTINUATION) carrying aFragment for aStreamID; aFlags may add END_STREAM.
    function HeadersFrameBytes(aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0): TBytes;
    // Serialize a DATA frame for aStreamID; aFlags may add END_STREAM.
    function DataFrameBytes(aStreamID: Cardinal; const aData: TBytes; aFlags: Byte = 0): TBytes;
    // Serialize an RST_STREAM frame for aStreamID with the given error code.
    function RstStreamFrameBytes(aStreamID: Cardinal; aErrorCode: Cardinal): TBytes;
    // Serialize a PING frame carrying aOpaque (8 octets) with aFlags (add
    // H2_FLAG_ACK to make it an ACK), via the codec.
    function PingFrameBytes(const aOpaque: TBytes; aFlags: Byte = 0): TBytes;
    // A minimal STATIC-ONLY HPACK fragment: [$82] = ':method: GET' (RFC 7541
    // static index 2). Decoding never mutates the dynamic table, so it is safe
    // to reuse across multiple streams within one connection/test.
    function StaticGetFragment: TBytes;
    // Serialize a WINDOW_UPDATE frame for aStreamID with aIncrement (via the codec).
    function WindowUpdateFrameBytes(aStreamID: Cardinal; aIncrement: Cardinal): TBytes;
    // A single-pair client SETTINGS array advertising SETTINGS_INITIAL_WINDOW_SIZE.
    function ClientSettingsWithInitialWindow(aWindow: Cardinal): TH2SettingsArray;
    // A valid client side advertising a specific stream INITIAL_WINDOW_SIZE, so a
    // test can seed a small per-stream send window.
    function ClientInputWithWindow(aWindow: Cardinal): TBytes;
    // A TBytes of aLen octets all equal to aFill (test payload generator).
    function FilledBytes(aLen: Integer; aFill: Byte): TBytes;
    // Build a TBytesStream over a connection's captured output, positioned just
    // PAST the two bootstrap SETTINGS frames (server SETTINGS + ACK) that Start
    // emits, ready to read the DATA frames SendData wrote. Caller frees it.
    function OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
    // A post-bootstrap SETTINGS frame (stream 0, non-ACK) carrying only
    // SETTINGS_INITIAL_WINDOW_SIZE = aWindow - fed through the frame loop AFTER
    // Conn.Start to exercise the §6.9.2 retroactive adjustment 
    function InitialWindowSettingsFrameBytes(aWindow: Cardinal): TBytes;
    // Assert two byte vectors are identical (length + each octet).
    procedure AssertSameBytes(const aMsg: string; const aExpected, aActual: TBytes);
  published
    procedure TestStartEmitsServerSettingsOnStream0;          
    procedure TestValidBootstrapRoundTripsAndAcks;            
    procedure TestCorruptPrefaceRaisesProtocolError;          
    procedure TestShortPrefaceRaisesProtocolError;            
    procedure TestTruncatedSettingsRaisesProtocolError;       
    procedure TestSeededBytesConsumedBeforeTransport;         
    procedure TestNonSettingsAfterMagicRaises;                
    procedure TestSettingsWrongStreamRaises;                  
    procedure TestAckFlaggedSettingsAfterMagicRaises;         
    { HPACK wiring }
    procedure TestDecodeKnownStaticBlock;                     
    procedure TestSendHeadersLockedRoundTrip;                 
    procedure TestSingleEncoderPersistsAcrossSends;           
    { Stream state machine / id validation / multiplexing }
    procedure TestHeadersOpensStream;                         
    procedure TestHeadersEndStreamHalfClosesRemote;           
    procedure TestDataEndStreamHalfClosesRemote;              
    procedure TestEvenStreamIdRaisesProtocolError;            
    procedure TestNonMonotonicAndReusedStreamIdRaise;         
    procedure TestMultiplexedStreamsTrackedIndependently;     
    procedure TestRSTStreamClosesStream;                      
    procedure TestDataOnHalfClosedRemoteRaisesStreamClosed;   
    { flow control }
    procedure TestStreamSendWindowSeededFromPeerInitialWindow;  
    procedure TestSendDataWithinWindowEmitsFrameAndDecrements;  
    procedure TestSendDataExceedingStreamWindowSendsPartial;    
    procedure TestSendDataLimitedByConnectionWindowSendsPartial;
    procedure TestSendDataLargerThanMaxFrameSizeSplits;         
    procedure TestStreamWindowUpdateGrowsStreamSendWindow;      
    procedure TestConnectionWindowUpdateGrowsConnSendWindow;    
    procedure TestZeroIncrementWindowUpdateRaisesProtocolError; 
    procedure TestOverflowingWindowUpdateRaisesFlowControlError;
    { PING + basic error frames }
    procedure TestPingNonAckAnsweredWithAckSamePayload;         
    procedure TestPingAckIgnoredNoResponse;                     
    procedure TestStreamErrorEmitsRSTStreamConnectionSurvives;  
    procedure TestConnectionErrorEmitsGoAwayAndCloses;          
    { full error handling + §6.8 GOAWAY last-stream-id }
    procedure TestGoAwayReportsHighestProcessedNotOpenedStreamId;   
    procedure TestGoAwayLastIdIsProcessedStreamAfterValidRequest;   
    procedure TestStreamErrorRSTThenConnectionSurvivesAndProcessesNextFrame; 
    procedure TestPostRSTStreamTreatedAsClosed;                     
    { §6.9 flow-control hardening }
    procedure TestRetroactiveInitialWindowIncreaseGrowsStreamWindows;  
    procedure TestRetroactiveInitialWindowDecreaseDrivesWindowNegative;
    procedure TestNewStreamSeedsFromRetroactiveInitialWindow;          
    procedure TestOverMaxInitialWindowRaisesFlowControlError;          
    procedure TestStreamWindowStallThenResume;                         
    procedure TestConnectionWindowStallThenResume;                     
    procedure TestFairResumeAcrossTwoStalledStreams;                   
    procedure TestEmptyBodyEndStreamWindowExempt;                      
    procedure TestOverflowWindowUpdateWithNegativeBaseWindow;          
    { §6.8 graceful shutdown - BeginShutdown + new-stream refusa }
    procedure TestBeginShutdownEmitsGracefulGoAwayWithLastProcessedId; 
    procedure TestBeginShutdownIsIdempotent;                           
    procedure TestNewStreamAfterGoAwayRefusedInFlightSurvives;         
    procedure TestErrorPathGoAwayAfterShutdownNoDoubleGoAway;          
    { §6.5.3 - post-bootstrap SETTINGS ACK }
    procedure TestPostBootstrapSettingsAcked;                          
    procedure TestAckFlaggedSettingsNotReacked;                        
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
  // Not seekable; the connection never seeks the transport.
  Result := 0;
end;

function TDuplexStream.OutputBytes: TBytes;
begin
  SetLength(Result, FOutput.Size);
  if FOutput.Size > 0 then
    Move(FOutput.Memory^, Result[0], FOutput.Size);
end;

{ helpers }

function TTestHTTP2Connection.Bytes(const aBytes: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(aBytes));
  for I := 0 to High(aBytes) do
    Result[I] := aBytes[I];
end;

function TTestHTTP2Connection.ClientPreface: TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(H2_CLIENT_PREFACE));
  for I := 0 to High(H2_CLIENT_PREFACE) do
    Result[I] := H2_CLIENT_PREFACE[I];
end;

function TTestHTTP2Connection.SettingsFrameBytes(const aSettings: TH2SettingsArray;
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

function TTestHTTP2Connection.Concat(const aHead, aTail: TBytes): TBytes;
begin
  SetLength(Result, Length(aHead) + Length(aTail));
  if Length(aHead) > 0 then
    Move(aHead[0], Result[0], Length(aHead));
  if Length(aTail) > 0 then
    Move(aTail[0], Result[Length(aHead)], Length(aTail));
end;

function TTestHTTP2Connection.ValidClientSettings: TH2SettingsArray;
begin
  SetLength(Result, 1);
  Result[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  Result[0].Value := 100;
end;

function TTestHTTP2Connection.ValidClientInput: TBytes;
begin
  Result := Concat(ClientPreface, SettingsFrameBytes(ValidClientSettings, 0));
end;

function TTestHTTP2Connection.HeadersFrameBytes(aStreamID: Cardinal;
  const aFragment: TBytes; aFlags: Byte = 0): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    // END_HEADERS always set: the block is complete in this single frame, so the
    // demux's AssembleHeaderBlock returns it without reading any CONTINUATION.
    WriteHeadersFrame(S, aStreamID, aFragment, aFlags or H2_FLAG_END_HEADERS);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Connection.DataFrameBytes(aStreamID: Cardinal;
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

function TTestHTTP2Connection.RstStreamFrameBytes(aStreamID: Cardinal;
  aErrorCode: Cardinal): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WriteRSTStreamFrame(S, aStreamID, aErrorCode);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Connection.PingFrameBytes(const aOpaque: TBytes;
  aFlags: Byte = 0): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WritePingFrame(S, aOpaque, aFlags);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Connection.StaticGetFragment: TBytes;
begin
  Result := Bytes([$82]);   // RFC 7541 static index 2 = ':method: GET'
end;

function TTestHTTP2Connection.WindowUpdateFrameBytes(aStreamID: Cardinal;
  aIncrement: Cardinal): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WriteWindowUpdateFrame(S, aStreamID, aIncrement);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Connection.ClientSettingsWithInitialWindow(
  aWindow: Cardinal): TH2SettingsArray;
begin
  SetLength(Result, 1);
  Result[0].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Result[0].Value := aWindow;
end;

function TTestHTTP2Connection.ClientInputWithWindow(aWindow: Cardinal): TBytes;
begin
  Result := Concat(ClientPreface,
    SettingsFrameBytes(ClientSettingsWithInitialWindow(aWindow), 0));
end;

function TTestHTTP2Connection.FilledBytes(aLen: Integer; aFill: Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, aLen);
  for I := 0 to aLen - 1 do
    Result[I] := aFill;
end;

function TTestHTTP2Connection.OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
var
  H: TH2FrameHeader;
begin
  Result := TBytesStream.Create;
  Result.WriteBuffer(aDup.OutputBytes[0], Length(aDup.OutputBytes));
  Result.Position := 0;
  // Start emits exactly two SETTINGS frames first: the server SETTINGS (non-ACK)
  // and the ACK of the client SETTINGS. Read past both so the cursor sits at the
  // first DATA frame SendData wrote.
  H := ReadFrameHeader(Result);
  ReadSettingsFrame(Result, H);
  H := ReadFrameHeader(Result);
  ReadSettingsFrame(Result, H);
end;

procedure TTestHTTP2Connection.AssertSameBytes(const aMsg: string;
  const aExpected, aActual: TBytes);
var
  I: Integer;
begin
  AssertEquals(aMsg + ' length', Length(aExpected), Length(aActual));
  for I := 0 to High(aExpected) do
    AssertEquals(aMsg + ' byte ' + IntToStr(I), aExpected[I], aActual[I]);
end;

function TTestHTTP2Connection.InitialWindowSettingsFrameBytes(
  aWindow: Cardinal): TBytes;
begin
  // SETTINGS is always carried on stream 0 (the codec writes stream 0); non-ACK
  // flags so it carries values. Reuses the existing INITIAL_WINDOW_SIZE settings
  // builder so the wire bytes match the bootstrap helper exactly.
  Result := SettingsFrameBytes(ClientSettingsWithInitialWindow(aWindow), 0);
end;

{ Start emits a server SETTINGS frame on stream 0 as the first frame. }

procedure TTestHTTP2Connection.TestStartEmitsServerSettingsOnStream0;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
begin
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
    finally
      Conn.Free;
    end;
    Outp := TBytesStream.Create;
    try
      Outp.WriteBuffer(Dup.OutputBytes[0], Length(Dup.OutputBytes));
      Outp.Position := 0;
      H := ReadFrameHeader(Outp);
      AssertEquals('server preface is SETTINGS', Ord(ftSettings), H.FrameType);
      AssertEquals('server preface on stream 0', 0, Integer(H.StreamID));
      AssertEquals('server preface not ACK', 0, H.Flags and H2_FLAG_ACK);
    finally
      Outp.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A valid client preface + SETTINGS round-trips: output is server SETTINGS
  (non-ACK) followed by a SETTINGS ACK; the peer settings are retained. }

procedure TTestHTTP2Connection.TestValidBootstrapRoundTripsAndAcks;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H1, H2: TH2FrameHeader;
  Settings: TH2SettingsArray;
begin
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertEquals('retained one peer setting', 1, Length(Conn.RemoteSettings));
      AssertEquals('peer setting id', H2_SETTINGS_MAX_CONCURRENT_STREAMS,
        Conn.RemoteSettings[0].Identifier);
      AssertEquals('peer setting value', 100, Integer(Conn.RemoteSettings[0].Value));
    finally
      Conn.Free;
    end;
    Outp := TBytesStream.Create;
    try
      Outp.WriteBuffer(Dup.OutputBytes[0], Length(Dup.OutputBytes));
      Outp.Position := 0;
      // First frame: server SETTINGS (non-ACK) on stream 0.
      H1 := ReadFrameHeader(Outp);
      ReadSettingsFrame(Outp, H1);
      AssertEquals('first out frame SETTINGS', Ord(ftSettings), H1.FrameType);
      AssertEquals('first out frame not ACK', 0, H1.Flags and H2_FLAG_ACK);
      // Second frame: SETTINGS ACK with empty payload.
      H2 := ReadFrameHeader(Outp);
      AssertEquals('second out frame SETTINGS', Ord(ftSettings), H2.FrameType);
      AssertTrue('second out frame is ACK', (H2.Flags and H2_FLAG_ACK) <> 0);
      AssertEquals('ACK payload empty', 0, Integer(H2.Len));
      Settings := ReadSettingsFrame(Outp, H2);
      AssertEquals('ACK carries no pairs', 0, Length(Settings));
    finally
      Outp.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A corrupted client preface raises EH2ConnectionError(ecProtocolError). }

procedure TTestHTTP2Connection.TestCorruptPrefaceRaisesProtocolError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  BadInput: TBytes;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  BadInput := ValidClientInput;
  BadInput[5] := BadInput[5] xor $FF;   // flip a byte inside the magic
  Dup := TDuplexStream.Create(BadInput);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('corrupt preface raises EH2ConnectionError', Raised);
      AssertEquals('corrupt preface error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A transport that ends before the full 24-octet magic is a
  connection-fatal protocol error }

procedure TTestHTTP2Connection.TestShortPrefaceRaisesProtocolError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  // Only the first 10 octets of the magic, then EOF.
  Dup := TDuplexStream.Create(Copy(ClientPreface, 0, 10));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('short preface raises EH2ConnectionError', Raised);
      AssertEquals('short preface error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A full magic followed by a truncated SETTINGS frame (header
  present, payload missing) is a connection error, not an uncaught stream error  }

procedure TTestHTTP2Connection.TestTruncatedSettingsRaisesProtocolError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Frame, Input: TBytes;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  // A one-pair SETTINGS frame is 9 (header) + 6 (payload) = 15 octets; keep the
  // 9-octet header (claims Len=6) but drop the payload → short read mid-frame.
  Frame := SettingsFrameBytes(ValidClientSettings, 0);
  Input := Concat(ClientPreface, Copy(Frame, 0, 9));
  Dup := TDuplexStream.Create(Input);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('truncated SETTINGS raises EH2ConnectionError', Raised);
      AssertEquals('truncated SETTINGS error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ Bytes supplied via Seed are consumed before transport bytes: seed the
  first 10 magic octets, leave the remaining 14 + SETTINGS on the transport. }

procedure TTestHTTP2Connection.TestSeededBytesConsumedBeforeTransport;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Full, SeedPart, TransportPart: TBytes;
begin
  Full := ValidClientInput;
  SeedPart := Copy(Full, 0, 10);
  TransportPart := Copy(Full, 10, Length(Full) - 10);
  Dup := TDuplexStream.Create(TransportPart);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Seed(SeedPart);
      Conn.Start;   // succeeds only if seeded bytes are replayed before the transport
      AssertEquals('bootstrap succeeded with split preface', 1, Length(Conn.RemoteSettings));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ The first frame after the magic must be a non-ACK SETTINGS on stream 0.
  A PING in that position is a connection error. }

procedure TTestHTTP2Connection.TestNonSettingsAfterMagicRaises;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Ping: TBytesStream;
  Input: TBytes;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  Ping := TBytesStream.Create;
  try
    WritePingFrame(Ping, Bytes([$01, $02, $03, $04, $05, $06, $07, $08]));
    Input := Concat(ClientPreface, Copy(Ping.Bytes, 0, Ping.Size));
  finally
    Ping.Free;
  end;
  Dup := TDuplexStream.Create(Input);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('PING after magic raises EH2ConnectionError', Raised);
      AssertEquals('PING after magic error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A SETTINGS frame on a non-zero stream id after the magic is an error.
  WriteSettingsFrame always emits stream 0, so patch the stream-id field
  (offset 5..8 of the 9-octet header) by hand. }

procedure TTestHTTP2Connection.TestSettingsWrongStreamRaises;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Frame, Input: TBytes;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  Frame := SettingsFrameBytes(nil, 0);   // empty SETTINGS on stream 0
  Frame[8] := $01;                        // force StreamID = 1 (low byte of the dword)
  Input := Concat(ClientPreface, Frame);
  Dup := TDuplexStream.Create(Input);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('SETTINGS on stream 1 raises EH2ConnectionError', Raised);
      AssertEquals('wrong-stream SETTINGS error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ An ACK-flagged SETTINGS as the first post-magic frame is an error
  (the client preface SETTINGS must not be an ACK). }

procedure TTestHTTP2Connection.TestAckFlaggedSettingsAfterMagicRaises;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Input: TBytes;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  Input := Concat(ClientPreface, SettingsFrameBytes(nil, H2_FLAG_ACK));
  Dup := TDuplexStream.Create(Input);
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('ACK SETTINGS after magic raises EH2ConnectionError', Raised);
      AssertEquals('ACK SETTINGS error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ DecodeHeaderBlock of the known static-table block $82 yields exactly
  one header :method = GET (RFC 7541 Appendix A static index 2). }

procedure TTestHTTP2Connection.TestDecodeKnownStaticBlock;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  List: THPackHeaderTextList;
begin
  Dup := TDuplexStream.Create(nil);
  try
    Conn := TH2Connection.Create(Dup);
    try
      List := THPackHeaderTextList.Create;
      try
        Conn.DecodeHeaderBlock(Bytes([$82]), List);
        AssertEquals('one header decoded from $82', 1, List.Count);
        AssertEquals('static index 2 name', ':method', List[0]^.HeaderName);
        AssertEquals('static index 2 value', 'GET', List[0]^.HeaderValue);
      finally
        List.Free;
      end;
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ Round-trip through the locked egress: SendHeaders over the duplex
  stream, parse the emitted HEADERS frame, and decode its fragment with a
  SEPARATE connection - the original header list comes back, in order, and the
  frame carries END_HEADERS. }

procedure TTestHTTP2Connection.TestSendHeadersLockedRoundTrip;
var
  Dup, Dup2: TDuplexStream;
  Conn, Conn2: TH2Connection;
  List, Decoded: THPackHeaderTextList;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Frag: TBytes;
begin
  Dup := TDuplexStream.Create(nil);
  try
    Conn := TH2Connection.Create(Dup);
    try
      List := THPackHeaderTextList.Create;
      try
        List.Add(':status', '200');
        List.Add('content-type', 'text/plain');
        List.Add('x-test', 'hello');
        Conn.SendHeaders(1, List);

        Outp := TBytesStream.Create;
        try
          Outp.WriteBuffer(Dup.OutputBytes[0], Length(Dup.OutputBytes));
          Outp.Position := 0;
          H := ReadFrameHeader(Outp);
          AssertEquals('round-trip frame is HEADERS', Ord(ftHeaders), H.FrameType);
          AssertEquals('round-trip stream id', 1, Integer(H.StreamID));
          AssertTrue('round-trip END_HEADERS set', (H.Flags and H2_FLAG_END_HEADERS) <> 0);
          Frag := ReadHeadersFrame(Outp, H);

          Dup2 := TDuplexStream.Create(nil);
          try
            Conn2 := TH2Connection.Create(Dup2);
            try
              Decoded := THPackHeaderTextList.Create;
              try
                Conn2.DecodeHeaderBlock(Frag, Decoded);
                AssertEquals('decoded count matches', List.Count, Decoded.Count);
                AssertEquals('h0 name', List[0]^.HeaderName, Decoded[0]^.HeaderName);
                AssertEquals('h0 value', List[0]^.HeaderValue, Decoded[0]^.HeaderValue);
                AssertEquals('h1 name', List[1]^.HeaderName, Decoded[1]^.HeaderName);
                AssertEquals('h1 value', List[1]^.HeaderValue, Decoded[1]^.HeaderValue);
                AssertEquals('h2 name', List[2]^.HeaderName, Decoded[2]^.HeaderName);
                AssertEquals('h2 value', List[2]^.HeaderValue, Decoded[2]^.HeaderValue);
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
          Outp.Free;
        end;
      finally
        List.Free;
      end;
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ Single connection-scoped encoder: the SAME custom header sent twice on
  one connection encodes the second time as a short indexed reference (the
  dynamic table inserted on the first emit), so the second fragment is strictly
  shorter - proving exactly one encoder/table is reused. }

procedure TTestHTTP2Connection.TestSingleEncoderPersistsAcrossSends;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  List: THPackHeaderTextList;
  Outp: TBytesStream;
  H1, H2hdr: TH2FrameHeader;
  Frag1, Frag2: TBytes;
begin
  Dup := TDuplexStream.Create(nil);
  try
    Conn := TH2Connection.Create(Dup);
    try
      List := THPackHeaderTextList.Create;
      try
        List.Add('x-custom', 'abcdefgh');
        Conn.SendHeaders(1, List);
        Conn.SendHeaders(3, List);
      finally
        List.Free;
      end;

      Outp := TBytesStream.Create;
      try
        Outp.WriteBuffer(Dup.OutputBytes[0], Length(Dup.OutputBytes));
        Outp.Position := 0;
        H1 := ReadFrameHeader(Outp);
        Frag1 := ReadHeadersFrame(Outp, H1);
        H2hdr := ReadFrameHeader(Outp);
        Frag2 := ReadHeadersFrame(Outp, H2hdr);
        AssertEquals('first emit on stream 1', 1, Integer(H1.StreamID));
        AssertEquals('second emit on stream 3', 3, Integer(H2hdr.StreamID));
        AssertTrue('repeated header indexes shorter on second emit',
          Length(Frag2) < Length(Frag1));
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

{ A HEADERS frame on stream 1 opens it: the stream is tracked and lands
  in ssOpen (no END_STREAM). }

procedure TTestHTTP2Connection.TestHeadersOpensStream;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;
      AssertTrue('stream 1 is tracked', Conn.FindStream(1) <> nil);
      AssertEquals('stream 1 is open', Ord(ssOpen), Ord(Conn.FindStream(1).State));
      AssertEquals('last peer stream id', 1, Integer(Conn.LastPeerStreamID));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A HEADERS frame with END_STREAM half-closes the remote side: the
  stream opens directly into ssHalfClosedRemote. }

procedure TTestHTTP2Connection.TestHeadersEndStreamHalfClosesRemote;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;
      AssertEquals('stream 1 half-closed (remote)',
        Ord(ssHalfClosedRemote), Ord(Conn.FindStream(1).State));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ An open stream that then receives DATA with END_STREAM half-closes the remote side. }

procedure TTestHTTP2Connection.TestDataEndStreamHalfClosesRemote;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment),
           DataFrameBytes(1, nil, H2_FLAG_END_STREAM))));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS -> ssOpen
      Conn.ProcessNextFrame;   // DATA(END_STREAM) -> ssHalfClosedRemote
      AssertEquals('stream 1 half-closed (remote) after DATA END_STREAM',
        Ord(ssHalfClosedRemote), Ord(Conn.FindStream(1).State));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ An even (server-id-space) stream id is a connection PROTOCOL_ERROR. }

procedure TTestHTTP2Connection.TestEvenStreamIdRaisesProtocolError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(2, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      try
        Conn.Start;
        Conn.ProcessNextFrame;
      except
        on E: EH2ConnectionError do
        begin
          Raised := True;
          Code := E.ErrorCode;
        end;
      end;
      AssertTrue('even stream id raises EH2ConnectionError', Raised);
      AssertEquals('even stream id error code', Ord(ecProtocolError), Ord(Code));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ Non-monotonic and reused (lower, implicitly-closed) client stream ids
  are connection PROTOCOL_ERRORs (RFC 9113 §5.1.1: a HEADERS opening an id that
  is not strictly greater than the highest opened id is an unexpected stream
  identifier). Two independent connections, both rejected on the second HEADERS:
    - non-monotonic: open 3, then HEADERS on 1 (1 < 3);
    - reused/lower:  open 5, then HEADERS on 3 (3 < 5, idle id implicitly closed).
  }

procedure TTestHTTP2Connection.TestNonMonotonicAndReusedStreamIdRaise;

  procedure AssertSecondHeadersRejected(aFirstID, aSecondID: Cardinal;
    const aLabel: string);
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
    Code: TH2ErrorCode;
  begin
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      Concat(HeadersFrameBytes(aFirstID, StaticGetFragment),
             HeadersFrameBytes(aSecondID, StaticGetFragment))));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Raised := False;
        Code := ecNoError;
        try
          Conn.Start;
          Conn.ProcessNextFrame;   // opens aFirstID
          Conn.ProcessNextFrame;   // aSecondID <= last -> reject
        except
          on E: EH2ConnectionError do
          begin
            Raised := True;
            Code := E.ErrorCode;
          end;
        end;
        AssertTrue(aLabel + ' raises EH2ConnectionError', Raised);
        AssertEquals(aLabel + ' error code', Ord(ecProtocolError), Ord(Code));
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  end;

begin
  AssertSecondHeadersRejected(3, 1, 'non-monotonic stream id');
  AssertSecondHeadersRejected(5, 3, 'reused (lower) stream id');
end;

{ Interleaved streams 1 and 3 are tracked with independent state: after
  oppening both, DATA(END_STREAM) on 1 half-closes 1's remote side while 3 stays
  open. }

procedure TTestHTTP2Connection.TestMultiplexedStreamsTrackedIndependently;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment),
    Concat(HeadersFrameBytes(3, StaticGetFragment),
           DataFrameBytes(1, nil, H2_FLAG_END_STREAM)))));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS stream 1 -> ssOpen
      Conn.ProcessNextFrame;   // HEADERS stream 3 -> ssOpen
      Conn.ProcessNextFrame;   // DATA(END_STREAM) stream 1 -> ssHalfClosedRemote
      AssertTrue('stream 1 tracked', Conn.FindStream(1) <> nil);
      AssertTrue('stream 3 tracked', Conn.FindStream(3) <> nil);
      AssertEquals('stream 1 half-closed (remote)',
        Ord(ssHalfClosedRemote), Ord(Conn.FindStream(1).State));
      AssertEquals('stream 3 still open',
        Ord(ssOpen), Ord(Conn.FindStream(3).State));
      AssertEquals('last peer stream id is 3', 3, Integer(Conn.LastPeerStreamID));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ RST_STREAM on an open stream transitions it to ssClosed. }

procedure TTestHTTP2Connection.TestRSTStreamClosesStream;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment),
           RstStreamFrameBytes(1, Ord(ecCancel)))));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS -> ssOpen
      Conn.ProcessNextFrame;   // RST_STREAM -> ssClosed
      AssertEquals('stream 1 closed after RST_STREAM',
        Ord(ssClosed), Ord(Conn.FindStream(1).State));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ DATA on a half-closed(remote) stream is a stream error STREAM_CLOSED:
  the peer already ended its side with END_STREAM. }

procedure TTestHTTP2Connection.TestDataOnHalfClosedRemoteRaisesStreamClosed;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Raised: Boolean;
  Code: TH2ErrorCode;
  SID: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM),
           DataFrameBytes(1, nil))));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Raised := False;
      Code := ecNoError;
      SID := 0;
      try
        Conn.Start;
        Conn.ProcessNextFrame;   // HEADERS(END_STREAM) -> ssHalfClosedRemote
        Conn.ProcessNextFrame;   // DATA on half-closed(remote) -> stream error
      except
        on E: EH2StreamError do
        begin
          Raised := True;
          Code := E.ErrorCode;
          SID := E.StreamID;
        end;
      end;
      AssertTrue('DATA on half-closed(remote) raises EH2StreamError', Raised);
      AssertEquals('stream error code', Ord(ecStreamClosed), Ord(Code));
      AssertEquals('stream error stream id', 1, Integer(SID));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A stream's initial send window equals the peer's advertised
  SETTINGS_INITIAL_WINDOW_SIZE (§6.9.2) - NOT the fixed connection window. }

procedure TTestHTTP2Connection.TestStreamSendWindowSeededFromPeerInitialWindow;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ClientInputWithWindow(12345),
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1
      AssertEquals('stream send window seeded from peer INITIAL_WINDOW_SIZE',
        Int64(12345), Conn.FindStream(1).SendWindow);
      // The connection window is fixed at 65535, independent of the setting.
      AssertEquals('connection window unaffected by INITIAL_WINDOW_SIZE',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE), Conn.SendWindow);
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ SendData within the window emits one DATA frame with exactly the given
  bytes (END_STREAM set when requested), decrements BOTH windows, returns the
  byte count, and drives the send-side state transition. }

procedure TTestHTTP2Connection.TestSendDataWithinWindowEmitsFrameAndDecrements;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Payload, Data: TBytes;
  N: Integer;
begin
  Data := Bytes([Ord('h'), Ord('e'), Ord('l'), Ord('l'), Ord('o')]);   // 5 octets
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (ssOpen, default 65535 window)
      N := Conn.SendData(1, Data, True);
      AssertEquals('returns octets written', 5, N);
      AssertEquals('connection window decremented',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) - 5, Conn.SendWindow);
      AssertEquals('stream window decremented',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) - 5, Conn.FindStream(1).SendWindow);
      AssertEquals('END_STREAM drove send-side half-close (local)',
        Ord(ssHalfClosedLocal), Ord(Conn.FindStream(1).State));

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is DATA', Ord(ftData), H.FrameType);
        AssertEquals('DATA on stream 1', 1, Integer(H.StreamID));
        AssertTrue('DATA carries END_STREAM', (H.Flags and H2_FLAG_END_STREAM) <> 0);
        Payload := ReadDataFrame(Outp, H);
        AssertSameBytes('DATA payload', Data, Payload);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ SendData of data larger than a small stream window sends only
  window-worth of bytes in one frame, returns that count, drives the stream
  window to 0, and leaves the remainder unsent (no END_STREAM). }

procedure TTestHTTP2Connection.TestSendDataExceedingStreamWindowSendsPartial;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Payload, Data: TBytes;
  N: Integer;
begin
  Data := FilledBytes(10, $41);   // 10 octets, stream window will be only 4
  Dup := TDuplexStream.Create(Concat(ClientInputWithWindow(4),
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 with a 4-octet send window
      N := Conn.SendData(1, Data, True);
      AssertEquals('sent only the window-worth of bytes', 4, N);
      AssertEquals('stream window driven to 0', Int64(0), Conn.FindStream(1).SendWindow);
      AssertEquals('connection window decremented by the 4 sent',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) - 4, Conn.SendWindow);
      // Remainder unsent => no END_STREAM => stream stays open (no send transition).
      AssertEquals('stream stays open (no END_STREAM sent)',
        Ord(ssOpen), Ord(Conn.FindStream(1).State));

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is DATA', Ord(ftData), H.FrameType);
        AssertEquals('partial DATA length is the window', 4, Integer(H.Len));
        AssertEquals('partial DATA does NOT carry END_STREAM',
          0, H.Flags and H2_FLAG_END_STREAM);
        Payload := ReadDataFrame(Outp, H);
        AssertSameBytes('partial payload is first 4 octets', Copy(Data, 0, 4), Payload);
        AssertEquals('only one DATA frame emitted', Outp.Size, Outp.Position);
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

{ When the CONNECTION send window (not the stream window) is the binding
  constraint, SendData sends only connection-window-worth of bytes and stops. The
  stream window is advertised large (100000) so the fixed 65535 connection window
  is the limit; 70000 octets of input drain the connection window to 0 and leave
  the remainder unsent (no END_STREAM) }

procedure TTestHTTP2Connection.TestSendDataLimitedByConnectionWindowSendsPartial;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Data: TBytes;
  N: Integer;
begin
  Data := FilledBytes(70000, $42);   // > 65535 conn window, < 100000 stream window
  Dup := TDuplexStream.Create(Concat(ClientInputWithWindow(100000),
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 with a 100000-octet send window
      N := Conn.SendData(1, Data, True);
      AssertEquals('sent only the connection-window-worth of bytes',
        Integer(H2_DEFAULT_INITIAL_WINDOW_SIZE), N);
      AssertEquals('connection window driven to 0', Int64(0), Conn.SendWindow);
      AssertEquals('stream window debited by the connection-limited amount',
        Int64(100000) - H2_DEFAULT_INITIAL_WINDOW_SIZE, Conn.FindStream(1).SendWindow);
      // Remainder unsent => no END_STREAM => stream stays open (no send transition).
      AssertEquals('stream stays open (no END_STREAM sent)',
        Ord(ssOpen), Ord(Conn.FindStream(1).State));
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ SendData of data larger than H2_DEFAULT_MAX_FRAME_SIZE splits into
  multiple DATA frames each <= 16384, together totalling the input, with
  END_STREAM only on the last. }

procedure TTestHTTP2Connection.TestSendDataLargerThanMaxFrameSizeSplits;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Data, Reassembled, Payload: TBytes;
  N, FrameCount: Integer;
  SawEndStream: Boolean;
begin
  // 20000 > 16384 and < 65535, so it splits into two frames yet fits both default
  // windows (65535) - exercising the frame-size split without a window stall.
  Data := FilledBytes(20000, $5A);
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (default 65535 windows)
      N := Conn.SendData(1, Data, True);
      AssertEquals('returns the full input length', 20000, N);
      AssertEquals('both windows decremented by the full input',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) - 20000, Conn.SendWindow);
      AssertEquals('stream window decremented by the full input',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) - 20000, Conn.FindStream(1).SendWindow);

      Outp := OutputAfterBootstrap(Dup);
      try
        Reassembled := nil;
        FrameCount := 0;
        SawEndStream := False;
        while Outp.Position < Outp.Size do
        begin
          H := ReadFrameHeader(Outp);
          AssertEquals('split frame is DATA', Ord(ftData), H.FrameType);
          AssertEquals('split frame on stream 1', 1, Integer(H.StreamID));
          AssertTrue('each DATA frame is <= the max frame size',
            Integer(H.Len) <= H2_DEFAULT_MAX_FRAME_SIZE);
          // END_STREAM must appear on the last frame only.
          AssertFalse('no frame after an END_STREAM frame', SawEndStream);
          if (H.Flags and H2_FLAG_END_STREAM) <> 0 then
            SawEndStream := True;
          Payload := ReadDataFrame(Outp, H);
          Reassembled := Concat(Reassembled, Payload);
          Inc(FrameCount);
        end;
        AssertTrue('split into more than one frame', FrameCount > 1);
        AssertTrue('last frame carried END_STREAM', SawEndStream);
        AssertSameBytes('reassembled payload equals input', Data, Reassembled);
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

{ A stream WINDOW_UPDATE increases that stream's send window. }

procedure TTestHTTP2Connection.TestStreamWindowUpdateGrowsStreamSendWindow;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment),
           WindowUpdateFrameBytes(1, 1000))));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (65535 window)
      Conn.ProcessNextFrame;   // stream WINDOW_UPDATE(+1000)
      AssertEquals('stream send window grown by the increment',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) + 1000, Conn.FindStream(1).SendWindow);
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A stream-0 WINDOW_UPDATE increases the connection send window. }

procedure TTestHTTP2Connection.TestConnectionWindowUpdateGrowsConnSendWindow;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    WindowUpdateFrameBytes(0, 2000)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // connection (stream 0) WINDOW_UPDATE(+2000)
      AssertEquals('connection send window grown by the increment',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE) + 2000, Conn.SendWindow);
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A zero-increment WINDOW_UPDATE is a PROTOCOL_ERROR: stream-scoped on a
  stream, connection-scoped on stream 0. }

procedure TTestHTTP2Connection.TestZeroIncrementWindowUpdateRaisesProtocolError;

  procedure AssertStreamZeroIncrementRaises;
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
    Code: TH2ErrorCode;
    SID: Cardinal;
  begin
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      Concat(HeadersFrameBytes(1, StaticGetFragment),
             WindowUpdateFrameBytes(1, 0))));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Raised := False;
        Code := ecNoError;
        SID := 0;
        try
          Conn.Start;
          Conn.ProcessNextFrame;   // opens stream 1
          Conn.ProcessNextFrame;   // zero-increment WINDOW_UPDATE on stream 1
        except
          on E: EH2StreamError do
          begin
            Raised := True;
            Code := E.ErrorCode;
            SID := E.StreamID;
          end;
        end;
        AssertTrue('stream zero-increment raises EH2StreamError', Raised);
        AssertEquals('stream zero-increment error code', Ord(ecProtocolError), Ord(Code));
        AssertEquals('stream zero-increment stream id', 1, Integer(SID));
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  end;

  procedure AssertConnectionZeroIncrementRaises;
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
    Code: TH2ErrorCode;
  begin
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      WindowUpdateFrameBytes(0, 0)));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Raised := False;
        Code := ecNoError;
        try
          Conn.Start;
          Conn.ProcessNextFrame;   // zero-increment WINDOW_UPDATE on stream 0
        except
          on E: EH2ConnectionError do
          begin
            Raised := True;
            Code := E.ErrorCode;
          end;
        end;
        AssertTrue('connection zero-increment raises EH2ConnectionError', Raised);
        AssertEquals('connection zero-increment error code',
          Ord(ecProtocolError), Ord(Code));
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  end;

begin
  AssertStreamZeroIncrementRaises;
  AssertConnectionZeroIncrementRaises;
end;

{ A WINDOW_UPDATE that would push a window above 2^31-1 is a
  FLOW_CONTROL_ERROR: stream-scoped on a stream, connection-scoped on stream 0. }

procedure TTestHTTP2Connection.TestOverflowingWindowUpdateRaisesFlowControlError;

  procedure AssertStreamOverflowRaises;
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
    Code: TH2ErrorCode;
    SID: Cardinal;
  begin
    // 65535 + (2^31-1) > 2^31-1 => overflow.
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      Concat(HeadersFrameBytes(1, StaticGetFragment),
             WindowUpdateFrameBytes(1, H2_MAX_WINDOW_SIZE))));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Raised := False;
        Code := ecNoError;
        SID := 0;
        try
          Conn.Start;
          Conn.ProcessNextFrame;   // opens stream 1 (65535 window)
          Conn.ProcessNextFrame;   // overflowing WINDOW_UPDATE on stream 1
        except
          on E: EH2StreamError do
          begin
            Raised := True;
            Code := E.ErrorCode;
            SID := E.StreamID;
          end;
        end;
        AssertTrue('stream overflow raises EH2StreamError', Raised);
        AssertEquals('stream overflow error code', Ord(ecFlowControlError), Ord(Code));
        AssertEquals('stream overflow stream id', 1, Integer(SID));
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  end;

  procedure AssertConnectionOverflowRaises;
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
    Code: TH2ErrorCode;
  begin
    // 65535 + (2^31-1) > 2^31-1 => overflow on the connection window.
    Dup := TDuplexStream.Create(Concat(ValidClientInput,
      WindowUpdateFrameBytes(0, H2_MAX_WINDOW_SIZE)));
    try
      Conn := TH2Connection.Create(Dup);
      try
        Raised := False;
        Code := ecNoError;
        try
          Conn.Start;
          Conn.ProcessNextFrame;   // overflowing WINDOW_UPDATE on stream 0
        except
          on E: EH2ConnectionError do
          begin
            Raised := True;
            Code := E.ErrorCode;
          end;
        end;
        AssertTrue('connection overflow raises EH2ConnectionError', Raised);
        AssertEquals('connection overflow error code',
          Ord(ecFlowControlError), Ord(Code));
      finally
        Conn.Free;
      end;
    finally
      Dup.Free;
    end;
  end;

begin
  AssertStreamOverflowRaises;
  AssertConnectionOverflowRaises;
end;

{ A non-ACK PING is answered with a PING ACK carrying the IDENTICAL 8
  opaque octets (§6.7), emitted through SendFrame; HandleNextFrame returns True
  and the connection is not Closing. }

procedure TTestHTTP2Connection.TestPingNonAckAnsweredWithAckSamePayload;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Opaque, Echoed: TBytes;
begin
  Opaque := Bytes([$DE, $AD, $BE, $EF, $01, $02, $03, $04]);
  Dup := TDuplexStream.Create(Concat(ValidClientInput, PingFrameBytes(Opaque, 0)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HandleNextFrame returns True (connection survives)',
        Conn.HandleNextFrame);
      AssertFalse('connection not Closing after PING', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is PING', Ord(ftPing), H.FrameType);
        AssertEquals('PING ACK on stream 0', 0, Integer(H.StreamID));
        AssertTrue('PING reply carries ACK flag', (H.Flags and H2_FLAG_ACK) <> 0);
        Echoed := ReadPingFrame(Outp, H);
        AssertSameBytes('PING ACK reflects the opaque payload', Opaque, Echoed);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ A PING that IS already an ACK (a reply to a server PING we never
  originate) is consumed and ignored - no response is emitted, so output is
  exactly the two bootstrap SETTINGS frames. }

procedure TTestHTTP2Connection.TestPingAckIgnoredNoResponse;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    PingFrameBytes(Bytes([$01, $02, $03, $04, $05, $06, $07, $08]), H2_FLAG_ACK)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HandleNextFrame returns True', Conn.HandleNextFrame);
      AssertFalse('connection not Closing', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        AssertEquals('PING ACK produced no further frame', Outp.Size, Outp.Position);
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

{ A stream-scoped violation (DATA on an unopened stream ->
  EH2StreamError(ecStreamClosed,1)) is converted to RST_STREAM on that stream;
  HandleNextFrame returns True and the connection survives (not Closing). }

procedure TTestHTTP2Connection.TestStreamErrorEmitsRSTStreamConnectionSurvives;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    DataFrameBytes(1, Bytes([$41, $42, $43]), 0)));   // DATA on never-opened stream 1
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HandleNextFrame returns True (stream error, conn survives)',
        Conn.HandleNextFrame);
      AssertFalse('connection not Closing after stream error', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST_STREAM on stream 1', 1, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('RST_STREAM carries STREAM_CLOSED',
          Cardinal(Ord(ecStreamClosed)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ A connection-scoped violation (HEADERS on an even/server-space id ->
  EH2ConnectionError(ecProtocolError)) is converted to GOAWAY; HandleNextFrame
  returns False and the connection is Closing. The GOAWAY last-stream-id is 0
  (no client stream was opened) and the error code is PROTOCOL_ERROR. }

procedure TTestHTTP2Connection.TestConnectionErrorEmitsGoAwayAndCloses;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(2, StaticGetFragment, H2_FLAG_END_STREAM)));   // even id
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertFalse('HandleNextFrame returns False (connection error)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after GOAWAY', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY last-stream-id is 0 (no stream opened)',
          0, Integer(LastStreamID));
        AssertEquals('GOAWAY carries PROTOCOL_ERROR',
          Cardinal(Ord(ecProtocolError)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ §6.8 discriminator - GOAWAY reports the highest PROCESSED peer id, not
  the highest OPENED. Stream 1 is processed cleanly. Stream 3 OPENS (bumping
  FLastPeerStreamID to 3) but its header block is a HPACK dynamic-table-size-
  update to 20, exceeding HPACKTableLimit=10 -> COMPRESSION_ERROR mid-decode, so
  ProcessStreamFrame raises and stream 3 is never PROCESSED. The GOAWAY last-id
  must therefore be 1 (processed), NOT 3 (opened) - the peer may retry stream 3.
  ($34 = 1-octet dynamic-table-size-update to 20; mechanism shared with
  tcfphttp2limits.TestHPACKTableLimitEmitsGoAwayCompressionError.) }

procedure TTestHTTP2Connection.TestGoAwayReportsHighestProcessedNotOpenedStreamId;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  L: TH2Limits;
  LastStreamID, Code: Cardinal;
begin
  L := DEFAULT_H2_LIMITS;
  L.HPACKTableLimit := 10;   // table-size-update to 20 exceeds it
  Dup := TDuplexStream.Create(Concat(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM)),
    HeadersFrameBytes(3, Bytes([$34]))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('HNF #1 returns True (stream 1 processed)', Conn.HandleNextFrame);
      AssertEquals('LastProcessedPeerStreamID is 1 after stream 1',
        1, Integer(Conn.LastProcessedPeerStreamID));
      AssertFalse('HNF #2 returns False (COMPRESSION_ERROR -> GOAWAY)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after GOAWAY', Conn.Closing);
      // The distinction, made explicit: stream 3 was OPENED but not PROCESSED.
      AssertEquals('LastPeerStreamID (highest opened) is 3',
        3, Integer(Conn.LastPeerStreamID));
      AssertEquals('LastProcessedPeerStreamID (highest processed) stays 1',
        1, Integer(Conn.LastProcessedPeerStreamID));

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY last-stream-id is the PROCESSED id 1 (not opened 3)',
          1, Integer(LastStreamID));
        AssertEquals('GOAWAY carries COMPRESSION_ERROR',
          Cardinal(Ord(ecCompressionError)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ §6.8 baseline - a valid request on stream 1 is processed (id 1), then
  an even id (2) is a connection PROTOCOL_ERROR. GOAWAY now reports the processed
  id 1. Contrast 1.9 TestConnectionErrorEmitsGoAwayAndCloses, which sends ONLY
  the even id and reports last-id 0. }

procedure TTestHTTP2Connection.TestGoAwayLastIdIsProcessedStreamAfterValidRequest;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM)),
    HeadersFrameBytes(2, StaticGetFragment, H2_FLAG_END_STREAM)));   // even id
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HNF #1 returns True (stream 1 processed)', Conn.HandleNextFrame);
      AssertFalse('HNF #2 returns False (even id -> GOAWAY)', Conn.HandleNextFrame);
      AssertTrue('connection is Closing', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY last-stream-id is the processed stream 1',
          1, Integer(LastStreamID));
        AssertEquals('GOAWAY carries PROTOCOL_ERROR',
          Cardinal(Ord(ecProtocolError)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ a stream error produces EXACTLY ONE RST_STREAM and the
  connection survives; a following valid frame on a higher stream id is processed
  normally. DATA on unopened stream 1 -> EH2StreamError(ecStreamClosed) -> one
  RST_STREAM(1); then a valid HEADERS(3) is processed (HNF #2 True, not Closing,
  no echo since OnRequestReady is unassigned). }

procedure TTestHTTP2Connection.TestStreamErrorRSTThenConnectionSurvivesAndProcessesNextFrame;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(Concat(ValidClientInput,
    DataFrameBytes(1, FilledBytes(4, $00))),                       // DATA on unopened 1
    HeadersFrameBytes(3, StaticGetFragment, H2_FLAG_END_STREAM))); // valid, higher id
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HNF #1 returns True (stream error, conn survives)',
        Conn.HandleNextFrame);
      AssertFalse('not Closing after stream error', Conn.Closing);
      AssertTrue('HNF #2 returns True (next valid frame processed)',
        Conn.HandleNextFrame);
      AssertFalse('still not Closing after the valid frame', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST_STREAM on stream 1', 1, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('RST_STREAM carries STREAM_CLOSED',
          Cardinal(Ord(ecStreamClosed)), Code);
        AssertEquals('exactly one RST and no echo from the valid HEADERS',
          Outp.Size, Outp.Position);
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

{ A post-RST stream is closed locally. Open stream 1 (no
  END_STREAM, stays open); a zero-increment WINDOW_UPDATE ->
  EH2StreamError(ecProtocolError) -> RST_STREAM(1) and Task 3 closes the local
  stream. A subsequent DATA on stream 1 is then handled via the CLOSED-state path
  -> EH2StreamError(ecStreamClosed) -> a second RST_STREAM. The STREAM_CLOSED of
  the second RST proves the stream was closed (not still open). }

procedure TTestHTTP2Connection.TestPostRSTStreamTreatedAsClosed;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(Concat(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, 0)),    // open stream 1, no END_STREAM
    WindowUpdateFrameBytes(1, 0)),                  // zero increment -> stream PROTOCOL_ERROR
    DataFrameBytes(1, FilledBytes(4, $00), 0)));    // DATA on the now-closed stream 1
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HNF #1 returns True (stream 1 opened)', Conn.HandleNextFrame);
      AssertTrue('HNF #2 returns True (zero WINDOW_UPDATE -> RST, conn survives)',
        Conn.HandleNextFrame);
      AssertFalse('not Closing after stream error', Conn.Closing);
      AssertTrue('HNF #3 returns True (DATA on closed stream -> RST, conn survives)',
        Conn.HandleNextFrame);
      AssertFalse('still not Closing', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        // RST #1 - the zero-increment WINDOW_UPDATE protocol error.
        H := ReadFrameHeader(Outp);
        AssertEquals('frame #1 is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST #1 on stream 1', 1, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('RST #1 carries PROTOCOL_ERROR',
          Cardinal(Ord(ecProtocolError)), Code);
        // RST #2 - DATA on the now-CLOSED stream is STREAM_CLOSED (proves the close).
        H := ReadFrameHeader(Outp);
        AssertEquals('frame #2 is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST #2 on stream 1', 1, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('post-RST DATA is treated as STREAM_CLOSED (stream was closed)',
          Cardinal(Ord(ecStreamClosed)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ A post-bootstrap SETTINGS_INITIAL_WINDOW_SIZE increase retroactively
  grows every existing stream's send window by the delta (§6.9.2); the connection
  window is untouched. }

procedure TTestHTTP2Connection.TestRetroactiveInitialWindowIncreaseGrowsStreamWindows;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  Dup := TDuplexStream.Create(Concat(Concat(ClientInputWithWindow(100),
    HeadersFrameBytes(1, StaticGetFragment)),
    InitialWindowSettingsFrameBytes(5000)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (send window 100)
      Conn.ProcessNextFrame;   // SETTINGS INITIAL_WINDOW_SIZE 100 -> 5000 (delta +4900)
      AssertEquals('stream window rose by exactly the delta (100 + 4900)',
        Int64(5000), Conn.FindStream(1).SendWindow);
      AssertEquals('connection window unchanged by INITIAL_WINDOW_SIZE',
        Int64(H2_DEFAULT_INITIAL_WINDOW_SIZE), Conn.SendWindow);
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A post-bootstrap SETTINGS_INITIAL_WINDOW_SIZE decrease retroactively
  drives an already-debited stream window NEGATIVE (§6.9.2) and is retained, not
  clamped; a subsequent send emits NO further DATA (MUST NOT send until positive). }

procedure TTestHTTP2Connection.TestRetroactiveInitialWindowDecreaseDrivesWindowNegative;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  N1, N2: Integer;
begin
  // Stream window 10; send 8 -> window 2; then INITIAL_WINDOW_SIZE 10 -> 2
  // (delta -8) -> window 2 + (-8) = -6.
  Dup := TDuplexStream.Create(Concat(Concat(ClientInputWithWindow(10),
    HeadersFrameBytes(1, StaticGetFragment)),
    InitialWindowSettingsFrameBytes(2)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (send window 10)
      N1 := Conn.SendData(1, FilledBytes(8, $41), False);   // 8 octets fit -> window 2
      AssertEquals('first send wrote 8 octets', 8, N1);
      Conn.ProcessNextFrame;   // SETTINGS INITIAL_WINDOW_SIZE 10 -> 2 (delta -8)
      AssertEquals('retroactive decrease drove the stream window negative',
        Int64(-6), Conn.FindStream(1).SendWindow);
      // A further send at a negative window buffers everything and emits nothing.
      N2 := Conn.SendData(1, FilledBytes(5, $42), True);
      AssertEquals('no octets sent at a negative window', 0, N2);
      AssertEquals('stream window still negative (nothing consumed)',
        Int64(-6), Conn.FindStream(1).SendWindow);

      Outp := OutputAfterBootstrap(Dup);
      try
        // The first emitted frame is the 8-octet DATA send (before the SETTINGS).
        H := ReadFrameHeader(Outp);
        AssertEquals('first frame is the 8-octet DATA send', Ord(ftData), H.FrameType);
        AssertEquals('that DATA frame length is 8', 8, Integer(H.Len));
        AssertEquals('that DATA frame carries no END_STREAM',
          0, H.Flags and H2_FLAG_END_STREAM);
        ReadDataFrame(Outp, H);
        // §6.5.3: the post-bootstrap non-ACK SETTINGS that drove the
        // window negative is itself acknowledged AFTER value application, so a
        // SETTINGS-ACK follows the DATA frame.
        H := ReadFrameHeader(Outp);
        AssertEquals('next frame is SETTINGS', Ord(ftSettings), H.FrameType);
        AssertTrue('it is the SETTINGS-ACK', (H.Flags and H2_FLAG_ACK) <> 0);
        AssertEquals('SETTINGS-ACK payload empty', 0, Integer(H.Len));
        ReadSettingsFrame(Outp, H);
        AssertEquals('no DATA frame emitted after the window went negative',
          Outp.Size, Outp.Position);
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

{ A stream opened AFTER a retroactive INITIAL_WINDOW_SIZE change seeds its
  send window from the NEW value, not the bootstrap one (§6.9.2). }

procedure TTestHTTP2Connection.TestNewStreamSeedsFromRetroactiveInitialWindow;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
begin
  // Bootstrap INITIAL_WINDOW_SIZE 100, then change to 7000 BEFORE opening stream 1.
  Dup := TDuplexStream.Create(Concat(Concat(ClientInputWithWindow(100),
    InitialWindowSettingsFrameBytes(7000)),
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // SETTINGS INITIAL_WINDOW_SIZE 100 -> 7000 (no streams yet)
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 -> seeded from the NEW 7000
      AssertEquals('new stream seeded from the retroactive INITIAL_WINDOW_SIZE',
        Int64(7000), Conn.FindStream(1).SendWindow);
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ A post-bootstrap SETTINGS_INITIAL_WINDOW_SIZE above 2^31-1 is a
  connection FLOW_CONTROL_ERROR (§6.5.2): HandleNextFrame emits GOAWAY with
  FLOW_CONTROL_ERROR and marks the connection Closing. }

procedure TTestHTTP2Connection.TestOverMaxInitialWindowRaisesFlowControlError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(ClientInputWithWindow(100),
    InitialWindowSettingsFrameBytes(Cardinal(H2_MAX_WINDOW_SIZE) + 1)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertFalse('HandleNextFrame returns False (connection error)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after the over-max SETTINGS GOAWAY',
        Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY last-stream-id is 0 (no stream opened)',
          0, Integer(LastStreamID));
        AssertEquals('GOAWAY carries FLOW_CONTROL_ERROR',
          Cardinal(Ord(ecFlowControlError)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ Zero/small-window stall-then-resume on the STREAM window: a send that
  exceeds the stream window emits only window-worth and buffers the rest (no
  END_STREAM); a later stream WINDOW_UPDATE drains the remainder and the FINAL
  frame carries END_STREAM. The concatenated payload equals the original input. }

procedure TTestHTTP2Connection.TestStreamWindowStallThenResume;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Data, Reassembled, Payload: TBytes;
  N: Integer;
  SawEndStream: Boolean;
begin
  Data := FilledBytes(10, $41);   // 10 octets; stream window only 4
  Dup := TDuplexStream.Create(Concat(Concat(ClientInputWithWindow(4),
    HeadersFrameBytes(1, StaticGetFragment)),
    WindowUpdateFrameBytes(1, 100)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (window 4)
      N := Conn.SendData(1, Data, True);
      AssertEquals('initial send wrote only the 4-octet window', 4, N);
      AssertEquals('stream stays open (END_STREAM still owed, buffered)',
        Ord(ssOpen), Ord(Conn.FindStream(1).State));
      Conn.ProcessNextFrame;   // stream WINDOW_UPDATE(+100) -> ResumeSends drains the rest
      AssertEquals('END_STREAM flushed on resume -> half-closed (local)',
        Ord(ssHalfClosedLocal), Ord(Conn.FindStream(1).State));

      Outp := OutputAfterBootstrap(Dup);
      try
        Reassembled := nil;
        SawEndStream := False;
        while Outp.Position < Outp.Size do
        begin
          H := ReadFrameHeader(Outp);
          AssertEquals('resumed frame is DATA', Ord(ftData), H.FrameType);
          AssertEquals('DATA on stream 1', 1, Integer(H.StreamID));
          AssertFalse('no frame after an END_STREAM frame', SawEndStream);
          if (H.Flags and H2_FLAG_END_STREAM) <> 0 then
            SawEndStream := True;
          Payload := ReadDataFrame(Outp, H);
          Reassembled := Concat(Reassembled, Payload);
        end;
        AssertTrue('final frame carried END_STREAM', SawEndStream);
        AssertSameBytes('all DATA reassembles to the original input', Data, Reassembled);
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

{ Stall-then-resume on the CONNECTION window: with a large stream window
  the fixed 65535 connection window is the binding constraint; the remainder
  buffers and drains on a stream-0 WINDOW_UPDATE, the final frame carrying
  END_STREAM. }

procedure TTestHTTP2Connection.TestConnectionWindowStallThenResume;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Data, Reassembled, Payload: TBytes;
  N: Integer;
  SawEndStream: Boolean;
begin
  Data := FilledBytes(70000, $42);   // > 65535 conn window, < 100000 stream window
  Dup := TDuplexStream.Create(Concat(Concat(ClientInputWithWindow(100000),
    HeadersFrameBytes(1, StaticGetFragment)),
    WindowUpdateFrameBytes(0, 20000)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (window 100000)
      N := Conn.SendData(1, Data, True);
      AssertEquals('initial send stalled at the connection window',
        Integer(H2_DEFAULT_INITIAL_WINDOW_SIZE), N);
      AssertEquals('connection window driven to 0', Int64(0), Conn.SendWindow);
      AssertEquals('stream stays open (END_STREAM still owed, buffered)',
        Ord(ssOpen), Ord(Conn.FindStream(1).State));
      Conn.ProcessNextFrame;   // connection WINDOW_UPDATE(+20000) -> ResumeSends drains
      AssertEquals('END_STREAM flushed on resume -> half-closed (local)',
        Ord(ssHalfClosedLocal), Ord(Conn.FindStream(1).State));
      AssertEquals('connection window debited by the resumed remainder',
        Int64(20000) - (70000 - H2_DEFAULT_INITIAL_WINDOW_SIZE), Conn.SendWindow);

      Outp := OutputAfterBootstrap(Dup);
      try
        Reassembled := nil;
        SawEndStream := False;
        while Outp.Position < Outp.Size do
        begin
          H := ReadFrameHeader(Outp);
          AssertEquals('frame is DATA', Ord(ftData), H.FrameType);
          AssertEquals('DATA on stream 1', 1, Integer(H.StreamID));
          AssertFalse('no frame after an END_STREAM frame', SawEndStream);
          if (H.Flags and H2_FLAG_END_STREAM) <> 0 then
            SawEndStream := True;
          Payload := ReadDataFrame(Outp, H);
          Reassembled := Concat(Reassembled, Payload);
        end;
        AssertTrue('final frame carried END_STREAM', SawEndStream);
        AssertSameBytes('all DATA reassembles to the original input', Data, Reassembled);
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

{ Fair resume across two stalled streams: with the connection window the
  binding constraint, two streams stall; one large stream-0 WINDOW_UPDATE drains
  BOTH, and ResumeSends visits them in ascending stream-id order (every stream-1
  DATA frame precedes every stream-3 DATA frame). }

procedure TTestHTTP2Connection.TestFairResumeAcrossTwoStalledStreams;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Data1, Data3, Payload: TBytes;
  N1, N3, Idx, Total1, Total3, MaxIdx1, MinIdx3: Integer;
  End1, End3: Boolean;
begin
  Data1 := FilledBytes(70000, $31);   // stalls stream 1 at the 65535 conn window
  Data3 := FilledBytes(5000, $33);    // wholly blocked once the conn window is 0
  Dup := TDuplexStream.Create(Concat(Concat(Concat(ClientInputWithWindow(100000),
    HeadersFrameBytes(1, StaticGetFragment)),
    HeadersFrameBytes(3, StaticGetFragment)),
    WindowUpdateFrameBytes(0, 100000)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1
      Conn.ProcessNextFrame;   // HEADERS opens stream 3
      N1 := Conn.SendData(1, Data1, True);   // sends 65535, conn window -> 0, buffers rest
      N3 := Conn.SendData(3, Data3, True);   // conn window 0 -> sends nothing, all buffered
      AssertEquals('stream 1 stalled at the connection window',
        Integer(H2_DEFAULT_INITIAL_WINDOW_SIZE), N1);
      AssertEquals('stream 3 sent nothing (connection window already 0)', 0, N3);
      Conn.ProcessNextFrame;   // connection WINDOW_UPDATE(+100000) -> ResumeSends both

      Outp := OutputAfterBootstrap(Dup);
      try
        Total1 := 0; Total3 := 0;
        End1 := False; End3 := False;
        MaxIdx1 := -1; MinIdx3 := MaxInt;
        Idx := 0;
        while Outp.Position < Outp.Size do
        begin
          H := ReadFrameHeader(Outp);
          AssertEquals('frame is DATA', Ord(ftData), H.FrameType);
          Payload := ReadDataFrame(Outp, H);
          if H.StreamID = 1 then
          begin
            Inc(Total1, Length(Payload));
            if (H.Flags and H2_FLAG_END_STREAM) <> 0 then End1 := True;
            MaxIdx1 := Idx;
          end
          else if H.StreamID = 3 then
          begin
            Inc(Total3, Length(Payload));
            if (H.Flags and H2_FLAG_END_STREAM) <> 0 then End3 := True;
            if Idx < MinIdx3 then MinIdx3 := Idx;
          end
          else
            Fail('unexpected DATA on stream ' + IntToStr(H.StreamID));
          Inc(Idx);
        end;
        AssertEquals('stream 1 fully drained', 70000, Total1);
        AssertEquals('stream 3 fully drained', 5000, Total3);
        AssertTrue('stream 1 ended with END_STREAM', End1);
        AssertTrue('stream 3 ended with END_STREAM', End3);
        AssertTrue('ascending-id fairness: all stream-1 frames precede stream-3',
          MaxIdx1 < MinIdx3);
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

{ The empty-body END_STREAM stays window-exempt (§6.9.1 regression guard):
  at a zero stream window, SendData(nil, True) still emits the single zero-length
  END_STREAM DATA frame. }

procedure TTestHTTP2Connection.TestEmptyBodyEndStreamWindowExempt;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  N: Integer;
begin
  Dup := TDuplexStream.Create(Concat(ClientInputWithWindow(0),
    HeadersFrameBytes(1, StaticGetFragment)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      Conn.ProcessNextFrame;   // HEADERS opens stream 1 (window 0)
      N := Conn.SendData(1, nil, True);   // zero-length END_STREAM, window-exempt
      AssertEquals('empty body sends zero octets', 0, N);
      AssertEquals('empty-body END_STREAM drove half-close (local) at a zero window',
        Ord(ssHalfClosedLocal), Ord(Conn.FindStream(1).State));

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is DATA', Ord(ftData), H.FrameType);
        AssertEquals('DATA on stream 1', 1, Integer(H.StreamID));
        AssertEquals('zero-length payload', 0, Integer(H.Len));
        AssertTrue('carries END_STREAM', (H.Flags and H2_FLAG_END_STREAM) <> 0);
        ReadDataFrame(Outp, H);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ The §6.9.1 overflow guard holds with a negative base window. A moderate
  WINDOW_UPDATE that only lifts a negative window toward positive does NOT raise
  (the Int64 add does not wrap, no spurious FLOW_CONTROL_ERROR); a WINDOW_UPDATE
  that genuinely pushes the window past 2^31-1 still raises EH2StreamError
  (FLOW_CONTROL_ERROR). }

procedure TTestHTTP2Connection.TestOverflowWindowUpdateWithNegativeBaseWindow;

  // Drive stream 1's send window to -6: window 10, send 8 -> 2, then
  // INITIAL_WINDOW_SIZE 10 -> 2 (delta -8) -> -6. Returns the started connection.
  procedure DriveStreamNegative(out aDup: TDuplexStream; out aConn: TH2Connection;
    const aTail: TBytes);
  begin
    aDup := TDuplexStream.Create(Concat(Concat(Concat(ClientInputWithWindow(10),
      HeadersFrameBytes(1, StaticGetFragment)),
      InitialWindowSettingsFrameBytes(2)), aTail));
    aConn := TH2Connection.Create(aDup);
    aConn.Start;
    aConn.ProcessNextFrame;                 // HEADERS opens stream 1 (window 10)
    aConn.SendData(1, FilledBytes(8, $41), False);   // window 10 -> 2
    aConn.ProcessNextFrame;                 // SETTINGS delta -8 -> window -6
  end;

  procedure AssertModerateIncrementDoesNotRaise;
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
  begin
    // Tail: a moderate WINDOW_UPDATE(+10) on stream 1 lifts -6 -> 4 (no overflow).
    DriveStreamNegative(Dup, Conn, WindowUpdateFrameBytes(1, 10));
    try
      AssertEquals('precondition: window is negative', Int64(-6),
        Conn.FindStream(1).SendWindow);
      Raised := False;
      try
        Conn.ProcessNextFrame;   // WINDOW_UPDATE(+10): -6 + 10 = 4, no raise
      except
        on EH2StreamError do Raised := True;
      end;
      AssertFalse('moderate increment from a negative base does NOT raise', Raised);
      AssertEquals('window correctly lifted to +4 (Int64, no wrap)',
        Int64(4), Conn.FindStream(1).SendWindow);
    finally
      Conn.Free;
      Dup.Free;
    end;
  end;

  procedure AssertOverflowStillRaises;
  var
    Dup: TDuplexStream;
    Conn: TH2Connection;
    Raised: Boolean;
    Code: TH2ErrorCode;
    SID: Cardinal;
  begin
    // A single 31-bit increment cannot overflow a negative base, so first lift the
    // window to its legal maximum with WINDOW_UPDATE(+2^31-1): -6 + (2^31-1) =
    // 2^31-7 (no raise), then a second WINDOW_UPDATE(+100) pushes past 2^31-1 and
    // MUST raise FLOW_CONTROL_ERROR - confirming the guard still fires after a
    // negative excursion.
    DriveStreamNegative(Dup, Conn, Concat(
      WindowUpdateFrameBytes(1, H2_MAX_WINDOW_SIZE),
      WindowUpdateFrameBytes(1, 100)));
    try
      Raised := False;
      Code := ecNoError;
      SID := 0;
      try
        Conn.ProcessNextFrame;   // +2^31-1 -> 2147483641 (no raise)
        Conn.ProcessNextFrame;   // +100 -> 2147483741 > 2^31-1 -> raises
      except
        on E: EH2StreamError do
        begin
          Raised := True;
          Code := E.ErrorCode;
          SID := E.StreamID;
        end;
      end;
      AssertTrue('overflow past 2^31-1 still raises EH2StreamError', Raised);
      AssertEquals('overflow error code', Ord(ecFlowControlError), Ord(Code));
      AssertEquals('overflow stream id', 1, Integer(SID));
    finally
      Conn.Free;
      Dup.Free;
    end;
  end;

begin
  AssertModerateIncrementDoesNotRaise;
  AssertOverflowStillRaises;
end;

{ graceful shutdown - after a request on stream 1 is PROCESSED,
  Conn.BeginShutdown (default ecNoError) emits EXACTLY ONE GOAWAY carrying the
  highest PROCESSED peer id (1) and the NO_ERROR code, and sets Closing. No
  OnRequestReady is wired, so no response frames sit between the bootstrap
  SETTINGS and the GOAWAY. }

procedure TTestHTTP2Connection.TestBeginShutdownEmitsGracefulGoAwayWithLastProcessedId;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('stream 1 processed (HNF True)', Conn.HandleNextFrame);
      AssertEquals('LastProcessedPeerStreamID is 1', 1,
        Integer(Conn.LastProcessedPeerStreamID));
      AssertFalse('not Closing before BeginShutdown', Conn.Closing);

      Conn.BeginShutdown;   // default ecNoError = graceful
      AssertTrue('Closing after BeginShutdown', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY last-stream-id is the processed id 1',
          1, Integer(LastStreamID));
        AssertEquals('graceful GOAWAY carries NO_ERROR',
          Cardinal(Ord(ecNoError)), Code);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ BeginShutdown is idempotent - calling it twice emits only ONE GOAWAY;
  the second call early-exits on FClosing. }

procedure TTestHTTP2Connection.TestBeginShutdownIsIdempotent;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('stream 1 processed', Conn.HandleNextFrame);

      Conn.BeginShutdown;
      Conn.BeginShutdown;   // second call must be a no-op (no second GOAWAY)
      AssertTrue('still Closing', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('first emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY last-stream-id is 1', 1, Integer(LastStreamID));
        AssertEquals('GOAWAY carries NO_ERROR', Cardinal(Ord(ecNoError)), Code);
        AssertEquals('exactly ONE GOAWAY (no second frame)',
          Outp.Size, Outp.Position);
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

{ §6.8 - once Closing, a NEW client stream id is REFUSED with
  RST_STREAM(REFUSED_STREAM) and the connection SURVIVES, while an already-open
  in-flight stream keeps being processed and drains.}
procedure TTestHTTP2Connection.TestNewStreamAfterGoAwayRefusedInFlightSurvives;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, GoCode, RstCode: Cardinal;
begin
  // preface+settings | HEADERS(1, open, NO end_stream) | HEADERS(3) | DATA(1, end)
  Dup := TDuplexStream.Create(
    Concat(Concat(Concat(ValidClientInput,
      HeadersFrameBytes(1, StaticGetFragment, 0)),
      HeadersFrameBytes(3, StaticGetFragment, 0)),
      DataFrameBytes(1, Bytes([$41, $42, $43]), H2_FLAG_END_STREAM)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('HEADERS(1) opens in-flight stream (HNF True)',
        Conn.HandleNextFrame);
      AssertTrue('stream 1 is open/in-flight', Assigned(Conn.FindStream(1)));

      Conn.BeginShutdown;
      AssertTrue('Closing after BeginShutdown', Conn.Closing);

      // HEADERS(3): a NEW id while Closing → REFUSED_STREAM → RST_STREAM, survives.
      AssertTrue('new-stream HEADERS refused but connection survives (HNF True)',
        Conn.HandleNextFrame);
      AssertTrue('stream 3 was never opened', Conn.FindStream(3) = nil);
      AssertTrue('in-flight stream 1 not torn down', Assigned(Conn.FindStream(1)));

      // DATA(1, END_STREAM): the in-flight stream drains (still processed).
      AssertTrue('in-flight DATA(1) still processed (HNF True)',
        Conn.HandleNextFrame);
      AssertTrue('stream 1 still present after draining',
        Assigned(Conn.FindStream(1)));

      Outp := OutputAfterBootstrap(Dup);
      try
        // First: the graceful GOAWAY from BeginShutdown.
        H := ReadFrameHeader(Outp);
        AssertEquals('first frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, GoCode);
        AssertEquals('GOAWAY carries NO_ERROR', Cardinal(Ord(ecNoError)), GoCode);
        // Then: the REFUSED_STREAM RST for stream 3.
        H := ReadFrameHeader(Outp);
        AssertEquals('second frame is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST_STREAM on the refused stream 3', 3, Integer(H.StreamID));
        RstCode := ReadRSTStreamFrame(Outp, H);
        AssertEquals('RST_STREAM carries REFUSED_STREAM',
          Cardinal(Ord(ecRefusedStream)), RstCode);
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ The error-path GOAWAY guard - after BeginShutdown (NO_ERROR GOAWAY,
  Closing=True), a subsequent connection-scoped violation (even stream id 2 →
  PROTOCOL_ERROR) drives HandleNextFrame whose EH2ConnectionError arm sees
  FClosing=True and does NOT emit a second GOAWAY. HNF returns False; exactly ONE
  GOAWAY total, and it is the original graceful NO_ERROR one (last id 1).}
  
procedure TTestHTTP2Connection.TestErrorPathGoAwayAfterShutdownNoDoubleGoAway;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
begin
  Dup := TDuplexStream.Create(Concat(Concat(ValidClientInput,
    HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM)),
    HeadersFrameBytes(2, StaticGetFragment, H2_FLAG_END_STREAM)));   // even id
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('stream 1 processed', Conn.HandleNextFrame);
      Conn.BeginShutdown;   // graceful GOAWAY(NO_ERROR), Closing=True
      // A connection error WHILE already Closing must not emit a second GOAWAY.
      AssertFalse('connection error returns False', Conn.HandleNextFrame);
      AssertTrue('still Closing', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('the single GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('it is the graceful GOAWAY (last id 1)', 1, Integer(LastStreamID));
        AssertEquals('it carries NO_ERROR (not a second error GOAWAY)',
          Cardinal(Ord(ecNoError)), Code);
        AssertEquals('exactly one GOAWAY total (no double-GOAWAY)',
          Outp.Size, Outp.Position);
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

{ post-bootstrap non-ACK SETTINGS MUST be acknowledged with an
  empty-payload SETTINGS carrying the ACK flag. Feed one INITIAL_WINDOW_SIZE
  SETTINGS through the frame loop; past the two bootstrap SETTINGS the next
  emitted frame is the SETTINGS-ACK (stream 0, ACK flag, length 0). }

procedure TTestHTTP2Connection.TestPostBootstrapSettingsAcked;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Settings: TH2SettingsArray;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    InitialWindowSettingsFrameBytes(5000)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('post-bootstrap SETTINGS processed (HNF True)', Conn.HandleNextFrame);
      AssertFalse('connection not Closing after a normal SETTINGS', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is SETTINGS', Ord(ftSettings), H.FrameType);
        AssertEquals('SETTINGS-ACK on stream 0', 0, Integer(H.StreamID));
        AssertTrue('SETTINGS carries the ACK flag', (H.Flags and H2_FLAG_ACK) <> 0);
        AssertEquals('SETTINGS-ACK payload empty', 0, Integer(H.Len));
        Settings := ReadSettingsFrame(Outp, H);
        AssertEquals('ACK carries no settings pairs', 0, Length(Settings));
        AssertEquals('no further frames emitted', Outp.Size, Outp.Position);
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

{ §6.5.3 - an ACK-flagged SETTINGS (the peer acking OUR server SETTINGS)
  is consumed WITHOUT a reply: acking an ACK would loop forever. Feeding one
  produces NO frame past the two bootstrap SETTINGS. }

procedure TTestHTTP2Connection.TestAckFlaggedSettingsNotReacked;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Outp: TBytesStream;
begin
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    SettingsFrameBytes(nil, H2_FLAG_ACK)));
  try
    Conn := TH2Connection.Create(Dup);
    try
      Conn.Start;
      AssertTrue('SETTINGS-ACK consumed (HNF True)', Conn.HandleNextFrame);
      AssertFalse('connection not Closing after a SETTINGS-ACK', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        AssertEquals('a SETTINGS-ACK is never re-acked (no further frame)',
          Outp.Size, Outp.Position);
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

initialization

  RegisterTest(TTestHTTP2Connection);
end.
