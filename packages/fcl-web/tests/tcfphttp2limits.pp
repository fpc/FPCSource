unit tcfphttp2limits;

{$mode objfpc}{$H+}

{
  Reject-path suite: 

  Cases (one per reject path):
    - MaxConcurrentStreams  -> REFUSED_STREAM (RST_STREAM), + HPACK stays in sync
    - MaxFrameSize (stream) -> FRAME_SIZE_ERROR (RST_STREAM), connection survives
    - MaxFrameSize (conn)   -> FRAME_SIZE_ERROR (GOAWAY), connection closes
    - MaxContinuationBytes  -> COMPRESSION_ERROR (GOAWAY) [CVE-2024-27316]
    - MaxHeaderListSize     -> COMPRESSION_ERROR (GOAWAY)
    - HPACKTableLimit       -> COMPRESSION_ERROR (GOAWAY) [RFC 7541 §6.3]
    - SETTINGS advertisement: server SETTINGS carries MAX_CONCURRENT_STREAMS
}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fphttp2connection, fphttp2frames, fphttp2consts, uhpack;

type
  // In-memory duplex TStream (copy of the tcfphttp2connection idiom): 
  // Read pops from a preloaded input byte vector; Write appends to an output buffer.
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

  TTestHTTP2Limits = class(TTestCase)
  private
    function Bytes(const aBytes: array of Byte): TBytes;
    function ClientPreface: TBytes;
    function SettingsFrameBytes(const aSettings: TH2SettingsArray; aFlags: Byte = 0): TBytes;
    function Concat(const aHead, aTail: TBytes): TBytes;
    function ValidClientSettings: TH2SettingsArray;
    function ValidClientInput: TBytes;
    // HEADERS with END_HEADERS always set (single complete block).
    function HeadersFrameBytes(aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0): TBytes;
    // HEADERS WITHOUT END_HEADERS (so a CONTINUATION must follow) - for the
    // MaxContinuationBytes flood case.
    function HeadersFrameBytesNoEnd(aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0): TBytes;
    function ContinuationFrameBytes(aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0): TBytes;
    function DataFrameBytes(aStreamID: Cardinal; const aData: TBytes; aFlags: Byte = 0): TBytes;
    // flood builders (codec writers already exist; mirror SettingsFrameBytes).
    function PingFrameBytes(const aOpaque: TBytes; aFlags: Byte = 0): TBytes;
    function RSTStreamFrameBytes(aStreamID: Cardinal; aErrorCode: Cardinal): TBytes;
    function StaticGetFragment: TBytes;
    function FilledBytes(aLen: Integer; aFill: Byte): TBytes;
    // Encode one header pair through aEnc (a persistent client-side encoder) into
    // a header-block fragment. Used to build HPACK-valid blocks whose dynamic-
    // table references prove the decoder stayed in sync across a refused stream.
    function EncodeBlock(aEnc: THPackEncoder; const aName, aValue: RawByteString): TBytes;
    // A TH2Limits seeded with the safe defaults; tests tighten individual fields.
    function DefaultLimits: TH2Limits;
    // Output positioned just past the two bootstrap SETTINGS frames.
    function OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
    procedure AssertSameBytes(const aMsg: string; const aExpected, aActual: TBytes);
  published
    procedure TestMaxConcurrentStreamsRefusesAndHPACKStaysInSync;
    procedure TestMaxFrameSizeStreamScopeEmitsRSTStream;
    procedure TestMaxFrameSizeConnectionScopeEmitsGoAway;
    procedure TestMaxContinuationBytesEmitsGoAwayCompressionError;
    procedure TestMaxHeaderListSizeEmitsGoAwayCompressionError;
    procedure TestHPACKTableLimitEmitsGoAwayCompressionError;
    procedure TestServerSettingsAdvertisesMaxConcurrentStreams;
    // flood / rapid-reset reject paths.
    procedure TestRapidResetBudgetEmitsGoAwayEnhanceYourCalm;
    procedure TestPingFloodEmitsGoAwayEnhanceYourCalm;
    procedure TestSettingsFloodEmitsGoAwayEnhanceYourCalm;
    procedure TestCompletedRequestCreditsFloodBudget;
    procedure TestSettingsAckDoesNotCountTowardFlood;
    // §4.2 - per-CONTINUATION frame-size enforcement.
    procedure TestPerContinuationFrameSizeEmitsGoAway;
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

function TTestHTTP2Limits.Bytes(const aBytes: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(aBytes));
  for I := 0 to High(aBytes) do
    Result[I] := aBytes[I];
end;

function TTestHTTP2Limits.ClientPreface: TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(H2_CLIENT_PREFACE));
  for I := 0 to High(H2_CLIENT_PREFACE) do
    Result[I] := H2_CLIENT_PREFACE[I];
end;

function TTestHTTP2Limits.SettingsFrameBytes(const aSettings: TH2SettingsArray;
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

function TTestHTTP2Limits.Concat(const aHead, aTail: TBytes): TBytes;
begin
  SetLength(Result, Length(aHead) + Length(aTail));
  if Length(aHead) > 0 then
    Move(aHead[0], Result[0], Length(aHead));
  if Length(aTail) > 0 then
    Move(aTail[0], Result[Length(aHead)], Length(aTail));
end;

function TTestHTTP2Limits.ValidClientSettings: TH2SettingsArray;
begin
  SetLength(Result, 1);
  Result[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  Result[0].Value := 100;
end;

function TTestHTTP2Limits.ValidClientInput: TBytes;
begin
  Result := Concat(ClientPreface, SettingsFrameBytes(ValidClientSettings, 0));
end;

function TTestHTTP2Limits.HeadersFrameBytes(aStreamID: Cardinal;
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

function TTestHTTP2Limits.HeadersFrameBytesNoEnd(aStreamID: Cardinal;
  const aFragment: TBytes; aFlags: Byte = 0): TBytes;
var
  S: TBytesStream;
begin
  // Deliberately NO END_HEADERS: the demux's AssembleHeaderBlock will keep
  // reading CONTINUATION frames (used to drive the accumulation cap).
  S := TBytesStream.Create;
  try
    WriteHeadersFrame(S, aStreamID, aFragment, aFlags);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Limits.ContinuationFrameBytes(aStreamID: Cardinal;
  const aFragment: TBytes; aFlags: Byte = 0): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    WriteContinuationFrame(S, aStreamID, aFragment, aFlags);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Limits.DataFrameBytes(aStreamID: Cardinal;
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

function TTestHTTP2Limits.PingFrameBytes(const aOpaque: TBytes;
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

function TTestHTTP2Limits.RSTStreamFrameBytes(aStreamID: Cardinal;
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

function TTestHTTP2Limits.StaticGetFragment: TBytes;
begin
  Result := Bytes([$82]);   // RFC 7541 static index 2 = ':method: GET' (no dyn-table mutation)
end;

function TTestHTTP2Limits.FilledBytes(aLen: Integer; aFill: Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, aLen);
  for I := 0 to aLen - 1 do
    Result[I] := aFill;
end;

function TTestHTTP2Limits.EncodeBlock(aEnc: THPackEncoder;
  const aName, aValue: RawByteString): TBytes;
var
  S: TBytesStream;
begin
  S := TBytesStream.Create;
  try
    aEnc.EncodeHeader(S, aName, aValue, False);
    Result := Copy(S.Bytes, 0, S.Size);
  finally
    S.Free;
  end;
end;

function TTestHTTP2Limits.DefaultLimits: TH2Limits;
begin
  Result := DEFAULT_H2_LIMITS;
end;

function TTestHTTP2Limits.OutputAfterBootstrap(aDup: TDuplexStream): TBytesStream;
var
  H: TH2FrameHeader;
begin
  Result := TBytesStream.Create;
  Result.WriteBuffer(aDup.OutputBytes[0], Length(aDup.OutputBytes));
  Result.Position := 0;
  // Start emits exactly two SETTINGS frames first (server SETTINGS + ACK). Read
  // both past - ReadSettingsFrame consumes the payload by Len, so the now-non-
  // empty server SETTINGS is handled correctly.
  H := ReadFrameHeader(Result);
  ReadSettingsFrame(Result, H);
  H := ReadFrameHeader(Result);
  ReadSettingsFrame(Result, H);
end;

procedure TTestHTTP2Limits.AssertSameBytes(const aMsg: string;
  const aExpected, aActual: TBytes);
var
  I: Integer;
begin
  AssertEquals(aMsg + ' length', Length(aExpected), Length(aActual));
  for I := 0 to High(aExpected) do
    AssertEquals(aMsg + ' byte ' + IntToStr(I), aExpected[I], aActual[I]);
end;

{ MaxConcurrentStreams -> REFUSED_STREAM, and the decode-before-refuse HPACK-sync
  crux: with limit 1, stream 1 opens (count=1), then HEADERS on stream 3 and 5 are
  refused with RST_STREAM(ecRefusedStream) while the connection survives. The
  blocks are built with one persistent client encoder so that stream 5's block is
  a dynamic-table indexed reference to an entry inserted while DECODING the refused
  stream 3 - it only resolves (no COMPRESSION_ERROR) if the refused stream was
  decoded before being refused. }

procedure TTestHTTP2Limits.TestMaxConcurrentStreamsRefusesAndHPACKStaysInSync;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Enc: THPackEncoder;
  L: TH2Limits;
  Blk1, Blk3, Blk5, Input: TBytes;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Code: Cardinal;
begin
  Enc := THPackEncoder.Create;   // one client-side encoder for the whole connection
  try
    // block1: x-a:1 (incremental indexing -> dyn[63] after block3 shift)
    Blk1 := EncodeBlock(Enc, 'x-a', '1');
    // block3: x-b:2 (incremental indexing -> dyn[62]); decoded though refused
    Blk3 := EncodeBlock(Enc, 'x-b', '2');
    // block5: x-a:1 again -> emitted as an indexed reference to dyn[63]; resolving
    // it requires the decoder to have inserted BOTH x-a:1 and x-b:2 (i.e. to have
    // decoded the refused stream 3).
    Blk5 := EncodeBlock(Enc, 'x-a', '1');
  finally
    Enc.Free;
  end;

  Input := Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, Blk1),
    Concat(HeadersFrameBytes(3, Blk3),
           HeadersFrameBytes(5, Blk5))));

  L := DefaultLimits;
  L.MaxConcurrentStreams := 1;
  Dup := TDuplexStream.Create(Input);
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('stream 1 HEADERS accepted (HandleNextFrame True)',
        Conn.HandleNextFrame);
      AssertEquals('stream 1 is open and counts', Ord(ssOpen),
        Ord(Conn.FindStream(1).State));

      AssertTrue('stream 3 over-limit: HandleNextFrame True (conn survives)',
        Conn.HandleNextFrame);
      AssertFalse('connection not Closing after stream-3 refusal', Conn.Closing);

      AssertTrue('stream 5 over-limit: HandleNextFrame True (HPACK still in sync)',
        Conn.HandleNextFrame);
      AssertFalse('connection not Closing after stream-5 refusal', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        // First emitted frame past bootstrap: RST_STREAM(3, REFUSED_STREAM).
        H := ReadFrameHeader(Outp);
        AssertEquals('first reject is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST_STREAM on stream 3', 3, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('stream 3 refused with REFUSED_STREAM',
          Cardinal(Ord(ecRefusedStream)), Code);
        // Second: RST_STREAM(5, REFUSED_STREAM) - proves block5 decoded cleanly
        // (no COMPRESSION_ERROR GOAWAY), i.e. the decoder table stayed in sync.
        H := ReadFrameHeader(Outp);
        AssertEquals('second reject is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST_STREAM on stream 5', 5, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('stream 5 refused with REFUSED_STREAM',
          Cardinal(Ord(ecRefusedStream)), Code);
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

{ MaxFrameSize (stream scope): an oversize DATA frame on an open stream is a
  stream error -> RST_STREAM(FRAME_SIZE_ERROR); the payload is discarded to stay
  byte-aligned and the connection survives (§4.2). }

procedure TTestHTTP2Limits.TestMaxFrameSizeStreamScopeEmitsRSTStream;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  Code: Cardinal;
  H: TH2FrameHeader;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxFrameSize := 8;   // tiny; the 1-byte HEADERS fits, the 20-byte DATA does not
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment),
           DataFrameBytes(1, FilledBytes(20, $41)))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('HEADERS within MaxFrameSize accepted', Conn.HandleNextFrame);
      AssertTrue('oversize DATA: HandleNextFrame True (stream error, conn survives)',
        Conn.HandleNextFrame);
      AssertFalse('connection not Closing after stream-scope FRAME_SIZE_ERROR',
        Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is RST_STREAM', Ord(ftRSTStream), H.FrameType);
        AssertEquals('RST_STREAM on stream 1', 1, Integer(H.StreamID));
        Code := ReadRSTStreamFrame(Outp, H);
        AssertEquals('RST_STREAM carries FRAME_SIZE_ERROR',
          Cardinal(Ord(ecFrameSizeError)), Code);
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

{ MaxFrameSize (connection scope): an oversize HEADERS frame "could alter the
  state of the entire connection" -> connection error GOAWAY(FRAME_SIZE_ERROR);
  HandleNextFrame returns False and the connection is Closing (§4.2). }

procedure TTestHTTP2Limits.TestMaxFrameSizeConnectionScopeEmitsGoAway;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxFrameSize := 4;   // a 10-byte HEADERS fragment exceeds it
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, FilledBytes(10, $00))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertFalse('oversize HEADERS: HandleNextFrame False (connection error)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after connection-scope FRAME_SIZE_ERROR',
        Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries FRAME_SIZE_ERROR',
          Cardinal(Ord(ecFrameSizeError)), Code);
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

{ MaxContinuationBytes: a HEADERS(no END_HEADERS) plus enough CONTINUATION
  fragment bytes to exceed the accumulation cap is a connection
  COMPRESSION_ERROR -> GOAWAY (CVE-2024-27316). The fragment bytes are never
  decoded - the cap fires at the wire-accumulation stage inside
  AssembleHeaderBlock, before any decode. }

procedure TTestHTTP2Limits.TestMaxContinuationBytesEmitsGoAwayCompressionError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxContinuationBytes := 10;   // 6 (first frag) + 10 (continuation) = 16 > 10
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytesNoEnd(1, FilledBytes(6, $00)),
           ContinuationFrameBytes(1, FilledBytes(10, $00), H2_FLAG_END_HEADERS))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertFalse('CONTINUATION flood: HandleNextFrame False (connection error)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after CONTINUATION flood', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries COMPRESSION_ERROR',
          Cardinal(Ord(ecCompressionError)), Code);
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

{ MaxHeaderListSize: a single HEADERS whose DECODED header list exceeds the cap
  is a connection COMPRESSION_ERROR -> GOAWAY (§6.5.2). The decoder (sized to
  MaxHeaderListSize) reports the overflow via EndHeaderBlockTruncated. }

procedure TTestHTTP2Limits.TestMaxHeaderListSizeEmitsGoAwayCompressionError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  Enc: THPackEncoder;
  L: TH2Limits;
  Blk, Input: TBytes;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  Enc := THPackEncoder.Create;
  try
    // name+value = 5 + 60 = 65 octets of decoded header list, far above the cap.
    Blk := EncodeBlock(Enc, 'x-big', StringOfChar('A', 60));
  finally
    Enc.Free;
  end;
  Input := Concat(ValidClientInput, HeadersFrameBytes(1, Blk));

  L := DefaultLimits;
  L.MaxHeaderListSize := 10;   // far below the 65-octet decoded list
  Dup := TDuplexStream.Create(Input);
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertFalse('oversize header list: HandleNextFrame False (connection error)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after header-list overflow', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries COMPRESSION_ERROR',
          Cardinal(Ord(ecCompressionError)), Code);
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

{ HPACKTableLimit: a header block beginning with a dynamic-table-size-update that
  encodes a size above the limit is a connection COMPRESSION_ERROR -> GOAWAY
  (RFC 7541 §6.3). Hand-crafted byte vector: $34 = '001' prefix (Dynamic Table
  Size Update) + 5-bit value 20 ($14). With HPACKTableLimit = 10, the decoder's
  SetDynamicTableSize(20) raises THPACKException, funnelled to COMPRESSION_ERROR. }

procedure TTestHTTP2Limits.TestHPACKTableLimitEmitsGoAwayCompressionError;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.HPACKTableLimit := 10;   // table-size-update to 20 exceeds it
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    HeadersFrameBytes(1, Bytes([$34]))));   // $34 = dyn-table-size-update(20)
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertFalse('over-limit table-size-update: HandleNextFrame False',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after HPACK-table overflow', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries COMPRESSION_ERROR',
          Cardinal(Ord(ecCompressionError)), Code);
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

{ SETTINGS advertisement (AC#3): the server preface SETTINGS advertises every
  limit we ENFORCE so the peer self-limits and our rejects are RFC-legitimate -
  MAX_CONCURRENT_STREAMS (the AC's explicit requirement) plus MAX_HEADER_LIST_SIZE
  and HEADER_TABLE_SIZE - each equal to its configured value. MAX_FRAME_SIZE is
  advertised ONLY when it differs from the 16384 protocol default; here the limit
  is left at the default, so it MUST be absent (the floor needs no announcement). }

procedure TTestHTTP2Limits.TestServerSettingsAdvertisesMaxConcurrentStreams;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  Outp: TBytesStream;
  H: TH2FrameHeader;
  Settings: TH2SettingsArray;
  I: Integer;
  GotMCS, GotMHLS, GotHTS, GotMFS: Boolean;
begin
  L := DefaultLimits;
  // Distinctive values per limit so the asserted pair is unambiguous. MaxFrameSize
  // is intentionally left at H2_DEFAULT_MAX_FRAME_SIZE so it is NOT advertised.
  L.MaxConcurrentStreams := 42;
  L.MaxHeaderListSize := 12345;
  L.HPACKTableLimit := 2048;
  Dup := TDuplexStream.Create(ValidClientInput);
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
    finally
      Conn.Free;
    end;
    Outp := TBytesStream.Create;
    try
      Outp.WriteBuffer(Dup.OutputBytes[0], Length(Dup.OutputBytes));
      Outp.Position := 0;
      // FIRST output frame is the server preface SETTINGS (non-ACK, stream 0).
      H := ReadFrameHeader(Outp);
      AssertEquals('server preface is SETTINGS', Ord(ftSettings), H.FrameType);
      AssertEquals('server preface on stream 0', 0, Integer(H.StreamID));
      AssertEquals('server preface not ACK', 0, H.Flags and H2_FLAG_ACK);
      Settings := ReadSettingsFrame(Outp, H);
      GotMCS := False; GotMHLS := False; GotHTS := False; GotMFS := False;
      for I := 0 to High(Settings) do
        if Settings[I].Identifier = H2_SETTINGS_MAX_CONCURRENT_STREAMS then
        begin
          GotMCS := True;
          AssertEquals('advertised MAX_CONCURRENT_STREAMS value',
            Integer(42), Integer(Settings[I].Value));
        end
        else if Settings[I].Identifier = H2_SETTINGS_MAX_HEADER_LIST_SIZE then
        begin
          GotMHLS := True;
          AssertEquals('advertised MAX_HEADER_LIST_SIZE value',
            Integer(12345), Integer(Settings[I].Value));
        end
        else if Settings[I].Identifier = H2_SETTINGS_HEADER_TABLE_SIZE then
        begin
          GotHTS := True;
          AssertEquals('advertised HEADER_TABLE_SIZE value',
            Integer(2048), Integer(Settings[I].Value));
        end
        else if Settings[I].Identifier = H2_SETTINGS_MAX_FRAME_SIZE then
          GotMFS := True;
      AssertTrue('server SETTINGS advertises MAX_CONCURRENT_STREAMS', GotMCS);
      AssertTrue('server SETTINGS advertises MAX_HEADER_LIST_SIZE', GotMHLS);
      AssertTrue('server SETTINGS advertises HEADER_TABLE_SIZE', GotHTS);
      AssertFalse('default MAX_FRAME_SIZE is NOT advertised (no announcement needed)',
        GotMFS);
    finally
      Outp.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ Rapid-reset budget (CVE-2023-44487): with MaxResetStreams = 2, three
  open-then-RST sequences on ids 1/3/5 
  The first two resets are within budget; the third (count 3 > 2) trips a connection
  ENHANCE_YOUR_CALM -> GOAWAY at the catch site. Uses HeadersFrameBytes (complete
  END_HEADERS block, StaticGetFragment) so each HEADERS decodes independently. }

procedure TTestHTTP2Limits.TestRapidResetBudgetEmitsGoAwayEnhanceYourCalm;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxResetStreams := 2;
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytes(1, StaticGetFragment),
    Concat(RSTStreamFrameBytes(1, Ord(ecCancel)),
    Concat(HeadersFrameBytes(3, StaticGetFragment),
    Concat(RSTStreamFrameBytes(3, Ord(ecCancel)),
    Concat(HeadersFrameBytes(5, StaticGetFragment),
           RSTStreamFrameBytes(5, Ord(ecCancel)))))))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('HEADERS(1) accepted', Conn.HandleNextFrame);
      AssertTrue('RST(1) within budget (count 1)', Conn.HandleNextFrame);
      AssertTrue('HEADERS(3) accepted', Conn.HandleNextFrame);
      AssertTrue('RST(3) within budget (count 2)', Conn.HandleNextFrame);
      AssertTrue('HEADERS(5) accepted', Conn.HandleNextFrame);
      AssertFalse('RST(5) trips rapid-reset budget (count 3 > 2): HandleNextFrame False',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after rapid-reset trip', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries ENHANCE_YOUR_CALM',
          Cardinal(Ord(ecEnhanceYourCalm)), Code);
        // The received HEADERS/RST_STREAM frames produce no echo and the tripping
        // RST is not answered - the GOAWAY is the only frame past bootstrap.
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

{ PING flood (§6.7): with MaxPingFlood = 2, three non-ACK PINGs. The
  first two are answered with PING-ACKs (within budget); the third (count 3 > 2)
  trips ENHANCE_YOUR_CALM BEFORE being answered (Task 4: count+check precede
  SendPingAck), so the output past bootstrap is exactly two PING ACKs then GOAWAY. }

procedure TTestHTTP2Limits.TestPingFloodEmitsGoAwayEnhanceYourCalm;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxPingFlood := 2;
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(PingFrameBytes(FilledBytes(8, $00)),
    Concat(PingFrameBytes(FilledBytes(8, $00)),
           PingFrameBytes(FilledBytes(8, $00))))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('ping 1 within budget', Conn.HandleNextFrame);
      AssertTrue('ping 2 within budget', Conn.HandleNextFrame);
      AssertFalse('ping 3 trips PING flood (count 3 > 2): HandleNextFrame False',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after PING flood trip', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        // Two PING ACKs (the budgeted pings) precede the GOAWAY.
        H := ReadFrameHeader(Outp);
        AssertEquals('first reply is PING', Ord(ftPing), H.FrameType);
        AssertEquals('first reply is PING-ACK', H2_FLAG_ACK, H.Flags and H2_FLAG_ACK);
        ReadPingFrame(Outp, H);
        H := ReadFrameHeader(Outp);
        AssertEquals('second reply is PING', Ord(ftPing), H.FrameType);
        AssertEquals('second reply is PING-ACK', H2_FLAG_ACK, H.Flags and H2_FLAG_ACK);
        ReadPingFrame(Outp, H);
        // The tripping (3rd) ping is NOT acked - next frame is the GOAWAY.
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries ENHANCE_YOUR_CALM',
          Cardinal(Ord(ecEnhanceYourCalm)), Code);
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

{ SETTINGS flood (§6.5): with MaxSettingsFlood = 2, three non-ACK
  SETTINGS through the frame loop. So the first two emit a
  SETTINGS-ACK each; the third (count 3 > 2) trips ENHANCE_YOUR_CALM BEFORE the
  ACK (the flood check precedes the ACK emit), so the output past bootstrap is
  exactly two SETTINGS-ACKs then the GOAWAY. }

procedure TTestHTTP2Limits.TestSettingsFloodEmitsGoAwayEnhanceYourCalm;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxSettingsFlood := 2;
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(SettingsFrameBytes(nil, 0),
    Concat(SettingsFrameBytes(nil, 0),
           SettingsFrameBytes(nil, 0)))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('settings 1 within budget', Conn.HandleNextFrame);
      AssertTrue('settings 2 within budget', Conn.HandleNextFrame);
      AssertFalse('settings 3 trips SETTINGS flood (count 3 > 2): HandleNextFrame False',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after SETTINGS flood trip', Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        // §6.5.3 : the two within-budget SETTINGS were each acked.
        H := ReadFrameHeader(Outp);
        AssertEquals('first reply is SETTINGS', Ord(ftSettings), H.FrameType);
        AssertEquals('first reply is SETTINGS-ACK', H2_FLAG_ACK, H.Flags and H2_FLAG_ACK);
        ReadSettingsFrame(Outp, H);
        H := ReadFrameHeader(Outp);
        AssertEquals('second reply is SETTINGS', Ord(ftSettings), H.FrameType);
        AssertEquals('second reply is SETTINGS-ACK', H2_FLAG_ACK, H.Flags and H2_FLAG_ACK);
        ReadSettingsFrame(Outp, H);
        // The tripping (3rd) SETTINGS is NOT acked - next frame is the GOAWAY.
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries ENHANCE_YOUR_CALM',
          Cardinal(Ord(ecEnhanceYourCalm)), Code);
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

{ A fully-received request credits the flood budgets. 
  With MaxPingFlood = 2: two pings (count 1,2), then a COMPLETE request (HEADERS with
  END_STREAM) which zeroes FPingCount, then two more pings (count 1,2 again). No
  frame trips, the connection never closes, and all four pings are acked. }

procedure TTestHTTP2Limits.TestCompletedRequestCreditsFloodBudget;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  Outp: TBytesStream;
  I: Integer;
begin
  L := DefaultLimits;
  L.MaxPingFlood := 2;
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(PingFrameBytes(FilledBytes(8, $00)),
    Concat(PingFrameBytes(FilledBytes(8, $00)),
    Concat(HeadersFrameBytes(1, StaticGetFragment, H2_FLAG_END_STREAM),
    Concat(PingFrameBytes(FilledBytes(8, $00)),
           PingFrameBytes(FilledBytes(8, $00))))))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('ping 1 (count 1)', Conn.HandleNextFrame);
      AssertTrue('ping 2 (count 2)', Conn.HandleNextFrame);
      AssertTrue('complete request (credits budget -> FPingCount 0)', Conn.HandleNextFrame);
      AssertTrue('ping 3 (count 1 after credit)', Conn.HandleNextFrame);
      AssertTrue('ping 4 (count 2 after credit)', Conn.HandleNextFrame);
      AssertFalse('connection NOT Closing (budget credited, no trip)', Conn.Closing);

      // Four PING ACKs, no GOAWAY, then end of output.
      Outp := OutputAfterBootstrap(Dup);
      try
        for I := 1 to 4 do
        begin
          H := ReadFrameHeader(Outp);
          AssertEquals('reply ' + IntToStr(I) + ' is PING', Ord(ftPing), H.FrameType);
          AssertEquals('reply ' + IntToStr(I) + ' is PING-ACK',
            H2_FLAG_ACK, H.Flags and H2_FLAG_ACK);
          ReadPingFrame(Outp, H);
        end;
        AssertEquals('no GOAWAY / no further frames emitted', Outp.Size, Outp.Position);
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

{ SETTINGS-ACK frames never count toward the flood budget. 
  With MaxSettingsFlood = 1, three SETTINGS-ACK frames are all consumed; none
  count, so no frame trips and the connection stays open. }

procedure TTestHTTP2Limits.TestSettingsAckDoesNotCountTowardFlood;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
begin
  L := DefaultLimits;
  L.MaxSettingsFlood := 1;
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(SettingsFrameBytes(nil, H2_FLAG_ACK),
    Concat(SettingsFrameBytes(nil, H2_FLAG_ACK),
           SettingsFrameBytes(nil, H2_FLAG_ACK)))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertTrue('settings-ACK 1 does not count', Conn.HandleNextFrame);
      AssertTrue('settings-ACK 2 does not count', Conn.HandleNextFrame);
      AssertTrue('settings-ACK 3 does not count', Conn.HandleNextFrame);
      AssertFalse('connection NOT Closing (ACKs never count)', Conn.Closing);
    finally
      Conn.Free;
    end;
  finally
    Dup.Free;
  end;
end;

{ §4.2 - the negotiated SETTINGS_MAX_FRAME_SIZE is enforced on EACH
  CONTINUATION frame, not just the first HEADERS. With MaxFrameSize = 8 the
  6-octet HEADERS fragment passes the first-frame §4.2 limit but the following 10-octet
  CONTINUATION exceeds MaxFrameSize. A CONTINUATION "could alter the state of the
  entire connection", so this is a connection FRAME_SIZE_ERROR -> GOAWAY }

procedure TTestHTTP2Limits.TestPerContinuationFrameSizeEmitsGoAway;
var
  Dup: TDuplexStream;
  Conn: TH2Connection;
  L: TH2Limits;
  H: TH2FrameHeader;
  LastStreamID, Code: Cardinal;
  Outp: TBytesStream;
begin
  L := DefaultLimits;
  L.MaxFrameSize := 8;   // 6-octet HEADERS frag fits; the 10-octet CONTINUATION does not
  Dup := TDuplexStream.Create(Concat(ValidClientInput,
    Concat(HeadersFrameBytesNoEnd(1, FilledBytes(6, $00)),
           ContinuationFrameBytes(1, FilledBytes(10, $00), H2_FLAG_END_HEADERS))));
  try
    Conn := TH2Connection.Create(Dup, L);
    try
      Conn.Start;
      AssertFalse('oversize CONTINUATION: HandleNextFrame False (connection error)',
        Conn.HandleNextFrame);
      AssertTrue('connection is Closing after per-CONTINUATION FRAME_SIZE_ERROR',
        Conn.Closing);

      Outp := OutputAfterBootstrap(Dup);
      try
        H := ReadFrameHeader(Outp);
        AssertEquals('emitted frame is GOAWAY', Ord(ftGoAway), H.FrameType);
        AssertEquals('GOAWAY on stream 0', 0, Integer(H.StreamID));
        ReadGoAwayFrame(Outp, H, LastStreamID, Code);
        AssertEquals('GOAWAY carries FRAME_SIZE_ERROR',
          Cardinal(Ord(ecFrameSizeError)), Code);
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

  RegisterTest(TTestHTTP2Limits);
end.
