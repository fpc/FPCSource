unit tcfphttp2frames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, streamex,
  fphttp2frames, fphttp2consts;

type

  TTestHTTP2Frames = class(TTestCase)
  private
    // Build a seekable TStream from a fixed byte array (positioned at 0).
    function StreamOf(const aBytes: array of Byte): TBytesStream;
    // Build a TBytes from a byte literal (avoids relying on a dynamic-array
    // constructor); used to express the expected payloads.
    function Bytes(const aBytes: array of Byte): TBytes;
    // Compare two byte vectors element-by-element.
    procedure AssertBytesEqual(const aExpected, aActual: TBytes; const aWhat: string);
    // Assert that reading the given malformed fixture raises EH2FrameError with
    // the expected ErrorCode.
    procedure AssertFrameError(const aBytes: array of Byte; aExpected: TH2ErrorCode;
      const aWhat: string);
  published
    // header codec
    procedure TestFrameHeaderRoundTrip;
    procedure TestFrameHeaderReservedBitMasked;
    procedure TestFrameHeaderMaxLenAccepted;
    procedure TestIsKnownFrameType;
    // per-frame round-trips / reads
    procedure TestDataFrame;
    procedure TestDataPaddedFrame;
    procedure TestDataFullPaddingAccepted;
    procedure TestHeadersFrame;
    procedure TestHeadersPaddedPriorityFrame;
    procedure TestSettingsFrame;
    procedure TestSettingsAck;
    procedure TestWindowUpdateFrame;
    procedure TestRSTStreamRawCode;
    procedure TestGoAwayFrame;
    procedure TestPingFrame;
    procedure TestContinuationReassembly;
    procedure TestPerContinuationFrameSizeCapRaises;   // Story 2.7 §4.2
    // malformed rejection
    procedure TestRejectShortWindowUpdate;
    procedure TestRejectBadRSTStreamLength;
    procedure TestRejectBadPingLength;
    procedure TestRejectOddSettings;
    procedure TestRejectShortGoAway;
    procedure TestRejectBadDataPadding;
    procedure TestRejectBadHeadersPadding;
    procedure TestRejectContinuationForeignStream;
    procedure TestRejectContinuationInterleaved;
  end;

implementation

{$i http2testdata.inc}

{ helpers }

function TTestHTTP2Frames.StreamOf(const aBytes: array of Byte): TBytesStream;
begin
  Result := TBytesStream.Create;
  if Length(aBytes) > 0 then
    Result.WriteBuffer(aBytes[0], Length(aBytes));
  Result.Position := 0;
end;

function TTestHTTP2Frames.Bytes(const aBytes: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(aBytes));
  for I := 0 to High(aBytes) do
    Result[I] := aBytes[I];
end;

procedure TTestHTTP2Frames.AssertBytesEqual(const aExpected, aActual: TBytes;
  const aWhat: string);
var
  I: Integer;
begin
  AssertEquals(aWhat + ' length', Length(aExpected), Length(aActual));
  for I := 0 to High(aExpected) do
    AssertEquals(aWhat + ' byte ' + IntToStr(I), aExpected[I], aActual[I]);
end;

procedure TTestHTTP2Frames.AssertFrameError(const aBytes: array of Byte;
  aExpected: TH2ErrorCode; const aWhat: string);
var
  S: TBytesStream;
  H: TH2FrameHeader;
  Raised: Boolean;
  Code: TH2ErrorCode;
  DummyA, DummyB: Cardinal;
begin
  S := StreamOf(aBytes);
  try
    Raised := False;
    Code := ecNoError;
    try
      H := ReadFrameHeader(S);
      // Dispatch on the raw type byte to the matching payload reader. For the
      // CONTINUATION-reassembly cases the HEADERS payload is read first, then
      // AssembleHeaderBlock drives the following frames (where it raises).
      case H.FrameType of
        Ord(ftData):         ReadDataFrame(S, H);
        Ord(ftHeaders):      AssembleHeaderBlock(S, H, ReadHeadersFrame(S, H));
        Ord(ftRSTStream):    ReadRSTStreamFrame(S, H);
        Ord(ftSettings):     ReadSettingsFrame(S, H);
        Ord(ftPing):         ReadPingFrame(S, H);
        Ord(ftGoAway):       ReadGoAwayFrame(S, H, DummyA, DummyB);
        Ord(ftWindowUpdate): ReadWindowUpdateFrame(S, H);
      end;
    except
      on E: EH2FrameError do
      begin
        Raised := True;
        Code := E.ErrorCode;
      end;
    end;
    AssertTrue(aWhat + ': expected EH2FrameError', Raised);
    AssertEquals(aWhat + ': error code', Ord(aExpected), Ord(Code));
  finally
    S.Free;
  end;
end;

{ header codec }

procedure TTestHTTP2Frames.TestFrameHeaderRoundTrip;
var
  S: TBytesStream;
  H, H2: TH2FrameHeader;
begin
  H.Len := 12345;
  H.FrameType := Ord(ftHeaders);
  H.Flags := H2_FLAG_END_HEADERS;
  H.StreamID := 7;
  S := TBytesStream.Create;
  try
    WriteFrameHeader(S, H);
    AssertEquals('header is 9 octets', 9, S.Size);
    S.Position := 0;
    H2 := ReadFrameHeader(S);
    AssertEquals('Len', Integer(H.Len), Integer(H2.Len));
    AssertEquals('FrameType', H.FrameType, H2.FrameType);
    AssertEquals('Flags', H.Flags, H2.Flags);
    AssertEquals('StreamID', Integer(H.StreamID), Integer(H2.StreamID));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestFrameHeaderReservedBitMasked;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  // Read side: fixture has R=1 in the StreamID dword; must decode to 1.
  S := StreamOf(H2F_RESERVED_BIT_HEADER);
  try
    H := ReadFrameHeader(S);
    AssertEquals('reserved bit masked on read', 1, Integer(H.StreamID));
  finally
    S.Free;
  end;
  // Write side: a StreamID with the high bit set must serialise with R=0.
  H.Len := 0;
  H.FrameType := Ord(ftData);
  H.Flags := 0;
  H.StreamID := Cardinal($80000001);
  S := TBytesStream.Create;
  try
    WriteFrameHeader(S, H);
    S.Position := 0;
    // First octet of the StreamID dword is at offset 5; top bit must be clear.
    AssertEquals('reserved bit written as 0', 0, S.Bytes[5] and $80);
    H := ReadFrameHeader(S);
    AssertEquals('StreamID re-read without reserved bit', 1, Integer(H.StreamID));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestFrameHeaderMaxLenAccepted;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  // Boundary: Len == H2_MAX_ALLOWED_FRAME_SIZE must be accepted (no raise).
  S := StreamOf(H2F_MAX_LEN_HEADER);
  try
    H := ReadFrameHeader(S);
    AssertEquals('max frame length accepted', H2_MAX_ALLOWED_FRAME_SIZE, Integer(H.Len));
    AssertEquals('StreamID', 1, Integer(H.StreamID));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestIsKnownFrameType;
begin
  AssertTrue('DATA known', IsKnownFrameType(Ord(ftData)));
  AssertTrue('CONTINUATION known', IsKnownFrameType(Ord(ftContinuation)));
  AssertFalse('type 10 unknown', IsKnownFrameType(10));
  AssertFalse('type 255 unknown', IsKnownFrameType(255));
end;

{ per-frame }

procedure TTestHTTP2Frames.TestDataFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  // Read a known-good fixture.
  S := StreamOf(H2F_DATA);
  try
    H := ReadFrameHeader(S);
    AssertEquals('type DATA', Ord(ftData), H.FrameType);
    AssertEquals('streamid', 1, Integer(H.StreamID));
    AssertBytesEqual(Bytes([$48, $69]), ReadDataFrame(S, H), 'DATA payload');
  finally
    S.Free;
  end;
  // Round-trip write -> read.
  S := TBytesStream.Create;
  try
    WriteDataFrame(S, 1, Bytes([$48, $69]), H2_FLAG_END_STREAM);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertEquals('rt flags', H2_FLAG_END_STREAM, H.Flags);
    AssertBytesEqual(Bytes([$48, $69]), ReadDataFrame(S, H), 'DATA round-trip');
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestDataPaddedFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  S := StreamOf(H2F_DATA_PADDED);
  try
    H := ReadFrameHeader(S);
    AssertTrue('PADDED set', (H.Flags and H2_FLAG_PADDED) <> 0);
    AssertBytesEqual(Bytes([$48, $69]), ReadDataFrame(S, H), 'padded DATA payload');
  finally
    S.Free;
  end;
  // Round-trip with padding: padding is stripped, original data recovered.
  S := TBytesStream.Create;
  try
    WriteDataFrame(S, 1, Bytes([$48, $69]), 0, 3);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertTrue('PADDED set on write', (H.Flags and H2_FLAG_PADDED) <> 0);
    AssertEquals('payload length incl padding', 6, Integer(H.Len));
    AssertBytesEqual(Bytes([$48, $69]), ReadDataFrame(S, H), 'padded DATA round-trip');
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestDataFullPaddingAccepted;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  // Boundary (RFC 9113 §6.1): Pad Length == bytes remaining after the pad-length
  // octet means zero application data - this is VALID and must NOT raise. Locks
  // the codec's `PadLen > Remaining` reject rule (only STRICTLY greater is bad).
  S := StreamOf(H2F_DATA_FULL_PAD);
  try
    H := ReadFrameHeader(S);
    AssertTrue('PADDED set', (H.Flags and H2_FLAG_PADDED) <> 0);
    AssertEquals('full-padding payload empty', 0, Length(ReadDataFrame(S, H)));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestHeadersFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  S := StreamOf(H2F_HEADERS);
  try
    H := ReadFrameHeader(S);
    AssertEquals('type HEADERS', Ord(ftHeaders), H.FrameType);
    AssertTrue('END_HEADERS set', (H.Flags and H2_FLAG_END_HEADERS) <> 0);
    AssertBytesEqual(Bytes([$82, $84]), ReadHeadersFrame(S, H), 'HEADERS fragment');
  finally
    S.Free;
  end;
  S := TBytesStream.Create;
  try
    WriteHeadersFrame(S, 1, Bytes([$82, $84]), H2_FLAG_END_HEADERS);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertBytesEqual(Bytes([$82, $84]), ReadHeadersFrame(S, H), 'HEADERS round-trip');
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestHeadersPaddedPriorityFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  // PADDED + PRIORITY: priority block and padding are skipped; fragment only.
  S := StreamOf(H2F_HEADERS_PAD_PRIO);
  try
    H := ReadFrameHeader(S);
    AssertTrue('PADDED set', (H.Flags and H2_FLAG_PADDED) <> 0);
    AssertTrue('PRIORITY set', (H.Flags and H2_FLAG_PRIORITY) <> 0);
    AssertBytesEqual(Bytes([$82, $84]), ReadHeadersFrame(S, H),
      'HEADERS pad+priority fragment');
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestSettingsFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
  Settings: TH2SettingsArray;
begin
  S := StreamOf(H2F_SETTINGS);
  try
    H := ReadFrameHeader(S);
    Settings := ReadSettingsFrame(S, H);
    AssertEquals('pair count', 2, Length(Settings));
    AssertEquals('id[0]', H2_SETTINGS_MAX_CONCURRENT_STREAMS, Settings[0].Identifier);
    AssertEquals('val[0]', 100, Integer(Settings[0].Value));
    AssertEquals('id[1]', H2_SETTINGS_INITIAL_WINDOW_SIZE, Settings[1].Identifier);
    AssertEquals('val[1]', 65535, Integer(Settings[1].Value));
  finally
    S.Free;
  end;
  // Round-trip.
  SetLength(Settings, 2);
  Settings[0].Identifier := H2_SETTINGS_MAX_CONCURRENT_STREAMS;
  Settings[0].Value := 100;
  Settings[1].Identifier := H2_SETTINGS_INITIAL_WINDOW_SIZE;
  Settings[1].Value := 65535;
  S := TBytesStream.Create;
  try
    WriteSettingsFrame(S, Settings);
    S.Position := 0;
    H := ReadFrameHeader(S);
    Settings := ReadSettingsFrame(S, H);
    AssertEquals('rt pair count', 2, Length(Settings));
    AssertEquals('rt val[1]', 65535, Integer(Settings[1].Value));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestSettingsAck;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  S := StreamOf(H2F_SETTINGS_ACK);
  try
    H := ReadFrameHeader(S);
    AssertTrue('ACK set', (H.Flags and H2_FLAG_ACK) <> 0);
    AssertEquals('ACK empty payload', 0, Length(ReadSettingsFrame(S, H)));
  finally
    S.Free;
  end;
  // Round-trip an ACK: length 0, no pairs.
  S := TBytesStream.Create;
  try
    WriteSettingsFrame(S, nil, H2_FLAG_ACK);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertEquals('rt ACK length', 0, Integer(H.Len));
    AssertEquals('rt ACK pairs', 0, Length(ReadSettingsFrame(S, H)));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestWindowUpdateFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  S := StreamOf(H2F_WINDOW_UPDATE);
  try
    H := ReadFrameHeader(S);
    AssertEquals('increment', 65535, Integer(ReadWindowUpdateFrame(S, H)));
  finally
    S.Free;
  end;
  S := TBytesStream.Create;
  try
    WriteWindowUpdateFrame(S, 1, 65535);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertEquals('rt increment', 65535, Integer(ReadWindowUpdateFrame(S, H)));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestRSTStreamRawCode;
var
  S: TBytesStream;
  H: TH2FrameHeader;
begin
  // 255 is not a registered TH2ErrorCode; it must survive as a raw Cardinal.
  S := StreamOf(H2F_RST_STREAM);
  try
    H := ReadFrameHeader(S);
    AssertEquals('raw error code', 255, Integer(ReadRSTStreamFrame(S, H)));
  finally
    S.Free;
  end;
  S := TBytesStream.Create;
  try
    WriteRSTStreamFrame(S, 1, 255);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertEquals('rt raw error code', 255, Integer(ReadRSTStreamFrame(S, H)));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestGoAwayFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
  Last, Code: Cardinal;
begin
  S := StreamOf(H2F_GOAWAY);
  try
    H := ReadFrameHeader(S);
    AssertBytesEqual(Bytes([$78]), ReadGoAwayFrame(S, H, Last, Code), 'GOAWAY debug data');
    AssertEquals('last stream id', 1, Integer(Last));
    AssertEquals('error code', 11, Integer(Code));
  finally
    S.Free;
  end;
  S := TBytesStream.Create;
  try
    WriteGoAwayFrame(S, 1, 11, Bytes([$78]));
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertBytesEqual(Bytes([$78]), ReadGoAwayFrame(S, H, Last, Code), 'GOAWAY round-trip debug');
    AssertEquals('rt last stream id', 1, Integer(Last));
    AssertEquals('rt error code', 11, Integer(Code));
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestPingFrame;
var
  S: TBytesStream;
  H: TH2FrameHeader;
  Opaque: TBytes;
begin
  Opaque := Bytes([$01, $02, $03, $04, $05, $06, $07, $08]);
  S := StreamOf(H2F_PING);
  try
    H := ReadFrameHeader(S);
    AssertBytesEqual(Opaque, ReadPingFrame(S, H), 'PING payload');
  finally
    S.Free;
  end;
  S := TBytesStream.Create;
  try
    WritePingFrame(S, Opaque);
    S.Position := 0;
    H := ReadFrameHeader(S);
    AssertBytesEqual(Opaque, ReadPingFrame(S, H), 'PING round-trip');
  finally
    S.Free;
  end;
end;

procedure TTestHTTP2Frames.TestContinuationReassembly;
var
  S: TBytesStream;
  H: TH2FrameHeader;
  Block: TBytes;
begin
  // Fixture: HEADERS (no END_HEADERS) + CONTINUATION (END_HEADERS), same stream.
  S := StreamOf(H2F_HEADERS_CONT);
  try
    H := ReadFrameHeader(S);
    Block := AssembleHeaderBlock(S, H, ReadHeadersFrame(S, H));
    AssertBytesEqual(Bytes([$AA, $BB, $CC, $DD]), Block, 'assembled header block');
  finally
    S.Free;
  end;
  // Round-trip: build the sequence via the writers, then reassemble.
  S := TBytesStream.Create;
  try
    WriteHeadersFrame(S, 3, Bytes([$AA, $BB]), 0);                // no END_HEADERS
    WriteContinuationFrame(S, 3, Bytes([$CC]), 0);                // no END_HEADERS
    WriteContinuationFrame(S, 3, Bytes([$DD]), H2_FLAG_END_HEADERS);
    S.Position := 0;
    H := ReadFrameHeader(S);
    Block := AssembleHeaderBlock(S, H, ReadHeadersFrame(S, H));
    AssertBytesEqual(Bytes([$AA, $BB, $CC, $DD]), Block, 'reassembly round-trip');
  finally
    S.Free;
  end;
end;

{ Story 2.7 §4.2 - the pure codec enforces aMaxFrameSize per CONTINUATION frame.
  A 2-octet CONTINUATION against aMaxFrameSize = 1 exceeds the per-frame cap and
  raises EH2FrameError(ecFrameSizeError). The default-0 call (no cap) stays
  cap-free: the same byte vector assembles cleanly, proving every existing caller
  and pure-codec test is unaffected. }

procedure TTestHTTP2Frames.TestPerContinuationFrameSizeCapRaises;
var
  S: TBytesStream;
  H: TH2FrameHeader;
  Block: TBytes;
  Raised: Boolean;
  Code: TH2ErrorCode;
begin
  // A 2-octet CONTINUATION; with aMaxFrameSize = 1 it exceeds the per-frame cap.
  S := TBytesStream.Create;
  try
    WriteHeadersFrame(S, 3, Bytes([$AA, $BB]), 0);                   // no END_HEADERS
    WriteContinuationFrame(S, 3, Bytes([$CC, $DD]), H2_FLAG_END_HEADERS);  // 2 octets
    S.Position := 0;
    Raised := False;
    Code := ecNoError;
    try
      H := ReadFrameHeader(S);
      AssembleHeaderBlock(S, H, ReadHeadersFrame(S, H), 0, 1);   // aMaxFrameSize = 1
    except
      on E: EH2FrameError do
      begin
        Raised := True;
        Code := E.ErrorCode;
      end;
    end;
    AssertTrue('oversize CONTINUATION raises EH2FrameError', Raised);
    AssertEquals('per-CONTINUATION cap is FRAME_SIZE_ERROR',
      Ord(ecFrameSizeError), Ord(Code));
  finally
    S.Free;
  end;
  // Regression: the default (aMaxFrameSize = 0) call is cap-free - assembles fine.
  S := TBytesStream.Create;
  try
    WriteHeadersFrame(S, 3, Bytes([$AA, $BB]), 0);
    WriteContinuationFrame(S, 3, Bytes([$CC, $DD]), H2_FLAG_END_HEADERS);
    S.Position := 0;
    H := ReadFrameHeader(S);
    Block := AssembleHeaderBlock(S, H, ReadHeadersFrame(S, H));   // default 0 = unlimited
    AssertBytesEqual(Bytes([$AA, $BB, $CC, $DD]), Block, 'default-0 stays cap-free');
  finally
    S.Free;
  end;
end;

{ malformed rejection }

procedure TTestHTTP2Frames.TestRejectShortWindowUpdate;
begin
  AssertFrameError(H2F_BAD_WINDOW_UPDATE_SHORT, ecFrameSizeError, 'short WINDOW_UPDATE');
end;

procedure TTestHTTP2Frames.TestRejectBadRSTStreamLength;
begin
  AssertFrameError(H2F_BAD_RST_STREAM_LEN, ecFrameSizeError, 'bad RST_STREAM length');
end;

procedure TTestHTTP2Frames.TestRejectBadPingLength;
begin
  AssertFrameError(H2F_BAD_PING_LEN, ecFrameSizeError, 'bad PING length');
end;

procedure TTestHTTP2Frames.TestRejectOddSettings;
begin
  AssertFrameError(H2F_BAD_SETTINGS_ODD, ecFrameSizeError, 'odd SETTINGS length');
end;

procedure TTestHTTP2Frames.TestRejectShortGoAway;
begin
  AssertFrameError(H2F_BAD_GOAWAY_SHORT, ecFrameSizeError, 'short GOAWAY');
end;

procedure TTestHTTP2Frames.TestRejectBadDataPadding;
begin
  AssertFrameError(H2F_BAD_DATA_PADDING, ecProtocolError, 'bad DATA padding');
end;

procedure TTestHTTP2Frames.TestRejectBadHeadersPadding;
begin
  AssertFrameError(H2F_BAD_HEADERS_PADDING, ecProtocolError, 'bad HEADERS padding');
end;

procedure TTestHTTP2Frames.TestRejectContinuationForeignStream;
begin
  AssertFrameError(H2F_CONT_FOREIGN_STREAM, ecProtocolError, 'CONTINUATION foreign stream');
end;

procedure TTestHTTP2Frames.TestRejectContinuationInterleaved;
begin
  AssertFrameError(H2F_CONT_INTERLEAVED, ecProtocolError, 'CONTINUATION interleaved');
end;

initialization

  RegisterTest(TTestHTTP2Frames);
end.
