{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2026 - by the Free Pascal development team

    HTTP/2 frame header and core-frame codec

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fphttp2frames;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Fcl.Streams.Extra, FpWeb.Http2.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, streamex, fphttp2consts;
{$ENDIF FPC_DOTTEDUNITS}

type
  // RFC 9113 §4.1 frame header (9 octets on the wire):
  TH2FrameHeader = record
    Len: Cardinal;        // 24-bit payload length
    FrameType: Byte;      // raw wire type byte
    Flags: Byte;
    StreamID: Cardinal;   // 31-bit stream id (reserved bit excluded)
  end;

  // RFC 9113 §6.5 SETTINGS parameter: Identifier(16) Value(32).
  TH2SettingsPair = record
    Identifier: Word;
    Value: Cardinal;
  end;
  TH2SettingsArray = array of TH2SettingsPair;

  // Structural framing error raised by this codec.
  EH2FrameError = class(Exception)
  public
    ErrorCode: TH2ErrorCode;
    constructor Create(aCode: TH2ErrorCode; const aMsg: string);
  end;

// Frame header codec (RFC 9113 §4.1 / §4.2).
function ReadFrameHeader(aStream: TStream): TH2FrameHeader;
procedure WriteFrameHeader(aStream: TStream; const aHeader: TH2FrameHeader);

// True when b is a registered RFC 9113 §11.2 frame type (0..9).
function IsKnownFrameType(b: Byte): Boolean;

// Core per-frame (de)serializers. All wire I/O uses the streamex BE helpers.

// DATA §6.1 - honors PADDED; returns/accepts application data only.
function ReadDataFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
procedure WriteDataFrame(aStream: TStream; aStreamID: Cardinal; const aData: TBytes;
  aFlags: Byte = 0; aPadLength: Byte = 0);

// HEADERS §6.2 - honors PADDED and PRIORITY; the priority block is parsed but ignored. 
function ReadHeadersFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
procedure WriteHeadersFrame(aStream: TStream; aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0; aPadLength: Byte = 0);

// CONTINUATION §6.10 - raw fragment read + complete-frame write.
function ReadContinuationFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
procedure WriteContinuationFrame(aStream: TStream; aStreamID: Cardinal; const aFragment: TBytes; aFlags: Byte = 0);

// SETTINGS §6.5 - (de)serializes Identifier/Value pairs; ACK =>empty payload.
function ReadSettingsFrame(aStream: TStream; const aHeader: TH2FrameHeader): TH2SettingsArray;
procedure WriteSettingsFrame(aStream: TStream; const aSettings: TH2SettingsArray; aFlags: Byte = 0);

// WINDOW_UPDATE §6.9 - R(1)+Window Size Increment(31); reserved bit masked.
function ReadWindowUpdateFrame(aStream: TStream; const aHeader: TH2FrameHeader): Cardinal;
procedure WriteWindowUpdateFrame(aStream: TStream; aStreamID: Cardinal; aIncrement: Cardinal);

// RST_STREAM §6.4 - Error Code(32) as a RAW Cardinal (never cast to the dense enum).
function ReadRSTStreamFrame(aStream: TStream; const aHeader: TH2FrameHeader): Cardinal;
procedure WriteRSTStreamFrame(aStream: TStream; aStreamID: Cardinal; aErrorCode: Cardinal);

// GOAWAY §6.8 - R(1)+Last-Stream-ID(31), raw Error Code(32), optional debug data.
function ReadGoAwayFrame(aStream: TStream; const aHeader: TH2FrameHeader;
  out aLastStreamID: Cardinal; out aErrorCode: Cardinal): TBytes;
procedure WriteGoAwayFrame(aStream: TStream; aLastStreamID: Cardinal; aErrorCode: Cardinal;
  const aDebugData: TBytes);

// PING §6.7 - exactly 8 opaque octets.
function ReadPingFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
procedure WritePingFrame(aStream: TStream; const aOpaque: TBytes; aFlags: Byte = 0);

// CONTINUATION reassembly §6.10.
function AssembleHeaderBlock(aStream: TStream; const aHeader: TH2FrameHeader;
                             const aFirstFragment: TBytes; aMaxBlockBytes: Cardinal = 0;
                             aMaxFrameSize: Cardinal = 0): TBytes;

implementation

{ EH2FrameError }

constructor EH2FrameError.Create(aCode: TH2ErrorCode; const aMsg: string);
begin
  inherited Create(aMsg);
  ErrorCode := aCode;
end;

{ Low-level byte helpers. The 24-bit length and the 31-bit stream id/increment
  are the ONLY hand-rolled byte fields (streamex has no helper for them) }

// 24-bit big-endian read. 
function ReadUInt24BE(aStream: TStream): Cardinal;
var
  B0, B1, B2: Byte;
begin
  B0 := aStream.ReadByte;
  B1 := aStream.ReadByte;
  B2 := aStream.ReadByte;
  Result := (Cardinal(B0) shl 16) or (Cardinal(B1) shl 8) or Cardinal(B2);
end;

procedure WriteUInt24BE(aStream: TStream; aValue: Cardinal);
begin
  aStream.WriteByte(Byte((aValue shr 16) and $FF));
  aStream.WriteByte(Byte((aValue shr 8) and $FF));
  aStream.WriteByte(Byte(aValue and $FF));
end;

function ReadRawBytes(aStream: TStream; aCount: Integer): TBytes;
begin
  SetLength(Result, aCount);
  if aCount > 0 then
    aStream.ReadBuffer(Result[0], aCount);
end;

procedure WriteRawBytes(aStream: TStream; const aBytes: TBytes);
begin
  if Length(aBytes) > 0 then
    aStream.WriteBuffer(aBytes[0], Length(aBytes));
end;

// Return aHead followed by aTail (header-block fragment concatenation).
function AppendBytes(const aHead, aTail: TBytes): TBytes;
var
  N: Integer;
begin
  N := Length(aHead);
  SetLength(Result, N + Length(aTail));
  if N > 0 then
    Move(aHead[0], Result[0], N);
  if Length(aTail) > 0 then
    Move(aTail[0], Result[N], Length(aTail));
end;

// Consume and discard aCount octets (frame padding / skipped priority).
procedure SkipBytes(aStream: TStream; aCount: Integer);
var
  Buf: array[0..255] of Byte;
  N: Integer;
begin
  while aCount > 0 do
  begin
    N := aCount;
    if N > SizeOf(Buf) then
      N := SizeOf(Buf);
    aStream.ReadBuffer(Buf, N);
    Dec(aCount, N);
  end;
end;

// Emit aCount zero padding octets (pad length is a Byte, so <= 255).
procedure WritePadding(aStream: TStream; aCount: Integer);
var
  Buf: array[0..255] of Byte;
begin
  if aCount <= 0 then
    Exit;
  FillChar(Buf, aCount, 0);
  aStream.WriteBuffer(Buf, aCount);
end;

{ Frame header codec }

function ReadFrameHeader(aStream: TStream): TH2FrameHeader;
begin
  Result.Len := ReadUInt24BE(aStream);
  // §4.2 absolute ceiling. H2_MAX_ALLOWED_FRAME_SIZE is $FFFFFF.
  if Result.Len > H2_MAX_ALLOWED_FRAME_SIZE then
    raise EH2FrameError.Create(ecFrameSizeError,
      Format('frame length %d exceeds maximum %d', [Result.Len, H2_MAX_ALLOWED_FRAME_SIZE]));
  Result.FrameType := aStream.ReadByte;
  Result.Flags := aStream.ReadByte;
  // R(1) reserved bit masked off; only the 31-bit stream id is retained.
  Result.StreamID := aStream.ReadDWordBE and $7FFFFFFF;
end;

procedure WriteFrameHeader(aStream: TStream; const aHeader: TH2FrameHeader);
begin
  WriteUInt24BE(aStream, aHeader.Len);
  aStream.WriteByte(aHeader.FrameType);
  aStream.WriteByte(aHeader.Flags);
  // Reserved bit always written as 0.
  aStream.WriteDWordBE(aHeader.StreamID and $7FFFFFFF);
end;

function IsKnownFrameType(b: Byte): Boolean;
begin
  Result := b <= Ord(High(TH2FrameType));
end;

{ DATA §6.1 }

function ReadDataFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
var
  PadLen, Remaining: Integer;
begin
  if (aHeader.Flags and H2_FLAG_PADDED) <> 0 then
  begin
    PadLen := aStream.ReadByte;
    Remaining := Integer(aHeader.Len) - 1;   // bytes left after the pad-length octet
    // Pad length must leave room for >= 0 data octets (§6.1).
    if PadLen > Remaining then
      raise EH2FrameError.Create(ecProtocolError, 'DATA pad length exceeds payload');
    Result := ReadRawBytes(aStream, Remaining - PadLen);
    SkipBytes(aStream, PadLen);
  end
  else
    Result := ReadRawBytes(aStream, Integer(aHeader.Len));
end;

procedure WriteDataFrame(aStream: TStream; aStreamID: Cardinal; const aData: TBytes;
  aFlags: Byte = 0; aPadLength: Byte = 0);
var
  H: TH2FrameHeader;
begin
  if aPadLength > 0 then
    aFlags := aFlags or H2_FLAG_PADDED;
  H.FrameType := Ord(ftData);
  H.Flags := aFlags;
  H.StreamID := aStreamID;
  if (aFlags and H2_FLAG_PADDED) <> 0 then
    H.Len := Cardinal(1 + Length(aData) + aPadLength)
  else
    H.Len := Cardinal(Length(aData));
  WriteFrameHeader(aStream, H);
  if (aFlags and H2_FLAG_PADDED) <> 0 then
  begin
    aStream.WriteByte(aPadLength);
    WriteRawBytes(aStream, aData);
    WritePadding(aStream, aPadLength);
  end
  else
    WriteRawBytes(aStream, aData);
end;

{ HEADERS §6.2 }

function ReadHeadersFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
var
  PadLen, Remaining: Integer;
begin
  Remaining := Integer(aHeader.Len);
  PadLen := 0;
  if (aHeader.Flags and H2_FLAG_PADDED) <> 0 then
  begin
    PadLen := aStream.ReadByte;
    Dec(Remaining);
  end;
  if (aHeader.Flags and H2_FLAG_PRIORITY) <> 0 then
  begin
    // E(1)+StreamDep(31)+Weight(8) = 5 octets, parsed-and-ignored.
    if Remaining < 5 then
      raise EH2FrameError.Create(ecProtocolError, 'HEADERS too short for PRIORITY block');
    SkipBytes(aStream, 5);
    Dec(Remaining, 5);
  end;
  if PadLen > Remaining then
    raise EH2FrameError.Create(ecProtocolError, 'HEADERS pad length exceeds payload');
  Result := ReadRawBytes(aStream, Remaining - PadLen);
  SkipBytes(aStream, PadLen);
end;

procedure WriteHeadersFrame(aStream: TStream; aStreamID: Cardinal; const aFragment: TBytes;
  aFlags: Byte = 0; aPadLength: Byte = 0);
var
  H: TH2FrameHeader;
begin
  // PRIORITY is not supported; this writer never emits a priority block.
  if aPadLength > 0 then
    aFlags := aFlags or H2_FLAG_PADDED;
  H.FrameType := Ord(ftHeaders);
  H.Flags := aFlags;
  H.StreamID := aStreamID;
  if (aFlags and H2_FLAG_PADDED) <> 0 then
    H.Len := Cardinal(1 + Length(aFragment) + aPadLength)
  else
    H.Len := Cardinal(Length(aFragment));
  WriteFrameHeader(aStream, H);
  if (aFlags and H2_FLAG_PADDED) <> 0 then
  begin
    aStream.WriteByte(aPadLength);
    WriteRawBytes(aStream, aFragment);
    WritePadding(aStream, aPadLength);
  end
  else
    WriteRawBytes(aStream, aFragment);
end;

{ CONTINUATION §6.10 }

function ReadContinuationFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
begin
  // Payload is the whole header-block fragment, unpadded.
  Result := ReadRawBytes(aStream, Integer(aHeader.Len));
end;

procedure WriteContinuationFrame(aStream: TStream; aStreamID: Cardinal; const aFragment: TBytes;
  aFlags: Byte = 0);
var
  H: TH2FrameHeader;
begin
  H.FrameType := Ord(ftContinuation);
  H.Flags := aFlags;
  H.StreamID := aStreamID;
  H.Len := Cardinal(Length(aFragment));
  WriteFrameHeader(aStream, H);
  WriteRawBytes(aStream, aFragment);
end;

{ SETTINGS §6.5 }

function ReadSettingsFrame(aStream: TStream; const aHeader: TH2FrameHeader): TH2SettingsArray;
var
  Count, I: Integer;
begin
  if (aHeader.Flags and H2_FLAG_ACK) <> 0 then
  begin
    // ACK carries an empty payload (§6.5).
    if aHeader.Len <> 0 then
      raise EH2FrameError.Create(ecFrameSizeError, 'SETTINGS ACK must have empty payload');
    Exit(nil);
  end;
  if (aHeader.Len mod 6) <> 0 then
    raise EH2FrameError.Create(ecFrameSizeError, 'SETTINGS length not a multiple of 6');
  Count := Integer(aHeader.Len) div 6;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    Result[I].Identifier := aStream.ReadWordBE;
    Result[I].Value := aStream.ReadDWordBE;
  end;
end;

procedure WriteSettingsFrame(aStream: TStream; const aSettings: TH2SettingsArray; aFlags: Byte = 0);
var
  H: TH2FrameHeader;
  I: Integer;
begin
  H.FrameType := Ord(ftSettings);
  H.Flags := aFlags;
  H.StreamID := 0;
  if (aFlags and H2_FLAG_ACK) <> 0 then
    H.Len := 0
  else
    H.Len := Cardinal(Length(aSettings) * 6);
  WriteFrameHeader(aStream, H);
  if (aFlags and H2_FLAG_ACK) = 0 then
    for I := 0 to High(aSettings) do
    begin
      aStream.WriteWordBE(aSettings[I].Identifier);
      aStream.WriteDWordBE(aSettings[I].Value);
    end;
end;

{ WINDOW_UPDATE §6.9 }

function ReadWindowUpdateFrame(aStream: TStream; const aHeader: TH2FrameHeader): Cardinal;
begin
  if aHeader.Len <> 4 then
    raise EH2FrameError.Create(ecFrameSizeError, 'WINDOW_UPDATE length must be 4');
  Result := aStream.ReadDWordBE and $7FFFFFFF;
end;

procedure WriteWindowUpdateFrame(aStream: TStream; aStreamID: Cardinal; aIncrement: Cardinal);
var
  H: TH2FrameHeader;
begin
  H.FrameType := Ord(ftWindowUpdate);
  H.Flags := 0;
  H.StreamID := aStreamID;
  H.Len := 4;
  WriteFrameHeader(aStream, H);
  aStream.WriteDWordBE(aIncrement and $7FFFFFFF);
end;

{ RST_STREAM §6.4 }

function ReadRSTStreamFrame(aStream: TStream; const aHeader: TH2FrameHeader): Cardinal;
begin
  if aHeader.Len <> 4 then
    raise EH2FrameError.Create(ecFrameSizeError, 'RST_STREAM length must be 4');
  { Raw 32-bit error code: a peer may send an unregistered code (§7), so do not cast into the dense TH2ErrorCode enum here. }
  Result := aStream.ReadDWordBE;
end;

procedure WriteRSTStreamFrame(aStream: TStream; aStreamID: Cardinal; aErrorCode: Cardinal);
var
  H: TH2FrameHeader;
begin
  H.FrameType := Ord(ftRSTStream);
  H.Flags := 0;
  H.StreamID := aStreamID;
  H.Len := 4;
  WriteFrameHeader(aStream, H);
  aStream.WriteDWordBE(aErrorCode);
end;

{ GOAWAY §6.8 }

function ReadGoAwayFrame(aStream: TStream; const aHeader: TH2FrameHeader;
  out aLastStreamID: Cardinal; out aErrorCode: Cardinal): TBytes;
begin
  if aHeader.Len < 8 then
    raise EH2FrameError.Create(ecFrameSizeError, 'GOAWAY length must be at least 8');
  aLastStreamID := aStream.ReadDWordBE and $7FFFFFFF;
  aErrorCode := aStream.ReadDWordBE;   // raw 32-bit code (see RST_STREAM note)
  Result := ReadRawBytes(aStream, Integer(aHeader.Len) - 8);
end;

procedure WriteGoAwayFrame(aStream: TStream; aLastStreamID: Cardinal; aErrorCode: Cardinal;
  const aDebugData: TBytes);
var
  H: TH2FrameHeader;
begin
  H.FrameType := Ord(ftGoAway);
  H.Flags := 0;
  H.StreamID := 0;
  H.Len := Cardinal(8 + Length(aDebugData));
  WriteFrameHeader(aStream, H);
  aStream.WriteDWordBE(aLastStreamID and $7FFFFFFF);
  aStream.WriteDWordBE(aErrorCode);
  WriteRawBytes(aStream, aDebugData);
end;

{ PING §6.7 }

function ReadPingFrame(aStream: TStream; const aHeader: TH2FrameHeader): TBytes;
begin
  if aHeader.Len <> 8 then
    raise EH2FrameError.Create(ecFrameSizeError, 'PING length must be 8');
  Result := ReadRawBytes(aStream, 8);
end;

procedure WritePingFrame(aStream: TStream; const aOpaque: TBytes; aFlags: Byte = 0);
var
  H: TH2FrameHeader;
  Buf: TBytes;
begin
  H.FrameType := Ord(ftPing);
  H.Flags := aFlags;
  H.StreamID := 0;
  H.Len := 8;
  WriteFrameHeader(aStream, H);
  // Normalise to exactly 8 octets (zero-pad / truncate a non-conforming caller).
  Buf := Copy(aOpaque);
  SetLength(Buf, 8);
  WriteRawBytes(aStream, Buf);
end;

{ CONTINUATION reassembly §6.10 }

function AssembleHeaderBlock(aStream: TStream; const aHeader: TH2FrameHeader;
  const aFirstFragment: TBytes; aMaxBlockBytes: Cardinal = 0;
  aMaxFrameSize: Cardinal = 0): TBytes;
var
  Cont: TH2FrameHeader;
  Total: Int64;   // Int64 so accumulating large fragments cannot wrap (Cardinal sum)
begin
  Result := Copy(aFirstFragment);
  // (§6.10 / CVE-2024-27316). Check the first fragment - an already-oversize block is rejected even before any CONTINUATION.
  Total := Length(aFirstFragment);
  if (aMaxBlockBytes > 0) and (Total > aMaxBlockBytes) then
    raise EH2FrameError.Create(ecCompressionError,
      'header block exceeds MaxContinuationBytes');
  // HEADERS already complete: nothing to assemble.
  if (aHeader.Flags and H2_FLAG_END_HEADERS) <> 0 then
    Exit;
  repeat
    Cont := ReadFrameHeader(aStream);
    if Cont.FrameType <> Ord(ftContinuation) then
      raise EH2FrameError.Create(ecProtocolError,
        'expected CONTINUATION frame in header block');
    if Cont.StreamID <> aHeader.StreamID then
      raise EH2FrameError.Create(ecProtocolError,
        'CONTINUATION stream id does not match HEADERS');
    // §4.2 - each CONTINUATION must fit the negotiated SETTINGS_MAX_FRAME_SIZE.
    if (aMaxFrameSize > 0) and (Cont.Len > aMaxFrameSize) then
      raise EH2FrameError.Create(ecFrameSizeError,
        'CONTINUATION frame length exceeds MaxFrameSize (§4.2)');
    Inc(Total, Cont.Len);
    if (aMaxBlockBytes > 0) and (Total > aMaxBlockBytes) then
      raise EH2FrameError.Create(ecCompressionError,
        'header block exceeds MaxContinuationBytes');
    Result := AppendBytes(Result, ReadContinuationFrame(aStream, Cont));
  until (Cont.Flags and H2_FLAG_END_HEADERS) <> 0;
end;

end.
