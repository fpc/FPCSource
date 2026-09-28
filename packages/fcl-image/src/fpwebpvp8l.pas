{
    The WebP lossless (VP8L) bitstream: a decoder of every feature of the format and an encoder
    using subtract-green and predictor transforms or a colour index, with LZ77 back references.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwebpvp8l;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, System.Math, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, Math, FpImage;
{$ENDIF FPC_DOTTEDUNITS}

type
  EWebPError = class(FPImageException);

  { Pixels as $AARRGGBB, row by row. }
  TWebPPixels = array of LongWord;

  { The header fields of a VP8L bitstream. }
  TVP8LInfo = record
    Width, Height: Integer;
    AlphaUsed: Boolean;
  end;

// Reads the header of the VP8L bitstream in aData; False when it is not one.
function VP8LReadInfo(aData: PByte; aSize: Integer; out aInfo: TVP8LInfo): Boolean;
// Decodes the VP8L bitstream in aData; raises EWebPError when it is invalid.
function VP8LDecode(aData: PByte; aSize: Integer; out aInfo: TVP8LInfo): TWebPPixels;
// Encodes aPixels of aWidth x aHeight as a VP8L bitstream.
function VP8LEncode(const aPixels: TWebPPixels; aWidth, aHeight: Integer): TBytes;
// Returns the colour of pixel aPixel.
function WebPPixelToColor(aPixel: LongWord): TFPColor;
// Returns aColor as a pixel with 8 bits per channel.
function WebPColorToPixel(const aColor: TFPColor): LongWord;

implementation

const
  VP8LSignature = $2F;
  VP8LMaxDimension = 16384;
  CodeLengthCodes = 19;
  MaxCodeLength = 15;
  NumLiteralCodes = 256;
  NumLengthCodes = 24;
  NumDistanceCodes = 40;
  MaxCacheBits = 11;
  MaxLength = 4096;
  MaxPlaneCode = 120;
  TableBits = 8;

  CodeLengthOrder: array[0..CodeLengthCodes - 1] of Byte =
    (17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

  // Offsets (x, y) of the first 120 distance codes.
  DistanceMap: array[0..MaxPlaneCode - 1, 0..1] of ShortInt = (
    (0, 1), (1, 0), (1, 1), (-1, 1), (0, 2), (2, 0), (1, 2),
    (-1, 2), (2, 1), (-2, 1), (2, 2), (-2, 2), (0, 3), (3, 0),
    (1, 3), (-1, 3), (3, 1), (-3, 1), (2, 3), (-2, 3), (3, 2),
    (-3, 2), (0, 4), (4, 0), (1, 4), (-1, 4), (4, 1), (-4, 1),
    (3, 3), (-3, 3), (2, 4), (-2, 4), (4, 2), (-4, 2), (0, 5),
    (3, 4), (-3, 4), (4, 3), (-4, 3), (5, 0), (1, 5), (-1, 5),
    (5, 1), (-5, 1), (2, 5), (-2, 5), (5, 2), (-5, 2), (4, 4),
    (-4, 4), (3, 5), (-3, 5), (5, 3), (-5, 3), (0, 6), (6, 0),
    (1, 6), (-1, 6), (6, 1), (-6, 1), (2, 6), (-2, 6), (6, 2),
    (-6, 2), (4, 5), (-4, 5), (5, 4), (-5, 4), (3, 6), (-3, 6),
    (6, 3), (-6, 3), (0, 7), (7, 0), (1, 7), (-1, 7), (5, 5),
    (-5, 5), (7, 1), (-7, 1), (4, 6), (-4, 6), (6, 4), (-6, 4),
    (2, 7), (-2, 7), (7, 2), (-7, 2), (3, 7), (-3, 7), (7, 3),
    (-7, 3), (5, 6), (-5, 6), (6, 5), (-6, 5), (8, 0), (4, 7),
    (-4, 7), (7, 4), (-7, 4), (8, 1), (8, 2), (6, 6), (-6, 6),
    (8, 3), (5, 7), (-5, 7), (7, 5), (-7, 5), (8, 4), (6, 7),
    (-6, 7), (7, 6), (-7, 6), (8, 5), (7, 7), (-7, 7), (8, 6),
    (8, 7));

  TransformPredictor = 0;
  TransformCrossColor = 1;
  TransformSubtractGreen = 2;
  TransformColorIndexing = 3;

type
  TCodeLengths = array of Byte;

  { A canonical prefix code: an 8-bit lookup table, and per length the count of codes for longer ones. }
  TPrefixCode = record
    Lookup: array[0..(1 shl TableBits) - 1] of LongWord;  // symbol shl 8 or length, 0 when longer
    Count: array[0..MaxCodeLength] of Word;
    Symbols: array of Word;
    Single: Integer;                                       // the one symbol of a code of 0 bits, or -1
  end;

  TPrefixGroup = array[0..4] of TPrefixCode;

  TTransform = record
    Kind: Integer;
    Bits: Integer;          // block size bits, or pixel bundling bits of the colour index
    Width, Height: Integer; // size of the image the transform applies to
    Data: TWebPPixels;      // sub-image, or colour table
  end;

  { Reads the bits of a VP8L bitstream, least significant bit first. }
  TVP8LDecoder = class
  private
    FData: PByte;
    FSize: Integer;
    FPos: Integer;
    FBits: QWord;
    FBitCount: Integer;
    FPastEnd: Boolean;
    procedure Fill;
    function ReadBits(aCount: Integer): LongWord;
    function ReadSymbol(const aCode: TPrefixCode): Integer;
    procedure BuildCode(var aCode: TPrefixCode; const aLengths: TCodeLengths);
    procedure ReadCode(var aCode: TPrefixCode; aAlphabet: Integer);
    function ReadValue(aPrefix: Integer): Integer;
    function DecodeImage(aWidth, aHeight: Integer; aLevel0: Boolean): TWebPPixels;
  public
    constructor Create(aData: PByte; aSize: Integer);
    function Decode(out aInfo: TVP8LInfo): TWebPPixels;
  end;

function DivRoundUp(aValue, aBits: Integer): Integer;

begin
  Result := (aValue + (1 shl aBits) - 1) shr aBits;
end;


function WebPPixelToColor(aPixel: LongWord): TFPColor;

begin
  Result.Alpha := ((aPixel shr 24) and $FF) * $101;
  Result.Red := ((aPixel shr 16) and $FF) * $101;
  Result.Green := ((aPixel shr 8) and $FF) * $101;
  Result.Blue := (aPixel and $FF) * $101;
end;


function WebPColorToPixel(const aColor: TFPColor): LongWord;

begin
  Result := (LongWord(aColor.Alpha shr 8) shl 24) or (LongWord(aColor.Red shr 8) shl 16)
    or (LongWord(aColor.Green shr 8) shl 8) or LongWord(aColor.Blue shr 8);
end;


// Returns the per-channel sum of two pixels, modulo 256.
function AddPixels(a, b: LongWord): LongWord;

begin
  Result := (((((a and $FF00FF00) shr 8) + ((b and $FF00FF00) shr 8)) and $00FF00FF) shl 8)
    or (((a and $00FF00FF) + (b and $00FF00FF)) and $00FF00FF);
end;


// Returns the per-channel difference of two pixels, modulo 256.
function SubPixels(a, b: LongWord): LongWord;

begin
  Result := ((((((a and $FF00FF00) shr 8) or $01000100) - ((b and $FF00FF00) shr 8)) and $00FF00FF) shl 8)
    or ((((a and $00FF00FF) or $01000100) - (b and $00FF00FF)) and $00FF00FF);
end;


// Returns the per-channel average of two pixels, rounded down.
function Average2(a, b: LongWord): LongWord;

begin
  Result := (((a xor b) and $FEFEFEFE) shr 1) + (a and b);
end;


function Clamp255(aValue: Integer): LongWord;

begin
  if aValue < 0 then
    Result := 0
  else if aValue > 255 then
    Result := 255
  else
    Result := aValue;
end;


// Returns the pixel of L, T and TL whose channels sum closest to those of L + T - TL.
function Select(aL, aT, aTL: LongWord): LongWord;

var
  lPL, lPT, i, lShift: Integer;
  lL, lT, lTL: Integer;

begin
  lPL := 0;
  lPT := 0;
  for i := 0 to 3 do
    begin
    lShift := i * 8;
    lL := (aL shr lShift) and $FF;
    lT := (aT shr lShift) and $FF;
    lTL := (aTL shr lShift) and $FF;
    Inc(lPL, Abs(lT - lTL));
    Inc(lPT, Abs(lL - lTL));
    end;
  if lPL < lPT then
    Result := aL
  else
    Result := aT;
end;


// Returns Clamp(a + b - c) per channel.
function ClampAddSubtractFull(a, b, c: LongWord): LongWord;

var
  i, lShift: Integer;

begin
  Result := 0;
  for i := 0 to 3 do
    begin
    lShift := i * 8;
    Result := Result or (Clamp255(Integer((a shr lShift) and $FF) + Integer((b shr lShift) and $FF)
      - Integer((c shr lShift) and $FF)) shl lShift);
    end;
end;


// Returns Clamp(a + (a - b) / 2) per channel.
function ClampAddSubtractHalf(a, b: LongWord): LongWord;

var
  i, lShift, lA: Integer;

begin
  Result := 0;
  for i := 0 to 3 do
    begin
    lShift := i * 8;
    lA := (a shr lShift) and $FF;
    Result := Result or (Clamp255(lA + (lA - Integer((b shr lShift) and $FF)) div 2) shl lShift);
    end;
end;


// Returns the prediction of predictor aMode from the left, top, top-right and top-left pixels.
function Predict(aMode: Integer; aL, aT, aTR, aTL: LongWord): LongWord;

begin
  case aMode of
    1: Result := aL;
    2: Result := aT;
    3: Result := aTR;
    4: Result := aTL;
    5: Result := Average2(Average2(aL, aTR), aT);
    6: Result := Average2(aL, aTL);
    7: Result := Average2(aL, aT);
    8: Result := Average2(aTL, aT);
    9: Result := Average2(aT, aTR);
    10: Result := Average2(Average2(aL, aTL), Average2(aT, aTR));
    11: Result := Select(aL, aT, aTL);
    12: Result := ClampAddSubtractFull(aL, aT, aTL);
    13: Result := ClampAddSubtractHalf(Average2(aL, aT), aTL);
  else
    Result := $FF000000;
  end;
end;


// Returns the low byte of aValue as a signed value.
function Signed8(aValue: LongWord): Integer;

begin
  Result := aValue and $FF;
  if Result >= 128 then
    Dec(Result, 256);
end;


// Returns the colour transform delta of a signed multiplier and a signed channel, rounded down.
function ColorDelta(aMultiplier, aColor: LongWord): Integer;

var
  lProduct: Integer;

begin
  lProduct := Signed8(aMultiplier) * Signed8(aColor);
  Result := lProduct div 32;
  if (lProduct < 0) and (lProduct mod 32 <> 0) then
    Dec(Result);
end;


// Returns the colour cache index of a pixel.
function CacheIndex(aPixel: LongWord; aBits: Integer): Integer;

begin
  Result := ((QWord(aPixel) * $1E35A7BD) and $FFFFFFFF) shr (32 - aBits);
end;


// Returns the codes of canonical prefix code lengths.
procedure CanonicalCodes(const aLengths: TCodeLengths; out aCodes: array of Word);

var
  lCount: array[0..MaxCodeLength] of Integer;
  lNext: array[0..MaxCodeLength] of Integer;
  i, lCode: Integer;

begin
  FillChar(lCount, SizeOf(lCount), 0);
  for i := 0 to High(aLengths) do
    Inc(lCount[aLengths[i]]);
  lCount[0] := 0;
  lCode := 0;
  for i := 1 to MaxCodeLength do
    begin
    lCode := (lCode + lCount[i - 1]) shl 1;
    lNext[i] := lCode;
    end;
  for i := 0 to High(aLengths) do
    if aLengths[i] > 0 then
      begin
      aCodes[i] := lNext[aLengths[i]];
      Inc(lNext[aLengths[i]]);
      end
    else
      aCodes[i] := 0;
end;


// Returns the lowest aLength bits of aCode in reverse order.
function ReverseBits(aCode: LongWord; aLength: Integer): LongWord;

var
  i: Integer;

begin
  Result := 0;
  for i := 1 to aLength do
    begin
    Result := (Result shl 1) or (aCode and 1);
    aCode := aCode shr 1;
    end;
end;


{ TVP8LDecoder }

constructor TVP8LDecoder.Create(aData: PByte; aSize: Integer);

begin
  inherited Create;
  FData := aData;
  FSize := aSize;
end;


procedure TVP8LDecoder.Fill;

begin
  while (FBitCount <= 56) and (FPos < FSize) do
    begin
    FBits := FBits or (QWord(FData[FPos]) shl FBitCount);
    Inc(FPos);
    Inc(FBitCount, 8);
    end;
end;


function TVP8LDecoder.ReadBits(aCount: Integer): LongWord;

begin
  if aCount = 0 then
    exit(0);
  if FBitCount < aCount then
    Fill;
  if FBitCount < aCount then
    begin
    FPastEnd := True;
    FBitCount := aCount;
    end;
  Result := FBits and ((QWord(1) shl aCount) - 1);
  FBits := FBits shr aCount;
  Dec(FBitCount, aCount);
end;


function TVP8LDecoder.ReadSymbol(const aCode: TPrefixCode): Integer;

var
  lEntry: LongWord;
  lCode, lFirst, lIndex, lCount, lLength: Integer;

begin
  if aCode.Single >= 0 then
    exit(aCode.Single);
  if FBitCount < MaxCodeLength then
    Fill;
  lEntry := aCode.Lookup[FBits and ((1 shl TableBits) - 1)];
  if (lEntry and $FF) <> 0 then
    begin
    lLength := lEntry and $FF;
    if lLength > FBitCount then
      begin
      FPastEnd := True;
      FBitCount := lLength;
      end;
    FBits := FBits shr lLength;
    Dec(FBitCount, lLength);
    exit(lEntry shr 8);
    end;
  lCode := 0;
  lFirst := 0;
  lIndex := 0;
  for lLength := 1 to MaxCodeLength do
    begin
    lCode := lCode or Integer(ReadBits(1));
    lCount := aCode.Count[lLength];
    if lCode - lCount < lFirst then
      exit(aCode.Symbols[lIndex + lCode - lFirst]);
    Inc(lIndex, lCount);
    Inc(lFirst, lCount);
    lFirst := lFirst shl 1;
    lCode := lCode shl 1;
    end;
  raise EWebPError.Create('Invalid prefix code in WebP data');
end;


procedure TVP8LDecoder.BuildCode(var aCode: TPrefixCode; const aLengths: TCodeLengths);

var
  lCodes: array of Word;
  lOffsets: array[0..MaxCodeLength + 1] of Integer;
  lLeft, lUsed, i, j, lLength: Integer;
  lReversed: LongWord;

begin
  FillChar(aCode.Count, SizeOf(aCode.Count), 0);
  FillChar(aCode.Lookup, SizeOf(aCode.Lookup), 0);
  aCode.Single := -1;
  lUsed := 0;
  for i := 0 to High(aLengths) do
    if aLengths[i] > 0 then
      begin
      Inc(aCode.Count[aLengths[i]]);
      Inc(lUsed);
      aCode.Single := i;
      end;
  if lUsed = 0 then
    raise EWebPError.Create('Empty prefix code in WebP data');
  if lUsed = 1 then
    exit;
  aCode.Single := -1;
  lLeft := 1;
  for i := 1 to MaxCodeLength do
    begin
    lLeft := lLeft * 2 - aCode.Count[i];
    if lLeft < 0 then
      raise EWebPError.Create('Over-subscribed prefix code in WebP data');
    end;
  if lLeft <> 0 then
    raise EWebPError.Create('Incomplete prefix code in WebP data');
  lOffsets[1] := 0;
  for i := 1 to MaxCodeLength do
    lOffsets[i + 1] := lOffsets[i] + aCode.Count[i];
  aCode.Symbols := nil;
  SetLength(aCode.Symbols, lUsed);
  for i := 0 to High(aLengths) do
    if aLengths[i] > 0 then
      begin
      aCode.Symbols[lOffsets[aLengths[i]]] := i;
      Inc(lOffsets[aLengths[i]]);
      end;
  lCodes := nil;
  SetLength(lCodes, Length(aLengths));
  CanonicalCodes(aLengths, lCodes);
  for i := 0 to High(aLengths) do
    begin
    lLength := aLengths[i];
    if (lLength = 0) or (lLength > TableBits) then
      continue;
    lReversed := ReverseBits(lCodes[i], lLength);
    j := lReversed;
    while j < (1 shl TableBits) do
      begin
      aCode.Lookup[j] := (LongWord(i) shl 8) or LongWord(lLength);
      Inc(j, 1 shl lLength);
      end;
    end;
end;


procedure TVP8LDecoder.ReadCode(var aCode: TPrefixCode; aAlphabet: Integer);

var
  lLengths, lCodeLengths: TCodeLengths;
  lLengthCode: TPrefixCode;
  lCount, lSymbol, lMax, lPrevious, lValue, lRepeat, i: Integer;
  lFirst8: Boolean;

begin
  lLengths := nil;
  SetLength(lLengths, aAlphabet);
  if ReadBits(1) = 1 then
    begin
    lCount := ReadBits(1) + 1;
    lFirst8 := ReadBits(1) = 1;
    if lFirst8 then
      lSymbol := ReadBits(8)
    else
      lSymbol := ReadBits(1);
    if lSymbol >= aAlphabet then
      raise EWebPError.Create('Invalid simple prefix code in WebP data');
    lLengths[lSymbol] := 1;
    if lCount = 2 then
      begin
      lSymbol := ReadBits(8);
      if lSymbol >= aAlphabet then
        raise EWebPError.Create('Invalid simple prefix code in WebP data');
      lLengths[lSymbol] := 1;
      end;
    end
  else
    begin
    lCodeLengths := nil;
    SetLength(lCodeLengths, CodeLengthCodes);
    lCount := ReadBits(4) + 4;
    for i := 0 to lCount - 1 do
      lCodeLengths[CodeLengthOrder[i]] := ReadBits(3);
    BuildCode(lLengthCode, lCodeLengths);
    if ReadBits(1) = 1 then
      begin
      lValue := 2 + 2 * Integer(ReadBits(3));
      lMax := 2 + Integer(ReadBits(lValue));
      if lMax > aAlphabet then
        raise EWebPError.Create('Invalid code length count in WebP data');
      end
    else
      lMax := aAlphabet;
    lSymbol := 0;
    lPrevious := 8;
    while lSymbol < aAlphabet do
      begin
      if lMax = 0 then
        break;
      Dec(lMax);
      lValue := ReadSymbol(lLengthCode);
      if lValue < 16 then
        begin
        lLengths[lSymbol] := lValue;
        Inc(lSymbol);
        if lValue <> 0 then
          lPrevious := lValue;
        end
      else
        begin
        case lValue of
          16: lRepeat := 3 + Integer(ReadBits(2));
          17: lRepeat := 3 + Integer(ReadBits(3));
        else
          lRepeat := 11 + Integer(ReadBits(7));
        end;
        if lSymbol + lRepeat > aAlphabet then
          raise EWebPError.Create('Invalid code length repeat in WebP data');
        if lValue = 16 then
          lValue := lPrevious
        else
          lValue := 0;
        for i := 1 to lRepeat do
          begin
          lLengths[lSymbol] := lValue;
          Inc(lSymbol);
          end;
        end;
      end;
    end;
  BuildCode(aCode, lLengths);
end;


function TVP8LDecoder.ReadValue(aPrefix: Integer): Integer;

var
  lExtra: Integer;

begin
  if aPrefix < 4 then
    exit(aPrefix + 1);
  lExtra := (aPrefix - 2) shr 1;
  Result := ((2 + (aPrefix and 1)) shl lExtra) + Integer(ReadBits(lExtra)) + 1;
end;


function TVP8LDecoder.DecodeImage(aWidth, aHeight: Integer; aLevel0: Boolean): TWebPPixels;

var
  lCacheBits, lCacheSize, lPrefixBits, lPrefixWidth, lGroupCount: Integer;
  lEntropy: TWebPPixels;
  lGroups: array of TPrefixGroup;
  lCache: TWebPPixels;
  lTotal, lPos, x, y, i, lGroup, lSymbol, lLength, lDistance, lCode: Integer;
  lPixel: LongWord;
  lOut: TWebPPixels;

  procedure Put(aPixel: LongWord);

  begin
    lOut[lPos] := aPixel;
    if lCacheBits > 0 then
      lCache[CacheIndex(aPixel, lCacheBits)] := aPixel;
    Inc(lPos);
    Inc(x);
    if x = aWidth then
      begin
      x := 0;
      Inc(y);
      end;
  end;

begin
  lCacheBits := 0;
  if ReadBits(1) = 1 then
    begin
    lCacheBits := ReadBits(4);
    if (lCacheBits < 1) or (lCacheBits > MaxCacheBits) then
      raise EWebPError.Create('Invalid colour cache size in WebP data');
    end;
  lCacheSize := 0;
  if lCacheBits > 0 then
    lCacheSize := 1 shl lCacheBits;
  lCache := nil;
  SetLength(lCache, lCacheSize);
  lPrefixBits := 0;
  lPrefixWidth := 0;
  lGroupCount := 1;
  lEntropy := nil;
  if aLevel0 and (ReadBits(1) = 1) then
    begin
    lPrefixBits := ReadBits(3) + 2;
    lPrefixWidth := DivRoundUp(aWidth, lPrefixBits);
    lEntropy := DecodeImage(lPrefixWidth, DivRoundUp(aHeight, lPrefixBits), False);
    for i := 0 to High(lEntropy) do
      begin
      lEntropy[i] := (lEntropy[i] shr 8) and $FFFF;
      if Integer(lEntropy[i]) + 1 > lGroupCount then
        lGroupCount := lEntropy[i] + 1;
      end;
    end;
  lGroups := nil;
  SetLength(lGroups, lGroupCount);
  for i := 0 to lGroupCount - 1 do
    begin
    ReadCode(lGroups[i][0], NumLiteralCodes + NumLengthCodes + lCacheSize);
    ReadCode(lGroups[i][1], NumLiteralCodes);
    ReadCode(lGroups[i][2], NumLiteralCodes);
    ReadCode(lGroups[i][3], NumLiteralCodes);
    ReadCode(lGroups[i][4], NumDistanceCodes);
    end;
  lTotal := aWidth * aHeight;
  lOut := nil;
  SetLength(lOut, lTotal);
  lPos := 0;
  x := 0;
  y := 0;
  lGroup := 0;
  while lPos < lTotal do
    begin
    if FPastEnd then
      raise EWebPError.Create('Truncated WebP data');
    if lPrefixBits > 0 then
      lGroup := lEntropy[(y shr lPrefixBits) * lPrefixWidth + (x shr lPrefixBits)];
    lSymbol := ReadSymbol(lGroups[lGroup][0]);
    if lSymbol < NumLiteralCodes then
      begin
      lPixel := (LongWord(ReadSymbol(lGroups[lGroup][1])) shl 16) or (LongWord(lSymbol) shl 8);
      lPixel := lPixel or LongWord(ReadSymbol(lGroups[lGroup][2]));
      lPixel := lPixel or (LongWord(ReadSymbol(lGroups[lGroup][3])) shl 24);
      Put(lPixel);
      end
    else if lSymbol < NumLiteralCodes + NumLengthCodes then
      begin
      lLength := ReadValue(lSymbol - NumLiteralCodes);
      lCode := ReadValue(ReadSymbol(lGroups[lGroup][4]));
      if lCode > MaxPlaneCode then
        lDistance := lCode - MaxPlaneCode
      else
        begin
        lDistance := DistanceMap[lCode - 1, 0] + DistanceMap[lCode - 1, 1] * aWidth;
        if lDistance < 1 then
          lDistance := 1;
        end;
      if (lDistance > lPos) or (lLength > lTotal - lPos) then
        raise EWebPError.Create('Invalid back reference in WebP data');
      for i := 1 to lLength do
        Put(lOut[lPos - lDistance]);
      end
    else
      begin
      i := lSymbol - NumLiteralCodes - NumLengthCodes;
      if i >= lCacheSize then
        raise EWebPError.Create('Invalid colour cache index in WebP data');
      Put(lCache[i]);
      end;
    end;
  if FPastEnd then
    raise EWebPError.Create('Truncated WebP data');
  Result := lOut;
end;


function TVP8LDecoder.Decode(out aInfo: TVP8LInfo): TWebPPixels;

var
  lTransforms: array[0..3] of TTransform;
  lCount, lWidth, lKind, i, j, x, y, lBits, lSize, lPos, lMode, lPerByte, lPixelBits: Integer;
  lSeen: set of 0..3;
  lPixel, lBlock, lRed, lBlue, lGreen: LongWord;
  lIndexed: TWebPPixels;

begin
  if ReadBits(8) <> VP8LSignature then
    raise EWebPError.Create('Not a VP8L bitstream');
  aInfo.Width := ReadBits(14) + 1;
  aInfo.Height := ReadBits(14) + 1;
  aInfo.AlphaUsed := ReadBits(1) = 1;
  if ReadBits(3) <> 0 then
    raise EWebPError.Create('Unknown VP8L version');
  lCount := 0;
  lSeen := [];
  lWidth := aInfo.Width;
  while ReadBits(1) = 1 do
    begin
    lKind := ReadBits(2);
    if lKind in lSeen then
      raise EWebPError.Create('A VP8L transform is used twice');
    Include(lSeen, lKind);
    with lTransforms[lCount] do
      begin
      Kind := lKind;
      Width := lWidth;
      Height := aInfo.Height;
      Bits := 0;
      Data := nil;
      case lKind of
        TransformPredictor, TransformCrossColor:
          begin
          Bits := ReadBits(3) + 2;
          Data := DecodeImage(DivRoundUp(Width, Bits), DivRoundUp(Height, Bits), False);
          end;
        TransformColorIndexing:
          begin
          lSize := ReadBits(8) + 1;
          Data := DecodeImage(lSize, 1, False);
          for i := 1 to lSize - 1 do
            Data[i] := AddPixels(Data[i], Data[i - 1]);
          if lSize <= 2 then
            Bits := 3
          else if lSize <= 4 then
            Bits := 2
          else if lSize <= 16 then
            Bits := 1;
          lWidth := DivRoundUp(lWidth, Bits);
          end;
      end;
      end;
    Inc(lCount);
    end;
  Result := DecodeImage(lWidth, aInfo.Height, True);
  for i := lCount - 1 downto 0 do
    with lTransforms[i] do
      case Kind of
        TransformSubtractGreen:
          for j := 0 to High(Result) do
            begin
            lGreen := (Result[j] shr 8) and $FF;
            Result[j] := AddPixels(Result[j], (lGreen shl 16) or lGreen);
            end;
        TransformPredictor:
          begin
          lBits := DivRoundUp(Width, Bits);
          for y := 0 to Height - 1 do
            for x := 0 to Width - 1 do
              begin
              lPos := y * Width + x;
              if y = 0 then
                begin
                if x = 0 then
                  lPixel := $FF000000
                else
                  lPixel := Result[lPos - 1];
                end
              else if x = 0 then
                lPixel := Result[lPos - Width]
              else
                begin
                lMode := (Data[(y shr Bits) * lBits + (x shr Bits)] shr 8) and $F;
                lPixel := Predict(lMode, Result[lPos - 1], Result[lPos - Width], Result[lPos - Width + 1],
                  Result[lPos - Width - 1]);
                end;
              Result[lPos] := AddPixels(Result[lPos], lPixel);
              end;
          end;
        TransformCrossColor:
          begin
          lBits := DivRoundUp(Width, Bits);
          for y := 0 to Height - 1 do
            for x := 0 to Width - 1 do
              begin
              lPos := y * Width + x;
              lBlock := Data[(y shr Bits) * lBits + (x shr Bits)];
              lPixel := Result[lPos];
              lGreen := (lPixel shr 8) and $FF;
              lRed := (Integer((lPixel shr 16) and $FF) + ColorDelta(lBlock, lGreen)) and $FF;
              lBlue := (Integer(lPixel and $FF) + ColorDelta(lBlock shr 8, lGreen)) and $FF;
              lBlue := (Integer(lBlue) + ColorDelta(lBlock shr 16, lRed)) and $FF;
              Result[lPos] := (lPixel and $FF00FF00) or (lRed shl 16) or lBlue;
              end;
          end;
        TransformColorIndexing:
          begin
          lIndexed := Result;
          Result := nil;
          SetLength(Result, Width * Height);
          lPerByte := 1 shl Bits;
          lPixelBits := 8 shr Bits;
          lBits := DivRoundUp(Width, Bits);
          for y := 0 to Height - 1 do
            for x := 0 to Width - 1 do
              begin
              lMode := (lIndexed[y * lBits + (x shr Bits)] shr 8) and $FF;
              lMode := (lMode shr ((x and (lPerByte - 1)) * lPixelBits)) and ((1 shl lPixelBits) - 1);
              if lMode < Length(Data) then
                Result[y * Width + x] := Data[lMode]
              else
                Result[y * Width + x] := 0;
              end;
          end;
      end;
end;


function VP8LReadInfo(aData: PByte; aSize: Integer; out aInfo: TVP8LInfo): Boolean;

var
  lBits: LongWord;

begin
  Result := (aSize >= 5) and (aData[0] = VP8LSignature);
  if not Result then
    exit;
  lBits := LongWord(aData[1]) or (LongWord(aData[2]) shl 8) or (LongWord(aData[3]) shl 16) or (LongWord(aData[4]) shl 24);
  aInfo.Width := (lBits and $3FFF) + 1;
  aInfo.Height := ((lBits shr 14) and $3FFF) + 1;
  aInfo.AlphaUsed := ((lBits shr 28) and 1) = 1;
  Result := (lBits shr 29) = 0;
end;


function VP8LDecode(aData: PByte; aSize: Integer; out aInfo: TVP8LInfo): TWebPPixels;

var
  lDecoder: TVP8LDecoder;

begin
  lDecoder := TVP8LDecoder.Create(aData, aSize);
  try
    Result := lDecoder.Decode(aInfo);
  finally
    lDecoder.Free;
  end;
end;


{ Encoder }

type
  { A literal pixel, or a back reference of Length pixels at Distance. }
  TToken = record
    Pixel: LongWord;
    Length: Integer;
    Distance: Integer;
  end;
  TTokens = array of TToken;

  { Writes bits least significant first. }
  TVP8LEncoder = class
  private
    FOut: TBytes;
    FSize: Integer;
    FBits: QWord;
    FBitCount: Integer;
    procedure WriteBits(aValue: LongWord; aCount: Integer);
    procedure Flush;
    procedure WriteCode(const aCounts: array of LongWord; out aLengths: TCodeLengths; out aCodes: TWebPPixels);
    procedure WriteImage(const aPixels: TWebPPixels; aWidth, aHeight: Integer; aLevel0: Boolean);
  public
    function Encode(const aPixels: TWebPPixels; aWidth, aHeight: Integer): TBytes;
  end;

procedure TVP8LEncoder.WriteBits(aValue: LongWord; aCount: Integer);

begin
  if aCount = 0 then
    exit;
  FBits := FBits or (QWord(aValue and ((QWord(1) shl aCount) - 1)) shl FBitCount);
  Inc(FBitCount, aCount);
  while FBitCount >= 8 do
    begin
    if FSize >= Length(FOut) then
      SetLength(FOut, Length(FOut) * 2 + 1024);
    FOut[FSize] := FBits and $FF;
    Inc(FSize);
    FBits := FBits shr 8;
    Dec(FBitCount, 8);
    end;
end;


procedure TVP8LEncoder.Flush;

begin
  if FBitCount > 0 then
    WriteBits(0, 8 - FBitCount);
end;


// Returns code lengths of at most aLimit bits for the symbol counts; one used symbol gets length 1.
function BuildLengths(const aCounts: array of LongWord; aLimit: Integer): TCodeLengths;

var
  lSymbols: array of Integer;
  lWeights: array of QWord;
  lParent: array of Integer;
  lDepth: array of Integer;
  lUsed, lNodes, i, j, lLeaf, lInner, lA, lB, lMax: Integer;
  lMin: QWord;

  // Returns the next node of least weight among the leaves and the inner nodes.
  function Take: Integer;

  begin
    if (lLeaf < lUsed) and ((lInner >= lNodes) or (lWeights[lLeaf] <= lWeights[lInner])) then
      begin
      Result := lLeaf;
      Inc(lLeaf);
      end
    else
      begin
      Result := lInner;
      Inc(lInner);
      end;
  end;

begin
  Result := nil;
  SetLength(Result, Length(aCounts));
  lSymbols := nil;
  for i := 0 to High(aCounts) do
    if aCounts[i] > 0 then
      begin
      SetLength(lSymbols, Length(lSymbols) + 1);
      lSymbols[High(lSymbols)] := i;
      end;
  lUsed := Length(lSymbols);
  if lUsed = 0 then
    exit;
  if lUsed = 1 then
    begin
    Result[lSymbols[0]] := 1;
    exit;
    end;
  // sort the used symbols by count, keeping symbol order for equal counts
  for i := 1 to lUsed - 1 do
    begin
    lA := lSymbols[i];
    j := i - 1;
    while (j >= 0) and (aCounts[lSymbols[j]] > aCounts[lA]) do
      begin
      lSymbols[j + 1] := lSymbols[j];
      Dec(j);
      end;
    lSymbols[j + 1] := lA;
    end;
  lWeights := nil;
  lParent := nil;
  lDepth := nil;
  SetLength(lWeights, 2 * lUsed);
  SetLength(lParent, 2 * lUsed);
  SetLength(lDepth, 2 * lUsed);
  lMin := 1;
  repeat
    for i := 0 to lUsed - 1 do
      if aCounts[lSymbols[i]] < lMin then
        lWeights[i] := lMin
      else
        lWeights[i] := aCounts[lSymbols[i]];
    lLeaf := 0;
    lInner := lUsed;
    lNodes := lUsed;
    while lNodes < 2 * lUsed - 1 do
      begin
      lA := Take;
      lB := Take;
      lWeights[lNodes] := lWeights[lA] + lWeights[lB];
      lParent[lA] := lNodes;
      lParent[lB] := lNodes;
      Inc(lNodes);
      end;
    lDepth[lNodes - 1] := 0;
    lMax := 0;
    for i := lNodes - 2 downto 0 do
      begin
      lDepth[i] := lDepth[lParent[i]] + 1;
      if (i < lUsed) and (lDepth[i] > lMax) then
        lMax := lDepth[i];
      end;
    lMin := lMin * 2;
  until lMax <= aLimit;
  for i := 0 to lUsed - 1 do
    Result[lSymbols[i]] := lDepth[i];
end;


procedure TVP8LEncoder.WriteCode(const aCounts: array of LongWord; out aLengths: TCodeLengths; out aCodes: TWebPPixels);

var
  lUsed: array of Integer;
  lTokens, lExtra: array of Integer;
  lCLCounts: array[0..CodeLengthCodes - 1] of LongWord;
  lCLLengths: TCodeLengths;
  lCLCodes: array of Word;
  lCodes: array of Word;
  i, lRun, lValue, lPrevious, lCount, lLast: Integer;

  procedure AddToken(aToken, aExtra: Integer);

  begin
    SetLength(lTokens, Length(lTokens) + 1);
    SetLength(lExtra, Length(lExtra) + 1);
    lTokens[High(lTokens)] := aToken;
    lExtra[High(lExtra)] := aExtra;
    Inc(lCLCounts[aToken]);
  end;

begin
  aLengths := BuildLengths(aCounts, MaxCodeLength);
  lUsed := nil;
  for i := 0 to High(aLengths) do
    if aLengths[i] > 0 then
      begin
      SetLength(lUsed, Length(lUsed) + 1);
      lUsed[High(lUsed)] := i;
      end;
  if Length(lUsed) = 0 then
    begin
    lUsed := [0];
    aLengths[0] := 1;
    end;
  aCodes := nil;
  SetLength(aCodes, Length(aLengths));
  if (Length(lUsed) <= 2) and (lUsed[High(lUsed)] < NumLiteralCodes) then
    begin
    // A simple code: one symbol takes no bits, two take one each.
    WriteBits(1, 1);
    WriteBits(Length(lUsed) - 1, 1);
    if lUsed[0] < 2 then
      begin
      WriteBits(0, 1);
      WriteBits(lUsed[0], 1);
      end
    else
      begin
      WriteBits(1, 1);
      WriteBits(lUsed[0], 8);
      end;
    if Length(lUsed) = 2 then
      begin
      WriteBits(lUsed[1], 8);
      aLengths[lUsed[0]] := 1;
      aLengths[lUsed[1]] := 1;
      aCodes[lUsed[1]] := 1 or (1 shl 16);
      aCodes[lUsed[0]] := 1 shl 16;
      end
    else
      aCodes[lUsed[0]] := 0;
    exit;
    end;
  WriteBits(0, 1);
  lTokens := nil;
  lExtra := nil;
  FillChar(lCLCounts, SizeOf(lCLCounts), 0);
  lPrevious := 8;
  i := 0;
  while i < Length(aLengths) do
    begin
    lValue := aLengths[i];
    lRun := 1;
    while (i + lRun < Length(aLengths)) and (aLengths[i + lRun] = lValue) do
      Inc(lRun);
    Inc(i, lRun);
    if lValue = 0 then
      begin
      while lRun >= 11 do
        begin
        lCount := lRun;
        if lCount > 138 then
          lCount := 138;
        AddToken(18, lCount - 11);
        Dec(lRun, lCount);
        end;
      while lRun >= 3 do
        begin
        lCount := lRun;
        if lCount > 10 then
          lCount := 10;
        AddToken(17, lCount - 3);
        Dec(lRun, lCount);
        end;
      end
    else
      begin
      if lValue <> lPrevious then
        begin
        AddToken(lValue, 0);
        lPrevious := lValue;
        Dec(lRun);
        end;
      while lRun >= 3 do
        begin
        lCount := lRun;
        if lCount > 6 then
          lCount := 6;
        AddToken(16, lCount - 3);
        Dec(lRun, lCount);
        end;
      end;
    while lRun > 0 do
      begin
      AddToken(lValue, 0);
      Dec(lRun);
      end;
    end;
  lCLLengths := BuildLengths(lCLCounts, 7);
  lLast := 3;
  for i := 0 to CodeLengthCodes - 1 do
    if lCLLengths[CodeLengthOrder[i]] > 0 then
      lLast := i;
  WriteBits(lLast + 1 - 4, 4);
  for i := 0 to lLast do
    WriteBits(lCLLengths[CodeLengthOrder[i]], 3);
  lCLCodes := nil;
  SetLength(lCLCodes, CodeLengthCodes);
  lValue := 0;
  for i := 0 to CodeLengthCodes - 1 do
    if lCLLengths[i] > 0 then
      Inc(lValue);
  if lValue > 1 then
    CanonicalCodes(lCLLengths, lCLCodes);
  WriteBits(0, 1);
  for i := 0 to High(lTokens) do
    begin
    if lValue > 1 then
      WriteBits(ReverseBits(lCLCodes[lTokens[i]], lCLLengths[lTokens[i]]), lCLLengths[lTokens[i]]);
    case lTokens[i] of
      16: WriteBits(lExtra[i], 2);
      17: WriteBits(lExtra[i], 3);
      18: WriteBits(lExtra[i], 7);
    end;
    end;
  lCodes := nil;
  SetLength(lCodes, Length(aLengths));
  if Length(lUsed) > 1 then
    begin
    CanonicalCodes(aLengths, lCodes);
    for i := 0 to High(aLengths) do
      if aLengths[i] > 0 then
        aCodes[i] := ReverseBits(lCodes[i], aLengths[i]) or (LongWord(aLengths[i]) shl 16);
    end;
end;


// Returns the prefix code of a length or distance value, with its extra bits.
procedure ValueToPrefix(aValue: Integer; out aPrefix, aExtraBits, aExtra: Integer);

var
  lValue, lHigh: Integer;

begin
  lValue := aValue - 1;
  if lValue < 4 then
    begin
    aPrefix := lValue;
    aExtraBits := 0;
    aExtra := 0;
    exit;
    end;
  lHigh := BsrDWord(lValue);
  aExtraBits := lHigh - 1;
  aPrefix := 2 * lHigh + ((lValue shr (lHigh - 1)) and 1);
  aExtra := lValue and ((1 shl aExtraBits) - 1);
end;


// Returns the pixels of aPixels as literals and back references.
function FindMatches(const aPixels: TWebPPixels; aWidth: Integer): TTokens;

const
  HashBits = 16;
  MaxChain = 32;
  MaxDistance = (1 shl 20) - MaxPlaneCode - 1;

var
  lHead: array of Integer;
  lPrevious: array of Integer;
  lCount, lTotal, lPos, lBest, lBestDistance, lCandidate, lLength, lChain, i: Integer;

  function Hash(aPos: Integer): Integer;

  begin
    Result := ((QWord(aPixels[aPos]) * $1E35A7BD + QWord(aPixels[aPos + 1]) * $9E3779B1) and $FFFFFFFF)
      shr (32 - HashBits);
  end;

  function MatchLength(aFrom, aTo: Integer): Integer;

  begin
    Result := 0;
    while (Result < MaxLength) and (aTo + Result < lTotal) and (aPixels[aFrom + Result] = aPixels[aTo + Result]) do
      Inc(Result);
  end;

  procedure TryMatch(aCandidate: Integer);

  var
    lFound: Integer;

  begin
    if (aCandidate < 0) or (lPos - aCandidate > MaxDistance) then
      exit;
    lFound := MatchLength(aCandidate, lPos);
    if lFound > lBest then
      begin
      lBest := lFound;
      lBestDistance := lPos - aCandidate;
      end;
  end;

  procedure Insert(aPos: Integer);

  var
    lHash: Integer;

  begin
    if aPos + 1 >= lTotal then
      exit;
    lHash := Hash(aPos);
    lPrevious[aPos] := lHead[lHash];
    lHead[lHash] := aPos;
  end;

begin
  lTotal := Length(aPixels);
  Result := nil;
  SetLength(Result, lTotal);
  lHead := nil;
  lPrevious := nil;
  SetLength(lHead, 1 shl HashBits);
  SetLength(lPrevious, lTotal);
  for i := 0 to High(lHead) do
    lHead[i] := -1;
  lCount := 0;
  lPos := 0;
  while lPos < lTotal do
    begin
    lBest := 0;
    lBestDistance := 0;
    if lPos > 0 then
      begin
      TryMatch(lPos - 1);
      TryMatch(lPos - aWidth);
      if lPos + 1 < lTotal then
        begin
        lCandidate := lHead[Hash(lPos)];
        lChain := 0;
        while (lCandidate >= 0) and (lChain < MaxChain) and (lBest < MaxLength) do
          begin
          TryMatch(lCandidate);
          lCandidate := lPrevious[lCandidate];
          Inc(lChain);
          end;
        end;
      end;
    if lBest >= 3 then
      begin
      Result[lCount].Length := lBest;
      Result[lCount].Distance := lBestDistance;
      for lLength := 0 to lBest - 1 do
        Insert(lPos + lLength);
      Inc(lPos, lBest);
      end
    else
      begin
      Result[lCount].Pixel := aPixels[lPos];
      Result[lCount].Length := 0;
      Insert(lPos);
      Inc(lPos);
      end;
    Inc(lCount);
    end;
  SetLength(Result, lCount);
end;


procedure TVP8LEncoder.WriteImage(const aPixels: TWebPPixels; aWidth, aHeight: Integer; aLevel0: Boolean);

var
  lTokens: TTokens;
  lCounts: array[0..4] of array of LongWord;
  lLengths: array[0..4] of TCodeLengths;
  lCodes: array[0..4] of TWebPPixels;
  lPlane: array of Integer;
  i, lPrefix, lExtraBits, lExtra, lCode, lDistCode: Integer;

  procedure Put(aTree: Integer; aSymbol: Integer);

  begin
    WriteBits(lCodes[aTree][aSymbol] and $FFFF, lCodes[aTree][aSymbol] shr 16);
  end;

  // Returns the distance code of a distance, a plane code for the nearest ones.
  function DistanceCode(aDistance: Integer): Integer;

  begin
    if (aDistance < Length(lPlane)) and (lPlane[aDistance] > 0) then
      Result := lPlane[aDistance]
    else
      Result := aDistance + MaxPlaneCode;
  end;

begin
  WriteBits(0, 1);
  if aLevel0 then
    WriteBits(0, 1);
  lPlane := nil;
  SetLength(lPlane, 8 * aWidth + 9);
  for i := MaxPlaneCode - 1 downto 0 do
    begin
    lCode := DistanceMap[i, 0] + DistanceMap[i, 1] * aWidth;
    if (lCode >= 1) and (lCode < Length(lPlane)) then
      lPlane[lCode] := i + 1;
    end;
  lTokens := FindMatches(aPixels, aWidth);
  SetLength(lCounts[0], NumLiteralCodes + NumLengthCodes);
  for i := 1 to 3 do
    SetLength(lCounts[i], NumLiteralCodes);
  SetLength(lCounts[4], NumDistanceCodes);
  for i := 0 to 4 do
    FillChar(lCounts[i][0], Length(lCounts[i]) * SizeOf(LongWord), 0);
  for i := 0 to High(lTokens) do
    with lTokens[i] do
      if Length = 0 then
        begin
        Inc(lCounts[0][(Pixel shr 8) and $FF]);
        Inc(lCounts[1][(Pixel shr 16) and $FF]);
        Inc(lCounts[2][Pixel and $FF]);
        Inc(lCounts[3][Pixel shr 24]);
        end
      else
        begin
        ValueToPrefix(Length, lPrefix, lExtraBits, lExtra);
        Inc(lCounts[0][NumLiteralCodes + lPrefix]);
        ValueToPrefix(DistanceCode(Distance), lPrefix, lExtraBits, lExtra);
        Inc(lCounts[4][lPrefix]);
        end;
  for i := 0 to 4 do
    WriteCode(lCounts[i], lLengths[i], lCodes[i]);
  for i := 0 to High(lTokens) do
    with lTokens[i] do
      if Length = 0 then
        begin
        Put(0, (Pixel shr 8) and $FF);
        Put(1, (Pixel shr 16) and $FF);
        Put(2, Pixel and $FF);
        Put(3, Pixel shr 24);
        end
      else
        begin
        ValueToPrefix(Length, lPrefix, lExtraBits, lExtra);
        Put(0, NumLiteralCodes + lPrefix);
        WriteBits(lExtra, lExtraBits);
        lDistCode := DistanceCode(Distance);
        ValueToPrefix(lDistCode, lPrefix, lExtraBits, lExtra);
        Put(4, lPrefix);
        WriteBits(lExtra, lExtraBits);
        end;
end;


// Returns the cost of a residual: the sum of the magnitudes of its signed channels.
function ResidualCost(aResidual: LongWord): Integer;

var
  i, lValue: Integer;

begin
  Result := 0;
  for i := 0 to 3 do
    begin
    lValue := (aResidual shr (i * 8)) and $FF;
    if lValue > 128 then
      lValue := 256 - lValue;
    Inc(Result, lValue);
    end;
end;


function TVP8LEncoder.Encode(const aPixels: TWebPPixels; aWidth, aHeight: Integer): TBytes;

const
  PredictorBits = 4;

var
  lPalette: array of LongWord;
  lAlpha: Boolean;
  lWork, lModes, lResidual, lPacked: TWebPPixels;
  i, j, x, y, lBlocksWide, lBlocksHigh, bx, by, lMode, lBest, lCost, lBestCost, lPos, lBits, lIndex: Integer;
  lPixel: LongWord;

  // Returns the index of aPixel in the sorted palette, or -1.
  function PaletteIndex(aPixel: LongWord): Integer;

  var
    lLow, lHigh, lMid: Integer;

  begin
    lLow := 0;
    lHigh := High(lPalette);
    while lLow <= lHigh do
      begin
      lMid := (lLow + lHigh) div 2;
      if lPalette[lMid] = aPixel then
        exit(lMid)
      else if lPalette[lMid] < aPixel then
        lLow := lMid + 1
      else
        lHigh := lMid - 1;
      end;
    Result := -1;
  end;

  // Adds aPixel to the sorted palette; False when it would hold more than 256 colours.
  function AddToPalette(aPixel: LongWord): Boolean;

  var
    lAt, k: Integer;

  begin
    Result := True;
    if PaletteIndex(aPixel) >= 0 then
      exit;
    if Length(lPalette) = 256 then
      exit(False);
    lAt := 0;
    while (lAt < Length(lPalette)) and (lPalette[lAt] < aPixel) do
      Inc(lAt);
    SetLength(lPalette, Length(lPalette) + 1);
    for k := High(lPalette) downto lAt + 1 do
      lPalette[k] := lPalette[k - 1];
    lPalette[lAt] := aPixel;
  end;

  // Returns the prediction for pixel x, y of lWork with mode aMode, or with the border rules.
  function PredictAt(aMode, ax, ay: Integer): LongWord;

  var
    p: Integer;

  begin
    p := ay * aWidth + ax;
    if ay = 0 then
      begin
      if ax = 0 then
        Result := $FF000000
      else
        Result := lWork[p - 1];
      end
    else if ax = 0 then
      Result := lWork[p - aWidth]
    else
      Result := Predict(aMode, lWork[p - 1], lWork[p - aWidth], lWork[p - aWidth + 1], lWork[p - aWidth - 1]);
  end;

begin
  if (aWidth < 1) or (aHeight < 1) or (aWidth > VP8LMaxDimension) or (aHeight > VP8LMaxDimension) then
    raise EWebPError.CreateFmt('A WebP image is 1 to %d pixels wide and high', [VP8LMaxDimension]);
  FOut := nil;
  SetLength(FOut, 1024);
  FSize := 0;
  FBits := 0;
  FBitCount := 0;
  lAlpha := False;
  lPalette := nil;
  lBits := 0;
  for i := 0 to High(aPixels) do
    begin
    if aPixels[i] shr 24 <> $FF then
      lAlpha := True;
    if (lBits = 0) and not AddToPalette(aPixels[i]) then
      lBits := -1;
    end;
  WriteBits(VP8LSignature, 8);
  WriteBits(aWidth - 1, 14);
  WriteBits(aHeight - 1, 14);
  WriteBits(Ord(lAlpha), 1);
  WriteBits(0, 3);
  if lBits = 0 then
    begin
    // A colour index of at most 256 colours, several indices in a pixel.
    WriteBits(1, 1);
    WriteBits(TransformColorIndexing, 2);
    WriteBits(Length(lPalette) - 1, 8);
    lWork := nil;
    SetLength(lWork, Length(lPalette));
    lWork[0] := lPalette[0];
    for i := 1 to High(lPalette) do
      lWork[i] := SubPixels(lPalette[i], lPalette[i - 1]);
    WriteImage(lWork, Length(lPalette), 1, False);
    if Length(lPalette) <= 2 then
      lBits := 3
    else if Length(lPalette) <= 4 then
      lBits := 2
    else if Length(lPalette) <= 16 then
      lBits := 1
    else
      lBits := 0;
    j := DivRoundUp(aWidth, lBits);
    lPacked := nil;
    SetLength(lPacked, j * aHeight);
    for y := 0 to aHeight - 1 do
      for x := 0 to aWidth - 1 do
        begin
        lIndex := PaletteIndex(aPixels[y * aWidth + x]);
        lPos := y * j + (x shr lBits);
        lPacked[lPos] := lPacked[lPos] or (LongWord(lIndex) shl (8 + (x and ((1 shl lBits) - 1)) * (8 shr lBits)));
        end;
    for i := 0 to High(lPacked) do
      lPacked[i] := lPacked[i] or $FF000000;
    WriteBits(0, 1);
    WriteImage(lPacked, j, aHeight, True);
    end
  else
    begin
    lWork := Copy(aPixels);
    WriteBits(1, 1);
    WriteBits(TransformSubtractGreen, 2);
    for i := 0 to High(lWork) do
      begin
      lPixel := (lWork[i] shr 8) and $FF;
      lWork[i] := SubPixels(lWork[i], (lPixel shl 16) or lPixel);
      end;
    WriteBits(1, 1);
    WriteBits(TransformPredictor, 2);
    WriteBits(PredictorBits - 2, 3);
    lBlocksWide := DivRoundUp(aWidth, PredictorBits);
    lBlocksHigh := DivRoundUp(aHeight, PredictorBits);
    lModes := nil;
    SetLength(lModes, lBlocksWide * lBlocksHigh);
    lResidual := nil;
    SetLength(lResidual, Length(lWork));
    for by := 0 to lBlocksHigh - 1 do
      for bx := 0 to lBlocksWide - 1 do
        begin
        lBest := 0;
        lBestCost := MaxInt;
        for lMode := 0 to 13 do
          begin
          lCost := 0;
          for y := by shl PredictorBits to Min(aHeight, (by + 1) shl PredictorBits) - 1 do
            for x := bx shl PredictorBits to Min(aWidth, (bx + 1) shl PredictorBits) - 1 do
              Inc(lCost, ResidualCost(SubPixels(lWork[y * aWidth + x], PredictAt(lMode, x, y))));
          if lCost < lBestCost then
            begin
            lBestCost := lCost;
            lBest := lMode;
            end;
          end;
        lModes[by * lBlocksWide + bx] := $FF000000 or (LongWord(lBest) shl 8);
        for y := by shl PredictorBits to Min(aHeight, (by + 1) shl PredictorBits) - 1 do
          for x := bx shl PredictorBits to Min(aWidth, (bx + 1) shl PredictorBits) - 1 do
            lResidual[y * aWidth + x] := SubPixels(lWork[y * aWidth + x], PredictAt(lBest, x, y));
        end;
    WriteImage(lModes, lBlocksWide, lBlocksHigh, False);
    WriteBits(0, 1);
    WriteImage(lResidual, aWidth, aHeight, True);
    end;
  Flush;
  SetLength(FOut, FSize);
  Result := FOut;
end;


function VP8LEncode(const aPixels: TWebPPixels; aWidth, aHeight: Integer): TBytes;

var
  lEncoder: TVP8LEncoder;

begin
  lEncoder := TVP8LEncoder.Create;
  try
    Result := lEncoder.Encode(aPixels, aWidth, aHeight);
  finally
    lEncoder.Free;
  end;
end;


end.
