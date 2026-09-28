{
    The WebP lossy (VP8 key frame) bitstream decoder, with the YUV to RGB conversion and chroma
    upsampling of libwebp, and the decoder of ALPH alpha chunks.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwebpvp8;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, FpImage, FpImage.WebP.VP8L;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, FpImage, fpwebpvp8l;
{$ENDIF FPC_DOTTEDUNITS}

// Reads the size in the header of the VP8 key frame in aData; False when it is not one.
function VP8ReadInfo(aData: PByte; aSize: Integer; out aWidth, aHeight: Integer): Boolean;
// Decodes the VP8 key frame in aData to opaque pixels; raises EWebPError when it is invalid.
function VP8Decode(aData: PByte; aSize: Integer; out aWidth, aHeight: Integer): TWebPPixels;
// Replaces the alpha of aPixels, of aWidth x aHeight, by that of the ALPH chunk data in aData.
procedure WebPApplyAlpha(aData: PByte; aSize, aWidth, aHeight: Integer; var aPixels: TWebPPixels);

implementation

{$i fpwebpvp8tables.inc}

const
  Bands: array[0..16] of Byte = (0, 1, 2, 3, 6, 4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 0);
  Zigzag: array[0..15] of Byte = (0, 1, 4, 8, 5, 2, 3, 6, 9, 12, 13, 10, 7, 11, 14, 15);
  Cat3: array[0..2] of Byte = (173, 148, 140);
  Cat4: array[0..3] of Byte = (176, 155, 140, 135);
  Cat5: array[0..4] of Byte = (180, 157, 141, 134, 130);
  Cat6: array[0..10] of Byte = (254, 254, 243, 230, 196, 177, 153, 140, 133, 130, 129);

  // Modes of the 4x4 intra predictors, and of the 16x16 and chroma ones.
  BDC = 0; BTM = 1; BVE = 2; BHE = 3; BRD = 4; BVR = 5; BLD = 6; BVL = 7; BHD = 8; BHU = 9;
  DCPred = 0; TMPred = 1; VPred = 2; HPred = 3;

  // The tree of the 4x4 intra modes: positive entries are nodes, others minus a mode.
  YModesIntra4: array[0..17] of ShortInt =
    (-BDC, 1, -BTM, 2, -BVE, 3, 4, 6, -BHE, 5, -BRD, -BVR, -BLD, 7, -BVL, 8, -BHD, -BHU);

type
  { Reads a boolean-coded partition. }
  TVP8Bits = class
  private
    FData: PByte;
    FSize: Integer;
    FPos: Integer;
    FValue: LongWord;
    FRange: LongWord;
    FBitCount: Integer;
    FOver: Integer;
    function NextByte: LongWord;
  public
    constructor Create(aData: PByte; aSize: Integer);
    function GetBit(aProb: Integer): Integer;
    function GetValue(aBits: Integer): Integer;
    function GetSigned(aBits: Integer): Integer;
    // Whether more than the two bytes the decoder reads ahead were read past the end.
    function PastEnd: Boolean;
  end;

  TQuant = array[0..1] of Integer;

  TFilterInfo = record
    Limit, ILevel, Hev: Integer;
    Inner: Boolean;
  end;

  TNonZero = record
    Y: array[0..3] of Byte;
    U, V: array[0..1] of Byte;
    DC: Byte;
  end;

  TCoeffs = array[0..383] of Integer;

  TVP8Decoder = class
  private
    FData: PByte;
    FSize: Integer;
    FWidth, FHeight, FMBW, FMBH: Integer;
    FBits: TVP8Bits;
    FParts: array[0..7] of TVP8Bits;
    FPartCount: Integer;
    FUseSegment, FUpdateMap, FAbsolute: Boolean;
    FSegQuant, FSegFilter: array[0..3] of Integer;
    FSegProba: array[0..2] of Integer;
    FSimple: Boolean;
    FLevel, FSharpness, FFilterType: Integer;
    FUseLFDelta: Boolean;
    FRefDelta, FModeDelta: array[0..3] of Integer;
    FY1, FY2, FUV: array[0..3] of TQuant;
    FProba: array[0..3, 0..7, 0..2, 0..10] of Byte;
    FUseSkip: Boolean;
    FSkipProb: Integer;
    FStrength: array[0..3, 0..1] of TFilterInfo;
    FY, FU, FV: array of Byte;
    FYStride, FUVStride: Integer;
    FFilters: array of TFilterInfo;
    FIntraT: array of Byte;
    FIntraL: array[0..3] of Byte;
    FTopNz: array of TNonZero;
    FLeftNz: TNonZero;
    procedure ParseHeaders;
    procedure ComputeStrengths;
    function GetCoeffs(aBits: TVP8Bits; aType, aCtx: Integer; const aQuant: TQuant; aFirst: Integer;
      var aOut: TCoeffs; aOffset: Integer): Integer;
    function GetLargeValue(aBits: TVP8Bits; aType, aBand, aCtx: Integer): Integer;
    procedure DecodeMacroblock(aX, aY: Integer);
    procedure Predict16(aMode, aX, aY: Integer);
    procedure PredictChroma(var aPlane: array of Byte; aMode, aX, aY: Integer);
    procedure Predict4(aMode, aMBX, aMBY, aBX, aBY: Integer);
    procedure AddResidual(var aPlane: array of Byte; aStride, aX, aY: Integer; const aCoeffs: TCoeffs;
      aOffset: Integer);
    procedure FilterMacroblock(aX, aY: Integer);
    function ToPixels: TWebPPixels;
  public
    constructor Create(aData: PByte; aSize: Integer);
    destructor Destroy; override;
    function Decode(out aWidth, aHeight: Integer): TWebPPixels;
  end;

{ TVP8Bits }

constructor TVP8Bits.Create(aData: PByte; aSize: Integer);

begin
  inherited Create;
  FData := aData;
  FSize := aSize;
  FRange := 255;
  FValue := (NextByte shl 8) or NextByte;
end;


function TVP8Bits.NextByte: LongWord;

begin
  if FPos < FSize then
    Result := FData[FPos]
  else
    begin
    Result := 0;
    Inc(FOver);
    end;
  Inc(FPos);
end;


function TVP8Bits.GetBit(aProb: Integer): Integer;

var
  lSplit, lBigSplit: LongWord;

begin
  lSplit := 1 + (((FRange - 1) * LongWord(aProb)) shr 8);
  lBigSplit := lSplit shl 8;
  if FValue >= lBigSplit then
    begin
    Result := 1;
    Dec(FRange, lSplit);
    Dec(FValue, lBigSplit);
    end
  else
    begin
    Result := 0;
    FRange := lSplit;
    end;
  while FRange < 128 do
    begin
    FValue := FValue shl 1;
    FRange := FRange shl 1;
    Inc(FBitCount);
    if FBitCount = 8 then
      begin
      FBitCount := 0;
      FValue := FValue or NextByte;
      end;
    end;
end;


function TVP8Bits.GetValue(aBits: Integer): Integer;

begin
  Result := 0;
  while aBits > 0 do
    begin
    Result := (Result shl 1) or GetBit(128);
    Dec(aBits);
    end;
end;


function TVP8Bits.GetSigned(aBits: Integer): Integer;

begin
  Result := GetValue(aBits);
  if GetBit(128) = 1 then
    Result := -Result;
end;


function TVP8Bits.PastEnd: Boolean;

begin
  Result := FOver > 2;
end;


function Clip255(aValue: Integer): Byte;

begin
  if aValue < 0 then
    Result := 0
  else if aValue > 255 then
    Result := 255
  else
    Result := aValue;
end;


// Returns aValue wrapped to a signed 16-bit value.
function Wrap16(aValue: Int64): Integer;

begin
  Result := Integer((aValue + 32768) and $FFFF) - 32768;
end;


function Clamp(aValue, aMax: Integer): Integer;

begin
  if aValue < 0 then
    Result := 0
  else if aValue > aMax then
    Result := aMax
  else
    Result := aValue;
end;


{ TVP8Decoder }

constructor TVP8Decoder.Create(aData: PByte; aSize: Integer);

begin
  inherited Create;
  FData := aData;
  FSize := aSize;
end;


destructor TVP8Decoder.Destroy;

var
  i: Integer;

begin
  FBits.Free;
  for i := 0 to High(FParts) do
    FParts[i].Free;
  inherited Destroy;
end;


procedure TVP8Decoder.ParseHeaders;

var
  lTag: LongWord;
  lPartLength, lPos, lLeft, lSize, lBase, lQ, i, t, b, c, p: Integer;
  lDQY1DC, lDQY2DC, lDQY2AC, lDQUVDC, lDQUVAC: Integer;

begin
  if FSize < 10 then
    raise EWebPError.Create('VP8 data too short');
  lTag := LongWord(FData[0]) or (LongWord(FData[1]) shl 8) or (LongWord(FData[2]) shl 16);
  if (lTag and 1) <> 0 then
    raise EWebPError.Create('A VP8 frame of a WebP file is a key frame');
  if ((lTag shr 1) and 7) > 3 then
    raise EWebPError.Create('Unknown VP8 profile');
  lPartLength := lTag shr 5;
  if (FData[3] <> $9D) or (FData[4] <> $01) or (FData[5] <> $2A) then
    raise EWebPError.Create('Invalid VP8 start code');
  FWidth := (FData[6] or (FData[7] shl 8)) and $3FFF;
  FHeight := (FData[8] or (FData[9] shl 8)) and $3FFF;
  if (FWidth = 0) or (FHeight = 0) then
    raise EWebPError.Create('Invalid VP8 dimensions');
  if 10 + Int64(lPartLength) > FSize then
    raise EWebPError.Create('Truncated VP8 header');
  FMBW := (FWidth + 15) shr 4;
  FMBH := (FHeight + 15) shr 4;
  FBits := TVP8Bits.Create(FData + 10, lPartLength);
  FBits.GetValue(1);
  FBits.GetValue(1);
  FUseSegment := FBits.GetValue(1) = 1;
  FUpdateMap := False;
  FAbsolute := False;
  for i := 0 to 3 do
    begin
    FSegQuant[i] := 0;
    FSegFilter[i] := 0;
    end;
  for i := 0 to 2 do
    FSegProba[i] := 255;
  if FUseSegment then
    begin
    FUpdateMap := FBits.GetValue(1) = 1;
    if FBits.GetValue(1) = 1 then
      begin
      FAbsolute := FBits.GetValue(1) = 1;
      for i := 0 to 3 do
        if FBits.GetValue(1) = 1 then
          FSegQuant[i] := FBits.GetSigned(7);
      for i := 0 to 3 do
        if FBits.GetValue(1) = 1 then
          FSegFilter[i] := FBits.GetSigned(6);
      end;
    if FUpdateMap then
      for i := 0 to 2 do
        if FBits.GetValue(1) = 1 then
          FSegProba[i] := FBits.GetValue(8);
    end;
  FSimple := FBits.GetValue(1) = 1;
  FLevel := FBits.GetValue(6);
  FSharpness := FBits.GetValue(3);
  FUseLFDelta := FBits.GetValue(1) = 1;
  for i := 0 to 3 do
    begin
    FRefDelta[i] := 0;
    FModeDelta[i] := 0;
    end;
  if FUseLFDelta and (FBits.GetValue(1) = 1) then
    begin
    for i := 0 to 3 do
      if FBits.GetValue(1) = 1 then
        FRefDelta[i] := FBits.GetSigned(6);
    for i := 0 to 3 do
      if FBits.GetValue(1) = 1 then
        FModeDelta[i] := FBits.GetSigned(6);
    end;
  if FLevel = 0 then
    FFilterType := 0
  else if FSimple then
    FFilterType := 1
  else
    FFilterType := 2;
  FPartCount := 1 shl FBits.GetValue(2);
  lPos := 10 + lPartLength;
  lLeft := FSize - lPos - 3 * (FPartCount - 1);
  if lLeft < 0 then
    raise EWebPError.Create('Truncated VP8 partition sizes');
  for i := 0 to FPartCount - 2 do
    begin
    lSize := FData[lPos + 3 * i] or (FData[lPos + 3 * i + 1] shl 8) or (FData[lPos + 3 * i + 2] shl 16);
    if lSize > lLeft then
      lSize := lLeft;
    FParts[i] := TVP8Bits.Create(FData + FSize - lLeft, lSize);
    Dec(lLeft, lSize);
    end;
  if lLeft <= 0 then
    raise EWebPError.Create('Truncated VP8 data');
  FParts[FPartCount - 1] := TVP8Bits.Create(FData + FSize - lLeft, lLeft);
  lBase := FBits.GetValue(7);
  lDQY1DC := 0;
  lDQY2DC := 0;
  lDQY2AC := 0;
  lDQUVDC := 0;
  lDQUVAC := 0;
  if FBits.GetValue(1) = 1 then
    lDQY1DC := FBits.GetSigned(4);
  if FBits.GetValue(1) = 1 then
    lDQY2DC := FBits.GetSigned(4);
  if FBits.GetValue(1) = 1 then
    lDQY2AC := FBits.GetSigned(4);
  if FBits.GetValue(1) = 1 then
    lDQUVDC := FBits.GetSigned(4);
  if FBits.GetValue(1) = 1 then
    lDQUVAC := FBits.GetSigned(4);
  for i := 0 to 3 do
    begin
    if FUseSegment then
      begin
      lQ := FSegQuant[i];
      if not FAbsolute then
        Inc(lQ, lBase);
      end
    else
      lQ := lBase;
    FY1[i][0] := DcTable[Clamp(lQ + lDQY1DC, 127)];
    FY1[i][1] := AcTable[Clamp(lQ, 127)];
    FY2[i][0] := DcTable[Clamp(lQ + lDQY2DC, 127)] * 2;
    FY2[i][1] := (AcTable[Clamp(lQ + lDQY2AC, 127)] * 101581) shr 16;
    if FY2[i][1] < 8 then
      FY2[i][1] := 8;
    FUV[i][0] := DcTable[Clamp(lQ + lDQUVDC, 117)];
    FUV[i][1] := AcTable[Clamp(lQ + lDQUVAC, 127)];
    end;
  FBits.GetValue(1);
  for t := 0 to 3 do
    for b := 0 to 7 do
      for c := 0 to 2 do
        for p := 0 to 10 do
          if FBits.GetBit(CoeffsUpdateProba[t, b, c, p]) = 1 then
            FProba[t, b, c, p] := FBits.GetValue(8)
          else
            FProba[t, b, c, p] := CoeffsProba0[t, b, c, p];
  FUseSkip := FBits.GetValue(1) = 1;
  if FUseSkip then
    FSkipProb := FBits.GetValue(8);
end;


procedure TVP8Decoder.ComputeStrengths;

var
  s, lI4, lBase, lLevel, lILevel: Integer;

begin
  for s := 0 to 3 do
    begin
    if FUseSegment then
      begin
      lBase := FSegFilter[s];
      if not FAbsolute then
        Inc(lBase, FLevel);
      end
    else
      lBase := FLevel;
    for lI4 := 0 to 1 do
      with FStrength[s, lI4] do
        begin
        lLevel := lBase;
        if FUseLFDelta then
          begin
          Inc(lLevel, FRefDelta[0]);
          if lI4 = 1 then
            Inc(lLevel, FModeDelta[0]);
          end;
        lLevel := Clamp(lLevel, 63);
        if lLevel > 0 then
          begin
          lILevel := lLevel;
          if FSharpness > 0 then
            begin
            if FSharpness > 4 then
              lILevel := lILevel shr 2
            else
              lILevel := lILevel shr 1;
            if lILevel > 9 - FSharpness then
              lILevel := 9 - FSharpness;
            end;
          if lILevel < 1 then
            lILevel := 1;
          ILevel := lILevel;
          Limit := 2 * lLevel + lILevel;
          if lLevel >= 40 then
            Hev := 2
          else if lLevel >= 15 then
            Hev := 1
          else
            Hev := 0;
          end
        else
          Limit := 0;
        Inner := lI4 = 1;
        end;
    end;
end;


function TVP8Decoder.GetLargeValue(aBits: TVP8Bits; aType, aBand, aCtx: Integer): Integer;

var
  lBit1, lBit0, i: Integer;

begin
  if aBits.GetBit(FProba[aType, aBand, aCtx, 3]) = 0 then
    begin
    if aBits.GetBit(FProba[aType, aBand, aCtx, 4]) = 0 then
      Result := 2
    else
      Result := 3 + aBits.GetBit(FProba[aType, aBand, aCtx, 5]);
    end
  else if aBits.GetBit(FProba[aType, aBand, aCtx, 6]) = 0 then
    begin
    if aBits.GetBit(FProba[aType, aBand, aCtx, 7]) = 0 then
      Result := 5 + aBits.GetBit(159)
    else
      begin
      Result := 7 + 2 * aBits.GetBit(165);
      Inc(Result, aBits.GetBit(145));
      end;
    end
  else
    begin
    lBit1 := aBits.GetBit(FProba[aType, aBand, aCtx, 8]);
    lBit0 := aBits.GetBit(FProba[aType, aBand, aCtx, 9 + lBit1]);
    Result := 0;
    case 2 * lBit1 + lBit0 of
      0: for i := 0 to High(Cat3) do
           Result := Result + Result + aBits.GetBit(Cat3[i]);
      1: for i := 0 to High(Cat4) do
           Result := Result + Result + aBits.GetBit(Cat4[i]);
      2: for i := 0 to High(Cat5) do
           Result := Result + Result + aBits.GetBit(Cat5[i]);
    else
      for i := 0 to High(Cat6) do
        Result := Result + Result + aBits.GetBit(Cat6[i]);
    end;
    Inc(Result, 3 + (8 shl (2 * lBit1 + lBit0)));
    end;
end;


// Reads the tokens of one block from position aFirst; returns the position of the last non-zero one plus one.
function TVP8Decoder.GetCoeffs(aBits: TVP8Bits; aType, aCtx: Integer; const aQuant: TQuant; aFirst: Integer;
  var aOut: TCoeffs; aOffset: Integer): Integer;

var
  n, lCtx, lValue: Integer;

begin
  n := aFirst;
  lCtx := aCtx;
  while n < 16 do
    begin
    if aBits.GetBit(FProba[aType, Bands[n], lCtx, 0]) = 0 then
      exit(n);
    while aBits.GetBit(FProba[aType, Bands[n], lCtx, 1]) = 0 do
      begin
      Inc(n);
      if n = 16 then
        exit(16);
      lCtx := 0;
      end;
    if aBits.GetBit(FProba[aType, Bands[n], lCtx, 2]) = 0 then
      begin
      lValue := 1;
      lCtx := 1;
      end
    else
      begin
      lValue := GetLargeValue(aBits, aType, Bands[n], lCtx);
      lCtx := 2;
      end;
    if aBits.GetBit(128) = 1 then
      lValue := -lValue;
    if n > 0 then
      aOut[aOffset + Zigzag[n]] := Wrap16(Int64(lValue) * aQuant[1])
    else
      aOut[aOffset + Zigzag[n]] := Wrap16(Int64(lValue) * aQuant[0]);
    Inc(n);
    end;
  Result := 16;
end;


// Returns the 16x16 or chroma DC prediction of the edge sums, following which edges exist.
function EdgeDC(aTop, aLeft: Integer; aHasTop, aHasLeft: Boolean; aShift: Integer): Integer;

begin
  if aHasTop and aHasLeft then
    Result := (aTop + aLeft + (1 shl aShift)) shr (aShift + 1)
  else if aHasTop then
    Result := (aTop + (1 shl (aShift - 1))) shr aShift
  else if aHasLeft then
    Result := (aLeft + (1 shl (aShift - 1))) shr aShift
  else
    Result := $80;
end;


procedure TVP8Decoder.Predict16(aMode, aX, aY: Integer);

var
  lTop, lLeft: array[0..15] of Integer;
  lTL, lSumT, lSumL, lDC, x, y, x0, y0: Integer;

begin
  x0 := aX * 16;
  y0 := aY * 16;
  lSumT := 0;
  lSumL := 0;
  for x := 0 to 15 do
    begin
    if aY > 0 then
      lTop[x] := FY[(y0 - 1) * FYStride + x0 + x]
    else
      lTop[x] := 127;
    if aX > 0 then
      lLeft[x] := FY[(y0 + x) * FYStride + x0 - 1]
    else
      lLeft[x] := 129;
    Inc(lSumT, lTop[x]);
    Inc(lSumL, lLeft[x]);
    end;
  if aY = 0 then
    lTL := 127
  else if aX = 0 then
    lTL := 129
  else
    lTL := FY[(y0 - 1) * FYStride + x0 - 1];
  lDC := EdgeDC(lSumT, lSumL, aY > 0, aX > 0, 4);
  for y := 0 to 15 do
    for x := 0 to 15 do
      case aMode of
        TMPred: FY[(y0 + y) * FYStride + x0 + x] := Clip255(lLeft[y] + lTop[x] - lTL);
        VPred: FY[(y0 + y) * FYStride + x0 + x] := lTop[x];
        HPred: FY[(y0 + y) * FYStride + x0 + x] := lLeft[y];
      else
        FY[(y0 + y) * FYStride + x0 + x] := lDC;
      end;
end;


procedure TVP8Decoder.PredictChroma(var aPlane: array of Byte; aMode, aX, aY: Integer);

var
  lTop, lLeft: array[0..7] of Integer;
  lTL, lSumT, lSumL, lDC, x, y, x0, y0: Integer;

begin
  x0 := aX * 8;
  y0 := aY * 8;
  lSumT := 0;
  lSumL := 0;
  for x := 0 to 7 do
    begin
    if aY > 0 then
      lTop[x] := aPlane[(y0 - 1) * FUVStride + x0 + x]
    else
      lTop[x] := 127;
    if aX > 0 then
      lLeft[x] := aPlane[(y0 + x) * FUVStride + x0 - 1]
    else
      lLeft[x] := 129;
    Inc(lSumT, lTop[x]);
    Inc(lSumL, lLeft[x]);
    end;
  if aY = 0 then
    lTL := 127
  else if aX = 0 then
    lTL := 129
  else
    lTL := aPlane[(y0 - 1) * FUVStride + x0 - 1];
  lDC := EdgeDC(lSumT, lSumL, aY > 0, aX > 0, 3);
  for y := 0 to 7 do
    for x := 0 to 7 do
      case aMode of
        TMPred: aPlane[(y0 + y) * FUVStride + x0 + x] := Clip255(lLeft[y] + lTop[x] - lTL);
        VPred: aPlane[(y0 + y) * FUVStride + x0 + x] := lTop[x];
        HPred: aPlane[(y0 + y) * FUVStride + x0 + x] := lLeft[y];
      else
        aPlane[(y0 + y) * FUVStride + x0 + x] := lDC;
      end;
end;


function Avg3(a, b, c: Integer): Integer;

begin
  Result := (a + 2 * b + c + 2) shr 2;
end;


function Avg2(a, b: Integer): Integer;

begin
  Result := (a + b + 1) shr 1;
end;


// Predicts sub-block aBX, aBY of macroblock aMBX, aMBY with 4x4 mode aMode.
procedure TVP8Decoder.Predict4(aMode, aMBX, aMBY, aBX, aBY: Integer);

var
  T: array[0..7] of Integer;
  L: array[0..3] of Integer;
  B: array[0..15] of Integer;
  X, sx, sy, x0, y0, i, lDC: Integer;

  // Returns the pixel above the sub-block in column ax of the plane, following the edges of the macroblock.
  function Above(ax: Integer): Integer;

  begin
    if aBY > 0 then
      Result := FY[(sy - 1) * FYStride + ax]
    else if aMBY = 0 then
      Result := 127
    else
      Result := FY[(y0 - 1) * FYStride + ax];
  end;

  // Returns the pixel left of the sub-block in row ay of the plane, following the edges of the macroblock.
  function Left(ay: Integer): Integer;

  begin
    if aBX > 0 then
      Result := FY[ay * FYStride + sx - 1]
    else if aMBX = 0 then
      Result := 129
    else
      Result := FY[ay * FYStride + x0 - 1];
  end;

  procedure Put(ax, ay, aValue: Integer);

  begin
    B[ax + 4 * ay] := aValue;
  end;

begin
  x0 := aMBX * 16;
  y0 := aMBY * 16;
  sx := x0 + 4 * aBX;
  sy := y0 + 4 * aBY;
  for i := 0 to 3 do
    begin
    T[i] := Above(sx + i);
    L[i] := Left(sy + i);
    end;
  if aBX < 3 then
    for i := 4 to 7 do
      T[i] := Above(sx + i)
  else if aMBY = 0 then
    for i := 4 to 7 do
      T[i] := 127
  else if aMBX = FMBW - 1 then
    for i := 4 to 7 do
      T[i] := FY[(y0 - 1) * FYStride + x0 + 15]
  else
    for i := 4 to 7 do
      T[i] := FY[(y0 - 1) * FYStride + x0 + 12 + i];
  if (aBX > 0) and (aBY > 0) then
    X := FY[(sy - 1) * FYStride + sx - 1]
  else if aBY > 0 then
    X := Left(sy - 1)
  else if aBX > 0 then
    X := Above(sx - 1)
  else if aMBY = 0 then
    X := 127
  else if aMBX = 0 then
    X := 129
  else
    X := FY[(y0 - 1) * FYStride + x0 - 1];
  case aMode of
    BVE:
      for i := 0 to 15 do
        if i mod 4 = 0 then
          B[i] := Avg3(X, T[0], T[1])
        else
          B[i] := Avg3(T[i mod 4 - 1], T[i mod 4], T[i mod 4 + 1]);
    BHE:
      for i := 0 to 3 do
        begin
        case i of
          0: lDC := Avg3(X, L[0], L[1]);
          1: lDC := Avg3(L[0], L[1], L[2]);
          2: lDC := Avg3(L[1], L[2], L[3]);
        else
          lDC := Avg3(L[2], L[3], L[3]);
        end;
        Put(0, i, lDC);
        Put(1, i, lDC);
        Put(2, i, lDC);
        Put(3, i, lDC);
        end;
    BTM:
      for i := 0 to 15 do
        B[i] := Clip255(L[i div 4] + T[i mod 4] - X);
    BRD:
      begin
      Put(0, 3, Avg3(L[1], L[2], L[3]));
      lDC := Avg3(L[0], L[1], L[2]);
      Put(1, 3, lDC); Put(0, 2, lDC);
      lDC := Avg3(X, L[0], L[1]);
      Put(2, 3, lDC); Put(1, 2, lDC); Put(0, 1, lDC);
      lDC := Avg3(T[0], X, L[0]);
      Put(3, 3, lDC); Put(2, 2, lDC); Put(1, 1, lDC); Put(0, 0, lDC);
      lDC := Avg3(T[1], T[0], X);
      Put(3, 2, lDC); Put(2, 1, lDC); Put(1, 0, lDC);
      lDC := Avg3(T[2], T[1], T[0]);
      Put(3, 1, lDC); Put(2, 0, lDC);
      Put(3, 0, Avg3(T[3], T[2], T[1]));
      end;
    BLD:
      begin
      Put(0, 0, Avg3(T[0], T[1], T[2]));
      lDC := Avg3(T[1], T[2], T[3]);
      Put(1, 0, lDC); Put(0, 1, lDC);
      lDC := Avg3(T[2], T[3], T[4]);
      Put(2, 0, lDC); Put(1, 1, lDC); Put(0, 2, lDC);
      lDC := Avg3(T[3], T[4], T[5]);
      Put(3, 0, lDC); Put(2, 1, lDC); Put(1, 2, lDC); Put(0, 3, lDC);
      lDC := Avg3(T[4], T[5], T[6]);
      Put(3, 1, lDC); Put(2, 2, lDC); Put(1, 3, lDC);
      lDC := Avg3(T[5], T[6], T[7]);
      Put(3, 2, lDC); Put(2, 3, lDC);
      Put(3, 3, Avg3(T[6], T[7], T[7]));
      end;
    BVR:
      begin
      lDC := Avg2(X, T[0]);
      Put(0, 0, lDC); Put(1, 2, lDC);
      lDC := Avg2(T[0], T[1]);
      Put(1, 0, lDC); Put(2, 2, lDC);
      lDC := Avg2(T[1], T[2]);
      Put(2, 0, lDC); Put(3, 2, lDC);
      Put(3, 0, Avg2(T[2], T[3]));
      Put(0, 3, Avg3(L[2], L[1], L[0]));
      Put(0, 2, Avg3(L[1], L[0], X));
      lDC := Avg3(L[0], X, T[0]);
      Put(0, 1, lDC); Put(1, 3, lDC);
      lDC := Avg3(X, T[0], T[1]);
      Put(1, 1, lDC); Put(2, 3, lDC);
      lDC := Avg3(T[0], T[1], T[2]);
      Put(2, 1, lDC); Put(3, 3, lDC);
      Put(3, 1, Avg3(T[1], T[2], T[3]));
      end;
    BVL:
      begin
      Put(0, 0, Avg2(T[0], T[1]));
      lDC := Avg2(T[1], T[2]);
      Put(1, 0, lDC); Put(0, 2, lDC);
      lDC := Avg2(T[2], T[3]);
      Put(2, 0, lDC); Put(1, 2, lDC);
      lDC := Avg2(T[3], T[4]);
      Put(3, 0, lDC); Put(2, 2, lDC);
      Put(0, 1, Avg3(T[0], T[1], T[2]));
      lDC := Avg3(T[1], T[2], T[3]);
      Put(1, 1, lDC); Put(0, 3, lDC);
      lDC := Avg3(T[2], T[3], T[4]);
      Put(2, 1, lDC); Put(1, 3, lDC);
      lDC := Avg3(T[3], T[4], T[5]);
      Put(3, 1, lDC); Put(2, 3, lDC);
      Put(3, 2, Avg3(T[4], T[5], T[6]));
      Put(3, 3, Avg3(T[5], T[6], T[7]));
      end;
    BHU:
      begin
      Put(0, 0, Avg2(L[0], L[1]));
      lDC := Avg2(L[1], L[2]);
      Put(2, 0, lDC); Put(0, 1, lDC);
      lDC := Avg2(L[2], L[3]);
      Put(2, 1, lDC); Put(0, 2, lDC);
      Put(1, 0, Avg3(L[0], L[1], L[2]));
      lDC := Avg3(L[1], L[2], L[3]);
      Put(3, 0, lDC); Put(1, 1, lDC);
      lDC := Avg3(L[2], L[3], L[3]);
      Put(3, 1, lDC); Put(1, 2, lDC);
      Put(3, 2, L[3]); Put(2, 2, L[3]);
      Put(0, 3, L[3]); Put(1, 3, L[3]); Put(2, 3, L[3]); Put(3, 3, L[3]);
      end;
    BHD:
      begin
      lDC := Avg2(L[0], X);
      Put(0, 0, lDC); Put(2, 1, lDC);
      lDC := Avg2(L[1], L[0]);
      Put(0, 1, lDC); Put(2, 2, lDC);
      lDC := Avg2(L[2], L[1]);
      Put(0, 2, lDC); Put(2, 3, lDC);
      Put(0, 3, Avg2(L[3], L[2]));
      Put(3, 0, Avg3(T[0], T[1], T[2]));
      Put(2, 0, Avg3(X, T[0], T[1]));
      lDC := Avg3(L[0], X, T[0]);
      Put(1, 0, lDC); Put(3, 1, lDC);
      lDC := Avg3(L[1], L[0], X);
      Put(1, 1, lDC); Put(3, 2, lDC);
      lDC := Avg3(L[2], L[1], L[0]);
      Put(1, 2, lDC); Put(3, 3, lDC);
      Put(1, 3, Avg3(L[3], L[2], L[1]));
      end;
  else
    begin
    lDC := 4;
    for i := 0 to 3 do
      Inc(lDC, T[i] + L[i]);
    lDC := lDC shr 3;
    for i := 0 to 15 do
      B[i] := lDC;
    end;
  end;
  for i := 0 to 15 do
    FY[(sy + i div 4) * FYStride + sx + i mod 4] := B[i];
end;


function Mul1(a: Integer): Int64;

begin
  Result := SarInt64(Int64(a) * 20091, 16) + a;
end;


function Mul2(a: Integer): Int64;

begin
  Result := SarInt64(Int64(a) * 35468, 16);
end;


// Adds the inverse transform of the 16 coefficients at aOffset to the 4x4 pixels at aX, aY.
procedure TVP8Decoder.AddResidual(var aPlane: array of Byte; aStride, aX, aY: Integer; const aCoeffs: TCoeffs;
  aOffset: Integer);

var
  C: array[0..15] of Int64;
  i, lPos: Integer;
  a, b, cc, d, dc: Int64;
  lZero: Boolean;

  procedure Store(aColumn, aRow: Integer; aValue: Int64);

  begin
    lPos := (aY + aRow) * aStride + aX + aColumn;
    aPlane[lPos] := Clip255(aPlane[lPos] + SarInt64(aValue, 3));
  end;

begin
  lZero := True;
  for i := 0 to 15 do
    if aCoeffs[aOffset + i] <> 0 then
      lZero := False;
  if lZero then
    exit;
  for i := 0 to 3 do
    begin
    a := aCoeffs[aOffset + i] + aCoeffs[aOffset + 8 + i];
    b := aCoeffs[aOffset + i] - aCoeffs[aOffset + 8 + i];
    cc := Mul2(aCoeffs[aOffset + 4 + i]) - Mul1(aCoeffs[aOffset + 12 + i]);
    d := Mul1(aCoeffs[aOffset + 4 + i]) + Mul2(aCoeffs[aOffset + 12 + i]);
    C[i * 4 + 0] := a + d;
    C[i * 4 + 1] := b + cc;
    C[i * 4 + 2] := b - cc;
    C[i * 4 + 3] := a - d;
    end;
  for i := 0 to 3 do
    begin
    dc := C[i] + 4;
    a := dc + C[8 + i];
    b := dc - C[8 + i];
    cc := Mul2(C[4 + i]) - Mul1(C[12 + i]);
    d := Mul1(C[4 + i]) + Mul2(C[12 + i]);
    Store(0, i, a + d);
    Store(1, i, b + cc);
    Store(2, i, b - cc);
    Store(3, i, a - d);
    end;
end;


procedure TVP8Decoder.DecodeMacroblock(aX, aY: Integer);

var
  lBits: TVP8Bits;
  lSegment, lMode, lUVMode, i, x, y, lNz, lCtx, lFirst, lType: Integer;
  lSkip, lI4, lAnyNonZero: Boolean;
  lModes: array[0..15] of Byte;
  lTop: array[0..3] of Byte;
  lCoeffs, lDC: TCoeffs;
  lTmp: array[0..15] of Integer;
  a0, a1, a2, a3, dc: Integer;
  lInfo: TFilterInfo;

begin
  lBits := FBits;
  if FUpdateMap then
    begin
    if lBits.GetBit(FSegProba[0]) = 0 then
      lSegment := lBits.GetBit(FSegProba[1])
    else
      lSegment := lBits.GetBit(FSegProba[2]) + 2;
    end
  else
    lSegment := 0;
  lSkip := FUseSkip and (lBits.GetBit(FSkipProb) = 1);
  lI4 := lBits.GetBit(145) = 0;
  for i := 0 to 3 do
    lTop[i] := FIntraT[aX * 4 + i];
  if not lI4 then
    begin
    if lBits.GetBit(156) = 1 then
      begin
      if lBits.GetBit(128) = 1 then
        lMode := TMPred
      else
        lMode := HPred;
      end
    else if lBits.GetBit(163) = 1 then
      lMode := VPred
    else
      lMode := DCPred;
    for i := 0 to 3 do
      begin
      FIntraT[aX * 4 + i] := lMode;
      FIntraL[i] := lMode;
      end;
    end
  else
    begin
    lMode := 0;
    for y := 0 to 3 do
      begin
      lMode := FIntraL[y];
      for x := 0 to 3 do
        begin
        i := YModesIntra4[lBits.GetBit(BModesProba[lTop[x], lMode, 0])];
        while i > 0 do
          i := YModesIntra4[2 * i + lBits.GetBit(BModesProba[lTop[x], lMode, i])];
        lMode := -i;
        lTop[x] := lMode;
        lModes[y * 4 + x] := lMode;
        end;
      FIntraL[y] := lMode;
      end;
    for i := 0 to 3 do
      FIntraT[aX * 4 + i] := lTop[i];
    end;
  if lBits.GetBit(142) = 0 then
    lUVMode := DCPred
  else if lBits.GetBit(114) = 0 then
    lUVMode := VPred
  else if lBits.GetBit(183) = 1 then
    lUVMode := TMPred
  else
    lUVMode := HPred;
  FillChar(lCoeffs, SizeOf(lCoeffs), 0);
  lAnyNonZero := False;
  lBits := FParts[aY and (FPartCount - 1)];
  if not lSkip then
    begin
    if not lI4 then
      begin
      FillChar(lDC, SizeOf(lDC), 0);
      lCtx := FTopNz[aX].DC + FLeftNz.DC;
      lNz := GetCoeffs(lBits, 1, lCtx, FY2[lSegment], 0, lDC, 0);
      FTopNz[aX].DC := Ord(lNz > 0);
      FLeftNz.DC := Ord(lNz > 0);
      for i := 0 to 3 do
        begin
        a0 := lDC[i] + lDC[12 + i];
        a1 := lDC[4 + i] + lDC[8 + i];
        a2 := lDC[4 + i] - lDC[8 + i];
        a3 := lDC[i] - lDC[12 + i];
        lTmp[i] := a0 + a1;
        lTmp[8 + i] := a0 - a1;
        lTmp[4 + i] := a3 + a2;
        lTmp[12 + i] := a3 - a2;
        end;
      for i := 0 to 3 do
        begin
        dc := lTmp[i * 4] + 3;
        a0 := dc + lTmp[i * 4 + 3];
        a1 := lTmp[i * 4 + 1] + lTmp[i * 4 + 2];
        a2 := lTmp[i * 4 + 1] - lTmp[i * 4 + 2];
        a3 := dc - lTmp[i * 4 + 3];
        lCoeffs[i * 64] := Wrap16(SarLongint(a0 + a1, 3));
        lCoeffs[i * 64 + 16] := Wrap16(SarLongint(a3 + a2, 3));
        lCoeffs[i * 64 + 32] := Wrap16(SarLongint(a0 - a1, 3));
        lCoeffs[i * 64 + 48] := Wrap16(SarLongint(a3 - a2, 3));
        end;
      lFirst := 1;
      lType := 0;
      end
    else
      begin
      lFirst := 0;
      lType := 3;
      end;
    for y := 0 to 3 do
      for x := 0 to 3 do
        begin
        lCtx := FLeftNz.Y[y] + FTopNz[aX].Y[x];
        lNz := GetCoeffs(lBits, lType, lCtx, FY1[lSegment], lFirst, lCoeffs, (y * 4 + x) * 16);
        FLeftNz.Y[y] := Ord(lNz > lFirst);
        FTopNz[aX].Y[x] := Ord(lNz > lFirst);
        end;
    for y := 0 to 1 do
      for x := 0 to 1 do
        begin
        lCtx := FLeftNz.U[y] + FTopNz[aX].U[x];
        lNz := GetCoeffs(lBits, 2, lCtx, FUV[lSegment], 0, lCoeffs, 256 + (y * 2 + x) * 16);
        FLeftNz.U[y] := Ord(lNz > 0);
        FTopNz[aX].U[x] := Ord(lNz > 0);
        end;
    for y := 0 to 1 do
      for x := 0 to 1 do
        begin
        lCtx := FLeftNz.V[y] + FTopNz[aX].V[x];
        lNz := GetCoeffs(lBits, 2, lCtx, FUV[lSegment], 0, lCoeffs, 320 + (y * 2 + x) * 16);
        FLeftNz.V[y] := Ord(lNz > 0);
        FTopNz[aX].V[x] := Ord(lNz > 0);
        end;
    for i := 0 to 383 do
      if lCoeffs[i] <> 0 then
        begin
        lAnyNonZero := True;
        break;
        end;
    end
  else
    begin
    for i := 0 to 3 do
      begin
      FLeftNz.Y[i] := 0;
      FTopNz[aX].Y[i] := 0;
      end;
    for i := 0 to 1 do
      begin
      FLeftNz.U[i] := 0;
      FLeftNz.V[i] := 0;
      FTopNz[aX].U[i] := 0;
      FTopNz[aX].V[i] := 0;
      end;
    if not lI4 then
      begin
      FLeftNz.DC := 0;
      FTopNz[aX].DC := 0;
      end;
    end;
  if lI4 then
    for i := 0 to 15 do
      begin
      Predict4(lModes[i], aX, aY, i mod 4, i div 4);
      AddResidual(FY, FYStride, aX * 16 + (i mod 4) * 4, aY * 16 + (i div 4) * 4, lCoeffs, i * 16);
      end
  else
    begin
    Predict16(lMode, aX, aY);
    for i := 0 to 15 do
      AddResidual(FY, FYStride, aX * 16 + (i mod 4) * 4, aY * 16 + (i div 4) * 4, lCoeffs, i * 16);
    end;
  PredictChroma(FU, lUVMode, aX, aY);
  PredictChroma(FV, lUVMode, aX, aY);
  for i := 0 to 3 do
    begin
    AddResidual(FU, FUVStride, aX * 8 + (i mod 2) * 4, aY * 8 + (i div 2) * 4, lCoeffs, 256 + i * 16);
    AddResidual(FV, FUVStride, aX * 8 + (i mod 2) * 4, aY * 8 + (i div 2) * 4, lCoeffs, 320 + i * 16);
    end;
  if FFilterType > 0 then
    begin
    lInfo := FStrength[lSegment, Ord(lI4)];
    lInfo.Inner := lInfo.Inner or lAnyNonZero;
    FFilters[aY * FMBW + aX] := lInfo;
    end;
end;


{ Loop filter }

function SClip1(aValue: Integer): Integer;

begin
  if aValue < -128 then
    Result := -128
  else if aValue > 127 then
    Result := 127
  else
    Result := aValue;
end;


function SClip2(aValue: Integer): Integer;

begin
  if aValue < -16 then
    Result := -16
  else if aValue > 15 then
    Result := 15
  else
    Result := aValue;
end;


// Filters the two pixels on either side of the edge before position aPos.
procedure DoFilter2(var P: array of Byte; aPos, aStep: Integer);

var
  p1, p0, q0, q1, a, a1, a2: Integer;

begin
  p1 := P[aPos - 2 * aStep];
  p0 := P[aPos - aStep];
  q0 := P[aPos];
  q1 := P[aPos + aStep];
  a := 3 * (q0 - p0) + SClip1(p1 - q1);
  a1 := SClip2(SarLongint(a + 4, 3));
  a2 := SClip2(SarLongint(a + 3, 3));
  P[aPos - aStep] := Clip255(p0 + a2);
  P[aPos] := Clip255(q0 - a1);
end;


procedure DoFilter4(var P: array of Byte; aPos, aStep: Integer);

var
  p1, p0, q0, q1, a, a1, a2, a3: Integer;

begin
  p1 := P[aPos - 2 * aStep];
  p0 := P[aPos - aStep];
  q0 := P[aPos];
  q1 := P[aPos + aStep];
  a := 3 * (q0 - p0);
  a1 := SClip2(SarLongint(a + 4, 3));
  a2 := SClip2(SarLongint(a + 3, 3));
  a3 := SarLongint(a1 + 1, 1);
  P[aPos - 2 * aStep] := Clip255(p1 + a3);
  P[aPos - aStep] := Clip255(p0 + a2);
  P[aPos] := Clip255(q0 - a1);
  P[aPos + aStep] := Clip255(q1 - a3);
end;


procedure DoFilter6(var P: array of Byte; aPos, aStep: Integer);

var
  p2, p1, p0, q0, q1, q2, a, a1, a2, a3: Integer;

begin
  p2 := P[aPos - 3 * aStep];
  p1 := P[aPos - 2 * aStep];
  p0 := P[aPos - aStep];
  q0 := P[aPos];
  q1 := P[aPos + aStep];
  q2 := P[aPos + 2 * aStep];
  a := SClip1(3 * (q0 - p0) + SClip1(p1 - q1));
  a1 := SarLongint(27 * a + 63, 7);
  a2 := SarLongint(18 * a + 63, 7);
  a3 := SarLongint(9 * a + 63, 7);
  P[aPos - 3 * aStep] := Clip255(p2 + a3);
  P[aPos - 2 * aStep] := Clip255(p1 + a2);
  P[aPos - aStep] := Clip255(p0 + a1);
  P[aPos] := Clip255(q0 - a1);
  P[aPos + aStep] := Clip255(q1 - a2);
  P[aPos + 2 * aStep] := Clip255(q2 - a3);
end;


function Hev(const P: array of Byte; aPos, aStep, aThresh: Integer): Boolean;

begin
  Result := (Abs(Integer(P[aPos - 2 * aStep]) - P[aPos - aStep]) > aThresh)
    or (Abs(Integer(P[aPos + aStep]) - P[aPos]) > aThresh);
end;


function NeedsFilter(const P: array of Byte; aPos, aStep, aThresh: Integer): Boolean;

begin
  Result := 4 * Abs(Integer(P[aPos - aStep]) - P[aPos]) + Abs(Integer(P[aPos - 2 * aStep]) - P[aPos + aStep])
    <= aThresh;
end;


function NeedsFilter2(const P: array of Byte; aPos, aStep, aThresh, aInner: Integer): Boolean;

var
  p3, p2, p1, p0, q0, q1, q2, q3: Integer;

begin
  p3 := P[aPos - 4 * aStep];
  p2 := P[aPos - 3 * aStep];
  p1 := P[aPos - 2 * aStep];
  p0 := P[aPos - aStep];
  q0 := P[aPos];
  q1 := P[aPos + aStep];
  q2 := P[aPos + 2 * aStep];
  q3 := P[aPos + 3 * aStep];
  if 4 * Abs(p0 - q0) + Abs(p1 - q1) > aThresh then
    exit(False);
  Result := (Abs(p3 - p2) <= aInner) and (Abs(p2 - p1) <= aInner) and (Abs(p1 - p0) <= aInner)
    and (Abs(q3 - q2) <= aInner) and (Abs(q2 - q1) <= aInner) and (Abs(q1 - q0) <= aInner);
end;


// Filters aCount pixels along an edge: aStep crosses it, aNext moves along it.
procedure FilterLoop(var P: array of Byte; aPos, aStep, aNext, aCount, aThresh, aInner, aHev: Integer;
  aSix: Boolean);

var
  lThresh2: Integer;

begin
  lThresh2 := 2 * aThresh + 1;
  while aCount > 0 do
    begin
    if NeedsFilter2(P, aPos, aStep, lThresh2, aInner) then
      begin
      if Hev(P, aPos, aStep, aHev) then
        DoFilter2(P, aPos, aStep)
      else if aSix then
        DoFilter6(P, aPos, aStep)
      else
        DoFilter4(P, aPos, aStep);
      end;
    Inc(aPos, aNext);
    Dec(aCount);
    end;
end;


procedure SimpleLoop(var P: array of Byte; aPos, aStep, aNext, aThresh: Integer);

var
  lThresh2, i: Integer;

begin
  lThresh2 := 2 * aThresh + 1;
  for i := 0 to 15 do
    begin
    if NeedsFilter(P, aPos, aStep, lThresh2) then
      DoFilter2(P, aPos, aStep);
    Inc(aPos, aNext);
    end;
end;


procedure TVP8Decoder.FilterMacroblock(aX, aY: Integer);

var
  lInfo: TFilterInfo;
  lY, lUV, k: Integer;

begin
  lInfo := FFilters[aY * FMBW + aX];
  if lInfo.Limit = 0 then
    exit;
  lY := aY * 16 * FYStride + aX * 16;
  lUV := aY * 8 * FUVStride + aX * 8;
  if FFilterType = 1 then
    begin
    if aX > 0 then
      SimpleLoop(FY, lY, 1, FYStride, lInfo.Limit + 4);
    if lInfo.Inner then
      for k := 1 to 3 do
        SimpleLoop(FY, lY + 4 * k, 1, FYStride, lInfo.Limit);
    if aY > 0 then
      SimpleLoop(FY, lY, FYStride, 1, lInfo.Limit + 4);
    if lInfo.Inner then
      for k := 1 to 3 do
        SimpleLoop(FY, lY + 4 * k * FYStride, FYStride, 1, lInfo.Limit);
    end
  else
    with lInfo do
      begin
      if aX > 0 then
        begin
        FilterLoop(FY, lY, 1, FYStride, 16, Limit + 4, ILevel, Hev, True);
        FilterLoop(FU, lUV, 1, FUVStride, 8, Limit + 4, ILevel, Hev, True);
        FilterLoop(FV, lUV, 1, FUVStride, 8, Limit + 4, ILevel, Hev, True);
        end;
      if Inner then
        begin
        for k := 1 to 3 do
          FilterLoop(FY, lY + 4 * k, 1, FYStride, 16, Limit, ILevel, Hev, False);
        FilterLoop(FU, lUV + 4, 1, FUVStride, 8, Limit, ILevel, Hev, False);
        FilterLoop(FV, lUV + 4, 1, FUVStride, 8, Limit, ILevel, Hev, False);
        end;
      if aY > 0 then
        begin
        FilterLoop(FY, lY, FYStride, 1, 16, Limit + 4, ILevel, Hev, True);
        FilterLoop(FU, lUV, FUVStride, 1, 8, Limit + 4, ILevel, Hev, True);
        FilterLoop(FV, lUV, FUVStride, 1, 8, Limit + 4, ILevel, Hev, True);
        end;
      if Inner then
        begin
        for k := 1 to 3 do
          FilterLoop(FY, lY + 4 * k * FYStride, FYStride, 1, 16, Limit, ILevel, Hev, False);
        FilterLoop(FU, lUV + 4 * FUVStride, FUVStride, 1, 8, Limit, ILevel, Hev, False);
        FilterLoop(FV, lUV + 4 * FUVStride, FUVStride, 1, 8, Limit, ILevel, Hev, False);
        end;
      end;
end;


{ YUV to RGB }

function Clip8(aValue: Integer): LongWord;

begin
  if (aValue and not ((256 shl 6) - 1)) = 0 then
    Result := aValue shr 6
  else if aValue < 0 then
    Result := 0
  else
    Result := 255;
end;


function MultHi(aValue, aCoeff: Integer): Integer;

begin
  Result := (aValue * aCoeff) shr 8;
end;


function YUVToPixel(y, u, v: Integer): LongWord;

begin
  Result := $FF000000
    or (Clip8(MultHi(y, 19077) + MultHi(v, 26149) - 14234) shl 16)
    or (Clip8(MultHi(y, 19077) - MultHi(u, 6419) - MultHi(v, 13320) + 8708) shl 8)
    or Clip8(MultHi(y, 19077) + MultHi(u, 33050) - 17685);
end;


// Converts the planes, upsampling the chroma between the two nearest chroma rows as libwebp does.
function TVP8Decoder.ToPixels: TWebPPixels;

var
  lChromaH, lLast, y, x, lNear, lFar, lPos: Integer;
  lU, lV: array of Integer;

  // Upsamples the chroma row pair of plane aPlane into aOut, one value per pixel of the row.
  procedure Upsample(const aPlane: array of Byte; var aOut: array of Integer);

  var
    N, F: Integer;
    k, lAvg: Integer;
    tl, t, l, c: Integer;

  begin
    N := lNear * FUVStride;
    F := lFar * FUVStride;
    aOut[0] := (3 * aPlane[N] + aPlane[F] + 2) shr 2;
    for k := 1 to lLast do
      begin
      tl := aPlane[N + k - 1];
      t := aPlane[N + k];
      l := aPlane[F + k - 1];
      c := aPlane[F + k];
      lAvg := tl + t + l + c + 8;
      aOut[2 * k - 1] := (((lAvg + 2 * (t + l)) shr 3) + tl) shr 1;
      aOut[2 * k] := (((lAvg + 2 * (tl + c)) shr 3) + t) shr 1;
      end;
    if not Odd(FWidth) then
      aOut[FWidth - 1] := (3 * aPlane[N + lLast] + aPlane[F + lLast] + 2) shr 2;
  end;

begin
  Result := nil;
  SetLength(Result, FWidth * FHeight);
  lU := nil;
  lV := nil;
  SetLength(lU, FWidth + 1);
  SetLength(lV, FWidth + 1);
  lChromaH := (FHeight + 1) shr 1;
  lLast := (FWidth - 1) shr 1;
  for y := 0 to FHeight - 1 do
    begin
    if y = 0 then
      begin
      lNear := 0;
      lFar := 0;
      end
    else if Odd(y) then
      begin
      lNear := (y - 1) shr 1;
      lFar := (y + 1) shr 1;
      if lFar >= lChromaH then
        lFar := lNear;
      end
    else
      begin
      lNear := y shr 1;
      lFar := lNear - 1;
      end;
    Upsample(FU, lU);
    Upsample(FV, lV);
    lPos := y * FWidth;
    for x := 0 to FWidth - 1 do
      Result[lPos + x] := YUVToPixel(FY[y * FYStride + x], lU[x], lV[x]);
    end;
end;


function TVP8Decoder.Decode(out aWidth, aHeight: Integer): TWebPPixels;

var
  x, y, i: Integer;

begin
  ParseHeaders;
  ComputeStrengths;
  FYStride := FMBW * 16;
  FUVStride := FMBW * 8;
  FY := nil;
  FU := nil;
  FV := nil;
  SetLength(FY, FYStride * FMBH * 16);
  SetLength(FU, FUVStride * FMBH * 8);
  SetLength(FV, FUVStride * FMBH * 8);
  FFilters := nil;
  SetLength(FFilters, FMBW * FMBH);
  FIntraT := nil;
  SetLength(FIntraT, FMBW * 4);
  FTopNz := nil;
  SetLength(FTopNz, FMBW);
  for y := 0 to FMBH - 1 do
    begin
    for i := 0 to 3 do
      FIntraL[i] := BDC;
    FillChar(FLeftNz, SizeOf(FLeftNz), 0);
    for x := 0 to FMBW - 1 do
      DecodeMacroblock(x, y);
    if FBits.PastEnd then
      raise EWebPError.Create('Truncated VP8 first partition');
    if FParts[y and (FPartCount - 1)].PastEnd then
      raise EWebPError.Create('Truncated VP8 token partition');
    end;
  if FFilterType > 0 then
    for y := 0 to FMBH - 1 do
      for x := 0 to FMBW - 1 do
        FilterMacroblock(x, y);
  aWidth := FWidth;
  aHeight := FHeight;
  Result := ToPixels;
end;


function VP8ReadInfo(aData: PByte; aSize: Integer; out aWidth, aHeight: Integer): Boolean;

begin
  Result := (aSize >= 10) and ((aData[0] and 1) = 0) and (aData[3] = $9D) and (aData[4] = $01) and (aData[5] = $2A);
  if not Result then
    exit;
  aWidth := (aData[6] or (aData[7] shl 8)) and $3FFF;
  aHeight := (aData[8] or (aData[9] shl 8)) and $3FFF;
  Result := (aWidth > 0) and (aHeight > 0);
end;


function VP8Decode(aData: PByte; aSize: Integer; out aWidth, aHeight: Integer): TWebPPixels;

var
  lDecoder: TVP8Decoder;

begin
  lDecoder := TVP8Decoder.Create(aData, aSize);
  try
    Result := lDecoder.Decode(aWidth, aHeight);
  finally
    lDecoder.Free;
  end;
end;


procedure WebPApplyAlpha(aData: PByte; aSize, aWidth, aHeight: Integer; var aPixels: TWebPPixels);

var
  lMethod, lFilter, lPre, x, y, lPos, lPred, lCount: Integer;
  lAlpha: array of Byte;
  lPlane: TWebPPixels;

begin
  if aSize < 1 then
    raise EWebPError.Create('Empty ALPH chunk');
  lMethod := aData[0] and 3;
  lFilter := (aData[0] shr 2) and 3;
  lPre := (aData[0] shr 4) and 3;
  if (lMethod > 1) or (lPre > 1) or ((aData[0] shr 6) <> 0) then
    raise EWebPError.Create('Invalid ALPH chunk');
  lCount := aWidth * aHeight;
  lAlpha := nil;
  SetLength(lAlpha, lCount);
  if lMethod = 0 then
    begin
    if aSize - 1 < lCount then
      raise EWebPError.Create('Truncated ALPH chunk');
    Move(aData[1], lAlpha[0], lCount);
    end
  else
    begin
    lPlane := VP8LDecodeImageStream(aData + 1, aSize - 1, aWidth, aHeight);
    for x := 0 to lCount - 1 do
      lAlpha[x] := (lPlane[x] shr 8) and $FF;
    end;
  if lFilter > 0 then
    for y := 0 to aHeight - 1 do
      for x := 0 to aWidth - 1 do
        begin
        lPos := y * aWidth + x;
        if (x = 0) and (y = 0) then
          continue;
        if y = 0 then
          lPred := lAlpha[lPos - 1]
        else if x = 0 then
          lPred := lAlpha[lPos - aWidth]
        else
          case lFilter of
            1: lPred := lAlpha[lPos - 1];
            2: lPred := lAlpha[lPos - aWidth];
          else
            lPred := Clip255(Integer(lAlpha[lPos - 1]) + Integer(lAlpha[lPos - aWidth])
              - Integer(lAlpha[lPos - aWidth - 1]));
          end;
        lAlpha[lPos] := (lAlpha[lPos] + lPred) and $FF;
        end;
  for x := 0 to lCount - 1 do
    aPixels[x] := (aPixels[x] and $00FFFFFF) or (LongWord(lAlpha[x]) shl 24);
end;


end.
