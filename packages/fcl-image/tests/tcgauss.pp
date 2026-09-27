{
    Tests for the Gaussian blur routines of fpimggauss: the matrices, the
    matrix blurs checked against a reference of the test, and the binomial blur.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcgauss;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, math, types, fpcunit, testregistry, fpimage, fpimggauss, fpimgtests;

type
  TTestGaussMatrix = class(TTestCase)
  private
    // Checks the sum, the symmetry and the Gaussian shape of the 1D matrix of aRadius.
    procedure CheckMatrix1D(aRadius: Integer);
    // Checks the sum, the symmetry and the Gaussian shape of the 2D matrix of aRadius.
    procedure CheckMatrix2D(aRadius: Integer);
  published
    procedure TestMatrix1D;
    procedure TestMatrix1DOfRadiusOne;
    procedure TestMatrix2D;
    procedure TestMatrix2DOfRadiusOne;
  end;

  TTestGaussianBlur = class(TTestCase)
  private
    // Blurs aImage in aArea with GaussianBlur, or with MatrixBlur2D if a2D.
    procedure Blur(aImage: TFPCustomImage; aRadius: Integer; const aArea: TRect; a2D: Boolean);
    // A copy of aImage with aArea blurred by the reference of the test for GaussianBlur or MatrixBlur2D.
    function Reference(aImage: TFPCustomImage; aRadius: Integer; const aArea: TRect; a2D: Boolean): TFPMemoryImage;
    // Checks that a constant image stays constant, for one routine.
    procedure CheckConstant(a2D: Boolean);
    // Checks the blur of a single white pixel against the matrix, for one routine.
    procedure CheckImpulse(a2D: Boolean);
    // Checks that mirroring the image mirrors the blur, for one routine.
    procedure CheckMirror(a2D: Boolean);
    // Checks that the sum of an impulse is kept, for one routine.
    procedure CheckEnergy(a2D: Boolean);
    // Checks a blur of a part of the image against the reference, for one routine.
    procedure CheckArea(a2D: Boolean);
    // Checks that alpha is blurred like red when both are equal, for one routine.
    procedure CheckAlpha(a2D: Boolean);
  published
    procedure TestConstantImageUnchanged;
    procedure TestMatrixBlur2DConstantImageUnchanged;
    procedure TestRadiusZeroIsIdentity;
    procedure TestImpulseSpreadsIntoTheMatrixBell;
    procedure TestMatrixBlur2DImpulseFollowsTheMatrix;
    procedure TestMirroredImageGivesMirroredBlur;
    procedure TestMatrixBlur2DMirroredImageGivesMirroredBlur;
    procedure TestEnergyIsPreserved;
    procedure TestMatrixBlur2DEnergyIsPreserved;
    procedure TestAreaBlurUsesTheOriginalPixels;
    procedure TestMatrixBlur2DAreaUsesTheOriginalPixels;
    procedure TestAlphaIsBlurredLikeTheColor;
    procedure TestMatrixBlur2DAlphaIsBlurredLikeTheColor;
  end;

  TTestGaussianBinominal = class(TTestCase)
  private
    // A black square image of aSize pixels with a white pixel in the middle, blurred with aRadius.
    function BlurredImpulse(aSize, aRadius: Integer): TFPMemoryImage;
    // Checks that a constant image stays constant when blurred with aRadius.
    procedure CheckConstant(aRadius: Integer);
  published
    procedure TestConstantImageUnchanged;
    procedure TestLargeRadiusConstantImageUnchanged;
    procedure TestRadiusZeroIsIdentity;
    procedure TestImpulseIsSymmetric;
    procedure TestImpulseIsTheSameInBothDirections;
    procedure TestImpulseNearTheEdgeIsTheSameInBothDirections;
    procedure TestEnergyIsPreserved;
    procedure TestAlphaIsBlurredLikeTheColor;
    procedure TestSeparateDestinationAtDestXY;
  end;

implementation

type
  TWords = array[0..MaxInt div 4] of Word;
  PWords = ^TWords;

const
  cSentinel: TFPColor = (Red: $0101; Green: $0202; Blue: $0303; Alpha: $FFFF);
  cRadii: array[0..4] of Integer = (2, 3, 5, 8, 12);


// Channel aIndex of aColor: 0 red, 1 green, 2 blue, 3 alpha.
function GetChannel(const aColor: TFPColor; aIndex: Integer): Word;

begin
  case aIndex of
    0: Result := aColor.Red;
    1: Result := aColor.Green;
    2: Result := aColor.Blue;
  else
    Result := aColor.Alpha;
  end;
end;


// Sets channel aIndex of aColor: 0 red, 1 green, 2 blue, 3 alpha.
procedure SetChannel(var aColor: TFPColor; aIndex: Integer; aValue: Word);

begin
  case aIndex of
    0: aColor.Red := aValue;
    1: aColor.Green := aValue;
    2: aColor.Blue := aValue;
  else
    aColor.Alpha := aValue;
  end;
end;


// The pixel of aImage at aX, aY with the coordinates clamped to the image.
function EdgeColor(aImage: TFPCustomImage; aX, aY: Integer): TFPColor;

begin
  Result := aImage.Colors[EnsureRange(aX, 0, aImage.Width - 1), EnsureRange(aY, 0, aImage.Height - 1)];
end;


// A copy of aImage.
function CopyImage(aImage: TFPCustomImage): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aImage.Width, aImage.Height);
  for lY := 0 to aImage.Height - 1 do
    for lX := 0 to aImage.Width - 1 do
      Result.Colors[lX, lY] := aImage.Colors[lX, lY];
end;


// A copy of aImage mirrored left to right or top to bottom.
function MirrorImage(aImage: TFPCustomImage; aHorizontal: Boolean): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aImage.Width, aImage.Height);
  for lY := 0 to aImage.Height - 1 do
    for lX := 0 to aImage.Width - 1 do
      if aHorizontal then
        Result.Colors[aImage.Width - 1 - lX, lY] := aImage.Colors[lX, lY]
      else
        Result.Colors[lX, aImage.Height - 1 - lY] := aImage.Colors[lX, lY];
end;


// A black image with a white pixel at aX, aY.
function CreateImpulseImage(aWidth, aHeight, aX, aY: Integer): TFPMemoryImage;

begin
  Result := CreateSolidImage(aWidth, aHeight, colBlack);
  Result.Colors[aX, aY] := colWhite;
end;


// The gradient image with alpha equal to red.
function CreateAlphaIsRedImage(aWidth, aHeight: Integer): TFPMemoryImage;

var
  lColor: TFPColor;
  lX, lY: Integer;

begin
  Result := CreateGradientImage(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      begin
      lColor := Result.Colors[lX, lY];
      lColor.Alpha := lColor.Red;
      Result.Colors[lX, lY] := lColor;
      end;
end;


// A copy of aImage with aArea blurred as documented for MatrixBlur1D, from the original pixels.
function ReferenceBlur1D(aImage: TFPCustomImage; aRadius: Integer; const aArea: TRect; aMatrix: PWord): TFPMemoryImage;

var
  lMatrix: PWords;
  lVertical: array of TFPColor;
  lColor: TFPColor;
  lSum: QWord;
  lX, lY, D, K, C: Integer;

begin
  lMatrix := PWords(aMatrix);
  Result := CopyImage(aImage);
  SetLength(lVertical, 2 * aRadius + 1);
  for lY := aArea.Top to aArea.Bottom - 1 do
    for lX := aArea.Left to aArea.Right - 1 do
      begin
      for D := -aRadius to aRadius do
        begin
        lColor := colTransparent;
        for C := 0 to 3 do
          begin
          lSum := 0;
          for K := -aRadius to aRadius do
            Inc(lSum, QWord(GetChannel(EdgeColor(aImage, lX + D, lY + K), C)) * lMatrix^[K + aRadius]);
          SetChannel(lColor, C, lSum shr 16);
          end;
        lVertical[D + aRadius] := lColor;
        end;
      lColor := colTransparent;
      for C := 0 to 3 do
        begin
        lSum := 0;
        for D := -aRadius to aRadius do
          Inc(lSum, QWord(GetChannel(lVertical[D + aRadius], C)) * lMatrix^[D + aRadius]);
        SetChannel(lColor, C, lSum shr 16);
        end;
      Result.Colors[lX, lY] := lColor;
      end;
end;


// A copy of aImage with aArea blurred as documented for MatrixBlur2D, from the original pixels.
function ReferenceBlur2D(aImage: TFPCustomImage; aRadius: Integer; const aArea: TRect; aMatrix: PWord): TFPMemoryImage;

var
  lMatrix: PWords;
  lColor: TFPColor;
  lSum: QWord;
  lWidth, lX, lY, DX, DY, C: Integer;

begin
  lMatrix := PWords(aMatrix);
  lWidth := 2 * aRadius + 1;
  Result := CopyImage(aImage);
  for lY := aArea.Top to aArea.Bottom - 1 do
    for lX := aArea.Left to aArea.Right - 1 do
      begin
      lColor := colTransparent;
      for C := 0 to 3 do
        begin
        lSum := 0;
        for DY := -aRadius to aRadius do
          for DX := -aRadius to aRadius do
            Inc(lSum, QWord(GetChannel(EdgeColor(aImage, lX + DX, lY + DY), C))
              * lMatrix^[DX + aRadius + (DY + aRadius) * lWidth]);
        SetChannel(lColor, C, lSum shr 16);
        end;
      Result.Colors[lX, lY] := lColor;
      end;
end;


// The sampled Gaussian of deviation aRadius/3 at distance squared aDist2, in one or two dimensions.
function Gauss(aRadius: Integer; aDist2: Double; a2D: Boolean): Double;

var
  lDeviation: Double;

begin
  lDeviation := aRadius / 3;
  Result := Exp(-aDist2 / (2 * lDeviation * lDeviation));
  if a2D then
    Result := Result / (2 * Pi * lDeviation * lDeviation)
  else
    Result := Result / Sqrt(2 * Pi * lDeviation * lDeviation);
end;


// The red channel summed over aImage.
function RedSum(aImage: TFPCustomImage): Int64;

var
  lX, lY: Integer;

begin
  Result := 0;
  for lY := 0 to aImage.Height - 1 do
    for lX := 0 to aImage.Width - 1 do
      Inc(Result, aImage.Colors[lX, lY].Red);
end;


{ TTestGaussMatrix }

procedure TTestGaussMatrix.CheckMatrix1D(aRadius: Integer);

var
  lMatrix: PWords;
  lTotal, lExpected: Double;
  lSum, I: Integer;

begin
  lMatrix := PWords(ComputeGaussianBlurMatrix1D(aRadius));
  try
    lSum := 0;
    for I := 0 to 2 * aRadius do
      Inc(lSum, lMatrix^[I]);
    AssertEquals(Format('Radius %d: the 1D matrix sums to 65536', [aRadius]), 65536, lSum);
    for I := 1 to aRadius do
      begin
      AssertEquals(Format('Radius %d: entry -%d equals entry %d', [aRadius, I, I]),
        lMatrix^[aRadius + I], lMatrix^[aRadius - I]);
      AssertTrue(Format('Radius %d: entry %d is at most entry %d', [aRadius, I, I - 1]),
        lMatrix^[aRadius + I] <= lMatrix^[aRadius + I - 1]);
      end;
    lTotal := 0;
    for I := -aRadius to aRadius do
      lTotal := lTotal + Gauss(aRadius, I * I, False);
    for I := -aRadius to aRadius do
      begin
      lExpected := 65536 * Gauss(aRadius, I * I, False) / lTotal;
      if Abs(lExpected - lMatrix^[I + aRadius]) > 656 then
        Fail(Format('Radius %d: entry %d is the normalized Gaussian %.0f, got %d',
          [aRadius, I, lExpected, lMatrix^[I + aRadius]]));
      end;
  finally
    FreeMem(lMatrix);
  end;
end;


procedure TTestGaussMatrix.CheckMatrix2D(aRadius: Integer);

var
  lMatrix: PWords;
  lTotal, lExpected: Double;
  lSum, lWidth, lValue, lX, lY: Integer;

begin
  lWidth := 2 * aRadius + 1;
  lMatrix := PWords(ComputeGaussianBlurMatrix2D(aRadius));
  try
    lSum := 0;
    for lX := 0 to lWidth * lWidth - 1 do
      Inc(lSum, lMatrix^[lX]);
    AssertEquals(Format('Radius %d: the 2D matrix sums to 65536', [aRadius]), 65536, lSum);
    for lY := 0 to lWidth - 1 do
      for lX := 0 to lWidth - 1 do
        begin
        lValue := lMatrix^[lX + lY * lWidth];
        if (lMatrix^[lWidth - 1 - lX + lY * lWidth] <> lValue)
          or (lMatrix^[lX + (lWidth - 1 - lY) * lWidth] <> lValue)
          or (lMatrix^[lY + lX * lWidth] <> lValue) then
          Fail(Format('Radius %d: entry (%d,%d) equals its mirrored and transposed entries', [aRadius, lX, lY]));
        end;
    lTotal := 0;
    for lY := -aRadius to aRadius do
      for lX := -aRadius to aRadius do
        lTotal := lTotal + Gauss(aRadius, lX * lX + lY * lY, True);
    for lY := -aRadius to aRadius do
      for lX := -aRadius to aRadius do
        begin
        lExpected := 65536 * Gauss(aRadius, lX * lX + lY * lY, True) / lTotal;
        if Abs(lExpected - lMatrix^[lX + aRadius + (lY + aRadius) * lWidth]) > 656 then
          Fail(Format('Radius %d: entry (%d,%d) is the normalized Gaussian %.0f, got %d',
            [aRadius, lX, lY, lExpected, lMatrix^[lX + aRadius + (lY + aRadius) * lWidth]]));
        end;
  finally
    FreeMem(lMatrix);
  end;
end;


procedure TTestGaussMatrix.TestMatrix1D;

var
  R: Integer;

begin
  for R := Low(cRadii) to High(cRadii) do
    CheckMatrix1D(cRadii[R]);
end;


procedure TTestGaussMatrix.TestMatrix1DOfRadiusOne;

begin
  CheckMatrix1D(1);
end;


procedure TTestGaussMatrix.TestMatrix2D;

var
  R: Integer;

begin
  for R := Low(cRadii) to High(cRadii) do
    CheckMatrix2D(cRadii[R]);
end;


procedure TTestGaussMatrix.TestMatrix2DOfRadiusOne;

begin
  CheckMatrix2D(1);
end;


{ TTestGaussianBlur }

procedure TTestGaussianBlur.Blur(aImage: TFPCustomImage; aRadius: Integer; const aArea: TRect; a2D: Boolean);

var
  lMatrix: PWord;

begin
  if not a2D then
    GaussianBlur(aImage, aRadius, aArea)
  else
    begin
    lMatrix := ComputeGaussianBlurMatrix2D(aRadius);
    try
      MatrixBlur2D(aImage, aRadius, aArea, lMatrix);
    finally
      FreeMem(lMatrix);
    end;
    end;
end;


function TTestGaussianBlur.Reference(aImage: TFPCustomImage; aRadius: Integer; const aArea: TRect; a2D: Boolean): TFPMemoryImage;

var
  lMatrix: PWord;

begin
  if a2D then
    lMatrix := ComputeGaussianBlurMatrix2D(aRadius)
  else
    lMatrix := ComputeGaussianBlurMatrix1D(aRadius);
  try
    if a2D then
      Result := ReferenceBlur2D(aImage, aRadius, aArea, lMatrix)
    else
      Result := ReferenceBlur1D(aImage, aRadius, aArea, lMatrix);
  finally
    FreeMem(lMatrix);
  end;
end;


procedure TTestGaussianBlur.CheckConstant(a2D: Boolean);

const
  cCases: array[0..3] of record Width, Height, Radius: Integer; end = (
    (Width: 12; Height: 9; Radius: 2),
    (Width: 12; Height: 9; Radius: 3),
    (Width: 20; Height: 17; Radius: 8),
    (Width: 5; Height: 4; Radius: 10));

var
  lImage, lExpected: TFPMemoryImage;
  lColor: TFPColor;
  C: Integer;

begin
  lColor := RGB8(200, 100, 50, 180);
  for C := Low(cCases) to High(cCases) do
    with cCases[C] do
      begin
      lExpected := nil;
      lImage := CreateSolidImage(Width, Height, lColor);
      try
        lExpected := CreateSolidImage(Width, Height, lColor);
        Blur(lImage, Radius, Rect(0, 0, Width, Height), a2D);
        AssertImagesEqual(Format('%dx%d, radius %d: a constant image is unchanged, edges included',
          [Width, Height, Radius]), lExpected, lImage);
      finally
        lExpected.Free;
        lImage.Free;
      end;
      end;
end;


procedure TTestGaussianBlur.CheckImpulse(a2D: Boolean);

const
  cSize = 15;
  cRadius = 3;
  cMid = cSize div 2;

var
  lImage, lExpected: TFPMemoryImage;
  D: Integer;

begin
  lExpected := nil;
  lImage := CreateImpulseImage(cSize, cSize, cMid, cMid);
  try
    lExpected := Reference(lImage, cRadius, Rect(0, 0, cSize, cSize), a2D);
    Blur(lImage, cRadius, Rect(0, 0, cSize, cSize), a2D);
    AssertImagesEqual('A white pixel spreads into the bell of the matrix', lExpected, lImage, 1);
    for D := 1 to cRadius do
      begin
      AssertEquals(Format('The bell is symmetric left and right at distance %d', [D]),
        lImage.Colors[cMid - D, cMid].Red, lImage.Colors[cMid + D, cMid].Red);
      AssertEquals(Format('The bell is symmetric up and down at distance %d', [D]),
        lImage.Colors[cMid, cMid - D].Red, lImage.Colors[cMid, cMid + D].Red);
      AssertEquals(Format('The bell is the same in both directions at distance %d', [D]),
        lImage.Colors[cMid + D, cMid].Red, lImage.Colors[cMid, cMid + D].Red);
      AssertTrue(Format('The bell falls at distance %d', [D]),
        lImage.Colors[cMid + D, cMid].Red <= lImage.Colors[cMid + D - 1, cMid].Red);
      end;
    AssertEquals('Nothing reaches beyond the radius', 0, lImage.Colors[cMid + cRadius + 1, cMid].Red);
  finally
    lExpected.Free;
    lImage.Free;
  end;
end;


procedure TTestGaussianBlur.CheckMirror(a2D: Boolean);

var
  lImage, lMirrored, lExpected: TFPMemoryImage;
  lHorizontal: Boolean;

begin
  for lHorizontal := False to True do
    begin
    lMirrored := nil;
    lExpected := nil;
    lImage := CreateAlphaImage(11, 9);
    try
      lMirrored := MirrorImage(lImage, lHorizontal);
      Blur(lImage, 2, Rect(0, 0, 11, 9), a2D);
      Blur(lMirrored, 2, Rect(0, 0, 11, 9), a2D);
      lExpected := MirrorImage(lImage, lHorizontal);
      AssertImagesEqual(Format('The blur of the image mirrored %s is the mirrored blur',
        [BoolToStr(lHorizontal, 'left to right', 'top to bottom')]), lExpected, lMirrored, 1);
    finally
      lExpected.Free;
      lMirrored.Free;
      lImage.Free;
    end;
    end;
end;


procedure TTestGaussianBlur.CheckEnergy(a2D: Boolean);

const
  cRadius = 3;

var
  lImage: TFPMemoryImage;
  lSum: Int64;

begin
  lImage := CreateImpulseImage(21, 21, 10, 10);
  try
    Blur(lImage, cRadius, Rect(0, 0, 21, 21), a2D);
    lSum := RedSum(lImage);
    if Abs(lSum - $FFFF) > 2 * Sqr(2 * cRadius + 1) then
      Fail(Format('The blurred white pixel keeps its sum of 65535, got %d', [lSum]));
  finally
    lImage.Free;
  end;
end;


procedure TTestGaussianBlur.CheckArea(a2D: Boolean);

var
  lImage, lExpected: TFPMemoryImage;
  lArea: TRect;

begin
  lExpected := nil;
  lArea := Rect(2, 5, 10, 12);
  lImage := CreateGradientImage(12, 16);
  try
    lExpected := Reference(lImage, 2, lArea, a2D);
    Blur(lImage, 2, lArea, a2D);
    AssertImagesEqual('A blurred area uses the original pixels around it; the rest is unchanged', lExpected, lImage, 1);
  finally
    lExpected.Free;
    lImage.Free;
  end;
end;


procedure TTestGaussianBlur.CheckAlpha(a2D: Boolean);

var
  lImage: TFPMemoryImage;
  lX, lY: Integer;

begin
  lImage := CreateAlphaIsRedImage(12, 10);
  try
    Blur(lImage, 3, Rect(0, 0, 12, 10), a2D);
    for lY := 0 to 9 do
      for lX := 0 to 11 do
        AssertEquals(Format('Pixel (%d,%d): alpha is blurred with the same weights as red', [lX, lY]),
          lImage.Colors[lX, lY].Red, lImage.Colors[lX, lY].Alpha);
  finally
    lImage.Free;
  end;
end;


procedure TTestGaussianBlur.TestConstantImageUnchanged;

begin
  CheckConstant(False);
end;


procedure TTestGaussianBlur.TestMatrixBlur2DConstantImageUnchanged;

begin
  CheckConstant(True);
end;


procedure TTestGaussianBlur.TestRadiusZeroIsIdentity;

var
  lImage, lExpected: TFPMemoryImage;

begin
  lExpected := nil;
  lImage := CreateAlphaImage(9, 7);
  try
    lExpected := CreateAlphaImage(9, 7);
    GaussianBlur(lImage, 0, Rect(0, 0, 9, 7));
    AssertImagesEqual('GaussianBlur with radius 0 changes nothing', lExpected, lImage);
    MatrixBlur1D(lImage, 0, Rect(0, 0, 9, 7), nil);
    AssertImagesEqual('MatrixBlur1D with radius 0 changes nothing', lExpected, lImage);
    MatrixBlur2D(lImage, 0, Rect(0, 0, 9, 7), nil);
    AssertImagesEqual('MatrixBlur2D with radius 0 changes nothing', lExpected, lImage);
  finally
    lExpected.Free;
    lImage.Free;
  end;
end;


procedure TTestGaussianBlur.TestImpulseSpreadsIntoTheMatrixBell;

begin
  CheckImpulse(False);
end;


procedure TTestGaussianBlur.TestMatrixBlur2DImpulseFollowsTheMatrix;

begin
  CheckImpulse(True);
end;


procedure TTestGaussianBlur.TestMirroredImageGivesMirroredBlur;

begin
  CheckMirror(False);
end;


procedure TTestGaussianBlur.TestMatrixBlur2DMirroredImageGivesMirroredBlur;

begin
  CheckMirror(True);
end;


procedure TTestGaussianBlur.TestEnergyIsPreserved;

begin
  CheckEnergy(False);
end;


procedure TTestGaussianBlur.TestMatrixBlur2DEnergyIsPreserved;

begin
  CheckEnergy(True);
end;


procedure TTestGaussianBlur.TestAreaBlurUsesTheOriginalPixels;

begin
  CheckArea(False);
end;


procedure TTestGaussianBlur.TestMatrixBlur2DAreaUsesTheOriginalPixels;

begin
  CheckArea(True);
end;


procedure TTestGaussianBlur.TestAlphaIsBlurredLikeTheColor;

begin
  CheckAlpha(False);
end;


procedure TTestGaussianBlur.TestMatrixBlur2DAlphaIsBlurredLikeTheColor;

begin
  CheckAlpha(True);
end;


{ TTestGaussianBinominal }

function TTestGaussianBinominal.BlurredImpulse(aSize, aRadius: Integer): TFPMemoryImage;

begin
  Result := CreateImpulseImage(aSize, aSize, aSize div 2, aSize div 2);
  try
    GaussianBlurBinominal4(Result, aRadius, Rect(0, 0, aSize, aSize));
  except
    Result.Free;
    raise;
  end;
end;


procedure TTestGaussianBinominal.CheckConstant(aRadius: Integer);

var
  lImage, lExpected: TFPMemoryImage;
  lColor: TFPColor;

begin
  lColor := RGB8(200, 100, 50, 180);
  lExpected := nil;
  lImage := CreateSolidImage(40, 30, lColor);
  try
    lExpected := CreateSolidImage(40, 30, lColor);
    GaussianBlurBinominal4(lImage, aRadius, Rect(0, 0, 40, 30));
    AssertImagesEqual(Format('Radius %d: a constant image is unchanged, edges included', [aRadius]),
      lExpected, lImage, 1);
  finally
    lExpected.Free;
    lImage.Free;
  end;
end;


procedure TTestGaussianBinominal.TestConstantImageUnchanged;

begin
  CheckConstant(1);
  CheckConstant(2);
  CheckConstant(3);
  CheckConstant(4);
end;


procedure TTestGaussianBinominal.TestLargeRadiusConstantImageUnchanged;

begin
  CheckConstant(16);
end;


procedure TTestGaussianBinominal.TestRadiusZeroIsIdentity;

var
  lImage, lExpected: TFPMemoryImage;

begin
  lExpected := nil;
  lImage := CreateAlphaImage(9, 7);
  try
    lExpected := CreateAlphaImage(9, 7);
    GaussianBlurBinominal4(lImage, 0, Rect(0, 0, 9, 7));
    AssertImagesEqual('GaussianBlurBinominal4 with radius 0 changes nothing', lExpected, lImage);
  finally
    lExpected.Free;
    lImage.Free;
  end;
end;


procedure TTestGaussianBinominal.TestImpulseIsSymmetric;

const
  cSize = 41;
  cMid = cSize div 2;

var
  lImage: TFPMemoryImage;
  D: Integer;

begin
  lImage := BlurredImpulse(cSize, 3);
  try
    AssertTrue('The white pixel spreads to its neighbours', lImage.Colors[cMid + 1, cMid].Red > 0);
    for D := 1 to 10 do
      begin
      AssertEquals(Format('The bell is symmetric left and right at distance %d', [D]),
        lImage.Colors[cMid - D, cMid].Red, lImage.Colors[cMid + D, cMid].Red, 64);
      AssertEquals(Format('The bell is symmetric up and down at distance %d', [D]),
        lImage.Colors[cMid, cMid - D].Red, lImage.Colors[cMid, cMid + D].Red, 64);
      end;
  finally
    lImage.Free;
  end;
end;


procedure TTestGaussianBinominal.TestImpulseIsTheSameInBothDirections;

const
  cSize = 41;
  cMid = cSize div 2;

var
  lImage: TFPMemoryImage;
  lX, lY: Integer;

begin
  lImage := BlurredImpulse(cSize, 3);
  try
    for lY := cMid - 10 to cMid + 10 do
      for lX := cMid - 10 to cMid + 10 do
        AssertEquals(Format('Pixel (%d,%d) equals the transposed pixel', [lX, lY]),
          lImage.Colors[lX, lY].Red, lImage.Colors[lY, lX].Red, 64);
  finally
    lImage.Free;
  end;
end;


procedure TTestGaussianBinominal.TestImpulseNearTheEdgeIsTheSameInBothDirections;

const
  cSize = 41;
  cMid = cSize div 2;

var
  lTop, lLeft: TFPMemoryImage;
  lX, lY: Integer;

begin
  lLeft := nil;
  lTop := CreateImpulseImage(cSize, cSize, cMid, 1);
  try
    lLeft := CreateImpulseImage(cSize, cSize, 1, cMid);
    GaussianBlurBinominal4(lTop, 3, Rect(0, 0, cSize, cSize));
    GaussianBlurBinominal4(lLeft, 3, Rect(0, 0, cSize, cSize));
    for lY := 0 to 15 do
      for lX := cMid - 10 to cMid + 10 do
        AssertEquals(Format('Pixel (%d,%d) near the top edge equals the transposed pixel near the left edge', [lX, lY]),
          lTop.Colors[lX, lY].Red, lLeft.Colors[lY, lX].Red, 64);
  finally
    lLeft.Free;
    lTop.Free;
  end;
end;


procedure TTestGaussianBinominal.TestEnergyIsPreserved;

var
  lImage: TFPMemoryImage;
  lSum: Int64;

begin
  lImage := BlurredImpulse(41, 3);
  try
    lSum := RedSum(lImage);
    if Abs(lSum - $FFFF) > $FFFF div 50 then
      Fail(Format('The blurred white pixel keeps its sum of 65535, got %d', [lSum]));
  finally
    lImage.Free;
  end;
end;


procedure TTestGaussianBinominal.TestAlphaIsBlurredLikeTheColor;

var
  lImage: TFPMemoryImage;
  lX, lY: Integer;

begin
  lImage := CreateAlphaIsRedImage(24, 20);
  try
    GaussianBlurBinominal4(lImage, 3, Rect(0, 0, 24, 20));
    for lY := 0 to 19 do
      for lX := 0 to 23 do
        AssertEquals(Format('Pixel (%d,%d): alpha is blurred with the same weights as red', [lX, lY]),
          lImage.Colors[lX, lY].Red, lImage.Colors[lX, lY].Alpha);
  finally
    lImage.Free;
  end;
end;


procedure TTestGaussianBinominal.TestSeparateDestinationAtDestXY;

var
  lSource, lDest: TFPMemoryImage;
  lColor: TFPColor;
  lX, lY: Integer;

begin
  lColor := RGB8(200, 100, 50);
  lDest := nil;
  lSource := CreateSolidImage(10, 10, lColor);
  try
    lDest := CreateSolidImage(20, 20, cSentinel);
    GaussianBlurBinominal4(lSource, lDest, 2, Rect(0, 0, 10, 10), Point(5, 5));
    for lY := 0 to 19 do
      for lX := 0 to 19 do
        if (lX >= 5) and (lX < 15) and (lY >= 5) and (lY < 15) then
          AssertColorsEqual(Format('Destination pixel (%d,%d) is the blurred constant source', [lX, lY]),
            lColor, lDest.Colors[lX, lY], 1)
        else
          AssertColorsEqual(Format('Destination pixel (%d,%d) outside DestXY and the area is unchanged', [lX, lY]),
            cSentinel, lDest.Colors[lX, lY]);
  finally
    lDest.Free;
    lSource.Free;
  end;
end;


initialization
  RegisterTests('blur', [TTestGaussMatrix, TTestGaussianBlur, TTestGaussianBinominal]);
end.
