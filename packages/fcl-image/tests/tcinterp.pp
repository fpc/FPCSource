{
    Tests for TFPCustomCanvas.StretchDraw with every interpolation class of
    fpcanvas and extinterpolation, drawn on a TFPImageCanvas.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcinterp;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, math, fpcunit, testregistry, fpimage, fpcanvas, fpimgcanv,
     extinterpolation, fpimgtests;

type
  TInterpolationClass = class of TFPCustomInterpolation;

  // A source size and a destination size.
  TScale = record
    OldWidth, OldHeight, NewWidth, NewHeight: Integer;
  end;

  TTestInterpolation = class(TTestCase)
  private
    FInterpolation: TFPCustomInterpolation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    // The interpolation class under test.
    function InterpolationClass: TInterpolationClass; virtual;
    // The largest overshoot or backward step allowed when enlarging a ramp.
    function RingingBound: Integer; virtual;
    // Draws aSource into aDest at aX, aY, scaled to aWidth x aHeight.
    procedure DrawStretched(aDest, aSource: TFPCustomImage; aX, aY, aWidth, aHeight: Integer; aMode: TFPDrawingMode = dmOpaque);
    // aSource scaled to a new image of aWidth x aHeight.
    function Stretched(aSource: TFPCustomImage; aWidth, aHeight: Integer): TFPMemoryImage;
    // Checks the contribution of every source pixel of an aOld pixel row shrunk to aNew pixels.
    procedure CheckShrinkWeights(aOld, aNew: Integer);
    // Checks that every pixel of an aOld pixel row contributes to the row scaled to aNew pixels.
    procedure CheckEverySourcePixelUsed(aOld, aNew: Integer);
    // Checks that mirroring the source mirrors the result for one scale and one axis.
    procedure CheckMirror(aOldWidth, aOldHeight, aNewWidth, aNewHeight: Integer; aHorizontal: Boolean);
    // Checks CheckMirror for each scale along both axes.
    procedure CheckMirrors(const aScales: array of TScale);
    // Checks that an enlarged two pixel ramp rises, along one axis.
    procedure CheckRamp(aLength: Integer; aHorizontal: Boolean);
  published
    procedure TestOnlyTheAreaIsWritten;
    procedure TestAreaPartlyOutsideTheCanvas;
    procedure TestEmptyAreaWritesNothing;
    procedure TestEmptySourceWritesNothing;
    procedure TestConstantImageStaysConstant;
    procedure TestHalvingACheckerGivesTheAverage;
    procedure TestShrinkWeightsFollowTheArea;
    procedure TestShrinkUsesEverySourcePixel;
    procedure TestEnlargeUsesEverySourcePixel;
    procedure TestEnlargedRampIsMonotonic;
    procedure TestMirroredShrinkGivesMirroredOutput;
    procedure TestMirroredEnlargeGivesMirroredOutput;
    procedure TestTransparentSourceInOpaqueMode;
    procedure TestTransparentSourceInAlphaBlendMode;
    procedure TestTransparentColorDoesNotBleed;
  end;

  TTestInterpolatingFilter = class(TTestInterpolation)
  published
    procedure TestOneToOneCopyIsExact;
  end;

  TTestInterpFPBox = class(TTestInterpolatingFilter)
  protected
    // TFPBoxInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpFPBase = class(TTestInterpolatingFilter)
  protected
    // TFPBaseInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpMitchell = class(TTestInterpolation)
  protected
    // TMitchellInterpolation.
    function InterpolationClass: TInterpolationClass; override;
    // The overshoot of the Mitchell-Netravali filter on a step.
    function RingingBound: Integer; override;
  published
    procedure TestKernelValues;
    procedure TestKernelSumsToOne;
    procedure TestEnlargeFollowsTheKernel;
  end;

  TTestInterpBlackman = class(TTestInterpolatingFilter)
  protected
    // TBlackmanInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpBlackmanSinc = class(TTestInterpolatingFilter)
  protected
    // TBlackmanSincInterpolation.
    function InterpolationClass: TInterpolationClass; override;
    // The overshoot of a windowed sinc on a step.
    function RingingBound: Integer; override;
  end;

  TTestInterpBlackmanBessel = class(TTestInterpolation)
  protected
    // TBlackmanBesselInterpolation.
    function InterpolationClass: TInterpolationClass; override;
    // The overshoot of a windowed jinc on a step.
    function RingingBound: Integer; override;
  end;

  TTestInterpGaussian = class(TTestInterpolation)
  protected
    // TGaussianInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpBox = class(TTestInterpolatingFilter)
  protected
    // TBoxInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpHermite = class(TTestInterpolatingFilter)
  protected
    // THermiteInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpLanczos = class(TTestInterpolatingFilter)
  protected
    // TLanczosInterpolation.
    function InterpolationClass: TInterpolationClass; override;
    // The overshoot of Lanczos 3 on a step.
    function RingingBound: Integer; override;
  end;

  TTestInterpQuadratic = class(TTestInterpolation)
  protected
    // TQuadraticInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpCubic = class(TTestInterpolation)
  protected
    // TCubicInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpCatrom = class(TTestInterpolatingFilter)
  protected
    // TCatromInterpolation.
    function InterpolationClass: TInterpolationClass; override;
    // The overshoot of Catmull-Rom on a step.
    function RingingBound: Integer; override;
  end;

  TTestInterpBilinear = class(TTestInterpolatingFilter)
  protected
    // TBilinearInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpHanning = class(TTestInterpolatingFilter)
  protected
    // THanningInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestInterpHamming = class(TTestInterpolation)
  protected
    // THammingInterpolation.
    function InterpolationClass: TInterpolationClass; override;
  end;

  TTestStretchDrawDefault = class(TTestCase)
  private
    // Draws the two pixel ramp enlarged to 9 x 1 with the canvas default interpolation.
    procedure DrawWithDefault;
  published
    procedure TestDefaultIsMitchell;
    procedure TestDefaultLeavesInterpolationUnset;
    procedure TestDefaultDoesNotLeak;
  end;

implementation

const
  cSentinel: TFPColor = (Red: $0101; Green: $0202; Blue: $0303; Alpha: $FFFF);
  cLevel = 257;

type
  TMitchellKernel = class(TMitchellInterpolation)
  public
    // The Mitchell filter at distance aX.
    function Value(aX: Double): Double;
  end;

const
  cScales: array[0..10] of TScale = (
    (OldWidth: 5; OldHeight: 5; NewWidth: 3; NewHeight: 3),
    (OldWidth: 3; OldHeight: 3; NewWidth: 7; NewHeight: 7),
    (OldWidth: 4; OldHeight: 4; NewWidth: 2; NewHeight: 2),
    (OldWidth: 7; OldHeight: 5; NewWidth: 2; NewHeight: 9),
    (OldWidth: 8; OldHeight: 3; NewWidth: 3; NewHeight: 8),
    (OldWidth: 2; OldHeight: 2; NewWidth: 9; NewHeight: 9),
    (OldWidth: 1; OldHeight: 1; NewWidth: 4; NewHeight: 4),
    (OldWidth: 6; OldHeight: 6; NewWidth: 6; NewHeight: 6),
    (OldWidth: 16; OldHeight: 4; NewWidth: 5; NewHeight: 11),
    (OldWidth: 1; OldHeight: 3; NewWidth: 5; NewHeight: 2),
    (OldWidth: 10; OldHeight: 10; NewWidth: 3; NewHeight: 3));


function TMitchellKernel.Value(aX: Double): Double;

begin
  Result := Filter(aX);
end;


// An image of aWidth x aHeight filled with the sentinel colour.
function CreateSentinelImage(aWidth, aHeight: Integer): TFPMemoryImage;

begin
  Result := CreateSolidImage(aWidth, aHeight, cSentinel);
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


// A black row of aWidth pixels with a white pixel at aIndex.
function CreateImpulseRow(aWidth, aIndex: Integer): TFPMemoryImage;

begin
  Result := CreateSolidImage(aWidth, 1, colBlack);
  Result.Colors[aIndex, 0] := colWhite;
end;


// The length of the overlap of the intervals [aStart1, aEnd1) and [aStart2, aEnd2).
function Overlap(aStart1, aEnd1, aStart2, aEnd2: Double): Double;

begin
  Result := Max(0.0, Min(aEnd1, aEnd2) - Max(aStart1, aStart2));
end;


{ TTestInterpolation }

procedure TTestInterpolation.SetUp;

begin
  inherited SetUp;
  FInterpolation := InterpolationClass.Create;
end;


procedure TTestInterpolation.TearDown;

begin
  FreeAndNil(FInterpolation);
  inherited TearDown;
end;


function TTestInterpolation.InterpolationClass: TInterpolationClass;

begin
  Result := TFPBaseInterpolation;
end;


function TTestInterpolation.RingingBound: Integer;

begin
  Result := 0;
end;


procedure TTestInterpolation.DrawStretched(aDest, aSource: TFPCustomImage; aX, aY, aWidth, aHeight: Integer; aMode: TFPDrawingMode);

var
  lCanvas: TFPImageCanvas;

begin
  lCanvas := TFPImageCanvas.Create(aDest);
  try
    lCanvas.Interpolation := FInterpolation;
    lCanvas.DrawingMode := aMode;
    lCanvas.StretchDraw(aX, aY, aWidth, aHeight, aSource);
  finally
    lCanvas.Free;
  end;
end;


function TTestInterpolation.Stretched(aSource: TFPCustomImage; aWidth, aHeight: Integer): TFPMemoryImage;

begin
  Result := CreateSentinelImage(aWidth, aHeight);
  try
    DrawStretched(Result, aSource, 0, 0, aWidth, aHeight);
  except
    Result.Free;
    raise;
  end;
end;


procedure TTestInterpolation.CheckShrinkWeights(aOld, aNew: Integer);

var
  lSource, lResult: TFPMemoryImage;
  lFactor, lExpected: Double;
  K, D: Integer;

begin
  lFactor := aOld / aNew;
  for K := 0 to aOld - 1 do
    begin
    lResult := nil;
    lSource := CreateImpulseRow(aOld, K);
    try
      lResult := Stretched(lSource, aNew, 1);
      for D := 0 to aNew - 1 do
        begin
        lExpected := $FFFF * Overlap(D * lFactor, (D + 1) * lFactor, K, K + 1) / lFactor;
        if Abs(lExpected - lResult.Colors[D, 0].Red) > 300 then
          Fail(Format('%d to %d pixels: source pixel %d gives destination pixel %d a share of %.0f, got %d',
            [aOld, aNew, K, D, lExpected, lResult.Colors[D, 0].Red]));
        end;
    finally
      lResult.Free;
      lSource.Free;
    end;
    end;
end;


procedure TTestInterpolation.CheckEverySourcePixelUsed(aOld, aNew: Integer);

var
  lSource, lResult: TFPMemoryImage;
  lMax: Word;
  K, D: Integer;

begin
  for K := 0 to aOld - 1 do
    begin
    lResult := nil;
    lSource := CreateImpulseRow(aOld, K);
    try
      lResult := Stretched(lSource, aNew, 1);
      lMax := 0;
      for D := 0 to aNew - 1 do
        lMax := Max(lMax, lResult.Colors[D, 0].Red);
      AssertTrue(Format('%d to %d pixels: source pixel %d contributes to the result', [aOld, aNew, K]), lMax > 1000);
    finally
      lResult.Free;
      lSource.Free;
    end;
    end;
end;


procedure TTestInterpolation.CheckMirror(aOldWidth, aOldHeight, aNewWidth, aNewHeight: Integer; aHorizontal: Boolean);

var
  lSource, lMirrored, lResult, lMirroredResult, lExpected: TFPMemoryImage;

begin
  lMirrored := nil;
  lResult := nil;
  lMirroredResult := nil;
  lExpected := nil;
  lSource := CreateGradientImage(aOldWidth, aOldHeight);
  try
    lMirrored := MirrorImage(lSource, aHorizontal);
    lResult := Stretched(lSource, aNewWidth, aNewHeight);
    lMirroredResult := Stretched(lMirrored, aNewWidth, aNewHeight);
    lExpected := MirrorImage(lResult, aHorizontal);
    AssertImagesEqual(Format('%dx%d to %dx%d, mirrored %s: the result is the mirrored result',
      [aOldWidth, aOldHeight, aNewWidth, aNewHeight, BoolToStr(aHorizontal, 'left to right', 'top to bottom')]),
      lExpected, lMirroredResult, 8);
  finally
    lExpected.Free;
    lMirroredResult.Free;
    lResult.Free;
    lMirrored.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.CheckRamp(aLength: Integer; aHorizontal: Boolean);

var
  lSource, lResult: TFPMemoryImage;
  lValues: array of Integer;
  I: Integer;

begin
  lResult := nil;
  if aHorizontal then
    lSource := TFPMemoryImage.Create(2, 1)
  else
    lSource := TFPMemoryImage.Create(1, 2);
  try
    lSource.Colors[0, 0] := colBlack;
    if aHorizontal then
      begin
      lSource.Colors[1, 0] := colWhite;
      lResult := Stretched(lSource, aLength, 1);
      end
    else
      begin
      lSource.Colors[0, 1] := colWhite;
      lResult := Stretched(lSource, 1, aLength);
      end;
    SetLength(lValues, aLength);
    for I := 0 to aLength - 1 do
      if aHorizontal then
        lValues[I] := lResult.Colors[I, 0].Red
      else
        lValues[I] := lResult.Colors[0, I].Red;
    for I := 1 to aLength - 1 do
      if lValues[I] < lValues[I - 1] - RingingBound then
        Fail(Format('2 to %d pixels: the ramp rises at pixel %d, got %d after %d (bound %d)',
          [aLength, I, lValues[I], lValues[I - 1], RingingBound]));
    AssertTrue(Format('2 to %d pixels: the ramp rises by at least a quarter of the range', [aLength]),
      lValues[aLength - 1] - lValues[0] > $4000);
  finally
    lResult.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.TestOnlyTheAreaIsWritten;

const
  cCases: array[0..3] of record X, Y: Integer; Scale: TScale; end = (
    (X: 2; Y: 3; Scale: (OldWidth: 5; OldHeight: 5; NewWidth: 3; NewHeight: 3)),
    (X: 4; Y: 1; Scale: (OldWidth: 3; OldHeight: 3; NewWidth: 7; NewHeight: 7)),
    (X: 0; Y: 0; Scale: (OldWidth: 6; OldHeight: 4; NewWidth: 6; NewHeight: 4)),
    (X: 5; Y: 2; Scale: (OldWidth: 10; OldHeight: 3; NewWidth: 9; NewHeight: 10)));

var
  lSource, lDest: TFPMemoryImage;
  lColor: TFPColor;
  lInside: Boolean;
  C, lX, lY: Integer;

begin
  lColor := RGB8(200, 100, 50);
  for C := Low(cCases) to High(cCases) do
    with cCases[C] do
      begin
      lDest := nil;
      lSource := CreateSolidImage(Scale.OldWidth, Scale.OldHeight, lColor);
      try
        lDest := CreateSentinelImage(16, 14);
        DrawStretched(lDest, lSource, X, Y, Scale.NewWidth, Scale.NewHeight);
        for lY := 0 to lDest.Height - 1 do
          for lX := 0 to lDest.Width - 1 do
            begin
            lInside := (lX >= X) and (lX < X + Scale.NewWidth) and (lY >= Y) and (lY < Y + Scale.NewHeight);
            if lInside then
              AssertColorsEqual(Format('Case %d: pixel (%d,%d) inside the area is drawn', [C, lX, lY]),
                lColor, lDest.Colors[lX, lY], cLevel)
            else
              AssertColorsEqual(Format('Case %d: pixel (%d,%d) outside the area is unchanged', [C, lX, lY]),
                cSentinel, lDest.Colors[lX, lY]);
            end;
      finally
        lDest.Free;
        lSource.Free;
      end;
      end;
end;


procedure TTestInterpolation.TestAreaPartlyOutsideTheCanvas;

var
  lSource, lDest: TFPMemoryImage;
  lColor: TFPColor;
  lX, lY: Integer;

begin
  lColor := RGB8(20, 120, 220);
  lDest := nil;
  lSource := CreateSolidImage(3, 3, lColor);
  try
    lDest := CreateSentinelImage(4, 4);
    DrawStretched(lDest, lSource, -2, -1, 6, 7);
    for lY := 0 to 3 do
      for lX := 0 to 3 do
        AssertColorsEqual(Format('An area covering the canvas draws pixel (%d,%d)', [lX, lY]),
          lColor, lDest.Colors[lX, lY], cLevel);
    FreeAndNil(lDest);
    lDest := CreateSentinelImage(4, 4);
    DrawStretched(lDest, lSource, 2, 2, 5, 5);
    for lY := 0 to 3 do
      for lX := 0 to 3 do
        if (lX >= 2) and (lY >= 2) then
          AssertColorsEqual(Format('An area past the corner draws pixel (%d,%d)', [lX, lY]),
            lColor, lDest.Colors[lX, lY], cLevel)
        else
          AssertColorsEqual(Format('An area past the corner leaves pixel (%d,%d)', [lX, lY]),
            cSentinel, lDest.Colors[lX, lY]);
  finally
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.TestEmptyAreaWritesNothing;

var
  lSource, lDest, lExpected: TFPMemoryImage;

begin
  lDest := nil;
  lExpected := nil;
  lSource := CreateGradientImage(4, 4);
  try
    lDest := CreateSentinelImage(6, 6);
    lExpected := CreateSentinelImage(6, 6);
    DrawStretched(lDest, lSource, 1, 1, 0, 3);
    DrawStretched(lDest, lSource, 1, 1, 3, 0);
    DrawStretched(lDest, lSource, 1, 1, -2, 3);
    AssertImagesEqual('An area without width or height draws nothing', lExpected, lDest);
  finally
    lExpected.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.TestEmptySourceWritesNothing;

var
  lSource, lDest, lExpected: TFPMemoryImage;

begin
  lDest := nil;
  lExpected := nil;
  lSource := TFPMemoryImage.Create(0, 0);
  try
    lDest := CreateSentinelImage(6, 6);
    lExpected := CreateSentinelImage(6, 6);
    DrawStretched(lDest, lSource, 1, 1, 3, 3);
    AssertImagesEqual('An empty source image draws nothing', lExpected, lDest);
  finally
    lExpected.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.TestConstantImageStaysConstant;

var
  lSource, lResult: TFPMemoryImage;
  lColors: array[0..1] of TFPColor;
  C, S, lX, lY: Integer;

begin
  lColors[0] := RGB8(200, 100, 50);
  lColors[1] := RGB8(10, 250, 128, 100);
  for C := 0 to 1 do
    for S := Low(cScales) to High(cScales) do
      with cScales[S] do
        begin
        lResult := nil;
        lSource := CreateSolidImage(OldWidth, OldHeight, lColors[C]);
        try
          lResult := Stretched(lSource, NewWidth, NewHeight);
          for lY := 0 to NewHeight - 1 do
            for lX := 0 to NewWidth - 1 do
              AssertColorsEqual(Format('%dx%d to %dx%d: pixel (%d,%d) of a constant image keeps the colour',
                [OldWidth, OldHeight, NewWidth, NewHeight, lX, lY]), lColors[C], lResult.Colors[lX, lY], cLevel);
        finally
          lResult.Free;
          lSource.Free;
        end;
        end;
end;


procedure TTestInterpolation.TestHalvingACheckerGivesTheAverage;

var
  lSource, lResult: TFPMemoryImage;
  lGray: TFPColor;
  lSize, lX, lY: Integer;

begin
  lGray := FPColor($8000, $8000, $8000);
  lSize := 4;
  while lSize <= 6 do
    begin
    lResult := nil;
    lSource := CreateCheckerImage(lSize, lSize, 1, colBlack, colWhite);
    try
      lResult := Stretched(lSource, lSize div 2, lSize div 2);
      for lY := 0 to lResult.Height - 1 do
        for lX := 0 to lResult.Width - 1 do
          AssertColorsEqual(Format('%d to %d: pixel (%d,%d) is the average of its 2x2 block',
            [lSize, lSize div 2, lX, lY]), lGray, lResult.Colors[lX, lY], cLevel);
    finally
      lResult.Free;
      lSource.Free;
    end;
    Inc(lSize, 2);
    end;
end;


procedure TTestInterpolation.TestShrinkWeightsFollowTheArea;

begin
  CheckShrinkWeights(4, 2);
  CheckShrinkWeights(3, 2);
  CheckShrinkWeights(5, 3);
  CheckShrinkWeights(7, 2);
  CheckShrinkWeights(8, 3);
end;


procedure TTestInterpolation.TestShrinkUsesEverySourcePixel;

begin
  CheckEverySourcePixelUsed(5, 3);
  CheckEverySourcePixelUsed(8, 3);
  CheckEverySourcePixelUsed(7, 2);
  CheckEverySourcePixelUsed(10, 3);
end;


procedure TTestInterpolation.TestEnlargeUsesEverySourcePixel;

begin
  CheckEverySourcePixelUsed(3, 7);
  CheckEverySourcePixelUsed(2, 9);
  CheckEverySourcePixelUsed(5, 6);
end;


procedure TTestInterpolation.TestEnlargedRampIsMonotonic;

begin
  CheckRamp(9, True);
  CheckRamp(16, True);
  CheckRamp(9, False);
end;


procedure TTestInterpolation.CheckMirrors(const aScales: array of TScale);

var
  S: Integer;

begin
  for S := Low(aScales) to High(aScales) do
    with aScales[S] do
      begin
      CheckMirror(OldWidth, OldHeight, NewWidth, NewHeight, True);
      CheckMirror(OldHeight, OldWidth, NewHeight, NewWidth, False);
      end;
end;


procedure TTestInterpolation.TestMirroredShrinkGivesMirroredOutput;

const
  cMirrorScales: array[0..3] of TScale = (
    (OldWidth: 4; OldHeight: 3; NewWidth: 2; NewHeight: 3),
    (OldWidth: 5; OldHeight: 3; NewWidth: 3; NewHeight: 3),
    (OldWidth: 8; OldHeight: 2; NewWidth: 3; NewHeight: 2),
    (OldWidth: 7; OldHeight: 3; NewWidth: 2; NewHeight: 3));

begin
  CheckMirrors(cMirrorScales);
end;


procedure TTestInterpolation.TestMirroredEnlargeGivesMirroredOutput;

const
  cMirrorScales: array[0..2] of TScale = (
    (OldWidth: 3; OldHeight: 2; NewWidth: 7; NewHeight: 2),
    (OldWidth: 2; OldHeight: 2; NewWidth: 8; NewHeight: 2),
    (OldWidth: 5; OldHeight: 2; NewWidth: 6; NewHeight: 2));

begin
  CheckMirrors(cMirrorScales);
end;


procedure TTestInterpolation.TestTransparentSourceInOpaqueMode;

var
  lSource, lDest: TFPMemoryImage;
  lX, lY: Integer;

begin
  lDest := nil;
  lSource := CreateSolidImage(3, 3, RGB8(10, 20, 30, 0));
  try
    lDest := CreateSentinelImage(8, 8);
    DrawStretched(lDest, lSource, 1, 1, 5, 5, dmOpaque);
    for lY := 0 to 7 do
      for lX := 0 to 7 do
        if (lX >= 1) and (lX < 6) and (lY >= 1) and (lY < 6) then
          AssertEquals(Format('dmOpaque replaces pixel (%d,%d) by the transparent source', [lX, lY]),
            0, lDest.Colors[lX, lY].Alpha)
        else
          AssertColorsEqual(Format('dmOpaque leaves pixel (%d,%d) outside the area', [lX, lY]),
            cSentinel, lDest.Colors[lX, lY]);
  finally
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.TestTransparentSourceInAlphaBlendMode;

var
  lSource, lDest, lExpected: TFPMemoryImage;

begin
  lDest := nil;
  lExpected := nil;
  lSource := CreateSolidImage(3, 3, RGB8(10, 20, 30, 0));
  try
    lDest := CreateSentinelImage(8, 8);
    lExpected := CreateSentinelImage(8, 8);
    DrawStretched(lDest, lSource, 1, 1, 5, 5, dmAlphaBlend);
    DrawStretched(lDest, lSource, 1, 1, 2, 2, dmAlphaBlend);
    AssertImagesEqual('dmAlphaBlend of a transparent source leaves the destination unchanged', lExpected, lDest);
  finally
    lExpected.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestInterpolation.TestTransparentColorDoesNotBleed;

const
  cSizes: array[0..3] of Integer = (1, 3, 5, 8);

var
  lSource, lResult: TFPMemoryImage;
  lColor: TFPColor;
  S, lX: Integer;

begin
  lSource := TFPMemoryImage.Create(2, 1);
  try
    lSource.Colors[0, 0] := RGB8(255, 0, 0);
    lSource.Colors[1, 0] := RGB8(0, 0, 0, 0);
    for S := Low(cSizes) to High(cSizes) do
      begin
      lResult := Stretched(lSource, cSizes[S], 1);
      try
        for lX := 0 to cSizes[S] - 1 do
          begin
          lColor := lResult.Colors[lX, 0];
          if lColor.Alpha >= cLevel then
            begin
            lColor.Alpha := alphaOpaque;
            AssertColorsEqual(Format('2 to %d pixels: pixel %d is red, the colour of the transparent pixel does not mix in',
              [cSizes[S], lX]), RGB8(255, 0, 0), lColor, 2 * cLevel);
            end;
          end;
      finally
        lResult.Free;
      end;
      end;
  finally
    lSource.Free;
  end;
end;


{ TTestInterpolatingFilter }

procedure TTestInterpolatingFilter.TestOneToOneCopyIsExact;

var
  lSource, lResult: TFPMemoryImage;

begin
  lResult := nil;
  lSource := CreateAlphaImage(7, 5);
  try
    lResult := Stretched(lSource, 7, 5);
    AssertImagesEqual('A 1:1 copy reproduces the source', lSource, lResult);
  finally
    lResult.Free;
    lSource.Free;
  end;
end;


{ Interpolation classes }

function TTestInterpFPBox.InterpolationClass: TInterpolationClass;

begin
  Result := TFPBoxInterpolation;
end;


function TTestInterpFPBase.InterpolationClass: TInterpolationClass;

begin
  Result := TFPBaseInterpolation;
end;


function TTestInterpMitchell.InterpolationClass: TInterpolationClass;

begin
  Result := TMitchellInterpolation;
end;


function TTestInterpMitchell.RingingBound: Integer;

begin
  Result := Round($FFFF * 0.04);
end;


procedure TTestInterpMitchell.TestKernelValues;

var
  lKernel: TMitchellKernel;
  I: Integer;

begin
  lKernel := TMitchellKernel.Create;
  try
    AssertEquals('Mitchell k(0) = 8/9', 8 / 9, lKernel.Value(0), 1e-6);
    AssertEquals('Mitchell k(1) = 1/18', 1 / 18, lKernel.Value(1), 1e-6);
    AssertEquals('Mitchell k(-1) = 1/18', 1 / 18, lKernel.Value(-1), 1e-6);
    AssertEquals('Mitchell k(2) = 0', 0, lKernel.Value(2), 1e-6);
    AssertEquals('Mitchell k(2.5) = 0', 0, lKernel.Value(2.5), 1e-6);
    for I := 1 to 19 do
      AssertEquals(Format('Mitchell k(-x) = k(x) for x = %.1f', [I / 10]),
        lKernel.Value(I / 10), lKernel.Value(-I / 10), 1e-6);
  finally
    lKernel.Free;
  end;
end;


procedure TTestInterpMitchell.TestKernelSumsToOne;

var
  lKernel: TMitchellKernel;
  lX, lSum: Double;
  I, K: Integer;

begin
  lKernel := TMitchellKernel.Create;
  try
    for I := 0 to 9 do
      begin
      lX := I / 10;
      lSum := 0;
      for K := -2 to 2 do
        lSum := lSum + lKernel.Value(lX + K);
      AssertEquals(Format('The Mitchell weights at offset %.1f sum to 1', [lX]), 1, lSum, 1e-6);
      end;
  finally
    lKernel.Free;
  end;
end;


procedure TTestInterpMitchell.TestEnlargeFollowsTheKernel;

const
  cNew = 9;

var
  lKernel: TMitchellKernel;
  lSource, lResult: TFPMemoryImage;
  lPos, lExpected: Double;
  I: Integer;

begin
  lResult := nil;
  lKernel := TMitchellKernel.Create;
  lSource := TFPMemoryImage.Create(2, 1);
  try
    lSource.Colors[0, 0] := colBlack;
    lSource.Colors[1, 0] := colWhite;
    lResult := Stretched(lSource, cNew, 1);
    for I := 0 to cNew - 1 do
      begin
      // Sample positions of TFPBaseInterpolation; the source is extended by its edge pixels.
      lPos := (I + 0.5) / cNew;
      lExpected := $FFFF * (lKernel.Value(lPos - 1) + lKernel.Value(lPos - 2));
      if Abs(lExpected - lResult.Colors[I, 0].Red) > $FFFF * 0.01 then
        Fail(Format('Pixel %d at source position %.3f is the Mitchell weighted sum %.0f, got %d',
          [I, lPos, lExpected, lResult.Colors[I, 0].Red]));
      end;
  finally
    lResult.Free;
    lSource.Free;
    lKernel.Free;
  end;
end;


function TTestInterpBlackman.InterpolationClass: TInterpolationClass;

begin
  Result := TBlackmanInterpolation;
end;


function TTestInterpBlackmanSinc.InterpolationClass: TInterpolationClass;

begin
  Result := TBlackmanSincInterpolation;
end;


function TTestInterpBlackmanSinc.RingingBound: Integer;

begin
  Result := Round($FFFF * 0.15);
end;


function TTestInterpBlackmanBessel.InterpolationClass: TInterpolationClass;

begin
  Result := TBlackmanBesselInterpolation;
end;


function TTestInterpBlackmanBessel.RingingBound: Integer;

begin
  Result := Round($FFFF * 0.15);
end;


function TTestInterpGaussian.InterpolationClass: TInterpolationClass;

begin
  Result := TGaussianInterpolation;
end;


function TTestInterpBox.InterpolationClass: TInterpolationClass;

begin
  Result := TBoxInterpolation;
end;


function TTestInterpHermite.InterpolationClass: TInterpolationClass;

begin
  Result := THermiteInterpolation;
end;


function TTestInterpLanczos.InterpolationClass: TInterpolationClass;

begin
  Result := TLanczosInterpolation;
end;


function TTestInterpLanczos.RingingBound: Integer;

begin
  Result := Round($FFFF * 0.15);
end;


function TTestInterpQuadratic.InterpolationClass: TInterpolationClass;

begin
  Result := TQuadraticInterpolation;
end;


function TTestInterpCubic.InterpolationClass: TInterpolationClass;

begin
  Result := TCubicInterpolation;
end;


function TTestInterpCatrom.InterpolationClass: TInterpolationClass;

begin
  Result := TCatromInterpolation;
end;


function TTestInterpCatrom.RingingBound: Integer;

begin
  Result := Round($FFFF * 0.15);
end;


function TTestInterpBilinear.InterpolationClass: TInterpolationClass;

begin
  Result := TBilinearInterpolation;
end;


function TTestInterpHanning.InterpolationClass: TInterpolationClass;

begin
  Result := THanningInterpolation;
end;


function TTestInterpHamming.InterpolationClass: TInterpolationClass;

begin
  Result := THammingInterpolation;
end;


{ TTestStretchDrawDefault }

procedure TTestStretchDrawDefault.DrawWithDefault;

var
  lSource, lDest: TFPMemoryImage;
  lCanvas: TFPImageCanvas;

begin
  lCanvas := nil;
  lDest := nil;
  lSource := TFPMemoryImage.Create(2, 1);
  try
    lSource.Colors[0, 0] := colBlack;
    lSource.Colors[1, 0] := colWhite;
    lDest := TFPMemoryImage.Create(9, 1);
    lCanvas := TFPImageCanvas.Create(lDest);
    lCanvas.StretchDraw(0, 0, 9, 1, lSource);
  finally
    lCanvas.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestStretchDrawDefault.TestDefaultIsMitchell;

var
  lSource, lDefault, lMitchell: TFPMemoryImage;
  lCanvas: TFPImageCanvas;
  lInterpolation: TMitchellInterpolation;

begin
  lDefault := nil;
  lMitchell := nil;
  lCanvas := nil;
  lInterpolation := nil;
  lSource := TFPMemoryImage.Create(2, 1);
  try
    lSource.Colors[0, 0] := colBlack;
    lSource.Colors[1, 0] := colWhite;
    lDefault := CreateSentinelImage(9, 1);
    lCanvas := TFPImageCanvas.Create(lDefault);
    lCanvas.StretchDraw(0, 0, 9, 1, lSource);
    FreeAndNil(lCanvas);
    lMitchell := CreateSentinelImage(9, 1);
    lInterpolation := TMitchellInterpolation.Create;
    lCanvas := TFPImageCanvas.Create(lMitchell);
    lCanvas.Interpolation := lInterpolation;
    lCanvas.StretchDraw(0, 0, 9, 1, lSource);
    AssertImagesEqual('Without an interpolation StretchDraw uses TMitchellInterpolation', lMitchell, lDefault, 8);
  finally
    lCanvas.Free;
    lInterpolation.Free;
    lMitchell.Free;
    lDefault.Free;
    lSource.Free;
  end;
end;


procedure TTestStretchDrawDefault.TestDefaultLeavesInterpolationUnset;

var
  lSource, lDest: TFPMemoryImage;
  lCanvas: TFPImageCanvas;

begin
  lCanvas := nil;
  lDest := nil;
  lSource := CreateGradientImage(3, 3);
  try
    lDest := TFPMemoryImage.Create(5, 5);
    lCanvas := TFPImageCanvas.Create(lDest);
    lCanvas.StretchDraw(0, 0, 5, 5, lSource);
    AssertNull('StretchDraw does not keep its default interpolation in the canvas', lCanvas.Interpolation);
  finally
    lCanvas.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestStretchDrawDefault.TestDefaultDoesNotLeak;

begin
  AssertNoLeak('StretchDraw frees its default interpolation', @DrawWithDefault);
end;


initialization
  RegisterTests('interp', [TTestInterpFPBox, TTestInterpFPBase, TTestInterpMitchell,
    TTestInterpBlackman, TTestInterpBlackmanSinc, TTestInterpBlackmanBessel, TTestInterpGaussian,
    TTestInterpBox, TTestInterpHermite, TTestInterpLanczos, TTestInterpQuadratic, TTestInterpCubic,
    TTestInterpCatrom, TTestInterpBilinear, TTestInterpHanning, TTestInterpHamming,
    TTestStretchDrawDefault]);
end.
