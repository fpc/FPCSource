{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the pixel comparison behind the svgdiff harness.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgdiff;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svgcompare, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpimage, fpcunit, testregistry, svgcompare,
     fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGCompositing = class(TTestCase)
  published
    procedure TestOpaqueColourIsUnchanged;
    procedure TestTransparentBecomesTheBackground;
    procedure TestHalfAlphaMeetsInTheMiddle;
    procedure TestCompositingAlwaysYieldsOpaque;
  end;

  TTestSVGPixelComparison = class(TTestCase)
  private
    FActual, FExpected, FDiff: TFPMemoryImage;
    FWhite: TSVGColor;
    // Fills an image with one colour.
    procedure Fill(aImage: TFPMemoryImage; const aColor: TSVGColor);
    // Compares the two images at the given tolerance.
    function Compare(aTolerance: Integer): TSVGComparison;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdenticalImagesMatch;
    procedure TestOneChangedPixelIsCounted;
    procedure TestDifferenceWithinToleranceIsIgnored;
    procedure TestDifferenceBeyondToleranceCounts;
    procedure TestWorstChannelIsReported;
    procedure TestPercentageIsOfTheWholeImage;
    procedure TestSizeMismatchIsRefused;
    procedure TestTransparencyIsResolvedAgainstTheBackground;
    procedure TestAlphaAloneIsNotADifference;
    procedure TestThresholdAcceptsASmallDifference;
    procedure TestThresholdRejectsALargeDifference;
    procedure TestMatchPassesEveryThreshold;
    procedure TestSizeMismatchPassesNothing;
    procedure TestDiffMapMarksTheChangedPixel;
    procedure TestDiffMapIsSizedToTheImages;
    procedure TestADistanceMapIsWhiteWhereTheTwoAgree;
    procedure TestADistanceMapSpreadsTheStepsOverTheWholeRange;
    procedure TestADistanceMapOfTwoAlikeIsWhiteThroughout;
    procedure TestComparingNothingIsRefused;
    procedure TestABoxCountsOnlyWhatItHolds;
    procedure TestABoxIsTrimmedToTheImages;
    procedure TestABoxOfNoExtentCountsNothing;
  end;

implementation

const
  Delta = 1e-9;

{ TTestSVGCompositing }

procedure TTestSVGCompositing.TestOpaqueColourIsUnchanged;

var
  lResult: TSVGColor;

begin
  lResult := SVGCompositeOver(TSVGColor.FromBytes(10, 20, 30, 255),
    TSVGColor.FromBytes(255, 255, 255, 255));
  AssertEquals('an opaque colour keeps its red', 10 * 257, lResult.Red);
  AssertEquals('and its green', 20 * 257, lResult.Green);
end;


procedure TTestSVGCompositing.TestTransparentBecomesTheBackground;

var
  lResult: TSVGColor;

begin
  lResult := SVGCompositeOver(TSVGColor.FromBytes(10, 20, 30, 0),
    TSVGColor.FromBytes(255, 255, 255, 255));
  AssertEquals('nothing of the colour survives', 65535, lResult.Red);
  AssertEquals('the background shows through', 65535, lResult.Blue);
end;


procedure TTestSVGCompositing.TestHalfAlphaMeetsInTheMiddle;

var
  lResult: TSVGColor;

begin
  lResult := SVGCompositeOver(TSVGColor.FromBytes(0, 0, 0, 128),
    TSVGColor.FromBytes(255, 255, 255, 255));
  AssertEquals('black at half alpha over white lands near the middle',
    32768, lResult.Red, 400);
end;


procedure TTestSVGCompositing.TestCompositingAlwaysYieldsOpaque;

var
  lResult: TSVGColor;

begin
  lResult := SVGCompositeOver(TSVGColor.FromBytes(1, 2, 3, 77),
    TSVGColor.FromBytes(255, 255, 255, 255));
  AssertEquals('the result has nothing left to composite', 65535,
    lResult.Alpha);
end;


{ TTestSVGPixelComparison }

procedure TTestSVGPixelComparison.SetUp;

begin
  inherited SetUp;
  FWhite := TSVGColor.FromBytes(255, 255, 255, 255);
  FActual := TFPMemoryImage.Create(10, 10);
  FExpected := TFPMemoryImage.Create(10, 10);
  FDiff := TFPMemoryImage.Create(1, 1);
  Fill(FActual, FWhite);
  Fill(FExpected, FWhite);
end;


procedure TTestSVGPixelComparison.TearDown;

begin
  FreeAndNil(FDiff);
  FreeAndNil(FExpected);
  FreeAndNil(FActual);
  inherited TearDown;
end;


procedure TTestSVGPixelComparison.Fill(aImage: TFPMemoryImage;
  const aColor: TSVGColor);

var
  X, Y: Integer;

begin
  for Y := 0 to aImage.Height - 1 do
    for X := 0 to aImage.Width - 1 do
      aImage.Colors[X, Y] := TFPColor(aColor);
end;


function TTestSVGPixelComparison.Compare(aTolerance: Integer): TSVGComparison;

begin
  Result := SVGCompareImages(FActual, FExpected, aTolerance, FWhite, nil);
end;


procedure TTestSVGPixelComparison.TestIdenticalImagesMatch;

var
  lResult: TSVGComparison;

begin
  lResult := Compare(0);
  AssertTrue('two identical images match', lResult.Outcome = crMatch);
  AssertEquals('no pixel differs', 0, lResult.Differing);
  AssertEquals('and the whole image was compared', 100, lResult.Pixels);
end;


procedure TTestSVGPixelComparison.TestABoxCountsOnlyWhatItHolds;

var
  lBlack: TFPColor;

begin
  lBlack := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  FActual.Colors[2, 2] := lBlack;
  FActual.Colors[8, 8] := lBlack;
  AssertEquals('both pixels differ over the whole image', 2,
    Compare(0).Differing);
  AssertEquals('one of them is inside the first box', 1,
    SVGCountDiffering(FActual, FExpected, 0, FWhite, 0, 0, 5, 5));
  AssertEquals('and the other is inside this one', 1,
    SVGCountDiffering(FActual, FExpected, 0, FWhite, 5, 5, 10, 10));
  AssertEquals('a box holding neither counts nothing', 0,
    SVGCountDiffering(FActual, FExpected, 0, FWhite, 3, 3, 8, 8));
end;


procedure TTestSVGPixelComparison.TestABoxIsTrimmedToTheImages;

begin
  FActual.Colors[9, 9] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  AssertEquals('a box reaching past the edge counts only the pixels inside it', 1,
    SVGCountDiffering(FActual, FExpected, 0, FWhite, -20, -20, 400, 400));
end;


procedure TTestSVGPixelComparison.TestABoxOfNoExtentCountsNothing;

begin
  FActual.Colors[4, 4] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  AssertEquals('a box of no width counts nothing', 0,
    SVGCountDiffering(FActual, FExpected, 0, FWhite, 4, 0, 4, 10));
  AssertEquals('nor does one of no height', 0,
    SVGCountDiffering(FActual, FExpected, 0, FWhite, 0, 4, 10, 4));
end;


procedure TTestSVGPixelComparison.TestOneChangedPixelIsCounted;

var
  lResult: TSVGComparison;

begin
  FActual.Colors[3, 4] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lResult := Compare(0);
  AssertTrue('the images differ', lResult.Outcome = crDiffers);
  AssertEquals('exactly one pixel differs', 1, lResult.Differing);
end;


procedure TTestSVGPixelComparison.TestDifferenceWithinToleranceIsIgnored;

var
  lResult: TSVGComparison;

begin
  FActual.Colors[3, 4] := TFPColor(TSVGColor.FromBytes(253, 255, 255, 255));
  lResult := Compare(2);
  AssertTrue('a difference of two is inside a tolerance of two',
    lResult.Outcome = crMatch);
end;


procedure TTestSVGPixelComparison.TestDifferenceBeyondToleranceCounts;

var
  lResult: TSVGComparison;

begin
  FActual.Colors[3, 4] := TFPColor(TSVGColor.FromBytes(252, 255, 255, 255));
  lResult := Compare(2);
  AssertEquals('a difference of three is outside it', 1, lResult.Differing);
end;


procedure TTestSVGPixelComparison.TestWorstChannelIsReported;

var
  lResult: TSVGComparison;

begin
  FActual.Colors[1, 1] := TFPColor(TSVGColor.FromBytes(255, 200, 255, 255));
  FActual.Colors[2, 2] := TFPColor(TSVGColor.FromBytes(255, 255, 100, 255));
  lResult := Compare(0);
  AssertEquals('the largest channel distance is kept', 155,
    lResult.MaxDifference);
end;


procedure TTestSVGPixelComparison.TestPercentageIsOfTheWholeImage;

var
  lResult: TSVGComparison;
  X: Integer;

begin
  for X := 0 to 4 do
    FActual.Colors[X, 0] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lResult := Compare(0);
  AssertEquals('five of a hundred pixels is five percent', 5.0,
    lResult.Percentage, Delta);
end;


procedure TTestSVGPixelComparison.TestSizeMismatchIsRefused;

var
  lResult: TSVGComparison;

begin
  FExpected.SetSize(8, 10);
  lResult := Compare(0);
  AssertTrue('images of different sizes cannot be compared',
    lResult.Outcome = crSizeMismatch);
  AssertEquals('and the report says so', 'size mismatch', lResult.ToString);
end;


procedure TTestSVGPixelComparison.TestTransparencyIsResolvedAgainstTheBackground;

var
  lResult: TSVGComparison;

begin
  Fill(FActual, TSVGColor.FromBytes(0, 0, 0, 0));
  lResult := Compare(0);
  AssertTrue('a transparent rendering matches a white reference',
    lResult.Outcome = crMatch);
end;


procedure TTestSVGPixelComparison.TestAlphaAloneIsNotADifference;

var
  lResult: TSVGComparison;

begin
  Fill(FActual, TSVGColor.FromBytes(255, 255, 255, 0));
  Fill(FExpected, TSVGColor.FromBytes(255, 255, 255, 255));
  lResult := Compare(0);
  AssertTrue('pixels that composite to the same colour are equal',
    lResult.Outcome = crMatch);
end;


procedure TTestSVGPixelComparison.TestThresholdAcceptsASmallDifference;

var
  lResult: TSVGComparison;

begin
  FActual.Colors[0, 0] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lResult := Compare(0);
  AssertTrue('one percent is within a threshold of two',
    lResult.Passes(2));
end;


procedure TTestSVGPixelComparison.TestThresholdRejectsALargeDifference;

var
  lResult: TSVGComparison;
  X: Integer;

begin
  for X := 0 to 9 do
    FActual.Colors[X, 0] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lResult := Compare(0);
  AssertFalse('ten percent is outside a threshold of two',
    lResult.Passes(2));
end;


procedure TTestSVGPixelComparison.TestMatchPassesEveryThreshold;

var
  lResult: TSVGComparison;

begin
  lResult := Compare(0);
  AssertTrue('an exact match needs no allowance', lResult.Passes(0));
end;


procedure TTestSVGPixelComparison.TestSizeMismatchPassesNothing;

var
  lResult: TSVGComparison;

begin
  FExpected.SetSize(8, 10);
  lResult := Compare(0);
  AssertFalse('a size mismatch cannot be allowed away',
    lResult.Passes(100));
end;


procedure TTestSVGPixelComparison.TestDiffMapMarksTheChangedPixel;

var
  lResult: TSVGComparison;
  lMark: TSVGColor;

begin
  FActual.Colors[3, 4] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lResult := SVGCompareImages(FActual, FExpected, 0, FWhite, FDiff);
  AssertEquals('one pixel differs', 1, lResult.Differing);
  lMark := TSVGColor(FDiff.Colors[3, 4]);
  AssertEquals('the changed pixel is marked in red', 65535, lMark.Red);
  AssertEquals('with no green', 0, lMark.Green);
  lMark := TSVGColor(FDiff.Colors[0, 0]);
  AssertTrue('an unchanged pixel is not marked', lMark.Red <> lMark.Green * 2);
end;


procedure TTestSVGPixelComparison.TestDiffMapIsSizedToTheImages;

begin
  SVGCompareImages(FActual, FExpected, 0, FWhite, FDiff);
  AssertEquals('the map took the width of the images', 10, FDiff.Width);
  AssertEquals('and the height', 10, FDiff.Height);
end;


procedure TTestSVGPixelComparison.TestADistanceMapIsWhiteWhereTheTwoAgree;

var
  lFar: Integer;

begin
  FActual.Colors[3, 4] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lFar := SVGDistanceImage(FActual, FExpected, FWhite, FDiff);
  AssertEquals('white against black is the whole of the range', 255, lFar);
  AssertEquals('the pixel that agrees is white', 65535,
    TSVGColor(FDiff.Colors[0, 0]).Red);
  AssertEquals('and the one that differs is black', 0,
    TSVGColor(FDiff.Colors[3, 4]).Red);
end;


procedure TTestSVGPixelComparison.TestADistanceMapSpreadsTheStepsOverTheWholeRange;

var
  lFar: Integer;

begin
  // Nothing here differs by more than sixty four, so sixty four is the
  // value drawn as the darkest grey, and half of it comes out halfway.
  FActual.Colors[3, 4] := TFPColor(TSVGColor.FromBytes(191, 255, 255, 255));
  FActual.Colors[5, 6] := TFPColor(TSVGColor.FromBytes(223, 255, 255, 255));
  lFar := SVGDistanceImage(FActual, FExpected, FWhite, FDiff);
  AssertEquals('the furthest apart is sixty four', 64, lFar);
  AssertEquals('which is drawn black', 0,
    TSVGColor(FDiff.Colors[3, 4]).Red);
  AssertEquals('and half of it halfway down the greys', 128,
    TSVGColor(FDiff.Colors[5, 6]).Red shr 8);
end;


procedure TTestSVGPixelComparison.TestADistanceMapOfTwoAlikeIsWhiteThroughout;

var
  lFar: Integer;

begin
  lFar := SVGDistanceImage(FActual, FExpected, FWhite, FDiff);
  AssertEquals('nothing differs', 0, lFar);
  AssertEquals('and the map is white', 65535,
    TSVGColor(FDiff.Colors[5, 5]).Red);
end;


procedure TTestSVGPixelComparison.TestComparingNothingIsRefused;

begin
  try
    SVGCompareImages(nil, FExpected, 0, FWhite, nil);
    Fail('comparing against nothing should raise');
  except
    on E: ESVGCompare do
      AssertTrue('the message reports the missing part',
        Pos('Nothing to compare', E.Message) > 0);
  end;
end;


initialization
  RegisterTest('diff', TTestSVGCompositing);
  RegisterTest('diff', TTestSVGPixelComparison);
end.
