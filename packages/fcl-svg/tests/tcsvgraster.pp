{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for flattening, the scanline rasterizer and the software backend.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgraster;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svggoldens, svgpixels, fpsvg.types,
     fpsvg.path, fpsvg.backend, fpsvg.geom, fpsvg.raster, fpsvg.soft;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpimage, fpcunit, testregistry, svggoldens,
     svgpixels, fpsvg.types, fpsvg.path, fpsvg.backend, fpsvg.geom,
     fpsvg.raster, fpsvg.soft;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGFlatten = class(TTestCase)
  private
    FPath: TSVGPath;
    FPoly: TSVGPolyPath;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyPathFlattensToNothing;
    procedure TestLineKeepsItsEndpoints;
    procedure TestRepeatedPointIsDropped;
    procedure TestCloseIsRecorded;
    procedure TestCubicSubdivides;
    procedure TestTighterToleranceSubdividesMore;
    procedure TestFlattenedCurveStaysOnTheCurve;
    procedure TestTransformIsApplied;
    procedure TestBoundsCoverEveryPoint;
    procedure TestSubdivisionIsBounded;
  end;

  TTestSVGRasterizer = class(TTestCase)
  private
    FPath: TSVGPath;
    FPoly: TSVGPolyPath;
    FRasterizer: TSVGRasterizer;
    FWidth, FHeight: Integer;
    FPixels: array of Integer;
    procedure CollectSpan(aY, aX, aCount: Integer; aCoverage: PByte);
    // Rasterizes the current path into the coverage grid.
    procedure Run(aRule: TSVGFillRule);
    // The coverage of the whole grid, added up in whole pixels.
    function CoveredArea: Double;
    // The coverage of one pixel, zero outside the grid.
    function At(aX, aY: Integer): Integer;
    // The grid, one character per pixel.
    function AsText: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWholePixelRectangleIsOpaque;
    procedure TestRectangleAreaMatchesItsGeometry;
    procedure TestHalfPixelColumnIsHalfCovered;
    procedure TestQuarterPixelIsQuarterCovered;
    procedure TestTriangleAreaMatchesItsGeometry;
    procedure TestCircleAreaMatchesItsGeometry;
    procedure TestNonZeroFillsNestedSameWinding;
    procedure TestEvenOddPunchesNestedHole;
    procedure TestNonZeroPunchesOppositeWinding;
    procedure TestOutsideTheClipIsDropped;
    procedure TestEmptyPathEmitsNothing;
    procedure TestDegenerateSliverEmitsNothing;
    procedure TestCoverageGolden;
    procedure TestSubSamplesStartsAtTheDefault;
    procedure TestSubSamplesIsHeldInRange;
    procedure TestMoreSubSamplesReadASlopeMoreClosely;
  end;

  TTestSVGSoftBackend = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FPath: TSVGPath;
    // Renders a filled path into a new frame of the given size.
    procedure RenderFill(aWidth, aHeight: Integer; const aColor: TSVGColor;
      aOpacity: Double);
    procedure FillOutsideFrame;
    // The alpha channel of the frame, one character per pixel.
    function AlphaText: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBackendIsRegistered;
    procedure TestFrameCreatesASurface;
    procedure TestFillPaintsTheInterior;
    procedure TestFillLeavesTheOutsideAlone;
    procedure TestOpacityScalesTheAlpha;
    procedure TestPaintNoneDrawsNothing;
    procedure TestSecondFillCompositesOverTheFirst;
    procedure TestFillOutsideAFrameIsRejected;
    procedure TestExternalTargetIsNotOwned;
    procedure TestFillPaintsInItsOwnColour;
    procedure TestOpacityKeepsTheChannels;
    procedure TestAntialiasedEdgeKeepsTheHue;
    procedure TestSecondFillCoversTheFirst;
    procedure TestRenderGolden;
  end;

implementation

const
  Delta = 1e-9;
  Shades = ' .:-=+*#%@';

// One character for each coverage value, densest last.
function ShadeOf(aValue: Integer): Char;

var
  lIndex: Integer;

begin
  if aValue <= 0 then
    Exit(' ');
  lIndex := 1 + (aValue * (Length(Shades) - 1)) div 255;
  if lIndex > Length(Shades) then
    lIndex := Length(Shades);
  Result := Shades[lIndex];
end;


{ TTestSVGFlatten }

procedure TTestSVGFlatten.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
  FPoly := TSVGPolyPath.Create;
end;


procedure TTestSVGFlatten.TearDown;

begin
  FreeAndNil(FPoly);
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TTestSVGFlatten.TestEmptyPathFlattensToNothing;

begin
  FPoly.Flatten(FPath, TSVGMatrix.Identity, SVGDefaultFlatness);
  AssertEquals('an empty path yields no subpaths', 0, FPoly.SubPathCount);
  AssertTrue('an empty flattening reports itself empty', FPoly.IsEmpty);
  FPoly.Flatten(nil, TSVGMatrix.Identity, SVGDefaultFlatness);
  AssertEquals('a nil path yields no subpaths', 0, FPoly.SubPathCount);
end;


procedure TTestSVGFlatten.TestLineKeepsItsEndpoints;

begin
  FPath.MoveTo(1, 2);
  FPath.LineTo(9, 4);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, SVGDefaultFlatness);
  AssertEquals('a line is one subpath', 1, FPoly.SubPathCount);
  AssertEquals('a line keeps two points', 2, FPoly.PointCount[0]);
  AssertEquals('the start is kept', 1, FPoly.Points[0, 0].X, Delta);
  AssertEquals('the end is kept', 9, FPoly.Points[0, 1].X, Delta);
end;


procedure TTestSVGFlatten.TestRepeatedPointIsDropped;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(0, 0);
  FPath.LineTo(5, 0);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, SVGDefaultFlatness);
  AssertEquals('a repeated point adds nothing', 2, FPoly.PointCount[0]);
end;


procedure TTestSVGFlatten.TestCloseIsRecorded;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(5, 0);
  FPath.Close;
  FPoly.Flatten(FPath, TSVGMatrix.Identity, SVGDefaultFlatness);
  AssertTrue('the subpath is marked closed', FPoly.Closed[0]);
end;


procedure TTestSVGFlatten.TestCubicSubdivides;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(0, 40, 40, 40, 40, 0);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, SVGDefaultFlatness);
  AssertTrue('a curved cubic becomes several segments',
    FPoly.PointCount[0] > 4);
end;


procedure TTestSVGFlatten.TestTighterToleranceSubdividesMore;

var
  lCoarse, lFine: Integer;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(0, 40, 40, 40, 40, 0);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, 2);
  lCoarse := FPoly.PointCount[0];
  FPoly.Flatten(FPath, TSVGMatrix.Identity, 0.05);
  lFine := FPoly.PointCount[0];
  AssertTrue('a tighter tolerance produces more segments', lFine > lCoarse);
end;


procedure TTestSVGFlatten.TestFlattenedCurveStaysOnTheCurve;

var
  I: Integer;
  lT, lX, lY, lBest, lDX, lDY: Double;
  J: Integer;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(0, 40, 40, 40, 40, 0);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, 0.1);
  for I := 0 to FPoly.PointCount[0] - 1 do
    begin
    lBest := 1e30;
    for J := 0 to 400 do
      begin
      lT := J / 400;
      lX := 3 * lT * Sqr(1 - lT) * 0 + 3 * Sqr(lT) * (1 - lT) * 40
        + lT * lT * lT * 40;
      lY := 3 * lT * Sqr(1 - lT) * 40 + 3 * Sqr(lT) * (1 - lT) * 40;
      lDX := FPoly.Points[0, I].X - lX;
      lDY := FPoly.Points[0, I].Y - lY;
      lBest := Min(lBest, Sqrt(lDX * lDX + lDY * lDY));
      end;
    AssertTrue('every flattened point lies within the tolerance of the curve',
      lBest < 0.2);
    end;
end;


procedure TTestSVGFlatten.TestTransformIsApplied;

begin
  FPath.MoveTo(1, 1);
  FPath.LineTo(2, 2);
  FPoly.Flatten(FPath, TSVGMatrix.Scaling(10, 10), SVGDefaultFlatness);
  AssertEquals('the CTM scales the first point', 10,
    FPoly.Points[0, 0].X, Delta);
  AssertEquals('the CTM scales the last point', 20,
    FPoly.Points[0, 1].Y, Delta);
end;


procedure TTestSVGFlatten.TestBoundsCoverEveryPoint;

var
  lBounds: TSVGRect;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(0, 40, 40, 40, 40, 0);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, 0.1);
  lBounds := FPoly.Bounds;
  AssertEquals('the bounds start at the first point', 0, lBounds.Left, Delta);
  AssertEquals('the bounds reach the last point', 40, lBounds.Right, Delta);
  AssertTrue('the bounds cover the bulge', lBounds.Bottom > 25);
end;


procedure TTestSVGFlatten.TestSubdivisionIsBounded;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(1e6, 1e6, -1e6, 1e6, 0, 0.001);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, 1e-9);
  AssertTrue('the recursion depth limit bounds the point count',
    FPoly.PointCount[0] < 200000);
end;


{ TTestSVGRasterizer }

procedure TTestSVGRasterizer.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
  FPoly := TSVGPolyPath.Create;
  FRasterizer := TSVGRasterizer.Create;
  FRasterizer.OnSpan := @CollectSpan;
  FWidth := 16;
  FHeight := 16;
end;


procedure TTestSVGRasterizer.TearDown;

begin
  FreeAndNil(FRasterizer);
  FreeAndNil(FPoly);
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TTestSVGRasterizer.CollectSpan(aY, aX, aCount: Integer;
  aCoverage: PByte);

var
  I: Integer;

begin
  for I := 0 to aCount - 1 do
    FPixels[aY * FWidth + aX + I] := aCoverage[I];
end;


procedure TTestSVGRasterizer.Run(aRule: TSVGFillRule);

begin
  SetLength(FPixels, FWidth * FHeight);
  FillChar(FPixels[0], Length(FPixels) * SizeOf(Integer), 0);
  FPoly.Flatten(FPath, TSVGMatrix.Identity, 0.05);
  FRasterizer.SetClip(0, 0, FWidth, FHeight);
  FRasterizer.Rasterize(FPoly, aRule);
end;


function TTestSVGRasterizer.At(aX, aY: Integer): Integer;

begin
  if (aX < 0) or (aY < 0) or (aX >= FWidth) or (aY >= FHeight) then
    Result := 0
  else
    Result := FPixels[aY * FWidth + aX];
end;


function TTestSVGRasterizer.CoveredArea: Double;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to High(FPixels) do
    Result := Result + FPixels[I] / 255;
end;


function TTestSVGRasterizer.AsText: TStringList;

var
  X, Y: Integer;
  lLine: String;

begin
  Result := TStringList.Create;
  for Y := 0 to FHeight - 1 do
    begin
    lLine := '';
    for X := 0 to FWidth - 1 do
      lLine := lLine + ShadeOf(At(X, Y));
    Result.Add(TrimRight(lLine));
    end;
end;


procedure TTestSVGRasterizer.TestWholePixelRectangleIsOpaque;

begin
  FPath.AddRect(2, 3, 6, 5, 0, 0);
  Run(frNonZero);
  AssertEquals('a pixel inside the rectangle is fully covered', 255,
    At(4, 5));
  AssertEquals('the first covered column is full', 255, At(2, 3));
  AssertEquals('the last covered column is full', 255, At(7, 7));
  AssertEquals('the column past the edge is untouched', 0, At(8, 5));
  AssertEquals('the row above the edge is untouched', 0, At(4, 2));
end;


procedure TTestSVGRasterizer.TestRectangleAreaMatchesItsGeometry;

begin
  FPath.AddRect(2.25, 3.5, 6.5, 5.25, 0, 0);
  Run(frNonZero);
  AssertEquals('the coverage sums to the rectangle area', 6.5 * 5.25,
    CoveredArea, 0.02);
end;


procedure TTestSVGRasterizer.TestHalfPixelColumnIsHalfCovered;

begin
  FPath.AddRect(2, 0, 0.5, 4, 0, 0);
  Run(frNonZero);
  AssertEquals('a half covered pixel reports half coverage', 128,
    At(2, 1), 2);
end;


procedure TTestSVGRasterizer.TestQuarterPixelIsQuarterCovered;

begin
  FPath.AddRect(2, 1, 0.5, 0.5, 0, 0);
  Run(frNonZero);
  AssertEquals('a quarter covered pixel reports a quarter', 64,
    At(2, 1), 4);
end;


procedure TTestSVGRasterizer.TestTriangleAreaMatchesItsGeometry;

begin
  FPath.MoveTo(1, 1);
  FPath.LineTo(11, 1);
  FPath.LineTo(1, 9);
  FPath.Close;
  Run(frNonZero);
  AssertEquals('the coverage sums to the triangle area', 10 * 8 / 2,
    CoveredArea, 0.1);
end;


procedure TTestSVGRasterizer.TestCircleAreaMatchesItsGeometry;

begin
  FPath.AddCircle(8, 8, 6);
  Run(frNonZero);
  AssertTrue('a flattened circle never exceeds the true circle area',
    CoveredArea <= Pi * 36);
  AssertTrue('the chord deficit of the flattened circle stays small',
    CoveredArea > Pi * 36 - 1.5);
end;


procedure TTestSVGRasterizer.TestNonZeroFillsNestedSameWinding;

begin
  FPath.AddRect(1, 1, 12, 12, 0, 0);
  FPath.AddRect(4, 4, 6, 6, 0, 0);
  Run(frNonZero);
  AssertEquals('the nonzero rule fills the inner rectangle too', 255,
    At(7, 7));
  AssertEquals('the nonzero rule fills the whole outer rectangle', 144,
    CoveredArea, 0.05);
end;


procedure TTestSVGRasterizer.TestEvenOddPunchesNestedHole;

begin
  FPath.AddRect(1, 1, 12, 12, 0, 0);
  FPath.AddRect(4, 4, 6, 6, 0, 0);
  Run(frEvenOdd);
  AssertEquals('the even-odd rule leaves the inner rectangle empty', 0,
    At(7, 7));
  AssertEquals('the even-odd rule fills only the ring', 144 - 36,
    CoveredArea, 0.05);
end;


procedure TTestSVGRasterizer.TestNonZeroPunchesOppositeWinding;

var
  lPoints: TSVGPointArray;

begin
  FPath.AddRect(1, 1, 12, 12, 0, 0);
  SetLength(lPoints, 4);
  lPoints[0] := TSVGPoint.Create(4, 4);
  lPoints[1] := TSVGPoint.Create(4, 10);
  lPoints[2] := TSVGPoint.Create(10, 10);
  lPoints[3] := TSVGPoint.Create(10, 4);
  FPath.AddPolygon(lPoints, True);
  Run(frNonZero);
  AssertEquals('an opposite winding cancels under the nonzero rule', 0,
    At(7, 7));
  AssertEquals('only the ring survives', 144 - 36, CoveredArea, 0.05);
end;


procedure TTestSVGRasterizer.TestOutsideTheClipIsDropped;

begin
  FPath.AddRect(-20, -20, 10, 10, 0, 0);
  Run(frNonZero);
  AssertEquals('a shape entirely left of the clip covers nothing', 0,
    CoveredArea, Delta);
  FPath.Clear;
  FPath.AddRect(-4, -4, 8, 8, 0, 0);
  Run(frNonZero);
  AssertEquals('a shape straddling the clip covers only the visible part',
    16, CoveredArea, 0.05);
end;


procedure TTestSVGRasterizer.TestEmptyPathEmitsNothing;

begin
  Run(frNonZero);
  AssertEquals('an empty path covers nothing', 0, CoveredArea, Delta);
end;


procedure TTestSVGRasterizer.TestDegenerateSliverEmitsNothing;

begin
  FPath.MoveTo(2, 2);
  FPath.LineTo(8, 2);
  FPath.LineTo(2, 2);
  FPath.Close;
  Run(frNonZero);
  AssertEquals('a zero-height sliver covers nothing', 0, CoveredArea, Delta);
end;


procedure TTestSVGRasterizer.TestSubSamplesStartsAtTheDefault;

begin
  AssertEquals('a new rasterizer reads a row on the usual lines',
    SVGSubSamples, FRasterizer.SubSamples);
end;


procedure TTestSVGRasterizer.TestSubSamplesIsHeldInRange;

begin
  FRasterizer.SubSamples := 0;
  AssertEquals('nothing at all is held up to the least there may be',
    SVGMinSubSamples, FRasterizer.SubSamples);
  FRasterizer.SubSamples := 100000;
  AssertEquals('and more than there may be is held down to it',
    SVGMaxSubSamples, FRasterizer.SubSamples);
end;


procedure TTestSVGRasterizer.TestMoreSubSamplesReadASlopeMoreClosely;

begin
  // The top edge lies a quarter of the way down the row, so three
  // quarters of that row is covered. One line to a row is drawn at the
  // middle of it and finds the whole row inside the shape; the area
  // comes out right either way, so it is the row that tells them apart.
  FRasterizer.SubSamples := 1;
  FPath.AddRect(2, 1.25, 6, 3.75, 0, 0);
  Run(frNonZero);
  AssertEquals('one line to a row reads the row as wholly covered', 255,
    At(4, 1));
  FPath.Clear;
  FRasterizer.SubSamples := 64;
  FPath.AddRect(2, 1.25, 6, 3.75, 0, 0);
  Run(frNonZero);
  AssertEquals('sixty four of them read the three quarters that is there',
    191, At(4, 1), 3);
  AssertEquals('and the row below is whole in both', 255, At(4, 2));
end;


procedure TTestSVGRasterizer.TestCoverageGolden;

var
  lLines, lBlock: TStringList;

begin
  lLines := TStringList.Create;
  try
    FPath.AddCircle(8, 8, 6);
    Run(frNonZero);
    lLines.Add('# circle nonzero');
    lBlock := AsText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    FPath.Clear;
    FPath.MoveTo(1, 1);
    FPath.LineTo(14, 4);
    FPath.LineTo(6, 14);
    FPath.Close;
    Run(frNonZero);
    lLines.Add('# triangle nonzero');
    lBlock := AsText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    FPath.Clear;
    FPath.AddRect(1, 1, 12, 12, 0, 0);
    FPath.AddRect(4, 4, 6, 6, 0, 0);
    Run(frEvenOdd);
    lLines.Add('# nested even-odd');
    lBlock := AsText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    AssertGolden(Self, 'coverage', lLines);
  finally
    lLines.Free;
  end;
end;


{ TTestSVGSoftBackend }

procedure TTestSVGSoftBackend.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGSoftBackend.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


procedure TTestSVGSoftBackend.RenderFill(aWidth, aHeight: Integer;
  const aColor: TSVGColor; aOpacity: Double);

begin
  FBackend.BeginFrame(aWidth, aHeight);
  FBackend.FillPath(FPath, TSVGMatrix.Identity, TSVGPaint.CreateColor(aColor),
    frNonZero, aOpacity);
  FBackend.EndFrame;
end;


procedure TTestSVGSoftBackend.FillOutsideFrame;

begin
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.Black), frNonZero, 1);
end;


function TTestSVGSoftBackend.AlphaText: TStringList;

var
  X, Y: Integer;
  lLine: String;

begin
  Result := TStringList.Create;
  for Y := 0 to FBackend.Image.Height - 1 do
    begin
    lLine := '';
    for X := 0 to FBackend.Image.Width - 1 do
      lLine := lLine + ShadeOf(FBackend.Image.Colors[X, Y].Alpha div 257);
    Result.Add(TrimRight(lLine));
    end;
end;


procedure TTestSVGSoftBackend.TestBackendIsRegistered;

begin
  AssertNotNull('the software backend registers itself',
    SVGBackends.FindBackend('software'));
end;


procedure TTestSVGSoftBackend.TestFrameCreatesASurface;

begin
  FBackend.BeginFrame(12, 9);
  try
    AssertNotNull('the frame creates a surface', FBackend.Image);
    AssertEquals('the surface has the frame width', 12, FBackend.Image.Width);
    AssertEquals('the surface has the frame height', 9, FBackend.Image.Height);
  finally
    FBackend.EndFrame;
  end;
end;


procedure TTestSVGSoftBackend.TestFillPaintsTheInterior;

begin
  FPath.AddRect(2, 2, 6, 6, 0, 0);
  RenderFill(12, 12, TSVGColor.FromBytes(255, 0, 0, 255), 1);
  AssertEquals('the interior takes the fill colour', $FFFF,
    FBackend.Image.Colors[4, 4].Red);
  AssertEquals('the interior is opaque', $FFFF,
    FBackend.Image.Colors[4, 4].Alpha);
end;


procedure TTestSVGSoftBackend.TestFillLeavesTheOutsideAlone;

begin
  FPath.AddRect(2, 2, 6, 6, 0, 0);
  RenderFill(12, 12, TSVGColor.FromBytes(255, 0, 0, 255), 1);
  AssertEquals('a pixel outside the shape keeps its alpha', 0,
    FBackend.Image.Colors[10, 10].Alpha);
end;


procedure TTestSVGSoftBackend.TestOpacityScalesTheAlpha;

begin
  FPath.AddRect(2, 2, 6, 6, 0, 0);
  RenderFill(12, 12, TSVGColor.FromBytes(255, 0, 0, 255), 0.5);
  AssertEquals('half opacity halves the alpha', $8000,
    FBackend.Image.Colors[4, 4].Alpha, 600);
end;


procedure TTestSVGSoftBackend.TestPaintNoneDrawsNothing;

begin
  FPath.AddRect(2, 2, 6, 6, 0, 0);
  FBackend.BeginFrame(12, 12);
  FBackend.FillPath(FPath, TSVGMatrix.Identity, TSVGPaint.None, frNonZero, 1);
  FBackend.EndFrame;
  AssertEquals('a paint of none leaves the surface untouched', 0,
    FBackend.Image.Colors[4, 4].Alpha);
end;


procedure TTestSVGSoftBackend.TestSecondFillCompositesOverTheFirst;

begin
  FPath.AddRect(2, 2, 6, 6, 0, 0);
  FBackend.BeginFrame(12, 12);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255)), frNonZero, 1);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 0, 255, 255)), frNonZero, 1);
  FBackend.EndFrame;
  AssertEquals('the second opaque fill replaces the first (red)', 0,
    FBackend.Image.Colors[4, 4].Red);
  AssertEquals('the second opaque fill replaces the first (blue)', $FFFF,
    FBackend.Image.Colors[4, 4].Blue);
end;


procedure TTestSVGSoftBackend.TestFillOutsideAFrameIsRejected;

begin
  FPath.AddRect(0, 0, 4, 4, 0, 0);
  AssertException('filling outside a frame is rejected', ESVGSoft,
    @FillOutsideFrame);
end;


procedure TTestSVGSoftBackend.TestExternalTargetIsNotOwned;

var
  lImage: TFPMemoryImage;

begin
  lImage := TFPMemoryImage.Create(8, 8);
  try
    FBackend.SetTarget(lImage);
    FPath.AddRect(1, 1, 4, 4, 0, 0);
    RenderFill(8, 8, TSVGColor.FromBytes(0, 255, 0, 255), 1);
    AssertEquals('the external surface receives the fill', $FFFF,
      lImage.Colors[3, 3].Green);
    FreeAndNil(FBackend);
    AssertEquals('freeing the backend leaves the external surface alive', 8,
      lImage.Width);
  finally
    lImage.Free;
  end;
end;


procedure TTestSVGSoftBackend.TestFillPaintsInItsOwnColour;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  RenderFill(16, 16, TSVGColor.FromBytes($20, $80, $C0, 255), 1);
  AssertPixel(Self, 'the fill reaches the surface unchanged', FBackend.Image,
    8, 8, TSVGColor.FromBytes($20, $80, $C0, 255));
end;


procedure TTestSVGSoftBackend.TestOpacityKeepsTheChannels;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  RenderFill(16, 16, TSVGColor.FromBytes($20, $80, $C0, 255), 0.5);
  AssertPixelNear(Self, 'opacity belongs to the alpha alone', FBackend.Image,
    8, 8, TSVGColor.FromBytes($20, $80, $C0, 128), 1);
end;


procedure TTestSVGSoftBackend.TestAntialiasedEdgeKeepsTheHue;

begin
  FPath.AddRect(2.5, 2, 11.5, 12, 0, 0);
  RenderFill(16, 16, TSVGColor.FromBytes($20, $80, $C0, 255), 1);
  AssertPixelNear(Self, 'a half covered edge is the colour at half alpha',
    FBackend.Image, 2, 8, TSVGColor.FromBytes($20, $80, $C0, 128), 2);
end;


procedure TTestSVGSoftBackend.TestSecondFillCoversTheFirst;

begin
  FPath.AddRect(0, 0, 16, 16, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255)), frNonZero, 1);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 0, 255, 255)), frNonZero, 1);
  FBackend.EndFrame;
  AssertPixel(Self, 'an opaque fill replaces the pixels under it',
    FBackend.Image, 8, 8, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGSoftBackend.TestRenderGolden;

var
  lLines, lBlock: TStringList;

begin
  lLines := TStringList.Create;
  try
    FPath.AddCircle(8, 8, 6.5);
    RenderFill(16, 16, TSVGColor.FromBytes(0, 0, 0, 255), 1);
    lLines.Add('# circle alpha');
    lBlock := AlphaText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    FPath.Clear;
    FPath.AddRect(2, 2, 11.5, 7.25, 2, 2);
    RenderFill(16, 16, TSVGColor.FromBytes(0, 0, 0, 255), 0.5);
    lLines.Add('# rounded rect at half opacity');
    lBlock := AlphaText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    AssertGolden(Self, 'render', lLines);
  finally
    lLines.Free;
  end;
end;


initialization
  RegisterTest('geom', TTestSVGFlatten);
  RegisterTest('raster', TTestSVGRasterizer);
  RegisterTest('soft', TTestSVGSoftBackend);
end.
