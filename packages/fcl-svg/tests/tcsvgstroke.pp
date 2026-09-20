{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for dashing, stroke outlines, joins, caps and the miter limit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgstroke;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svggoldens, svgpixels, fpsvg.types,
     fpsvg.path, fpsvg.geom, fpsvg.raster, fpsvg.soft;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpimage, fpcunit, testregistry, svggoldens,
     svgpixels, fpsvg.types, fpsvg.path, fpsvg.geom, fpsvg.raster,
     fpsvg.soft;
{$ENDIF FPC_DOTTEDUNITS}

type
  { The vertices of a path, the positions where markers are placed. }
  TTestSVGPathVertices = class(TTestCase)
  private
    FPath: TSVGPath;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestALineHasAStartAndAnEnd;
    procedure TestEveryTurnIsAVertex;
    procedure TestACurveIsOneVertexAtItsEnd;
    procedure TestClosingComesBackToTheStart;
    procedure TestTheAngleBisectsTheTurn;
    procedure TestTheEndsTakeTheOneDirectionTheyHave;
    procedure TestAnEmptyPathHasNoVertices;
  end;

  TTestSVGDash = class(TTestCase)
  private
    FPath: TSVGPath;
    FSource: TSVGPolyPath;
    FDashed: TSVGPolyPath;
    // Flattens the path and cuts it into the given dash pattern.
    procedure Dash(const aPattern: array of Double; aOffset: Double);
    // The total length of all the dashed subpaths.
    function DashedLength: Double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNoPatternCopiesTheSource;
    procedure TestPatternCutsTheLine;
    procedure TestDashLengthsMatchThePattern;
    procedure TestGapsRemoveTheirLength;
    procedure TestOffsetShiftsThePattern;
    procedure TestOffsetLargerThanThePatternWraps;
    procedure TestOddPatternRepeats;
    procedure TestClosedSubPathDashesRoundTheLoop;
    procedure TestZeroPatternProducesNothing;
  end;

  TTestSVGStroke = class(TTestCase)
  private
    FPath: TSVGPath;
    FSource: TSVGPolyPath;
    FStroke: TSVGPolyPath;
    FRasterizer: TSVGRasterizer;
    FWidth, FHeight: Integer;
    FPixels: array of Integer;
    procedure CollectSpan(aY, aX, aCount: Integer; aCoverage: PByte);
    // Strokes the current path with the given pen, then rasterizes the
    // outline.
    procedure StrokeAndRaster(const aPen: TSVGPen);
    // The coverage of the whole grid, added up in whole pixels.
    function CoveredArea: Double;
    // The coverage of one pixel, zero outside the grid.
    function At(aX, aY: Integer): Integer;
    // A pen of the given width, join and cap.
    function Pen(aWidth: Double; aCap: TSVGLineCap;
      aJoin: TSVGLineJoin): TSVGPen;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestZeroWidthStrokesNothing;
    procedure TestStraightLineAreaIsLengthTimesWidth;
    procedure TestButtCapStopsAtTheEndpoint;
    procedure TestSquareCapExtendsByHalfTheWidth;
    procedure TestRoundCapAddsASemicircleAtEachEnd;
    procedure TestClosedSquareHasNoCaps;
    procedure TestMiterJoinReachesTheCorner;
    procedure TestMiterLimitFallsBackToBevel;
    procedure TestBevelJoinCutsTheCorner;
    procedure TestRoundJoinRoundsTheCorner;
    procedure TestOverlappingPiecesDoNotCancel;
    procedure TestSinglePointWithRoundCapIsADisc;
    procedure TestDashedStrokeAreaMatchesTheDashedLength;
  end;

  TTestSVGStrokeBackend = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FPath: TSVGPath;
    // Strokes the current path into a fresh frame.
    procedure Render(aSize: Integer; const aPen: TSVGPen;
      const aCTM: TSVGMatrix);
    // Strokes the current path in a colour, into a fresh frame.
    procedure RenderInk(aSize: Integer; const aPen: TSVGPen;
      const aColor: TSVGColor; aOpacity: Double);
    // The alpha channel of the frame, one character per pixel.
    function AlphaText: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStrokePaintsTheOutline;
    procedure TestStrokeLeavesTheInteriorAlone;
    procedure TestZeroWidthPenPaintsNothing;
    procedure TestStrokeWidthFollowsTheTransform;
    procedure TestDashedStrokeLeavesGaps;
    procedure TestStrokePaintsInItsOwnColour;
    procedure TestStrokeOpacityKeepsTheHue;
    procedure TestDashesTakeTheColour;
    procedure TestJoinDoesNotDarkenWhereItOverlaps;
    procedure TestStrokeGolden;
  end;

implementation

{ TTestSVGPathVertices }

procedure TTestSVGPathVertices.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGPathVertices.TearDown;

begin
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TTestSVGPathVertices.TestALineHasAStartAndAnEnd;

var
  lVertices: TSVGPathVertexArray;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  lVertices := SVGPathVertices(FPath);
  AssertEquals('a line runs between two vertices', 2, Length(lVertices));
  AssertTrue('the first is where it starts', lVertices[0].Kind = vkStart);
  AssertTrue('the last is where it ends', lVertices[1].Kind = vkEnd);
  AssertEquals('which is the point it was drawn to', 10.0,
    lVertices[1].Point.X, 1E-9);
end;


procedure TTestSVGPathVertices.TestEveryTurnIsAVertex;

var
  lVertices: TSVGPathVertexArray;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  FPath.LineTo(10, 10);
  lVertices := SVGPathVertices(FPath);
  AssertEquals('two segments give three vertices', 3, Length(lVertices));
  AssertTrue('the turn between them is a middle one',
    lVertices[1].Kind = vkMid);
end;


procedure TTestSVGPathVertices.TestACurveIsOneVertexAtItsEnd;

var
  lVertices: TSVGPathVertexArray;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(0, 10, 10, 10, 10, 0);
  lVertices := SVGPathVertices(FPath);
  AssertEquals('the controls of a curve are not vertices', 2,
    Length(lVertices));
  AssertEquals('and it ends where it was drawn to', 10.0,
    lVertices[1].Point.X, 1E-9);
  // The last control sits directly above the end point, so the curve
  // arrives moving up the page.
  AssertEquals('arriving along the control that shapes its end', -1.0,
    lVertices[1].InDir.Y, 1E-9);
end;


procedure TTestSVGPathVertices.TestClosingComesBackToTheStart;

var
  lVertices: TSVGPathVertexArray;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  FPath.LineTo(10, 10);
  FPath.Close;
  lVertices := SVGPathVertices(FPath);
  AssertEquals('closing draws one more segment, so one more vertex', 4,
    Length(lVertices));
  AssertEquals('back to where the subpath began', 0.0,
    lVertices[3].Point.X, 1E-9);
end;


procedure TTestSVGPathVertices.TestTheAngleBisectsTheTurn;

var
  lVertices: TSVGPathVertexArray;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  FPath.LineTo(10, 10);
  lVertices := SVGPathVertices(FPath);
  AssertEquals('the turn is halved', 45.0, lVertices[1].AutoAngle, 1E-6);
end;


procedure TTestSVGPathVertices.TestTheEndsTakeTheOneDirectionTheyHave;

var
  lVertices: TSVGPathVertexArray;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  FPath.LineTo(10, 10);
  lVertices := SVGPathVertices(FPath);
  AssertEquals('the first turns the way the path leaves', 0.0,
    lVertices[0].AutoAngle, 1E-6);
  AssertEquals('and the last the way it arrived', 90.0,
    lVertices[2].AutoAngle, 1E-6);
end;


procedure TTestSVGPathVertices.TestAnEmptyPathHasNoVertices;

var
  lVertices: TSVGPathVertexArray;

begin
  lVertices := SVGPathVertices(FPath);
  AssertEquals('nothing drawn is nowhere to put a marker', 0,
    Length(lVertices));
end;



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


{ TTestSVGDash }

procedure TTestSVGDash.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
  FSource := TSVGPolyPath.Create;
  FDashed := TSVGPolyPath.Create;
end;


procedure TTestSVGDash.TearDown;

begin
  FreeAndNil(FDashed);
  FreeAndNil(FSource);
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TTestSVGDash.Dash(const aPattern: array of Double; aOffset: Double);

var
  lPattern: TSVGDoubleArray;
  I: Integer;

begin
  SetLength(lPattern, Length(aPattern));
  for I := 0 to High(aPattern) do
    lPattern[I] := aPattern[I];
  FSource.Flatten(FPath, TSVGMatrix.Identity, 0.05);
  FDashed.BuildDashes(FSource, lPattern, aOffset);
end;


function TTestSVGDash.DashedLength: Double;

var
  I, J: Integer;
  lDX, lDY: Double;

begin
  Result := 0;
  for I := 0 to FDashed.SubPathCount - 1 do
    for J := 0 to FDashed.PointCount[I] - 2 do
      begin
      lDX := FDashed.Points[I, J + 1].X - FDashed.Points[I, J].X;
      lDY := FDashed.Points[I, J + 1].Y - FDashed.Points[I, J].Y;
      Result := Result + Sqrt(lDX * lDX + lDY * lDY);
      end;
end;


procedure TTestSVGDash.TestNoPatternCopiesTheSource;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  Dash([], 0);
  AssertEquals('an empty pattern keeps one subpath', 1, FDashed.SubPathCount);
  AssertEquals('an empty pattern keeps the whole length', 10,
    DashedLength, Delta);
end;


procedure TTestSVGDash.TestPatternCutsTheLine;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  Dash([2, 2], 0);
  AssertEquals('a ten unit line under a four unit period gives three dashes',
    3, FDashed.SubPathCount);
end;


procedure TTestSVGDash.TestDashLengthsMatchThePattern;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  Dash([2, 2], 0);
  AssertEquals('the first dash starts at the origin', 0,
    FDashed.Points[0, 0].X, Delta);
  AssertEquals('the first dash is two units long', 2,
    FDashed.Points[0, 1].X, Delta);
  AssertEquals('the second dash starts after the gap', 4,
    FDashed.Points[1, 0].X, Delta);
end;


procedure TTestSVGDash.TestGapsRemoveTheirLength;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(12, 0);
  Dash([2, 2], 0);
  AssertEquals('half of a twelve unit line survives an even pattern', 6,
    DashedLength, Delta);
end;


procedure TTestSVGDash.TestOffsetShiftsThePattern;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  Dash([2, 2], 1);
  AssertEquals('an offset of one shortens the first dash', 1,
    FDashed.Points[0, 1].X, Delta);
end;


procedure TTestSVGDash.TestOffsetLargerThanThePatternWraps;

var
  lPlain, lWrapped: Double;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  Dash([2, 2], 1);
  lPlain := FDashed.Points[0, 1].X;
  Dash([2, 2], 1 + 4 * 5);
  lWrapped := FDashed.Points[0, 1].X;
  AssertEquals('an offset of a whole number of periods changes nothing',
    lPlain, lWrapped, Delta);
end;


procedure TTestSVGDash.TestOddPatternRepeats;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(12, 0);
  Dash([3], 0);
  AssertEquals('an odd pattern alternates on and off in threes', 6,
    DashedLength, Delta);
end;


procedure TTestSVGDash.TestClosedSubPathDashesRoundTheLoop;

begin
  FPath.AddRect(0, 0, 10, 10, 0, 0);
  Dash([5, 5], 0);
  AssertEquals('half of the forty unit perimeter survives', 20,
    DashedLength, 1e-6);
end;


procedure TTestSVGDash.TestZeroPatternProducesNothing;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  Dash([0, 0], 0);
  AssertEquals('a pattern of zeros produces no dashes', 0,
    FDashed.SubPathCount);
end;


{ TTestSVGStroke }

procedure TTestSVGStroke.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
  FSource := TSVGPolyPath.Create;
  FStroke := TSVGPolyPath.Create;
  FRasterizer := TSVGRasterizer.Create;
  FRasterizer.OnSpan := @CollectSpan;
  FWidth := 32;
  FHeight := 32;
end;


procedure TTestSVGStroke.TearDown;

begin
  FreeAndNil(FRasterizer);
  FreeAndNil(FStroke);
  FreeAndNil(FSource);
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TTestSVGStroke.CollectSpan(aY, aX, aCount: Integer;
  aCoverage: PByte);

var
  I: Integer;

begin
  for I := 0 to aCount - 1 do
    FPixels[aY * FWidth + aX + I] := aCoverage[I];
end;


function TTestSVGStroke.Pen(aWidth: Double; aCap: TSVGLineCap;
  aJoin: TSVGLineJoin): TSVGPen;

begin
  Result := TSVGPen.Create(aWidth, aCap, aJoin);
end;


procedure TTestSVGStroke.StrokeAndRaster(const aPen: TSVGPen);

begin
  SetLength(FPixels, FWidth * FHeight);
  FillChar(FPixels[0], Length(FPixels) * SizeOf(Integer), 0);
  FSource.Flatten(FPath, TSVGMatrix.Identity, 0.02);
  FStroke.BuildStroke(FSource, aPen, 0.02);
  FRasterizer.SetClip(0, 0, FWidth, FHeight);
  FRasterizer.Rasterize(FStroke, frNonZero);
end;


function TTestSVGStroke.At(aX, aY: Integer): Integer;

begin
  if (aX < 0) or (aY < 0) or (aX >= FWidth) or (aY >= FHeight) then
    Result := 0
  else
    Result := FPixels[aY * FWidth + aX];
end;


function TTestSVGStroke.CoveredArea: Double;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to High(FPixels) do
    Result := Result + FPixels[I] / 255;
end;


procedure TTestSVGStroke.TestZeroWidthStrokesNothing;

begin
  FPath.MoveTo(4, 8);
  FPath.LineTo(24, 8);
  StrokeAndRaster(Pen(0, lcButt, ljMiter));
  AssertEquals('a pen of no width covers nothing', 0, CoveredArea, Delta);
end;


procedure TTestSVGStroke.TestStraightLineAreaIsLengthTimesWidth;

begin
  FPath.MoveTo(4, 8);
  FPath.LineTo(24, 8);
  StrokeAndRaster(Pen(4, lcButt, ljMiter));
  AssertEquals('a butt-capped line covers its length times its width', 20 * 4,
    CoveredArea, 0.05);
end;


procedure TTestSVGStroke.TestButtCapStopsAtTheEndpoint;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(24, 8);
  StrokeAndRaster(Pen(4, lcButt, ljMiter));
  AssertEquals('the pixel inside the end is covered', 255, At(23, 8));
  AssertEquals('the pixel past the butt cap is empty', 0, At(24, 8));
end;


procedure TTestSVGStroke.TestSquareCapExtendsByHalfTheWidth;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(24, 8);
  StrokeAndRaster(Pen(4, lcSquare, ljMiter));
  AssertEquals('a square cap covers the two pixels past the end', 255,
    At(25, 8));
  AssertEquals('a square cap stops after half the width', 0, At(26, 8));
  AssertEquals('the square caps add a square at each end', 16 * 4 + 2 * 4 * 2,
    CoveredArea, 0.05);
end;


procedure TTestSVGStroke.TestRoundCapAddsASemicircleAtEachEnd;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(24, 8);
  StrokeAndRaster(Pen(4, lcRound, ljMiter));
  AssertEquals('the round caps add a disc between them', 16 * 4 + Pi * 4,
    CoveredArea, 0.2);
end;


procedure TTestSVGStroke.TestClosedSquareHasNoCaps;

begin
  FPath.AddRect(8, 8, 12, 12, 0, 0);
  StrokeAndRaster(Pen(2, lcButt, ljMiter));
  AssertEquals('a closed mitred square covers its perimeter band',
    14 * 14 - 10 * 10, CoveredArea, 0.05);
end;


procedure TTestSVGStroke.TestMiterJoinReachesTheCorner;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(20, 8);
  FPath.LineTo(20, 20);
  StrokeAndRaster(Pen(4, lcButt, ljMiter));
  AssertEquals('the miter fills the outer corner', 255, At(21, 6));
end;


procedure TTestSVGStroke.TestMiterLimitFallsBackToBevel;

var
  lPen: TSVGPen;
  lMitred, lBevelled: Double;

begin
  FPath.MoveTo(4, 16);
  FPath.LineTo(20, 15);
  FPath.LineTo(4, 14);
  lPen := Pen(3, lcButt, ljMiter);
  lPen.MiterLimit := 100;
  StrokeAndRaster(lPen);
  lMitred := CoveredArea;
  lPen.MiterLimit := 1.2;
  StrokeAndRaster(lPen);
  lBevelled := CoveredArea;
  AssertTrue('a spike beyond the miter limit is cut back',
    lBevelled < lMitred);
end;


procedure TTestSVGStroke.TestBevelJoinCutsTheCorner;

var
  lMitred, lBevelled: Double;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(20, 8);
  FPath.LineTo(20, 20);
  StrokeAndRaster(Pen(4, lcButt, ljMiter));
  lMitred := CoveredArea;
  StrokeAndRaster(Pen(4, lcButt, ljBevel));
  lBevelled := CoveredArea;
  AssertTrue('a bevel covers less than a miter', lBevelled < lMitred);
  AssertEquals('the bevel cuts exactly the miter triangle', 2,
    lMitred - lBevelled, 0.05);
end;


procedure TTestSVGStroke.TestRoundJoinRoundsTheCorner;

var
  lBevelled, lRounded: Double;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(20, 8);
  FPath.LineTo(20, 20);
  StrokeAndRaster(Pen(4, lcButt, ljBevel));
  lBevelled := CoveredArea;
  StrokeAndRaster(Pen(4, lcButt, ljRound));
  lRounded := CoveredArea;
  AssertTrue('a round join covers more than a bevel', lRounded > lBevelled);
end;


procedure TTestSVGStroke.TestOverlappingPiecesDoNotCancel;

begin
  FPath.MoveTo(8, 8);
  FPath.LineTo(20, 8);
  FPath.LineTo(8, 8);
  StrokeAndRaster(Pen(4, lcButt, ljRound));
  AssertEquals('a doubled-back segment stays covered, not cancelled', 255,
    At(14, 8));
end;


procedure TTestSVGStroke.TestSinglePointWithRoundCapIsADisc;

begin
  FPath.MoveTo(16, 16);
  StrokeAndRaster(Pen(6, lcRound, ljMiter));
  AssertTrue('a lone point with a round cap never exceeds the disc area',
    CoveredArea <= Pi * 9);
  AssertTrue('the chord deficit of the approximated disc stays small',
    CoveredArea > Pi * 9 - 0.5);
  StrokeAndRaster(Pen(6, lcButt, ljMiter));
  AssertEquals('a lone point with a butt cap draws nothing', 0,
    CoveredArea, Delta);
end;


procedure TTestSVGStroke.TestDashedStrokeAreaMatchesTheDashedLength;

var
  lDashed: TSVGPolyPath;
  lPen: TSVGPen;
  I, J: Integer;
  lLength, lDX, lDY: Double;

begin
  FPath.AddCircle(16, 16, 10);
  lPen := Pen(3, lcButt, ljMiter);
  SetLength(lPen.Dashes, 2);
  lPen.Dashes[0] := 6;
  lPen.Dashes[1] := 4;
  SetLength(FPixels, FWidth * FHeight);
  FillChar(FPixels[0], Length(FPixels) * SizeOf(Integer), 0);
  FSource.Flatten(FPath, TSVGMatrix.Identity, 0.02);
  lDashed := TSVGPolyPath.Create;
  try
    lDashed.BuildDashes(FSource, lPen.Dashes, lPen.DashOffset);
    lLength := 0;
    for I := 0 to lDashed.SubPathCount - 1 do
      for J := 0 to lDashed.PointCount[I] - 2 do
        begin
        lDX := lDashed.Points[I, J + 1].X - lDashed.Points[I, J].X;
        lDY := lDashed.Points[I, J + 1].Y - lDashed.Points[I, J].Y;
        lLength := lLength + Sqrt(lDX * lDX + lDY * lDY);
        end;
    FStroke.BuildStroke(lDashed, lPen, 0.02);
  finally
    lDashed.Free;
  end;
  FRasterizer.SetClip(0, 0, FWidth, FHeight);
  FRasterizer.Rasterize(FStroke, frNonZero);
  AssertEquals('a butt-capped dashed stroke covers its length times its width',
    lLength * 3, CoveredArea, 0.5);
end;


{ TTestSVGStrokeBackend }

procedure TTestSVGStrokeBackend.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGStrokeBackend.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


procedure TTestSVGStrokeBackend.Render(aSize: Integer; const aPen: TSVGPen;
  const aCTM: TSVGMatrix);

begin
  FBackend.BeginFrame(aSize, aSize);
  FBackend.StrokePath(FPath, aCTM,
    TSVGPaint.CreateColor(TSVGColor.Black), aPen, 1);
  FBackend.EndFrame;
end;


procedure TTestSVGStrokeBackend.RenderInk(aSize: Integer; const aPen: TSVGPen;
  const aColor: TSVGColor; aOpacity: Double);

begin
  FBackend.BeginFrame(aSize, aSize);
  FBackend.StrokePath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(aColor), aPen, aOpacity);
  FBackend.EndFrame;
end;


procedure TTestSVGStrokeBackend.TestStrokePaintsInItsOwnColour;

var
  lPen: TSVGPen;

begin
  lPen := TSVGPen.Create(4, lcButt, ljMiter);
  FPath.MoveTo(4, 8);
  FPath.LineTo(12, 8);
  RenderInk(16, lPen, TSVGColor.FromBytes($20, $80, $C0, 255), 1);
  AssertPixel(Self, 'the stroke paints in the colour it was given',
    FBackend.Image, 8, 8, TSVGColor.FromBytes($20, $80, $C0, 255));
end;


procedure TTestSVGStrokeBackend.TestStrokeOpacityKeepsTheHue;

var
  lPen: TSVGPen;

begin
  lPen := TSVGPen.Create(4, lcButt, ljMiter);
  FPath.MoveTo(4, 8);
  FPath.LineTo(12, 8);
  RenderInk(16, lPen, TSVGColor.FromBytes($20, $80, $C0, 255), 0.5);
  AssertPixelNear(Self, 'half opacity halves the alpha and leaves the '
    + 'channels', FBackend.Image, 8, 8,
    TSVGColor.FromBytes($20, $80, $C0, 128), 1);
end;


procedure TTestSVGStrokeBackend.TestDashesTakeTheColour;

var
  lPen: TSVGPen;

begin
  lPen := TSVGPen.Create(4, lcButt, ljMiter);
  SetLength(lPen.Dashes, 2);
  lPen.Dashes[0] := 4;
  lPen.Dashes[1] := 4;
  FPath.MoveTo(0, 8);
  FPath.LineTo(16, 8);
  RenderInk(16, lPen, TSVGColor.FromBytes($20, $80, $C0, 255), 1);
  AssertPixel(Self, 'a dash paints in the stroke colour', FBackend.Image,
    2, 8, TSVGColor.FromBytes($20, $80, $C0, 255));
  AssertPixelClear(Self, 'and the gap after it is untouched', FBackend.Image,
    6, 8);
end;


procedure TTestSVGStrokeBackend.TestJoinDoesNotDarkenWhereItOverlaps;

var
  lPen: TSVGPen;

begin
  // The outline is a union of overlapping pieces. A join would show as a
  // darker patch if any piece were blended twice.
  lPen := TSVGPen.Create(4, lcButt, ljMiter);
  FPath.MoveTo(4, 4);
  FPath.LineTo(12, 4);
  FPath.LineTo(12, 12);
  RenderInk(16, lPen, TSVGColor.FromBytes(255, 0, 0, 255), 0.5);
  AssertPixelNear(Self, 'the corner is no darker than the run into it',
    FBackend.Image, 12, 4, TSVGColor.FromBytes(255, 0, 0, 128), 2);
  AssertPixelNear(Self, 'which is the value the straight part shows',
    FBackend.Image, 8, 4, TSVGColor.FromBytes(255, 0, 0, 128), 2);
end;


function TTestSVGStrokeBackend.AlphaText: TStringList;

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


procedure TTestSVGStrokeBackend.TestStrokePaintsTheOutline;

begin
  FPath.AddRect(4, 4, 16, 16, 0, 0);
  Render(24, TSVGPen.Create(4, lcButt, ljMiter), TSVGMatrix.Identity);
  AssertEquals('the band along the edge is painted', $FFFF,
    FBackend.Image.Colors[4, 12].Alpha);
end;


procedure TTestSVGStrokeBackend.TestStrokeLeavesTheInteriorAlone;

begin
  FPath.AddRect(4, 4, 16, 16, 0, 0);
  Render(24, TSVGPen.Create(4, lcButt, ljMiter), TSVGMatrix.Identity);
  AssertEquals('the interior of a stroked rectangle stays empty', 0,
    FBackend.Image.Colors[12, 12].Alpha);
end;


procedure TTestSVGStrokeBackend.TestZeroWidthPenPaintsNothing;

begin
  FPath.AddRect(4, 4, 16, 16, 0, 0);
  Render(24, TSVGPen.Create(0, lcButt, ljMiter), TSVGMatrix.Identity);
  AssertEquals('a pen of no width paints nothing', 0,
    FBackend.Image.Colors[4, 12].Alpha);
end;


procedure TTestSVGStrokeBackend.TestStrokeWidthFollowsTheTransform;

begin
  FPath.MoveTo(2, 8);
  FPath.LineTo(14, 8);
  Render(32, TSVGPen.Create(2, lcButt, ljMiter), TSVGMatrix.Scaling(2, 2));
  AssertEquals('the scaled pen covers the doubled half width', $FFFF,
    FBackend.Image.Colors[16, 15].Alpha);
  AssertEquals('the scaled pen stops at the doubled half width', 0,
    FBackend.Image.Colors[16, 18].Alpha);
end;


procedure TTestSVGStrokeBackend.TestDashedStrokeLeavesGaps;

var
  lPen: TSVGPen;

begin
  FPath.MoveTo(1, 8);
  FPath.LineTo(31, 8);
  lPen := TSVGPen.Create(4, lcButt, ljMiter);
  SetLength(lPen.Dashes, 2);
  lPen.Dashes[0] := 4;
  lPen.Dashes[1] := 4;
  Render(32, lPen, TSVGMatrix.Identity);
  AssertEquals('the first dash is painted', $FFFF,
    FBackend.Image.Colors[3, 8].Alpha);
  AssertEquals('the first gap is empty', 0,
    FBackend.Image.Colors[7, 8].Alpha);
  AssertEquals('the second dash is painted', $FFFF,
    FBackend.Image.Colors[11, 8].Alpha);
end;


procedure TTestSVGStrokeBackend.TestStrokeGolden;

var
  lLines, lBlock: TStringList;
  lPen: TSVGPen;

begin
  lLines := TStringList.Create;
  try
    FPath.MoveTo(4, 20);
    FPath.LineTo(16, 5);
    FPath.LineTo(28, 20);
    Render(32, TSVGPen.Create(5, lcButt, ljMiter), TSVGMatrix.Identity);
    lLines.Add('# mitred chevron');
    lBlock := AlphaText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    Render(32, TSVGPen.Create(5, lcRound, ljRound), TSVGMatrix.Identity);
    lLines.Add('# round join and cap');
    lBlock := AlphaText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    FPath.Clear;
    FPath.MoveTo(2, 8);
    FPath.LineTo(30, 8);
    FPath.MoveTo(2, 16);
    FPath.LineTo(30, 16);
    lPen := TSVGPen.Create(4, lcButt, ljMiter);
    SetLength(lPen.Dashes, 2);
    lPen.Dashes[0] := 6;
    lPen.Dashes[1] := 3;
    Render(32, lPen, TSVGMatrix.Identity);
    lLines.Add('# dashed lines');
    lBlock := AlphaText;
    lLines.AddStrings(lBlock);
    lBlock.Free;

    AssertGolden(Self, 'stroke', lLines);
  finally
    lLines.Free;
  end;
end;


initialization
  RegisterTest('stroke', TTestSVGPathVertices);
  RegisterTest('dash', TTestSVGDash);
  RegisterTest('stroke', TTestSVGStroke);
  RegisterTest('stroke', TTestSVGStrokeBackend);
end.
