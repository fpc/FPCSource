{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the path data grammar, arc conversion and the basic shapes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgpath;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Math, FpcUnit.Test, FpcUnit.Registry,
     fpsvg.types, fpsvg.path;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, math, fpcunit, testregistry, fpsvg.types, fpsvg.path;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSVGPathTestCase = class(TTestCase)
  protected
    FPath: TSVGPath;
    procedure SetUp; override;
    procedure TearDown; override;
    // Fails unless the segment at aIndex has the given kind.
    procedure AssertKind(const aMessage: String; aIndex: Integer;
      aKind: TSVGPathSegmentKind);
    // Fails unless a point of a segment has the given coordinates.
    procedure AssertPoint(const aMessage: String; aIndex, aPoint: Integer;
      aX, aY: Double);
    // Fails unless the current point of the path is the given one.
    procedure AssertCurrent(const aMessage: String; aX, aY: Double);
  end;

  TTestSVGPathGrammar = class(TSVGPathTestCase)
  published
    procedure TestEmptyDataIsAnEmptyPath;
    procedure TestAbsoluteMoveAndLine;
    procedure TestRelativeCommandsAccumulate;
    procedure TestImplicitLineToAfterMoveTo;
    procedure TestImplicitRepeatKeepsCommand;
    procedure TestHorizontalAndVertical;
    procedure TestCloseReturnsToStart;
    procedure TestRelativeMoveAfterCloseStartsAtSubpathStart;
    procedure TestFirstMoveIsAlwaysAbsolute;
    procedure TestQuadraticBecomesCubic;
    procedure TestSmoothCubicReflectsControl;
    procedure TestSmoothCubicWithoutPredecessorUsesCurrentPoint;
    procedure TestSmoothQuadReflectsControl;
    procedure TestNoSeparatorBetweenNegativeNumbers;
    procedure TestExponentInCoordinate;
    procedure TestDataMustStartWithMoveTo;
    procedure TestUnknownCommandIsRejected;
    procedure TestTruncatedCommandKeepsEarlierSegments;
    procedure TestCoordinateWithoutCommandAfterCloseIsRejected;
  end;

  TTestSVGArc = class(TSVGPathTestCase)
  private
    // Distance of a path point from the given centre.
    function RadiusAt(aIndex, aPoint: Integer; aCX, aCY: Double): Double;
  published
    procedure TestArcToSamePointIsOmitted;
    procedure TestZeroRadiusBecomesLine;
    procedure TestQuarterArcEndsAtEndpoint;
    procedure TestQuarterArcStaysOnTheCircle;
    procedure TestLargeArcUsesMoreSegments;
    procedure TestSweepFlagChoosesTheOtherSide;
    procedure TestUndersizedRadiiAreScaledUp;
    procedure TestRotatedArcEndsAtEndpoint;
    procedure TestArcFlagsNeedNoSeparator;
  end;

  TTestSVGShapes = class(TSVGPathTestCase)
  published
    procedure TestRectIsFourLinesClosed;
    procedure TestDegenerateRectIsEmpty;
    procedure TestRoundedRectHasArcs;
    procedure TestRoundedRectClampsRadii;
    procedure TestRectRadiusDefaultsToTheOther;
    procedure TestCircleClosesOnItself;
    procedure TestCircleStaysOnTheCircle;
    procedure TestZeroRadiusCircleIsEmpty;
    procedure TestEllipseSpansBothRadii;
    procedure TestLineIsTwoPoints;
    procedure TestPolylineStaysOpen;
    procedure TestPolygonCloses;
    procedure TestEmptyPointListAddsNothing;
  end;

implementation

const
  Delta = 1e-9;
  Loose = 1e-6;

{ TSVGPathTestCase }

procedure TSVGPathTestCase.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
end;


procedure TSVGPathTestCase.TearDown;

begin
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TSVGPathTestCase.AssertKind(const aMessage: String; aIndex: Integer;
  aKind: TSVGPathSegmentKind);

begin
  AssertTrue(Format('%s (segment %d exists)', [aMessage, aIndex]),
    aIndex < FPath.SegmentCount);
  AssertTrue(aMessage, FPath[aIndex].Kind = aKind);
end;


procedure TSVGPathTestCase.AssertPoint(const aMessage: String;
  aIndex, aPoint: Integer; aX, aY: Double);

begin
  AssertEquals(aMessage + ' (x)', aX, FPath[aIndex].Points[aPoint].X, Loose);
  AssertEquals(aMessage + ' (y)', aY, FPath[aIndex].Points[aPoint].Y, Loose);
end;


procedure TSVGPathTestCase.AssertCurrent(const aMessage: String; aX, aY: Double);

begin
  AssertEquals(aMessage + ' (x)', aX, FPath.CurrentPoint.X, Loose);
  AssertEquals(aMessage + ' (y)', aY, FPath.CurrentPoint.Y, Loose);
end;


{ TTestSVGPathGrammar }

procedure TTestSVGPathGrammar.TestEmptyDataIsAnEmptyPath;

begin
  AssertTrue('empty path data is valid', FPath.TryParse(''));
  AssertTrue('empty path data produces no segments', FPath.IsEmpty);
  AssertTrue('whitespace alone is valid', FPath.TryParse('   '));
  AssertTrue('whitespace alone produces no segments', FPath.IsEmpty);
end;


procedure TTestSVGPathGrammar.TestAbsoluteMoveAndLine;

begin
  AssertTrue('a move and a line parse', FPath.TryParse('M 10 20 L 30 40'));
  AssertEquals('two commands produce two segments', 2, FPath.SegmentCount);
  AssertKind('the first segment is a moveto', 0, skMoveTo);
  AssertPoint('the moveto keeps its coordinates', 0, 0, 10, 20);
  AssertKind('the second segment is a lineto', 1, skLineTo);
  AssertPoint('the lineto keeps its coordinates', 1, 0, 30, 40);
end;


procedure TTestSVGPathGrammar.TestRelativeCommandsAccumulate;

begin
  AssertTrue('relative commands parse', FPath.TryParse('M10,10 l5,5 l5,5'));
  AssertPoint('the first relative line adds to the start', 1, 0, 15, 15);
  AssertPoint('the second relative line adds again', 2, 0, 20, 20);
end;


procedure TTestSVGPathGrammar.TestImplicitLineToAfterMoveTo;

begin
  AssertTrue('a moveto with extra pairs parses',
    FPath.TryParse('M0,0 10,0 10,10'));
  AssertEquals('the extra pairs become segments', 3, FPath.SegmentCount);
  AssertKind('the first pair is the moveto', 0, skMoveTo);
  AssertKind('the second pair becomes a lineto', 1, skLineTo);
  AssertKind('the third pair becomes a lineto', 2, skLineTo);
end;


procedure TTestSVGPathGrammar.TestImplicitRepeatKeepsCommand;

begin
  AssertTrue('a lineto with several pairs parses',
    FPath.TryParse('M0,0 L1,1 2,2 3,3'));
  AssertEquals('each pair is one lineto', 4, FPath.SegmentCount);
  AssertPoint('the last implicit lineto is read', 3, 0, 3, 3);
end;


procedure TTestSVGPathGrammar.TestHorizontalAndVertical;

begin
  AssertTrue('horizontal and vertical lines parse',
    FPath.TryParse('M10,10 H50 V60 h-20 v-10'));
  AssertPoint('an absolute horizontal keeps y', 1, 0, 50, 10);
  AssertPoint('an absolute vertical keeps x', 2, 0, 50, 60);
  AssertPoint('a relative horizontal moves along x', 3, 0, 30, 60);
  AssertPoint('a relative vertical moves along y', 4, 0, 30, 50);
end;


procedure TTestSVGPathGrammar.TestCloseReturnsToStart;

begin
  AssertTrue('a closed triangle parses',
    FPath.TryParse('M10,10 L20,10 L20,20 Z'));
  AssertKind('the last segment is a close', 3, skClose);
  AssertCurrent('close restores the subpath start', 10, 10);
end;


procedure TTestSVGPathGrammar.TestRelativeMoveAfterCloseStartsAtSubpathStart;

begin
  AssertTrue('a second subpath parses',
    FPath.TryParse('M10,10 L20,10 Z m5,5 l1,0'));
  AssertKind('the second subpath opens with a moveto', 3, skMoveTo);
  AssertPoint('the relative move starts from the closed subpath start',
    3, 0, 15, 15);
end;


procedure TTestSVGPathGrammar.TestFirstMoveIsAlwaysAbsolute;

begin
  AssertTrue('a relative first move parses', FPath.TryParse('m10,20 l1,1'));
  AssertPoint('the first move is taken as absolute', 0, 0, 10, 20);
end;


procedure TTestSVGPathGrammar.TestQuadraticBecomesCubic;

begin
  AssertTrue('a quadratic parses', FPath.TryParse('M0,0 Q3,3 6,0'));
  AssertKind('the quadratic is stored as a cubic', 1, skCubicTo);
  AssertPoint('the first control is two thirds towards the quadratic control',
    1, 0, 2, 2);
  AssertPoint('the endpoint is preserved', 1, 2, 6, 0);
end;


procedure TTestSVGPathGrammar.TestSmoothCubicReflectsControl;

begin
  AssertTrue('a smooth cubic parses',
    FPath.TryParse('M0,0 C1,1 2,2 3,3 S5,5 6,6'));
  AssertKind('the smooth cubic is a cubic', 2, skCubicTo);
  AssertPoint('the first control reflects the previous second control',
    2, 0, 4, 4);
  AssertPoint('the given control is used as the second', 2, 1, 5, 5);
end;


procedure TTestSVGPathGrammar.TestSmoothCubicWithoutPredecessorUsesCurrentPoint;

begin
  AssertTrue('a smooth cubic after a line parses',
    FPath.TryParse('M0,0 L2,2 S5,5 6,6'));
  AssertPoint('the first control falls back to the current point',
    2, 0, 2, 2);
end;


procedure TTestSVGPathGrammar.TestSmoothQuadReflectsControl;

begin
  AssertTrue('a smooth quadratic parses',
    FPath.TryParse('M0,0 Q2,2 4,0 T8,0'));
  AssertKind('the smooth quadratic is stored as a cubic', 2, skCubicTo);
  AssertPoint('the smooth quadratic ends where asked', 2, 2, 8, 0);
end;


procedure TTestSVGPathGrammar.TestNoSeparatorBetweenNegativeNumbers;

begin
  AssertTrue('a minus sign separates coordinates',
    FPath.TryParse('M0,0L-1-2'));
  AssertPoint('both negative coordinates are read', 1, 0, -1, -2);
end;


procedure TTestSVGPathGrammar.TestExponentInCoordinate;

begin
  AssertTrue('an exponent in a coordinate parses',
    FPath.TryParse('M0,0 L1e2,2.5e-1'));
  AssertPoint('the exponent scales the coordinate', 1, 0, 100, 0.25);
end;


procedure TTestSVGPathGrammar.TestDataMustStartWithMoveTo;

begin
  AssertFalse('path data may not start with a lineto',
    FPath.TryParse('L10,10'));
  AssertTrue('nothing is produced from the rejected data', FPath.IsEmpty);
end;


procedure TTestSVGPathGrammar.TestUnknownCommandIsRejected;

begin
  AssertFalse('an unknown command letter is rejected',
    FPath.TryParse('M0,0 X10,10'));
end;


procedure TTestSVGPathGrammar.TestTruncatedCommandKeepsEarlierSegments;

begin
  AssertFalse('a command missing its second coordinate is rejected',
    FPath.TryParse('M0,0 L10,10 L20'));
  AssertEquals('the segments before the error are kept', 2,
    FPath.SegmentCount);
end;


procedure TTestSVGPathGrammar.TestCoordinateWithoutCommandAfterCloseIsRejected;

begin
  AssertFalse('a coordinate pair after a close needs a command',
    FPath.TryParse('M0,0 L1,1 Z 2,2'));
end;


{ TTestSVGArc }

function TTestSVGArc.RadiusAt(aIndex, aPoint: Integer; aCX, aCY: Double): Double;

var
  lDX, lDY: Double;

begin
  lDX := FPath[aIndex].Points[aPoint].X - aCX;
  lDY := FPath[aIndex].Points[aPoint].Y - aCY;
  Result := Sqrt(lDX * lDX + lDY * lDY);
end;


procedure TTestSVGArc.TestArcToSamePointIsOmitted;

begin
  FPath.MoveTo(10, 10);
  FPath.ArcTo(5, 5, 0, False, True, 10, 10);
  AssertEquals('an arc ending where it starts adds nothing', 1,
    FPath.SegmentCount);
end;


procedure TTestSVGArc.TestZeroRadiusBecomesLine;

begin
  FPath.MoveTo(0, 0);
  FPath.ArcTo(0, 5, 0, False, True, 10, 0);
  AssertKind('a zero radius degenerates to a straight line', 1, skLineTo);
  AssertPoint('the line reaches the endpoint', 1, 0, 10, 0);
end;


procedure TTestSVGArc.TestQuarterArcEndsAtEndpoint;

begin
  FPath.MoveTo(10, 0);
  FPath.ArcTo(10, 10, 0, False, True, 0, 10);
  AssertEquals('a quarter arc needs one cubic', 2, FPath.SegmentCount);
  AssertKind('the arc is a cubic', 1, skCubicTo);
  AssertPoint('the arc ends where asked', 1, 2, 0, 10);
end;


procedure TTestSVGArc.TestQuarterArcStaysOnTheCircle;

begin
  FPath.MoveTo(10, 0);
  FPath.ArcTo(10, 10, 0, False, True, 0, 10);
  AssertEquals('the arc endpoint lies on the circle', 10,
    RadiusAt(1, 2, 0, 0), Loose);
  AssertTrue('the control points bow outwards',
    RadiusAt(1, 0, 0, 0) > 10);
end;


procedure TTestSVGArc.TestLargeArcUsesMoreSegments;

begin
  FPath.MoveTo(10, 0);
  FPath.ArcTo(10, 10, 0, True, True, 0, 10);
  AssertTrue('the long way round needs more than one cubic',
    FPath.SegmentCount > 2);
  AssertPoint('the long arc still ends where asked',
    FPath.SegmentCount - 1, 2, 0, 10);
end;


procedure TTestSVGArc.TestSweepFlagChoosesTheOtherSide;

var
  lSweepY, lCounterY: Double;

begin
  FPath.MoveTo(10, 0);
  FPath.ArcTo(10, 10, 0, False, True, 0, 10);
  lSweepY := FPath[1].Points[0].Y;
  FPath.Clear;
  FPath.MoveTo(10, 0);
  FPath.ArcTo(10, 10, 0, False, False, 0, 10);
  lCounterY := FPath[1].Points[0].Y;
  AssertTrue('the two sweeps bow to opposite sides', lSweepY <> lCounterY);
  AssertPoint('the opposite sweep still ends where asked', 1, 2, 0, 10);
end;


procedure TTestSVGArc.TestUndersizedRadiiAreScaledUp;

begin
  FPath.MoveTo(0, 0);
  FPath.ArcTo(1, 1, 0, False, True, 10, 0);
  AssertEquals('the scaled arc is a half circle, so two cubics', 3,
    FPath.SegmentCount);
  AssertPoint('radii too small to span the chord are scaled up', 2, 2, 10, 0);
  AssertEquals('the halfway point sits a scaled radius from the centre', 5,
    RadiusAt(1, 2, 5, 0), Loose);
end;


procedure TTestSVGArc.TestRotatedArcEndsAtEndpoint;

begin
  FPath.MoveTo(0, 0);
  FPath.ArcTo(10, 5, 45, False, True, 10, 10);
  AssertPoint('a rotated arc ends where asked',
    FPath.SegmentCount - 1, 2, 10, 10);
end;


procedure TTestSVGArc.TestArcFlagsNeedNoSeparator;

begin
  AssertTrue('arc flags may run into the following coordinate',
    FPath.TryParse('M0,0 a5 5 0 0110 0'));
  AssertPoint('the endpoint after the joined flags is read',
    FPath.SegmentCount - 1, 2, 10, 0);
end;


{ TTestSVGShapes }

procedure TTestSVGShapes.TestRectIsFourLinesClosed;

begin
  FPath.AddRect(10, 20, 30, 40, 0, 0);
  AssertEquals('a plain rect is a move, three lines and a close', 5,
    FPath.SegmentCount);
  AssertKind('it starts with a moveto', 0, skMoveTo);
  AssertPoint('it starts at the origin corner', 0, 0, 10, 20);
  AssertKind('it ends with a close', 4, skClose);
  AssertPoint('it reaches the far corner', 2, 0, 40, 60);
end;


procedure TTestSVGShapes.TestDegenerateRectIsEmpty;

begin
  FPath.AddRect(0, 0, 0, 10, 0, 0);
  AssertTrue('a rect with no width produces nothing', FPath.IsEmpty);
  FPath.AddRect(0, 0, 10, -1, 0, 0);
  AssertTrue('a rect with negative height produces nothing', FPath.IsEmpty);
end;


procedure TTestSVGShapes.TestRoundedRectHasArcs;

var
  I, lCubics: Integer;

begin
  FPath.AddRect(0, 0, 100, 50, 10, 10);
  lCubics := 0;
  for I := 0 to FPath.SegmentCount - 1 do
    if FPath[I].Kind = skCubicTo then
      Inc(lCubics);
  AssertEquals('a rounded rect has one cubic per corner', 4, lCubics);
  AssertPoint('it starts after the first corner radius', 0, 0, 10, 0);
end;


procedure TTestSVGShapes.TestRoundedRectClampsRadii;

begin
  FPath.AddRect(0, 0, 20, 10, 100, 100);
  AssertPoint('an oversized radius is clamped to half the width',
    0, 0, 10, 0);
end;


procedure TTestSVGShapes.TestRectRadiusDefaultsToTheOther;

var
  lWithBoth, lWithOne: Integer;

begin
  FPath.AddRect(0, 0, 100, 50, 10, -1);
  lWithOne := FPath.SegmentCount;
  FPath.Clear;
  FPath.AddRect(0, 0, 100, 50, 10, 10);
  lWithBoth := FPath.SegmentCount;
  AssertEquals('an absent ry follows rx', lWithBoth, lWithOne);
end;


procedure TTestSVGShapes.TestCircleClosesOnItself;

begin
  FPath.AddCircle(50, 50, 25);
  AssertKind('a circle starts with a moveto', 0, skMoveTo);
  AssertPoint('a circle starts at its rightmost point', 0, 0, 75, 50);
  AssertKind('a circle is closed', FPath.SegmentCount - 1, skClose);
  AssertCurrent('the close returns to the start', 75, 50);
end;


procedure TTestSVGShapes.TestCircleStaysOnTheCircle;

var
  I: Integer;
  lDX, lDY: Double;

begin
  FPath.AddCircle(0, 0, 10);
  for I := 0 to FPath.SegmentCount - 1 do
    if FPath[I].Kind = skCubicTo then
      begin
      lDX := FPath[I].Points[2].X;
      lDY := FPath[I].Points[2].Y;
      AssertEquals('every arc endpoint lies on the circle', 10,
        Sqrt(lDX * lDX + lDY * lDY), Loose);
      end;
end;


procedure TTestSVGShapes.TestZeroRadiusCircleIsEmpty;

begin
  FPath.AddCircle(10, 10, 0);
  AssertTrue('a circle with no radius produces nothing', FPath.IsEmpty);
end;


procedure TTestSVGShapes.TestEllipseSpansBothRadii;

var
  lBounds: TSVGRect;

begin
  FPath.AddEllipse(0, 0, 20, 10);
  lBounds := FPath.ControlBounds;
  AssertTrue('the ellipse spans at least its horizontal diameter',
    lBounds.Width >= 40 - Loose);
  AssertTrue('the ellipse spans at least its vertical diameter',
    lBounds.Height >= 20 - Loose);
end;


procedure TTestSVGShapes.TestLineIsTwoPoints;

begin
  FPath.AddLine(1, 2, 3, 4);
  AssertEquals('a line is a move and a lineto', 2, FPath.SegmentCount);
  AssertPoint('the line starts at its first point', 0, 0, 1, 2);
  AssertPoint('the line ends at its second point', 1, 0, 3, 4);
end;


procedure TTestSVGShapes.TestPolylineStaysOpen;

var
  lPoints: TSVGPointArray;

begin
  SetLength(lPoints, 3);
  lPoints[0] := TSVGPoint.Create(0, 0);
  lPoints[1] := TSVGPoint.Create(10, 0);
  lPoints[2] := TSVGPoint.Create(10, 10);
  FPath.AddPolygon(lPoints, False);
  AssertEquals('a polyline is a move and one line per later point', 3,
    FPath.SegmentCount);
  AssertKind('a polyline does not close', 2, skLineTo);
end;


procedure TTestSVGShapes.TestPolygonCloses;

var
  lPoints: TSVGPointArray;

begin
  SetLength(lPoints, 3);
  lPoints[0] := TSVGPoint.Create(0, 0);
  lPoints[1] := TSVGPoint.Create(10, 0);
  lPoints[2] := TSVGPoint.Create(10, 10);
  FPath.AddPolygon(lPoints, True);
  AssertEquals('a polygon adds a close', 4, FPath.SegmentCount);
  AssertKind('a polygon closes', 3, skClose);
end;


procedure TTestSVGShapes.TestEmptyPointListAddsNothing;

var
  lPoints: TSVGPointArray;

begin
  lPoints := nil;
  FPath.AddPolygon(lPoints, True);
  AssertTrue('an empty point list produces nothing', FPath.IsEmpty);
end;


initialization
  RegisterTest('path', TTestSVGPathGrammar);
  RegisterTest('path', TTestSVGArc);
  RegisterTest('path', TTestSVGShapes);
end.
