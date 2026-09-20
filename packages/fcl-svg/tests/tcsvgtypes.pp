{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Unit tests for the fpsvg.types geometry, colour and path primitives.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgtypes;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, FpcUnit.Test, FpcUnit.Registry, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpcunit, testregistry, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGColor = class(TTestCase)
  published
    procedure TestFullChannelsExpandToMaximum;
    procedure TestMidChannelExpandsProportionally;
  end;

  TTestSVGMatrix = class(TTestCase)
  private
    procedure AssertPoint(const aMessage: String; aX, aY: Double;
      const aPoint: TSVGPoint);
  published
    procedure TestIdentityLeavesPointUnchanged;
    procedure TestIsIdentity;
    procedure TestComposeAppliesFirstOperandFirst;
    procedure TestComposeIsNotCommutative;
    procedure TestRotateQuarterTurn;
    procedure TestInvertRoundTrips;
    procedure TestInvertSingularFails;
    procedure TestTransformVectorIgnoresTranslation;
  end;

  TTestSVGRect = class(TTestCase)
  published
    procedure TestEmptyRectHasNoExtent;
    procedure TestUnionAbsorbsEmpty;
    procedure TestUnionCoversBoth;
    procedure TestIntersectOfDisjointIsEmpty;
    procedure TestTransformBoundsRotatedRect;
  end;

  TTestSVGPath = class(TTestCase)
  private
    FPath: TSVGPath;
    procedure LineWithoutMoveTo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNewPathIsEmpty;
    procedure TestSegmentKindsAreRecorded;
    procedure TestQuadIsConvertedToCubic;
    procedure TestCloseReturnsToSubpathStart;
    procedure TestSegmentBeforeMoveToRaises;
    procedure TestAssignCopiesSegments;
    procedure TestControlBoundsCoversControlPoints;
  end;

implementation

const
  Delta = 1e-9;

{ TTestSVGColor }

procedure TTestSVGColor.TestFullChannelsExpandToMaximum;

var
  lColor: TSVGColor;

begin
  lColor := TSVGColor.FromBytes(255, 255, 255, 255);
  AssertEquals('255 expands to a saturated red channel', $FFFF, lColor.Red);
  AssertEquals('255 expands to a saturated alpha channel', $FFFF, lColor.Alpha);
end;


procedure TTestSVGColor.TestMidChannelExpandsProportionally;

var
  lColor: TSVGColor;

begin
  lColor := TSVGColor.FromBytes(128, 0, 0, 255);
  AssertEquals('128 expands by 257', 128 * 257, lColor.Red);
  AssertEquals('zero stays zero', 0, lColor.Green);
end;


{ TTestSVGMatrix }

procedure TTestSVGMatrix.AssertPoint(const aMessage: String; aX, aY: Double;
  const aPoint: TSVGPoint);

begin
  AssertEquals(aMessage + ' (x)', aX, aPoint.X, Delta);
  AssertEquals(aMessage + ' (y)', aY, aPoint.Y, Delta);
end;


procedure TTestSVGMatrix.TestIdentityLeavesPointUnchanged;

begin
  AssertPoint('identity maps a point to itself', 3, 4,
    TSVGMatrix.Identity.Transform(TSVGPoint.Create(3, 4)));
end;


procedure TTestSVGMatrix.TestIsIdentity;

begin
  AssertTrue('the identity matrix is recognised',
    TSVGMatrix.Identity.IsIdentity);
  AssertFalse('a translation is not the identity',
    TSVGMatrix.Translation(1, 0).IsIdentity);
end;


procedure TTestSVGMatrix.TestComposeAppliesFirstOperandFirst;

var
  lComposed: TSVGMatrix;

begin
  lComposed := TSVGMatrix.Translation(10, 0).Compose(TSVGMatrix.Scaling(2, 2));
  AssertPoint('translate then scale doubles the translated point', 22, 0,
    lComposed.Transform(TSVGPoint.Create(1, 0)));
end;


procedure TTestSVGMatrix.TestComposeIsNotCommutative;

var
  lComposed: TSVGMatrix;

begin
  lComposed := TSVGMatrix.Scaling(2, 2).Compose(TSVGMatrix.Translation(10, 0));
  AssertPoint('scale then translate shifts the scaled point', 12, 0,
    lComposed.Transform(TSVGPoint.Create(1, 0)));
end;


procedure TTestSVGMatrix.TestRotateQuarterTurn;

begin
  AssertPoint('90 degrees turns the x axis towards the y axis', 0, 1,
    TSVGMatrix.Rotation(90).Transform(TSVGPoint.Create(1, 0)));
end;


procedure TTestSVGMatrix.TestInvertRoundTrips;

var
  lMatrix, lInverse: TSVGMatrix;
  lPoint: TSVGPoint;

begin
  lMatrix := TSVGMatrix.Rotation(30)
    .Compose(TSVGMatrix.Scaling(2, 3).Compose(TSVGMatrix.Translation(5, -7)));
  AssertTrue('a composed affine matrix is invertible',
    lMatrix.Invert(lInverse));
  lPoint := lInverse.Transform(lMatrix.Transform(TSVGPoint.Create(4, 9)));
  AssertPoint('the inverse undoes the transform', 4, 9, lPoint);
end;


procedure TTestSVGMatrix.TestInvertSingularFails;

var
  lInverse: TSVGMatrix;

begin
  AssertFalse('a zero-scale matrix has no inverse',
    TSVGMatrix.Scaling(0, 1).Invert(lInverse));
end;


procedure TTestSVGMatrix.TestTransformVectorIgnoresTranslation;

begin
  AssertPoint('a vector is unaffected by translation', 1, 0,
    TSVGMatrix.Translation(10, 20).TransformVector(TSVGPoint.Create(1, 0)));
end;


{ TTestSVGRect }

procedure TTestSVGRect.TestEmptyRectHasNoExtent;

begin
  AssertTrue('the empty rectangle reports itself empty',
    TSVGRect.Empty.IsEmpty);
  AssertEquals('an empty rectangle has zero width', 0,
    TSVGRect.Empty.Width, Delta);
  AssertEquals('an empty rectangle has zero height', 0,
    TSVGRect.Empty.Height, Delta);
end;


procedure TTestSVGRect.TestUnionAbsorbsEmpty;

var
  lRect: TSVGRect;

begin
  lRect := TSVGRect.Empty.Union(TSVGRect.CreateSize(1, 2, 3, 4));
  AssertEquals('the union with an empty rectangle keeps the left edge', 1,
    lRect.Left, Delta);
  AssertEquals('the union with an empty rectangle keeps the right edge', 4,
    lRect.Right, Delta);
end;


procedure TTestSVGRect.TestUnionCoversBoth;

var
  lRect: TSVGRect;

begin
  lRect := TSVGRect.Create(0, 0, 10, 10).Union(TSVGRect.Create(5, -5, 20, 8));
  AssertEquals('the union spans the leftmost edge', 0, lRect.Left, Delta);
  AssertEquals('the union spans the topmost edge', -5, lRect.Top, Delta);
  AssertEquals('the union spans the rightmost edge', 20, lRect.Right, Delta);
  AssertEquals('the union spans the bottommost edge', 10, lRect.Bottom, Delta);
end;


procedure TTestSVGRect.TestIntersectOfDisjointIsEmpty;

begin
  AssertTrue('disjoint rectangles do not intersect',
    TSVGRect.Create(0, 0, 1, 1).Intersect(TSVGRect.Create(5, 5, 6, 6)).IsEmpty);
end;


procedure TTestSVGRect.TestTransformBoundsRotatedRect;

var
  lRect: TSVGRect;

begin
  lRect := TSVGRect.Create(0, 0, 2, 1).Transform(TSVGMatrix.Rotation(90));
  AssertEquals('a quarter turn maps the width onto the height', 2,
    lRect.Height, Delta);
  AssertEquals('a quarter turn maps the height onto the width', 1,
    lRect.Width, Delta);
end;


{ TTestSVGPath }

procedure TTestSVGPath.SetUp;

begin
  inherited SetUp;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGPath.TearDown;

begin
  FreeAndNil(FPath);
  inherited TearDown;
end;


procedure TTestSVGPath.LineWithoutMoveTo;

begin
  FPath.LineTo(1, 1);
end;


procedure TTestSVGPath.TestNewPathIsEmpty;

begin
  AssertTrue('a fresh path is empty', FPath.IsEmpty);
  AssertEquals('a fresh path has no segments', 0, FPath.SegmentCount);
end;


procedure TTestSVGPath.TestSegmentKindsAreRecorded;

begin
  FPath.MoveTo(0, 0);
  FPath.LineTo(10, 0);
  FPath.CubicTo(10, 5, 5, 10, 0, 10);
  FPath.Close;
  AssertEquals('four calls produce four segments', 4, FPath.SegmentCount);
  AssertTrue('the first segment is a moveto', FPath[0].Kind = skMoveTo);
  AssertTrue('the second segment is a lineto', FPath[1].Kind = skLineTo);
  AssertTrue('the third segment is a cubicto', FPath[2].Kind = skCubicTo);
  AssertTrue('the fourth segment is a close', FPath[3].Kind = skClose);
end;


procedure TTestSVGPath.TestQuadIsConvertedToCubic;

begin
  FPath.MoveTo(0, 0);
  FPath.QuadTo(3, 3, 6, 0);
  AssertEquals('the quadratic becomes one segment', 2, FPath.SegmentCount);
  AssertTrue('the quadratic is stored as a cubic', FPath[1].Kind = skCubicTo);
  AssertEquals('the first control point is two thirds towards the quadratic control', 2,
    FPath[1].Points[0].X, Delta);
  AssertEquals('the second control point is two thirds back from the endpoint', 4,
    FPath[1].Points[1].X, Delta);
  AssertEquals('the endpoint is preserved', 6, FPath[1].Points[2].X, Delta);
end;


procedure TTestSVGPath.TestCloseReturnsToSubpathStart;

begin
  FPath.MoveTo(3, 4);
  FPath.LineTo(10, 10);
  FPath.Close;
  AssertEquals('close restores the subpath start as current point (x)', 3,
    FPath.CurrentPoint.X, Delta);
  AssertEquals('close restores the subpath start as current point (y)', 4,
    FPath.CurrentPoint.Y, Delta);
end;


procedure TTestSVGPath.TestSegmentBeforeMoveToRaises;

begin
  AssertException('a lineto without a preceding moveto is rejected',
    ESVGError, @LineWithoutMoveTo);
end;


procedure TTestSVGPath.TestAssignCopiesSegments;

var
  lOther: TSVGPath;

begin
  FPath.MoveTo(1, 1);
  FPath.LineTo(2, 2);
  lOther := TSVGPath.Create;
  try
    lOther.Assign(FPath);
    AssertEquals('the copy has the same segment count', 2, lOther.SegmentCount);
    AssertEquals('the copy keeps the endpoint', 2, lOther[1].Points[0].X, Delta);
    FPath.LineTo(3, 3);
    AssertEquals('the copy is independent of later changes', 2, lOther.SegmentCount);
  finally
    lOther.Free;
  end;
end;


procedure TTestSVGPath.TestControlBoundsCoversControlPoints;

var
  lRect: TSVGRect;

begin
  FPath.MoveTo(0, 0);
  FPath.CubicTo(0, -10, 10, -10, 10, 0);
  lRect := FPath.ControlBounds;
  AssertEquals('the bounds reach the control points', -10, lRect.Top, Delta);
  AssertEquals('the bounds reach the endpoint', 10, lRect.Right, Delta);
end;


initialization
  RegisterTest('types', TTestSVGColor);
  RegisterTest('types', TTestSVGMatrix);
  RegisterTest('types', TTestSVGRect);
  RegisterTest('types', TTestSVGPath);
end.
