{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for gradients, paint servers and group opacity through layers.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvggradient;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svggoldens, svgpixels, fpsvg.types,
     fpsvg.path, fpsvg.dom, fpsvg.read, fpsvg.style, fpsvg.backend,
     fpsvg.soft;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpimage, fpcunit, testregistry, svggoldens,
     svgpixels, fpsvg.types, fpsvg.path, fpsvg.dom, fpsvg.read,
     fpsvg.style, fpsvg.backend, fpsvg.soft;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGGradientModel = class(TTestCase)
  private
    // A gradient of two stops, black to white along the unit x axis.
    function Ramp: TSVGGradient;
  published
    procedure TestLinearOffsetAlongTheAxis;
    procedure TestLinearOffsetOutsideTheAxis;
    procedure TestDegenerateLinearIsTheEndStop;
    procedure TestRadialOffsetIsTheRadiusFraction;
    procedure TestRadialWithFocusStillEndsAtTheEdge;
    procedure TestFocusOutsideTheCircleIsPulledIn;
    procedure TestPadClampsOutsideTheRange;
    procedure TestRepeatWrapsAround;
    procedure TestReflectMirrorsAlternately;
    procedure TestColourInterpolatesBetweenStops;
    procedure TestStopOpacityFoldsIntoTheAlpha;
    procedure TestSingleStopIsFlat;
    procedure TestNoStopsIsTransparent;
    procedure TestBoxTransformMapsTheUnitSquare;
    procedure TestLinearRGBMixesLighterThanSRGB;
    procedure TestTheStopsThemselvesDoNotMove;
    procedure TestAlphaMixesStraightInEitherSpace;
    procedure TestTheRoundTripThroughLinearKeepsAChannel;
  end;

  TTestSVGGradientDocument = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FStyle: TSVGStyleResolver;
    // The gradient that the element with this id resolves to.
    function GradientOf(const aID: String): TSVGGradient;
    // The fill paint computed for the element with this id.
    function FillOf(const aID: String): TSVGPaint;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFillResolvesToAServer;
    procedure TestStopsAreRead;
    procedure TestPercentageOffsetsBecomeFractions;
    procedure TestStopOpacityIsRead;
    procedure TestDefaultGeometryIsHorizontal;
    procedure TestExplicitGeometryIsRead;
    procedure TestSpreadMethodIsRead;
    procedure TestUserSpaceUnitsAndTransformAreRead;
    procedure TestStopsAreInheritedThroughHref;
    procedure TestDecreasingOffsetsArePulledUp;
    procedure TestGradientWithoutStopsPaintsNothing;
    procedure TestMissingReferenceLeavesTheFillAlone;
    procedure TestServerIsCachedPerElement;
    procedure TestFillNamedByAnInlineStyleResolves;
    procedure TestInlineStyleBeatsThePresentationAttribute;
    procedure TestAStopTakesTheColourItInheritsWhereItStands;
    procedure TestAStopTakesTheOpacityItInheritsWhereItStands;
    procedure TestCurrentColorInAStopIsTheColourWhereItStands;
    procedure TestAStopWithoutAColourIsBlack;
    procedure TestAStopIsColouredByAStylesheetRule;
    procedure TestStopColourComesFromAnInlineStyle;
    procedure TestStopOpacityComesFromAnInlineStyle;
  end;

  { What a gradient takes from the gradient it refers to. SVG 1.1 lets it
    take every attribute it does not set itself, and not only the
    stops. }
  TTestSVGGradientInheritance = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FStyle: TSVGStyleResolver;
    // The gradient of the element with id "shape", in a document holding
    // the given definitions and a rectangle filled from "sub".
    function GradientFrom(const aDefs: String): TSVGGradient;
  protected
    procedure TearDown; override;
  published
    procedure TestRadiusIsInherited;
    procedure TestCentreIsInherited;
    procedure TestFocusIsInherited;
    procedure TestAnOwnAttributeBeatsTheInheritedOne;
    procedure TestUnitsAreInherited;
    procedure TestSpreadMethodIsInherited;
    procedure TestTransformIsInherited;
    procedure TestInheritanceFollowsMoreThanOneLink;
    procedure TestAFocusFallsBackToTheInheritedCentre;
    procedure TestARingOfReferencesTerminates;
  end;

  TTestSVGGradientRender = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FDocument: TSVGDocument;
    FStyle: TSVGStyleResolver;
    FPath: TSVGPath;
    // Fills a square of 64 units with the paint of the element with this
    // id.
    procedure RenderFill(const aID: String);
    // The frame as one character per pixel, sampling every fourth pixel.
    function Sample(aChannel: Integer): TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLinearRunsLeftToRight;
    procedure TestUserSpaceGradientIgnoresTheBoundingBox;
    procedure TestRadialIsBrightestNearTheFocus;
    procedure TestGradientGolden;
    procedure TestAPaintIsReadAtTheMiddleOfAPixel;
    procedure TestAPaintMayBeReadAtTheCornerInstead;
  end;

  TTestSVGLayers = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FPath: TSVGPath;
    procedure PopWithoutPush;
    procedure EndFrameWithOpenLayer;
    // Fills the current path with an opaque colour.
    procedure Fill(const aColor: TSVGColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBackendReportsGroupOpacity;
    procedure TestLayerCompositesAtItsOpacity;
    procedure TestOverlapInsideALayerDoesNotDoubleUp;
    procedure TestNestedLayersMultiply;
    procedure TestFullyTransparentLayerLeavesNothing;
    procedure TestBoundsLimitWhatIsComposited;
    procedure TestPopWithoutPushIsRejected;
    procedure TestEndFrameWithAnOpenLayerIsRejected;
    procedure TestLayerGivesBackTheColourItHolds;
    procedure TestNestedLayersDoNotDarkenTheColour;
    procedure TestLayerOverAGroundMixesTowardsIt;
    procedure TestOverlapInsideALayerKeepsOneColour;
  end;

implementation

const
  Delta = 1e-9;
  Shades = ' .:-=+*#%@';

// One character for each channel value, densest last.
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


{ TTestSVGGradientModel }

function TTestSVGGradientModel.Ramp: TSVGGradient;

begin
  Result := TSVGGradient.CreateLinear(TSVGPoint.Create(0, 0),
    TSVGPoint.Create(1, 0));
  Result.AddStop(TSVGGradientStop.Create(0, TSVGColor.Black, 1));
  Result.AddStop(TSVGGradientStop.Create(1,
    TSVGColor.FromBytes(255, 255, 255, 255), 1));
end;


procedure TTestSVGGradientModel.TestLinearOffsetAlongTheAxis;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  AssertEquals('the start of the axis is offset zero', 0,
    lGradient.OffsetAt(TSVGPoint.Create(0, 0)), Delta);
  AssertEquals('the end of the axis is offset one', 1,
    lGradient.OffsetAt(TSVGPoint.Create(1, 0)), Delta);
  AssertEquals('the midpoint is offset one half', 0.5,
    lGradient.OffsetAt(TSVGPoint.Create(0.5, 0)), Delta);
end;


procedure TTestSVGGradientModel.TestLinearOffsetOutsideTheAxis;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  AssertEquals('a point off the axis projects onto it', 0.25,
    lGradient.OffsetAt(TSVGPoint.Create(0.25, 9)), Delta);
  AssertEquals('a point before the start is negative', -1,
    lGradient.OffsetAt(TSVGPoint.Create(-1, 0)), Delta);
end;


procedure TTestSVGGradientModel.TestDegenerateLinearIsTheEndStop;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateLinear(TSVGPoint.Create(2, 2),
    TSVGPoint.Create(2, 2));
  AssertEquals('a gradient of no length is everywhere its end', 1,
    lGradient.OffsetAt(TSVGPoint.Create(9, 9)), Delta);
end;


procedure TTestSVGGradientModel.TestRadialOffsetIsTheRadiusFraction;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateRadial(TSVGPoint.Create(0, 0), 10,
    TSVGPoint.Create(0, 0));
  AssertEquals('the centre is offset zero', 0,
    lGradient.OffsetAt(TSVGPoint.Create(0, 0)), Delta);
  AssertEquals('the edge is offset one', 1,
    lGradient.OffsetAt(TSVGPoint.Create(10, 0)), Delta);
  AssertEquals('half way out is offset one half', 0.5,
    lGradient.OffsetAt(TSVGPoint.Create(0, 5)), Delta);
end;


procedure TTestSVGGradientModel.TestRadialWithFocusStillEndsAtTheEdge;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateRadial(TSVGPoint.Create(0, 0), 10,
    TSVGPoint.Create(4, 0));
  AssertEquals('the focus is offset zero', 0,
    lGradient.OffsetAt(TSVGPoint.Create(4, 0)), Delta);
  AssertEquals('the circle edge is still offset one', 1,
    lGradient.OffsetAt(TSVGPoint.Create(10, 0)), 1e-6);
  AssertEquals('the far edge is also offset one', 1,
    lGradient.OffsetAt(TSVGPoint.Create(-10, 0)), 1e-6);
end;


procedure TTestSVGGradientModel.TestFocusOutsideTheCircleIsPulledIn;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateRadial(TSVGPoint.Create(0, 0), 10,
    TSVGPoint.Create(40, 0));
  lGradient.ClampFocus;
  AssertTrue('a focus beyond the circle is brought inside it',
    lGradient.Focus.X < 10);
  AssertTrue('the focus is brought almost to the edge',
    lGradient.Focus.X > 9.9);
end;


procedure TTestSVGGradientModel.TestPadClampsOutsideTheRange;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  AssertEquals('pad clamps below zero', 0, lGradient.ApplySpread(-3), Delta);
  AssertEquals('pad clamps above one', 1, lGradient.ApplySpread(4), Delta);
end;


procedure TTestSVGGradientModel.TestRepeatWrapsAround;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  lGradient.Spread := smRepeat;
  AssertEquals('repeat wraps a value above one', 0.25,
    lGradient.ApplySpread(1.25), Delta);
  AssertEquals('repeat wraps a negative value', 0.75,
    lGradient.ApplySpread(-0.25), Delta);
end;


procedure TTestSVGGradientModel.TestReflectMirrorsAlternately;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  lGradient.Spread := smReflect;
  AssertEquals('the first period runs forward', 0.25,
    lGradient.ApplySpread(0.25), Delta);
  AssertEquals('the second period runs backward', 0.75,
    lGradient.ApplySpread(1.25), Delta);
  AssertEquals('the third period runs forward again', 0.25,
    lGradient.ApplySpread(2.25), Delta);
  AssertEquals('reflection is symmetric about zero', 0.25,
    lGradient.ApplySpread(-0.25), Delta);
end;


procedure TTestSVGGradientModel.TestColourInterpolatesBetweenStops;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  AssertEquals('the midpoint is halfway between the stops', $8080,
    lGradient.ColorAt(0.5).Red, 200);
  AssertEquals('the start takes the first stop', 0,
    lGradient.ColorAt(0).Red);
  AssertEquals('the end takes the last stop', $FFFF,
    lGradient.ColorAt(1).Red);
end;


procedure TTestSVGGradientModel.TestLinearRGBMixesLighterThanSRGB;

var
  lGradient: TSVGGradient;
  lSRGB, lLinear: TSVGColor;

begin
  lGradient := Ramp;
  lSRGB := lGradient.ColorAt(0.5);
  lGradient.Mixing := ciLinearRGB;
  lLinear := lGradient.ColorAt(0.5);
  // Half way from black to white is mid grey in sRGB. Mixing by light
  // instead gives a lighter grey, around 188.
  AssertEquals('sRGB halves the channel value', 32767, lSRGB.Red, 200);
  AssertTrue('linearRGB gives a lighter grey than sRGB does',
    lLinear.Red > lSRGB.Red + 8000);
  AssertEquals('which is the sRGB of half the light', 48174, lLinear.Red,
    200);
  AssertEquals('and every channel moves together', lLinear.Red,
    lLinear.Blue, 1);
end;


procedure TTestSVGGradientModel.TestTheStopsThemselvesDoNotMove;

var
  lGradient: TSVGGradient;

begin
  lGradient := Ramp;
  lGradient.Mixing := ciLinearRGB;
  AssertEquals('the first stop is its own colour', 0,
    lGradient.ColorAt(0).Red);
  AssertEquals('and the last one is too', 65535, lGradient.ColorAt(1).Red);
end;


procedure TTestSVGGradientModel.TestAlphaMixesStraightInEitherSpace;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateLinear(TSVGPoint.Create(0, 0),
    TSVGPoint.Create(1, 0));
  lGradient.AddStop(TSVGGradientStop.Create(0, TSVGColor.Black, 0));
  lGradient.AddStop(TSVGGradientStop.Create(1, TSVGColor.Black, 1));
  AssertEquals('half way is half opaque in sRGB', 32767,
    lGradient.ColorAt(0.5).Alpha, 200);
  lGradient.Mixing := ciLinearRGB;
  AssertEquals('and the space does not touch alpha', 32767,
    lGradient.ColorAt(0.5).Alpha, 200);
end;


procedure TTestSVGGradientModel.TestTheRoundTripThroughLinearKeepsAChannel;

var
  I: Integer;
  lChannel: Word;

begin
  for I := 0 to 16 do
    begin
    lChannel := I * 4095;
    AssertEquals(Format('channel %d comes back as itself', [lChannel]),
      lChannel, SVGFromLinear(SVGToLinear(lChannel)), 1);
    end;
end;


procedure TTestSVGGradientModel.TestStopOpacityFoldsIntoTheAlpha;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateLinear(TSVGPoint.Create(0, 0),
    TSVGPoint.Create(1, 0));
  lGradient.AddStop(TSVGGradientStop.Create(0, TSVGColor.Black, 0.5));
  AssertEquals('a stop opacity of a half halves the alpha', $7FFF,
    lGradient.ColorAt(0).Alpha, 2);
end;


procedure TTestSVGGradientModel.TestSingleStopIsFlat;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateLinear(TSVGPoint.Create(0, 0),
    TSVGPoint.Create(1, 0));
  lGradient.AddStop(TSVGGradientStop.Create(0.5,
    TSVGColor.FromBytes(255, 0, 0, 255), 1));
  AssertEquals('one stop paints its colour before it', $FFFF,
    lGradient.ColorAt(0).Red);
  AssertEquals('one stop paints its colour after it', $FFFF,
    lGradient.ColorAt(1).Red);
end;


procedure TTestSVGGradientModel.TestNoStopsIsTransparent;

var
  lGradient: TSVGGradient;

begin
  lGradient := TSVGGradient.CreateLinear(TSVGPoint.Create(0, 0),
    TSVGPoint.Create(1, 0));
  AssertFalse('a gradient with no stops reports none', lGradient.HasStops);
  AssertEquals('a gradient with no stops is transparent', 0,
    lGradient.ColorAt(0.5).Alpha);
end;


procedure TTestSVGGradientModel.TestBoxTransformMapsTheUnitSquare;

var
  lMatrix: TSVGMatrix;
  lPoint: TSVGPoint;

begin
  lMatrix := TSVGGradient.BoxTransform(TSVGRect.CreateSize(10, 20, 40, 80));
  lPoint := lMatrix.Transform(TSVGPoint.Create(0, 0));
  AssertEquals('the unit origin maps to the box origin (x)', 10, lPoint.X, Delta);
  AssertEquals('the unit origin maps to the box origin (y)', 20, lPoint.Y, Delta);
  lPoint := lMatrix.Transform(TSVGPoint.Create(1, 1));
  AssertEquals('the unit corner maps to the far corner (x)', 50, lPoint.X, Delta);
  AssertEquals('the unit corner maps to the far corner (y)', 100, lPoint.Y, Delta);
end;


{ TTestSVGGradientDocument }

procedure TTestSVGGradientDocument.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'gradients.svg');
  FStyle := TSVGStyleResolver.Create;
  FStyle.LengthContext := TSVGLengthContext.Create(
    TSVGRect.CreateSize(0, 0, 64, 64));
  FStyle.LoadDocument(FDocument);
end;


procedure TTestSVGGradientDocument.TearDown;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGGradientDocument.FillOf(const aID: String): TSVGPaint;

var
  lElement: TSVGElement;

begin
  lElement := FDocument.ElementByID(aID);
  AssertNotNull('the document holds an element with id ' + aID, lElement);
  Result := FStyle.ComputeStyleOf(lElement).Fill;
end;


function TTestSVGGradientDocument.GradientOf(const aID: String): TSVGGradient;

var
  lServer: ISVGPaintServer;

begin
  lServer := FStyle.PaintServerOf(FDocument.ElementByID(aID));
  AssertNotNull('the element with id ' + aID + ' defines a paint server',
    TObject(lServer));
  AssertTrue('the server yields a gradient', lServer.GetGradient(Result));
end;


procedure TTestSVGGradientDocument.TestFillResolvesToAServer;

var
  lPaint: TSVGPaint;

begin
  lPaint := FillOf('a');
  AssertTrue('a url fill becomes a server paint', lPaint.Kind = spServer);
  AssertNotNull('the server is resolved', TObject(lPaint.Server));
  AssertEquals('the server keeps the gradient id', 'plain',
    lPaint.Server.GetPaintServerID);
  AssertTrue('the server reports itself linear',
    lPaint.Server.GetPaintServerKind = pkLinearGradient);
end;


procedure TTestSVGGradientDocument.TestStopsAreRead;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('plain');
  AssertEquals('both stops are read', 2, Length(lGradient.Stops));
  AssertEquals('the first stop is at zero', 0,
    lGradient.Stops[0].Offset, Delta);
  AssertEquals('the first stop is red', $FFFF, lGradient.Stops[0].Color.Red);
  AssertEquals('the last stop is blue', $FFFF, lGradient.Stops[1].Color.Blue);
end;


procedure TTestSVGGradientDocument.TestPercentageOffsetsBecomeFractions;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('diagonal');
  AssertEquals('fifty percent becomes one half', 0.5,
    lGradient.Stops[1].Offset, Delta);
  AssertEquals('a hundred percent becomes one', 1,
    lGradient.Stops[2].Offset, Delta);
end;


procedure TTestSVGGradientDocument.TestAStopTakesTheColourItInheritsWhereItStands;

begin
  // The gradient takes the stop-color of its parent, and so does the
  // stop. Neither one inherits it, so both have to look it up.
  AssertEquals('the colour comes from the group holding the gradient',
    '#ff00ffff', GradientOf('inheriting').Stops[1].Color.ToString);
end;


procedure TTestSVGGradientDocument.TestAStopTakesTheOpacityItInheritsWhereItStands;

begin
  AssertEquals('the opacity comes from there as well', 0.25,
    GradientOf('inheriting').Stops[1].Opacity, Delta);
end;


procedure TTestSVGGradientDocument.TestCurrentColorInAStopIsTheColourWhereItStands;

begin
  AssertEquals('currentColor reads the colour of the group',
    '#ffff00ff', GradientOf('currentcolour').Stops[1].Color.ToString);
end;


procedure TTestSVGGradientDocument.TestAStopWithoutAColourIsBlack;

begin
  // stop-color is not an inherited property, so the stop-color of the
  // group above does not reach a stop that sets none.
  AssertEquals('a stop without a colour is black', '#000000ff',
    GradientOf('bare').Stops[1].Color.ToString);
end;


procedure TTestSVGGradientDocument.TestAStopIsColouredByAStylesheetRule;

begin
  AssertEquals('the rule for the stop colours it', '#0000ffff',
    GradientOf('ruled').Stops[1].Color.ToString);
end;


procedure TTestSVGGradientDocument.TestStopOpacityIsRead;

begin
  AssertEquals('the stop opacity is read', 0.5,
    GradientOf('diagonal').Stops[1].Opacity, Delta);
end;


procedure TTestSVGGradientDocument.TestDefaultGeometryIsHorizontal;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('plain');
  AssertEquals('the default gradient starts at the left edge', 0,
    lGradient.First.X, Delta);
  AssertEquals('the default gradient ends at the right edge', 1,
    lGradient.Second.X, Delta);
  AssertEquals('the default gradient is level', 0,
    lGradient.Second.Y, Delta);
  AssertTrue('the default units are the bounding box',
    lGradient.Units = guObjectBoundingBox);
  AssertTrue('the default spread is pad', lGradient.Spread = smPad);
end;


procedure TTestSVGGradientDocument.TestExplicitGeometryIsRead;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('ball');
  AssertEquals('the centre is read', 0.4, lGradient.First.X, Delta);
  AssertEquals('the radius is read', 0.5, lGradient.Radius, Delta);
  AssertEquals('the focus is read', 0.3, lGradient.Focus.X, Delta);
  AssertTrue('the element is radial', lGradient.Kind = pkRadialGradient);
end;


procedure TTestSVGGradientDocument.TestSpreadMethodIsRead;

begin
  AssertTrue('reflect is read', GradientOf('diagonal').Spread = smReflect);
  AssertTrue('repeat is read', GradientOf('tiles').Spread = smRepeat);
end;


procedure TTestSVGGradientDocument.TestUserSpaceUnitsAndTransformAreRead;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('userspace');
  AssertTrue('the units are user space', lGradient.Units = guUserSpaceOnUse);
  AssertEquals('a user space coordinate is a length', 10,
    lGradient.First.X, Delta);
  AssertFalse('the gradient transform is read',
    lGradient.Transform.IsIdentity);
  AssertEquals('the gradient transform translates', 4,
    lGradient.Transform.e, Delta);
end;


procedure TTestSVGGradientDocument.TestStopsAreInheritedThroughHref;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('derived');
  AssertEquals('the referenced stops are inherited', 2,
    Length(lGradient.Stops));
  AssertEquals('the inherited first stop is red', $FFFF,
    lGradient.Stops[0].Color.Red);
  AssertEquals('the own geometry wins over the referenced one', 1,
    lGradient.Second.Y, Delta);
end;


procedure TTestSVGGradientDocument.TestDecreasingOffsetsArePulledUp;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('unsorted');
  AssertEquals('the first offset is kept', 0.8,
    lGradient.Stops[0].Offset, Delta);
  AssertEquals('an offset below the last one is pulled up to it', 0.8,
    lGradient.Stops[1].Offset, Delta);
end;


procedure TTestSVGGradientDocument.TestGradientWithoutStopsPaintsNothing;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('empty');
  AssertFalse('a gradient with no stops has none', lGradient.HasStops);
end;


procedure TTestSVGGradientDocument.TestMissingReferenceLeavesTheFillAlone;

var
  lPaint: TSVGPaint;

begin
  lPaint := FillOf('i');
  AssertTrue('a url pointing to nothing leaves the inherited fill',
    lPaint.Kind = spColor);
end;


procedure TTestSVGGradientDocument.TestServerIsCachedPerElement;

var
  lFirst, lSecond: ISVGPaintServer;

begin
  lFirst := FStyle.PaintServerOf(FDocument.ElementByID('plain'));
  lSecond := FStyle.PaintServerOf(FDocument.ElementByID('plain'));
  AssertSame('the same element yields the same server object',
    TObject(lFirst), TObject(lSecond));
end;


procedure TTestSVGGradientDocument.TestFillNamedByAnInlineStyleResolves;

var
  lPaint: TSVGPaint;

begin
  // The CSS resolver has no url token, so a fill written this way never
  // reaches the cascade. It has to be read from the style attribute.
  lPaint := FillOf('j');
  AssertTrue('a fill from a style attribute finds its server',
    lPaint.Kind = spServer);
  AssertEquals('and it is the one the style gave', 'plain',
    lPaint.Server.GetPaintServerID);
end;


procedure TTestSVGGradientDocument.TestInlineStyleBeatsThePresentationAttribute;

var
  lPaint: TSVGPaint;

begin
  lPaint := FillOf('k');
  AssertTrue('the fill resolves to a server', lPaint.Kind = spServer);
  AssertEquals('the style attribute wins over the plain one', 'ball',
    lPaint.Server.GetPaintServerID);
end;


procedure TTestSVGGradientDocument.TestStopColourComesFromAnInlineStyle;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('styledstops');
  AssertEquals('both stops were read', 2, Length(lGradient.Stops));
  AssertEquals('the first stop takes its colour from the style', $FFFF,
    lGradient.Stops[0].Color.Red);
  AssertEquals('and nothing of the default black', 0,
    lGradient.Stops[0].Color.Green);
  AssertEquals('the second stop too', $FFFF, lGradient.Stops[1].Color.Blue);
end;


procedure TTestSVGGradientDocument.TestStopOpacityComesFromAnInlineStyle;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientOf('styledstops');
  AssertEquals('the opacity of the style reaches the stop', 0.25,
    lGradient.Stops[0].Opacity, Delta);
  AssertEquals('a stop without one stays opaque', 1.0,
    lGradient.Stops[1].Opacity, Delta);
end;


{ TTestSVGGradientRender }

procedure TTestSVGGradientRender.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'gradients.svg');
  FStyle := TSVGStyleResolver.Create;
  FStyle.LengthContext := TSVGLengthContext.Create(
    TSVGRect.CreateSize(0, 0, 64, 64));
  FStyle.LoadDocument(FDocument);
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGGradientRender.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGGradientRender.RenderFill(const aID: String);

var
  lPaint: TSVGPaint;

begin
  lPaint := FStyle.ComputeStyleOf(FDocument.ElementByID(aID)).Fill;
  FPath.Clear;
  FPath.AddRect(0, 0, 64, 64, 0, 0);
  FBackend.BeginFrame(64, 64);
  FBackend.FillPath(FPath, TSVGMatrix.Identity, lPaint, frNonZero, 1);
  FBackend.EndFrame;
end;


function TTestSVGGradientRender.Sample(aChannel: Integer): TStringList;

var
  X, Y: Integer;
  lLine: String;
  lColor: TFPColor;

begin
  Result := TStringList.Create;
  Y := 2;
  while Y < FBackend.Image.Height do
    begin
    lLine := '';
    X := 2;
    while X < FBackend.Image.Width do
      begin
      lColor := FBackend.Image.Colors[X, Y];
      case aChannel of
        0: lLine := lLine + ShadeOf(lColor.Red div 257);
        1: lLine := lLine + ShadeOf(lColor.Green div 257);
        2: lLine := lLine + ShadeOf(lColor.Blue div 257);
      else
        lLine := lLine + ShadeOf(lColor.Alpha div 257);
      end;
      Inc(X, 4);
      end;
    Result.Add(TrimRight(lLine));
    Inc(Y, 4);
    end;
end;


procedure TTestSVGGradientRender.TestLinearRunsLeftToRight;

begin
  RenderFill('a');
  // The centre of the leftmost pixel lies half a pixel into the ramp, so
  // the end stops are approached but never reached exactly.
  AssertEquals('the left edge is almost all of the first stop', $FFFF,
    FBackend.Image.Colors[0, 32].Red, 600);
  AssertEquals('the left edge has almost none of the last stop', 0,
    FBackend.Image.Colors[0, 32].Blue, 600);
  AssertEquals('the right edge is almost all of the last stop', $FFFF,
    FBackend.Image.Colors[63, 32].Blue, 600);
  AssertEquals('the middle is halfway between them', $8080,
    FBackend.Image.Colors[32, 32].Red, 800);
end;


procedure TTestSVGGradientRender.TestAPaintIsReadAtTheMiddleOfAPixel;

begin
  // Red at nothing, blue at one, over sixty-four pixels. The middle of
  // the first pixel is half of one sixty-fourth along, so a little blue
  // has already arrived.
  RenderFill('a');
  AssertEquals('the first pixel is read half a pixel into the ramp',
    Round($FFFF * 0.5 / 64), FBackend.Image.Colors[0, 32].Blue, 200);
end;


procedure TTestSVGGradientRender.TestAPaintMayBeReadAtTheCornerInstead;

begin
  // Reading at the corner of the pixel puts the first pixel at the very
  // start of the ramp, which is the first stop and nothing else. That is
  // how the reference images of the W3C suite were drawn.
  FBackend.PaintSample := psCorner;
  RenderFill('a');
  AssertEquals('the first pixel is the first stop whole', $FFFF,
    FBackend.Image.Colors[0, 32].Red, 2);
  AssertEquals('with none of the last stop in it', 0,
    FBackend.Image.Colors[0, 32].Blue, 2);
end;


procedure TTestSVGGradientRender.TestUserSpaceGradientIgnoresTheBoundingBox;

begin
  RenderFill('c');
  // After its own translation the gradient runs from x=14 to x=54. The pad
  // on each side is flat, and does not follow the 64 unit box.
  AssertEquals('left of the gradient start is the first stop', $FFFF,
    FBackend.Image.Colors[2, 32].Green);
  AssertEquals('the first stop still holds just before the start', $FFFF,
    FBackend.Image.Colors[13, 32].Green);
  AssertTrue('the ramp has begun past the start',
    FBackend.Image.Colors[30, 32].Green < $FFFF);
end;


procedure TTestSVGGradientRender.TestRadialIsBrightestNearTheFocus;

var
  lNear, lFar: Integer;

begin
  RenderFill('e');
  lNear := FBackend.Image.Colors[19, 19].Red;
  lFar := FBackend.Image.Colors[60, 60].Red;
  AssertTrue('the focus is the lightest part of the ball', lNear > lFar);
  AssertEquals('the focus takes the first stop', $FFFF, lNear, 1200);
end;


procedure TTestSVGGradientRender.TestGradientGolden;

var
  lLines, lBlock: TStringList;

begin
  lLines := TStringList.Create;
  try
    RenderFill('a');
    lLines.Add('# plain linear, red channel');
    lBlock := Sample(0);
    lLines.AddStrings(lBlock);
    lBlock.Free;

    RenderFill('b');
    lLines.Add('# diagonal reflect, alpha channel');
    lBlock := Sample(3);
    lLines.AddStrings(lBlock);
    lBlock.Free;

    RenderFill('e');
    lLines.Add('# radial with focus, red channel');
    lBlock := Sample(0);
    lLines.AddStrings(lBlock);
    lBlock.Free;

    RenderFill('f');
    lLines.Add('# radial repeat, red channel');
    lBlock := Sample(0);
    lLines.AddStrings(lBlock);
    lBlock.Free;

    AssertGolden(Self, 'gradient', lLines);
  finally
    lLines.Free;
  end;
end;


{ TTestSVGLayers }

procedure TTestSVGLayers.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGLayers.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


procedure TTestSVGLayers.Fill(const aColor: TSVGColor);

begin
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(aColor), frNonZero, 1);
end;


procedure TTestSVGLayers.TestLayerGivesBackTheColourItHolds;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes($33, $66, $CC, 255));
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertPixelNear(Self, 'the opacity reaches the alpha and not the channels',
    FBackend.Image, 8, 8, TSVGColor.FromBytes($33, $66, $CC, 128), 1);
end;


procedure TTestSVGLayers.TestNestedLayersDoNotDarkenTheColour;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes($33, $66, $CC, 255));
  FBackend.PopLayer;
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertPixelNear(Self, 'two layers multiply the alpha and leave the colour',
    FBackend.Image, 8, 8, TSVGColor.FromBytes($33, $66, $CC, 64), 2);
end;


procedure TTestSVGLayers.TestLayerOverAGroundMixesTowardsIt;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  Fill(TSVGColor.FromBytes(255, 255, 255, 255));
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255));
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertPixelNear(Self, 'half red over white lands halfway on green and blue',
    FBackend.Image, 8, 8, TSVGColor.FromBytes(255, 128, 128, 255), 2);
end;


procedure TTestSVGLayers.TestOverlapInsideALayerKeepsOneColour;

var
  lSecond: TSVGPath;

begin
  lSecond := TSVGPath.Create;
  try
    FPath.AddRect(2, 2, 12, 12, 0, 0);
    lSecond.AddRect(4, 4, 8, 8, 0, 0);
    FBackend.BeginFrame(16, 16);
    FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
    Fill(TSVGColor.FromBytes(255, 0, 0, 255));
    FBackend.FillPath(lSecond, TSVGMatrix.Identity,
      TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255)),
      frNonZero, 1);
    FBackend.PopLayer;
    FBackend.EndFrame;
    AssertPixelNear(Self, 'where the two overlap the colour is unchanged',
      FBackend.Image, 8, 8, TSVGColor.FromBytes(255, 0, 0, 128), 2);
    AssertPixelNear(Self, 'as it is where only one of them paints',
      FBackend.Image, 3, 3, TSVGColor.FromBytes(255, 0, 0, 128), 2);
  finally
    lSecond.Free;
  end;
end;


procedure TTestSVGLayers.PopWithoutPush;

begin
  FBackend.PopLayer;
end;


procedure TTestSVGLayers.EndFrameWithOpenLayer;

begin
  FBackend.EndFrame;
end;


procedure TTestSVGLayers.TestBackendReportsGroupOpacity;

begin
  AssertTrue('the software backend composites group opacity itself',
    bcGroupOpacity in TSVGSoftBackend.Capabilities);
end;


procedure TTestSVGLayers.TestLayerCompositesAtItsOpacity;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255));
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('the layer arrives at half opacity', $8000,
    FBackend.Image.Colors[8, 8].Alpha, 600);
end;


procedure TTestSVGLayers.TestOverlapInsideALayerDoesNotDoubleUp;

var
  lLayered, lDirect: Integer;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255));
  Fill(TSVGColor.FromBytes(255, 0, 0, 255));
  FBackend.PopLayer;
  FBackend.EndFrame;
  lLayered := FBackend.Image.Colors[8, 8].Alpha;
  FBackend.BeginFrame(16, 16);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255)), frNonZero, 0.5);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255)), frNonZero, 0.5);
  FBackend.EndFrame;
  lDirect := FBackend.Image.Colors[8, 8].Alpha;
  AssertTrue('group opacity applies once, unlike two half-opacity fills',
    lLayered < lDirect);
end;


procedure TTestSVGLayers.TestNestedLayersMultiply;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255));
  FBackend.PopLayer;
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('two nested half opacities give a quarter', $4000,
    FBackend.Image.Colors[8, 8].Alpha, 600);
end;


procedure TTestSVGLayers.TestFullyTransparentLayerLeavesNothing;

begin
  FPath.AddRect(2, 2, 12, 12, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.Empty, 0, True);
  Fill(TSVGColor.Black);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('a layer at zero opacity contributes nothing', 0,
    FBackend.Image.Colors[8, 8].Alpha);
end;


procedure TTestSVGLayers.TestBoundsLimitWhatIsComposited;

begin
  FPath.AddRect(0, 0, 16, 16, 0, 0);
  FBackend.BeginFrame(16, 16);
  FBackend.PushLayer(TSVGRect.CreateSize(0, 0, 8, 16), 1, True);
  Fill(TSVGColor.Black);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('inside the layer bounds the fill arrives', $FFFF,
    FBackend.Image.Colors[4, 8].Alpha);
  AssertEquals('outside the layer bounds nothing is composited', 0,
    FBackend.Image.Colors[12, 8].Alpha);
end;


procedure TTestSVGLayers.TestPopWithoutPushIsRejected;

begin
  FBackend.BeginFrame(8, 8);
  AssertException('popping a layer that was never pushed is rejected',
    ESVGSoft, @PopWithoutPush);
end;


procedure TTestSVGLayers.TestEndFrameWithAnOpenLayerIsRejected;

begin
  FBackend.BeginFrame(8, 8);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  AssertException('ending a frame with a layer open is rejected',
    ESVGSoft, @EndFrameWithOpenLayer);
end;


{ TTestSVGGradientInheritance }

procedure TTestSVGGradientInheritance.TearDown;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGGradientInheritance.GradientFrom(
  const aDefs: String): TSVGGradient;

var
  lServer: ISVGPaintServer;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink" width="64" height="64">'
    + '<defs>' + aDefs + '</defs>'
    + '<rect id="shape" width="64" height="64" fill="url(#sub)"/></svg>');
  FStyle := TSVGStyleResolver.Create;
  FStyle.LengthContext := TSVGLengthContext.Create(
    TSVGRect.CreateSize(0, 0, 64, 64));
  FStyle.LoadDocument(FDocument);
  lServer := FStyle.PaintServerOf(FDocument.ElementByID('sub'));
  AssertNotNull('the definitions declare a paint server', TObject(lServer));
  AssertTrue('the server yields a gradient', lServer.GetGradient(Result));
end;


procedure TTestSVGGradientInheritance.TestRadiusIsInherited;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<radialGradient id="base" r="20%"><stop offset="0" stop-color="red"/>'
    + '</radialGradient>'
    + '<radialGradient id="sub" xlink:href="#base" cx="0%"/>');
  AssertEquals('the radius comes from the gradient it refers to', 0.2,
    lGradient.Radius, Delta);
  AssertEquals('and the own centre still wins', 0, lGradient.First.X, Delta);
end;


procedure TTestSVGGradientInheritance.TestCentreIsInherited;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<radialGradient id="base" cx="25%" cy="75%">'
    + '<stop offset="0" stop-color="red"/></radialGradient>'
    + '<radialGradient id="sub" xlink:href="#base" r="10%"/>');
  AssertEquals('cx comes from the gradient it refers to', 0.25,
    lGradient.First.X, Delta);
  AssertEquals('and so does cy', 0.75, lGradient.First.Y, Delta);
end;


procedure TTestSVGGradientInheritance.TestFocusIsInherited;

var
  lGradient: TSVGGradient;

begin
  // The focus sits inside the circle of the centre and radius, so the
  // value read here is the inherited one and not a clamped one.
  lGradient := GradientFrom(
    '<radialGradient id="base" cx="100%" r="20%" fx="10%">'
    + '<stop offset="0" stop-color="red"/></radialGradient>'
    + '<radialGradient id="sub" xlink:href="#base" cx="0%"/>');
  AssertEquals('the focus comes from the gradient it refers to', 0.1,
    lGradient.Focus.X, Delta);
end;


procedure TTestSVGGradientInheritance.TestAnOwnAttributeBeatsTheInheritedOne;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<radialGradient id="base" r="20%"><stop offset="0" stop-color="red"/>'
    + '</radialGradient>'
    + '<radialGradient id="sub" xlink:href="#base" r="40%"/>');
  AssertEquals('a gradient uses the value it sets itself', 0.4,
    lGradient.Radius, Delta);
end;


procedure TTestSVGGradientInheritance.TestUnitsAreInherited;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<linearGradient id="base" gradientUnits="userSpaceOnUse" x2="32">'
    + '<stop offset="0" stop-color="red"/></linearGradient>'
    + '<linearGradient id="sub" xlink:href="#base" x1="8"/>');
  AssertTrue('the units come from the gradient it refers to',
    lGradient.Units = guUserSpaceOnUse);
  AssertEquals('and are used to read the own coordinate', 8,
    lGradient.First.X, Delta);
end;


procedure TTestSVGGradientInheritance.TestSpreadMethodIsInherited;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<linearGradient id="base" spreadMethod="reflect">'
    + '<stop offset="0" stop-color="red"/></linearGradient>'
    + '<linearGradient id="sub" xlink:href="#base" x1="0.25"/>');
  AssertTrue('the spread method comes from the gradient it refers to',
    lGradient.Spread = smReflect);
end;


procedure TTestSVGGradientInheritance.TestTransformIsInherited;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<linearGradient id="base" gradientTransform="translate(5 7)">'
    + '<stop offset="0" stop-color="red"/></linearGradient>'
    + '<linearGradient id="sub" xlink:href="#base" x1="0.25"/>');
  AssertEquals('the transform comes from the gradient it refers to', 5,
    lGradient.Transform.E, Delta);
  AssertEquals('on both axes', 7, lGradient.Transform.F, Delta);
end;


procedure TTestSVGGradientInheritance.TestInheritanceFollowsMoreThanOneLink;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<radialGradient id="base" r="20%"><stop offset="0" stop-color="red"/>'
    + '</radialGradient>'
    + '<radialGradient id="middle" xlink:href="#base" cx="10%"/>'
    + '<radialGradient id="sub" xlink:href="#middle" cy="30%"/>');
  AssertEquals('the radius comes from two links away', 0.2,
    lGradient.Radius, Delta);
  AssertEquals('the centre from one', 0.1, lGradient.First.X, Delta);
  AssertEquals('and the own one is its own', 0.3, lGradient.First.Y, Delta);
end;


procedure TTestSVGGradientInheritance.TestAFocusFallsBackToTheInheritedCentre;

var
  lGradient: TSVGGradient;

begin
  // Nothing in the chain gives a focus, so it sits on the centre, and the
  // centre here is the inherited one.
  lGradient := GradientFrom(
    '<radialGradient id="base" cx="25%" r="40%">'
    + '<stop offset="0" stop-color="red"/></radialGradient>'
    + '<radialGradient id="sub" xlink:href="#base" cy="60%"/>');
  AssertEquals('the focus sits on the centre it inherited', 0.25,
    lGradient.Focus.X, Delta);
  AssertEquals('on both axes', 0.6, lGradient.Focus.Y, Delta);
end;


procedure TTestSVGGradientInheritance.TestARingOfReferencesTerminates;

var
  lGradient: TSVGGradient;

begin
  lGradient := GradientFrom(
    '<radialGradient id="sub" xlink:href="#other" cx="10%">'
    + '<stop offset="0" stop-color="red"/></radialGradient>'
    + '<radialGradient id="other" xlink:href="#sub"/>');
  AssertEquals('two gradients referring to each other still resolve',
    0.1, lGradient.First.X, Delta);
  AssertEquals('and a value neither gives falls back to the SVG initial one', 0.5,
    lGradient.Radius, Delta);
end;


initialization
  RegisterTest('gradient', TTestSVGGradientModel);
  RegisterTest('gradient', TTestSVGGradientDocument);
  RegisterTest('gradient', TTestSVGGradientInheritance);
  RegisterTest('gradient', TTestSVGGradientRender);
  RegisterTest('layer', TTestSVGLayers);
end.
