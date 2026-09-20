{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for clipping, masking and image drawing in the software backend.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgclip;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svggoldens, svgpixels, fpsvg.types,
     fpsvg.path, fpsvg.dom, fpsvg.read, fpsvg.backend, fpsvg.render,
     fpsvg.soft;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpimage, fpcunit, testregistry, svggoldens,
     svgpixels, fpsvg.types, fpsvg.path, fpsvg.dom, fpsvg.read,
     fpsvg.backend, fpsvg.render, fpsvg.soft;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGClipping = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FPath: TSVGPath;
    FClip: TSVGPath;
    // The alpha of one pixel, from 0 to 1.
    function AlphaAt(aX, aY: Integer): Double;
    // The alpha of the whole surface, added up in pixel units.
    function CoveredArea: Double;
    // Fills the whole surface with opaque black.
    procedure FillEverything;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClipRestrictsTheFill;
    procedure TestClipAreaMatchesTheClipRectangle;
    procedure TestNestedClipsIntersect;
    procedure TestPopClipRestoresTheSurface;
    procedure TestClipEdgesAreAntialiased;
    procedure TestEvenOddClipLeavesAHole;
    procedure TestDisjointClipsDrawNothing;
    procedure TestClipOutsideTheSurfaceDrawsNothing;
    procedure TestPopClipWithoutPushIsRefused;
    procedure TestFrameEndingWithAnOpenClipIsRefused;
    procedure TestClipAppliesInsideALayer;
    procedure TestClipKeepsTheColourItLetsThrough;
    procedure TestClipEdgeKeepsTheHue;
  end;

  { What a clip path of a whole document cuts. A clip path with no
    silhouette keeps nothing, for whatever reason it has none. }
  TTestSVGDocumentClip = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FSoft: TSVGSoftBackend;
    FRenderer: TSVGRenderer;
    // Renders a hundred square document holding the given clip path over
    // a black rectangle cut by it.
    procedure ClipWith(const aClip: String);
    function CoveredArea: Double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOneShapeClipsToThatShape;
    procedure TestAShapeThatIsNotDrawnKeepsNothing;
    procedure TestAShapeOfNoExtentKeepsNothing;
    procedure TestAShapeThatIsNotDrawnLeavesTheOthers;
    procedure TestAShapeThatIsHiddenKeepsNothing;
    procedure TestWhatPaintsTheShapeSaysNothingAboutTheSilhouette;
    procedure TestAStrokeDoesNotWidenTheSilhouette;
  end;

  { The surface holds straight alpha. A colour laid on a transparent pixel
    keeps its own channels, and the coverage becomes its alpha. }
  TTestSVGStraightAlpha = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FPath: TSVGPath;
    // Fills the whole surface with a colour at the given opacity.
    procedure Fill(const aColor: TSVGColor; aOpacity: Double);
    function ColorAt(aX, aY: Integer): TSVGColor;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestColourOnTransparentKeepsItsChannels;
    procedure TestColourOnTransparentTakesTheOpacityAsAlpha;
    procedure TestColourOnOpaqueMixesTowardsIt;
    procedure TestTwoTranslucentLayersReachTheRightColour;
    procedure TestFullyOpaquePaintIsUnchanged;
    procedure TestLayerOpacityDoesNotDarkenTheColour;
  end;

  TTestSVGMasking = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FPath: TSVGPath;
    function AlphaAt(aX, aY: Integer): Double;
    // Fills the given rectangle of the current target with a colour.
    procedure FillRect(aX, aY, aWidth, aHeight: Double;
      const aColor: TSVGColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAlphaMaskKeepsWhatItCovers;
    procedure TestAlphaMaskClearsWhatItMisses;
    procedure TestPartialAlphaMaskScalesTheTarget;
    procedure TestLuminanceMaskUsesTheColour;
    procedure TestBlackLuminanceMaskHidesEverything;
    procedure TestMaskLeavesTheColourAlone;
    procedure TestPopLayerAsMaskWithoutPushIsRefused;
  end;

  TTestSVGImageDrawing = class(TTestCase)
  private
    FBackend: TSVGSoftBackend;
    FSource: ISVGImageSource;
    function AlphaAt(aX, aY: Integer): Double;
    function ColorAt(aX, aY: Integer): TSVGColor;
    // A four by four image: red on the left half, blue on the right.
    procedure BuildSource;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestImageFillsItsRectangle;
    procedure TestImageIsScaledIntoTheRectangle;
    procedure TestImageKeepsItsColumns;
    procedure TestImageFollowsTheTransform;
    procedure TestTransparentPixelsAreSkipped;
    procedure TestAnImageDrawnAtItsOwnSizeIsUnchanged;
    procedure TestScalingUpMixesTheNeighbours;
    procedure TestAClearPixelLendsNoColourToItsNeighbour;
    procedure TestImageOpacityScalesTheAlpha;
    procedure TestImageIsClipped;
    procedure TestEmptyRectangleDrawsNothing;
  end;

implementation

const
  Delta = 1e-9;

// A surface of the given size, filled with one opaque colour.
function BuildImage(aWidth, aHeight: Integer;
  const aColor: TSVGColor): TFPMemoryImage;

var
  X, Y: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for Y := 0 to aHeight - 1 do
    for X := 0 to aWidth - 1 do
      Result.Colors[X, Y] := TFPColor(aColor);
end;


{ TTestSVGClipping }

procedure TTestSVGClipping.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
  FClip := TSVGPath.Create;
end;


procedure TTestSVGClipping.TearDown;

begin
  FreeAndNil(FClip);
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


function TTestSVGClipping.AlphaAt(aX, aY: Integer): Double;

begin
  Result := FBackend.Image.Colors[aX, aY].Alpha / 65535;
end;


function TTestSVGClipping.CoveredArea: Double;

var
  X, Y: Integer;

begin
  Result := 0;
  for Y := 0 to FBackend.Image.Height - 1 do
    for X := 0 to FBackend.Image.Width - 1 do
      Result := Result + FBackend.Image.Colors[X, Y].Alpha / 65535;
end;


procedure TTestSVGClipping.FillEverything;

begin
  FPath.Clear;
  FPath.AddRect(0, 0, 20, 20, 0, 0);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.Black), frNonZero, 1);
end;


procedure TTestSVGClipping.TestClipRestrictsTheFill;

begin
  FClip.AddRect(4, 4, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FillEverything;
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertEquals('a pixel inside the clip is painted', 1.0, AlphaAt(6, 6), Delta);
  AssertEquals('a pixel outside the clip is not', 0.0, AlphaAt(2, 2), Delta);
  AssertEquals('nor is one past the far edge', 0.0, AlphaAt(14, 14), Delta);
end;


procedure TTestSVGClipping.TestClipAreaMatchesTheClipRectangle;

begin
  FClip.AddRect(4, 4, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FillEverything;
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertEquals('the painted area is the area of the clip', 64.0, CoveredArea,
    0.01);
end;


procedure TTestSVGClipping.TestNestedClipsIntersect;

var
  lSecond: TSVGPath;

begin
  lSecond := TSVGPath.Create;
  try
    FClip.AddRect(2, 2, 10, 10, 0, 0);
    lSecond.AddRect(8, 8, 10, 10, 0, 0);
    FBackend.BeginFrame(20, 20);
    FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
    FBackend.PushClip(lSecond, TSVGMatrix.Identity, frNonZero);
    FillEverything;
    FBackend.PopClip;
    FBackend.PopClip;
    FBackend.EndFrame;
    AssertEquals('the overlap of the two clips is painted', 16.0, CoveredArea,
      0.01);
    AssertEquals('a pixel in the overlap is painted', 1.0, AlphaAt(9, 9),
      Delta);
    AssertEquals('a pixel in the first clip alone is not', 0.0, AlphaAt(4, 4),
      Delta);
  finally
    lSecond.Free;
  end;
end;


procedure TTestSVGClipping.TestPopClipRestoresTheSurface;

begin
  FClip.AddRect(4, 4, 4, 4, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FBackend.PopClip;
  FillEverything;
  FBackend.EndFrame;
  AssertEquals('the whole surface paints again', 400.0, CoveredArea, 0.01);
end;


procedure TTestSVGClipping.TestClipEdgesAreAntialiased;

begin
  FClip.AddRect(4.5, 4, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FillEverything;
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertEquals('a half-covered clip column is half painted', 0.5,
    AlphaAt(4, 6), 0.02);
  AssertEquals('the clipped area follows the fractional edge', 64.0,
    CoveredArea, 0.05);
end;


procedure TTestSVGClipping.TestEvenOddClipLeavesAHole;

begin
  FClip.AddRect(2, 2, 16, 16, 0, 0);
  FClip.AddRect(6, 6, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frEvenOdd);
  FillEverything;
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertEquals('the ring is painted', 1.0, AlphaAt(3, 3), Delta);
  AssertEquals('the hole is not', 0.0, AlphaAt(10, 10), Delta);
  AssertEquals('the area is the ring alone', 256.0 - 64.0, CoveredArea, 0.01);
end;


procedure TTestSVGClipping.TestDisjointClipsDrawNothing;

var
  lSecond: TSVGPath;

begin
  lSecond := TSVGPath.Create;
  try
    FClip.AddRect(1, 1, 4, 4, 0, 0);
    lSecond.AddRect(12, 12, 4, 4, 0, 0);
    FBackend.BeginFrame(20, 20);
    FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
    FBackend.PushClip(lSecond, TSVGMatrix.Identity, frNonZero);
    FillEverything;
    FBackend.PopClip;
    FBackend.PopClip;
    FBackend.EndFrame;
    AssertEquals('clips that do not meet paint nothing', 0.0, CoveredArea,
      Delta);
  finally
    lSecond.Free;
  end;
end;


procedure TTestSVGClipping.TestClipOutsideTheSurfaceDrawsNothing;

begin
  FClip.AddRect(40, 40, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FillEverything;
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertEquals('a clip off the surface paints nothing', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGClipping.TestPopClipWithoutPushIsRefused;

begin
  FBackend.BeginFrame(8, 8);
  try
    FBackend.PopClip;
    Fail('PopClip without PushClip should raise');
  except
    on E: ESVGSoft do
      AssertTrue('the message reports the imbalance',
        Pos('without a matching', E.Message) > 0);
  end;
end;


procedure TTestSVGClipping.TestFrameEndingWithAnOpenClipIsRefused;

begin
  FClip.AddRect(1, 1, 4, 4, 0, 0);
  FBackend.BeginFrame(8, 8);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  try
    FBackend.EndFrame;
    Fail('EndFrame with an open clip should raise');
  except
    on E: ESVGSoft do
      AssertTrue('the message counts the open clips',
        Pos('1 clips open', E.Message) > 0);
  end;
end;


procedure TTestSVGClipping.TestClipAppliesInsideALayer;

begin
  FClip.AddRect(4, 4, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  FillEverything;
  FBackend.PopLayer;
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertEquals('the clip still bounds the drawing of the layer', 32.0,
    CoveredArea, 0.05);
end;


procedure TTestSVGClipping.TestClipKeepsTheColourItLetsThrough;

begin
  FClip.AddRect(4, 4, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FPath.AddRect(0, 0, 20, 20, 0, 0);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes($20, $80, $C0, 255)),
    frNonZero, 1);
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertPixel(Self, 'a clip cuts the fill and does not tint it',
    FBackend.Image, 6, 6, TSVGColor.FromBytes($20, $80, $C0, 255));
end;


procedure TTestSVGClipping.TestClipEdgeKeepsTheHue;

begin
  FClip.AddRect(4.5, 4, 8, 8, 0, 0);
  FBackend.BeginFrame(20, 20);
  FBackend.PushClip(FClip, TSVGMatrix.Identity, frNonZero);
  FPath.AddRect(0, 0, 20, 20, 0, 0);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes($20, $80, $C0, 255)),
    frNonZero, 1);
  FBackend.PopClip;
  FBackend.EndFrame;
  AssertPixelNear(Self, 'a half covered clip column halves the alpha alone',
    FBackend.Image, 4, 6, TSVGColor.FromBytes($20, $80, $C0, 128), 2);
end;


{ TTestSVGStraightAlpha }

procedure TTestSVGStraightAlpha.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
  FPath.AddRect(0, 0, 8, 8, 0, 0);
end;


procedure TTestSVGStraightAlpha.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


procedure TTestSVGStraightAlpha.Fill(const aColor: TSVGColor;
  aOpacity: Double);

begin
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(aColor), frNonZero, aOpacity);
end;


function TTestSVGStraightAlpha.ColorAt(aX, aY: Integer): TSVGColor;

begin
  Result := TSVGColor(FBackend.Image.Colors[aX, aY]);
end;


procedure TTestSVGStraightAlpha.TestColourOnTransparentKeepsItsChannels;

begin
  FBackend.BeginFrame(8, 8);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255), 0.5);
  FBackend.EndFrame;
  AssertEquals('the red channel is not premultiplied away', 65535,
    ColorAt(4, 4).Red);
  AssertEquals('and the green channel stays out', 0, ColorAt(4, 4).Green);
end;


procedure TTestSVGStraightAlpha.TestColourOnTransparentTakesTheOpacityAsAlpha;

begin
  FBackend.BeginFrame(8, 8);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255), 0.5);
  FBackend.EndFrame;
  AssertEquals('the opacity became the alpha', 0.5,
    ColorAt(4, 4).Alpha / 65535, 0.01);
end;


procedure TTestSVGStraightAlpha.TestColourOnOpaqueMixesTowardsIt;

begin
  FBackend.BeginFrame(8, 8);
  Fill(TSVGColor.FromBytes(255, 255, 255, 255), 1);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255), 0.5);
  FBackend.EndFrame;
  AssertEquals('red over white keeps the red channel', 65535,
    ColorAt(4, 4).Red);
  AssertEquals('and lands halfway on the green', 0.5,
    ColorAt(4, 4).Green / 65535, 0.01);
  AssertEquals('over an opaque ground the result is opaque', 65535,
    ColorAt(4, 4).Alpha);
end;


procedure TTestSVGStraightAlpha.TestTwoTranslucentLayersReachTheRightColour;

begin
  FBackend.BeginFrame(8, 8);
  Fill(TSVGColor.FromBytes(0, 0, 255, 255), 0.5);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255), 0.5);
  FBackend.EndFrame;
  // Half red over half blue gives an alpha of three quarters. The red
  // contributes two thirds of that.
  AssertEquals('the alpha accumulates', 0.75,
    ColorAt(4, 4).Alpha / 65535, 0.01);
  AssertEquals('two thirds of the colour is the red on top', 2 / 3,
    ColorAt(4, 4).Red / 65535, 0.02);
  AssertEquals('and a third is the blue below', 1 / 3,
    ColorAt(4, 4).Blue / 65535, 0.02);
end;


procedure TTestSVGStraightAlpha.TestFullyOpaquePaintIsUnchanged;

begin
  FBackend.BeginFrame(8, 8);
  Fill(TSVGColor.FromBytes(10, 20, 30, 255), 1);
  FBackend.EndFrame;
  AssertEquals('an opaque fill keeps its red', 10 * 257, ColorAt(4, 4).Red);
  AssertEquals('its green', 20 * 257, ColorAt(4, 4).Green);
  AssertEquals('and its blue', 30 * 257, ColorAt(4, 4).Blue);
end;


procedure TTestSVGStraightAlpha.TestLayerOpacityDoesNotDarkenTheColour;

begin
  FBackend.BeginFrame(8, 8);
  FBackend.PushLayer(TSVGRect.Empty, 0.5, True);
  Fill(TSVGColor.FromBytes(255, 0, 0, 255), 1);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('a half opaque layer keeps the colour it holds', 65535,
    ColorAt(4, 4).Red);
  AssertEquals('and halves its alpha instead', 0.5,
    ColorAt(4, 4).Alpha / 65535, 0.01);
end;


{ TTestSVGMasking }

procedure TTestSVGMasking.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGMasking.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


function TTestSVGMasking.AlphaAt(aX, aY: Integer): Double;

begin
  Result := FBackend.Image.Colors[aX, aY].Alpha / 65535;
end;


procedure TTestSVGMasking.FillRect(aX, aY, aWidth, aHeight: Double;
  const aColor: TSVGColor);

begin
  FPath.Clear;
  FPath.AddRect(aX, aY, aWidth, aHeight, 0, 0);
  FBackend.FillPath(FPath, TSVGMatrix.Identity, TSVGPaint.CreateColor(aColor),
    frNonZero, 1);
end;


procedure TTestSVGMasking.TestAlphaMaskKeepsWhatItCovers;

begin
  FBackend.BeginFrame(20, 20);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.Black);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(4, 4, 8, 8, TSVGColor.FromBytes(255, 255, 255, 255));
  FBackend.PopLayerAsMask(mmAlpha);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('a pixel the mask covers survives', 1.0, AlphaAt(6, 6), Delta);
end;


procedure TTestSVGMasking.TestAlphaMaskClearsWhatItMisses;

begin
  FBackend.BeginFrame(20, 20);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.Black);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(4, 4, 8, 8, TSVGColor.FromBytes(255, 255, 255, 255));
  FBackend.PopLayerAsMask(mmAlpha);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('a pixel the mask misses is cleared', 0.0, AlphaAt(1, 1),
    Delta);
end;


procedure TTestSVGMasking.TestPartialAlphaMaskScalesTheTarget;

begin
  FBackend.BeginFrame(20, 20);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.Black);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.FromBytes(255, 255, 255, 128));
  FBackend.PopLayerAsMask(mmAlpha);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('a half transparent mask halves the alpha', 128 / 255,
    AlphaAt(6, 6), 0.01);
end;


procedure TTestSVGMasking.TestLuminanceMaskUsesTheColour;

begin
  FBackend.BeginFrame(20, 20);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.Black);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.FromBytes(255, 255, 255, 255));
  FBackend.PopLayerAsMask(mmLuminance);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('white keeps the target', 1.0, AlphaAt(6, 6), 0.01);
end;


procedure TTestSVGMasking.TestBlackLuminanceMaskHidesEverything;

begin
  FBackend.BeginFrame(20, 20);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.FromBytes(255, 0, 0, 255));
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.FromBytes(0, 0, 0, 255));
  FBackend.PopLayerAsMask(mmLuminance);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('an opaque black mask hides the target', 0.0, AlphaAt(6, 6),
    Delta);
end;


procedure TTestSVGMasking.TestMaskLeavesTheColourAlone;

begin
  FBackend.BeginFrame(20, 20);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.FromBytes(255, 0, 0, 255));
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  FillRect(0, 0, 20, 20, TSVGColor.FromBytes(255, 255, 255, 255));
  FBackend.PopLayerAsMask(mmAlpha);
  FBackend.PopLayer;
  FBackend.EndFrame;
  AssertEquals('the red channel comes through the mask', $FFFF,
    FBackend.Image.Colors[6, 6].Red);
  AssertEquals('and the green channel stays out', 0,
    FBackend.Image.Colors[6, 6].Green);
end;


procedure TTestSVGMasking.TestPopLayerAsMaskWithoutPushIsRefused;

begin
  FBackend.BeginFrame(8, 8);
  try
    FBackend.PopLayerAsMask(mmAlpha);
    Fail('PopLayerAsMask without PushLayer should raise');
  except
    on E: ESVGSoft do
      AssertTrue('the message reports the imbalance',
        Pos('without a matching', E.Message) > 0);
  end;
end;


{ TTestSVGImageDrawing }

procedure TTestSVGImageDrawing.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGSoftBackend.Create;
  BuildSource;
end;


procedure TTestSVGImageDrawing.TearDown;

begin
  FSource := nil;
  FreeAndNil(FBackend);
  inherited TearDown;
end;


function TTestSVGImageDrawing.AlphaAt(aX, aY: Integer): Double;

begin
  Result := FBackend.Image.Colors[aX, aY].Alpha / 65535;
end;


function TTestSVGImageDrawing.ColorAt(aX, aY: Integer): TSVGColor;

begin
  Result := TSVGColor(FBackend.Image.Colors[aX, aY]);
end;


procedure TTestSVGImageDrawing.BuildSource;

var
  lImage: TFPMemoryImage;
  X, Y: Integer;

begin
  lImage := TFPMemoryImage.Create(4, 4);
  for Y := 0 to 3 do
    for X := 0 to 3 do
      if X < 2 then
        lImage.Colors[X, Y] := TFPColor(TSVGColor.FromBytes(255, 0, 0, 255))
      else
        lImage.Colors[X, Y] := TFPColor(TSVGColor.FromBytes(0, 0, 255, 255));
  FSource := TSVGImageSource.Create(lImage, True);
end;


procedure TTestSVGImageDrawing.TestImageFillsItsRectangle;

begin
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(2, 2, 4, 4),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  AssertEquals('a pixel inside the rectangle is drawn', 1.0, AlphaAt(3, 3),
    Delta);
  AssertEquals('a pixel outside it is not', 0.0, AlphaAt(1, 1), Delta);
  AssertEquals('nor is one past the far edge', 0.0, AlphaAt(6, 6), Delta);
end;


procedure TTestSVGImageDrawing.TestImageIsScaledIntoTheRectangle;

begin
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 16, 16),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  AssertEquals('the four by four image covers the whole surface', 1.0,
    AlphaAt(15, 15), Delta);
  AssertEquals('the left half is red', $FFFF, ColorAt(3, 8).Red);
  AssertEquals('the right half is blue', $FFFF, ColorAt(12, 8).Blue);
end;


procedure TTestSVGImageDrawing.TestImageKeepsItsColumns;

begin
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 4, 4),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  AssertEquals('the first column is red', $FFFF, ColorAt(0, 0).Red);
  AssertEquals('the third column is blue', $FFFF, ColorAt(2, 0).Blue);
  AssertEquals('and it is not red', 0, ColorAt(2, 0).Red);
end;


procedure TTestSVGImageDrawing.TestImageFollowsTheTransform;

begin
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 4, 4),
    TSVGMatrix.Translation(8, 8), 1);
  FBackend.EndFrame;
  AssertEquals('the image moved with the transform', 1.0, AlphaAt(9, 9),
    Delta);
  AssertEquals('and left its old place empty', 0.0, AlphaAt(1, 1), Delta);
end;


procedure TTestSVGImageDrawing.TestTransparentPixelsAreSkipped;

var
  lImage: TFPMemoryImage;
  lSource: ISVGImageSource;

begin
  lImage := BuildImage(2, 2, TSVGColor.FromBytes(0, 255, 0, 0));
  lSource := TSVGImageSource.Create(lImage, True);
  FBackend.BeginFrame(8, 8);
  FBackend.DrawImage(lSource, TSVGRect.CreateSize(0, 0, 8, 8),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  AssertEquals('a fully transparent image leaves nothing', 0.0, AlphaAt(4, 4),
    Delta);
end;


procedure TTestSVGImageDrawing.TestImageOpacityScalesTheAlpha;

begin
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 8, 8),
    TSVGMatrix.Identity, 0.5);
  FBackend.EndFrame;
  AssertEquals('half opacity halves the alpha', 0.5, AlphaAt(4, 4), 0.01);
end;


procedure TTestSVGImageDrawing.TestImageIsClipped;

var
  lClip: TSVGPath;

begin
  lClip := TSVGPath.Create;
  try
    lClip.AddRect(0, 0, 4, 16, 0, 0);
    FBackend.BeginFrame(16, 16);
    FBackend.PushClip(lClip, TSVGMatrix.Identity, frNonZero);
    FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 16, 16),
      TSVGMatrix.Identity, 1);
    FBackend.PopClip;
    FBackend.EndFrame;
    AssertEquals('the clipped part of the image is drawn', 1.0, AlphaAt(2, 8),
      Delta);
    AssertEquals('the rest is not', 0.0, AlphaAt(8, 8), Delta);
  finally
    lClip.Free;
  end;
end;


procedure TTestSVGImageDrawing.TestEmptyRectangleDrawsNothing;

begin
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(2, 2, 0, 4),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  AssertEquals('a rectangle with no width draws nothing', 0.0, AlphaAt(2, 3),
    Delta);
end;


{ TTestSVGDocumentClip }

procedure TTestSVGDocumentClip.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGDocumentClip.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FSoft);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGDocumentClip.ClipWith(const aClip: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FSoft);
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">'
    + '<clipPath id="c">' + aClip + '</clipPath>'
    + '<rect x="0" y="0" width="100" height="100" fill="black"'
    + ' clip-path="url(#c)"/></svg>');
  FRenderer.Render(FDocument, FSoft);
end;


function TTestSVGDocumentClip.CoveredArea: Double;

var
  X, Y: Integer;

begin
  Result := 0;
  for Y := 0 to FSoft.Image.Height - 1 do
    for X := 0 to FSoft.Image.Width - 1 do
      Result := Result + FSoft.Image.Colors[X, Y].Alpha / 65535;
end;


procedure TTestSVGDocumentClip.TestOneShapeClipsToThatShape;

begin
  ClipWith('<rect x="10" y="10" width="20" height="30"/>');
  AssertEquals('the clip keeps the one shape', 600.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGDocumentClip.TestAShapeThatIsNotDrawnKeepsNothing;

begin
  // The only shape of the clip path is not drawn, so the path has no
  // silhouette and keeps nothing at all.
  ClipWith('<rect x="10" y="10" width="20" height="30" display="none"/>');
  AssertEquals('a clip path holding nothing keeps nothing', 0.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGDocumentClip.TestAShapeOfNoExtentKeepsNothing;

begin
  ClipWith('<rect x="10" y="10" width="0" height="0"/>');
  AssertEquals('a shape of no extent is a silhouette of nothing', 0.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGDocumentClip.TestAShapeThatIsNotDrawnLeavesTheOthers;

begin
  ClipWith('<rect x="10" y="10" width="20" height="30" display="none"/>'
    + '<rect x="50" y="50" width="10" height="10"/>');
  AssertEquals('the shape that is drawn is the one that cuts', 100.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGDocumentClip.TestAShapeThatIsHiddenKeepsNothing;

begin
  // Being drawn at all is not a painting property: a shape that is not
  // drawn adds no silhouette to the clip path.
  ClipWith('<rect x="10" y="10" width="20" height="30"'
    + ' visibility="hidden"/>');
  AssertEquals('a hidden shape leaves the clip path empty', 0.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGDocumentClip.TestWhatPaintsTheShapeSaysNothingAboutTheSilhouette;

begin
  // The silhouette is the bare geometry, so none of these changes it.
  ClipWith('<rect x="10" y="10" width="20" height="30" fill="none"'
    + ' stroke="none"/>');
  AssertEquals('a shape painting nothing still cuts its geometry', 600.0,
    CoveredArea, 0.5);
  ClipWith('<rect x="10" y="10" width="20" height="30" opacity="0"/>');
  AssertEquals('and so does one at no opacity', 600.0, CoveredArea, 0.5);
  ClipWith('<rect x="10" y="10" width="20" height="30" fill-opacity="0"/>');
  AssertEquals('and one whose fill is clear', 600.0, CoveredArea, 0.5);
end;


procedure TTestSVGDocumentClip.TestAStrokeDoesNotWidenTheSilhouette;

begin
  ClipWith('<rect x="10" y="10" width="20" height="30" stroke="black"'
    + ' stroke-width="40"/>');
  AssertEquals('the line a pen would draw is no part of the silhouette',
    600.0, CoveredArea, 0.5);
end;


procedure TTestSVGImageDrawing.TestAnImageDrawnAtItsOwnSizeIsUnchanged;

begin
  // One pixel to one pixel takes the pixel whole: nothing of the one
  // beside it is mixed in.
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 4, 4),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  AssertEquals('the red is the red of the image', $FFFF, ColorAt(1, 0).Red);
  AssertEquals('with none of the blue beside it', 0, ColorAt(1, 0).Blue);
  AssertEquals('and the blue is the blue', $FFFF, ColorAt(2, 0).Blue);
  AssertEquals('with none of the red', 0, ColorAt(2, 0).Red);
end;


procedure TTestSVGImageDrawing.TestScalingUpMixesTheNeighbours;

var
  lColor: TSVGColor;

begin
  // Four pixels drawn over eight puts a device pixel halfway between two
  // of the image, which takes half of each rather than all of the nearer.
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(FSource, TSVGRect.CreateSize(0, 0, 8, 8),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  lColor := ColorAt(4, 0);
  AssertTrue('the pixel where the two meet holds some red',
    (lColor.Red > $2000) and (lColor.Red < $E000));
  AssertTrue('and some blue',
    (lColor.Blue > $2000) and (lColor.Blue < $E000));
end;


procedure TTestSVGImageDrawing.TestAClearPixelLendsNoColourToItsNeighbour;

var
  lImage: TFPMemoryImage;
  lSource: ISVGImageSource;
  lColor: TSVGColor;

begin
  // A pixel that is clear still has a colour, and mixing that colour
  // in would draw a halo along every edge. Only its coverage is used.
  lImage := TFPMemoryImage.Create(2, 1);
  lImage.Colors[0, 0] := TFPColor(TSVGColor.FromBytes(0, 0, 0, 255));
  lImage.Colors[1, 0] := TFPColor(TSVGColor.FromBytes(255, 0, 0, 0));
  lSource := TSVGImageSource.Create(lImage, True);
  FBackend.BeginFrame(16, 16);
  FBackend.DrawImage(lSource, TSVGRect.CreateSize(0, 0, 8, 4),
    TSVGMatrix.Identity, 1);
  FBackend.EndFrame;
  lColor := ColorAt(4, 1);
  AssertTrue('the edge is covered in part', (lColor.Alpha > 0)
    and (lColor.Alpha < $FFFF));
  AssertEquals('and the red of the clear pixel is no part of it', 0,
    lColor.Red);
end;


initialization
  RegisterTest('clip', TTestSVGDocumentClip);
  RegisterTest('clip', TTestSVGClipping);
  RegisterTest('mask', TTestSVGMasking);
  RegisterTest('alpha', TTestSVGStraightAlpha);
  RegisterTest('image', TTestSVGImageDrawing);
end.
