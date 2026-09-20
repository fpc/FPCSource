{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the render tree walk: viewports, layers, use and conditions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgrender;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svggoldens, fpsvg.types, fpsvg.dom,
     fpsvg.read, fpsvg.style, fpsvg.backend, fpsvg.trace,
     fpsvg.render, fpsvg.soft;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpimage, fpcunit, testregistry, svggoldens,
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.style, fpsvg.backend,
     fpsvg.trace, fpsvg.render, fpsvg.soft;
{$ENDIF FPC_DOTTEDUNITS}

type
  { A tracing backend that refuses layers, to test the fallback path. }
  TSVGFlatTraceBackend = class(TSVGTraceBackend)
  public
    class function BackendName: String; override;
    class function Capabilities: TSVGBackendCapabilities; override;
  end;

  TTestSVGRenderWalk = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FBackend: TSVGTraceBackend;
    FRenderer: TSVGRenderer;
    // Parses SVG source and renders it. The log is left in FBackend.
    procedure RenderSource(const aText: String);
    // Renders SVG source through a backend that has no layer support.
    procedure RenderFlat(const aText: String);
    // Number of log lines that start with the given text, after trimming.
    function CountLines(const aPrefix: String): Integer;
    // The first log line that starts with the given text, after trimming.
    function FirstLine(const aPrefix: String): String;
    // Fails unless there is a log line starting with aPrefix.
    procedure AssertLogged(const aMessage, aPrefix: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyDocumentStillOpensAFrame;
    procedure TestRootViewBoxScalesTheContents;
    procedure TestRootWithoutViewBoxIsUnscaled;
    procedure TestGroupTransformComposesWithTheParent;
    procedure TestDefsContentIsNotRendered;
    procedure TestTitleAndDescAreNotRendered;
    procedure TestDisplayNonePrunesTheSubtree;
    procedure TestVisibilityHiddenSkipsOnlyTheShape;
    procedure TestVisibilityHiddenStillRendersChildren;
    procedure TestFillAndStrokeArePaintedInThatOrder;
    procedure TestShapeWithoutPaintDrawsNothing;
    procedure TestGroupOpacityPushesALayer;
    procedure TestGroupOpacityFoldsInWithoutLayerSupport;
    procedure TestShapeOpacityFoldsIntoASinglePaint;
    procedure TestShapeOpacityLayersTwoPaints;
    procedure TestNestedGroupOpacitiesMultiply;
    procedure TestPercentageLengthsUseTheViewport;
    procedure TestElementCountCountsTheWalk;
    procedure TestLinkDrawsWhatItHolds;
    procedure TestLinkPassesItsPropertiesDown;
    procedure TestLinkTransformReachesItsChildren;
    procedure TestMarkerEndDrawsOnceAtTheLastVertex;
    procedure TestMarkerMidDrawsAtEveryTurn;
    procedure TestAllThreeMarkersDrawTogether;
    procedure TestWithoutAMarkerPropertyNothingExtraIsDrawn;
    procedure TestAMarkerNamingNothingDrawsNothing;
    procedure TestAClosedPrimitiveTakesNoMarkers;
    procedure TestAMarkerIsNotPaintedInTheShapesColour;
    procedure TestAMarkerOfAMarkerStopsAtTheDepthLimit;
  end;

  TTestSVGRenderSize = class(TTestCase)
  private
    FRenderer: TSVGRenderer;
    // The device size the given SVG source requests.
    procedure DocSize(const aText: String; out aWidth, aHeight: Integer;
      out aOK: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSizeFromWidthAndHeight;
    procedure TestSizeFromViewBox;
    procedure TestSizeFallsBackToThreeHundredByOneFifty;
    procedure TestPercentageSizeTakesTheFallbackBase;
    procedure TestFractionalSizeRoundsUp;
    procedure TestZeroSizeRendersNothing;
    procedure TestRenderToSizeScalesTheDocument;
    procedure TestRelativeRootTakesTheFrameAsItsViewport;
    procedure TestRelativeRootCentresUnderMeet;
    procedure TestFitScalesByTheTighterSide;
    procedure TestFitCentresTheSpareRoom;
    procedure TestFitOfAMatchingShapeAddsNoOffset;
    procedure TestFitLeavesARelativeRootToPlaceItself;
  end;

  TTestSVGRenderUse = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FBackend: TSVGTraceBackend;
    FRenderer: TSVGRenderer;
    procedure RenderSource(const aText: String);
    function FirstLine(const aPrefix: String): String;
    function CountLines(const aPrefix: String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUseTranslatesByXAndY;
    procedure TestUseTransformComposesWithTheTranslate;
    procedure TestUseInheritsFillFromTheUseSite;
    procedure TestUseAttributesOverrideTheTarget;
    procedure TestStyleSheetAppliesInsideAnExpansion;
    procedure TestMissingReferenceRendersNothing;
    procedure TestCyclicUseRendersNothing;
    procedure TestSymbolBecomesAViewport;
    procedure TestSymbolTakesTheSizeFromTheUse;
    procedure TestSymbolOutsideAUseIsNotRendered;
  end;

  { The view a render frames the root by. }
  TTestSVGRenderView = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FBackend: TSVGTraceBackend;
    FRenderer: TSVGRenderer;
    // Renders a 100 by 100 document holding one rectangle, under the
    // view set on the renderer.
    procedure RenderUnderView;
    // The matrix of the first fill of the log.
    function FirstFillMatrix: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWithoutAViewTheRootFramesItself;
    procedure TestAViewBoxFramesTheRootInstead;
    procedure TestAViewRatioIsUsedWithTheRootViewBox;
    procedure TestAViewTransformMovesWhatTheViewFrames;
    procedure TestAViewFramesTheRootAndNotANestedSVG;
  end;

  TTestSVGRenderConditions = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FBackend: TSVGTraceBackend;
    FRenderer: TSVGRenderer;
    procedure RenderSource(const aText: String);
    function FillCount: Integer;
    function FirstFillPaint: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSwitchTakesTheFirstPassingChild;
    procedure TestSwitchSkipsUnknownExtensions;
    procedure TestSwitchSkipsUnknownFeatures;
    procedure TestSwitchAcceptsASupportedFeature;
    procedure TestEmptyRequiredFeaturesFails;
    procedure TestSystemLanguageMatchesThePrimaryTag;
    procedure TestSystemLanguageWithoutAMatchIsSkipped;
    procedure TestConditionsApplyOutsideASwitch;
    procedure TestEveryImplementedFeatureIsClaimed;
    procedure TestFeaturesNotImplementedAreNotClaimed;
  end;

  TTestSVGRenderGoldens = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTrace: TSVGTraceBackend;
    FSoft: TSVGSoftBackend;
    FRenderer: TSVGRenderer;
    // The alpha channel of the software surface, one character per pixel.
    function AlphaText: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRenderWalkGolden;
    procedure TestViewportGolden;
    procedure TestDocumentCoverageGolden;
  end;

implementation

const
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


// Wraps shape markup in a root element of the given size.
function Doc(const aBody: String; const aRoot: String = ''): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100"'
    + aRoot + '>' + aBody + '</svg>';
end;


// Index of the first line whose trimmed text starts with aPrefix, or -1.
function IndexOfLine(aLog: TStrings; const aPrefix: String): Integer;

var
  I: Integer;

begin
  for I := 0 to aLog.Count - 1 do
    if Pos(aPrefix, TrimLeft(aLog[I])) = 1 then
      Exit(I);
  Result := -1;
end;


// Number of lines whose trimmed text starts with aPrefix.
function CountOfLines(aLog: TStrings; const aPrefix: String): Integer;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to aLog.Count - 1 do
    if Pos(aPrefix, TrimLeft(aLog[I])) = 1 then
      Inc(Result);
end;


{ TSVGFlatTraceBackend }

class function TSVGFlatTraceBackend.BackendName: String;

begin
  Result := 'trace-flat';
end;


class function TSVGFlatTraceBackend.Capabilities: TSVGBackendCapabilities;

begin
  Result := inherited Capabilities - [bcGroupOpacity];
end;


{ TTestSVGRenderWalk }

procedure TTestSVGRenderWalk.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGRenderWalk.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FBackend);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGRenderWalk.RenderSource(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  FBackend := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FBackend);
end;


procedure TTestSVGRenderWalk.RenderFlat(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  FBackend := TSVGFlatTraceBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FBackend);
end;


function TTestSVGRenderWalk.CountLines(const aPrefix: String): Integer;

begin
  Result := CountOfLines(FBackend.Log, aPrefix);
end;


function TTestSVGRenderWalk.FirstLine(const aPrefix: String): String;

var
  lIndex: Integer;

begin
  lIndex := IndexOfLine(FBackend.Log, aPrefix);
  if lIndex < 0 then
    Result := ''
  else
    Result := TrimLeft(FBackend.Log[lIndex]);
end;


procedure TTestSVGRenderWalk.AssertLogged(const aMessage, aPrefix: String);

begin
  AssertTrue(aMessage, IndexOfLine(FBackend.Log, aPrefix) >= 0);
end;


procedure TTestSVGRenderWalk.TestEmptyDocumentStillOpensAFrame;

begin
  RenderSource(Doc(''));
  AssertEquals('the frame takes the declared size', 'begin-frame 100 100',
    FBackend.Log[0]);
  AssertEquals('the root viewport clips to itself',
    'push-clip rule=nonzero ctm=identity', FBackend.Log[1]);
  AssertEquals('nothing is painted inside it', 'pop-clip',
    TrimLeft(FBackend.Log[FBackend.Log.Count - 2]));
  AssertEquals('the frame is closed', 'end-frame',
    FBackend.Log[FBackend.Log.Count - 1]);
end;


procedure TTestSVGRenderWalk.TestRootViewBoxScalesTheContents;

begin
  RenderSource(Doc('<rect x="0" y="0" width="10" height="10"/>',
    ' viewBox="0 0 50 50"'));
  AssertEquals('the viewBox doubles the contents',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 ctm=[2 0 0 2 0 0]',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestRootWithoutViewBoxIsUnscaled;

begin
  RenderSource(Doc('<rect x="0" y="0" width="10" height="10"/>'));
  AssertEquals('without a viewBox the root maps one to one',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestGroupTransformComposesWithTheParent;

begin
  RenderSource(Doc('<g transform="translate(10,0)">'
    + '<g transform="scale(2)"><rect width="4" height="4"/></g></g>'));
  AssertEquals('the inner transform applies before the outer one',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 ctm=[2 0 0 2 10 0]',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestDefsContentIsNotRendered;

begin
  RenderSource(Doc('<defs><rect width="4" height="4"/></defs>'));
  AssertEquals('nothing inside defs is painted', 0, CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestTitleAndDescAreNotRendered;

begin
  RenderSource(Doc('<title>t</title><desc>d</desc><metadata>m</metadata>'));
  AssertEquals('metadata elements paint nothing', 0, CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestDisplayNonePrunesTheSubtree;

begin
  RenderSource(Doc('<g display="none"><rect width="4" height="4"/></g>'
    + '<rect width="2" height="2"/>'));
  AssertEquals('only the visible rectangle is painted', 1,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestVisibilityHiddenSkipsOnlyTheShape;

begin
  RenderSource(Doc('<rect width="4" height="4" visibility="hidden"/>'));
  AssertEquals('a hidden shape paints nothing', 0, CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestVisibilityHiddenStillRendersChildren;

begin
  RenderSource(Doc('<g visibility="hidden">'
    + '<rect width="4" height="4" visibility="visible"/></g>'));
  AssertEquals('a child may turn visibility back on', 1,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestFillAndStrokeArePaintedInThatOrder;

begin
  RenderSource(Doc('<rect width="4" height="4" fill="red" stroke="blue"/>'));
  AssertTrue('the fill precedes the stroke',
    IndexOfLine(FBackend.Log, 'fill-path')
      < IndexOfLine(FBackend.Log, 'stroke-path'));
end;


procedure TTestSVGRenderWalk.TestShapeWithoutPaintDrawsNothing;

begin
  RenderSource(Doc('<rect width="4" height="4" fill="none"/>'));
  AssertEquals('a shape with no paint is skipped', 0,
    CountLines('fill-path'));
  AssertEquals('and it is not stroked either', 0, CountLines('stroke-path'));
end;


procedure TTestSVGRenderWalk.TestGroupOpacityPushesALayer;

begin
  RenderSource(Doc('<g opacity="0.5"><rect width="4" height="4"/></g>'));
  AssertLogged('a translucent group opens a layer', 'push-layer');
  AssertEquals('the layer has the group opacity',
    'push-layer bounds=empty opacity=0.5 isolate=true',
    FirstLine('push-layer'));
  AssertEquals('the child paints at full opacity',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestGroupOpacityFoldsInWithoutLayerSupport;

begin
  RenderFlat(Doc('<g opacity="0.5"><rect width="4" height="4"/></g>'));
  AssertEquals('no layer is opened', 0, CountLines('push-layer'));
  AssertEquals('the opacity reaches the paint instead',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=0.5 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestShapeOpacityFoldsIntoASinglePaint;

begin
  RenderSource(Doc('<rect width="4" height="4" opacity="0.5"/>'));
  AssertEquals('a shape that paints once needs no layer', 0,
    CountLines('push-layer'));
  AssertEquals('the opacity multiplies the fill opacity',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=0.5 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestShapeOpacityLayersTwoPaints;

begin
  RenderSource(Doc('<rect width="4" height="4" stroke="blue" opacity="0.5"/>'));
  AssertEquals('fill and stroke together need a layer', 1,
    CountLines('push-layer'));
  AssertEquals('the fill itself is opaque',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestNestedGroupOpacitiesMultiply;

begin
  RenderFlat(Doc('<g opacity="0.5"><g opacity="0.5">'
    + '<rect width="4" height="4"/></g></g>'));
  AssertEquals('two folded group opacities multiply',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=0.25 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderWalk.TestPercentageLengthsUseTheViewport;

begin
  RenderSource(Doc('<rect x="0" y="0" width="50%" height="25%"/>'));
  AssertEquals('half the width of a hundred unit viewport', 'lineto 50 0',
    TrimLeft(FBackend.Log[IndexOfLine(FBackend.Log, 'fill-path') + 2]));
  AssertEquals('a quarter of its height', 'lineto 50 25',
    TrimLeft(FBackend.Log[IndexOfLine(FBackend.Log, 'fill-path') + 3]));
end;


procedure TTestSVGRenderWalk.TestLinkDrawsWhatItHolds;

begin
  // A link is a container, like a group. It drew nothing while it was
  // read as an element with an unknown name.
  RenderSource(Doc('<a xlink:href="http://example.com">'
    + '<rect width="10" height="10" fill="#0000ff"/></a>'));
  AssertEquals('the shape a link holds is drawn once', 1,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestLinkPassesItsPropertiesDown;

begin
  RenderSource(Doc('<a xlink:href="http://example.com" fill="#00ff00">'
    + '<rect width="10" height="10"/></a>'));
  AssertTrue('the child paints in the colour the link inherited',
    Pos('paint=color(#00ff00ff)', FirstLine('fill-path')) > 0);
end;


procedure TTestSVGRenderWalk.TestLinkTransformReachesItsChildren;

begin
  RenderSource(Doc('<a xlink:href="http://example.com" '
    + 'transform="translate(20,0)">'
    + '<rect width="10" height="10" fill="#0000ff"/></a>'));
  AssertTrue('the transform of a link composes like any other',
    Pos('20', FirstLine('fill-path')) > 0);
end;



const
  MarkerDefs = '<defs><marker id="m" markerWidth="4" markerHeight="4">'
    + '<rect width="4" height="4" fill="#ff0000"/></marker></defs>';

procedure TTestSVGRenderWalk.TestMarkerEndDrawsOnceAtTheLastVertex;

begin
  RenderSource(Doc(MarkerDefs
    + '<path d="M0,0 L10,0" fill="none" stroke="#000000" '
    + 'marker-end="url(#m)"/>'));
  AssertEquals('the path strokes once', 1, CountLines('stroke-path'));
  AssertEquals('and the marker fills once, at the one end', 1,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestMarkerMidDrawsAtEveryTurn;

begin
  RenderSource(Doc(MarkerDefs
    + '<path d="M0,0 L10,0 L10,10 L20,10" fill="none" stroke="#000000" '
    + 'marker-mid="url(#m)"/>'));
  AssertEquals('three segments leave two turns between the ends', 2,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestAllThreeMarkersDrawTogether;

begin
  RenderSource(Doc(MarkerDefs
    + '<path d="M0,0 L10,0 L10,10" fill="none" stroke="#000000" '
    + 'marker-start="url(#m)" marker-mid="url(#m)" marker-end="url(#m)"/>'));
  AssertEquals('a marker on each of the three vertices', 3,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestWithoutAMarkerPropertyNothingExtraIsDrawn;

begin
  RenderSource(Doc(MarkerDefs
    + '<path d="M0,0 L10,0" fill="none" stroke="#000000"/>'));
  AssertEquals('a marker nothing points at is never drawn', 0,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestAMarkerNamingNothingDrawsNothing;

begin
  RenderSource(Doc('<path d="M0,0 L10,0" fill="none" stroke="#000000" '
    + 'marker-end="url(#absent)"/>'));
  AssertEquals('a reference to no marker draws nothing', 0,
    CountLines('fill-path'));
  AssertEquals('and the path itself is drawn all the same', 1,
    CountLines('stroke-path'));
end;


procedure TTestSVGRenderWalk.TestAClosedPrimitiveTakesNoMarkers;

begin
  // SVG puts markers on the shapes drawn from a list of points. A rect is
  // not one of them.
  RenderSource(Doc(MarkerDefs
    + '<rect width="10" height="10" fill="none" stroke="#000000" '
    + 'marker-end="url(#m)"/>'));
  AssertEquals('a rect wears no marker', 0, CountLines('fill-path'));
end;


procedure TTestSVGRenderWalk.TestAMarkerIsNotPaintedInTheShapesColour;

begin
  RenderSource(Doc(MarkerDefs
    + '<path d="M0,0 L10,0" fill="none" stroke="#00ff00" '
    + 'marker-end="url(#m)"/>'));
  AssertTrue('the marker keeps the paint its own content asked for',
    Pos('paint=color(#ff0000ff)', FirstLine('fill-path')) > 0);
end;


procedure TTestSVGRenderWalk.TestAMarkerOfAMarkerStopsAtTheDepthLimit;

begin
  // The marker draws a path that uses the same marker. Without a limit
  // this would never end.
  RenderSource(Doc('<defs><marker id="m" markerWidth="4" markerHeight="4">'
    + '<path d="M0,0 L4,0" fill="none" stroke="#ff0000" '
    + 'marker-end="url(#m)"/>'
    + '</marker></defs>'
    + '<path d="M0,0 L10,0" fill="none" stroke="#000000" '
    + 'marker-end="url(#m)"/>'));
  AssertTrue('it stops rather than running away',
    CountLines('stroke-path') < 8);
end;


procedure TTestSVGRenderWalk.TestElementCountCountsTheWalk;

begin
  RenderSource(Doc('<g><rect width="4" height="4"/><rect width="4" height="4"/>'
    + '</g><defs><rect width="4" height="4"/></defs>'));
  AssertEquals('the root, the group and its two shapes', 4,
    FRenderer.ElementCount);
end;


{ TTestSVGRenderSize }

procedure TTestSVGRenderSize.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGRenderSize.TearDown;

begin
  FreeAndNil(FRenderer);
  inherited TearDown;
end;


procedure TTestSVGRenderSize.DocSize(const aText: String;
  out aWidth, aHeight: Integer; out aOK: Boolean);

var
  lDocument: TSVGDocument;

begin
  lDocument := ReadSVGString(aText);
  try
    aOK := FRenderer.DocumentSize(lDocument, aWidth, aHeight);
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGRenderSize.TestSizeFromWidthAndHeight;

var
  lWidth, lHeight: Integer;
  lOK: Boolean;

begin
  DocSize('<svg xmlns="http://www.w3.org/2000/svg" width="120" height="60"/>',
    lWidth, lHeight, lOK);
  AssertTrue('the document has a size', lOK);
  AssertEquals('width from the attribute', 120, lWidth);
  AssertEquals('height from the attribute', 60, lHeight);
end;


procedure TTestSVGRenderSize.TestSizeFromViewBox;

var
  lWidth, lHeight: Integer;
  lOK: Boolean;

begin
  DocSize('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 20"/>',
    lWidth, lHeight, lOK);
  AssertTrue('the viewBox supplies a size', lOK);
  AssertEquals('width from the viewBox', 40, lWidth);
  AssertEquals('height from the viewBox', 20, lHeight);
end;


procedure TTestSVGRenderSize.TestSizeFallsBackToThreeHundredByOneFifty;

var
  lWidth, lHeight: Integer;
  lOK: Boolean;

begin
  DocSize('<svg xmlns="http://www.w3.org/2000/svg"/>', lWidth, lHeight, lOK);
  AssertTrue('a bare root still has a size', lOK);
  AssertEquals('the CSS fallback width', 300, lWidth);
  AssertEquals('the CSS fallback height', 150, lHeight);
end;


procedure TTestSVGRenderSize.TestPercentageSizeTakesTheFallbackBase;

var
  lWidth, lHeight: Integer;
  lOK: Boolean;

begin
  DocSize('<svg xmlns="http://www.w3.org/2000/svg" width="50%" height="100%"/>',
    lWidth, lHeight, lOK);
  AssertTrue('a percentage still yields a size', lOK);
  AssertEquals('half of the fallback width', 150, lWidth);
  AssertEquals('all of the fallback height', 150, lHeight);
end;


procedure TTestSVGRenderSize.TestFractionalSizeRoundsUp;

var
  lWidth, lHeight: Integer;
  lOK: Boolean;

begin
  DocSize('<svg xmlns="http://www.w3.org/2000/svg" width="10.25" height="4.5"/>',
    lWidth, lHeight, lOK);
  AssertTrue('a fractional size is still a size', lOK);
  AssertEquals('the width covers the fraction', 11, lWidth);
  AssertEquals('the height covers the fraction', 5, lHeight);
end;


procedure TTestSVGRenderSize.TestZeroSizeRendersNothing;

var
  lWidth, lHeight: Integer;
  lOK: Boolean;
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  DocSize('<svg xmlns="http://www.w3.org/2000/svg" width="0" height="10"/>',
    lWidth, lHeight, lOK);
  AssertFalse('a zero width is not a size', lOK);
  lBackend := TSVGTraceBackend.Create;
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="0" height="10"><rect '
    + 'width="4" height="4"/></svg>');
  try
    FRenderer.Render(lDocument, lBackend);
    AssertEquals('no frame is opened at all', 0, lBackend.Log.Count);
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestRenderToSizeScalesTheDocument;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="50" height="50">'
    + '<rect x="0" y="0" width="10" height="10"/></svg>');
  try
    FRenderer.RenderToSize(lDocument, lBackend, 100, 100);
    AssertEquals('the frame takes the requested size', 'begin-frame 100 100',
      lBackend.Log[0]);
    AssertEquals('the document is scaled into it',
      'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
      + 'ctm=[2 0 0 2 0 0]',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path')]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestRelativeRootTakesTheFrameAsItsViewport;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%">'
    + '<rect x="0" y="0" width="50%" height="50%"/></svg>');
  try
    FRenderer.RenderToSize(lDocument, lBackend, 200, 100);
    AssertEquals('a percentage width measures the frame, not the fallback',
      'lineto 100 0',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path') + 2]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestRelativeRootCentresUnderMeet;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  // A square viewBox in a frame twice as wide. meet scales to the shorter
  // side, and xMid splits the spare width on both sides.
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" '
    + 'viewBox="0 0 100 100" preserveAspectRatio="xMidYMid meet">'
    + '<rect x="0" y="0" width="100" height="100"/></svg>');
  try
    FRenderer.RenderToSize(lDocument, lBackend, 200, 100);
    AssertEquals('the viewBox scales uniformly and sits in the middle',
      'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
      + 'ctm=[1 0 0 1 50 0]',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path')]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestFitScalesByTheTighterSide;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="200" height="100">'
    + '<rect x="0" y="0" width="10" height="10"/></svg>');
  try
    // Two hundred by a hundred into a square: the width runs out first.
    FRenderer.RenderToFit(lDocument, lBackend, 400, 400);
    AssertEquals('the scale is the same in both directions',
      'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
      + 'ctm=[2 0 0 2 0 100]',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path')]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestFitCentresTheSpareRoom;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">'
    + '<rect x="0" y="0" width="10" height="10"/></svg>');
  try
    FRenderer.RenderToFit(lDocument, lBackend, 200, 100);
    AssertEquals('the spare width is split either side',
      'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
      + 'ctm=[1 0 0 1 50 0]',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path')]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestFitOfAMatchingShapeAddsNoOffset;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="50" height="25">'
    + '<rect x="0" y="0" width="10" height="10"/></svg>');
  try
    FRenderer.RenderToFit(lDocument, lBackend, 200, 100);
    AssertEquals('a frame of the same shape is filled exactly',
      'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
      + 'ctm=[4 0 0 4 0 0]',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path')]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


procedure TTestSVGRenderSize.TestFitLeavesARelativeRootToPlaceItself;

var
  lDocument: TSVGDocument;
  lBackend: TSVGTraceBackend;

begin
  lBackend := TSVGTraceBackend.Create;
  // The root centres itself with its own preserveAspectRatio. The fit
  // must not centre it a second time.
  lDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" '
    + 'viewBox="0 0 100 100" preserveAspectRatio="xMidYMid meet">'
    + '<rect x="0" y="0" width="100" height="100"/></svg>');
  try
    FRenderer.RenderToFit(lDocument, lBackend, 200, 100);
    AssertEquals('the root placed itself and the fit was not applied',
      'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
      + 'ctm=[1 0 0 1 50 0]',
      TrimLeft(lBackend.Log[IndexOfLine(lBackend.Log, 'fill-path')]));
  finally
    lDocument.Free;
    lBackend.Free;
  end;
end;


{ TTestSVGRenderUse }

procedure TTestSVGRenderUse.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGRenderUse.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FBackend);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGRenderUse.RenderSource(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  FBackend := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FBackend);
end;


function TTestSVGRenderUse.FirstLine(const aPrefix: String): String;

var
  lIndex: Integer;

begin
  lIndex := IndexOfLine(FBackend.Log, aPrefix);
  if lIndex < 0 then
    Result := ''
  else
    Result := TrimLeft(FBackend.Log[lIndex]);
end;


function TTestSVGRenderUse.CountLines(const aPrefix: String): Integer;

begin
  Result := CountOfLines(FBackend.Log, aPrefix);
end;


procedure TTestSVGRenderUse.TestUseTranslatesByXAndY;

begin
  RenderSource(Doc('<defs><rect id="r" width="4" height="4"/></defs>'
    + '<use xlink:href="#r" x="10" y="20"/>'));
  AssertEquals('the use offsets its target',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
    + 'ctm=[1 0 0 1 10 20]', FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestUseTransformComposesWithTheTranslate;

begin
  RenderSource(Doc('<defs><rect id="r" width="4" height="4"/></defs>'
    + '<use xlink:href="#r" x="10" y="0" transform="scale(2)"/>'));
  AssertEquals('the offset applies inside the transform',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
    + 'ctm=[2 0 0 2 20 0]', FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestUseInheritsFillFromTheUseSite;

begin
  RenderSource(Doc('<defs><rect id="r" width="4" height="4"/></defs>'
    + '<use xlink:href="#r" fill="red"/>'));
  AssertEquals('the target takes the fill of the use',
    'fill-path rule=nonzero paint=color(#ff0000ff) opacity=1 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestUseAttributesOverrideTheTarget;

begin
  RenderSource(Doc('<defs><rect id="r" width="4" height="4" fill="blue"/>'
    + '</defs><use xlink:href="#r" fill="red"/>'));
  AssertEquals('the target keeps its own fill',
    'fill-path rule=nonzero paint=color(#0000ffff) opacity=1 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestStyleSheetAppliesInsideAnExpansion;

begin
  RenderSource(Doc('<style>.hot { fill: red; }</style>'
    + '<defs><rect id="r" class="hot" width="4" height="4"/></defs>'
    + '<use xlink:href="#r"/>'));
  AssertEquals('the stylesheet reaches the expanded copy',
    'fill-path rule=nonzero paint=color(#ff0000ff) opacity=1 ctm=identity',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestMissingReferenceRendersNothing;

begin
  RenderSource(Doc('<use xlink:href="#absent"/>'));
  AssertEquals('a dangling use paints nothing', 0, CountLines('fill-path'));
end;


procedure TTestSVGRenderUse.TestCyclicUseRendersNothing;

begin
  RenderSource(Doc('<g id="loop"><use xlink:href="#loop"/></g>'));
  AssertEquals('a self-referencing use paints nothing', 0,
    CountLines('fill-path'));
end;


procedure TTestSVGRenderUse.TestSymbolBecomesAViewport;

begin
  RenderSource(Doc('<defs><symbol id="s" viewBox="0 0 10 10">'
    + '<rect width="10" height="10"/></symbol></defs>'
    + '<use xlink:href="#s" width="20" height="20"/>'));
  AssertEquals('the symbol viewBox maps into the use size',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 ctm=[2 0 0 2 0 0]',
    FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestSymbolTakesTheSizeFromTheUse;

begin
  RenderSource(Doc('<defs><symbol id="s" viewBox="0 0 10 10" width="5" '
    + 'height="5"><rect width="10" height="10"/></symbol></defs>'
    + '<use xlink:href="#s" x="10" y="10" width="40" height="40"/>'));
  AssertEquals('the use size wins over the symbol size',
    'fill-path rule=nonzero paint=color(#000000ff) opacity=1 '
    + 'ctm=[4 0 0 4 10 10]', FirstLine('fill-path'));
end;


procedure TTestSVGRenderUse.TestSymbolOutsideAUseIsNotRendered;

begin
  RenderSource(Doc('<symbol id="s"><rect width="10" height="10"/></symbol>'));
  AssertEquals('a symbol draws nothing by itself', 0,
    CountLines('fill-path'));
end;


{ TTestSVGRenderConditions }

procedure TTestSVGRenderConditions.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGRenderConditions.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FBackend);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGRenderConditions.RenderSource(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  FBackend := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FBackend);
end;


function TTestSVGRenderConditions.FillCount: Integer;

begin
  Result := CountOfLines(FBackend.Log, 'fill-path');
end;


function TTestSVGRenderConditions.FirstFillPaint: String;

var
  lIndex: Integer;
  lLine: String;

begin
  Result := '';
  lIndex := IndexOfLine(FBackend.Log, 'fill-path');
  if lIndex < 0 then
    Exit;
  lLine := TrimLeft(FBackend.Log[lIndex]);
  lIndex := Pos('paint=', lLine);
  Result := Copy(lLine, lIndex, Pos(' opacity', lLine) - lIndex);
end;


procedure TTestSVGRenderConditions.TestSwitchTakesTheFirstPassingChild;

begin
  RenderSource(Doc('<switch><rect width="4" height="4" fill="red"/>'
    + '<rect width="4" height="4" fill="blue"/></switch>'));
  AssertEquals('only one branch is taken', 1, FillCount);
  AssertEquals('and it is the first', 'paint=color(#ff0000ff)',
    FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestSwitchSkipsUnknownExtensions;

begin
  RenderSource(Doc('<switch>'
    + '<rect requiredExtensions="http://example.com/x" width="4" height="4" '
    + 'fill="red"/><rect width="4" height="4" fill="blue"/></switch>'));
  AssertEquals('the fallback is taken', 'paint=color(#0000ffff)',
    FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestSwitchSkipsUnknownFeatures;

begin
  RenderSource(Doc('<switch><rect requiredFeatures='
    + '"http://www.w3.org/TR/SVG11/feature#Filter" width="4" height="4" '
    + 'fill="red"/><rect width="4" height="4" fill="blue"/></switch>'));
  AssertEquals('an unimplemented feature is refused', 'paint=color(#0000ffff)',
    FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestSwitchAcceptsASupportedFeature;

begin
  RenderSource(Doc('<switch><rect requiredFeatures='
    + '"http://www.w3.org/TR/SVG11/feature#Shape" width="4" height="4" '
    + 'fill="red"/><rect width="4" height="4" fill="blue"/></switch>'));
  AssertEquals('a feature the renderer has is accepted',
    'paint=color(#ff0000ff)', FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestEmptyRequiredFeaturesFails;

begin
  RenderSource(Doc('<switch><rect requiredFeatures="" width="4" height="4" '
    + 'fill="red"/><rect width="4" height="4" fill="blue"/></switch>'));
  AssertEquals('an empty feature list evaluates to false',
    'paint=color(#0000ffff)', FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestSystemLanguageMatchesThePrimaryTag;

begin
  RenderSource(Doc('<switch><rect systemLanguage="fr,en-GB" width="4" '
    + 'height="4" fill="red"/><rect width="4" height="4" fill="blue"/>'
    + '</switch>'));
  AssertEquals('a region tag matches its primary language',
    'paint=color(#ff0000ff)', FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestSystemLanguageWithoutAMatchIsSkipped;

begin
  RenderSource(Doc('<switch><rect systemLanguage="fr,de" width="4" '
    + 'height="4" fill="red"/><rect width="4" height="4" fill="blue"/>'
    + '</switch>'));
  AssertEquals('no matching language takes the fallback',
    'paint=color(#0000ffff)', FirstFillPaint);
end;


procedure TTestSVGRenderConditions.TestConditionsApplyOutsideASwitch;

begin
  RenderSource(Doc('<rect requiredExtensions="http://example.com/x" '
    + 'width="4" height="4"/>'));
  AssertEquals('a failing condition suppresses the element anywhere', 0,
    FillCount);
end;


procedure TTestSVGRenderConditions.TestEveryImplementedFeatureIsClaimed;

const
  Implemented: array[0..6] of String = ('Shape', 'Gradient', 'Pattern',
    'Clip', 'Mask', 'Image', 'BasicText');

var
  I: Integer;

begin
  // When the renderer draws a feature but does not claim it, a switch
  // skips content it could have drawn.
  for I := Low(Implemented) to High(Implemented) do
    begin
    RenderSource(Doc('<switch><rect requiredFeatures='
      + '"http://www.w3.org/TR/SVG11/feature#' + Implemented[I] + '" '
      + 'width="4" height="4" fill="red"/>'
      + '<rect width="4" height="4" fill="blue"/></switch>'));
    AssertEquals(Implemented[I] + ' is drawn and so must be claimed',
      'paint=color(#ff0000ff)', FirstFillPaint);
    end;
end;


procedure TTestSVGRenderConditions.TestFeaturesNotImplementedAreNotClaimed;

const
  Absent: array[0..3] of String = ('Filter', 'BasicFilter', 'Font',
    'Animation');

var
  I: Integer;

begin
  for I := Low(Absent) to High(Absent) do
    begin
    RenderSource(Doc('<switch><rect requiredFeatures='
      + '"http://www.w3.org/TR/SVG11/feature#' + Absent[I] + '" '
      + 'width="4" height="4" fill="red"/>'
      + '<rect width="4" height="4" fill="blue"/></switch>'));
    AssertEquals(Absent[I] + ' is not drawn and so must not be claimed',
      'paint=color(#0000ffff)', FirstFillPaint);
    end;
end;


{ TTestSVGRenderGoldens }

procedure TTestSVGRenderGoldens.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
  FTrace := TSVGTraceBackend.Create;
end;


procedure TTestSVGRenderGoldens.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FTrace);
  FreeAndNil(FSoft);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGRenderGoldens.AlphaText: TStringList;

var
  X, Y: Integer;
  lLine: String;

begin
  Result := TStringList.Create;
  for Y := 0 to FSoft.Image.Height - 1 do
    begin
    lLine := '';
    for X := 0 to FSoft.Image.Width - 1 do
      lLine := lLine + ShadeOf(FSoft.Image.Colors[X, Y].Alpha div 257);
    Result.Add(TrimRight(lLine));
    end;
end;


procedure TTestSVGRenderGoldens.TestRenderWalkGolden;

begin
  FDocument := ReadSVGFile(DataDir + 'render.svg');
  FRenderer.Render(FDocument, FTrace);
  AssertGolden(Self, 'render-walk', FTrace.Log);
end;


procedure TTestSVGRenderGoldens.TestViewportGolden;

begin
  FDocument := ReadSVGFile(DataDir + 'viewports.svg');
  FRenderer.Render(FDocument, FTrace);
  AssertGolden(Self, 'render-viewports', FTrace.Log);
end;


procedure TTestSVGRenderGoldens.TestDocumentCoverageGolden;

var
  lLines, lBlock: TStringList;

begin
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGFile(DataDir + 'render.svg');
  FRenderer.RenderToSize(FDocument, FSoft, 50, 25);
  lLines := TStringList.Create;
  try
    lLines.Add('# render.svg at 50 by 25');
    lBlock := AlphaText;
    try
      lLines.AddStrings(lBlock);
    finally
      lBlock.Free;
    end;
    AssertGolden(Self, 'render-coverage', lLines);
  finally
    lLines.Free;
  end;
end;


procedure TTestSVGRenderView.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGRenderView.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


procedure TTestSVGRenderView.RenderUnderView;

begin
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  FBackend := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100" '
    + 'viewBox="0 0 100 100">'
    + '<view id="quarter" viewBox="0 0 50 50"/>'
    + '<rect x="0" y="0" width="10" height="10"/></svg>');
  FRenderer.Render(FDocument, FBackend);
end;


function TTestSVGRenderView.FirstFillMatrix: String;

var
  lIndex, lAt: Integer;
  lLine: String;

begin
  Result := '';
  lIndex := IndexOfLine(FBackend.Log, 'fill-path');
  if lIndex < 0 then
    Exit;
  lLine := FBackend.Log[lIndex];
  lAt := Pos('ctm=', lLine);
  if lAt = 0 then
    Exit;
  Result := Copy(lLine, lAt + 4, Length(lLine));
  lAt := Pos(']', Result);
  if lAt > 0 then
    Result := Copy(Result, 1, lAt);
end;


procedure TTestSVGRenderView.TestWithoutAViewTheRootFramesItself;

begin
  RenderUnderView;
  AssertEquals('the viewBox of the root maps one to one',
    'identity', FirstFillMatrix);
end;


procedure TTestSVGRenderView.TestAViewBoxFramesTheRootInstead;

var
  lView: TSVGView;

begin
  lView := TSVGView.None;
  lView.ViewBox := TSVGRect.CreateSize(0, 0, 50, 50);
  lView.HasViewBox := True;
  FRenderer.View := lView;
  RenderUnderView;
  AssertEquals('half the document fills the frame, so it draws twice as '
    + 'large', '[2 0 0 2 0 0]', FirstFillMatrix);
end;


procedure TTestSVGRenderView.TestAViewRatioIsUsedWithTheRootViewBox;

var
  lView: TSVGView;

begin
  lView := TSVGView.None;
  lView.ViewBox := TSVGRect.CreateSize(0, 0, 50, 100);
  lView.HasViewBox := True;
  FRenderer.View := lView;
  RenderUnderView;
  // Half as wide as it is tall, in a square frame: it is scaled by one
  // either way, and the default ratio centres it across.
  AssertEquals('without a ratio of its own the view is centred',
    '[1 0 0 1 25 0]', FirstFillMatrix);
  lView.Ratio := TSVGPreserveAspectRatio.Create(paXMinYMin, msMeet);
  lView.HasRatio := True;
  FRenderer.View := lView;
  RenderUnderView;
  AssertEquals('and the ratio of the view puts it against the left edge',
    'identity', FirstFillMatrix);
end;


procedure TTestSVGRenderView.TestAViewTransformMovesWhatTheViewFrames;

var
  lView: TSVGView;

begin
  lView := TSVGView.None;
  lView.Transform := TSVGMatrix.Translation(10, 20);
  lView.HasTransform := True;
  FRenderer.View := lView;
  RenderUnderView;
  AssertEquals('a transform alone moves the drawing', '[1 0 0 1 10 20]',
    FirstFillMatrix);
  lView.ViewBox := TSVGRect.CreateSize(0, 0, 50, 50);
  lView.HasViewBox := True;
  FRenderer.View := lView;
  RenderUnderView;
  AssertEquals('under a viewBox it moves in the units the view frames, so '
    + 'the scale of the frame applies to it as well', '[2 0 0 2 20 40]',
    FirstFillMatrix);
end;


procedure TTestSVGRenderView.TestAViewFramesTheRootAndNotANestedSVG;

var
  lView: TSVGView;

begin
  lView := TSVGView.None;
  lView.ViewBox := TSVGRect.CreateSize(0, 0, 50, 50);
  lView.HasViewBox := True;
  FRenderer.View := lView;
  FreeAndNil(FDocument);
  FreeAndNil(FBackend);
  FBackend := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100" '
    + 'viewBox="0 0 100 100">'
    + '<svg x="0" y="0" width="100" height="100" viewBox="0 0 100 100">'
    + '<rect x="0" y="0" width="10" height="10"/></svg></svg>');
  FRenderer.Render(FDocument, FBackend);
  AssertEquals('the root doubles and the nested one is left alone',
    '[2 0 0 2 0 0]', FirstFillMatrix);
end;


initialization
  RegisterTest('render', TTestSVGRenderView);
  RegisterTest('render', TTestSVGRenderWalk);
  RegisterTest('render', TTestSVGRenderSize);
  RegisterTest('render', TTestSVGRenderUse);
  RegisterTest('render', TTestSVGRenderConditions);
  RegisterTest('render', TTestSVGRenderGoldens);
end.
