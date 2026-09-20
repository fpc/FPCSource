{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for referenced content: clip paths, masks, patterns and images.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgrefs;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Hash.Base64, System.Math,
     FpImage, FpcUnit.Test, FpcUnit.Registry, svggoldens, svgpixels,
     svgstubfont, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.style,
     fpsvg.backend, fpsvg.trace, fpsvg.render, fpsvg.soft;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, base64, math, fpimage, fpcunit, testregistry,
     svggoldens, svgpixels, svgstubfont, fpsvg.types, fpsvg.dom,
     fpsvg.read, fpsvg.style, fpsvg.backend, fpsvg.trace,
     fpsvg.render, fpsvg.soft;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Supplies one image of a fixed size and colour for every request. }
  TTestImageResolver = class(TObject, ISVGImageResolver)
  private
    FWidth, FHeight: Integer;
    FCalls: Integer;
    FLastHRef: String;
  public
    constructor Create(aWidth, aHeight: Integer);
    function ResolveImage(const aHRef, aBaseURI: String): ISVGImageSource;
    // Number of times an image was requested.
    property Calls: Integer read FCalls;
    // The reference of the most recent request.
    property LastHRef: String read FLastHRef;
  end;

  { Shared setup for rendering a document through both backends. }
  TTestSVGReferenceCase = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTrace: TSVGTraceBackend;
    FSoft: TSVGSoftBackend;
    FRenderer: TSVGRenderer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    // Renders source through the tracing backend.
    procedure Trace(const aText: String);
    // Renders source through the software backend at the document size.
    procedure Draw(const aText: String);
    // The red of an eight by eight patch of noise at aX, added up.
    function NoiseSum(aX: Integer): Integer;
    // The alpha of one pixel of the software surface, from 0 to 1.
    function AlphaAt(aX, aY: Integer): Double;
    // The alpha of the whole software surface, added up in pixel units.
    function CoveredArea: Double;
    // Number of trace lines that start with the given text.
    function CountLines(const aPrefix: String): Integer;
    // The first trace line that starts with the given text.
    function FirstLine(const aPrefix: String): String;
  end;

  { A filter resolved into the chain a backend is handed. Nothing here
    works a pixel: the trace says what the chain came to. }
  TTestSVGFilterChain = class(TTestSVGReferenceCase)
  published
    procedure TestAFilterOpensALayerAndCloses;
    procedure TestTheRegionIsATenthOutsideTheBox;
    procedure TestTheRegionMayBeGivenInUserSpace;
    procedure TestAPrimitiveReadsTheOneBeforeIt;
    procedure TestAPrimitiveReadsAResultByName;
    procedure TestTheFirstPrimitiveReadsTheSource;
    procedure TestASourceIsNamedByItsOwnName;
    procedure TestABlurHasItsTwoDeviations;
    procedure TestAMergeReadsEveryNodeItHolds;
    procedure TestAFilterHoldingNothingDrawsNothing;
    procedure TestAFilterNamingNothingDrawsNothing;
    procedure TestAFilterOfNoneLeavesTheElementAlone;
  end;

  { What the filter primitives do to the pixels of a layer. }
  TTestSVGFilterDrawing = class(TTestSVGReferenceCase)
  published
    procedure TestAFloodFillsTheRegion;
    procedure TestAnOffsetMovesWhatWasDrawn;
    procedure TestABlurSpreadsBeyondTheShape;
    procedure TestABlurOfNothingLeavesTheShapeAlone;
    procedure TestAMergeLaysOneOverTheOther;
    procedure TestACompositeInKeepsTheOverlap;
    procedure TestAColourMatrixTurnsTheChannels;
    procedure TestAnAlphaSourceKeepsOnlyTheShape;
    procedure TestABlendMultipliesTheTwo;
    procedure TestADilateThickensTheShape;
    procedure TestAnErodeThinsIt;
    procedure TestATransferTableTurnsAChannel;
    procedure TestATransferOfNothingLeavesItAlone;
    procedure TestAConvolutionOfOneLeavesTheShape;
    procedure TestAConvolutionSpreadsWhatItWeighs;
    procedure TestADisplacementOfNothingMovesNothing;
    procedure TestTurbulenceFillsTheRegionWithNoise;
    procedure TestTurbulenceIsTheSameForTheSameSeed;
    procedure TestALightLightsTheShape;
    procedure TestAConvolutionIsReadBackwards;
    procedure TestALightFollowsTheReliefOfTheSurface;
    procedure TestTheNoiseIsWhatTheAppendixGives;
    procedure TestNoiseStandsInTheSpaceTheShapeDrawsIn;
    procedure TestASeedIsCutOffAndNotRounded;
    procedure TestAnImageMayNameAnElementOfTheDocument;
    procedure TestAnImageNamingNothingDrawsNothing;
    procedure TestAPrimitiveIsHeldToTheBoxItNames;
    procedure TestAnAxisThePrimitiveOmitsIsTheOneOfTheRegion;
    procedure TestABoxUnitCornerStartsAtTheCornerOfTheBox;
    procedure TestAPercentageInBoxUnitsIsAFractionOfTheBox;
    procedure TestABiasReachesTheColourAndNotTheAlpha;
    procedure TestTheOpacityIsLaidOnTheResultNotOnTheSource;
    procedure TestFillPaintIsAPlaneOfTheFill;
    procedure TestStrokePaintIsAPlaneOfTheStroke;
    procedure TestAPlaneOfAGradientIsReadAgainstTheBoxOfTheElement;
    procedure TestAPlaneOfAPatternIsEmpty;
    procedure TestAPlaneHasTheOpacityOfThePaint;
    procedure TestAPlaneIsTurnedIntoTheLightTheChainWorksIn;
    procedure TestABlurOfAnEvenWidthKeepsToItsOwnSpread;
    procedure TestAnImageIsTurnedIntoTheLightTheChainWorksIn;
    procedure TestAFloodIsTurnedIntoThatLightToo;
    procedure TestALightingColourIsTurnedIntoItAsWell;
    procedure TestAPointLightStandsWhereTheDocumentPutsIt;
    procedure TestASpotLightsItsConeAndNothingOutsideIt;
    procedure TestASpotExponentNarrowsTheBeam;
    procedure TestALightInBoxUnitsIsReadAgainstTheBox;
    procedure TestTheNormalIsWeighedAsSVGWeighsIt;
    procedure TestALightShiningAwayFromTheEyeLightsNothing;
    procedure TestALightingColourMayNameTheColourItStandsIn;
    procedure TestAFloodColourMayNameTheColourItStandsIn;
    procedure TestBackgroundImageReadsWhatWasDrawnUnder;
    procedure TestBackgroundAlphaReadsThatInBlack;
    procedure TestWithoutEnableBackgroundThereIsNoBackground;
  end;

  TTestSVGClipPath = class(TTestSVGReferenceCase)
  published
    procedure TestSingleShapeClipUsesAClip;
    procedure TestClipRestrictsTheArea;
    procedure TestClipRuleIsHonoured;
    procedure TestMultipleShapesClipToTheirUnion;
    procedure TestBoundingBoxUnitsScaleWithTheShape;
    procedure TestClipOnAGroupCoversItsChildren;
    procedure TestMissingClipReferenceIsIgnored;
    procedure TestClipPathNoneIsIgnored;
    procedure TestClipPathElementIsNotRenderedItself;
    procedure TestClipTransformIsApplied;
    procedure TestUseInsideAClipPathContributesItsShape;
    procedure TestUseInsideAClipPathAppliesItsOffset;
    procedure TestClipChildIsCutByItsOwnClipFirst;
    procedure TestClipChildWithoutItsOwnClipIsWhole;
    procedure TestAClipPathIsCutByItsOwnClip;
    procedure TestAClipPathNamingItselfStillDraws;
    procedure TestASelectorMayAskAfterAnAttribute;
    procedure TestAUsedElementIsStyledWhereItWasWritten;
    procedure TestAUsedElementStillTakesWhatItInherits;
  end;

  TTestSVGMaskElement = class(TTestSVGReferenceCase)
  published
    procedure TestMaskWrapsTheElementInALayer;
    procedure TestWhiteMaskKeepsTheShape;
    procedure TestBlackMaskHidesTheShape;
    procedure TestGreyMaskDimsTheShape;
    procedure TestMaskElementIsNotRenderedItself;
    procedure TestMissingMaskReferenceIsIgnored;
    procedure TestMaskAndClipBothApply;
  end;

  TTestSVGColourSpace = class(TTestSVGReferenceCase)
  published
    procedure TestHalfCoverBlendsInSRGBByDefault;
    procedure TestHalfCoverBlendsInLinearRGBWhenAsked;
    procedure TestTheSpaceIsInheritedAndPutBack;
    procedure TestAMaskIsWeighedInTheSpaceItAsksFor;
  end;

  TTestSVGPattern = class(TTestSVGReferenceCase)
  published
    procedure TestPatternTilesAcrossTheShape;
    procedure TestPatternIsClippedToTheShape;
    procedure TestUserSpaceUnitsTileInUserSpace;
    procedure TestPatternWithoutSizeDrawsNothing;
    procedure TestMissingPatternKeepsTheInheritedFill;
    procedure TestPatternContentIsNotRenderedWhereItStands;
    procedure TestPatternViewBoxScalesTheTile;
    procedure TestEmptyPatternFallsBackToTheColour;
    procedure TestMissingReferenceFallsBackToTheColour;
    procedure TestFallbackOfNonePaintsNothing;
    procedure TestPatternWinsOverItsFallback;
    procedure TestPatternContentDoesNotTakeTheStrokeOfWhatItFills;
    procedure TestTheShapeIsStillStrokedAfterAPatternFill;
    procedure TestFallbackAppliesToAStrokeToo;
    procedure TestGradientStrokesLikeAnyPaint;
    procedure TestTextIsFilledWithAPattern;
    procedure TestTextWithAnEmptyPatternTakesItsFallback;
    procedure TestAStrokeIsDrawnWithAPattern;
    procedure TestAPatternStrokeStaysInsideTheLine;
    procedure TestTextIsStrokedWithAPattern;
    procedure TestAPatternStrokeWithNoWidthTakesItsFallback;
    procedure TestAPatternWithoutATileTakesTheOneItNames;
    procedure TestAPatternTakesTheAttributesItLeftOut;
    procedure TestAPatternKeepsATileOfItsOwn;
    procedure TestATileIsFoundAlongTheWholeChain;
  end;

  TTestSVGImageElement = class(TTestSVGReferenceCase)
  private
    FResolver: TTestImageResolver;
    // Renders source with an image resolver that supplies one image.
    procedure DrawWithImage(const aText: String; aWidth, aHeight: Integer);
  protected
    procedure TearDown; override;
  published
    procedure TestImageIsDrawnIntoItsRectangle;
    procedure TestAFilterOnAnImageStillDrawsIt;
    procedure TestTheFilterRegionOfAnImageIsItsRectangle;
    procedure TestTheBoxOfAnImageIsWhatItDraws;
    procedure TestTheBoxOfAnImageToldToCoverIsTheViewport;
    procedure TestAFilterImageIsFittedIntoItsBox;
    procedure TestAFilterImageMayBeToldToStretch;
    procedure TestImageAsksTheResolverForTheHRef;
    procedure TestWithoutAResolverNothingIsDrawn;
    procedure TestZeroSizedImageIsSkipped;
    procedure TestImagePreservesItsAspectRatio;
    procedure TestSliceIsClippedToTheViewport;
  end;

  { What the referencing features do to colour. The area tests above read
    the alpha only, so they cannot see colour at all. }
  TTestSVGReferenceColours = class(TTestSVGReferenceCase)
  private
    // Fails unless the pixel holds the given colour.
    procedure AssertColour(const aMessage: String; aX, aY: Integer;
      const aExpected: TSVGColor);
  published
    procedure TestClipKeepsTheColourOfWhatItCuts;
    procedure TestUnionClipKeepsTheColourToo;
    procedure TestClipDoesNotTintTheEdge;
    procedure TestWhiteMaskLeavesTheColourAlone;
    procedure TestGreyMaskDimsWithoutTinting;
    procedure TestPatternTilesKeepTheirOwnColour;
    procedure TestPatternFallbackPaintsItsColour;
    procedure TestImageKeepsThePixelsItWasGiven;
    procedure TestTranslucentImageKeepsItsHue;
  end;

  { References into a second file: a use that points to an element of
    another file, and an image that points to a whole document. }
  TTestSVGExternalReferences = class(TTestSVGReferenceCase)
  private
    FResolver: TSVGFileDocumentResolver;
    FSheets: TSVGFileStyleSheetResolver;
    // Renders source with a resolver that reads the test data directory.
    procedure DrawWithDocuments(const aText: String);
    procedure DrawAsFile(const aName, aText: String);
    // Renders source with the document and stylesheet resolvers both
    // reading that directory.
    procedure DrawWithSheets(const aText: String);
  protected
    procedure TearDown; override;
  published
    procedure TestUseDrawsAnElementOfAnotherFile;
    procedure TestUseAppliesItsOffsetToTheOtherFile;
    procedure TestUseKeepsTheColourOfWhatItNames;
    procedure TestUseOfAWholeFileScalesItToTheUseSize;
    procedure TestNestedUseInsideTheOtherFileIsResolved;
    procedure TestImageDrawsAWholeDocument;
    procedure TestImageDocumentHonoursItsAspectRatio;
    procedure TestWithoutAResolverNothingIsDrawn;
    procedure TestMissingFileDrawsNothing;
    procedure TestFragmentNamingNothingDrawsNothing;
    procedure TestAFileNamedTwiceIsReadOnce;
    procedure TestFillNamesAGradientOfAnotherFile;
    procedure TestGradientOfAnotherFileInheritsThereNotHere;
    procedure TestFillNamesAPatternOfAnotherFile;
    procedure TestExternalPaintWithoutAResolverFallsBack;
    procedure TestExternalPaintNamingNothingFallsBack;
    procedure TestAUseOfAFileNamingItselfDrawsItOnce;
    procedure TestTwoUsesOfOneFileAreBothDrawn;
    procedure TestAnImageNamingItselfStopsToo;
    procedure TestAnImageThatCyclesDrawsNothing;
    procedure TestAnImageCyclingThroughAnotherFileDrawsNothing;
    procedure TestAnImageNamingTheDocumentBeingDrawnDrawsNothing;
    procedure TestTwoImagesOfOneFileAreBothDrawn;
    procedure TestAnImageHasItsDocumentInADataURI;
    procedure TestAFileDrawnAsAnImageKeepsItsOwnGradient;
    procedure TestADocumentInAURIKeepsItsOwnGradient;
    procedure TestAnImageInsideAnImageResolvesFromItsOwnFile;
    procedure TestTheOuterFileIsWhatFollowsResolveAgainst;
    procedure TestAUsedFillNamesTheGradientOfItsOwnFile;
    procedure TestAUsedClipNamesTheClipPathOfItsOwnFile;
    procedure TestAStyleSheetInstructionIsLoaded;
    procedure TestAnAlternateStyleSheetIsLeftOut;
    procedure TestAStyleSheetOfAnotherTypeIsLeftOut;
    procedure TestWithoutAResolverNoStyleSheetIsLoaded;
    procedure TestAStyleElementWinsOverTheSheetBeforeIt;
    procedure TestAUsedElementTakesTheStyleOfItsOwnFile;
    procedure TestAStyleHereDoesNotReachAUsedElement;
    procedure TestTheStyleOfAnotherFileStaysThere;
    procedure TestAnImageDocumentTakesItsOwnStyle;
    procedure TestAnImportLoadsTheSheetItNames;
    procedure TestTheImportingSheetWinsOverWhatItImports;
    procedure TestAnImportThatLeadsNowhereLeavesTheRest;
    procedure TestAnImportInACommentIsNotFollowed;
    procedure TestAnImportedSheetMayImportAnother;
  end;

  { Where a viewport sits, and which attributes place it. }
  TTestSVGViewport = class(TTestSVGReferenceCase)
  published
    procedure TestTheOutermostSVGIsNotPlacedByItsXAndY;
    procedure TestANestedSVGIsPlacedByItsXAndY;
  end;

  TTestSVGReferenceGoldens = class(TTestSVGReferenceCase)
  private
    // The alpha channel of the software surface, one character per pixel.
    function AlphaText: TStringList;
  published
    procedure TestReferenceWalkGolden;
    procedure TestReferenceCoverageGolden;
  end;

  { Finds the files that font-face rules name. }
  TTestSVGFontFileResolving = class(TTestCase)
  private
    FResolver: TSVGFileFontResolver;
    FAsked: String;
    FSupply: String;
    // Returns the value of FSupply, and records the url it was asked
    // for.
    procedure SupplyFont(aSender: TObject; const aURL, aBaseURI: String;
      var aFileName: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAFileBesideTheDocumentIsFound;
    procedure TestAnAbsentFileResolvesToNothing;
    procedure TestTheSearchPathIsTriedNext;
    procedure TestTheEventIsAskedWhenNothingIsFound;
    procedure TestTheEventIsNotAskedWhenTheFileIsThere;
    procedure TestAnEventNamingNothingReadableIsRefused;
  end;

  { Reading a stylesheet from a file. }
  TTestSVGStyleSheetResolving = class(TTestCase)
  private
    FResolver: TSVGFileStyleSheetResolver;
    FFileName: String;
    // Writes a sheet of those bytes beside the test data and returns its
    // name.
    function WriteSheet(const aBytes: RawByteString): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestASheetIsReadByteForByte;
    procedure TestTheLineEndingsOfASheetAreKept;
    procedure TestAnAbsentSheetIsEmpty;
  end;

  TTestSVGImageResolving = class(TTestCase)
  private
    FResolver: TSVGFileImageResolver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMissingFileResolvesToNothing;
    procedure TestEmptyReferenceResolvesToNothing;
    procedure TestDataURIWithoutBase64IsRefused;
    procedure TestUnreadableDataIsRefused;
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


// Wraps markup in a root element of a hundred units.
function Doc(const aBody: String): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100">'
    + aBody + '</svg>';
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


{ TTestImageResolver }

constructor TTestImageResolver.Create(aWidth, aHeight: Integer);

begin
  inherited Create;
  FWidth := aWidth;
  FHeight := aHeight;
end;


function TTestImageResolver.ResolveImage(
  const aHRef, aBaseURI: String): ISVGImageSource;

var
  lImage: TFPMemoryImage;
  X, Y: Integer;

begin
  Inc(FCalls);
  FLastHRef := aHRef;
  Result := nil;
  if (FWidth <= 0) or (FHeight <= 0) then
    Exit;
  lImage := TFPMemoryImage.Create(FWidth, FHeight);
  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
      lImage.Colors[X, Y] := TFPColor(TSVGColor.FromBytes(255, 0, 0, 255));
  Result := TSVGImageSource.Create(lImage, True);
end;


{ TTestSVGReferenceCase }

procedure TTestSVGReferenceCase.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGReferenceCase.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FTrace);
  FreeAndNil(FSoft);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGReferenceCase.Trace(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FTrace);
  FTrace := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FTrace);
end;


procedure TTestSVGReferenceCase.Draw(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FSoft);
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FSoft);
end;


function TTestSVGReferenceCase.NoiseSum(aX: Integer): Integer;

var
  X, Y: Integer;

begin
  Result := 0;
  for Y := 1 to 8 do
    for X := aX to aX + 7 do
      Result := Result + FSoft.Image.Colors[X, Y].Red shr 8;
end;


function TTestSVGReferenceCase.AlphaAt(aX, aY: Integer): Double;

begin
  Result := FSoft.Image.Colors[aX, aY].Alpha / 65535;
end;


function TTestSVGReferenceCase.CoveredArea: Double;

var
  X, Y: Integer;

begin
  Result := 0;
  for Y := 0 to FSoft.Image.Height - 1 do
    for X := 0 to FSoft.Image.Width - 1 do
      Result := Result + FSoft.Image.Colors[X, Y].Alpha / 65535;
end;


function TTestSVGReferenceCase.CountLines(const aPrefix: String): Integer;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to FTrace.Log.Count - 1 do
    if Pos(aPrefix, TrimLeft(FTrace.Log[I])) = 1 then
      Inc(Result);
end;


function TTestSVGReferenceCase.FirstLine(const aPrefix: String): String;

var
  lIndex: Integer;

begin
  lIndex := IndexOfLine(FTrace.Log, aPrefix);
  if lIndex < 0 then
    Result := ''
  else
    Result := TrimLeft(FTrace.Log[lIndex]);
end;


{ TTestSVGClipPath }

procedure TTestSVGClipPath.TestSingleShapeClipUsesAClip;

begin
  Trace(Doc('<clipPath id="c"><rect x="10" y="10" width="20" height="20"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" clip-path="url(#c)"/>'));
  AssertEquals('one shape needs a clip and no layer', 2,
    CountLines('push-clip'));
  AssertEquals('and no layer is opened for it', 0, CountLines('push-layer'));
end;


procedure TTestSVGClipPath.TestClipRestrictsTheArea;

begin
  Draw(Doc('<clipPath id="c"><rect x="10" y="10" width="20" height="20"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" clip-path="url(#c)"/>'));
  AssertEquals('only the clip rectangle is painted', 400.0, CoveredArea, 0.01);
  AssertEquals('a pixel inside the clip is painted', 1.0, AlphaAt(20, 20),
    Delta);
  AssertEquals('a pixel outside it is not', 0.0, AlphaAt(50, 50), Delta);
end;


procedure TTestSVGClipPath.TestClipRuleIsHonoured;

begin
  Draw(Doc('<clipPath id="c">'
    + '<path clip-rule="evenodd" d="M10 10H50V50H10Z M20 20H40V40H20Z"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" clip-path="url(#c)"/>'));
  AssertEquals('the even-odd hole is not painted', 0.0, AlphaAt(30, 30),
    Delta);
  AssertEquals('the ring around it is', 1.0, AlphaAt(15, 15), Delta);
  AssertEquals('the area is the ring alone', 1600.0 - 400.0, CoveredArea,
    0.01);
end;


procedure TTestSVGClipPath.TestMultipleShapesClipToTheirUnion;

begin
  Draw(Doc('<clipPath id="c">'
    + '<rect x="10" y="10" width="20" height="20"/>'
    + '<rect x="20" y="20" width="20" height="20"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" clip-path="url(#c)"/>'));
  AssertEquals('the union of two overlapping squares is painted',
    400.0 + 400.0 - 100.0, CoveredArea, 0.01);
  AssertEquals('a pixel in the first square is painted', 1.0, AlphaAt(12, 12),
    Delta);
  AssertEquals('a pixel in the second is too', 1.0, AlphaAt(38, 38), Delta);
  AssertEquals('and one in the overlap is not doubled', 1.0, AlphaAt(25, 25),
    Delta);
end;


procedure TTestSVGClipPath.TestBoundingBoxUnitsScaleWithTheShape;

begin
  Draw(Doc('<clipPath id="c" clipPathUnits="objectBoundingBox">'
    + '<rect x="0" y="0" width="0.5" height="0.5"/></clipPath>'
    + '<rect x="20" y="20" width="40" height="40" clip-path="url(#c)"/>'));
  AssertEquals('half the box in each direction is a quarter of the area',
    400.0, CoveredArea, 0.01);
  AssertEquals('the near corner is painted', 1.0, AlphaAt(25, 25), Delta);
  AssertEquals('the far corner is not', 0.0, AlphaAt(55, 55), Delta);
end;


procedure TTestSVGClipPath.TestClipOnAGroupCoversItsChildren;

begin
  Draw(Doc('<clipPath id="c"><rect x="0" y="0" width="20" height="100"/>'
    + '</clipPath>'
    + '<g clip-path="url(#c)"><rect width="50" height="50"/>'
    + '<rect x="0" y="50" width="50" height="50"/></g>'));
  AssertEquals('both children are cut to the same clip', 2000.0, CoveredArea,
    0.01);
end;


procedure TTestSVGClipPath.TestMissingClipReferenceIsIgnored;

begin
  Draw(Doc('<rect width="20" height="20" clip-path="url(#absent)"/>'));
  AssertEquals('an unresolved clip leaves the shape alone', 400.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGClipPath.TestClipPathNoneIsIgnored;

begin
  Draw(Doc('<rect width="20" height="20" clip-path="none"/>'));
  AssertEquals('clip-path none leaves the shape alone', 400.0, CoveredArea,
    0.01);
end;


procedure TTestSVGClipPath.TestClipPathElementIsNotRenderedItself;

begin
  Draw(Doc('<clipPath id="c"><rect width="50" height="50"/></clipPath>'));
  AssertEquals('a clip path draws nothing by itself', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGClipPath.TestClipTransformIsApplied;

begin
  Draw(Doc('<clipPath id="c">'
    + '<rect x="0" y="0" width="20" height="20" transform="translate(30,30)"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" clip-path="url(#c)"/>'));
  AssertEquals('the clip shape moved with its transform', 1.0, AlphaAt(35, 35),
    Delta);
  AssertEquals('and left its untransformed place empty', 0.0, AlphaAt(5, 5),
    Delta);
end;


procedure TTestSVGClipPath.TestUseInsideAClipPathContributesItsShape;

begin
  Draw(Doc('<defs><rect id="r" x="10" y="10" width="20" height="20"/></defs>'
    + '<clipPath id="c"><use xlink:href="#r"/></clipPath>'
    + '<rect width="100" height="100" fill="black" clip-path="url(#c)"/>'));
  AssertEquals('the shape a use points to becomes the clip', 400.0, CoveredArea,
    0.01);
  AssertEquals('a pixel inside it is painted', 1.0, AlphaAt(20, 20), Delta);
  AssertEquals('and one outside is not', 0.0, AlphaAt(50, 50), Delta);
end;


procedure TTestSVGClipPath.TestUseInsideAClipPathAppliesItsOffset;

begin
  Draw(Doc('<defs><rect id="r" x="10" y="10" width="20" height="20"/></defs>'
    + '<clipPath id="c"><use xlink:href="#r" x="10" y="10"/></clipPath>'
    + '<rect width="100" height="100" fill="black" clip-path="url(#c)"/>'));
  AssertEquals('the clip moved with the use', 1.0, AlphaAt(25, 25), Delta);
  AssertEquals('and left the place it came from', 0.0, AlphaAt(15, 15),
    Delta);
end;


procedure TTestSVGClipPath.TestAUsedElementIsStyledWhereItWasWritten;

begin
  // SVG styles the content of a use as if it were at the position where
  // that content was written, so a selector that walks its ancestors and
  // siblings matches there, and not in the copy the use draws.
  Draw('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100">'
    + '<style type="text/css"><![CDATA['
    + '.wrap circle { fill: black }'
    + '.kin > rect { fill: black }'
    + 'rect + rect { fill: black }'
    + ']]></style>'
    + '<defs>'
    + '<g class="wrap"><circle id="c" cx="5" cy="5" r="5" fill="none"/></g>'
    + '<g class="kin"><rect id="r" width="10" height="10" fill="none"/></g>'
    + '<g><rect width="10" height="10"/>'
    + '<rect id="s" width="10" height="10" fill="none"/></g>'
    + '</defs>'
    + '<use xlink:href="#c" x="0" y="0"/>'
    + '<use xlink:href="#r" x="20" y="0"/>'
    + '<use xlink:href="#s" x="40" y="0"/>'
    + '</svg>');
  AssertEquals('the descendant rule reaches it', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('the child rule reaches it', 1.0, AlphaAt(25, 5), Delta);
  AssertEquals('the sibling rule reaches it', 1.0, AlphaAt(45, 5), Delta);
end;


procedure TTestSVGClipPath.TestAUsedElementStillTakesWhatItInherits;

begin
  // Selectors match at the position where the element was written, but
  // inheritance still comes down the tree that holds the use.
  Draw('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100">'
    + '<defs><rect id="r" width="10" height="10"/></defs>'
    + '<g fill="black"><use xlink:href="#r" x="0" y="0"/></g>'
    + '<g fill="none"><use xlink:href="#r" x="20" y="0"/></g>'
    + '</svg>');
  AssertEquals('the one under a black group is painted', 1.0,
    AlphaAt(5, 5), Delta);
  AssertEquals('the one under a group filling nothing is not', 0.0,
    AlphaAt(25, 5), Delta);
end;


procedure TTestSVGClipPath.TestASelectorMayAskAfterAnAttribute;

begin
  // An attribute selector matches through the CSS registry, which knows
  // the style properties and nothing else until a document is loaded.
  // Every attribute the document uses is registered as it loads.
  Draw('<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">'
    + '<style type="text/css"><![CDATA['
    + 'rect[cx] { fill: black }'
    + 'rect[foo~="two"] { fill: black }'
    + 'rect[lang|="en"] { fill: black }'
    + ']]></style>'
    + '<rect x="0" y="0" width="10" height="10" cx="1" fill="none"/>'
    + '<rect x="20" y="0" width="10" height="10" foo="one two" fill="none"/>'
    + '<rect x="40" y="0" width="10" height="10" lang="en-us" fill="none"/>'
    + '<rect x="60" y="0" width="10" height="10" fill="none"/>'
    + '</svg>');
  AssertEquals('the one with the attribute is painted', 1.0,
    AlphaAt(5, 5), Delta);
  AssertEquals('the one whose list holds the word too', 1.0,
    AlphaAt(25, 5), Delta);
  AssertEquals('and the one whose value starts with it', 1.0,
    AlphaAt(45, 5), Delta);
  AssertEquals('the one with none of them is left alone', 0.0,
    AlphaAt(65, 5), Delta);
end;


procedure TTestSVGClipPath.TestAClipPathIsCutByItsOwnClip;

begin
  // The clip path holds a 40 by 20 bar and is itself cut to the left 10
  // of it, so it clips to that overlap alone.
  Draw(Doc('<clipPath id="half"><rect x="0" y="0" width="10" height="20"/>'
    + '</clipPath>'
    + '<clipPath id="c" clip-path="url(#half)">'
    + '<rect x="0" y="0" width="40" height="20"/></clipPath>'
    + '<rect width="100" height="100" fill="black" clip-path="url(#c)"/>'));
  AssertEquals('only the overlap of the two is painted',
    10.0 * 20, CoveredArea, 0.01);
  AssertEquals('a pixel in the overlap is painted', 1.0, AlphaAt(5, 10),
    Delta);
  AssertEquals('one the outer clip cuts away is not', 0.0, AlphaAt(25, 10),
    Delta);
end;


procedure TTestSVGClipPath.TestAClipPathNamingItselfStillDraws;

begin
  // A clip path that refers to itself must not be followed for ever. It
  // clips to the shapes it holds.
  Draw(Doc('<clipPath id="c" clip-path="url(#c)">'
    + '<rect x="0" y="0" width="20" height="20"/></clipPath>'
    + '<rect width="100" height="100" fill="black" clip-path="url(#c)"/>'));
  AssertEquals('it cuts to the shape it holds', 400.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGClipPath.TestClipChildIsCutByItsOwnClipFirst;

begin
  Draw(Doc('<clipPath id="half"><rect x="0" y="0" width="10" height="20"/>'
    + '</clipPath>'
    + '<clipPath id="c">'
    + '<rect x="0" y="0" width="20" height="20" clip-path="url(#half)"/>'
    + '<rect x="20" y="0" width="20" height="20"/></clipPath>'
    + '<rect width="100" height="100" fill="black" clip-path="url(#c)"/>'));
  AssertEquals('the cut child contributes half of itself, the other all',
    10.0 * 20 + 20.0 * 20, CoveredArea, 0.01);
  AssertEquals('the kept half of the first child is painted', 1.0,
    AlphaAt(5, 10), Delta);
  AssertEquals('the half its own clip removed is not', 0.0, AlphaAt(15, 10),
    Delta);
  AssertEquals('and the child beside it is whole', 1.0, AlphaAt(30, 10),
    Delta);
end;


procedure TTestSVGClipPath.TestClipChildWithoutItsOwnClipIsWhole;

begin
  Draw(Doc('<clipPath id="c">'
    + '<rect x="0" y="0" width="20" height="20"/>'
    + '<rect x="20" y="0" width="20" height="20"/></clipPath>'
    + '<rect width="100" height="100" fill="black" clip-path="url(#c)"/>'));
  AssertEquals('both children contribute all of themselves', 2 * 20.0 * 20,
    CoveredArea, 0.01);
end;


{ TTestSVGMaskElement }

procedure TTestSVGMaskElement.TestMaskWrapsTheElementInALayer;

begin
  Trace(Doc('<mask id="m"><rect width="100" height="100" fill="white"/></mask>'
    + '<rect width="50" height="50" mask="url(#m)"/>'));
  AssertEquals('the element and the mask each take a layer', 2,
    CountLines('push-layer'));
  AssertEquals('the mask layer is applied as a mask', 1,
    CountLines('pop-layer-as-mask'));
end;


procedure TTestSVGMaskElement.TestWhiteMaskKeepsTheShape;

begin
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse">'
    + '<rect width="100" height="100" fill="white"/></mask>'
    + '<rect width="50" height="50" mask="url(#m)"/>'));
  AssertEquals('a white mask leaves the shape whole', 2500.0, CoveredArea,
    1.0);
  AssertEquals('and a pixel of it is opaque', 1.0, AlphaAt(25, 25), 0.01);
end;


procedure TTestSVGMaskElement.TestBlackMaskHidesTheShape;

begin
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse">'
    + '<rect width="100" height="100" fill="black"/></mask>'
    + '<rect width="50" height="50" mask="url(#m)"/>'));
  AssertEquals('a black mask hides everything', 0.0, CoveredArea, Delta);
end;


procedure TTestSVGMaskElement.TestGreyMaskDimsTheShape;

begin
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse">'
    + '<rect width="100" height="100" fill="#808080"/></mask>'
    + '<rect width="50" height="50" mask="url(#m)"/>'));
  AssertEquals('a mid grey mask halves the alpha', 0.5, AlphaAt(25, 25), 0.01);
end;


procedure TTestSVGMaskElement.TestMaskElementIsNotRenderedItself;

begin
  Draw(Doc('<mask id="m"><rect width="50" height="50" fill="white"/></mask>'));
  AssertEquals('a mask draws nothing by itself', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGMaskElement.TestMissingMaskReferenceIsIgnored;

begin
  Draw(Doc('<rect width="20" height="20" mask="url(#absent)"/>'));
  AssertEquals('an unresolved mask leaves the shape alone', 400.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGMaskElement.TestMaskAndClipBothApply;

begin
  Draw(Doc('<clipPath id="c"><rect x="0" y="0" width="25" height="100"/>'
    + '</clipPath>'
    + '<mask id="m" maskUnits="userSpaceOnUse">'
    + '<rect width="100" height="100" fill="white"/></mask>'
    + '<rect width="50" height="50" clip-path="url(#c)" mask="url(#m)"/>'));
  AssertEquals('the clip cuts the shape and the mask keeps the rest', 1250.0,
    CoveredArea, 1.0);
end;


{ TTestSVGColourSpace }

procedure TTestSVGColourSpace.TestHalfCoverBlendsInSRGBByDefault;

begin
  // White at half opacity over black is a half in whatever space the
  // sum is worked out in. Read back as sRGB that is the middle grey.
  Draw(Doc('<rect width="40" height="40" fill="black"/>'
    + '<rect width="40" height="40" fill="white" fill-opacity="0.5"/>'));
  AssertPixel(Self, 'the two mix in sRGB unless something says else',
    FSoft.Image, 20, 20, TSVGColor.FromBytes(127, 127, 127, 255));
end;


procedure TTestSVGColourSpace.TestHalfCoverBlendsInLinearRGBWhenAsked;

begin
  // Half the light of white is a good deal brighter than half its sRGB
  // value, which is the whole point of the property.
  Draw(Doc('<rect width="40" height="40" fill="black"/>'
    + '<rect width="40" height="40" fill="white" fill-opacity="0.5" '
    + 'color-interpolation="linearRGB"/>'));
  AssertPixel(Self, 'the two mix in linear light when asked to',
    FSoft.Image, 20, 20, TSVGColor.FromBytes(187, 187, 187, 255));
end;


procedure TTestSVGColourSpace.TestTheSpaceIsInheritedAndPutBack;

begin
  // The first rect is inside the group and takes its colour space. The
  // second one is outside the group and does not.
  Draw(Doc('<rect width="80" height="40" fill="black"/>'
    + '<g color-interpolation="linearRGB">'
    + '<rect width="40" height="40" fill="white" fill-opacity="0.5"/>'
    + '</g>'
    + '<rect x="40" width="40" height="40" fill="white" '
    + 'fill-opacity="0.5"/>'));
  AssertPixel(Self, 'the shape inside the group mixes in linear light',
    FSoft.Image, 20, 20, TSVGColor.FromBytes(187, 187, 187, 255));
  AssertPixel(Self, 'and the shape after it is back in sRGB',
    FSoft.Image, 60, 20, TSVGColor.FromBytes(127, 127, 127, 255));
end;


procedure TTestSVGColourSpace.TestAMaskIsWeighedInTheSpaceItAsksFor;

var
  lLinear, lPlain: Double;

begin
  // A mid grey is much darker in linear light than its sRGB value, so a
  // mask of one lets less through.
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse" x="0" y="0" '
    + 'width="40" height="40" color-interpolation="linearRGB">'
    + '<rect width="40" height="40" fill="#808080"/></mask>'
    + '<rect width="40" height="40" fill="black" mask="url(#m)"/>'));
  lLinear := AlphaAt(20, 20);
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse" x="0" y="0" '
    + 'width="40" height="40">'
    + '<rect width="40" height="40" fill="#808080"/></mask>'
    + '<rect width="40" height="40" fill="black" mask="url(#m)"/>'));
  lPlain := AlphaAt(20, 20);
  AssertEquals('a mask weighed in sRGB lets about half through', 0.5,
    lPlain, 0.02);
  AssertTrue('and one weighed in linear light lets less through',
    lLinear < lPlain - 0.1);
end;


{ TTestSVGPattern }

procedure TTestSVGPattern.TestPatternTilesAcrossTheShape;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<rect width="40" height="40" fill="url(#p)"/>'));
  AssertEquals('four tiles each a quarter covered', 400.0, CoveredArea, 0.01);
  AssertEquals('the first tile is painted', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('the gap beside it is not', 0.0, AlphaAt(15, 5), Delta);
  AssertEquals('the second tile is painted', 1.0, AlphaAt(25, 5), Delta);
end;


procedure TTestSVGPattern.TestAPatternWithoutATileTakesTheOneItNames;

begin
  Draw(Doc('<pattern id="a" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<pattern id="b" xlink:href="#a" width="20" height="20"/>'
    + '<rect width="40" height="40" fill="url(#b)"/>'));
  AssertEquals('four tiles each a quarter covered', 400.0, CoveredArea, 0.01);
  AssertEquals('the first tile is painted', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('the gap beside it is not', 0.0, AlphaAt(15, 5), Delta);
end;


procedure TTestSVGPattern.TestAPatternTakesTheAttributesItLeftOut;

begin
  // Only the href is written on b, so its size and its units come from a
  // as well as its tile.
  Draw(Doc('<pattern id="a" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<pattern id="b" xlink:href="#a"/>'
    + '<rect width="40" height="40" fill="url(#b)"/>'));
  AssertEquals('it tiles like the pattern it refers to', 400.0, CoveredArea,
    0.01);
end;


procedure TTestSVGPattern.TestAPatternKeepsATileOfItsOwn;

begin
  Draw(Doc('<pattern id="a" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<pattern id="b" xlink:href="#a" width="20" height="20">'
    + '<rect width="20" height="10" fill="black"/></pattern>'
    + '<rect width="40" height="40" fill="url(#b)"/>'));
  AssertEquals('four tiles each half covered', 800.0, CoveredArea, 0.01);
  AssertEquals('its own wider tile reaches here', 1.0, AlphaAt(15, 5),
    Delta);
end;


procedure TTestSVGPattern.TestATileIsFoundAlongTheWholeChain;

begin
  Draw(Doc('<pattern id="a" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<pattern id="b" xlink:href="#a"/>'
    + '<pattern id="c" xlink:href="#b"/>'
    + '<rect width="40" height="40" fill="url(#c)"/>'));
  AssertEquals('the tile two links away is drawn', 400.0, CoveredArea,
    0.01);
end;


procedure TTestSVGPattern.TestTextIsFilledWithAPattern;

var
  lFonts: ISVGFontProvider;

begin
  // The glyphs become the shape the pattern is clipped to. The stub face
  // draws every glyph as a full square of its advance.
  lFonts := TSVGStubFontProvider.Create;
  FRenderer.Fonts := lFonts;
  try
    Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
      + 'width="20" height="20">'
      + '<rect width="10" height="10" fill="black"/></pattern>'
      + '<text x="0" y="20" font-size="20" fill="url(#p)">ab</text>'));
    // Each glyph is a box ten wide and sixteen tall sitting on the
    // baseline at twenty, and the tile paints the top left ten by ten of
    // it.
    AssertTrue('the pattern reaches the glyphs', CoveredArea > 0);
    AssertEquals('the tile paints where it meets a glyph', 1.0,
      AlphaAt(2, 5), Delta);
    AssertEquals('and the gap of the tile stays clear', 0.0,
      AlphaAt(12, 5), Delta);
    AssertEquals('and so does the space below both', 0.0,
      AlphaAt(2, 12), Delta);
  finally
    FRenderer.Fonts := nil;
  end;
end;


procedure TTestSVGPattern.TestTextWithAnEmptyPatternTakesItsFallback;

var
  lFonts: ISVGFontProvider;

begin
  lFonts := TSVGStubFontProvider.Create;
  FRenderer.Fonts := lFonts;
  try
    Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
      + 'width="0" height="0"/>'
      + '<text x="0" y="20" font-size="20" '
      + 'fill="url(#p) #ff0000">ab</text>'));
    AssertPixel(Self, 'a pattern with no extent leaves the colour behind '
      + 'it to paint', FSoft.Image, 2, 5,
      TSVGColor.FromBytes(255, 0, 0, 255));
  finally
    FRenderer.Fonts := nil;
  end;
end;


procedure TTestSVGPattern.TestAStrokeIsDrawnWithAPattern;

begin
  // The line the pen draws is ten wide about y=5, so it covers y 0 to 10
  // across forty. The tile paints its left ten of every twenty.
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<path d="M0 5 H40" fill="none" stroke="url(#p)" '
    + 'stroke-width="10"/>'));
  AssertEquals('the tile paints where it meets the line', 1.0,
    AlphaAt(2, 2), Delta);
  AssertEquals('and again in the next tile', 1.0, AlphaAt(22, 2), Delta);
  AssertEquals('two tiles of ten by ten are covered', 200.0, CoveredArea,
    0.01);
end;


procedure TTestSVGPattern.TestAPatternStrokeStaysInsideTheLine;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<path d="M0 5 H40" fill="none" stroke="url(#p)" '
    + 'stroke-width="10"/>'));
  AssertEquals('the gap of the tile is not painted', 0.0, AlphaAt(12, 2),
    Delta);
  AssertEquals('and nothing is painted off the line', 0.0,
    AlphaAt(2, 15), Delta);
end;


procedure TTestSVGPattern.TestTextIsStrokedWithAPattern;

var
  lFonts: ISVGFontProvider;

begin
  lFonts := TSVGStubFontProvider.Create;
  FRenderer.Fonts := lFonts;
  try
    Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
      + 'width="20" height="20">'
      + '<rect width="10" height="10" fill="black"/></pattern>'
      + '<text x="0" y="20" font-size="20" fill="none" '
      + 'stroke="url(#p)" stroke-width="4">ab</text>'));
    AssertTrue('the pattern reaches the line round the glyphs',
      CoveredArea > 0);
    // The glyph box runs from four to twenty, so its top edge is drawn
    // near y=4 and the tile paints there.
    AssertEquals('and paints the top of the first glyph', 1.0,
      AlphaAt(4, 4), Delta);
  finally
    FRenderer.Fonts := nil;
  end;
end;


procedure TTestSVGPattern.TestAPatternStrokeWithNoWidthTakesItsFallback;

begin
  // A pen of no width draws no line, so there is nothing to clip the
  // tiles to, and the fallback colour behind the pattern is used instead.
  // It draws nothing either, the pen still having no width.
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<path d="M0 5 H40" fill="none" stroke="url(#p) #ff0000" '
    + 'stroke-width="0"/>'));
  AssertEquals('a pen of no width paints nothing at all', 0.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGPattern.TestPatternContentDoesNotTakeTheStrokeOfWhatItFills;

begin
  // The pattern sits where it is written and inherits from there. The
  // rect it fills is not its parent, so the stroke of that rect stays
  // with the rect and no line is drawn around every tile.
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<rect width="40" height="40" fill="url(#p)" stroke="none" '
    + 'stroke-width="4"/>'));
  // Four tiles of the 40 by 40 square, each a tenth of it filled.
  AssertEquals('only the tiles themselves are painted', 4.0 * 100,
    CoveredArea, 0.01);
  AssertEquals('a corner of a tile is painted', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('the space beside it is not', 0.0, AlphaAt(15, 5), Delta);
end;


procedure TTestSVGPattern.TestTheShapeIsStillStrokedAfterAPatternFill;

begin
  // Every tile of a pattern builds its path in the one the shape used.
  // The shape needs it back, or its own stroke is drawn from whatever
  // the last tile left there.
  // The rect runs from 10 to 50, so its four wide stroke covers 8 to 12.
  // A pixel at 9 lies under the stroke and outside the fill.
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<rect x="10" y="10" width="40" height="40" fill="url(#p)" '
    + 'stroke="black" stroke-width="4"/>'));
  AssertEquals('the border of the shape is drawn', 1.0, AlphaAt(9, 30),
    Delta);
  AssertEquals('and the far side of it too', 1.0, AlphaAt(51, 30), Delta);
  AssertEquals('nothing is drawn beyond the stroke', 0.0, AlphaAt(6, 30),
    Delta);
end;


procedure TTestSVGPattern.TestPatternIsClippedToTheShape;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="20" height="20" fill="black"/></pattern>'
    + '<rect width="30" height="30" fill="url(#p)"/>'));
  AssertEquals('the tiles stop at the edge of the shape', 900.0, CoveredArea,
    0.01);
  AssertEquals('a pixel just past the shape is empty', 0.0, AlphaAt(35, 5),
    Delta);
end;


procedure TTestSVGPattern.TestUserSpaceUnitsTileInUserSpace;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" x="0" y="0" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<rect x="20" y="0" width="20" height="20" fill="url(#p)"/>'));
  AssertEquals('the tile grid is anchored in user space, not the shape', 1.0,
    AlphaAt(25, 5), Delta);
  AssertEquals('and the empty quarter stays empty', 0.0, AlphaAt(35, 15),
    Delta);
end;


procedure TTestSVGPattern.TestPatternWithoutSizeDrawsNothing;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" width="0" '
    + 'height="20"><rect width="10" height="10"/></pattern>'
    + '<rect width="40" height="40" fill="url(#p)"/>'));
  AssertEquals('a pattern with no width paints nothing', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGPattern.TestMissingPatternKeepsTheInheritedFill;

begin
  Draw(Doc('<rect width="40" height="40" fill="url(#absent)"/>'));
  AssertEquals('an unresolved paint reference falls back to the inherited '
    + 'fill, as it does for a gradient', 1600.0, CoveredArea, 0.01);
end;


procedure TTestSVGPattern.TestPatternContentIsNotRenderedWhereItStands;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20"><rect width="20" height="20"/></pattern>'));
  AssertEquals('a pattern draws nothing by itself', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGPattern.TestPatternViewBoxScalesTheTile;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20" viewBox="0 0 10 10">'
    + '<rect width="5" height="5" fill="black"/></pattern>'
    + '<rect width="20" height="20" fill="url(#p)"/>'));
  AssertEquals('the viewBox doubles the tile content', 100.0, CoveredArea,
    0.01);
  AssertEquals('the scaled quarter is painted', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('and the rest of the tile is not', 0.0, AlphaAt(15, 15), Delta);
end;


procedure TTestSVGPattern.TestEmptyPatternFallsBackToTheColour;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" width="0" '
    + 'height="0"><rect width="10" height="10"/></pattern>'
    + '<rect width="20" height="20" fill="url(#p) black"/>'));
  AssertEquals('a pattern with no extent paints its fallback instead', 400.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGPattern.TestMissingReferenceFallsBackToTheColour;

begin
  Draw(Doc('<rect width="20" height="20" fill="url(#absent) black"/>'));
  AssertEquals('an unresolved reference paints its fallback', 400.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGPattern.TestFallbackOfNonePaintsNothing;

begin
  Draw(Doc('<rect width="20" height="20" fill="url(#absent) none"/>'));
  AssertEquals('a fallback of none paints nothing', 0.0, CoveredArea, Delta);
end;


procedure TTestSVGPattern.TestPatternWinsOverItsFallback;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="black"/></pattern>'
    + '<rect width="20" height="20" fill="url(#p) black"/>'));
  AssertEquals('a pattern that paints leaves its fallback unused', 100.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGPattern.TestFallbackAppliesToAStrokeToo;

begin
  Draw(Doc('<rect x="5" y="5" width="20" height="20" fill="none" '
    + 'stroke="url(#absent) black" stroke-width="2"/>'));
  AssertEquals('a stroke falls back the same way', 4 * 20 * 2, CoveredArea,
    1.0);
end;


procedure TTestSVGPattern.TestGradientStrokesLikeAnyPaint;

begin
  Draw(Doc('<linearGradient id="g"><stop offset="0" stop-color="black"/>'
    + '<stop offset="1" stop-color="black"/></linearGradient>'
    + '<rect x="5" y="5" width="20" height="20" fill="none" '
    + 'stroke="url(#g)" stroke-width="2"/>'));
  AssertEquals('a gradient paints a stroke, which a pattern cannot',
    4 * 20 * 2, CoveredArea, 1.0);
end;


{ TTestSVGImageElement }

procedure TTestSVGImageElement.TearDown;

begin
  FreeAndNil(FResolver);
  inherited TearDown;
end;


procedure TTestSVGImageElement.DrawWithImage(const aText: String;
  aWidth, aHeight: Integer);

begin
  FreeAndNil(FResolver);
  FResolver := TTestImageResolver.Create(aWidth, aHeight);
  FRenderer.Images := FResolver;
  Draw(aText);
end;


procedure TTestSVGImageElement.TestImageIsDrawnIntoItsRectangle;

begin
  DrawWithImage(Doc('<image xlink:href="a.png" x="10" y="10" '
    + 'width="20" height="20"/>'), 4, 4);
  AssertEquals('the image covers its rectangle', 400.0, CoveredArea, 0.01);
  AssertEquals('a pixel inside it is opaque', 1.0, AlphaAt(20, 20), Delta);
  AssertEquals('a pixel outside it is empty', 0.0, AlphaAt(5, 5), Delta);
end;


procedure TTestSVGImageElement.TestAFilterOnAnImageStillDrawsIt;

begin
  DrawWithImage(Doc('<defs><filter id="f" '
    + 'color-interpolation-filters="sRGB"><feOffset dx="0" dy="0"/>'
    + '</filter></defs><image xlink:href="a.png" x="10" y="10" '
    + 'width="20" height="20" filter="url(#f)"/>'), 4, 4);
  AssertEquals('the image is drawn through the filter', 1.0,
    AlphaAt(20, 20), Delta);
  AssertEquals('and the drawing is the image', $FFFF,
    FSoft.Image.Colors[20, 20].Red);
end;


procedure TTestSVGImageElement.TestTheFilterRegionOfAnImageIsItsRectangle;

begin
  // Twenty units square at ten, so a tenth on each side opens the region
  // at eight.
  DrawWithImage(Doc('<defs><filter id="f" '
    + 'color-interpolation-filters="sRGB"><feFlood flood-color="#00ff00"/>'
    + '</filter></defs><image xlink:href="a.png" x="10" y="10" '
    + 'width="20" height="20" filter="url(#f)"/>'), 4, 4);
  AssertEquals('the flood reaches past the image to the region', 1.0,
    AlphaAt(9, 9), Delta);
  AssertEquals('and no further', 0.0, AlphaAt(5, 5), Delta);
  AssertEquals('nor past the far side of it', 0.0, AlphaAt(35, 35), Delta);
end;


procedure TTestSVGImageElement.TestTheBoxOfAnImageIsWhatItDraws;

begin
  // A four by two image in a twenty square viewport is drawn ten tall and
  // centred, so its drawing runs from fifteen to twenty five, and the
  // region a tenth outside that runs from fourteen to twenty six. Reading
  // the region against the whole viewport would put it at eight.
  DrawWithImage(Doc('<defs><filter id="f" '
    + 'color-interpolation-filters="sRGB">'
    + '<feFlood flood-color="#00ff00"/></filter></defs>'
    + '<image xlink:href="a.png" x="10" y="10" width="20" height="20" '
    + 'filter="url(#f)"/>'), 4, 2);
  AssertEquals('the region reaches a tenth outside the drawing', 1.0,
    AlphaAt(20, 16), Delta);
  AssertEquals('and no further than that', 0.0, AlphaAt(20, 12), Delta);
end;


procedure TTestSVGImageElement.TestTheBoxOfAnImageToldToCoverIsTheViewport;

begin
  // Told to cover, the four by two image is drawn forty across and runs
  // past the viewport on both sides. The overhang is cut off, so the box
  // is the viewport, and the region is a tenth outside it.
  DrawWithImage(Doc('<defs><filter id="f" '
    + 'color-interpolation-filters="sRGB">'
    + '<feFlood flood-color="#00ff00"/></filter></defs>'
    + '<image xlink:href="a.png" x="10" y="10" width="20" height="20" '
    + 'preserveAspectRatio="xMidYMid slice" filter="url(#f)"/>'), 4, 2);
  AssertEquals('the region reaches a tenth outside the viewport', 1.0,
    AlphaAt(20, 9), Delta);
  AssertEquals('and does not follow the drawing past it', 0.0,
    AlphaAt(4, 20), Delta);
end;


procedure TTestSVGImageElement.TestAFilterImageIsFittedIntoItsBox;

begin
  // The region is twenty four square and the image four by two, so it
  // fits by its width and leaves six clear above it and six below.
  DrawWithImage(Doc('<defs><filter id="f" '
    + 'color-interpolation-filters="sRGB">'
    + '<feImage xlink:href="a.png"/></filter></defs>'
    + '<rect x="10" y="10" width="20" height="20" fill="#0000ff" '
    + 'filter="url(#f)"/>'), 4, 2);
  AssertEquals('the image covers the middle of the box', 1.0,
    AlphaAt(20, 20), Delta);
  AssertEquals('and leaves the room above it clear', 0.0,
    AlphaAt(20, 10), Delta);
end;


procedure TTestSVGImageElement.TestAFilterImageMayBeToldToStretch;

begin
  DrawWithImage(Doc('<defs><filter id="f" '
    + 'color-interpolation-filters="sRGB">'
    + '<feImage xlink:href="a.png" preserveAspectRatio="none"/>'
    + '</filter></defs>'
    + '<rect x="10" y="10" width="20" height="20" fill="#0000ff" '
    + 'filter="url(#f)"/>'), 4, 2);
  AssertEquals('told none, the image is pulled to the whole box', 1.0,
    AlphaAt(20, 10), Delta);
end;


procedure TTestSVGImageElement.TestImageAsksTheResolverForTheHRef;

begin
  DrawWithImage(Doc('<image xlink:href="picture.png" width="10" height="10"/>'),
    2, 2);
  AssertEquals('the resolver was asked once', 1, FResolver.Calls);
  AssertEquals('and was given the reference', 'picture.png',
    FResolver.LastHRef);
end;


procedure TTestSVGImageElement.TestWithoutAResolverNothingIsDrawn;

begin
  Draw(Doc('<image xlink:href="a.png" width="20" height="20"/>'));
  AssertEquals('an image without a resolver paints nothing', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGImageElement.TestZeroSizedImageIsSkipped;

begin
  DrawWithImage(Doc('<image xlink:href="a.png" width="0" height="20"/>'), 4, 4);
  AssertEquals('an image with no width paints nothing', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGImageElement.TestImagePreservesItsAspectRatio;

begin
  DrawWithImage(Doc('<image xlink:href="a.png" x="0" y="0" '
    + 'width="40" height="20"/>'), 10, 10);
  AssertEquals('a square image meets the short side of the rectangle', 400.0,
    CoveredArea, 1.0);
  AssertEquals('and is centred across the long one', 1.0, AlphaAt(20, 10),
    Delta);
  AssertEquals('leaving the near edge empty', 0.0, AlphaAt(2, 10), Delta);
end;


procedure TTestSVGImageElement.TestSliceIsClippedToTheViewport;

begin
  DrawWithImage(Doc('<image xlink:href="a.png" x="0" y="0" width="40" '
    + 'height="20" preserveAspectRatio="xMidYMid slice"/>'), 10, 10);
  AssertEquals('slice fills the rectangle and no more', 800.0, CoveredArea,
    1.0);
  AssertEquals('a pixel below the rectangle is empty', 0.0, AlphaAt(20, 25),
    Delta);
end;


{ TTestSVGExternalReferences }

procedure TTestSVGExternalReferences.TearDown;

begin
  if FRenderer <> nil then
    begin
    FRenderer.Documents := nil;
    FRenderer.StyleSheets := nil;
    end;
  FreeAndNil(FResolver);
  FreeAndNil(FSheets);
  inherited TearDown;
end;


procedure TTestSVGExternalReferences.DrawWithSheets(const aText: String);

begin
  if FSheets = nil then
    begin
    FSheets := TSVGFileStyleSheetResolver.Create(DataDir);
    FRenderer.StyleSheets := FSheets;
    end;
  DrawWithDocuments(aText);
end;


procedure TTestSVGExternalReferences.DrawWithDocuments(const aText: String);

begin
  if FResolver = nil then
    begin
    FResolver := TSVGFileDocumentResolver.Create(DataDir);
    FRenderer.Documents := FResolver;
    end;
  Draw(aText);
end;


procedure TTestSVGExternalReferences.TestUseDrawsAnElementOfAnotherFile;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#box"/>'));
  AssertEquals('the ten unit square of the other file is drawn', 100.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGExternalReferences.TestUseAppliesItsOffsetToTheOtherFile;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#box" x="20" y="20"/>'));
  AssertEquals('a pixel at the offset is painted', 1.0, AlphaAt(25, 25),
    Delta);
  AssertEquals('and the origin is left empty', 0.0, AlphaAt(5, 5), Delta);
end;


procedure TTestSVGExternalReferences.TestUseKeepsTheColourOfWhatItNames;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#box"/>'));
  AssertPixel(Self, 'the colour comes from the file it was read from',
    FSoft.Image, 5, 5, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestUseOfAWholeFileScalesItToTheUseSize;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg" '
    + 'width="20" height="20"/>'));
  AssertEquals('the forty unit viewBox is drawn into twenty units', 400.0,
    CoveredArea, 0.01);
  AssertPixel(Self, 'in the colour the other file paints with', FSoft.Image,
    10, 10, TSVGColor.FromBytes(0, 255, 0, 255));
end;


procedure TTestSVGExternalReferences.TestNestedUseInsideTheOtherFileIsResolved;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#pair"/>'));
  AssertEquals('both uses of the other file contribute a square', 200.0,
    CoveredArea, 0.01);
  AssertEquals('the second one sits at its own offset', 1.0,
    AlphaAt(25, 5), Delta);
end;


procedure TTestSVGExternalReferences.TestAnImageInsideAnImageResolvesFromItsOwnFile;

begin
  // nested/mid.svg refers to leaf.svg next to itself, one directory away
  // from the document that refers to mid.svg. Resolving it against the
  // outer file would look in the wrong place and draw nothing.
  DrawWithDocuments(Doc('<image xlink:href="nested/mid.svg" x="0" y="0" '
    + 'width="20" height="20"/>'));
  AssertEquals('the whole rectangle is drawn', 400.0, CoveredArea, 0.01);
  AssertPixel(Self, 'in the colour of the file two references away',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestTheOuterFileIsWhatFollowsResolveAgainst;

begin
  // After the nested document is drawn, the base returns to the outer
  // one, so a second image next to it still resolves as before.
  DrawWithDocuments(Doc('<image xlink:href="nested/mid.svg" x="0" y="0" '
    + 'width="10" height="10"/>'
    + '<image xlink:href="external.svg" x="10" y="10" '
    + 'width="10" height="10"/>'));
  AssertPixel(Self, 'the nested one drew', FSoft.Image, 5, 5,
    TSVGColor.FromBytes(0, 0, 255, 255));
  AssertPixel(Self, 'and the one after it drew as well', FSoft.Image,
    15, 15, TSVGColor.FromBytes(0, 255, 0, 255));
end;


procedure TTestSVGExternalReferences.TestImageDrawsAWholeDocument;

begin
  DrawWithDocuments(Doc('<image xlink:href="external.svg" x="0" y="0" '
    + 'width="20" height="20"/>'));
  AssertEquals('the document fills the rectangle of the image', 400.0,
    CoveredArea, 0.01);
  AssertPixel(Self, 'and paints in its own colour', FSoft.Image, 10, 10,
    TSVGColor.FromBytes(0, 255, 0, 255));
end;


procedure TTestSVGExternalReferences.TestImageDocumentHonoursItsAspectRatio;

begin
  DrawWithDocuments(Doc('<image xlink:href="external.svg" x="0" y="0" '
    + 'width="40" height="20"/>'));
  AssertEquals('a square document meets the short side of the rectangle',
    400.0, CoveredArea, 0.01);
  AssertEquals('and is centred across the long one', 1.0, AlphaAt(20, 10),
    Delta);
  AssertEquals('leaving the near edge empty', 0.0, AlphaAt(2, 10), Delta);
end;


procedure TTestSVGExternalReferences.TestWithoutAResolverNothingIsDrawn;

begin
  Draw(Doc('<use xlink:href="external.svg#box"/>'));
  AssertEquals('a reference to another file needs a resolver', 0.0,
    CoveredArea, Delta);
end;


procedure TTestSVGExternalReferences.TestMissingFileDrawsNothing;

begin
  DrawWithDocuments(Doc('<use xlink:href="nosuchfile.svg#box"/>'));
  AssertEquals('a file that is not there draws nothing', 0.0, CoveredArea,
    Delta);
end;


procedure TTestSVGExternalReferences.TestFragmentNamingNothingDrawsNothing;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#absent"/>'));
  AssertEquals('a fragment pointing to no element draws nothing', 0.0,
    CoveredArea, Delta);
end;


procedure TTestSVGExternalReferences.TestAFileNamedTwiceIsReadOnce;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#box"/>'
    + '<use xlink:href="external.svg#box" x="20"/>'));
  AssertEquals('both uses are drawn', 200.0, CoveredArea, 0.01);
  AssertEquals('and the file behind them was read once', 1, FResolver.Reads);
end;


procedure TTestSVGExternalReferences.TestFillNamesAGradientOfAnotherFile;

begin
  DrawWithDocuments(Doc('<rect width="20" height="20" '
    + 'fill="url(external.svg#ramp)"/>'));
  AssertPixel(Self, 'the gradient of the other file paints the shape',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(255, 0, 0, 255));
end;


procedure TTestSVGExternalReferences.TestGradientOfAnotherFileInheritsThereNotHere;

begin
  // The gradient being referred to has no stops. It takes them through an
  // href that points to an id inside its own file.
  DrawWithDocuments(Doc('<rect width="20" height="20" '
    + 'fill="url(external.svg#heir)"/>'));
  AssertPixel(Self, 'the stops came from the other file', FSoft.Image,
    10, 10, TSVGColor.FromBytes(255, 0, 0, 255));
end;


procedure TTestSVGExternalReferences.TestAUsedFillNamesTheGradientOfItsOwnFile;

begin
  DrawWithDocuments(Doc('<linearGradient id="ramp">'
    + '<stop offset="0" stop-color="#0000ff"/>'
    + '<stop offset="1" stop-color="#0000ff"/></linearGradient>'
    + '<use xlink:href="external.svg#painted"/>'));
  AssertPixel(Self, 'the ramp of the other file paints it, not this one',
    FSoft.Image, 5, 5, TSVGColor.FromBytes(255, 0, 0, 255));
end;


procedure TTestSVGExternalReferences.TestAUsedClipNamesTheClipPathOfItsOwnFile;

begin
  DrawWithDocuments(Doc('<clipPath id="hole">'
    + '<rect x="0" y="0" width="10" height="10"/></clipPath>'
    + '<use xlink:href="external.svg#clipped"/>'));
  AssertEquals('the narrow clip of the other file cuts it in half', 50.0,
    CoveredArea, 0.01);
  AssertPixelClear(Self, 'and the half beyond it stays empty', FSoft.Image,
    7, 5);
end;


procedure TTestSVGExternalReferences.TestAStyleSheetInstructionIsLoaded;

begin
  DrawWithSheets('<?xml version="1.0"?><?xml-stylesheet type="text/css" href="sheet.css" ?>'
    + Doc('<rect width="20" height="20"/>'));
  AssertPixel(Self, 'the file of the instruction sets the fill',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 255, 0, 255));
end;


procedure TTestSVGExternalReferences.TestAnAlternateStyleSheetIsLeftOut;

begin
  DrawWithSheets('<?xml version="1.0"?><?xml-stylesheet type="text/css" href="sheet.css" '
    + 'alternate="yes" ?>' + Doc('<rect width="20" height="20"/>'));
  AssertPixel(Self, 'an alternate sheet is not the one in use',
    FSoft.Image, 10, 10, TSVGColor.Black);
end;


procedure TTestSVGExternalReferences.TestAStyleSheetOfAnotherTypeIsLeftOut;

begin
  DrawWithSheets('<?xml version="1.0"?><?xml-stylesheet type="text/xsl" href="sheet.css" ?>'
    + Doc('<rect width="20" height="20"/>'));
  AssertPixel(Self, 'only a CSS sheet is loaded', FSoft.Image, 10, 10,
    TSVGColor.Black);
end;


procedure TTestSVGExternalReferences.TestWithoutAResolverNoStyleSheetIsLoaded;

begin
  Draw('<?xml version="1.0"?><?xml-stylesheet type="text/css" href="sheet.css" ?>'
    + Doc('<rect width="20" height="20"/>'));
  AssertPixel(Self, 'without a resolver the sheet is not read',
    FSoft.Image, 10, 10, TSVGColor.Black);
end;


procedure TTestSVGExternalReferences.TestAStyleElementWinsOverTheSheetBeforeIt;

begin
  DrawWithSheets('<?xml version="1.0"?><?xml-stylesheet type="text/css" href="sheet.css" ?>'
    + Doc('<style type="text/css">rect {fill:#0000ff;}</style>'
    + '<rect width="20" height="20"/>'));
  AssertPixel(Self, 'the style element comes later and takes the fill',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestAUsedElementTakesTheStyleOfItsOwnFile;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#ruled"/>'));
  AssertPixel(Self, 'the rule of the other file colours it', FSoft.Image,
    10, 10, TSVGColor.FromBytes(255, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestAStyleHereDoesNotReachAUsedElement;

begin
  DrawWithDocuments(Doc('<style type="text/css">circle {fill:#0000ff;}'
    + '</style><use xlink:href="external.svg#ruled"/>'));
  AssertPixel(Self, 'a rule written here does not match the copy',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(255, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestTheStyleOfAnotherFileStaysThere;

begin
  DrawWithDocuments(Doc('<use xlink:href="external.svg#ruled"/>'
    + '<circle cx="40" cy="10" r="8"/>'));
  AssertPixel(Self, 'the rule of the other file does not colour this one',
    FSoft.Image, 40, 10, TSVGColor.Black);
end;


procedure TTestSVGExternalReferences.TestAnImageDocumentTakesItsOwnStyle;

begin
  DrawWithSheets(Doc('<image xlink:href="styled.svg" x="0" y="0" '
    + 'width="40" height="40"/>'));
  AssertPixel(Self, 'the sheet it imports colours the rectangle',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 255, 0, 255));
  AssertPixel(Self, 'and its own style element colours the circle',
    FSoft.Image, 30, 10, TSVGColor.FromBytes(255, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestAnImportLoadsTheSheetItNames;

begin
  DrawWithSheets(Doc('<style type="text/css">@import url("sheet.css");'
    + '</style><rect width="20" height="20"/>'));
  AssertPixel(Self, 'the imported sheet sets the fill', FSoft.Image,
    10, 10, TSVGColor.FromBytes(0, 255, 0, 255));
end;


procedure TTestSVGExternalReferences.TestTheImportingSheetWinsOverWhatItImports;

begin
  // An import takes effect at the position where it is written, before
  // everything after it, so a rule of the same weight below it wins.
  DrawWithSheets(Doc('<style type="text/css">@import url("sheet.css");'
    + ' rect {fill:#0000ff;}</style><rect width="20" height="20"/>'));
  AssertPixel(Self, 'the rule after the import takes the fill',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestAnImportThatLeadsNowhereLeavesTheRest;

begin
  DrawWithSheets(Doc('<style type="text/css">@import url("nowhere.css");'
    + ' rect {fill:#0000ff;}</style><rect width="20" height="20"/>'));
  AssertPixel(Self, 'the rest of the sheet is read all the same',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestAnImportInACommentIsNotFollowed;

begin
  // The comment closes without a semicolon, so a reader that took this
  // for a rule would run on to the one below and swallow it.
  DrawWithSheets(Doc('<style type="text/css">'
    + '/* @import url("sheet.css") */ rect {fill:#0000ff;}</style>'
    + '<rect width="20" height="20"/>'));
  AssertPixel(Self, 'the rule below the comment is still read',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestAnImportedSheetMayImportAnother;

begin
  DrawWithSheets(Doc('<style type="text/css">@import url("imports.css");'
    + '</style><rect width="20" height="20"/>'
    + '<circle cx="40" cy="10" r="8"/>'));
  AssertPixel(Self, 'the sheet two imports away sets the rectangle',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 255, 0, 255));
  AssertPixel(Self, 'and the one imported directly sets the circle',
    FSoft.Image, 40, 10, TSVGColor.FromBytes(0, 0, 255, 255));
end;


procedure TTestSVGExternalReferences.TestFillNamesAPatternOfAnotherFile;

begin
  DrawWithDocuments(Doc('<rect width="40" height="40" '
    + 'fill="url(external.svg#tile)"/>'));
  AssertEquals('four tiles each a quarter covered', 400.0, CoveredArea,
    0.01);
  AssertPixel(Self, 'in the colour the other file gave the tile',
    FSoft.Image, 5, 5, TSVGColor.FromBytes(0, 255, 255, 255));
  AssertPixelClear(Self, 'and the gap beside it stays empty', FSoft.Image,
    15, 5);
end;


procedure TTestSVGExternalReferences.TestExternalPaintWithoutAResolverFallsBack;

begin
  Draw(Doc('<rect width="20" height="20" '
    + 'fill="url(external.svg#ramp) #000000"/>'));
  AssertPixel(Self, 'without a resolver the fallback paints instead',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 0, 255));
end;


procedure TTestSVGExternalReferences.TestExternalPaintNamingNothingFallsBack;

begin
  DrawWithDocuments(Doc('<rect width="20" height="20" '
    + 'fill="url(external.svg#absent) #000000"/>'));
  AssertPixel(Self, 'a fragment pointing to no server falls back too',
    FSoft.Image, 10, 10, TSVGColor.FromBytes(0, 0, 0, 255));
end;


procedure TTestSVGExternalReferences.TestAUseOfAFileNamingItselfDrawsItOnce;

begin
  // The file uses its own root, which closes a cycle on the file the
  // subtree came from, so it is drawn once and the use inside it draws
  // nothing.
  DrawWithDocuments(Doc('<use xlink:href="cycle.svg#svg-root" '
    + 'width="40" height="40"/>'));
  AssertEquals('the square of the file is drawn', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('and nothing is nested inside it', 0.0, AlphaAt(12, 12),
    Delta);
  AssertEquals('so the square is the whole of it', 100.0, CoveredArea, 0.5);
end;


procedure TTestSVGExternalReferences.TestTwoUsesOfOneFileAreBothDrawn;

begin
  // Referring twice to the same file is no cycle: the first reference is
  // finished by the time the second one is read.
  DrawWithDocuments(Doc('<use xlink:href="external.svg#box"/>'
    + '<use xlink:href="external.svg#box" x="20"/>'));
  AssertEquals('the first is drawn', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('and the second, to the same file, is drawn too', 1.0,
    AlphaAt(25, 5), Delta);
end;


// Draws with the document written into the data directory under a name of
// its own, so that a file referring to that name closes a cycle on it.
procedure TTestSVGExternalReferences.DrawAsFile(const aName,
  aText: String);

begin
  if FResolver = nil then
    begin
    FResolver := TSVGFileDocumentResolver.Create(DataDir);
    FRenderer.Documents := FResolver;
    end;
  FreeAndNil(FDocument);
  FreeAndNil(FSoft);
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGString(aText);
  FDocument.BaseURI := IncludeTrailingPathDelimiter(DataDir) + aName;
  FRenderer.Render(FDocument, FSoft);
end;


procedure TTestSVGExternalReferences.TestAnImageNamingTheDocumentBeingDrawnDrawsNothing;

begin
  // The document is known under the name its own image refers to, so the
  // image closes a cycle on the document at the head of the chain.
  DrawAsFile('imagecycle.svg',
    Doc('<image xlink:href="imagecycle.svg" x="0" y="0" width="40" '
        + 'height="40"/>'));
  AssertEquals('an image of the document being drawn draws nothing', 0.0,
    CoveredArea, 0.01);
end;


procedure TTestSVGExternalReferences.TestTwoImagesOfOneFileAreBothDrawn;

begin
  // Referring twice to the same file is no cycle: the first reference is
  // finished by the time the second one is read.
  DrawWithDocuments(Doc('<image xlink:href="external.svg" x="0" y="0" '
    + 'width="10" height="10"/>'
    + '<image xlink:href="external.svg" x="20" y="0" width="10" '
    + 'height="10"/>'));
  AssertEquals('the first is drawn', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('and the second, to the same file, is drawn too', 1.0,
    AlphaAt(25, 5), Delta);
end;


procedure TTestSVGExternalReferences.TestAnImageHasItsDocumentInADataURI;

begin
  // A data uri points to no file and contains the document itself, so it is
  // read without a resolver looking anything up.
  DrawWithDocuments(Doc('<image x="0" y="0" width="10" height="10" '
    + 'xlink:href="'
    + 'data:image/svg+xml;base64,'
    + EncodeStringBase64('<svg xmlns="http://www.w3.org/2000/svg" '
      + 'width="10" height="10" viewBox="0 0 10 10">'
      + '<rect width="10" height="10" fill="#0000ff"/></svg>')
    + '"/>'));
  AssertEquals('the square inside the uri is drawn', 100.0, CoveredArea,
    0.5);
end;


procedure TTestSVGExternalReferences.TestAFileDrawnAsAnImageKeepsItsOwnGradient;

begin
  // The file fills itself from a gradient inside itself. That gradient is
  // its own, and not one of the document that draws it.
  DrawWithDocuments(Doc('<image x="0" y="0" width="40" height="40" '
    + 'xlink:href="grad.svg"/>'));
  AssertEquals('the gradient of the file paints it red', $FFFF,
    FSoft.Image.Colors[20, 20].Red);
  AssertEquals('and nothing paints it black', 0,
    FSoft.Image.Colors[20, 20].Green);
end;


procedure TTestSVGExternalReferences.TestADocumentInAURIKeepsItsOwnGradient;

begin
  DrawWithDocuments(Doc('<image x="0" y="0" width="40" height="40" '
    + 'xlink:href="data:image/svg+xml;base64,'
    + EncodeStringBase64('<svg xmlns="http://www.w3.org/2000/svg" '
      + 'width="40" height="40" viewBox="0 0 40 40"><defs>'
      + '<linearGradient id="ramp" x1="0" y1="0" x2="1" y2="0">'
      + '<stop offset="0" stop-color="#ff0000"/>'
      + '<stop offset="1" stop-color="#ff0000"/></linearGradient></defs>'
      + '<rect width="40" height="40" fill="url(#ramp)"/></svg>')
    + '"/>'));
  AssertEquals('a gradient inside an embedded document is found too',
    $FFFF, FSoft.Image.Colors[20, 20].Red);
  AssertEquals('and nothing paints it black', 0,
    FSoft.Image.Colors[20, 20].Green);
end;


procedure TTestSVGExternalReferences.TestAnImageThatCyclesDrawsNothing;

begin
  // The file holds a ten unit square and an image of itself. The square is
  // drawn once and the image inside it closes a cycle, so it draws none.
  DrawWithDocuments(Doc('<image xlink:href="imagecycle.svg" x="0" y="0" '
    + 'width="40" height="40"/>'));
  AssertEquals('the square of the file is drawn', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('and nothing is nested inside it', 0.0, AlphaAt(12, 12),
    Delta);
  AssertEquals('so the square is the whole of it', 100.0, CoveredArea, 0.5);
end;


procedure TTestSVGExternalReferences.TestAnImageCyclingThroughAnotherFileDrawsNothing;

begin
  // The first file images the second and the second images the first, so
  // the cycle closes on a file other than the one that opened it.
  DrawWithDocuments(Doc('<image xlink:href="imagepair-a.svg" x="0" y="0" '
    + 'width="40" height="40"/>'));
  AssertEquals('the square of the first file is drawn', 1.0,
    AlphaAt(5, 5), Delta);
  AssertEquals('and the cycle through the second draws nothing', 100.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGExternalReferences.TestAnImageNamingItselfStopsToo;

begin
  // The file is imaged, and the use inside it refers to the file again, so
  // the cycle closes whichever kind of reference opened it.
  DrawWithDocuments(Doc('<image xlink:href="cycle.svg" x="0" y="0" '
    + 'width="40" height="40"/>'));
  AssertEquals('the square of the file is drawn', 1.0, AlphaAt(5, 5), Delta);
  AssertEquals('and nothing is nested inside it', 100.0, CoveredArea, 0.5);
end;


{ TTestSVGReferenceColours }

procedure TTestSVGReferenceColours.AssertColour(const aMessage: String;
  aX, aY: Integer; const aExpected: TSVGColor);

begin
  AssertPixelNear(Self, aMessage, FSoft.Image, aX, aY, aExpected, 1);
end;


procedure TTestSVGReferenceColours.TestClipKeepsTheColourOfWhatItCuts;

begin
  Draw(Doc('<clipPath id="c"><rect x="10" y="10" width="20" height="20"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" fill="#3366cc" clip-path="url(#c)"/>'));
  AssertColour('a clip cuts the shape without changing its colour', 20, 20,
    TSVGColor.FromBytes($33, $66, $CC, 255));
end;


procedure TTestSVGReferenceColours.TestUnionClipKeepsTheColourToo;

begin
  Draw(Doc('<clipPath id="c">'
    + '<rect x="10" y="10" width="20" height="20"/>'
    + '<rect x="20" y="20" width="20" height="20"/></clipPath>'
    + '<rect width="100" height="100" fill="#3366cc" clip-path="url(#c)"/>'));
  AssertColour('the layer the union is built in gives the colour back', 25,
    25, TSVGColor.FromBytes($33, $66, $CC, 255));
end;


procedure TTestSVGReferenceColours.TestClipDoesNotTintTheEdge;

begin
  Draw(Doc('<clipPath id="c"><rect x="10" y="10" width="20.5" height="20"/>'
    + '</clipPath>'
    + '<rect width="100" height="100" fill="#3366cc" clip-path="url(#c)"/>'));
  AssertColour('a half covered edge keeps the hue and halves the alpha', 30,
    20, TSVGColor.FromBytes($33, $66, $CC, 128));
end;


procedure TTestSVGReferenceColours.TestWhiteMaskLeavesTheColourAlone;

begin
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse">'
    + '<rect width="100" height="100" fill="white"/></mask>'
    + '<rect width="50" height="50" fill="#cc3366" mask="url(#m)"/>'));
  AssertColour('a white mask changes neither colour nor alpha', 25, 25,
    TSVGColor.FromBytes($CC, $33, $66, 255));
end;


procedure TTestSVGReferenceColours.TestGreyMaskDimsWithoutTinting;

begin
  Draw(Doc('<mask id="m" maskUnits="userSpaceOnUse">'
    + '<rect width="100" height="100" fill="#808080"/></mask>'
    + '<rect width="50" height="50" fill="#cc3366" mask="url(#m)"/>'));
  AssertColour('a grey mask takes the alpha and leaves the channels', 25, 25,
    TSVGColor.FromBytes($CC, $33, $66, 128));
end;


procedure TTestSVGReferenceColours.TestPatternTilesKeepTheirOwnColour;

begin
  Draw(Doc('<pattern id="p" patternUnits="userSpaceOnUse" '
    + 'width="20" height="20">'
    + '<rect width="10" height="10" fill="#22aa44"/></pattern>'
    + '<rect width="40" height="40" fill="url(#p)"/>'));
  AssertColour('the tile paints in its own colour', 5, 5,
    TSVGColor.FromBytes($22, $AA, $44, 255));
  AssertPixelClear(Self, 'and the gap beside it stays empty', FSoft.Image,
    15, 5);
end;


procedure TTestSVGReferenceColours.TestPatternFallbackPaintsItsColour;

begin
  Draw(Doc('<rect width="20" height="20" fill="url(#absent) #22aa44"/>'));
  AssertColour('the fallback is the colour that reaches the surface', 10, 10,
    TSVGColor.FromBytes($22, $AA, $44, 255));
end;


procedure TTestSVGReferenceColours.TestImageKeepsThePixelsItWasGiven;

var
  lResolver: TTestImageResolver;

begin
  lResolver := TTestImageResolver.Create(4, 4);
  try
    FRenderer.Images := lResolver;
    Draw(Doc('<image xlink:href="a.png" x="0" y="0" '
      + 'width="20" height="20"/>'));
    AssertColour('the image arrives in the colour it holds', 10, 10,
      TSVGColor.FromBytes(255, 0, 0, 255));
  finally
    FRenderer.Images := nil;
    lResolver.Free;
  end;
end;


procedure TTestSVGReferenceColours.TestTranslucentImageKeepsItsHue;

var
  lResolver: TTestImageResolver;

begin
  lResolver := TTestImageResolver.Create(4, 4);
  try
    FRenderer.Images := lResolver;
    Draw(Doc('<image xlink:href="a.png" x="0" y="0" width="20" '
      + 'height="20" opacity="0.5"/>'));
    AssertColour('half opacity halves the alpha and keeps the red', 10, 10,
      TSVGColor.FromBytes(255, 0, 0, 128));
  finally
    FRenderer.Images := nil;
    lResolver.Free;
  end;
end;


{ TTestSVGReferenceGoldens }

function TTestSVGReferenceGoldens.AlphaText: TStringList;

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


procedure TTestSVGReferenceGoldens.TestReferenceWalkGolden;

begin
  FTrace := TSVGTraceBackend.Create;
  FDocument := ReadSVGFile(DataDir + 'refs.svg');
  FRenderer.Render(FDocument, FTrace);
  AssertGolden(Self, 'refs-walk', FTrace.Log);
end;


procedure TTestSVGReferenceGoldens.TestReferenceCoverageGolden;

var
  lLines, lBlock: TStringList;

begin
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGFile(DataDir + 'refs.svg');
  FRenderer.Render(FDocument, FSoft);
  lLines := TStringList.Create;
  try
    lLines.Add('# refs.svg at 60 by 40');
    lBlock := AlphaText;
    try
      lLines.AddStrings(lBlock);
    finally
      lBlock.Free;
    end;
    AssertGolden(Self, 'refs-coverage', lLines);
  finally
    lLines.Free;
  end;
end;


{ TTestSVGImageResolving }

procedure TTestSVGImageResolving.SetUp;

begin
  inherited SetUp;
  FResolver := TSVGFileImageResolver.Create(DataDir);
end;


procedure TTestSVGImageResolving.TearDown;

begin
  FreeAndNil(FResolver);
  inherited TearDown;
end;


procedure TTestSVGImageResolving.TestMissingFileResolvesToNothing;

begin
  AssertTrue('a file that is not there resolves to nothing',
    FResolver.ResolveImage('nosuchfile.png', '') = nil);
end;


procedure TTestSVGImageResolving.TestEmptyReferenceResolvesToNothing;

begin
  AssertTrue('an empty reference resolves to nothing',
    FResolver.ResolveImage('', '') = nil);
end;


procedure TTestSVGImageResolving.TestDataURIWithoutBase64IsRefused;

begin
  AssertTrue('a data URI that is not base64 resolves to nothing',
    FResolver.ResolveImage('data:image/png,notencoded', '') = nil);
end;


procedure TTestSVGImageResolving.TestUnreadableDataIsRefused;

begin
  AssertTrue('data that decodes to no known format resolves to nothing',
    FResolver.ResolveImage('data:image/png;base64,bm90YW5pbWFnZQ==', '')
      = nil);
end;


{ TTestSVGFontFileResolving }

{ TTestSVGStyleSheetResolving }

procedure TTestSVGStyleSheetResolving.SetUp;

begin
  inherited SetUp;
  FResolver := TSVGFileStyleSheetResolver.Create(DataDir);
  FFileName := '';
end;


procedure TTestSVGStyleSheetResolving.TearDown;

begin
  FreeAndNil(FResolver);
  if (FFileName <> '') and FileExists(FFileName) then
    DeleteFile(FFileName);
  inherited TearDown;
end;


function TTestSVGStyleSheetResolving.WriteSheet(
  const aBytes: RawByteString): String;

var
  lFile: TFileStream;

begin
  Result := IncludeTrailingPathDelimiter(DataDir) + 'bytes.css';
  FFileName := Result;
  lFile := TFileStream.Create(Result, fmCreate);
  try
    if aBytes <> '' then
      lFile.WriteBuffer(aBytes[1], Length(aBytes));
  finally
    lFile.Free;
  end;
  Result := ExtractFileName(Result);
end;


procedure TTestSVGStyleSheetResolving.TestASheetIsReadByteForByte;

var
  lSheet: RawByteString;

begin
  // A family name and a content string, both written in UTF-8.
  lSheet := 'text { font-family: "Zalam' + #$C3#$A4 + 'nder"; }'
    + LineEnding + '.euro:before { content: "' + #$E2#$82#$AC + '"; }';
  AssertEquals('the sheet comes back as it was written', lSheet,
    FResolver.ResolveStyleSheet(WriteSheet(lSheet), ''));
  AssertEquals('and it was read once', 1, FResolver.Reads);
end;


procedure TTestSVGStyleSheetResolving.TestTheLineEndingsOfASheetAreKept;

var
  lSheet: RawByteString;

begin
  // Neither ending is rewritten, and no ending is added at the end.
  lSheet := 'a { fill: red; }'#13#10'b { fill: blue; }'#10'c { fill: aqua; }';
  AssertEquals('the endings are the ones of the file', lSheet,
    FResolver.ResolveStyleSheet(WriteSheet(lSheet), ''));
end;


procedure TTestSVGStyleSheetResolving.TestAnAbsentSheetIsEmpty;

begin
  AssertEquals('a sheet that is not there reads as nothing', '',
    FResolver.ResolveStyleSheet('nosuchsheet.css', ''));
  AssertEquals('and nothing was read', 0, FResolver.Reads);
end;


procedure TTestSVGFontFileResolving.SetUp;

begin
  inherited SetUp;
  FResolver := TSVGFileFontResolver.Create(DataDir);
  FAsked := '';
  FSupply := '';
end;


procedure TTestSVGFontFileResolving.TearDown;

begin
  FreeAndNil(FResolver);
  inherited TearDown;
end;


procedure TTestSVGFontFileResolving.SupplyFont(aSender: TObject;
  const aURL, aBaseURI: String; var aFileName: String);

begin
  FAsked := aURL;
  aFileName := FSupply;
end;


procedure TTestSVGFontFileResolving.TestAFileBesideTheDocumentIsFound;

begin
  AssertTrue('a file that is there resolves to its full path',
    FResolver.ResolveFontFile('basic.svg', '') <> '');
  AssertTrue('and the path leads to it',
    FileExists(FResolver.ResolveFontFile('basic.svg', '')));
end;


procedure TTestSVGFontFileResolving.TestAnAbsentFileResolvesToNothing;

begin
  AssertEquals('a file that is nowhere resolves to nothing', '',
    FResolver.ResolveFontFile('nothing-is-named-this.woff', ''));
end;


procedure TTestSVGFontFileResolving.TestTheSearchPathIsTriedNext;

begin
  FResolver.BasePath := DataDir + 'nowhere';
  AssertEquals('the url on its own leads nowhere', '',
    FResolver.ResolveFontFile('fonts/basic.svg', ''));
  FResolver.SearchPath := DataDir;
  AssertTrue('but the search path holds the file of the url',
    FResolver.ResolveFontFile('fonts/basic.svg', '') <> '');
end;


procedure TTestSVGFontFileResolving.TestTheEventIsAskedWhenNothingIsFound;

begin
  FResolver.OnFontFileNeeded := @SupplyFont;
  FSupply := DataDir + 'basic.svg';
  AssertEquals('the file the handler supplies is taken', FSupply,
    FResolver.ResolveFontFile('absent.woff', ''));
  AssertEquals('and it was told which url wanted a file', 'absent.woff',
    FAsked);
end;


procedure TTestSVGFontFileResolving.TestTheEventIsNotAskedWhenTheFileIsThere;

begin
  FResolver.OnFontFileNeeded := @SupplyFont;
  FResolver.ResolveFontFile('basic.svg', '');
  AssertEquals('a url that leads somewhere never reaches the handler', '',
    FAsked);
end;


procedure TTestSVGFontFileResolving.TestAnEventNamingNothingReadableIsRefused;

begin
  FResolver.OnFontFileNeeded := @SupplyFont;
  FSupply := DataDir + 'no-such-file.woff';
  AssertEquals('a handler supplying a file that is not there settles nothing',
    '', FResolver.ResolveFontFile('absent.woff', ''));
end;


{ TTestSVGFilterChain }

// A document whose one rect is drawn through the filter the caller writes.
function FilterDoc(const aFilter: String): String;

begin
  Result := Doc('<defs><filter id="f">' + aFilter + '</filter></defs>'
    + '<rect x="10" y="10" width="20" height="20" filter="url(#f)"/>');
end;


procedure TTestSVGFilterChain.TestAFilterOpensALayerAndCloses;

begin
  Trace(FilterDoc('<feFlood/>'));
  AssertEquals('the element draws into a layer of its own', 1,
    CountLines('push-layer'));
  AssertEquals('and the layer is closed through the chain', 1,
    CountLines('pop-layer-as-filter'));
end;


procedure TTestSVGFilterChain.TestTheRegionIsATenthOutsideTheBox;

begin
  // Twenty units square at ten, so a tenth on each side opens it at eight
  // and makes it twenty four.
  Trace(FilterDoc('<feFlood/>'));
  AssertEquals('the region reaches a tenth outside the box',
    'pop-layer-as-filter region=[8 8 32 32] linear=True count=1',
    FirstLine('pop-layer-as-filter'));
end;


procedure TTestSVGFilterChain.TestTheRegionMayBeGivenInUserSpace;

begin
  Trace(Doc('<defs><filter id="f" filterUnits="userSpaceOnUse" x="0" y="0"'
    + ' width="50" height="40"><feFlood/></filter></defs>'
    + '<rect x="10" y="10" width="20" height="20" filter="url(#f)"/>'));
  AssertEquals('the region is the one the filter gives',
    'pop-layer-as-filter region=[0 0 50 40] linear=True count=1',
    FirstLine('pop-layer-as-filter'));
end;


procedure TTestSVGFilterChain.TestAPrimitiveReadsTheOneBeforeIt;

begin
  Trace(FilterDoc('<feFlood/><feGaussianBlur stdDeviation="2"/>'));
  AssertEquals('a primitive without an input reads the one before it',
    '1 blur in=0 2 2', Trim(FirstLine('1 blur')));
end;


procedure TTestSVGFilterChain.TestAPrimitiveReadsAResultByName;

begin
  Trace(FilterDoc('<feFlood result="paint"/>'
    + '<feGaussianBlur stdDeviation="1"/>'
    + '<feOffset in="paint" dx="3" dy="4"/>'));
  AssertEquals('a result stored under a name is read back by that name',
    '2 offset in=0 3 4', Trim(FirstLine('2 offset')));
end;


procedure TTestSVGFilterChain.TestTheFirstPrimitiveReadsTheSource;

begin
  Trace(FilterDoc('<feGaussianBlur stdDeviation="1"/>'));
  AssertEquals('the first primitive without an input reads the drawing',
    '0 blur in=source 1 1', Trim(FirstLine('0 blur')));
end;


procedure TTestSVGFilterChain.TestASourceIsNamedByItsOwnName;

begin
  Trace(FilterDoc('<feGaussianBlur in="SourceAlpha" stdDeviation="1"/>'));
  AssertEquals('the alpha of the drawing is a source of its own',
    '0 blur in=source-alpha 1 1', Trim(FirstLine('0 blur')));
end;


procedure TTestSVGFilterChain.TestABlurHasItsTwoDeviations;

begin
  Trace(FilterDoc('<feGaussianBlur stdDeviation="2 5"/>'));
  AssertEquals('a blur given two deviations keeps both',
    '0 blur in=source 2 5', Trim(FirstLine('0 blur')));
end;


procedure TTestSVGFilterChain.TestAMergeReadsEveryNodeItHolds;

begin
  Trace(FilterDoc('<feFlood result="a"/><feOffset result="b"/>'
    + '<feMerge><feMergeNode in="a"/><feMergeNode in="b"/></feMerge>'));
  AssertEquals('a merge reads the input of each of its nodes',
    '2 merge in=0,1', Trim(FirstLine('2 merge')));
end;


procedure TTestSVGFilterChain.TestAFilterHoldingNothingDrawsNothing;

begin
  // A filter of no primitives leaves the element unfiltered rather than
  // opening a layer for nothing.
  Trace(FilterDoc(''));
  AssertEquals('no layer is opened for a filter of nothing', 0,
    CountLines('pop-layer-as-filter'));
end;


procedure TTestSVGFilterChain.TestAFilterNamingNothingDrawsNothing;

begin
  // SVG 1.1 does not draw an element whose filter refers to something that
  // is not a filter. SVG 2 draws it unfiltered instead; this package is
  // 1.1.
  Trace(Doc('<rect x="10" y="10" width="20" height="20" '
    + 'filter="url(#gone)"/>'));
  AssertEquals('a filter that is not there filters nothing', 0,
    CountLines('pop-layer-as-filter'));
  AssertEquals('and the element it was asked of is not drawn', 0,
    CountLines('fill-path'));
end;


procedure TTestSVGFilterChain.TestAFilterOfNoneLeavesTheElementAlone;

begin
  Trace(Doc('<rect x="10" y="10" width="20" height="20" filter="none"/>'));
  AssertEquals('nothing is filtered', 0,
    CountLines('pop-layer-as-filter'));
  AssertEquals('and the element is drawn unfiltered', 1,
    CountLines('fill-path'));
end;


{ TTestSVGFilterDrawing }

// A blue ten unit square at ten, drawn through the filter the caller
// writes. Nothing else is on the surface.
function DrawnDoc(const aFilter: String): String;

begin
  Result := Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + aFilter + '</filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>');
end;


procedure TTestSVGFilterDrawing.TestAFloodFillsTheRegion;

begin
  // The region reaches a tenth outside the square, so it starts at nine
  // and is twelve across.
  Draw(DrawnDoc('<feFlood flood-color="#ff0000"/>'));
  AssertEquals('the flood covers the region', 1.0, AlphaAt(15, 15), 0.01);
  // The square itself runs from ten to twenty, so nine is inside the
  // region and outside the shape.
  AssertEquals('and reaches beyond the shape to the corner of it', 1.0,
    AlphaAt(9, 9), 0.01);
  AssertEquals('but no further', 0.0, AlphaAt(5, 5), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAnOffsetMovesWhatWasDrawn;

begin
  Draw(DrawnDoc('<feOffset dx="2" dy="0"/>'));
  AssertEquals('the drawing has moved two further along', 1.0,
    AlphaAt(15, 15), 0.01);
  AssertEquals('and nothing is left at the old position', 0.0, AlphaAt(11, 15),
    0.01);
end;


procedure TTestSVGFilterDrawing.TestABlurSpreadsBeyondTheShape;

begin
  Draw(DrawnDoc('<feGaussianBlur stdDeviation="1"/>'));
  AssertTrue('the blur reaches outside the square the shape covered',
    AlphaAt(9, 15) > 0.02);
  AssertTrue('and softens the middle of an edge',
    (AlphaAt(10, 15) > 0.1) and (AlphaAt(10, 15) < 0.95));
end;


procedure TTestSVGFilterDrawing.TestABlurOfNothingLeavesTheShapeAlone;

begin
  Draw(DrawnDoc('<feGaussianBlur stdDeviation="0"/>'));
  AssertEquals('a blur of no deviation is the shape itself', 1.0,
    AlphaAt(15, 15), 0.01);
  AssertEquals('with the edge it always had', 0.0, AlphaAt(9, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAMergeLaysOneOverTheOther;

begin
  Draw(DrawnDoc('<feFlood flood-color="#ff0000" result="ground"/>'
    + '<feMerge><feMergeNode in="ground"/>'
    + '<feMergeNode in="SourceGraphic"/></feMerge>'));
  AssertEquals('the square is laid over the flood and is blue', 0,
    FSoft.Image.Colors[15, 15].Red);
  AssertEquals('and is blue', $FFFF, FSoft.Image.Colors[15, 15].Blue);
  AssertEquals('the flood shows outside the square', $FFFF,
    FSoft.Image.Colors[9, 9].Red);
end;


procedure TTestSVGFilterDrawing.TestACompositeInKeepsTheOverlap;

begin
  Draw(DrawnDoc('<feFlood flood-color="#ff0000" result="paint"/>'
    + '<feComposite in="paint" in2="SourceGraphic" operator="in"/>'));
  AssertEquals('the flood is kept only under the square', 1.0,
    AlphaAt(15, 15), 0.01);
  AssertEquals('and dropped everywhere else', 0.0, AlphaAt(9, 9), 0.01);
  AssertEquals('the flood is kept, not the square', $FFFF,
    FSoft.Image.Colors[15, 15].Red);
end;


procedure TTestSVGFilterDrawing.TestAColourMatrixTurnsTheChannels;

begin
  // The matrix moves blue into red and leaves the alpha unchanged.
  Draw(DrawnDoc('<feColorMatrix type="matrix" values="'
    + '0 0 1 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0"/>'));
  AssertEquals('the blue of the square is read as red', $FFFF,
    FSoft.Image.Colors[15, 15].Red);
  AssertEquals('and no blue is left', 0, FSoft.Image.Colors[15, 15].Blue);
end;


procedure TTestSVGFilterDrawing.TestAnAlphaSourceKeepsOnlyTheShape;

begin
  Draw(DrawnDoc('<feColorMatrix in="SourceAlpha" type="matrix" values="'
    + '1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 1 0"/>'));
  AssertEquals('the alpha of the drawing is black', 0,
    FSoft.Image.Colors[15, 15].Blue);
  AssertEquals('and covers the area of the shape', 1.0, AlphaAt(15, 15),
    0.01);
end;


procedure TTestSVGFilterDrawing.TestABlendMultipliesTheTwo;

begin
  // Blue multiplied by red leaves nothing of either.
  Draw(DrawnDoc('<feFlood flood-color="#ff0000" result="paint"/>'
    + '<feBlend in="paint" in2="SourceGraphic" mode="multiply"/>'));
  AssertEquals('red multiplied by blue keeps no red', 0,
    FSoft.Image.Colors[15, 15].Red);
  AssertEquals('and no blue', 0, FSoft.Image.Colors[15, 15].Blue);
end;


procedure TTestSVGFilterDrawing.TestADilateThickensTheShape;

begin
  Draw(DrawnDoc('<feMorphology operator="dilate" radius="2"/>'));
  AssertEquals('the shape reaches two further out', 1.0, AlphaAt(9, 15),
    0.01);
  AssertEquals('and no further than that', 0.0, AlphaAt(6, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAnErodeThinsIt;

begin
  Draw(DrawnDoc('<feMorphology operator="erode" radius="2"/>'));
  AssertEquals('the edge of the shape is eaten away', 0.0, AlphaAt(11, 15),
    0.01);
  AssertEquals('and the middle of it is left', 1.0, AlphaAt(15, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestATransferTableTurnsAChannel;

begin
  // The table takes the blue of the square down to nothing and brings the
  // red up to the whole of it.
  Draw(DrawnDoc('<feComponentTransfer>'
    + '<feFuncR type="table" tableValues="1 1"/>'
    + '<feFuncB type="table" tableValues="0 0"/></feComponentTransfer>'));
  AssertEquals('red is brought up by the table it was given', $FFFF,
    FSoft.Image.Colors[15, 15].Red);
  AssertEquals('and blue taken down by its own', 0,
    FSoft.Image.Colors[15, 15].Blue);
end;


procedure TTestSVGFilterDrawing.TestATransferOfNothingLeavesItAlone;

begin
  Draw(DrawnDoc('<feComponentTransfer><feFuncR type="identity"/>'
    + '</feComponentTransfer>'));
  AssertEquals('a channel through the identity is unchanged', $FFFF,
    FSoft.Image.Colors[15, 15].Blue);
  AssertEquals('and the shape has not moved', 1.0, AlphaAt(15, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAConvolutionOfOneLeavesTheShape;

begin
  Draw(DrawnDoc('<feConvolveMatrix order="1" kernelMatrix="1"/>'));
  AssertEquals('a grid of one weight leaves its input unchanged', 1.0,
    AlphaAt(15, 15), 0.01);
  AssertEquals('and reaches no further', 0.0, AlphaAt(9, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAConvolutionSpreadsWhatItWeighs;

begin
  // Every cell of three by three weighed alike is a blur of a box, which
  // spreads the shape one pixel out on each side.
  Draw(DrawnDoc('<feConvolveMatrix order="3" '
    + 'kernelMatrix="1 1 1 1 1 1 1 1 1"/>'));
  AssertTrue('the shape reaches one further out', AlphaAt(9, 15) > 0.1);
  AssertEquals('and no further than one', 0.0, AlphaAt(8, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestADisplacementOfNothingMovesNothing;

begin
  Draw(DrawnDoc('<feDisplacementMap in2="SourceGraphic" scale="0"/>'));
  AssertEquals('a displacement of no scale leaves its input unchanged', 1.0,
    AlphaAt(15, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestTurbulenceFillsTheRegionWithNoise;

var
  lFirst, lSecond: Word;

begin
  // The noise is nothing at all on a lattice point, which with this
  // frequency is every fifth unit, so it is read between them.
  Draw(DrawnDoc('<feTurbulence baseFrequency="0.2" numOctaves="2"/>'));
  lFirst := FSoft.Image.Colors[12, 12].Red;
  lSecond := FSoft.Image.Colors[17, 17].Red;
  AssertTrue('the noise covers the region', AlphaAt(13, 13) > 0);
  AssertTrue('and is not the one value everywhere', lFirst <> lSecond);
end;


procedure TTestSVGFilterDrawing.TestTurbulenceIsTheSameForTheSameSeed;

var
  lValue: Word;

begin
  Draw(DrawnDoc('<feTurbulence baseFrequency="0.2" seed="3"/>'));
  lValue := FSoft.Image.Colors[13, 13].Red;
  Draw(DrawnDoc('<feTurbulence baseFrequency="0.2" seed="3"/>'));
  AssertEquals('the one seed gives the one noise', lValue,
    FSoft.Image.Colors[13, 13].Red);
  Draw(DrawnDoc('<feTurbulence baseFrequency="0.2" seed="9"/>'));
  AssertTrue('and another seed gives another',
    lValue <> FSoft.Image.Colors[13, 13].Red);
end;


procedure TTestSVGFilterDrawing.TestALightLightsTheShape;

begin
  // A light straight overhead on a surface of no relief lights the whole
  // of it evenly.
  Draw(DrawnDoc('<feDiffuseLighting surfaceScale="0" lighting-color="#ff0000">'
    + '<feDistantLight azimuth="0" elevation="90"/></feDiffuseLighting>'));
  AssertEquals('the light is the colour it was given', $FFFF,
    FSoft.Image.Colors[15, 15].Red);
  AssertEquals('and nothing of another', 0,
    FSoft.Image.Colors[15, 15].Blue);
end;


procedure TTestSVGFilterDrawing.TestAConvolutionIsReadBackwards;

begin
  // The grid is laid over the pixel backwards, as a convolution does and
  // a correlation does not. The single weight sits to the right of the
  // middle, so the shape moves one pixel to the right.
  Draw(DrawnDoc('<feConvolveMatrix order="3" '
    + 'kernelMatrix="0 0 0 0 0 1 0 0 0"/>'));
  AssertEquals('the shape moves one to the right', 1.0,
    AlphaAt(20, 15), 0.01);
  AssertEquals('and is gone from its old position', 0.0, AlphaAt(10, 15),
    0.01);
end;


procedure TTestSVGFilterDrawing.TestALightFollowsTheReliefOfTheSurface;

var
  lMiddle, lEdge: Word;

begin
  // On a surface with relief the normal tilts where the alpha changes, so
  // a light from one side falls differently at an edge than in the
  // middle.
  Draw(DrawnDoc('<feDiffuseLighting surfaceScale="10" '
    + 'lighting-color="#ffffff">'
    + '<feDistantLight azimuth="0" elevation="45"/></feDiffuseLighting>'));
  lMiddle := FSoft.Image.Colors[15, 15].Red;
  lEdge := FSoft.Image.Colors[10, 15].Red;
  AssertTrue('the middle of a flat face takes the light evenly',
    lMiddle > 0);
  AssertTrue('and an edge, where the surface tilts, takes it differently',
    lEdge <> lMiddle);
end;


procedure TTestSVGFilterDrawing.TestTheNoiseIsWhatTheAppendixGives;

begin
  // The generator is the one written out in the SVG specification, down to
  // the shuffling of its lattice. These are the values it produces. A
  // generator that produces anything else is not the one the reference
  // images were drawn with.
  Draw(DrawnDoc('<feTurbulence baseFrequency="0.2" numOctaves="2" '
    + 'seed="3"/>'));
  AssertEquals('the red of the noise at thirteen', 21419,
    FSoft.Image.Colors[13, 13].Red);
  AssertEquals('and its green', 19150, FSoft.Image.Colors[13, 13].Green);
  AssertEquals('and the alpha it has', 8695,
    FSoft.Image.Colors[13, 13].Alpha);
end;


procedure TTestSVGFilterDrawing.TestAnImageMayNameAnElementOfTheDocument;

begin
  // An feImage that refers to an element draws that element, and not the
  // element the filter is applied to. The green square is drawn at the
  // position of the blue one.
  Draw(Doc('<defs><rect id="green" x="10" y="10" width="10" height="10" '
    + 'fill="#00ff00"/>'
    + '<filter id="f" color-interpolation-filters="sRGB">'
    + '<feImage xlink:href="#green"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the element the image points to is the one drawn', $FFFF,
    FSoft.Image.Colors[15, 15].Green);
  AssertEquals('and not the element the filter is applied to', 0,
    FSoft.Image.Colors[15, 15].Blue);
  AssertEquals('it covers the area of that element', 1.0,
    AlphaAt(15, 15), 0.01);
end;


procedure TTestSVGFilterDrawing.TestABiasReachesTheColourAndNotTheAlpha;

begin
  // The kernel adds up to one, so it leaves a flat half clear square as
  // it found it and only the bias tells on the result.
  Draw(Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feConvolveMatrix order="3" kernelMatrix="1 1 1 1 -7 1 1 1 1" '
    + 'preserveAlpha="false" bias="1"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'fill-opacity="0.5" filter="url(#f)"/>'));
  AssertEquals('a bias leaves a half clear pixel half clear', 0.5,
    AlphaAt(15, 15), 0.02);
  AssertEquals('and raises the colour it did not reach before', $FFFF,
    FSoft.Image.Colors[15, 15].Red);
end;


procedure TTestSVGFilterDrawing.TestTheOpacityIsLaidOnTheResultNotOnTheSource;

begin
  // Multiplying an opaque blue by a half clear green leaves the blue
  // alone and opaque, and the opacity then halves it. Read at the
  // opacity of the shape the blend would let a quarter of the green
  // through and leave the result three quarters covering.
  Draw(Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feFlood flood-color="#00ff00" flood-opacity="0.5" result="a"/>'
    + '<feBlend in="SourceGraphic" in2="a" mode="multiply"/>'
    + '</filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'opacity="0.5" filter="url(#f)"/>'));
  AssertEquals('the opacity covers the result of the chain', 0.5,
    AlphaAt(15, 15), 0.02);
  AssertEquals('and the chain read a shape that was not clear', 0,
    FSoft.Image.Colors[15, 15].Green);
end;


function PlaneDoc(const aIn, aShape: String): String;

begin
  Result := Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feOffset in="' + aIn + '" dx="0" dy="0"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" ' + aShape
    + ' filter="url(#f)"/>');
end;


// A backdrop bar across the top, and a filter that fetches the background
// down into the room below it.
function BackdropDoc(const aIn, aGroup: String): String;

begin
  Result := Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feOffset in="' + aIn + '" dx="0" dy="6"/></filter></defs>'
    + '<g ' + aGroup + '>'
    + '<rect x="0" y="0" width="40" height="12" fill="#00ff00"/>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/></g>');
end;


procedure TTestSVGFilterDrawing.TestFillPaintIsAPlaneOfTheFill;

begin
  Draw(PlaneDoc('FillPaint', 'fill="#ff0000"'));
  AssertEquals('the plane has the fill', $FFFF,
    FSoft.Image.Colors[15, 15].Red);
  AssertEquals('and reaches the whole region, not the shape', 1.0,
    AlphaAt(9, 9), Delta);
  AssertEquals('but no further', 0.0, AlphaAt(5, 5), Delta);
end;


procedure TTestSVGFilterDrawing.TestStrokePaintIsAPlaneOfTheStroke;

begin
  Draw(PlaneDoc('StrokePaint', 'fill="#ff0000" stroke="#0000ff"'));
  AssertEquals('the plane has the stroke', $FFFF,
    FSoft.Image.Colors[15, 15].Blue);
  AssertEquals('and none of the fill', 0,
    FSoft.Image.Colors[15, 15].Red);
end;


procedure TTestSVGFilterDrawing.TestAPlaneOfAGradientIsReadAgainstTheBoxOfTheElement;

begin
  // The region runs from ten to thirty and the square only from ten to
  // twenty. The step in the middle of the gradient therefore falls at
  // fifteen when it is read against the box of the element, and at twenty
  // when it is read against the whole plane.
  Draw(Doc('<defs><linearGradient id="g" x1="0" y1="0" x2="1" y2="0">'
    + '<stop offset="0.5" stop-color="#ff0000"/>'
    + '<stop offset="0.5" stop-color="#0000ff"/></linearGradient>'
    + '<filter id="f" color-interpolation-filters="sRGB" x="0%" y="0%" '
    + 'width="200%" height="100%">'
    + '<feOffset in="FillPaint" dx="0" dy="0"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="url(#g)" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the plane has the near half of the gradient', $FFFF,
    FSoft.Image.Colors[12, 15].Red);
  AssertEquals('the step falls at the position the box of the element gives', $FFFF,
    FSoft.Image.Colors[17, 15].Blue);
  AssertEquals('and nothing of the near half is left there', 0,
    FSoft.Image.Colors[17, 15].Red);
  AssertEquals('the plane reaches past the box, over the whole region',
    1.0, AlphaAt(25, 15), Delta);
end;


procedure TTestSVGFilterDrawing.TestAPlaneOfAPatternIsEmpty;

begin
  // A pattern has an extent of its own, so no plane is made of it.
  Draw(Doc('<defs><pattern id="p" width="4" height="4" '
    + 'patternUnits="userSpaceOnUse">'
    + '<rect width="4" height="4" fill="#ff0000"/></pattern>'
    + '<filter id="f" color-interpolation-filters="sRGB">'
    + '<feOffset in="FillPaint" dx="0" dy="0"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="url(#p)" '
    + 'filter="url(#f)"/>'));
  AssertEquals('nothing is drawn for it', 0.0, AlphaAt(15, 15), Delta);
end;


procedure TTestSVGFilterDrawing.TestAPlaneHasTheOpacityOfThePaint;

begin
  Draw(PlaneDoc('FillPaint', 'fill="#ff0000" fill-opacity="0.5"'));
  AssertEquals('a plane of a colour is as clear as the paint is', 0.5,
    AlphaAt(15, 15), 0.02);
  Draw(Doc('<defs><linearGradient id="g" x1="0" y1="0" x2="1" y2="0">'
    + '<stop offset="0" stop-color="#ff0000"/>'
    + '<stop offset="1" stop-color="#0000ff"/></linearGradient>'
    + '<filter id="f" color-interpolation-filters="sRGB">'
    + '<feOffset in="FillPaint" dx="0" dy="0"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="url(#g)" '
    + 'fill-opacity="0.5" filter="url(#f)"/>'));
  AssertEquals('and so is a plane of a gradient', 0.5,
    AlphaAt(15, 15), 0.02);
end;


procedure TTestSVGFilterDrawing.TestAPlaneIsTurnedIntoTheLightTheChainWorksIn;

begin
  // The chain works on light unless it is told otherwise, and a grey that
  // went in as a plane has to come back out the grey it went in as.
  Draw(Doc('<defs><filter id="f">'
    + '<feOffset in="FillPaint" dx="0" dy="0"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#808080" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the grey comes back the grey it went in as', 0.502,
    FSoft.Image.Colors[15, 15].Red / 65535, 0.01);
end;


procedure TTestSVGFilterDrawing.TestABlurOfAnEvenWidthKeepsToItsOwnSpread;

begin
  // A deviation of one gives a box width of two, which is even: two
  // passes of width two on either side of the pixel, and one of width
  // three. The three together reach two pixels past an edge, not three.
  Draw(Doc('<defs><filter id="f" color-interpolation-filters="sRGB" '
    + 'x="-50%" y="-50%" width="200%" height="200%">'
    + '<feGaussianBlur stdDeviation="1"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertTrue('the blur reaches two pixels past the edge',
    AlphaAt(8, 15) > 0.001);
  AssertEquals('and no further', 0.0, AlphaAt(7, 15), Delta);
end;


procedure TTestSVGFilterDrawing.TestAnImageIsTurnedIntoTheLightTheChainWorksIn;

begin
  // The chain works on light unless told otherwise. A grey drawn as the
  // image of a primitive has to come back the grey it went in as.
  Draw(Doc('<defs><rect id="grey" x="10" y="10" width="10" height="10" '
    + 'fill="#808080"/>'
    + '<filter id="f"><feImage xlink:href="#grey"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the grey comes back the grey it went in as', 0.502,
    FSoft.Image.Colors[15, 15].Red / 65535, 0.01);
end;


procedure TTestSVGFilterDrawing.TestAFloodIsTurnedIntoThatLightToo;

begin
  Draw(Doc('<defs><filter id="f"><feFlood flood-color="#808080"/>'
    + '</filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the flood is the grey it was given', 0.502,
    FSoft.Image.Colors[15, 15].Red / 65535, 0.01);
end;


procedure TTestSVGFilterDrawing.TestALightingColourIsTurnedIntoItAsWell;

const
  Diffuse = '<feDiffuseLighting surfaceScale="0" diffuseConstant="1" '
    + 'lighting-color="#808080"><feDistantLight azimuth="0" '
    + 'elevation="90"/></feDiffuseLighting>';
  Specular = '<feSpecularLighting surfaceScale="0" specularConstant="1" '
    + 'specularExponent="1" lighting-color="#808080">'
    + '<feDistantLight azimuth="0" elevation="90"/></feSpecularLighting>';
  SRGB = ' color-interpolation-filters="sRGB"';

  function RedOf(const aPrimitive, aSpace: String): Double;
  begin
    Draw(Doc('<defs><filter id="f"' + aSpace + '>' + aPrimitive
      + '</filter></defs>'
      + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
      + 'filter="url(#f)"/>'));
    Result := FSoft.Image.Colors[15, 15].Red / 65535;
  end;

begin
  // A light multiplies its colour by a number, so lighting in linear
  // light and lighting in the displayed colours give the same result.
  // They only agree when the colour is converted on the way in.
  AssertEquals('a diffuse light comes to the same in either space',
    RedOf(Diffuse, SRGB), RedOf(Diffuse, ''), 0.01);
  AssertEquals('and so does a specular one',
    RedOf(Specular, SRGB), RedOf(Specular, ''), 0.01);
end;


// A flat lit square, so that the light alone decides what is drawn.
// aWhere is the light, aPlace the transform put on the shape.
function LitDoc(const aLight, aPlace: String): String;

begin
  Result := Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feDiffuseLighting in="SourceGraphic" surfaceScale="0" '
    + 'diffuseConstant="1" lighting-color="white">' + aLight
    + '</feDiffuseLighting></filter></defs>'
    + '<g ' + aPlace + '><rect x="0" y="0" width="20" height="20" '
    + 'fill="#0000ff" filter="url(#f)"/></g>');
end;


procedure TTestSVGFilterDrawing.TestAPointLightStandsWhereTheDocumentPutsIt;

begin
  // The shape is moved by ten and ten, so a light at twenty and twenty in
  // its own space sits over the far corner of the shape and not over the
  // middle.
  Draw(LitDoc('<fePointLight x="20" y="20" z="5"/>',
    'transform="translate(10, 10)"'));
  AssertEquals('the far corner is under the light', 0.871,
    FSoft.Image.Colors[28, 28].Red / 65535, 0.05);
  AssertEquals('and the near one is a long way off it', 0.193,
    FSoft.Image.Colors[12, 12].Red / 65535, 0.05);
end;


procedure TTestSVGFilterDrawing.TestASpotLightsItsConeAndNothingOutsideIt;

begin
  // The light is ten above the middle of the square and points straight
  // down. A point six pixels off the axis lies forty degrees out, well
  // beyond a cone of twenty, and the square reaches thirty so that point
  // is still on it.
  Draw(LitDoc('<feSpotLight x="10" y="10" z="10" pointsAtX="10" '
    + 'pointsAtY="10" pointsAtZ="0" limitingConeAngle="20"/>',
    'transform="translate(10, 10)"'));
  AssertEquals('the point the spot aims at is lit', 1.0,
    FSoft.Image.Colors[20, 20].Red / 65535, 0.05);
  AssertEquals('and a point outside the cone is not', 0.0,
    FSoft.Image.Colors[26, 26].Red / 65535, 0.02);
end;


procedure TTestSVGFilterDrawing.TestASpotExponentNarrowsTheBeam;

  function OffAxisAt(const aExponent: String): Double;
  begin
    Draw(LitDoc('<feSpotLight x="10" y="10" z="10" pointsAtX="10" '
      + 'pointsAtY="10" pointsAtZ="0" specularExponent="' + aExponent
      + '"/>', 'transform="translate(10, 10)"'));
    Result := FSoft.Image.Colors[26, 26].Red / 65535;
  end;

begin
  // An exponent of zero leaves the beam as wide as the light reaches. A
  // high exponent narrows it to the point the spot aims at.
  AssertEquals('an exponent of nothing does not narrow it', 0.763,
    OffAxisAt('0'), 0.05);
  AssertEquals('a high one leaves little off the axis', 0.088,
    OffAxisAt('8'), 0.03);
end;


procedure TTestSVGFilterDrawing.TestALightInBoxUnitsIsReadAgainstTheBox;

  function LitAt(const aUnits, aLight: String): Double;
  begin
    Draw(Doc('<defs><filter id="f" color-interpolation-filters="sRGB" '
      + 'primitiveUnits="' + aUnits + '">'
      + '<feDiffuseLighting in="SourceGraphic" surfaceScale="0" '
      + 'diffuseConstant="1" lighting-color="white"><fePointLight '
      + aLight + '/></feDiffuseLighting></filter></defs>'
      + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
      + 'filter="url(#f)"/>'));
    Result := FSoft.Image.Colors[13, 13].Red / 65535;
  end;

begin
  // The square is ten across at ten, so the whole of the box each way is
  // the far corner at twenty, and half its diagonal is five.
  AssertEquals('the light sits where the box units put it', 0.451,
    LitAt('objectBoundingBox', 'x="1" y="1" z="0.5"'), 0.02);
  AssertEquals('which is where user space puts the same light',
    LitAt('userSpaceOnUse', 'x="20" y="20" z="5"'),
    LitAt('objectBoundingBox', 'x="1" y="1" z="0.5"'), 0.005);
end;


// Two squares of noise side by side, each ten across in its own space.
function TurbDoc(const aFirst, aSecond: String): String;

begin
  Result := Doc('<defs>'
    + '<filter id="a" color-interpolation-filters="sRGB">'
    + '<feTurbulence baseFrequency="0.1" ' + aFirst + '/></filter>'
    + '<filter id="b" color-interpolation-filters="sRGB">'
    + '<feTurbulence baseFrequency="0.1" ' + aSecond + '/></filter>'
    + '</defs>'
    + '<rect width="10" height="10" filter="url(#a)"/>'
    + '<rect transform="translate(20 0)" width="10" height="10" '
    + 'filter="url(#b)"/>');
end;


procedure TTestSVGFilterDrawing.TestNoiseStandsInTheSpaceTheShapeDrawsIn;

begin
  // Both squares stand at nothing in a space of their own, so the same
  // stretch of the noise falls on each.
  Draw(TurbDoc('seed="1"', 'seed="1"'));
  AssertEquals('the noise follows the shape and not the page',
    NoiseSum(0), NoiseSum(20));
end;


procedure TTestSVGFilterDrawing.TestASeedIsCutOffAndNotRounded;

begin
  // A seed is a whole number to SVG, so one and a half is one. Rounding
  // it would make it two.
  Draw(TurbDoc('seed="1.5"', 'seed="1"'));
  AssertEquals('a seed of one and a half is a seed of one',
    NoiseSum(20), NoiseSum(0));
  Draw(TurbDoc('seed="1.5"', 'seed="2"'));
  AssertTrue('and is not a seed of two', NoiseSum(0) <> NoiseSum(20));
end;


procedure TTestSVGFilterDrawing.TestALightingColourMayNameTheColourItStandsIn;

begin
  // The filter sits inside a green defs and is used by a red shape.
  // currentColor is the green it inherits, and not the red of the shape it
  // is drawn for.
  Draw(Doc('<defs color="#00ff00"><filter id="f" '
    + 'color-interpolation-filters="sRGB">'
    + '<feDiffuseLighting in="SourceGraphic" surfaceScale="0" '
    + 'diffuseConstant="1" lighting-color="currentColor">'
    + '<feDistantLight azimuth="0" elevation="90"/>'
    + '</feDiffuseLighting></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'color="#ff0000" filter="url(#f)"/>'));
  AssertEquals('the light takes the colour at the filter', $FFFF,
    FSoft.Image.Colors[15, 15].Green);
  AssertEquals('and none of the colour of the shape', 0,
    FSoft.Image.Colors[15, 15].Red);
end;


procedure TTestSVGFilterDrawing.TestAFloodColourMayNameTheColourItStandsIn;

begin
  Draw(Doc('<defs color="#00ff00"><filter id="f" '
    + 'color-interpolation-filters="sRGB">'
    + '<feFlood flood-color="currentColor"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'color="#ff0000" filter="url(#f)"/>'));
  AssertEquals('the flood takes the colour at the filter', $FFFF,
    FSoft.Image.Colors[15, 15].Green);
  AssertEquals('and none of the colour of the shape', 0,
    FSoft.Image.Colors[15, 15].Red);
end;


procedure TTestSVGFilterDrawing.TestTheNormalIsWeighedAsSVGWeighsIt;

begin
  // The alpha climbs from zero to one across twenty pixels and the surface
  // is forty high, so the height rises by two per pixel. SVG weights that
  // to a slope of four, while the plain difference between the two
  // neighbours would give two.
  Draw(Doc('<defs><linearGradient id="g" x1="0" y1="0" x2="1" y2="0">'
    + '<stop offset="0" stop-color="#ffffff" stop-opacity="0"/>'
    + '<stop offset="1" stop-color="#ffffff" stop-opacity="1"/>'
    + '</linearGradient>'
    + '<filter id="f" color-interpolation-filters="sRGB">'
    + '<feDiffuseLighting in="SourceGraphic" surfaceScale="40" '
    + 'diffuseConstant="1" lighting-color="#ffffff">'
    + '<feDistantLight azimuth="180" elevation="45"/>'
    + '</feDiffuseLighting></filter></defs>'
    + '<rect x="10" y="10" width="20" height="20" fill="url(#g)" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the slope SVG weighs lights it this much', 0.858,
    FSoft.Image.Colors[20, 20].Red / 65535, 0.03);
end;


procedure TTestSVGFilterDrawing.TestALightShiningAwayFromTheEyeLightsNothing;

begin
  // An elevation of 270 points the light straight away from the eye, so
  // the half way between the two is nothing at all and lights nothing.
  Draw(Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feSpecularLighting in="SourceGraphic" surfaceScale="10" '
    + 'specularConstant="1" specularExponent="1" lighting-color="#ffffff">'
    + '<feDistantLight azimuth="0" elevation="270"/>'
    + '</feSpecularLighting></filter></defs>'
    + '<circle cx="20" cy="20" r="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('nothing of the shape is lit', 0.0, CoveredArea, 0.5);
end;


procedure TTestSVGFilterDrawing.TestBackgroundImageReadsWhatWasDrawnUnder;

begin
  // The bar stops at twelve, so the pixel at fifteen came from nine and
  // can only have been read from underneath.
  Draw(BackdropDoc('BackgroundImage', 'enable-background="new"'));
  AssertEquals('the background is the drawing under the element', $FFFF,
    FSoft.Image.Colors[15, 15].Green);
  AssertEquals('and not the element itself', 0,
    FSoft.Image.Colors[15, 15].Blue);
end;


procedure TTestSVGFilterDrawing.TestBackgroundAlphaReadsThatInBlack;

begin
  Draw(BackdropDoc('BackgroundAlpha', 'enable-background="new"'));
  AssertEquals('the background covers the area it was drawn in', 1.0,
    AlphaAt(15, 15), Delta);
  AssertEquals('and is black', 0, FSoft.Image.Colors[15, 15].Green);
end;


procedure TTestSVGFilterDrawing.TestWithoutEnableBackgroundThereIsNoBackground;

begin
  Draw(BackdropDoc('BackgroundImage', 'id="plain"'));
  AssertEquals('nothing gathers unless an element asks for it', 0.0,
    AlphaAt(15, 15), Delta);
end;


procedure TTestSVGFilterDrawing.TestAPrimitiveIsHeldToTheBoxItNames;

begin
  // The region runs from nine to twenty one, the box only from twelve to
  // sixteen.
  Draw(DrawnDoc('<feFlood flood-color="#ff0000" x="12" y="12" '
    + 'width="4" height="4"/>'));
  AssertEquals('the flood covers the box it was given', 1.0,
    AlphaAt(14, 14), 0.01);
  AssertEquals('and stops there, inside the region', 0.0,
    AlphaAt(10, 10), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAnAxisThePrimitiveOmitsIsTheOneOfTheRegion;

begin
  // Only the left edge is given, so the box runs from twelve to the right
  // edge of the region and keeps the full height of it.
  Draw(DrawnDoc('<feFlood flood-color="#ff0000" x="12"/>'));
  AssertEquals('the box starts at the position the primitive gives', 0.0,
    AlphaAt(10, 15), 0.01);
  AssertEquals('and takes the full height of the region', 1.0,
    AlphaAt(14, 10), 0.01);
end;


procedure TTestSVGFilterDrawing.TestABoxUnitCornerStartsAtTheCornerOfTheBox;

begin
  // Zero to one in object bounding box units is the square itself, which
  // sits at ten and not at the origin.
  Draw(Doc('<defs><filter id="f" primitiveUnits="objectBoundingBox" '
    + 'color-interpolation-filters="sRGB">'
    + '<feFlood flood-color="#ff0000" x="0" y="0" width="1" height="1"/>'
    + '</filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('the box of the primitive is the box of the shape', 1.0,
    AlphaAt(15, 15), 0.01);
  AssertEquals('and does not start at the origin of the user space', 0.0,
    AlphaAt(5, 5), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAPercentageInBoxUnitsIsAFractionOfTheBox;

begin
  Draw(Doc('<defs><filter id="f" primitiveUnits="objectBoundingBox" '
    + 'color-interpolation-filters="sRGB">'
    + '<feFlood flood-color="#ff0000" x="0%" y="0%" width="50%" '
    + 'height="50%"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('half the box is five units of the ten', 1.0,
    AlphaAt(12, 12), 0.01);
  AssertEquals('and the far half is left alone', 0.0, AlphaAt(17, 17), 0.01);
end;


procedure TTestSVGFilterDrawing.TestAnImageNamingNothingDrawsNothing;

begin
  Draw(Doc('<defs><filter id="f" color-interpolation-filters="sRGB">'
    + '<feImage xlink:href="#gone"/></filter></defs>'
    + '<rect x="10" y="10" width="10" height="10" fill="#0000ff" '
    + 'filter="url(#f)"/>'));
  AssertEquals('an image pointing to nothing draws nothing at all', 0.0,
    AlphaAt(15, 15), 0.01);
end;


{ TTestSVGViewport }

procedure TTestSVGViewport.TestTheOutermostSVGIsNotPlacedByItsXAndY;

begin
  // SVG says the x and y of the outermost svg have no effect. Honouring
  // them would move the whole drawing off the frame.
  Draw('<svg xmlns="http://www.w3.org/2000/svg" width="40" height="40" '
    + 'x="20" y="20"><rect x="0" y="0" width="10" height="10" '
    + 'fill="#0000ff"/></svg>');
  AssertEquals('the shape sits where the document puts it', 1.0,
    AlphaAt(5, 5), Delta);
  AssertEquals('and is not moved by the x and y of the root', 0.0,
    AlphaAt(25, 25), Delta);
end;


procedure TTestSVGViewport.TestANestedSVGIsPlacedByItsXAndY;

begin
  Draw('<svg xmlns="http://www.w3.org/2000/svg" width="40" height="40">'
    + '<svg x="20" y="20" width="20" height="20">'
    + '<rect x="0" y="0" width="10" height="10" fill="#0000ff"/>'
    + '</svg></svg>');
  AssertEquals('a viewport inside another is placed by them', 1.0,
    AlphaAt(25, 25), Delta);
  AssertEquals('and does not draw at the origin', 0.0,
    AlphaAt(5, 5), Delta);
end;


initialization
  RegisterTest('filter', TTestSVGFilterChain);
  RegisterTest('filter', TTestSVGFilterDrawing);
  RegisterTest('clippath', TTestSVGClipPath);
  RegisterTest('maskelement', TTestSVGMaskElement);
  RegisterTest('colourspace', TTestSVGColourSpace);
  RegisterTest('pattern', TTestSVGPattern);
  RegisterTest('imageelement', TTestSVGImageElement);
  RegisterTest('imageelement', TTestSVGImageResolving);
  RegisterTest('viewport', TTestSVGViewport);
  RegisterTest('refs', TTestSVGReferenceGoldens);
  RegisterTest('colour', TTestSVGReferenceColours);
  RegisterTest('external', TTestSVGExternalReferences);
  RegisterTest('external', TTestSVGFontFileResolving);
  RegisterTest('external', TTestSVGStyleSheetResolving);
end.
