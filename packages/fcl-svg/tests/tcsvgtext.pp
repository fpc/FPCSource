{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for text layout, glyph runs and the font registry.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgtext;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, FpImage, FpcUnit.Test,
     FpcUnit.Registry, svggoldens, svgstubfont, svgpixels,
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.style, fpsvg.backend,
     fpsvg.trace, fpsvg.text, fpsvg.render, fpsvg.soft,
     fpsvg.freetype, fpsvg.fonts.support;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpimage, fpcunit, testregistry, svggoldens,
     svgstubfont, svgpixels, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.style, fpsvg.backend, fpsvg.trace, fpsvg.text,
     fpsvg.render, fpsvg.soft, fpsvg.freetype, fpsvg.fonts.support;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGCodePoints = class(TTestCase)
  published
    procedure TestAsciiIsOneByte;
    procedure TestTwoByteSequence;
    procedure TestThreeByteSequence;
    procedure TestFourByteSequence;
    procedure TestTruncatedSequenceIsReplaced;
    procedure TestStrayContinuationIsReplaced;
    procedure TestIndexRunsPastTheEnd;
  end;

  TTestSVGTextLayout = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FStyles: TSVGStyleResolver;
    FFonts: TSVGStubFontProvider;
    FLayout: TSVGTextLayout;
    // Lays out the first text element of a document built from source.
    function LayoutSource(const aText: String): Boolean;
    // Total number of glyphs across all runs.
    function GlyphCount: Integer;
    // The nth glyph across all runs, in order.
    function GlyphAt(aIndex: Integer): TSVGGlyph;
    // Lays out text against a document that holds one path with id p.
    function LayoutOnPath(const aPath, aContent: String): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGlyphsAdvanceByHalfTheSize;
    procedure TestTextXAndYPlaceTheBaseline;
    procedure TestAnchorMiddleCentresTheText;
    procedure TestAnchorEndPutsTheTextBeforeTheOrigin;
    procedure TestTspanContinuesWhereTheTextLeftOff;
    procedure TestTspanAbsoluteXMovesTheRest;
    procedure TestTspanAbsoluteXStartsANewAnchorChunk;
    procedure TestDxShiftsTheRestOfTheLine;
    procedure TestDyShiftsTheBaseline;
    procedure TestPositionListsApplyPerCharacter;
    procedure TestLetterSpacingWidensEveryAdvance;
    procedure TestWordSpacingAppliesToSpacesOnly;
    procedure TestWhitespaceIsCollapsed;
    procedure TestLeadingAndTrailingSpaceIsDropped;
    procedure TestPreserveKeepsEverySpace;
    procedure TestTextLengthSpreadsTheGlyphs;
    procedure TestTextLengthWithGlyphsScalesThePositions;
    procedure TestTextLengthWithGlyphsWidensTheGlyphsToo;
    procedure TestTextLengthWithGlyphsEndsWhereItSays;
    procedure TestTextLengthWithGlyphsNarrowsThemToSqueeze;
    procedure TestTextLengthBySpacingLeavesTheGlyphsAlone;
    procedure TestTspanSizeChangesTheAdvance;
    procedure TestMultibyteTextIsOneGlyphPerCharacter;
    procedure TestWithoutAFontNothingIsLaidOut;
    procedure TestEmptyTextLaysOutNothing;
    procedure TestBoundsCoverAscentAndDescent;
    procedure TestRunsSplitAtATspan;
    procedure TestTextPathLaysTheGlyphsAlongThePath;
    procedure TestTextPathStartOffsetMovesTheTextAlong;
    procedure TestTextPathStartOffsetTakesAShareOfTheLength;
    procedure TestTextPathTurnsTheGlyphsWithTheTangent;
    procedure TestTextPathDyLiftsTheGlyphsOffThePath;
    procedure TestGlyphsPastTheEndOfThePathAreDropped;
    procedure TestTextPathNamingAShapeDrawsNothing;
    procedure TestTextPathAnchorsAgainstTheOffset;
    procedure TestACoveredCharacterKeepsTheWantedFace;
    procedure TestAnUncoveredCharacterTakesTheCoveringFace;
    procedure TestTheRunSplitsWhereTheFaceChanges;
    procedure TestWithoutACoverTheWantedFaceDrawsItAnyway;
    procedure TestASpaceIsNeverLookedUp;
    procedure TestTheCoveringFaceMeasuresItsOwnAdvance;
    procedure TestTheBaselineSitsWhereItWasGivenByDefault;
    procedure TestMiddleRaisesTheTextByHalfALowerCaseLetter;
    procedure TestCentralRaisesTheTextByHalfItsHeight;
    procedure TestHangingLetsTheTextHangBelowThePoint;
    procedure TestTheEdgeBaselinesPutTheBoxOnThePoint;
    procedure TestTheShiftDoesNotReachTheNextCharacter;
    procedure TestAnUnknownBaselineKeywordChangesNothing;
    procedure TestSuperLiftsTheTextOffTheBaseline;
    procedure TestSubDropsTheTextBelowIt;
    procedure TestALengthLiftsTheTextByThatMuch;
    procedure TestAPercentageCountsAgainstTheFontSize;
    procedure TestAShiftInsideAShiftAddsUp;
    procedure TestAChildOfAShiftedElementDoesNotShiftAgain;
    procedure TestTheBaselineKeywordShiftsNothingFurther;
    procedure TestAShiftMovesNothingSideways;
    procedure TestTheSpaceBeforeALineStaysWithTheParent;
    procedure TestALineCoversTheSpacesInsideItsOwnElement;
    procedure TestTheSpaceIsSpacedByTheElementItWasReadIn;
    procedure TestTheSpaceDoesNotTakeTheNextElementsPosition;
    procedure TestTheChunkIsAnchoredByTheElementThatOpensIt;
    procedure TestRotateTurnsEachCharacterByItsOwnAngle;
    procedure TestTheLastAngleTurnsEveryCharacterAfterIt;
    procedure TestAnInnerListTurnsTheCharactersOfItsOwnElement;
    procedure TestAnElementWithoutAListFollowsTheOneAbove;
    procedure TestRotateTurnsOnTopOfWhatThePathTurns;
    procedure TestRotateMovesNothingSideways;
    procedure TestATrefTakesTheTextItNames;
    procedure TestATrefTakesTheTextOfEverythingTheTargetHolds;
    procedure TestATrefThatNamesNothingAddsNoText;
    procedure TestATrefIsPlacedAndPaintedLikeATspan;
    procedure TestATrefThatNamesItselfAddsNoText;
    procedure TestAChildKeepsItsOwnSpacesWhereTheTextCollapses;
    procedure TestAChildCollapsesWhereTheTextKeepsItsSpaces;
    procedure TestFontStretchReachesTheRequest;
    procedure TestFontStretchIsInheritedByAChild;
    procedure TestWiderStepsOneWidthFromTheParent;
    procedure TestNarrowerStopsAtTheNarrowestWidth;
    procedure TestSmallCapsDrawsALowerCaseLetterAsItsCapital;
    procedure TestASmallCapitalIsNarrowerThanARealOne;
    procedure TestARealCapitalKeepsItsSize;
    procedure TestSmallCapsSplitsTheRunAtEveryChange;
    procedure TestWithoutSmallCapsTheLetterIsLeftAlone;
    procedure TestAStyleNameGivesTheWeightOfAFace;
    procedure TestAStyleNameThatNamesNoWeight;
    procedure TestAFaceNameGivesTheWidthOfAFace;
    procedure TestAFaceNameThatNamesNoWidth;
    procedure TestACoordinateMayHaveAUnit;
    procedure TestEveryEntryOfAListMayHaveItsOwn;
    procedure TestACoordinateWithAnUnknownUnitIsRefused;
    procedure TestAVerticalTextAdvancesDownThePage;
    procedure TestAVerticalGlyphStandsBackHalfItsOwnWidth;
    procedure TestAVerticalTextHangsFromTheAscent;
    procedure TestTheAnchorRunsDownTheColumn;
    procedure TestAShiftMovesAcrossTheColumn;
    procedure TestAnAbsoluteYOpensAChunkDownTheColumn;
    procedure TestTextLengthIsSpreadDownTheColumn;
    procedure TestAcrossThePageIsLeftAsItWas;
    procedure TestAutoLaysALatinGlyphOnItsSide;
    procedure TestAutoStandsAnIdeographUpright;
    procedure TestAGlyphOnItsSideRunsOnItsWidth;
    procedure TestAnUprightGlyphRunsOnItsHeight;
    procedure TestZeroStandsALatinGlyphUpright;
    procedure TestAQuarterTurnLaysDownEvenAnIdeograph;
    procedure TestAGlyphOnItsSideIsCentredOnWhatItStands;
    procedure TestTheUprightCharactersAreTheIdeographicOnes;
  end;

  TTestSVGTextRender = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTrace: TSVGTraceBackend;
    FSoft: TSVGSoftBackend;
    FRenderer: TSVGRenderer;
    FFonts: TSVGStubFontProvider;
    procedure Trace(const aText: String);
    procedure Draw(const aText: String);
    function CountLines(const aPrefix: String): Integer;
    function FirstLine(const aPrefix: String): String;
    function CoveredArea: Double;
    function AlphaAt(aX, aY: Integer): Double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTextEmitsOneGlyphRun;
    procedure TestGlyphRunHasTheFill;
    procedure TestWithoutAProviderNothingIsDrawn;
    procedure TestHiddenTextDrawsNothing;
    procedure TestTextWithoutFillDrawsNothing;
    procedure TestGlyphsReachTheSurface;
    procedure TestGlyphsSitAboveTheBaseline;
    procedure TestTransformMovesTheText;
    procedure TestTspanSplitsIntoTwoRuns;
    procedure TestTextIsClippedLikeAnyShape;
    procedure TestALinkInsideATextIsLaidOutWithIt;
    procedure TestALinkInsideATextTakesItsOwnPaint;
    procedure TestAnAltGlyphSetsTheGlyphItNames;
    procedure TestAnAltGlyphMissingOneGlyphFallsBackWhole;
    procedure TestAnAltGlyphTakesTheFirstItemThatResolves;
    procedure TestALigatureIsSetWithOneGlyph;
    procedure TestGlyphsPaintInTheFillColour;
    procedure TestTspanPaintsInItsOwnColour;
    procedure TestFillOpacityKeepsTheHue;
    procedure TestGroupOpacityDoesNotDarkenTheGlyphs;
    procedure TestUnderlineDrawsOneBar;
    procedure TestTwoDecorationsDrawTwoBars;
    procedure TestWithoutADecorationNothingIsDrawn;
    procedure TestTheBarTakesThePaintOfTheText;
    procedure TestATspanIsDecoratedOnItsOwn;
    procedure TestTextWithoutFillGetsNoBar;
    procedure TestStrokedTextIsStroked;
    procedure TestTextFillsAndStrokesInThatOrder;
    procedure TestTextWithoutAStrokeIsNotStroked;
    procedure TestAStrokeOfNoWidthDrawsNothing;
    procedure TestTheStrokeTakesThePenOfTheText;
    procedure TestATspanIsStrokedOnItsOwn;
    procedure TestTextThatPaintsTwiceUnderOpacityGetsALayer;
    procedure TestTheLineTakesThePaintOfWhatDeclaredIt;
    procedure TestATspanAddsALineOfItsOwnOverTheOne;
    procedure TestTheLineRunsUnderEverythingItsElementHolds;
    procedure TestAFaceIsTakenAtItsWordAboutItsUnderline;
    procedure TestAFaceThatSaysNothingLeavesTheLineToTheSize;
    procedure TestALineUnderAColumnRunsDownItsLeft;
    procedure TestALineOverAColumnRunsDownItsRight;
    procedure TestAStrikeRunsDownTheMiddleOfTheColumn;
    procedure TestTheLineReachesTheEndsOfTheColumnAndNoFurther;
    procedure TestAColumnIsKernedByThePairsWrittenForIt;
    procedure TestAColumnWithoutSuchAPairIsLeftAlone;
    procedure TestThePairsThatKernARowDoNotKernAColumn;
    procedure TestAPairOnItsSideTakesThePairsThatKernARow;
    procedure TestAPairOnItsSideIgnoresThePairsWrittenForAColumn;
    procedure TestAPairStandingOneOfEachWayIsLeftAlone;
    procedure TestAFaceOfSmallCapitalsIsLeftToDrawThem;
    procedure TestAFaceWithoutThemHasACapitalStandIn;
  end;

  { A text inside a clipPath. Its glyphs are the silhouette that cuts
    whatever names the clip path. Every glyph of the stub font is a square
    half an em wide and eight tenths of an em tall, sitting on the
    baseline, and a space has no outline at all. }
  TTestSVGTextAsClipPath = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FSoft: TSVGSoftBackend;
    FRenderer: TSVGRenderer;
    FFonts: TSVGStubFontProvider;
    // Renders a document holding aClip as the content of a clipPath, with
    // a black rectangle over the whole surface cut by it.
    procedure ClipWith(const aClip: String);
    function CoveredArea: Double;
    function AlphaAt(aX, aY: Integer): Double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGlyphsBecomeTheSilhouette;
    procedure TestTheAreaIsThatOfTheGlyphs;
    procedure TestNothingOutsideTheGlyphsIsKept;
    procedure TestASpaceLeavesAHole;
    procedure TestATspanJoinsTheSilhouette;
    procedure TestTheFillOfTheClipTextIsIgnored;
    procedure TestHiddenClipTextClipsNothingAway;
    procedure TestDisplayNoneClipTextDrawsNothing;
    procedure TestAShapeAndATextJoinOneSilhouette;
    procedure TestWithoutAProviderNothingIsKept;
  end;

  TTestSVGTextGoldens = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTrace: TSVGTraceBackend;
    FSoft: TSVGSoftBackend;
    FRenderer: TSVGRenderer;
    FFonts: TSVGStubFontProvider;
    // The alpha channel of the software surface, one character per pixel.
    function AlphaText: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTextWalkGolden;
    procedure TestTextCoverageGolden;
  end;

  TTestSVGFontRegistry = class(TTestCase)
  private
    FProvider: TSVGFreeTypeProvider;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProviderConstructsWithoutFonts;
    procedure TestMissingFileIsRefused;
    procedure TestMissingDirectoryAddsNothing;
    procedure TestEmptyRegistryResolvesNothing;
    procedure TestSystemFontPathsAreNamed;
    procedure TestARecordedFontOpensWhenItIsAskedFor;
    procedure TestTheGenericFamilyNamesAreRecognised;
    procedure TestARealFamilyIsNotAGenericOne;
    procedure TestAGenericFamilyCanBeChosenByHand;
    procedure TestAGenericFamilyResolvesToARecordedOne;
    procedure TestAWellKnownFamilyIsClassified;
    procedure TestAFamilyNothingKnowsOfFallsToSansSerif;
    procedure TestASystemFaceIsNotKernedUnlessItIsAskedFor;
    procedure TestACoverComesFromTheFamiliesTheDocumentNamed;
    procedure TestACoverNoNamedFamilyHoldsIsLeftToTheSearch;
    procedure TestADirectoryOfWebFontsIsRead;
    procedure TestAFileOfNoKnownExtensionIsPassedOver;
  end;

  { What a face of the system reports for text running down the page.
    Every test here needs a face with a Han character, and is skipped when
    the machine has none. }
  TTestSVGVerticalMetrics = class(TTestCase)
  private
    FProvider: TSVGFreeTypeProvider;
    function FaceHolding(aCode: Cardinal): ISVGFont;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAFullWidthGlyphMovesThePenItsOwnWidthDown;
    procedure TestAFullWidthGlyphStandsBackHalfOfItself;
    procedure TestANarrowGlyphStandsBackHalfOfItsOwnWidth;
    procedure TestAMixedColumnHangsFromOneBaseline;
    procedure TestEveryGlyphMovesThePenTheSameWayDown;
  end;

  { The coverage source that fpsvg.fonts.support selects. Only the unit of
    the platform being built for is compiled, so the test asks which one
    that is rather than holding all three. }
  TTestSVGPlatformCoverage = class(TTestCase)
  private
    FProvider: TSVGFreeTypeProvider;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheSelectedSourceIsTheOneOfTheBuildPlatform;
    procedure TestTheSourceIsNamedWhenThereIsOne;
    procedure TestTheSameSourceComesBackEveryTime;
    procedure TestItCanBePluggedIntoTheProvider;
  end;

implementation

const
  Delta = 1e-9;
  Size = 10;
  Upright = ' glyph-orientation-vertical="0"';
  Advance = Size * StubAdvanceRatio;

// The bytes written out, as the text of a document. A literal handed
// straight to a TSVGString is re-encoded from the code page the compiler
// takes the source for; through a RawByteString it is not.
function TextOf(const aBytes: RawByteString): TSVGString;

begin
  Result := aBytes;
end;


// Wraps markup in a root element of a hundred units, at a known font
// size.
function Doc(const aBody: TSVGString): TSVGString;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" width="100" '
    + 'height="100" font-size="10">' + aBody + '</svg>';
end;


// The weight given by a style name, or -1 when it gives none.
function NamedWeight(const aText: String): Integer;

begin
  if not SVGWeightOfFaceName(aText, Result) then
    Result := -1;
end;


// The width given by a face name, as its ordinal, or -1 when it gives
// none.
function NamedWidth(const aText: String): Integer;

var
  lStretch: TSVGFontStretch;

begin
  if SVGStretchOfFaceName(aText, lStretch) then
    Result := Ord(lStretch)
  else
    Result := -1;
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


{ TTestSVGCodePoints }

procedure TTestSVGCodePoints.TestAsciiIsOneByte;

var
  lIndex: Integer;

begin
  lIndex := 1;
  AssertEquals('a plain letter decodes to itself', Ord('A'),
    Integer(SVGNextCodePoint('A', lIndex)));
  AssertEquals('and the index moved one byte', 2, lIndex);
end;


procedure TTestSVGCodePoints.TestTwoByteSequence;

var
  lIndex: Integer;

begin
  lIndex := 1;
  AssertEquals('e acute decodes to U+00E9', $E9,
    Integer(SVGNextCodePoint(TextOf(#$C3#$A9), lIndex)));
  AssertEquals('and the index moved two bytes', 3, lIndex);
end;


procedure TTestSVGCodePoints.TestThreeByteSequence;

var
  lIndex: Integer;

begin
  lIndex := 1;
  AssertEquals('the euro sign decodes to U+20AC', $20AC,
    Integer(SVGNextCodePoint(TextOf(#$E2#$82#$AC), lIndex)));
  AssertEquals('and the index moved three bytes', 4, lIndex);
end;


procedure TTestSVGCodePoints.TestFourByteSequence;

var
  lIndex: Integer;

begin
  lIndex := 1;
  AssertEquals('a supplementary character decodes whole', $1F600,
    Integer(SVGNextCodePoint(TextOf(#$F0#$9F#$98#$80), lIndex)));
  AssertEquals('and the index moved four bytes', 5, lIndex);
end;


procedure TTestSVGCodePoints.TestTruncatedSequenceIsReplaced;

var
  lIndex: Integer;

begin
  lIndex := 1;
  AssertEquals('a sequence cut short yields the replacement character',
    $FFFD, Integer(SVGNextCodePoint(TextOf(#$E2#$82), lIndex)));
end;


procedure TTestSVGCodePoints.TestStrayContinuationIsReplaced;

var
  lIndex: Integer;

begin
  lIndex := 1;
  AssertEquals('a continuation byte on its own is not a character', $FFFD,
    Integer(SVGNextCodePoint(TextOf(#$82), lIndex)));
end;


procedure TTestSVGCodePoints.TestIndexRunsPastTheEnd;

var
  lIndex: Integer;

begin
  lIndex := 5;
  AssertEquals('an index past the end yields the replacement character',
    $FFFD, Integer(SVGNextCodePoint('ab', lIndex)));
end;


{ TTestSVGTextLayout }

procedure TTestSVGTextLayout.SetUp;

begin
  inherited SetUp;
  FStyles := TSVGStyleResolver.Create;
  FFonts := TSVGStubFontProvider.Create;
  FLayout := TSVGTextLayout.Create;
end;


procedure TTestSVGTextLayout.TearDown;

begin
  FreeAndNil(FLayout);
  FreeAndNil(FFonts);
  FreeAndNil(FStyles);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGTextLayout.LayoutSource(const aText: String): Boolean;

var
  lText: TSVGElement;
  lStyle: TSVGComputedStyle;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(aText);
  FStyles.LoadDocument(FDocument);
  lText := FDocument.Root.FindChildElement('text');
  AssertNotNull('the document holds a text element', lText);
  lStyle := FStyles.ComputeStyleOf(lText);
  Result := FLayout.Layout(lText, FStyles, FFonts, lStyle,
    TSVGLengthContext.Create(TSVGRect.CreateSize(0, 0, 100, 100)));
end;


function TTestSVGTextLayout.GlyphCount: Integer;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to FLayout.RunCount - 1 do
    Inc(Result, Length(FLayout.Runs[I].Glyphs));
end;


function TTestSVGTextLayout.GlyphAt(aIndex: Integer): TSVGGlyph;

var
  I, lSeen: Integer;

begin
  lSeen := 0;
  for I := 0 to FLayout.RunCount - 1 do
    begin
    if aIndex < lSeen + Length(FLayout.Runs[I].Glyphs) then
      Exit(FLayout.Runs[I].Glyphs[aIndex - lSeen]);
    Inc(lSeen, Length(FLayout.Runs[I].Glyphs));
    end;
  raise ESVGText.CreateFmt('No glyph at %d', [aIndex]);
end;


procedure TTestSVGTextLayout.TestGlyphsAdvanceByHalfTheSize;

begin
  AssertTrue('the text lays out', LayoutSource(Doc('<text>abc</text>')));
  AssertEquals('three characters give three glyphs', 3, GlyphCount);
  AssertEquals('the first sits at the origin', 0.0, GlyphAt(0).X, Delta);
  AssertEquals('the second one advance along', Advance, GlyphAt(1).X, Delta);
  AssertEquals('the third two advances along', 2 * Advance, GlyphAt(2).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestTextXAndYPlaceTheBaseline;

begin
  LayoutSource(Doc('<text x="20" y="30">ab</text>'));
  AssertEquals('the first glyph takes x', 20.0, GlyphAt(0).X, Delta);
  AssertEquals('and y', 30.0, GlyphAt(0).Y, Delta);
  AssertEquals('the second follows it', 20.0 + Advance, GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestAnchorMiddleCentresTheText;

begin
  LayoutSource(Doc('<text x="50" text-anchor="middle">abcd</text>'));
  AssertEquals('four glyphs span two full sizes', 4, GlyphCount);
  AssertEquals('the run is centred on the origin', 50.0 - 2 * Advance,
    GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestAnchorEndPutsTheTextBeforeTheOrigin;

begin
  LayoutSource(Doc('<text x="50" text-anchor="end">abcd</text>'));
  AssertEquals('the run ends at the origin', 50.0 - 4 * Advance,
    GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestTspanContinuesWhereTheTextLeftOff;

begin
  LayoutSource(Doc('<text x="0">ab<tspan>cd</tspan></text>'));
  AssertEquals('four glyphs in all', 4, GlyphCount);
  AssertEquals('the tspan continues from the text', 2 * Advance,
    GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestTspanAbsoluteXMovesTheRest;

begin
  LayoutSource(Doc('<text x="0">ab<tspan x="40">cd</tspan></text>'));
  AssertEquals('the tspan restarts at its own x', 40.0, GlyphAt(2).X, Delta);
  AssertEquals('and the next follows it', 40.0 + Advance, GlyphAt(3).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestTspanAbsoluteXStartsANewAnchorChunk;

begin
  LayoutSource(Doc('<text x="0" text-anchor="middle">ab'
    + '<tspan x="40">cd</tspan></text>'));
  AssertEquals('the first chunk is centred on its own origin', -Advance,
    GlyphAt(0).X, Delta);
  AssertEquals('and the second on its own', 40.0 - Advance, GlyphAt(2).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestDxShiftsTheRestOfTheLine;

begin
  LayoutSource(Doc('<text x="0">a<tspan dx="10">b</tspan></text>'));
  AssertEquals('the shifted glyph moved by dx', Advance + 10.0,
    GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestDyShiftsTheBaseline;

begin
  LayoutSource(Doc('<text x="0" y="0">a<tspan dy="8">b</tspan></text>'));
  AssertEquals('the first glyph stays on the baseline', 0.0, GlyphAt(0).Y,
    Delta);
  AssertEquals('the shifted glyph drops by dy', 8.0, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestPositionListsApplyPerCharacter;

begin
  LayoutSource(Doc('<text x="0 10 20">abc</text>'));
  AssertEquals('the first takes the first x', 0.0, GlyphAt(0).X, Delta);
  AssertEquals('the second the second', 10.0, GlyphAt(1).X, Delta);
  AssertEquals('the third the third', 20.0, GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestLetterSpacingWidensEveryAdvance;

begin
  LayoutSource(Doc('<text letter-spacing="2">abc</text>'));
  AssertEquals('each advance grew by the spacing', Advance + 2.0,
    GlyphAt(1).X, Delta);
  AssertEquals('and it accumulates', 2 * (Advance + 2.0), GlyphAt(2).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestWordSpacingAppliesToSpacesOnly;

begin
  LayoutSource(Doc('<text word-spacing="4">a b</text>'));
  AssertEquals('the space is one glyph of its own', 3, GlyphCount);
  AssertEquals('the letter after the space takes the word spacing',
    2 * Advance + 4.0, GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestWhitespaceIsCollapsed;

begin
  LayoutSource(Doc('<text>a' + LineEnding + '   b</text>'));
  AssertEquals('a run of whitespace becomes one space', 3, GlyphCount);
end;


procedure TTestSVGTextLayout.TestLeadingAndTrailingSpaceIsDropped;

begin
  LayoutSource(Doc('<text>   ab   </text>'));
  AssertEquals('only the letters remain', 2, GlyphCount);
  AssertEquals('and the first sits at the origin', 0.0, GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestPreserveKeepsEverySpace;

begin
  LayoutSource(Doc('<text xml:space="preserve"> ab </text>'));
  AssertEquals('both spaces survive alongside the letters', 4, GlyphCount);
  AssertEquals('the first letter sits one advance in', Advance,
    GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthSpreadsTheGlyphs;

begin
  LayoutSource(Doc('<text textLength="30">abc</text>'));
  AssertEquals('the first glyph stays put', 0.0, GlyphAt(0).X, Delta);
  AssertEquals('the last starts one advance before the target',
    30.0 - Advance, GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthWithGlyphsScalesThePositions;

begin
  LayoutSource(Doc('<text textLength="30" lengthAdjust="spacingAndGlyphs">'
    + 'abc</text>'));
  AssertEquals('the positions scale from the start', 0.0, GlyphAt(0).X,
    Delta);
  AssertEquals('the middle glyph scales with them', 10.0, GlyphAt(1).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthWithGlyphsWidensTheGlyphsToo;

begin
  // Three glyphs of five make fifteen, so thirty is twice the width.
  LayoutSource(Doc('<text textLength="30" lengthAdjust="spacingAndGlyphs">'
    + 'abc</text>'));
  AssertEquals('the first glyph is drawn twice as wide', 2.0,
    GlyphAt(0).Stretch, Delta);
  AssertEquals('and so is the last', 2.0, GlyphAt(2).Stretch, Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthWithGlyphsEndsWhereItSays;

begin
  LayoutSource(Doc('<text textLength="30" lengthAdjust="spacingAndGlyphs">'
    + 'abc</text>'));
  AssertEquals('the last glyph ends on the length asked for', 30.0,
    GlyphAt(2).X + Advance * GlyphAt(2).Stretch, Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthWithGlyphsNarrowsThemToSqueeze;

begin
  LayoutSource(Doc('<text textLength="7.5" lengthAdjust="spacingAndGlyphs">'
    + 'abc</text>'));
  AssertEquals('a length below the width draws them narrower', 0.5,
    GlyphAt(0).Stretch, Delta);
  AssertEquals('and they end there as well', 7.5,
    GlyphAt(2).X + Advance * GlyphAt(2).Stretch, Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthBySpacingLeavesTheGlyphsAlone;

begin
  // Spacing moves the glyphs apart and draws each at its own width.
  LayoutSource(Doc('<text textLength="30">abc</text>'));
  AssertEquals('the first glyph keeps its width', 1.0,
    GlyphAt(0).Stretch, Delta);
  AssertEquals('and so does the last', 1.0, GlyphAt(2).Stretch, Delta);
end;


procedure TTestSVGTextLayout.TestTspanSizeChangesTheAdvance;

begin
  LayoutSource(Doc('<text x="0">a<tspan font-size="20">b</tspan>c</text>'));
  AssertEquals('the larger glyph starts after the first advance', Advance,
    GlyphAt(1).X, Delta);
  AssertEquals('and advances by half of its own size', Advance + 10.0,
    GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestMultibyteTextIsOneGlyphPerCharacter;

begin
  LayoutSource(Doc('<text>a' + TextOf(#$C3#$A9) + 'b</text>'));
  AssertEquals('three characters, not four bytes', 3, GlyphCount);
  AssertEquals('the accented letter kept its code point', $E9,
    Integer(GlyphAt(1).GlyphID));
end;


procedure TTestSVGTextLayout.TestWithoutAFontNothingIsLaidOut;

begin
  FFonts.Refuse := True;
  AssertFalse('a refused font leaves nothing to draw',
    LayoutSource(Doc('<text>abc</text>')));
  AssertEquals('and no runs are produced', 0, FLayout.RunCount);
end;


procedure TTestSVGTextLayout.TestEmptyTextLaysOutNothing;

begin
  AssertFalse('an empty text element lays out nothing',
    LayoutSource(Doc('<text></text>')));
end;


procedure TTestSVGTextLayout.TestBoundsCoverAscentAndDescent;

var
  lBounds: TSVGRect;

begin
  LayoutSource(Doc('<text x="0" y="0">ab</text>'));
  lBounds := FLayout.Bounds;
  AssertEquals('the box starts at the first glyph', 0.0, lBounds.Left, Delta);
  AssertEquals('it runs to the end of the last advance', 2 * Advance,
    lBounds.Right, Delta);
  AssertEquals('it reaches the ascent above the baseline',
    -Size * StubAscentRatio, lBounds.Top, Delta);
  AssertEquals('and the descent below it', Size * StubDescentRatio,
    lBounds.Bottom, Delta);
end;


procedure TTestSVGTextLayout.TestRunsSplitAtATspan;

begin
  LayoutSource(Doc('<text>ab<tspan fill="red">cd</tspan>ef</text>'));
  AssertEquals('the text, the tspan and the tail are three runs', 3,
    FLayout.RunCount);
  AssertEquals('and the glyphs are all still there', 6, GlyphCount);
end;


{ TTestSVGTextRender }

procedure TTestSVGTextRender.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
  FFonts := TSVGStubFontProvider.Create;
  FRenderer.Fonts := FFonts;
end;


procedure TTestSVGTextRender.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FFonts);
  FreeAndNil(FTrace);
  FreeAndNil(FSoft);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGTextRender.Trace(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FTrace);
  FTrace := TSVGTraceBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FTrace);
end;


procedure TTestSVGTextRender.Draw(const aText: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FSoft);
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FSoft);
end;


function TTestSVGTextRender.CountLines(const aPrefix: String): Integer;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to FTrace.Log.Count - 1 do
    if Pos(aPrefix, TrimLeft(FTrace.Log[I])) = 1 then
      Inc(Result);
end;


function TTestSVGTextRender.FirstLine(const aPrefix: String): String;

var
  lIndex: Integer;

begin
  lIndex := IndexOfLine(FTrace.Log, aPrefix);
  if lIndex < 0 then
    Result := ''
  else
    Result := TrimLeft(FTrace.Log[lIndex]);
end;


function TTestSVGTextRender.CoveredArea: Double;

var
  X, Y: Integer;

begin
  Result := 0;
  for Y := 0 to FSoft.Image.Height - 1 do
    for X := 0 to FSoft.Image.Width - 1 do
      Result := Result + FSoft.Image.Colors[X, Y].Alpha / 65535;
end;


function TTestSVGTextRender.AlphaAt(aX, aY: Integer): Double;

begin
  Result := FSoft.Image.Colors[aX, aY].Alpha / 65535;
end;


procedure TTestSVGTextRender.TestTextEmitsOneGlyphRun;

begin
  Trace(Doc('<text x="10" y="20">abc</text>'));
  AssertEquals('one element gives one run', 1, CountLines('draw-glyphs'));
  AssertEquals('the run has the resolved face and the paint',
    'draw-glyphs font="stub" paint=color(#000000ff) opacity=1 ctm=identity',
    FirstLine('draw-glyphs'));
end;


procedure TTestSVGTextRender.TestGlyphRunHasTheFill;

begin
  Trace(Doc('<text fill="red">a</text>'));
  AssertTrue('the fill reaches the run',
    Pos('paint=color(#ff0000ff)', FirstLine('draw-glyphs')) > 0);
end;


procedure TTestSVGTextRender.TestWithoutAProviderNothingIsDrawn;

begin
  FRenderer.Fonts := nil;
  Trace(Doc('<text>abc</text>'));
  AssertEquals('no provider means no glyphs', 0, CountLines('draw-glyphs'));
end;


procedure TTestSVGTextRender.TestHiddenTextDrawsNothing;

begin
  Trace(Doc('<text visibility="hidden">abc</text>'));
  AssertEquals('hidden text draws nothing', 0, CountLines('draw-glyphs'));
end;


procedure TTestSVGTextRender.TestTextWithoutFillDrawsNothing;

begin
  Trace(Doc('<text fill="none">abc</text>'));
  AssertEquals('text with no paint draws nothing', 0,
    CountLines('draw-glyphs'));
end;


procedure TTestSVGTextRender.TestGlyphsReachTheSurface;

begin
  Draw(Doc('<text x="0" y="20">ab</text>'));
  AssertEquals('two square glyphs of five by eight', 2 * 5 * 8, CoveredArea,
    0.01);
end;


procedure TTestSVGTextRender.TestGlyphsSitAboveTheBaseline;

begin
  Draw(Doc('<text x="0" y="20">a</text>'));
  AssertEquals('a pixel above the baseline is painted', 1.0, AlphaAt(2, 15),
    Delta);
  AssertEquals('a pixel below it is not', 0.0, AlphaAt(2, 21), Delta);
end;


procedure TTestSVGTextRender.TestTransformMovesTheText;

begin
  Draw(Doc('<text x="0" y="20" transform="translate(40,0)">a</text>'));
  AssertEquals('the glyph moved with the transform', 1.0, AlphaAt(42, 15),
    Delta);
  AssertEquals('and left its old place empty', 0.0, AlphaAt(2, 15), Delta);
end;


procedure TTestSVGTextRender.TestTspanSplitsIntoTwoRuns;

begin
  Trace(Doc('<text>ab<tspan fill="red">cd</tspan></text>'));
  AssertEquals('each span draws its own run', 2, CountLines('draw-glyphs'));
end;


procedure TTestSVGTextRender.TestTextIsClippedLikeAnyShape;

begin
  Draw(Doc('<clipPath id="c"><rect x="0" y="0" width="5" height="100"/>'
    + '</clipPath><text x="0" y="20" clip-path="url(#c)">ab</text>'));
  AssertEquals('only the first glyph survives the clip', 5 * 8, CoveredArea,
    0.01);
end;


// A document holding a text with a link in it. The link namespace is
// declared, which the shared wrapper above has no call to do.
function LinkDoc(const aBody: String): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink" width="100" '
    + 'height="100" font-size="10">' + aBody + '</svg>';
end;


procedure TTestSVGTextRender.TestALinkInsideATextIsLaidOutWithIt;

begin
  // The stub sets every glyph half an em wide, so a text of one glyph and
  // a link of two covers three of them and no more.
  Draw(LinkDoc('<text x="0" y="20" font-size="10">a'
    + '<a xlink:href="other.svg">bc</a></text>'));
  AssertEquals('the characters of the link are laid out with the text',
    3 * 5 * 8, CoveredArea, 0.5);
  AssertEquals('and they follow the character before them', 1.0,
    AlphaAt(12, 15), 0.01);
end;


procedure TTestSVGTextRender.TestALinkInsideATextTakesItsOwnPaint;

begin
  Draw(LinkDoc('<text x="0" y="20" font-size="10" fill="#3366cc">a'
    + '<a xlink:href="other.svg" fill="#cc3366">b</a></text>'));
  AssertPixelNear(Self, 'the text keeps its own colour', FSoft.Image,
    2, 15, TSVGColor.FromBytes($33, $66, $CC, 255), 1);
  AssertPixelNear(Self, 'and the link paints in its', FSoft.Image,
    7, 15, TSVGColor.FromBytes($CC, $33, $66, 255), 1);
end;


// A document with a font of its own: an "a" half an em wide, a named
// glyph that is twice as wide, and lists that use them.
function GlyphDoc(const aBody: String): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink" width="100"'
    + ' height="100"><defs>'
    + '<font><font-face font-family="Named" units-per-em="1000"'
    + ' ascent="1000" descent="0"/>'
    + '<glyph unicode="a" horiz-adv-x="500" d="M0 0H500V1000H0Z"/>'
    + '<glyph unicode="ab" horiz-adv-x="500" d="M0 0H500V1000H0Z"/>'
    + '<glyph unicode="b" horiz-adv-x="500" d="M0 0H500V1000H0Z"/>'
    + '<glyph id="wide" horiz-adv-x="1000" d="M0 0H1000V1000H0Z"/>'
    + '</font>'
    + '<altGlyphDef id="good"><glyphRef xlink:href="#wide"/></altGlyphDef>'
    + '<altGlyphDef id="bad"><glyphRef xlink:href="#wide"/>'
    + '<glyphRef xlink:href="#nowhere"/></altGlyphDef>'
    + '<altGlyphDef id="items">'
    + '<altGlyphItem><glyphRef xlink:href="#nowhere"/></altGlyphItem>'
    + '<altGlyphItem><glyphRef xlink:href="#wide"/></altGlyphItem>'
    + '</altGlyphDef>'
    + '</defs>' + aBody + '</svg>';
end;


// One text of the font above, at a size of twenty.
function GlyphText(const aContent: String): String;

begin
  Result := GlyphDoc('<text x="0" y="20" font-family="Named"'
    + ' font-size="20">' + aContent + '</text>');
end;


procedure TTestSVGTextRender.TestAnAltGlyphSetsTheGlyphItNames;

begin
  // The glyph being referred to is a full em square, while the "a" is
  // half of one.
  Draw(GlyphText('<altGlyph xlink:href="#good">a</altGlyph>'));
  AssertEquals('the glyph that was asked for is set in place of the character', 400.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGTextRender.TestAnAltGlyphMissingOneGlyphFallsBackWhole;

begin
  // The list holds one glyph that exists and one that does not, so the
  // element is set with its own characters instead.
  Draw(GlyphText('<altGlyph xlink:href="#bad">a</altGlyph>'));
  AssertEquals('a list one glyph short sets the characters', 200.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGTextRender.TestAnAltGlyphTakesTheFirstItemThatResolves;

begin
  Draw(GlyphText('<altGlyph xlink:href="#items">a</altGlyph>'));
  AssertEquals('the item that resolves is the one set', 400.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGTextRender.TestALigatureIsSetWithOneGlyph;

begin
  // The font sets "ab" with one glyph half an em wide, while the two
  // characters on their own would cover a whole em.
  Draw(GlyphText('ab'));
  AssertEquals('the two characters are set with the one glyph', 200.0,
    CoveredArea, 0.5);
  Draw(GlyphText('a b'));
  AssertEquals('and apart they are two glyphs', 400.0, CoveredArea, 0.5);
end;


procedure TTestSVGTextRender.TestGlyphsPaintInTheFillColour;

begin
  Draw(Doc('<text x="0" y="20" fill="#3366cc">a</text>'));
  AssertPixel(Self, 'the glyph takes the fill of its element', FSoft.Image,
    2, 15, TSVGColor.FromBytes($33, $66, $CC, 255));
end;


procedure TTestSVGTextRender.TestTspanPaintsInItsOwnColour;

begin
  Draw(Doc('<text x="0" y="20" fill="#3366cc">a<tspan fill="#cc6633">b'
    + '</tspan></text>'));
  AssertPixel(Self, 'the first glyph keeps the colour of the text',
    FSoft.Image, 2, 15, TSVGColor.FromBytes($33, $66, $CC, 255));
  AssertPixel(Self, 'and the tspan paints in its own', FSoft.Image, 7, 15,
    TSVGColor.FromBytes($CC, $66, $33, 255));
end;


procedure TTestSVGTextRender.TestFillOpacityKeepsTheHue;

begin
  Draw(Doc('<text x="0" y="20" fill="#3366cc" fill-opacity="0.5">a</text>'));
  AssertPixelNear(Self, 'the channels survive the opacity', FSoft.Image,
    2, 15, TSVGColor.FromBytes($33, $66, $CC, 128), 1);
end;


procedure TTestSVGTextRender.TestUnderlineDrawsOneBar;

begin
  Trace(Doc('<text x="10" y="20" text-decoration="underline">ab</text>'));
  AssertEquals('the run is drawn once', 1, CountLines('draw-glyphs'));
  AssertEquals('and one bar is filled under it', 1, CountLines('fill-path'));
end;


procedure TTestSVGTextRender.TestTwoDecorationsDrawTwoBars;

begin
  Trace(Doc('<text x="10" y="20" '
    + 'text-decoration="underline overline">ab</text>'));
  AssertEquals('a bar for each line asked for', 2, CountLines('fill-path'));
end;


procedure TTestSVGTextRender.TestWithoutADecorationNothingIsDrawn;

begin
  Trace(Doc('<text x="10" y="20">ab</text>'));
  AssertEquals('text that asks for no line gets none', 0,
    CountLines('fill-path'));
end;


procedure TTestSVGTextRender.TestTheBarTakesThePaintOfTheText;

begin
  Trace(Doc('<text x="10" y="20" fill="#00ff00" '
    + 'text-decoration="underline">ab</text>'));
  AssertTrue('the bar is drawn in the colour the glyphs are',
    Pos('paint=color(#00ff00ff)', FirstLine('fill-path')) > 0);
end;


procedure TTestSVGTextRender.TestATspanIsDecoratedOnItsOwn;

begin
  Trace(Doc('<text x="10" y="20">ab'
    + '<tspan text-decoration="underline">cd</tspan></text>'));
  AssertEquals('the two runs are drawn', 2, CountLines('draw-glyphs'));
  AssertEquals('and only the one that asked for a line gets one', 1,
    CountLines('fill-path'));
end;


procedure TTestSVGTextRender.TestTextWithoutFillGetsNoBar;

begin
  Trace(Doc('<text x="10" y="20" fill="none" '
    + 'text-decoration="underline">ab</text>'));
  AssertEquals('text that paints nothing draws no line either', 0,
    CountLines('fill-path'));
end;


procedure TTestSVGTextRender.TestTheLineTakesThePaintOfWhatDeclaredIt;

begin
  // The text element declares the line, so the line is drawn in its paint
  // and not in the paint of the tspan it happens to run under.
  Trace(Doc('<text x="10" y="20" fill="#0000ff" text-decoration="underline">'
    + '<tspan fill="#ffff00">ab</tspan></text>'));
  AssertEquals('one line is drawn', 1, CountLines('fill-path'));
  AssertTrue('in the colour of the element that asked for it',
    Pos('paint=color(#0000ffff)', FirstLine('fill-path')) > 0);
end;


procedure TTestSVGTextRender.TestATspanAddsALineOfItsOwnOverTheOne;

begin
  Trace(Doc('<text x="10" y="20" fill="#0000ff" text-decoration="underline">'
    + 'ab<tspan fill="#ffff00" text-decoration="underline">cd</tspan>'
    + '</text>'));
  AssertEquals('the text draws one line and the tspan another', 2,
    CountLines('fill-path'));
end;


procedure TTestSVGTextRender.TestTheLineRunsUnderEverythingItsElementHolds;

var
  lWhole, lPart: Double;

begin
  // Both documents draw the same four glyphs. The text element underlines
  // all of them, the tspan only the last two, so the first covers more.
  Draw(Doc('<text x="2" y="20" text-decoration="underline">abcd</text>'));
  lWhole := CoveredArea;
  Draw(Doc('<text x="2" y="20">ab'
    + '<tspan text-decoration="underline">cd</tspan></text>'));
  lPart := CoveredArea;
  AssertTrue('the line of the text element is the longer one',
    lWhole > lPart + 1);
end;


procedure TTestSVGTextRender.TestStrokedTextIsStroked;

begin
  Trace(Doc('<text x="10" y="20" fill="none" stroke="#ff0000">ab</text>'));
  AssertEquals('the outlines of the run are stroked once', 1,
    CountLines('stroke-path'));
  AssertEquals('and nothing is filled', 0, CountLines('draw-glyphs'));
end;


procedure TTestSVGTextRender.TestTextFillsAndStrokesInThatOrder;

begin
  Trace(Doc('<text x="10" y="20" fill="#0000ff" stroke="#ff0000">ab</text>'));
  AssertEquals('the glyphs are filled', 1, CountLines('draw-glyphs'));
  AssertEquals('and their outlines stroked', 1, CountLines('stroke-path'));
  AssertTrue('the fill is drawn before the stroke',
    IndexOfLine(FTrace.Log, 'draw-glyphs')
      < IndexOfLine(FTrace.Log, 'stroke-path'));
end;


procedure TTestSVGTextRender.TestTextWithoutAStrokeIsNotStroked;

begin
  Trace(Doc('<text x="10" y="20" fill="#0000ff">ab</text>'));
  AssertEquals('text without a stroke is not stroked', 0,
    CountLines('stroke-path'));
end;


procedure TTestSVGTextRender.TestAStrokeOfNoWidthDrawsNothing;

begin
  Trace(Doc('<text x="10" y="20" stroke="#ff0000" '
    + 'stroke-width="0">ab</text>'));
  AssertEquals('a pen of no width draws nothing', 0,
    CountLines('stroke-path'));
end;


procedure TTestSVGTextRender.TestTheStrokeTakesThePenOfTheText;

begin
  Trace(Doc('<text x="10" y="20" fill="none" stroke="#ff0000" '
    + 'stroke-width="3">ab</text>'));
  AssertTrue('the pen of the style reaches the backend',
    Pos('width=3', FirstLine('stroke-path')) > 0);
  AssertTrue('and so does the colour it strokes in',
    Pos('paint=color(#ff0000ff)', FirstLine('stroke-path')) > 0);
end;


procedure TTestSVGTextRender.TestATspanIsStrokedOnItsOwn;

begin
  Trace(Doc('<text x="10" y="20" fill="#0000ff">ab'
    + '<tspan stroke="#ff0000">cd</tspan></text>'));
  AssertEquals('both runs are filled', 2, CountLines('draw-glyphs'));
  AssertEquals('and only the one with a stroke is stroked', 1,
    CountLines('stroke-path'));
end;


procedure TTestSVGTextRender.TestTextThatPaintsTwiceUnderOpacityGetsALayer;

begin
  // Fill and stroke overlap along the edge of every glyph, so a run that
  // paints both needs a layer to keep the opacity from doubling there.
  Trace(Doc('<text x="10" y="20" fill="#0000ff" stroke="#ff0000" '
    + 'opacity="0.5">ab</text>'));
  AssertEquals('the run is drawn into a layer', 1, CountLines('push-layer'));
  Trace(Doc('<text x="10" y="20" fill="#0000ff" opacity="0.5">ab</text>'));
  AssertEquals('a run that paints once takes the opacity itself', 0,
    CountLines('push-layer'));
end;


procedure TTestSVGTextRender.TestGroupOpacityDoesNotDarkenTheGlyphs;

begin
  Draw(Doc('<g opacity="0.5"><text x="0" y="20" fill="#3366cc">a</text>'
    + '</g>'));
  AssertPixelNear(Self, 'the layer gives the colour back undimmed',
    FSoft.Image, 2, 15, TSVGColor.FromBytes($33, $66, $CC, 128), 1);
end;


{ TTestSVGTextAsClipPath }

procedure TTestSVGTextAsClipPath.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
  FFonts := TSVGStubFontProvider.Create;
  FRenderer.Fonts := FFonts;
end;


procedure TTestSVGTextAsClipPath.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FFonts);
  FreeAndNil(FSoft);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGTextAsClipPath.ClipWith(const aClip: String);

begin
  FreeAndNil(FDocument);
  FreeAndNil(FSoft);
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGString(Doc(
    '<clipPath id="c">' + aClip + '</clipPath>'
    + '<rect x="0" y="0" width="100" height="100" fill="black"'
    + ' clip-path="url(#c)"/>'));
  FRenderer.Render(FDocument, FSoft);
end;


function TTestSVGTextAsClipPath.CoveredArea: Double;

var
  X, Y: Integer;

begin
  Result := 0;
  for Y := 0 to FSoft.Image.Height - 1 do
    for X := 0 to FSoft.Image.Width - 1 do
      Result := Result + FSoft.Image.Colors[X, Y].Alpha / 65535;
end;


function TTestSVGTextAsClipPath.AlphaAt(aX, aY: Integer): Double;

begin
  Result := FSoft.Image.Colors[aX, aY].Alpha / 65535;
end;


procedure TTestSVGTextAsClipPath.TestGlyphsBecomeTheSilhouette;

begin
  ClipWith('<text x="0" y="20">ab</text>');
  AssertEquals('the rectangle is kept under a glyph', 1.0,
    AlphaAt(2, 15), 0.01);
end;


procedure TTestSVGTextAsClipPath.TestTheAreaIsThatOfTheGlyphs;

begin
  // Two glyphs, each five wide and eight tall at a font size of ten.
  ClipWith('<text x="0" y="20">ab</text>');
  AssertEquals('the silhouette is the two squares and nothing more', 80.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGTextAsClipPath.TestNothingOutsideTheGlyphsIsKept;

begin
  ClipWith('<text x="0" y="20">ab</text>');
  AssertEquals('above the glyphs nothing is kept', 0.0, AlphaAt(2, 5), 0.01);
  AssertEquals('below the baseline nothing is kept', 0.0,
    AlphaAt(2, 25), 0.01);
  AssertEquals('past the last glyph nothing is kept', 0.0,
    AlphaAt(15, 15), 0.01);
end;


procedure TTestSVGTextAsClipPath.TestASpaceLeavesAHole;

begin
  // The space has no outline, so it cuts a gap from five to ten.
  ClipWith('<text x="0" y="20">a b</text>');
  AssertEquals('the first glyph is kept', 1.0, AlphaAt(2, 15), 0.01);
  AssertEquals('the space keeps nothing', 0.0, AlphaAt(7, 15), 0.01);
  AssertEquals('the glyph after it is kept', 1.0, AlphaAt(12, 15), 0.01);
end;


procedure TTestSVGTextAsClipPath.TestATspanJoinsTheSilhouette;

begin
  ClipWith('<text x="0" y="20">a<tspan>b</tspan></text>');
  AssertEquals('the glyph of the text is kept', 1.0, AlphaAt(2, 15), 0.01);
  AssertEquals('and so is the glyph of the tspan', 1.0,
    AlphaAt(7, 15), 0.01);
end;


procedure TTestSVGTextAsClipPath.TestTheFillOfTheClipTextIsIgnored;

begin
  // A clip path is built from the geometry of its children alone, so the
  // paint of the text has no effect on what it cuts.
  ClipWith('<text x="0" y="20" fill="none">ab</text>');
  AssertEquals('a text painting nothing still cuts its glyphs', 80.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGTextAsClipPath.TestHiddenClipTextClipsNothingAway;

begin
  // A text that is not drawn adds no silhouette to the clip path, and a
  // clip path with no silhouette keeps nothing.
  ClipWith('<text x="0" y="20" visibility="hidden">ab</text>');
  AssertEquals('a text that is not drawn leaves the clip path empty',
    0.0, CoveredArea, 0.5);
end;


procedure TTestSVGTextAsClipPath.TestDisplayNoneClipTextDrawsNothing;

begin
  ClipWith('<text x="0" y="20" display="none">ab</text>');
  AssertEquals('a text that is not there leaves an empty silhouette', 0.0,
    CoveredArea, 0.5);
end;


procedure TTestSVGTextAsClipPath.TestAShapeAndATextJoinOneSilhouette;

begin
  ClipWith('<text x="0" y="20">a</text>'
    + '<rect x="40" y="40" width="10" height="10"/>');
  AssertEquals('the glyph is kept', 1.0, AlphaAt(2, 15), 0.01);
  AssertEquals('and the rectangle beside it', 1.0, AlphaAt(45, 45), 0.01);
  AssertEquals('together they cover both areas', 140.0, CoveredArea, 0.5);
end;


procedure TTestSVGTextAsClipPath.TestWithoutAProviderNothingIsKept;

begin
  FRenderer.Fonts := nil;
  ClipWith('<text x="0" y="20">ab</text>');
  AssertEquals('no font lays out no glyphs, and the silhouette is empty',
    0.0, CoveredArea, 0.5);
end;


// A document declaring one square-glyph font of its own, with the
// underline metrics the caller passes in.
function RuledDoc(const aMetrics: String): String;

begin
  Result := Doc(
    '<defs><font><font-face font-family="Ruled" units-per-em="1000"'
    + ' ascent="800" descent="-200"' + aMetrics + '/>'
    + '<glyph unicode="A" horiz-adv-x="500" d="M0 0H500V1000H0Z"/>'
    + '</font></defs>'
    + '<text x="0" y="40" font-family="Ruled" font-size="20"'
    + ' text-decoration="underline">A</text>');
end;


// A face whose lower case glyph is the capital, as a face of small
// capitals has it. It declares the variant only when the caller asks. At
// a size of twenty the glyph is sixteen tall above the baseline.
function CapsDoc(const aVariant: String): String;

begin
  Result := Doc(
    '<defs><font horiz-adv-x="500"><font-face font-family="Caps"'
    + ' units-per-em="1000" ascent="800" descent="-200"' + aVariant + '/>'
    + '<glyph unicode="a" d="M0 0H500V800H0Z"/>'
    + '<glyph unicode="A" d="M0 0H500V800H0Z"/>'
    + '</font></defs>'
    + '<text x="10" y="40" font-family="Caps" font-size="20"'
    + ' font-variant="small-caps">a</text>');
end;


// A face of one short glyph set down the page, with whatever pairs the
// caller writes for it. At a size of twenty the em is twenty, so the
// second glyph opens twenty below the first unless a pair closes it up.
function StackedDoc(const aPairs, aOrientation, aText: String): String;

begin
  Result := Doc(
    '<defs><font horiz-adv-x="500"><font-face font-family="Stack"'
    + ' units-per-em="1000" ascent="800" descent="-200"/>'
    + '<glyph unicode="A" d="M0 0H500V200H0Z"/>'
    + '<glyph unicode="&#x4E00;" d="M0 0H500V200H0Z"/>'
    + aPairs + '</font></defs>'
    + '<text x="40" y="10" font-family="Stack" font-size="20"'
    + ' writing-mode="tb"' + aOrientation + '>' + aText + '</text>');
end;


// The same face set down the page. At a size of twenty the em is twenty,
// so a column at forty has its lines at thirty and at fifty, and the
// glyph itself, five hundred of the em wide, runs from thirty five to
// forty five.
function RuledColumnDoc(const aLines: String): String;

begin
  Result := Doc(
    '<defs><font><font-face font-family="Ruled" units-per-em="1000"'
    + ' ascent="800" descent="-200" underline-thickness="100"/>'
    + '<glyph unicode="A" horiz-adv-x="500" d="M0 0H500V1000H0Z"/>'
    + '</font></defs>'
    + '<text x="40" y="10" font-family="Ruled" font-size="20"'
    + ' writing-mode="tb" glyph-orientation-vertical="0"'
    + ' text-decoration="' + aLines + '">A</text>');
end;


procedure TTestSVGTextRender.TestAFaceIsTakenAtItsWordAboutItsUnderline;

begin
  // Two hundred units of a thousand, at a size of twenty, puts the top of
  // the line four below the baseline at forty, and it is two thick.
  Draw(RuledDoc(' underline-position="-200" underline-thickness="100"'));
  AssertEquals('the line starts at the position the face gives', 1.0,
    AlphaAt(5, 44), 0.01);
  AssertEquals('and is as thick as it says', 1.0, AlphaAt(5, 45), 0.01);
  AssertEquals('nothing is drawn above it', 0.0, AlphaAt(5, 42), 0.01);
  AssertEquals('nor below it', 0.0, AlphaAt(5, 47), 0.01);
end;


procedure TTestSVGTextRender.TestAFaceThatSaysNothingLeavesTheLineToTheSize;

begin
  // When the face states nothing, the line sits just under the baseline,
  // at the fractions of the size used as a fallback.
  Draw(RuledDoc(''));
  // The fallback line is thinner than a pixel, so it is spread over the
  // row rather than filling it.
  AssertTrue('the line sits close under the baseline', AlphaAt(5, 41) > 0.5);
  AssertEquals('and not at the position the other face asks for', 0.0,
    AlphaAt(5, 45), 0.01);
end;


{ TTestSVGTextGoldens }

procedure TTestSVGTextGoldens.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
  FFonts := TSVGStubFontProvider.Create;
  FRenderer.Fonts := FFonts;
end;


procedure TTestSVGTextGoldens.TearDown;

begin
  FreeAndNil(FRenderer);
  FreeAndNil(FFonts);
  FreeAndNil(FTrace);
  FreeAndNil(FSoft);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGTextGoldens.AlphaText: TStringList;

var
  X, Y: Integer;
  lLine: String;
  lShades: String;
  lIndex: Integer;

begin
  lShades := ' .:-=+*#%@';
  Result := TStringList.Create;
  for Y := 0 to FSoft.Image.Height - 1 do
    begin
    lLine := '';
    for X := 0 to FSoft.Image.Width - 1 do
      begin
      lIndex := FSoft.Image.Colors[X, Y].Alpha div 257;
      if lIndex <= 0 then
        lLine := lLine + ' '
      else
        lLine := lLine + lShades[1 + Min(9, lIndex * 9 div 255)];
      end;
    Result.Add(TrimRight(lLine));
    end;
end;


procedure TTestSVGTextGoldens.TestTextWalkGolden;

begin
  FTrace := TSVGTraceBackend.Create;
  FDocument := ReadSVGFile(DataDir + 'text.svg');
  FRenderer.Render(FDocument, FTrace);
  AssertGolden(Self, 'text-walk', FTrace.Log);
end;


procedure TTestSVGTextGoldens.TestTextCoverageGolden;

var
  lLines, lBlock: TStringList;

begin
  FSoft := TSVGSoftBackend.Create;
  FDocument := ReadSVGFile(DataDir + 'text.svg');
  FRenderer.Render(FDocument, FSoft);
  lLines := TStringList.Create;
  try
    lLines.Add('# text.svg at 60 by 40, in the stub font');
    lBlock := AlphaText;
    try
      lLines.AddStrings(lBlock);
    finally
      lBlock.Free;
    end;
    AssertGolden(Self, 'text-coverage', lLines);
  finally
    lLines.Free;
  end;
end;


function TTestSVGTextLayout.LayoutOnPath(const aPath,
  aContent: String): Boolean;

begin
  Result := LayoutSource(
    '<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100" '
    + 'font-size="10"><defs><path id="p" d="' + aPath + '"/></defs>'
    + '<text>' + aContent + '</text></svg>');
end;


procedure TTestSVGTextLayout.TestTheBaselineSitsWhereItWasGivenByDefault;

begin
  LayoutSource(Doc('<text x="0" y="30">ab</text>'));
  AssertEquals('a document that asks for no baseline keeps the plain one',
    30.0, GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestMiddleRaisesTheTextByHalfALowerCaseLetter;

begin
  // The context puts the height of a lower case letter at 8 units, so the
  // baseline drops by 4 and the middle of the text lands on the point.
  LayoutSource(Doc('<text x="0" y="30" dominant-baseline="middle">ab</text>'));
  AssertEquals('the baseline moves down by half that height', 34.0,
    GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestCentralRaisesTheTextByHalfItsHeight;

begin
  // The stub face reaches 8 above the baseline and 2 below it.
  LayoutSource(Doc('<text x="0" y="30" dominant-baseline="central">ab</text>'));
  AssertEquals('the baseline moves down by half the height of the face',
    33.0, GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestHangingLetsTheTextHangBelowThePoint;

begin
  LayoutSource(Doc('<text x="0" y="30" dominant-baseline="hanging">ab</text>'));
  AssertTrue('the baseline moves down, so the text hangs under the point',
    GlyphAt(0).Y > 30);
  AssertEquals('by four fifths of the height above the baseline', 36.4,
    GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTheEdgeBaselinesPutTheBoxOnThePoint;

begin
  LayoutSource(Doc('<text x="0" y="30" '
    + 'dominant-baseline="text-before-edge">ab</text>'));
  AssertEquals('before-edge puts the top of the text on the point', 38.0,
    GlyphAt(0).Y, Delta);
  LayoutSource(Doc('<text x="0" y="30" '
    + 'dominant-baseline="text-after-edge">ab</text>'));
  AssertEquals('after-edge puts the bottom of it there', 28.0,
    GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTheShiftDoesNotReachTheNextCharacter;

begin
  LayoutSource(Doc('<text x="0" y="30" dominant-baseline="central">ab</text>'));
  AssertEquals('every character of the run is shifted the same',
    GlyphAt(0).Y, GlyphAt(1).Y, Delta);
  AssertEquals('and the shift moves nothing sideways', Advance,
    GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestAnUnknownBaselineKeywordChangesNothing;

begin
  LayoutSource(Doc('<text x="0" y="30" dominant-baseline="sideways">ab</text>'));
  AssertEquals('a keyword this does not draw differently is ignored',
    30.0, GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestACoordinateMayHaveAUnit;

begin
  // The context these tests lay out in puts the font size at 16 and the
  // viewport at 100 across, whatever the document says.
  LayoutSource(Doc('<text x="1em" y="2em">a</text>'));
  AssertEquals('the x is read as a length', 16.0, GlyphAt(0).X, Delta);
  AssertEquals('and so is the y', 32.0, GlyphAt(0).Y, Delta);
  LayoutSource(Doc('<text x="50%" y="10">a</text>'));
  AssertEquals('a percentage counts against the viewport', 50.0,
    GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestEveryEntryOfAListMayHaveItsOwn;

begin
  LayoutSource(Doc('<text x="0 1em 20" y="30">abc</text>'));
  AssertEquals('the first is a plain number', 0.0, GlyphAt(0).X, Delta);
  AssertEquals('the second is an em', 16.0, GlyphAt(1).X, Delta);
  AssertEquals('the third a plain number again', 20.0,
    GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestACoordinateWithAnUnknownUnitIsRefused;

begin
  // A list that cannot be read leaves the text where it would be without
  // a list at all.
  LayoutSource(Doc('<text x="5zz" y="30">a</text>'));
  AssertEquals('a unit that is not one leaves the list empty', 0.0,
    GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestAStyleNameGivesTheWeightOfAFace;

begin
  AssertEquals('Thin', 100, NamedWeight('Thin'));
  AssertEquals('ExtraLight', 200, NamedWeight('ExtraLight'));
  AssertEquals('Extra Light', 200, NamedWeight('Extra Light'));
  AssertEquals('Light', 300, NamedWeight('Light'));
  AssertEquals('Medium', 500, NamedWeight('Medium'));
  AssertEquals('SemiBold', 600, NamedWeight('SemiBold'));
  AssertEquals('Bold', 700, NamedWeight('Bold'));
  AssertEquals('ExtraBold', 800, NamedWeight('ExtraBold'));
  AssertEquals('Black', 900, NamedWeight('Black'));
  AssertEquals('Bold Oblique is bold', 700, NamedWeight('Bold Oblique'));
  AssertEquals('ExtraLight is not read as Light', 200,
    NamedWeight('ExtraLight Italic'));
  AssertEquals('ExtraBold is not read as Bold', 800,
    NamedWeight('ExtraBold Italic'));
end;


procedure TTestSVGTextLayout.TestAStyleNameThatNamesNoWeight;

begin
  AssertEquals('Book gives no weight', -1, NamedWeight('Book'));
  AssertEquals('nor does Regular', -1, NamedWeight('Regular'));
  AssertEquals('nor does an empty name', -1, NamedWeight(''));
  AssertEquals('nor does Italic on its own', -1, NamedWeight('Italic'));
end;


procedure TTestSVGTextLayout.TestAFaceNameGivesTheWidthOfAFace;

begin
  AssertEquals('Condensed', Ord(fsCondensed),
    NamedWidth('DejaVu Sans Condensed'));
  AssertEquals('Narrow', Ord(fsCondensed),
    NamedWidth('Liberation Sans Narrow Regular'));
  AssertEquals('SemiCondensed', Ord(fsSemiCondensed),
    NamedWidth('Roboto SemiCondensed'));
  AssertEquals('ExtraCondensed', Ord(fsExtraCondensed),
    NamedWidth('Roboto Extra Condensed'));
  AssertEquals('UltraCondensed', Ord(fsUltraCondensed),
    NamedWidth('Roboto UltraCondensed'));
  AssertEquals('Expanded', Ord(fsExpanded), NamedWidth('Roboto Expanded'));
  AssertEquals('UltraExpanded', Ord(fsUltraExpanded),
    NamedWidth('Roboto Ultra Expanded'));
end;


procedure TTestSVGTextLayout.TestAFaceNameThatNamesNoWidth;

begin
  AssertEquals('a plain family gives no width', -1,
    NamedWidth('DejaVu Sans Book'));
  AssertEquals('nor does one whose name gives only a weight', -1,
    NamedWidth('DejaVu Sans ExtraLight'));
end;


procedure TTestSVGTextLayout.TestSmallCapsDrawsALowerCaseLetterAsItsCapital;

begin
  LayoutSource(Doc('<text x="0" y="30" font-variant="small-caps">a</text>'));
  AssertEquals('the glyph drawn is the capital', Ord('A'),
    Integer(GlyphAt(0).GlyphID));
end;


procedure TTestSVGTextLayout.TestASmallCapitalIsNarrowerThanARealOne;

begin
  // The small capital is drawn at four fifths of the size, so it advances
  // four fifths as far: 4 rather than 5.
  LayoutSource(Doc('<text x="0" y="30" font-variant="small-caps">aa</text>'));
  AssertEquals('the second letter follows at the narrower advance',
    Advance * 0.8, GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestARealCapitalKeepsItsSize;

begin
  LayoutSource(Doc('<text x="0" y="30" font-variant="small-caps">Aa</text>'));
  AssertEquals('a letter already capital is drawn at the full size',
    Advance, GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestSmallCapsSplitsTheRunAtEveryChange;

begin
  // The two sizes are two faces, and a run holds one face.
  LayoutSource(Doc('<text x="0" y="30" font-variant="small-caps">aA</text>'));
  AssertEquals('the small capital and the capital are drawn apart', 2,
    FLayout.RunCount);
end;


procedure TTestSVGTextLayout.TestWithoutSmallCapsTheLetterIsLeftAlone;

begin
  LayoutSource(Doc('<text x="0" y="30">aa</text>'));
  AssertEquals('the lower case glyph is the one drawn', Ord('a'),
    Integer(GlyphAt(0).GlyphID));
  AssertEquals('at the full advance', Advance, GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestFontStretchReachesTheRequest;

begin
  LayoutSource(Doc('<text x="0" y="30" font-stretch="condensed">a</text>'));
  AssertEquals('the provider is asked for a condensed face',
    Ord(fsCondensed), Ord(FFonts.LastStretch));
end;


procedure TTestSVGTextLayout.TestFontStretchIsInheritedByAChild;

begin
  LayoutSource(Doc('<text x="0" y="30" font-stretch="expanded">'
    + '<tspan>a</tspan></text>'));
  AssertEquals('the tspan is asked for the width of the text element',
    Ord(fsExpanded), Ord(FFonts.LastStretch));
end;


procedure TTestSVGTextLayout.TestWiderStepsOneWidthFromTheParent;

begin
  LayoutSource(Doc('<text x="0" y="30" font-stretch="condensed">'
    + '<tspan font-stretch="wider">a</tspan></text>'));
  AssertEquals('one step wider than condensed is semi-condensed',
    Ord(fsSemiCondensed), Ord(FFonts.LastStretch));
end;


procedure TTestSVGTextLayout.TestNarrowerStopsAtTheNarrowestWidth;

begin
  LayoutSource(Doc('<text x="0" y="30" font-stretch="ultra-condensed">'
    + '<tspan font-stretch="narrower">a</tspan></text>'));
  AssertEquals('there is nothing narrower to step to',
    Ord(fsUltraCondensed), Ord(FFonts.LastStretch));
end;


procedure TTestSVGTextLayout.TestAChildKeepsItsOwnSpacesWhereTheTextCollapses;

begin
  // The text element collapses, the tspan does not, so both of its spaces
  // are drawn: a, two spaces, b.
  LayoutSource(Doc('<text x="0" y="30">a'
    + '<tspan xml:space="preserve">  b</tspan></text>'));
  AssertEquals('the tspan keeps every space it holds', 4, GlyphCount);
end;


procedure TTestSVGTextLayout.TestAChildCollapsesWhereTheTextKeepsItsSpaces;

begin
  // The text element keeps its one space, the tspan collapses its two
  // into one: a, the kept space, the collapsed space, b.
  LayoutSource(Doc('<text x="0" y="30" xml:space="preserve">a '
    + '<tspan xml:space="default">  b</tspan></text>'));
  AssertEquals('the tspan collapses its own spaces', 4, GlyphCount);
end;


// A document whose defs hold aSource, with aBody as the text drawn.
function TRefDoc(const aSource, aBody: String): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100" '
    + 'font-size="10"><defs>' + aSource + '</defs>'
    + '<text x="0" y="30">' + aBody + '</text></svg>';
end;


procedure TTestSVGTextLayout.TestATrefTakesTheTextItNames;

begin
  AssertTrue('the text lays out', LayoutSource(TRefDoc(
    '<text id="src">abc</text>', '<tref xlink:href="#src"/>')));
  AssertEquals('the three characters of the target are drawn', 3,
    GlyphCount);
  AssertEquals('starting at the position the text element gives', 0.0,
    GlyphAt(0).X, Delta);
  AssertEquals('and advancing as usual', Advance, GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestATrefTakesTheTextOfEverythingTheTargetHolds;

begin
  AssertTrue('the text lays out', LayoutSource(TRefDoc(
    '<text id="src">a<tspan>b</tspan>c</text>', '<tref xlink:href="#src"/>')));
  AssertEquals('the text of the target and of its children is taken', 3,
    GlyphCount);
end;


procedure TTestSVGTextLayout.TestATrefThatNamesNothingAddsNoText;

begin
  AssertFalse('a target that is not there gives no text', LayoutSource(
    TRefDoc('<text id="src">abc</text>', '<tref xlink:href="#gone"/>')));
end;


procedure TTestSVGTextLayout.TestATrefIsPlacedAndPaintedLikeATspan;

begin
  AssertTrue('the text lays out', LayoutSource(TRefDoc(
    '<text id="src">ab</text>',
    '<tref x="20" fill="#ff0000" xlink:href="#src"/>')));
  AssertEquals('the x of the tref places its first character', 20.0,
    GlyphAt(0).X, Delta);
  AssertEquals('and the run takes the fill of the tref',
    Integer(TSVGColor.FromBytes($FF, 0, 0, 255).Red),
    Integer(FLayout.Runs[0].Style.Fill.Color.Red));
end;


procedure TTestSVGTextLayout.TestATrefThatNamesItselfAddsNoText;

begin
  AssertFalse('a tref pointing to itself gives no text', LayoutSource(
    '<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100" '
    + 'font-size="10"><text x="0" y="30">'
    + '<tref id="me" xlink:href="#me"/></text></svg>'));
end;


procedure TTestSVGTextLayout.TestRotateTurnsEachCharacterByItsOwnAngle;

begin
  LayoutSource(Doc('<text x="0" y="30" rotate="10,20,30">abc</text>'));
  AssertEquals('the first character takes the first angle', 10.0,
    GlyphAt(0).Angle, Delta);
  AssertEquals('the second the second', 20.0, GlyphAt(1).Angle, Delta);
  AssertEquals('the third the third', 30.0, GlyphAt(2).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestTheLastAngleTurnsEveryCharacterAfterIt;

begin
  LayoutSource(Doc('<text x="0" y="30" rotate="10,20">abcd</text>'));
  AssertEquals('the second character takes the last angle given', 20.0,
    GlyphAt(1).Angle, Delta);
  AssertEquals('and so does the third', 20.0, GlyphAt(2).Angle, Delta);
  AssertEquals('and the fourth', 20.0, GlyphAt(3).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestAnInnerListTurnsTheCharactersOfItsOwnElement;

begin
  // The list of the tspan counts from the first character of the tspan.
  // The c is back in the text element, whose one angle covers it.
  LayoutSource(Doc('<text x="0" y="30" rotate="10">'
    + '<tspan rotate="90,80">ab</tspan>c</text>'));
  AssertEquals('the tspan turns its first character', 90.0,
    GlyphAt(0).Angle, Delta);
  AssertEquals('and its second', 80.0, GlyphAt(1).Angle, Delta);
  AssertEquals('the text after the tspan is turned by the text element',
    10.0, GlyphAt(2).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestAnElementWithoutAListFollowsTheOneAbove;

begin
  // The tspan gives no angle, so the list of the text element applies,
  // counted over its own characters and not over those of the tspan.
  LayoutSource(Doc('<text x="0" y="30" rotate="10,20,30,40">'
    + '<tspan>ab</tspan>cd</text>'));
  AssertEquals('the first character of the tspan takes the first angle',
    10.0, GlyphAt(0).Angle, Delta);
  AssertEquals('the second the second', 20.0, GlyphAt(1).Angle, Delta);
  AssertEquals('and the count runs on after the tspan', 30.0,
    GlyphAt(2).Angle, Delta);
  AssertEquals('to the last', 40.0, GlyphAt(3).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestRotateTurnsOnTopOfWhatThePathTurns;

begin
  // The path is straight and rotates nothing, so the only angle left is
  // the one rotate specifies.
  AssertTrue('the text lays out', LayoutOnPath('M 0 50 L 200 50',
    '<textPath xlink:href="#p" rotate="25">ab</textPath>'));
  AssertEquals('the glyph has the angle rotate gave it', 25.0,
    GlyphAt(0).Angle, Delta);
  AssertEquals('and so does the next', 25.0, GlyphAt(1).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestRotateMovesNothingSideways;

begin
  LayoutSource(Doc('<text x="0" y="30" rotate="45,45,45">abc</text>'));
  AssertEquals('the characters still advance along the baseline',
    Advance, GlyphAt(1).X, Delta);
  AssertEquals('and stay on it', 30.0, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTheSpaceIsSpacedByTheElementItWasReadIn;

begin
  // The whitespace belongs to the text element, so the space takes its
  // word-spacing of 4 and not the 20 of the tspan it is added in.
  // a and b advance 5 each, the space 5 and 4 more, so c starts at 19.
  LayoutSource(Doc('<text x="0" y="30" word-spacing="4">ab '
    + '<tspan word-spacing="20">cd</tspan></text>'));
  AssertEquals('the space is spaced by the text element', 19.0,
    GlyphAt(3).X, Delta);
end;


procedure TTestSVGTextLayout.TestTheSpaceDoesNotTakeTheNextElementsPosition;

begin
  // The x of the tspan addresses the characters of the tspan. The space
  // is not one of them, so it stays in the flow and the c takes the 50.
  LayoutSource(Doc('<text x="0" y="30">ab '
    + '<tspan x="50">cd</tspan></text>'));
  AssertEquals('the space follows the b', 2 * Advance, GlyphAt(2).X, Delta);
  AssertEquals('and the c starts at the position the tspan gives', 50.0,
    GlyphAt(3).X, Delta);
end;


procedure TTestSVGTextLayout.TestTheChunkIsAnchoredByTheElementThatOpensIt;

begin
  // The c opens a chunk, so the anchor of the tspan applies to it: cd is
  // 10 wide and ends on the 50 rather than starting there.
  LayoutSource(Doc('<text x="0" y="30">ab '
    + '<tspan x="50" text-anchor="end">cd</tspan></text>'));
  AssertEquals('the chunk ends at the position the tspan gives', 40.0,
    GlyphAt(3).X, Delta);
  AssertEquals('and the space before it did not move', 2 * Advance,
    GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestTheSpaceBeforeALineStaysWithTheParent;

begin
  // The space between the two tspans is text of the text element. It is
  // only added once the c follows it, which must not put it in the line
  // the second tspan declared.
  LayoutSource(Doc('<text x="0" y="30" text-decoration="underline">'
    + '<tspan>ab</tspan> <tspan text-decoration="underline">cd</tspan>'
    + '</text>'));
  AssertEquals('the text and the tspan each drew a line', 2,
    FLayout.DecorationCount);
  AssertEquals('the line of the text starts at the first character', 0.0,
    FLayout.Decorations[0].Left, Delta);
  AssertEquals('and runs to the last', 5 * Advance,
    FLayout.Decorations[0].Right, Delta);
  AssertEquals('the line of the tspan starts at the c, not at the space',
    3 * Advance, FLayout.Decorations[1].Left, Delta);
  AssertEquals('and ends with the d', 5 * Advance,
    FLayout.Decorations[1].Right, Delta);
end;


procedure TTestSVGTextLayout.TestALineCoversTheSpacesInsideItsOwnElement;

begin
  LayoutSource(Doc('<text x="0" y="30">'
    + '<tspan text-decoration="underline">a b</tspan></text>'));
  AssertEquals('one line is drawn', 1, FLayout.DecorationCount);
  AssertEquals('it starts at the a', 0.0,
    FLayout.Decorations[0].Left, Delta);
  AssertEquals('and crosses the space to the b', 3 * Advance,
    FLayout.Decorations[0].Right, Delta);
end;


procedure TTestSVGTextLayout.TestSuperLiftsTheTextOffTheBaseline;

begin
  // The stub face is one size tall, so super lifts by half of that.
  LayoutSource(Doc('<text x="0" y="30">a'
    + '<tspan baseline-shift="super">b</tspan></text>'));
  AssertEquals('the text before the shift stays on the baseline', 30.0,
    GlyphAt(0).Y, Delta);
  AssertEquals('the shifted text sits above it', 25.0, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestSubDropsTheTextBelowIt;

begin
  // sub drops by the same amount that super lifts.
  LayoutSource(Doc('<text x="0" y="30">a'
    + '<tspan baseline-shift="sub">b</tspan></text>'));
  AssertEquals('the shifted text sits below the baseline', 35.0,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestALengthLiftsTheTextByThatMuch;

begin
  LayoutSource(Doc('<text x="0" y="30">a'
    + '<tspan baseline-shift="3">b</tspan></text>'));
  AssertEquals('a positive length moves the baseline up the page', 27.0,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAPercentageCountsAgainstTheFontSize;

begin
  // The font size is 10, so -70% is 7 units, and the minus moves it down.
  LayoutSource(Doc('<text x="0" y="30">a'
    + '<tspan baseline-shift="-70%">b</tspan></text>'));
  AssertEquals('the percentage is read against the size, not the viewport',
    37.0, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAShiftInsideAShiftAddsUp;

begin
  LayoutSource(Doc('<text x="0" y="30">a<tspan baseline-shift="super">b'
    + '<tspan baseline-shift="super">c</tspan></tspan></text>'));
  AssertEquals('the first shift lifts by five units', 25.0,
    GlyphAt(1).Y, Delta);
  AssertEquals('and the second lifts from there, not from the baseline',
    20.0, GlyphAt(2).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAChildOfAShiftedElementDoesNotShiftAgain;

begin
  LayoutSource(Doc('<text x="0" y="30"><tspan baseline-shift="super">a'
    + '<tspan>b</tspan></tspan></text>'));
  AssertEquals('the child sits on the baseline its parent moved to',
    GlyphAt(0).Y, GlyphAt(1).Y, Delta);
  AssertEquals('which is five units up', 25.0, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTheBaselineKeywordShiftsNothingFurther;

begin
  LayoutSource(Doc('<text x="0" y="30"><tspan baseline-shift="super">a'
    + '<tspan baseline-shift="baseline">b</tspan></tspan></text>'));
  AssertEquals('baseline adds no shift of its own', 25.0,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAShiftMovesNothingSideways;

begin
  LayoutSource(Doc('<text x="0" y="30">a'
    + '<tspan baseline-shift="super">b</tspan>c</text>'));
  AssertEquals('the second character follows the first', Advance,
    GlyphAt(1).X, Delta);
  AssertEquals('and the third follows the shifted one', 2 * Advance,
    GlyphAt(2).X, Delta);
  AssertEquals('which is back on the baseline', 30.0, GlyphAt(2).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTextPathLaysTheGlyphsAlongThePath;

begin
  AssertTrue('the text lays out', LayoutOnPath('M 0 50 L 200 50',
    '<textPath xlink:href="#p">abc</textPath>'));
  AssertEquals('three characters give three glyphs', 3, GlyphCount);
  AssertEquals('the first starts at the start of the path', 0.0,
    GlyphAt(0).X, Delta);
  AssertEquals('and sits on it', 50.0, GlyphAt(0).Y, Delta);
  AssertEquals('a straight path turns nothing', 0.0, GlyphAt(0).Angle, Delta);
  AssertEquals('the second is one advance along', Advance, GlyphAt(1).X,
    Delta);
  AssertEquals('the third two advances along', 2 * Advance, GlyphAt(2).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestTextPathStartOffsetMovesTheTextAlong;

begin
  LayoutOnPath('M 0 50 L 200 50',
    '<textPath xlink:href="#p" startOffset="10">abc</textPath>');
  AssertEquals('the text starts a startOffset along the path', 10.0,
    GlyphAt(0).X, Delta);
  AssertEquals('and runs on from there', 10.0 + Advance, GlyphAt(1).X, Delta);
end;


procedure TTestSVGTextLayout.TestTextPathStartOffsetTakesAShareOfTheLength;

begin
  LayoutOnPath('M 0 50 L 200 50',
    '<textPath xlink:href="#p" startOffset="25%">abc</textPath>');
  AssertEquals('a percentage measures the length of the path', 50.0,
    GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestTextPathTurnsTheGlyphsWithTheTangent;

begin
  LayoutOnPath('M 50 0 L 50 200',
    '<textPath xlink:href="#p">abc</textPath>');
  AssertEquals('a path running down turns the glyphs a quarter turn', 90.0,
    GlyphAt(0).Angle, Delta);
  AssertEquals('the first glyph starts at the start of the path', 50.0,
    GlyphAt(0).X, Delta);
  AssertEquals('with nothing left over along the path', 0.0, GlyphAt(0).Y,
    Delta);
  AssertEquals('the second advances down rather than across', 50.0,
    GlyphAt(1).X, Delta);
  AssertEquals('by one advance', Advance, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTextPathDyLiftsTheGlyphsOffThePath;

begin
  LayoutOnPath('M 0 50 L 200 50',
    '<textPath xlink:href="#p" dy="-4">abc</textPath>');
  AssertEquals('a dy moves the glyph off the path along the normal', 46.0,
    GlyphAt(0).Y, Delta);
  AssertEquals('and the ones that follow it keep the shift', 46.0,
    GlyphAt(2).Y, Delta);
  AssertEquals('without moving them along the path', 0.0, GlyphAt(0).X,
    Delta);
end;


procedure TTestSVGTextLayout.TestGlyphsPastTheEndOfThePathAreDropped;

begin
  LayoutOnPath('M 0 50 L 12 50',
    '<textPath xlink:href="#p">abcd</textPath>');
  AssertEquals('only the glyphs whose middle falls on the path are drawn',
    2, GlyphCount);
end;


procedure TTestSVGTextLayout.TestTextPathNamingAShapeDrawsNothing;

begin
  LayoutSource('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100" '
    + 'font-size="10"><defs><rect id="p" width="200" height="10"/></defs>'
    + '<text><textPath xlink:href="#p">abc</textPath></text></svg>');
  AssertEquals('a textPath pointing to something other than a path draws nothing',
    0, GlyphCount);
end;


procedure TTestSVGTextLayout.TestTextPathAnchorsAgainstTheOffset;

begin
  LayoutOnPath('M 0 50 L 200 50',
    '<textPath xlink:href="#p" startOffset="100" text-anchor="middle">'
    + 'abcd</textPath>');
  AssertEquals('four glyphs spanning two sizes centre on the offset', 4,
    GlyphCount);
  AssertEquals('so the first sits half the run before it', 90.0,
    GlyphAt(0).X, Delta);
  AssertEquals('and the last one advance before the end', 105.0,
    GlyphAt(3).X, Delta);
end;


procedure TTestSVGTextLayout.TestACoveredCharacterKeepsTheWantedFace;

begin
  FFonts.Limit := 255;
  FFonts.CoverName := 'wide';
  LayoutSource(Doc('<text>abc</text>'));
  AssertEquals('text the face covers stays in one run', 1,
    FLayout.RunCount);
  AssertEquals('drawn by the face that was wanted', 'stub',
    FLayout.Runs[0].Font.GetFontName);
  AssertEquals('and no cover was ever asked for', 0, FFonts.Covered);
end;


procedure TTestSVGTextLayout.TestAnUncoveredCharacterTakesTheCoveringFace;

begin
  FFonts.Limit := 255;
  FFonts.CoverName := 'wide';
  LayoutSource(Doc('<text>' + TextOf(#$E6#$97#$A5) + '</text>'));
  AssertEquals('one character gives one run', 1, FLayout.RunCount);
  AssertEquals('drawn by the face that covers it', 'wide',
    FLayout.Runs[0].Font.GetFontName);
  AssertEquals('which was asked for once', 1, FFonts.Covered);
end;


procedure TTestSVGTextLayout.TestTheRunSplitsWhereTheFaceChanges;

begin
  FFonts.Limit := 255;
  FFonts.CoverName := 'wide';
  LayoutSource(Doc('<text>a' + TextOf(#$E6#$97#$A5) + 'b</text>'));
  AssertEquals('the line breaks into a run per face', 3, FLayout.RunCount);
  AssertEquals('the first is the wanted face', 'stub',
    FLayout.Runs[0].Font.GetFontName);
  AssertEquals('the second the covering one', 'wide',
    FLayout.Runs[1].Font.GetFontName);
  AssertEquals('and the third the wanted face again', 'stub',
    FLayout.Runs[2].Font.GetFontName);
  AssertEquals('every glyph is still drawn', 3, GlyphCount);
end;


procedure TTestSVGTextLayout.TestWithoutACoverTheWantedFaceDrawsItAnyway;

begin
  FFonts.Limit := 255;
  LayoutSource(Doc('<text>a' + TextOf(#$E6#$97#$A5) + 'b</text>'));
  AssertEquals('nothing covers it, so the line stays one run', 1,
    FLayout.RunCount);
  AssertEquals('and the character still takes its place', 3, GlyphCount);
  AssertEquals('drawn as the notdef of the wanted face', 0,
    Integer(GlyphAt(1).GlyphID));
end;


procedure TTestSVGTextLayout.TestASpaceIsNeverLookedUp;

begin
  // A face with no glyph for a space still spaces the line. Asking another
  // face to draw nothing would only split the run.
  FFonts.Limit := 1;
  FFonts.CoverName := 'wide';
  LayoutSource(Doc('<text xml:space="preserve"> </text>'));
  AssertEquals('a space asks for no cover', 0, FFonts.Covered);
end;


procedure TTestSVGTextLayout.TestTheCoveringFaceMeasuresItsOwnAdvance;

begin
  FFonts.Limit := 255;
  FFonts.CoverName := 'wide';
  LayoutSource(Doc('<text>a' + TextOf(#$E6#$97#$A5) + '</text>'));
  AssertEquals('the second glyph follows the first by its advance',
    Advance, GlyphAt(1).X, Delta);
end;


{ TTestSVGFontRegistry }

procedure TTestSVGFontRegistry.SetUp;

begin
  inherited SetUp;
  FProvider := TSVGFreeTypeProvider.Create;
end;


procedure TTestSVGFontRegistry.TearDown;

begin
  FreeAndNil(FProvider);
  inherited TearDown;
end;


procedure TTestSVGFontRegistry.TestProviderConstructsWithoutFonts;

begin
  AssertEquals('a fresh provider knows no font files', 0,
    FProvider.EntryCount);
end;


procedure TTestSVGFontRegistry.TestMissingFileIsRefused;

begin
  AssertFalse('a file that is not there is not recorded',
    FProvider.AddFontFile('nosuchfont.ttf'));
end;


procedure TTestSVGFontRegistry.TestMissingDirectoryAddsNothing;

begin
  AssertEquals('a directory that is not there adds nothing', 0,
    FProvider.AddFontPath('nosuchdirectory'));
end;


procedure TTestSVGFontRegistry.TestEmptyRegistryResolvesNothing;

begin
  AssertTrue('with no fonts recorded nothing resolves',
    FProvider.ResolveFont(TSVGFontRequest.Create('serif', 12)) = nil);
end;


procedure TTestSVGFontRegistry.TestSystemFontPathsAreNamed;

begin
  AssertTrue('the platform gives a directory to look for fonts in',
    SVGSystemFontPaths <> '');
end;


procedure TTestSVGFontRegistry.TestARecordedFontOpensWhenItIsAskedFor;

var
  lFont: ISVGFont;

begin
  // Registering a font records the values of the face and then closes it.
  // This test takes the path that opens it again. A machine with no fonts
  // installed has nothing to check.
  if FProvider.AddSystemFonts = 0 then
    Exit;
  lFont := FProvider.ResolveFont(
    TSVGFontRequest.Create(FProvider.Entries[0].Family, 20));
  AssertTrue('the recorded family resolves to a face', lFont <> nil);
  AssertTrue('the face reopened and knows its design units',
    lFont.GetUnitsPerEm > 0);
  AssertTrue('and measures a glyph through it',
    lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('A'))) > 0);
end;


procedure TTestSVGFontRegistry.TestASystemFaceIsNotKernedUnlessItIsAskedFor;

var
  lFont: ISVGFont;
  lRequest: TSVGFontRequest;
  lKerned: Double;

begin
  // A machine with no fonts installed has nothing to check, and neither
  // has one whose faces have no kern table of the old kind.
  if FProvider.AddSystemFonts = 0 then
    Exit;
  lRequest := TSVGFontRequest.Create('Liberation Sans', 1000);
  FProvider.Kerning := True;
  lFont := FProvider.ResolveFont(lRequest);
  if lFont = nil then
    Exit;
  lKerned := lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('A')),
    lFont.GetGlyphIndex(Ord('V')));
  if lKerned = 0 then
    Exit;
  FProvider.Kerning := False;
  lFont := FProvider.ResolveFont(lRequest);
  AssertEquals('the pair is left alone when the table is not asked for',
    0.0, lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('A')),
      lFont.GetGlyphIndex(Ord('V'))), 1e-9);
end;


procedure TTestSVGFontRegistry.TestTheGenericFamilyNamesAreRecognised;

begin
  AssertTrue('serif is generic', SVGGenericFamilyOf('serif') = gfSerif);
  AssertTrue('sans-serif is generic',
    SVGGenericFamilyOf('sans-serif') = gfSansSerif);
  AssertTrue('cursive is generic', SVGGenericFamilyOf('cursive') = gfCursive);
  AssertTrue('fantasy is generic', SVGGenericFamilyOf('fantasy') = gfFantasy);
  AssertTrue('monospace is generic',
    SVGGenericFamilyOf('monospace') = gfMonospace);
  AssertTrue('the names are not case sensitive',
    SVGGenericFamilyOf('Sans-Serif') = gfSansSerif);
  AssertTrue('and the space a list leaves behind does not count',
    SVGGenericFamilyOf('  monospace ') = gfMonospace);
end;


procedure TTestSVGFontRegistry.TestARealFamilyIsNotAGenericOne;

begin
  AssertTrue('a family of its own is not generic',
    SVGGenericFamilyOf('DejaVu Sans') = gfNone);
  AssertTrue('and neither is nothing', SVGGenericFamilyOf('') = gfNone);
end;


procedure TTestSVGFontRegistry.TestAGenericFamilyCanBeChosenByHand;

begin
  FProvider.GenericFamily[gfSerif] := 'A Family Of My Own';
  AssertEquals('a generic name resolves to the family it was given',
    'A Family Of My Own', FProvider.GenericFamily[gfSerif]);
end;


procedure TTestSVGFontRegistry.TestAGenericFamilyResolvesToARecordedOne;

var
  lFont: ISVGFont;

begin
  // A machine with no fonts installed has nothing to resolve against.
  if FProvider.AddSystemFonts = 0 then
    Exit;
  AssertTrue('a system with fonts has a family for sans-serif',
    FProvider.GenericFamily[gfSansSerif] <> '');
  lFont := FProvider.ResolveFont(
    TSVGFontRequest.Create('No Such Family, sans-serif', 20));
  AssertTrue('a list ending in a generic name resolves', lFont <> nil);
  AssertEquals('to the family of that generic name',
    FProvider.GenericFamily[gfSansSerif], lFont.GetFontName);
end;


procedure TTestSVGFontRegistry.TestAWellKnownFamilyIsClassified;

begin
  AssertTrue('Georgia is a serif', SVGFamilyKindOf('Georgia') = gfSerif);
  AssertTrue('Times New Roman is a serif',
    SVGFamilyKindOf('Times New Roman') = gfSerif);
  AssertTrue('Arial is a sans', SVGFamilyKindOf('Arial') = gfSansSerif);
  AssertTrue('Verdana is a sans', SVGFamilyKindOf('Verdana') = gfSansSerif);
  AssertTrue('Courier New is a monospace',
    SVGFamilyKindOf('Courier New') = gfMonospace);
  AssertTrue('the names are not case sensitive',
    SVGFamilyKindOf('GEORGIA') = gfSerif);
  AssertTrue('a family this knows nothing of is not classified',
    SVGFamilyKindOf('Nothing Named This') = gfNone);
end;


procedure TTestSVGFontRegistry.TestAFamilyNothingKnowsOfFallsToSansSerif;

var
  lFont: ISVGFont;

begin
  // A machine with no fonts installed has nothing to resolve against.
  if FProvider.AddSystemFonts = 0 then
    Exit;
  lFont := FProvider.ResolveFont(
    TSVGFontRequest.Create('Nothing Named This', 20));
  AssertTrue('a family the system does not have still resolves', lFont <> nil);
  AssertEquals('to the face used for sans-serif',
    FProvider.GenericFamily[gfSansSerif], lFont.GetFontName);
end;


// Copies a file, so that a face of the system can be put where a test
// wants it, under any name the test chooses.
procedure CopyOnto(const aFrom, aTo: String);

var
  lIn, lOut: TFileStream;

begin
  lIn := TFileStream.Create(aFrom, fmOpenRead or fmShareDenyWrite);
  try
    lOut := TFileStream.Create(aTo, fmCreate);
    try
      lOut.CopyFrom(lIn, lIn.Size);
    finally
      lOut.Free;
    end;
  finally
    lIn.Free;
  end;
end;


// A directory holding one face of the system under each of the names, and
// nothing else. Empty when the machine has no face to copy.
function DirectoryOfCopies(aProvider: TSVGFreeTypeProvider;
  const aNames: array of String): String;

var
  I: Integer;
  lSource: String;

begin
  Result := '';
  aProvider.AddSystemFonts;
  lSource := '';
  for I := 0 to aProvider.EntryCount - 1 do
    if LowerCase(ExtractFileExt(aProvider.Entries[I].FileName)) = '.ttf' then
      begin
      lSource := aProvider.Entries[I].FileName;
      Break;
      end;
  if lSource = '' then
    Exit;
  Result := IncludeTrailingPathDelimiter(GetTempDir) + 'fpsvgfaces';
  ForceDirectories(Result);
  Result := IncludeTrailingPathDelimiter(Result);
  for I := Low(aNames) to High(aNames) do
    CopyOnto(lSource, Result + aNames[I]);
end;


// Deletes the copies again and removes the directory.
procedure ClearDirectory(const aDirectory: String;
  const aNames: array of String);

var
  I: Integer;

begin
  if aDirectory = '' then
    Exit;
  for I := Low(aNames) to High(aNames) do
    DeleteFile(aDirectory + aNames[I]);
  RemoveDir(ExcludeTrailingPathDelimiter(aDirectory));
end;


// Whether a family is among the registered faces. A machine without it
// cannot run the tests below.
function Records(aProvider: TSVGFreeTypeProvider;
  const aFamily: String): Boolean;

var
  I: Integer;

begin
  Result := False;
  for I := 0 to aProvider.EntryCount - 1 do
    if SameText(aProvider.Entries[I].Family, aFamily) then
      Exit(True);
end;


procedure TTestSVGFontRegistry.TestACoverComesFromTheFamiliesTheDocumentNamed;

var
  lRequest: TSVGFontRequest;
  lFont: ISVGFont;

begin
  // No coverage source is given, so only the families of the request can
  // answer. A no-break space is a character the first family here does not
  // have and the second one does.
  if FProvider.AddSystemFonts = 0 then
    Exit;
  if not Records(FProvider, 'Liberation Sans') then
    Exit;
  lRequest := TSVGFontRequest.Create('NoFamilyOfThisName, Liberation Sans', 40);
  lFont := FProvider.ResolveCover($00A0, lRequest);
  AssertTrue('the second family of the list covers it', lFont <> nil);
  AssertEquals('and it is the one that resolves', 'Liberation Sans',
    lFont.GetFontName);
end;


procedure TTestSVGFontRegistry.TestACoverNoNamedFamilyHoldsIsLeftToTheSearch;

var
  lRequest: TSVGFontRequest;

begin
  // Without a coverage source there is nothing beyond the families of the
  // request, so a code point that none of them has is not answered at
  // all.
  if FProvider.AddSystemFonts = 0 then
    Exit;
  if not Records(FProvider, 'Liberation Sans') then
    Exit;
  lRequest := TSVGFontRequest.Create('NoFamilyOfThisName, Liberation Sans', 40);
  AssertTrue('a code point that no family of the list has is left to the search',
    FProvider.ResolveCover($10FFFD, lRequest) = nil);
end;


{ TTestSVGPlatformCoverage }

procedure TTestSVGPlatformCoverage.SetUp;

begin
  inherited SetUp;
  FProvider := TSVGFreeTypeProvider.Create;
end;


procedure TTestSVGPlatformCoverage.TearDown;

begin
  FreeAndNil(FProvider);
  inherited TearDown;
end;


procedure TTestSVGPlatformCoverage.TestTheSelectedSourceIsTheOneOfTheBuildPlatform;

begin
  AssertEquals('macOS, Windows and Unix each have a source',
    {$IF defined(DARWIN) or defined(WINDOWS) or defined(UNIX)}
    True
    {$ELSE}
    False
    {$ENDIF}, SVGHasPlatformCoverage);
  AssertEquals('and the name says which one it is',
    {$IF defined(DARWIN)}
    'Core Text'
    {$ELSEIF defined(WINDOWS)}
    'the GDI font tables'
    {$ELSEIF defined(UNIX)}
    'fontconfig'
    {$ELSE}
    ''
    {$ENDIF}, SVGPlatformCoverageName);
end;


procedure TTestSVGPlatformCoverage.TestTheSourceIsNamedWhenThereIsOne;

begin
  AssertEquals('a build without a source names none',
    SVGHasPlatformCoverage, SVGPlatformCoverageName <> '');
end;


procedure TTestSVGPlatformCoverage.TestTheSameSourceComesBackEveryTime;

begin
  // The unit owns the source, so asking twice gives the one object.
  AssertTrue('the source is created once',
    SVGPlatformCoverage = SVGPlatformCoverage);
end;


procedure TTestSVGPlatformCoverage.TestItCanBePluggedIntoTheProvider;

begin
  // A build whose system cannot answer selects nothing, and a provider
  // takes that as well, so a program needs no guard around the
  // assignment.
  FProvider.Coverage := SVGPlatformCoverage;
  AssertEquals('the source plugs in', SVGPlatformCoverage <> nil,
    FProvider.Coverage <> nil);
  FProvider.Coverage := nil;
  AssertTrue('and can be taken out again', FProvider.Coverage = nil);
end;


{ TTestSVGVerticalMetrics }

const
  Han = $6211;
  VerticalSize = 32;
  MetricDelta = 1e-6;

procedure TTestSVGVerticalMetrics.SetUp;

begin
  inherited SetUp;
  FProvider := TSVGFreeTypeProvider.Create;
  if FProvider.Available then
    FProvider.AddSystemFonts;
end;


procedure TTestSVGVerticalMetrics.TearDown;

begin
  FreeAndNil(FProvider);
  inherited TearDown;
end;


// The first of the usual CJK families for which the machine returns a
// glyph for the character. A family the machine lacks falls back to one it
// has, which has no glyph for a Han character, so the check guards
// itself.
function TTestSVGVerticalMetrics.FaceHolding(aCode: Cardinal): ISVGFont;

const
  Families: array[0..7] of String = (
    'Noto Sans CJK JP', 'Noto Sans CJK SC', 'Noto Serif CJK JP',
    'Source Han Sans', 'Droid Sans Fallback', 'MS Gothic',
    'Hiragino Sans', 'SimSun');

var
  I: Integer;
  lFont: ISVGFont;

begin
  Result := nil;
  if not FProvider.Available then
    Exit;
  for I := Low(Families) to High(Families) do
    begin
    lFont := FProvider.ResolveFont(
      TSVGFontRequest.Create(Families[I], VerticalSize));
    if (lFont <> nil) and (lFont.GetGlyphIndex(aCode) <> 0) then
      Exit(lFont);
    end;
end;


procedure TTestSVGVerticalMetrics.TestAFullWidthGlyphMovesThePenItsOwnWidthDown;

var
  lFont: ISVGFont;
  lGlyph: Cardinal;

begin
  lFont := FaceHolding(Han);
  if lFont = nil then
    begin
    Ignore('no face here holds a Han character');
    Exit;
    end;
  lGlyph := lFont.GetGlyphIndex(Han);
  AssertEquals('the glyph is a full width one', VerticalSize,
    lFont.GetGlyphAdvance(lGlyph), 0.01);
  AssertEquals('so the pen moves down by what it moves across',
    lFont.GetGlyphAdvance(lGlyph),
    lFont.GetGlyphVerticalAdvance(lGlyph), MetricDelta);
end;


procedure TTestSVGVerticalMetrics.TestAFullWidthGlyphStandsBackHalfOfItself;

var
  lFont: ISVGFont;
  lGlyph: Cardinal;
  lX, lY: Double;

begin
  lFont := FaceHolding(Han);
  if lFont = nil then
    begin
    Ignore('no face here holds a Han character');
    Exit;
    end;
  lGlyph := lFont.GetGlyphIndex(Han);
  lFont.GetGlyphVerticalOrigin(lGlyph, lX, lY);
  AssertEquals('the glyph is centred on the baseline of the column',
    -lFont.GetGlyphAdvance(lGlyph) / 2, lX, 0.01);
end;


procedure TTestSVGVerticalMetrics.TestANarrowGlyphStandsBackHalfOfItsOwnWidth;

var
  lFont: ISVGFont;
  lGlyph: Cardinal;
  lX, lY: Double;

begin
  lFont := FaceHolding(Han);
  if lFont = nil then
    begin
    Ignore('no face here holds a Han character');
    Exit;
    end;
  lGlyph := lFont.GetGlyphIndex(Ord('A'));
  if lGlyph = 0 then
    begin
    Ignore('the face holding the Han character holds no Latin one');
    Exit;
    end;
  lFont.GetGlyphVerticalOrigin(lGlyph, lX, lY);
  AssertTrue('a Latin glyph is narrower than a full width one',
    lFont.GetGlyphAdvance(lGlyph) < lFont.GetGlyphAdvance(
      lFont.GetGlyphIndex(Han)));
  AssertEquals('and is centred on half of its own width, not of the em',
    -lFont.GetGlyphAdvance(lGlyph) / 2, lX, 0.01);
end;


procedure TTestSVGVerticalMetrics.TestAMixedColumnHangsFromOneBaseline;

var
  lFont: ISVGFont;
  lWide, lNarrow: Cardinal;
  lWideX, lWideY, lNarrowX, lNarrowY: Double;

begin
  lFont := FaceHolding(Han);
  if lFont = nil then
    begin
    Ignore('no face here holds a Han character');
    Exit;
    end;
  lNarrow := lFont.GetGlyphIndex(Ord('A'));
  if lNarrow = 0 then
    begin
    Ignore('the face holding the Han character holds no Latin one');
    Exit;
    end;
  lWide := lFont.GetGlyphIndex(Han);
  lFont.GetGlyphVerticalOrigin(lWide, lWideX, lWideY);
  lFont.GetGlyphVerticalOrigin(lNarrow, lNarrowX, lNarrowY);
  AssertEquals('both hang the same way down from the pen',
    lWideY, lNarrowY, MetricDelta);
end;


procedure TTestSVGVerticalMetrics.TestEveryGlyphMovesThePenTheSameWayDown;

var
  lFont: ISVGFont;
  lWide, lNarrow: Cardinal;

begin
  lFont := FaceHolding(Han);
  if lFont = nil then
    begin
    Ignore('no face here holds a Han character');
    Exit;
    end;
  lNarrow := lFont.GetGlyphIndex(Ord('A'));
  if lNarrow = 0 then
    begin
    Ignore('the face holding the Han character holds no Latin one');
    Exit;
    end;
  lWide := lFont.GetGlyphIndex(Han);
  AssertTrue('the narrow glyph is narrower across',
    lFont.GetGlyphAdvance(lNarrow) < lFont.GetGlyphAdvance(lWide));
  // That is the difference between the two advances: a column keeps one
  // spacing however wide the glyph is.
  AssertTrue('but takes a whole cell of the column all the same',
    lFont.GetGlyphVerticalAdvance(lNarrow) > lFont.GetGlyphAdvance(lNarrow));
  AssertEquals('the same cell the wide one takes',
    lFont.GetGlyphVerticalAdvance(lWide),
    lFont.GetGlyphVerticalAdvance(lNarrow), MetricDelta);
end;


{ Text running down the page. Every glyph of the stub font advances a
  whole size down, stands back half of its half-size width, and hangs
  eight tenths of a size below the pen. }

procedure TTestSVGTextLayout.TestAVerticalTextAdvancesDownThePage;

begin
  AssertTrue('the text lays out', LayoutSource(
    Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="20" y="30">abc</text>')));
  AssertEquals('three characters give three glyphs', 3, GlyphCount);
  AssertEquals('the first hangs below the point it was given',
    30.0 + Size * StubAscentRatio, GlyphAt(0).Y, Delta);
  AssertEquals('the second a whole size further down',
    30.0 + Size + Size * StubAscentRatio, GlyphAt(1).Y, Delta);
  AssertEquals('the third one more',
    30.0 + 2 * Size + Size * StubAscentRatio, GlyphAt(2).Y, Delta);
  AssertEquals('and none of them moves across', GlyphAt(0).X,
    GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestAVerticalGlyphStandsBackHalfItsOwnWidth;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="20" y="30">ab</text>'));
  // The pen sits on the baseline of the column and the glyph is centred on
  // it, so the glyph starts half its width to the left.
  AssertEquals('the glyph is centred on the baseline of the column',
    20.0 - Advance / 2, GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestAVerticalTextHangsFromTheAscent;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="0" y="0">a</text>'));
  AssertEquals('the glyph hangs an ascent below the pen',
    Size * StubAscentRatio, GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTheAnchorRunsDownTheColumn;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="0" y="50"'
    + ' text-anchor="middle">abcd</text>'));
  AssertEquals('four glyphs take four sizes of column', 4, GlyphCount);
  // Four whole sizes of column, centred on fifty, so it opens two above.
  AssertEquals('the column is centred on the point it was given',
    50.0 - 2 * Size + Size * StubAscentRatio, GlyphAt(0).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAShiftMovesAcrossTheColumn;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="20" y="30">a'
    + '<tspan baseline-shift="7">b</tspan>c</text>'));
  AssertEquals('the shifted glyph moves across the column',
    20.0 + 7 - Advance / 2, GlyphAt(1).X, Delta);
  AssertEquals('and keeps the place down it that the pen gave it',
    30.0 + Size + Size * StubAscentRatio, GlyphAt(1).Y, Delta);
  AssertEquals('the one after it is back on the baseline',
    20.0 - Advance / 2, GlyphAt(2).X, Delta);
end;


procedure TTestSVGTextLayout.TestAnAbsoluteYOpensAChunkDownTheColumn;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="0" y="10"'
    + ' text-anchor="end">ab<tspan y="60">cd</tspan></text>'));
  AssertEquals('four glyphs in two chunks', 4, GlyphCount);
  // Each chunk is two sizes long and ends at the point it was given, so
  // each one starts two sizes above its own y.
  AssertEquals('the first chunk ends at its own y',
    10.0 - 2 * Size + Size * StubAscentRatio, GlyphAt(0).Y, Delta);
  AssertEquals('and the second at the y that opened it',
    60.0 - 2 * Size + Size * StubAscentRatio, GlyphAt(2).Y, Delta);
end;


procedure TTestSVGTextLayout.TestTextLengthIsSpreadDownTheColumn;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0" x="0" y="0"'
    + ' textLength="100">abcd</text>'));
  // Four glyphs take forty of the hundred requested, so the remaining
  // sixty is shared out among the three gaps.
  AssertEquals('the first stays at the position it was given',
    Size * StubAscentRatio, GlyphAt(0).Y, Delta);
  AssertEquals('the last closes the length asked for',
    90.0 + Size * StubAscentRatio, GlyphAt(3).Y, Delta);
  AssertEquals('and the gaps are even', 30.0 + Size * StubAscentRatio,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAcrossThePageIsLeftAsItWas;

begin
  LayoutSource(Doc('<text writing-mode="lr-tb" x="20" y="30">abc</text>'));
  AssertEquals('the glyphs still run across the page', 20.0 + Advance,
    GlyphAt(1).X, Delta);
  AssertEquals('and sit on the baseline they were given', 30.0,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAutoLaysALatinGlyphOnItsSide;

begin
  AssertTrue('the text lays out', LayoutSource(
    Doc('<text writing-mode="tb" x="20" y="30">a</text>')));
  AssertEquals('a character that is not full width turns a quarter',
    90.0, GlyphAt(0).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestAutoStandsAnIdeographUpright;

begin
  LayoutSource(Doc('<text writing-mode="tb" x="20" y="30">&#x4E00;</text>'));
  AssertEquals('an ideograph reads upright and turns with nothing',
    0.0, GlyphAt(0).Angle, Delta);
end;


procedure TTestSVGTextLayout.TestAGlyphOnItsSideRunsOnItsWidth;

begin
  LayoutSource(Doc('<text writing-mode="tb" x="20" y="30">ab</text>'));
  // Laid on its side it runs down the column on the width it would have
  // taken across the page, and nothing moves it below the pen.
  AssertEquals('the first sits at the pen', 30.0, GlyphAt(0).Y, Delta);
  AssertEquals('and the second one width along', 30.0 + Advance,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAnUprightGlyphRunsOnItsHeight;

begin
  LayoutSource(Doc('<text writing-mode="tb" x="20" y="30">'
    + '&#x4E00;&#x4E8C;</text>'));
  AssertEquals('an upright one hangs from the ascent',
    30.0 + Size * StubAscentRatio, GlyphAt(0).Y, Delta);
  AssertEquals('and the next runs a whole size down',
    30.0 + Size + Size * StubAscentRatio, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestZeroStandsALatinGlyphUpright;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="0"'
    + ' x="20" y="30">ab</text>'));
  AssertEquals('nothing turns', 0.0, GlyphAt(0).Angle, Delta);
  AssertEquals('and it runs down the column on its height',
    30.0 + Size + Size * StubAscentRatio, GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAQuarterTurnLaysDownEvenAnIdeograph;

begin
  LayoutSource(Doc('<text writing-mode="tb" glyph-orientation-vertical="90"'
    + ' x="20" y="30">&#x4E00;&#x4E8C;</text>'));
  AssertEquals('an angle is taken at its word', 90.0, GlyphAt(0).Angle,
    Delta);
  AssertEquals('and the ideograph runs on its width too', 30.0 + Advance,
    GlyphAt(1).Y, Delta);
end;


procedure TTestSVGTextLayout.TestAGlyphOnItsSideIsCentredOnWhatItStands;

begin
  LayoutSource(Doc('<text writing-mode="tb" x="20" y="30">a</text>'));
  // A quarter turn puts the height of the glyph across the column, so it
  // steps back by half of that height.
  AssertEquals('the turned glyph is centred on the column',
    20.0 - (Size * StubAscentRatio - Size * StubDescentRatio) / 2,
    GlyphAt(0).X, Delta);
end;


procedure TTestSVGTextLayout.TestTheUprightCharactersAreTheIdeographicOnes;

begin
  AssertTrue('a Han character is upright', SVGStandsUpright($4E00));
  AssertTrue('so does a hiragana', SVGStandsUpright($3042));
  AssertTrue('and a Hangul syllable', SVGStandsUpright($AC00));
  AssertTrue('and a full width letter', SVGStandsUpright($FF21));
  AssertFalse('a Latin letter does not', SVGStandsUpright(Ord('A')));
  AssertFalse('nor a digit', SVGStandsUpright(Ord('7')));
  AssertFalse('nor a character between the blocks',
    SVGStandsUpright($303F));
end;


procedure TTestSVGTextRender.TestALineUnderAColumnRunsDownItsLeft;

begin
  Draw(RuledColumnDoc('underline'));
  AssertEquals('the line runs down the left of the column', 1.0,
    AlphaAt(30, 20), 0.01);
  AssertEquals('nothing reaches further out', 0.0, AlphaAt(26, 20), 0.01);
  AssertEquals('nor between the line and the glyphs', 0.0,
    AlphaAt(33, 20), 0.01);
end;


procedure TTestSVGTextRender.TestALineOverAColumnRunsDownItsRight;

begin
  Draw(RuledColumnDoc('overline'));
  AssertEquals('the line runs down the other side', 1.0,
    AlphaAt(50, 20), 0.01);
  AssertEquals('nothing reaches further out', 0.0, AlphaAt(54, 20), 0.01);
  AssertEquals('and nothing is left on the near side', 0.0,
    AlphaAt(30, 20), 0.01);
end;


procedure TTestSVGTextRender.TestAStrikeRunsDownTheMiddleOfTheColumn;

begin
  Draw(RuledColumnDoc('line-through'));
  // Below the single glyph, where the strike is the only thing drawn.
  AssertEquals('the strike runs down the middle', 1.0, AlphaAt(40, 28),
    0.01);
  AssertEquals('and neither side has a line', 0.0, AlphaAt(30, 28),
    0.01);
end;


procedure TTestSVGTextRender.TestTheLineReachesTheEndsOfTheColumnAndNoFurther;

begin
  Draw(RuledColumnDoc('underline'));
  // The column opens at ten and one glyph moves it a whole em down.
  AssertEquals('the line reaches the head of the column', 1.0,
    AlphaAt(30, 12), 0.01);
  AssertEquals('and stops at its foot', 0.0, AlphaAt(30, 35), 0.01);
end;


procedure TTestSVGTextRender.TestAColumnIsKernedByThePairsWrittenForIt;

begin
  // Two hundred of the em closes the pair by four, so the second glyph
  // runs from thirty eight to forty two instead of starting at forty
  // two.
  Draw(StackedDoc('<vkern u1="A" u2="A" k="200"/>',
    Upright, 'AA'));
  AssertEquals('the second glyph is set closer up the column', 1.0,
    AlphaAt(40, 40), 0.01);
  AssertEquals('and no longer sits at the unkerned position', 0.0,
    AlphaAt(40, 44), 0.01);
end;


procedure TTestSVGTextRender.TestAColumnWithoutSuchAPairIsLeftAlone;

begin
  Draw(StackedDoc('', Upright, 'AA'));
  AssertEquals('the second glyph keeps the whole em', 1.0,
    AlphaAt(40, 44), 0.01);
  AssertEquals('and nothing sits at the kerned position', 0.0,
    AlphaAt(40, 40), 0.01);
end;


procedure TTestSVGTextRender.TestThePairsThatKernARowDoNotKernAColumn;

begin
  Draw(StackedDoc('<hkern u1="A" u2="A" k="200"/>',
    Upright, 'AA'));
  AssertEquals('a pair written for a row leaves the column alone', 1.0,
    AlphaAt(40, 44), 0.01);
  AssertEquals('and closes nothing up', 0.0, AlphaAt(40, 40), 0.01);
end;


procedure TTestSVGTextRender.TestAPairOnItsSideTakesThePairsThatKernARow;

begin
  // Laid on its side each glyph runs ten down the column, so the second
  // reaches thirty unkerned and twenty six with two hundred of the em
  // taken off.
  Draw(StackedDoc('<hkern u1="A" u2="A" k="200"/>', '', 'AA'));
  AssertEquals('the pair is set as a row is, and closed up by four', 0.0,
    AlphaAt(36, 28), 0.01);
  AssertEquals('and sits at the position the kerning gives', 1.0,
    AlphaAt(36, 24), 0.01);
end;


procedure TTestSVGTextRender.TestAPairOnItsSideIgnoresThePairsWrittenForAColumn;

begin
  Draw(StackedDoc('<vkern u1="A" u2="A" k="200"/>', '', 'AA'));
  AssertEquals('a pair written for a column closes nothing on its side',
    1.0, AlphaAt(36, 28), 0.01);
end;


procedure TTestSVGTextRender.TestAPairStandingOneOfEachWayIsLeftAlone;

begin
  // The ideograph is upright and runs a whole em down the column, the
  // letter after it lies on its side, and neither kerning pair covers the
  // two of them together.
  Draw(StackedDoc('<hkern u1="&#x4E00;" u2="A" k="200"/>'
    + '<vkern u1="&#x4E00;" u2="A" k="200"/>', '', '&#x4E00;A'));
  AssertEquals('nothing closes the pair up', 1.0, AlphaAt(36, 38), 0.01);
end;


procedure TTestSVGFontRegistry.TestADirectoryOfWebFontsIsRead;

const
  Names: array[0..0] of String = ('probe.woff');

var
  lDir: String;
  lProbe: TSVGFreeTypeProvider;

begin
  lDir := DirectoryOfCopies(FProvider, Names);
  if lDir = '' then
    begin
    Ignore('this machine holds no face to copy');
    Exit;
    end;
  lProbe := TSVGFreeTypeProvider.Create;
  try
    // A woff is a face packed for the web, and a directory of them is
    // read like any other.
    AssertEquals('the face under a web font extension is read', 1,
      lProbe.AddFontPath(lDir));
  finally
    lProbe.Free;
    ClearDirectory(lDir, Names);
  end;
end;


procedure TTestSVGFontRegistry.TestAFileOfNoKnownExtensionIsPassedOver;

const
  Names: array[0..1] of String = ('probe.woff', 'probe.zzz');

var
  lDir: String;
  lProbe: TSVGFreeTypeProvider;

begin
  lDir := DirectoryOfCopies(FProvider, Names);
  if lDir = '' then
    begin
    Ignore('this machine holds no face to copy');
    Exit;
    end;
  lProbe := TSVGFreeTypeProvider.Create;
  try
    // Both files hold the same face, so it is the file name and not the
    // content that leaves one of the two out.
    AssertEquals('only the file with a known extension is read', 1,
      lProbe.AddFontPath(lDir));
  finally
    lProbe.Free;
    ClearDirectory(lDir, Names);
  end;
end;


procedure TTestSVGTextRender.TestAFaceOfSmallCapitalsIsLeftToDrawThem;

begin
  Draw(CapsDoc(' font-variant="small-caps"'));
  // The face states that it has real small capitals, so its glyph is
  // drawn at the size the element requests and is the full sixteen
  // tall.
  AssertEquals('the letter has its full height', 1.0, AlphaAt(14, 25),
    0.01);
  AssertEquals('and nothing above it', 0.0, AlphaAt(14, 22), 0.01);
end;


procedure TTestSVGTextRender.TestAFaceWithoutThemHasACapitalStandIn;

begin
  Draw(CapsDoc(''));
  // The face states nothing, so a capital at four fifths of the size is
  // used instead, and it reaches only twelve and four fifths.
  AssertEquals('the letter is set shorter', 0.0, AlphaAt(14, 25), 0.01);
  AssertEquals('but is drawn all the same', 1.0, AlphaAt(14, 30), 0.01);
end;


initialization
  RegisterTest('text', TTestSVGPlatformCoverage);
  RegisterTest('text', TTestSVGCodePoints);
  RegisterTest('text', TTestSVGTextLayout);
  RegisterTest('text', TTestSVGTextRender);
  RegisterTest('text', TTestSVGTextAsClipPath);
  RegisterTest('text', TTestSVGTextGoldens);
  RegisterTest('fonts', TTestSVGFontRegistry);
  RegisterTest('fonts', TTestSVGVerticalMetrics);
end.
