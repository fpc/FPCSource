{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the cascade over the FPC CSS resolver and computed values.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgcascade;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test, FpcUnit.Registry,
     svggoldens, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.style;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit, testregistry, svggoldens, fpsvg.types,
     fpsvg.dom, fpsvg.read, fpsvg.style;
{$ENDIF FPC_DOTTEDUNITS}

type
  { A history holding one address, so that a test can say a link was
    followed without keeping one of its own. }
  TSVGOneVisitHistory = class(TObject, ISVGLinkHistory)
  private
    FHRef: String;
  public
    constructor Create(const aHRef: String);
    function WasVisited(const aHRef, aBaseURI: String): Boolean;
  end;

  TTestSVGCascade = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FStyle: TSVGStyleResolver;
    // The computed style of the element with the given id.
    function StyleOf(const aID: String): TSVGComputedStyle;
    // Replaces the document under test with one read from source.
    procedure LoadSource(const aText: String);
    // Fails unless the element with the given id is filled orange.
    procedure AssertOrangeFill(const aWhat, aID: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStyleSheetIsCollected;
    procedure TestStylesheetRuleBeatsPresentationAttribute;
    procedure TestIdSelectorBeatsTypeSelector;
    procedure TestClassSelectorBeatsTypeSelector;
    procedure TestInlineStyleBeatsEverything;
    procedure TestDescendantSelectorMatches;
    procedure TestPseudoClassMatches;
    procedure TestALinkMatchesAnAnchorThatNamesSomething;
    procedure TestALinkDoesNotMatchAnAnchorNamingNothing;
    procedure TestALinkDoesNotMatchWhatIsNotAnAnchor;
    procedure TestTheStatePseudoClassesMatchNothing;
    procedure TestAVisitedLinkIsReachedByVisited;
    procedure TestAVisitedLinkIsNoLongerALink;
    procedure TestALinkNotInTheHistoryStaysALink;
    procedure TestAHistoryGivenAfterLoadingStillReaches;
    procedure TestPresentationAttributeAppliesWithoutARule;
    procedure TestInheritedPropertyReachesChild;
    procedure TestNonInheritedPropertyStopsAtTheElement;
    procedure TestCurrentColorReadsTheColourProperty;
    procedure TestInheritKeywordTakesTheParentValue;
    procedure TestDisplayNoneIsNotInherited;
    procedure TestARuleNamesAPropertyWhateverItsCase;
    procedure TestAStyleAttributeNamesAPropertyWhateverItsCase;
    procedure TestTwoSpellingsOfANameAreOneProperty;
    procedure TestAMisCasedPresentationAttributeIsIgnored;
    procedure TestASelectorKeepsTheCaseThatMatches;
    procedure TestARuleMayNameAPaintServer;
    procedure TestARuleMayNameAMarker;
    procedure TestAStyleAttributeBeatsARuleThatNamesAServer;
    procedure TestARuleKeepsTheColourWrittenAfterTheServer;
    procedure TestTheMarkerShorthandSetsAllThree;
    procedure TestDashPatternIsParsed;
    procedure TestADashPatternMayHaveUnits;
    procedure TestADashPatternWithOneBadLengthIsPassedOver;
    procedure TestMalformedValuesKeepTheInheritedValue;
    procedure TestUnloadClearsTheDocumentLink;
    procedure TestWritingModeIsReadFromAPresentationAttribute;
    procedure TestWritingModeTBRLAlsoRunsDownThePage;
    procedure TestWritingModeReachesAChild;
    procedure TestRightToLeftIsReadAsLeftToRightForNow;
    procedure TestGlyphOrientationStartsAtAuto;
    procedure TestGlyphOrientationReadsAQuarterTurn;
    procedure TestGlyphOrientationReadsAnAngleWrittenInDegrees;
    procedure TestGlyphOrientationAutoOverridesAnInheritedTurn;
    procedure TestGlyphOrientationIgnoresAnAngleOffTheQuarters;
    procedure TestComputedStyleGolden;
  end;

  { The deprecated clip property, which narrows the viewport an element
    establishes. SVG 1.1 takes the sides of rect() as offsets from the
    side of that viewport each one faces. }
  TTestSVGClipProperty = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FStyle: TSVGStyleResolver;
    // The clip of the image element in a document with the given
    // clip declaration, written as an attribute unless it holds a colon.
    function ClipOf(const aDeclaration: String): TSVGClipShape;
  protected
    procedure TearDown; override;
  published
    procedure TestAutoIsTheInitialValue;
    procedure TestCommasSeparateTheSides;
    procedure TestWhitespaceSeparatesTheSides;
    procedure TestTheSidesAreReadTopRightBottomLeft;
    procedure TestOneSideMayBeAuto;
    procedure TestAStylesheetSetsIt;
    procedure TestTooFewSidesLeaveTheValueAlone;
    procedure TestAMissingBracketLeavesTheValueAlone;
    procedure TestItIsNotInherited;
    procedure TestEachSideMovesInFromTheSideItFaces;
    procedure TestAnAutoSideStaysOnTheViewport;
    procedure TestCrossedSidesClipEverythingAway;
    procedure TestAutoClipsToTheViewportItself;
  end;

  { The font-face rules that a document's stylesheets declare. }
  TTestSVGFontFaceRules = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FStyle: TSVGStyleResolver;
    // Loads a document whose only style element holds the given css.
    procedure LoadStyle(const aCSS: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestARuleIsCollectedWithItsSource;
    procedure TestTheWeightAndSlantDescriptorsAreRead;
    procedure TestAFormatAfterTheUrlIsNotPartOfIt;
    procedure TestAQuotedUrlLosesItsQuotes;
    procedure TestEverySourceIsKeptInOrder;
    procedure TestARuleWithoutASourceIsDropped;
    procedure TestADocumentWithoutAStyleSheetDeclaresNoFonts;
    procedure TestUnloadForgetsTheRules;
  end;

implementation

const
  Delta = 1e-9;

procedure TTestSVGCascade.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'cascade.svg');
  FStyle := TSVGStyleResolver.Create;
  FStyle.LoadDocument(FDocument);
end;


procedure TTestSVGCascade.TearDown;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGCascade.LoadSource(const aText: String);

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(aText);
  FStyle := TSVGStyleResolver.Create;
  FStyle.LoadDocument(FDocument);
end;


procedure TTestSVGCascade.AssertOrangeFill(const aWhat, aID: String);

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf(aID);
  AssertEquals(aWhat + ' (red)', $FFFF, lStyle.Fill.Color.Red);
  AssertEquals(aWhat + ' (green)', $A5A5, lStyle.Fill.Color.Green);
  AssertEquals(aWhat + ' (blue)', 0, lStyle.Fill.Color.Blue);
end;


function TTestSVGCascade.StyleOf(const aID: String): TSVGComputedStyle;

var
  lElement: TSVGElement;

begin
  lElement := FDocument.ElementByID(aID);
  AssertNotNull('the document holds an element with id ' + aID, lElement);
  Result := FStyle.ComputeStyleOf(lElement);
end;


procedure TTestSVGCascade.TestALinkMatchesAnAnchorThatNamesSomething;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
    + '<style type="text/css">:link {fill: orange}</style>'
    + '<a id="x" xlink:href="other.svg"><rect/></a></svg>');
  AssertOrangeFill('an anchor with an href is a link', 'x');
end;


procedure TTestSVGCascade.TestALinkDoesNotMatchAnAnchorNamingNothing;

var
  lStyle: TSVGComputedStyle;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">:link {fill: orange}</style>'
    + '<a id="x"><rect/></a></svg>');
  lStyle := StyleOf('x');
  AssertEquals('an anchor leading nowhere is no link', 0,
    lStyle.Fill.Color.Red);
end;


procedure TTestSVGCascade.TestALinkDoesNotMatchWhatIsNotAnAnchor;

var
  lStyle: TSVGComputedStyle;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
    + '<style type="text/css">:link {fill: orange}</style>'
    + '<rect id="x" xlink:href="other.svg"/></svg>');
  lStyle := StyleOf('x');
  AssertEquals('an href on anything else makes no link', 0,
    lStyle.Fill.Color.Red);
end;


procedure TTestSVGCascade.TestTheStatePseudoClassesMatchNothing;

var
  lStyle: TSVGComputedStyle;

begin
  // Nothing here has been visited, hovered or given the keyboard, so a
  // rule for one of those states matches no element.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
    + '<style type="text/css">:visited {fill: orange} :hover {fill: orange}'
    + ' :active {fill: orange} :focus {fill: orange}</style>'
    + '<a id="x" xlink:href="other.svg"><rect/></a></svg>');
  lStyle := StyleOf('x');
  AssertEquals('a link that was never visited stays as it was', 0,
    lStyle.Fill.Color.Red);
end;


// A history of one address, for the tests below.
function OneVisit(const aHRef: String): TSVGOneVisitHistory;

begin
  Result := TSVGOneVisitHistory.Create(aHRef);
end;


procedure TTestSVGCascade.TestAVisitedLinkIsReachedByVisited;

var
  lHistory: TSVGOneVisitHistory;

begin
  lHistory := OneVisit('there.svg');
  try
    LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
      + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
      + '<style type="text/css">:visited {fill: orange}</style>'
      + '<a id="x" xlink:href="there.svg"><rect/></a></svg>');
    FStyle.LinkHistory := lHistory;
    AssertOrangeFill('a link in the history has been visited', 'x');
  finally
    FStyle.LinkHistory := nil;
    lHistory.Free;
  end;
end;


procedure TTestSVGCascade.TestAVisitedLinkIsNoLongerALink;

var
  lHistory: TSVGOneVisitHistory;
  lStyle: TSVGComputedStyle;

begin
  // CSS has a link be the one or the other, never both.
  lHistory := OneVisit('there.svg');
  try
    LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
      + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
      + '<style type="text/css">:link {fill: orange}</style>'
      + '<a id="x" xlink:href="there.svg"><rect/></a></svg>');
    FStyle.LinkHistory := lHistory;
    lStyle := StyleOf('x');
    AssertEquals('a link that was followed is no longer unvisited', 0,
      lStyle.Fill.Color.Red);
  finally
    FStyle.LinkHistory := nil;
    lHistory.Free;
  end;
end;


procedure TTestSVGCascade.TestALinkNotInTheHistoryStaysALink;

var
  lHistory: TSVGOneVisitHistory;

begin
  lHistory := OneVisit('elsewhere.svg');
  try
    LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
      + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
      + '<style type="text/css">:link {fill: orange}</style>'
      + '<a id="x" xlink:href="there.svg"><rect/></a></svg>');
    FStyle.LinkHistory := lHistory;
    AssertOrangeFill('a link outside the history is unvisited', 'x');
  finally
    FStyle.LinkHistory := nil;
    lHistory.Free;
  end;
end;


procedure TTestSVGCascade.TestAHistoryGivenAfterLoadingStillReaches;

var
  lHistory: TSVGOneVisitHistory;
  lStyle: TSVGComputedStyle;

begin
  lHistory := OneVisit('there.svg');
  try
    LoadSource('<svg xmlns="http://www.w3.org/2000/svg"'
      + ' xmlns:xlink="http://www.w3.org/1999/xlink">'
      + '<style type="text/css">:visited {fill: orange}</style>'
      + '<a id="x" xlink:href="there.svg"><rect/></a></svg>');
    lStyle := StyleOf('x');
    AssertEquals('nothing is visited before a history is given', 0,
      lStyle.Fill.Color.Red);
    FStyle.LinkHistory := lHistory;
    AssertOrangeFill('and the document loaded already is told', 'x');
  finally
    FStyle.LinkHistory := nil;
    lHistory.Free;
  end;
end;


procedure TTestSVGCascade.TestARuleNamesAPropertyWhateverItsCase;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">#x {FiLl: oRaNgE}</style>'
    + '<rect id="x" fill="red"/></svg>');
  AssertOrangeFill('the rule is read though its name is mixed', 'x');
end;


procedure TTestSVGCascade.TestAStyleAttributeNamesAPropertyWhateverItsCase;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<rect id="x" fill="red" style="FiLl: oRaNgE"/></svg>');
  AssertOrangeFill('the declaration is read though its name is mixed', 'x');
end;


procedure TTestSVGCascade.TestTwoSpellingsOfANameAreOneProperty;

begin
  // Both rules weigh the same, so the later wins. That can only happen if
  // the two spellings reach the cascade as one property.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">#x {fill: red} #x {FiLl: oRaNgE}</style>'
    + '<rect id="x"/></svg>');
  AssertOrangeFill('the later of the two spellings wins', 'x');
end;


procedure TTestSVGCascade.TestAMisCasedPresentationAttributeIsIgnored;

begin
  // An attribute is not a declaration and its name is read exactly, so
  // this one sets no property and the fill comes from the group.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<g fill="orange"><rect id="x" FiLl="red"/></g></svg>');
  AssertOrangeFill('the mis-cased attribute is passed over', 'x');
end;


procedure TTestSVGCascade.TestASelectorKeepsTheCaseThatMatches;

begin
  // The id spells a property name and a pseudo-class follows it. Reading
  // it as a declaration would lower its case, and it would then select
  // nothing.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<rect id="FiLl" fill="red"/>'
    + '<style type="text/css">#FiLl:first-child {fill: orange}</style>'
    + '</svg>');
  AssertOrangeFill('the id still selects the element', 'FiLl');
end;


procedure TTestSVGCascade.TestARuleMayNameAPaintServer;

var
  lStyle: TSVGComputedStyle;

begin
  // The token model of the CSS engine reads a url and keeps its text
  // without the closing bracket, so the value is rebuilt from the tree
  // instead of being taken as it is.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<linearGradient id="g"><stop offset="0" stop-color="#00ff00"/>'
    + '</linearGradient>'
    + '<style type="text/css">rect {fill: url(#g)}</style>'
    + '<rect id="x"/></svg>');
  lStyle := StyleOf('x');
  AssertTrue('the rule gives a server and the fill takes it',
    lStyle.Fill.Kind = spServer);
end;


procedure TTestSVGCascade.TestARuleMayNameAMarker;

var
  lStyle: TSVGComputedStyle;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">.m {marker-start: url(#tip)}</style>'
    + '<path id="x" class="m" d="M0 0 L10 10"/></svg>');
  lStyle := StyleOf('x');
  AssertEquals('the marker of the rule reaches the style',
    '#tip', lStyle.MarkerStart);
end;


procedure TTestSVGCascade.TestAStyleAttributeBeatsARuleThatNamesAServer;

var
  lStyle: TSVGComputedStyle;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">.m {marker-start: url(#weak)}</style>'
    + '<path id="x" class="m" style="marker-start: url(#strong)"'
    + ' d="M0 0 L10 10"/></svg>');
  lStyle := StyleOf('x');
  AssertEquals('the style attribute is read after the rule',
    '#strong', lStyle.MarkerStart);
end;


procedure TTestSVGCascade.TestARuleKeepsTheColourWrittenAfterTheServer;

var
  lStyle: TSVGComputedStyle;

begin
  // The reference leads nowhere, so the colour beside it paints. That
  // only works if the whole value came through, brackets and all.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">rect {fill: url(#nowhere) #ff0000}</style>'
    + '<rect id="x"/></svg>');
  lStyle := StyleOf('x');
  AssertEquals('the colour behind the reference paints (red)', $FFFF,
    lStyle.Fill.Color.Red);
  AssertEquals('and nothing else does (green)', 0,
    lStyle.Fill.Color.Green);
end;


procedure TTestSVGCascade.TestTheMarkerShorthandSetsAllThree;

var
  lStyle: TSVGComputedStyle;

begin
  // marker sets all three marker properties, and can only be written as a
  // declaration, never as an attribute.
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<style type="text/css">path {marker: url(#tip)}</style>'
    + '<path id="x" d="M0 0 L10 10"/></svg>');
  lStyle := StyleOf('x');
  AssertEquals('the shorthand reaches the start', '#tip',
    lStyle.MarkerStart);
  AssertEquals('and the middle', '#tip', lStyle.MarkerMid);
  AssertEquals('and the end', '#tip', lStyle.MarkerEnd);
end;


procedure TTestSVGCascade.TestStyleSheetIsCollected;

begin
  AssertEquals('the style element becomes one stylesheet', 1,
    FStyle.StyleSheetCount);
end;


procedure TTestSVGCascade.TestStylesheetRuleBeatsPresentationAttribute;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('plain');
  AssertEquals('the rule wins over the fill attribute (red)', 0,
    lStyle.Fill.Color.Red);
  AssertEquals('the rule wins over the fill attribute (green)', $8080,
    lStyle.Fill.Color.Green);
end;


procedure TTestSVGCascade.TestIdSelectorBeatsTypeSelector;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('special');
  AssertEquals('the id rule wins over the type rule (red)', $8080,
    lStyle.Fill.Color.Red);
  AssertEquals('the id rule wins over the type rule (blue)', $8080,
    lStyle.Fill.Color.Blue);
end;


procedure TTestSVGCascade.TestClassSelectorBeatsTypeSelector;

begin
  AssertEquals('the class rule wins over the type rule', 7,
    StyleOf('classy').Pen.Width, Delta);
  AssertEquals('the type rule still applies where no class does', 3,
    StyleOf('plain').Pen.Width, Delta);
end;


procedure TTestSVGCascade.TestInlineStyleBeatsEverything;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('inlined');
  AssertEquals('the inline style wins (red)', $FFFF, lStyle.Fill.Color.Red);
  AssertEquals('the inline style wins (green)', $A5A5, lStyle.Fill.Color.Green);
end;


procedure TTestSVGCascade.TestDescendantSelectorMatches;

begin
  AssertEquals('a descendant selector reaches the nested rect', $FFFF,
    StyleOf('nested').Stroke.Color.Blue);
  AssertTrue('a rect outside the group is unstroked',
    StyleOf('plain').Stroke.Kind = spNone);
end;


procedure TTestSVGCascade.TestPseudoClassMatches;

begin
  AssertEquals('first-of-type matches the first circle', 0.25,
    StyleOf('first').FillOpacity, Delta);
  AssertEquals('first-of-type leaves the second circle alone', 1,
    StyleOf('second').FillOpacity, Delta);
end;


procedure TTestSVGCascade.TestPresentationAttributeAppliesWithoutARule;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('group');
  AssertEquals('the fill attribute applies where no rule matches', $8080,
    lStyle.Fill.Color.Green);
  AssertTrue('the stroke-linejoin attribute applies',
    lStyle.Pen.Join = ljRound);
end;


procedure TTestSVGCascade.TestInheritedPropertyReachesChild;

begin
  AssertTrue('stroke-linejoin inherits into the child',
    StyleOf('nested').Pen.Join = ljRound);
end;


procedure TTestSVGCascade.TestNonInheritedPropertyStopsAtTheElement;

begin
  AssertEquals('the group has its own opacity', 0.5,
    StyleOf('group').Opacity, Delta);
  AssertEquals('opacity does not reach the child', 1,
    StyleOf('nested').Opacity, Delta);
end;


procedure TTestSVGCascade.TestCurrentColorReadsTheColourProperty;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('current');
  AssertEquals('currentColor takes the colour property (red)', $FFFF,
    lStyle.Fill.Color.Red);
  AssertEquals('currentColor takes the colour property (green)', 0,
    lStyle.Fill.Color.Green);
  AssertEquals('currentColor takes the colour property (blue)', $FFFF,
    lStyle.Fill.Color.Blue);
end;


procedure TTestSVGCascade.TestInheritKeywordTakesTheParentValue;

begin
  AssertEquals('fill inherit takes the group teal, not the green rule', $8080,
    StyleOf('explicit').Fill.Color.Blue);
end;


procedure TTestSVGCascade.TestDisplayNoneIsNotInherited;

begin
  AssertFalse('the group with display none is not displayed',
    StyleOf('hidden').IsDisplayed);
  AssertTrue('display does not inherit, so the child reports displayed',
    StyleOf('buried').IsDisplayed);
end;


procedure TTestSVGCascade.TestDashPatternIsParsed;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('dashed');
  AssertTrue('the dash pattern is recorded', lStyle.Pen.IsDashed);
  AssertEquals('every dash length is read', 3, Length(lStyle.Pen.Dashes));
  AssertEquals('the last dash length is read', 1,
    lStyle.Pen.Dashes[2], Delta);
  AssertEquals('the dash offset is read', 1.5, lStyle.Pen.DashOffset, Delta);
end;


procedure TTestSVGCascade.TestADashPatternMayHaveUnits;

var
  lStyle: TSVGComputedStyle;

begin
  // A dash is a length, so px may stand after it as after any other.
  lStyle := StyleOf('dashedpx');
  AssertTrue('the dash pattern is recorded', lStyle.Pen.IsDashed);
  AssertEquals('every dash length is read', 3, Length(lStyle.Pen.Dashes));
  AssertEquals('the first is read past its unit', 4,
    lStyle.Pen.Dashes[0], Delta);
  AssertEquals('and the last', 1, lStyle.Pen.Dashes[2], Delta);
end;


procedure TTestSVGCascade.TestADashPatternWithOneBadLengthIsPassedOver;

var
  lStyle: TSVGComputedStyle;

begin
  // One length that cannot be read leaves the whole list unread, instead
  // of building a pattern from the rest of it.
  lStyle := StyleOf('dashedbad');
  AssertTrue('the stroke is not dashed at all', not lStyle.Pen.IsDashed);
  AssertEquals('and no length was kept', 0, Length(lStyle.Pen.Dashes));
end;


procedure TTestSVGCascade.TestMalformedValuesKeepTheInheritedValue;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := StyleOf('badpaint');
  AssertEquals('a fill that is not a colour leaves the cascaded value', 0,
    lStyle.Fill.Color.Red);
  AssertEquals('a negative stroke width is ignored', 3,
    lStyle.Pen.Width, Delta);
end;


procedure TTestSVGCascade.TestUnloadClearsTheDocumentLink;

begin
  FStyle.Unload;
  AssertNull('unloading clears the resolver from the document',
    FDocument.CSSResolver);
  AssertEquals('unloading forgets the stylesheets', 0,
    FStyle.StyleSheetCount);
end;


procedure TTestSVGCascade.TestWritingModeIsReadFromAPresentationAttribute;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text id="down" writing-mode="tb">down</text></svg>');
  AssertTrue('writing-mode="tb" runs the text down the page',
    StyleOf('down').WritingMode = wmTB);
end;


procedure TTestSVGCascade.TestWritingModeTBRLAlsoRunsDownThePage;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text id="down" writing-mode="tb-rl">down</text></svg>');
  AssertTrue('writing-mode="tb-rl" runs the text down the page',
    StyleOf('down').WritingMode = wmTB);
end;


procedure TTestSVGCascade.TestWritingModeReachesAChild;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<g writing-mode="tb"><text id="child">down</text></g></svg>');
  AssertTrue('a child inherits the writing mode of its group',
    StyleOf('child').WritingMode = wmTB);
end;


procedure TTestSVGCascade.TestRightToLeftIsReadAsLeftToRightForNow;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text id="back" writing-mode="rl">back</text></svg>');
  AssertTrue('writing-mode="rl" is read as running across the page',
    StyleOf('back').WritingMode = wmLRTB);
end;


procedure TTestSVGCascade.TestGlyphOrientationStartsAtAuto;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text id="plain">plain</text></svg>');
  AssertTrue('a document setting none leaves the orientation at auto',
    StyleOf('plain').GlyphOrientation = goAuto);
end;


procedure TTestSVGCascade.TestGlyphOrientationReadsAQuarterTurn;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text id="up" glyph-orientation-vertical="0">up</text>'
    + '<text id="turned" glyph-orientation-vertical="90">turned</text>'
    + '</svg>');
  AssertTrue('0 keeps the glyphs upright',
    StyleOf('up').GlyphOrientation = go0);
  AssertTrue('90 turns them a quarter turn',
    StyleOf('turned').GlyphOrientation = go90);
end;


procedure TTestSVGCascade.TestGlyphOrientationReadsAnAngleWrittenInDegrees;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text id="turned" glyph-orientation-vertical="90deg">turned</text>'
    + '</svg>');
  AssertTrue('an angle may have its unit',
    StyleOf('turned').GlyphOrientation = go90);
end;


procedure TTestSVGCascade.TestGlyphOrientationAutoOverridesAnInheritedTurn;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<g glyph-orientation-vertical="90">'
    + '<text id="child" glyph-orientation-vertical="auto">back</text>'
    + '</g></svg>');
  AssertTrue('auto undoes the turn of the group',
    StyleOf('child').GlyphOrientation = goAuto);
end;


procedure TTestSVGCascade.TestGlyphOrientationIgnoresAnAngleOffTheQuarters;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<g glyph-orientation-vertical="90">'
    + '<text id="child" glyph-orientation-vertical="45">odd</text>'
    + '</g></svg>');
  AssertTrue('an angle SVG does not allow keeps the inherited one',
    StyleOf('child').GlyphOrientation = go90);
end;


procedure TTestSVGCascade.TestComputedStyleGolden;

const
  Ids: array[0..11] of String = ('plain', 'special', 'classy', 'inlined',
    'group', 'nested', 'first', 'second', 'current', 'explicit', 'dashed',
    'badpaint');

var
  lLines, lBlock: TStringList;
  I: Integer;

begin
  lLines := TStringList.Create;
  lBlock := TStringList.Create;
  try
    for I := Low(Ids) to High(Ids) do
      begin
      lLines.Add('# ' + Ids[I]);
      lBlock.Text := StyleOf(Ids[I]).ToString;
      lLines.AddStrings(lBlock);
      end;
    AssertGolden(Self, 'cascade', lLines);
  finally
    lBlock.Free;
    lLines.Free;
  end;
end;


{ TTestSVGFontFaceRules }

procedure TTestSVGFontFaceRules.SetUp;

begin
  inherited SetUp;
  FStyle := TSVGStyleResolver.Create;
end;


procedure TTestSVGFontFaceRules.TearDown;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGFontFaceRules.LoadStyle(const aCSS: String);

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">'
    + '<style type="text/css"><![CDATA[' + aCSS + ']]></style>'
    + '<text>a</text></svg>');
  FStyle.LoadDocument(FDocument);
end;


procedure TTestSVGFontFaceRules.TestARuleIsCollectedWithItsSource;

begin
  LoadStyle('@font-face { font-family: ZC; src: url(fonts/zc.woff) }');
  AssertEquals('the rule is collected', 1, FStyle.FontFaceCount);
  AssertEquals('under the family it declares', 'ZC', FStyle.FontFaces[0].Family);
  AssertEquals('with one source', 1, Length(FStyle.FontFaces[0].Sources));
  AssertEquals('which is the url it gave', 'fonts/zc.woff',
    FStyle.FontFaces[0].Sources[0]);
  AssertEquals('a rule without a weight is a normal one', 400,
    FStyle.FontFaces[0].Weight);
end;


procedure TTestSVGFontFaceRules.TestTheWeightAndSlantDescriptorsAreRead;

begin
  LoadStyle('@font-face { font-family: ZC; font-weight: bold;'
    + ' font-style: italic; src: url(a.woff) }'
    + '@font-face { font-family: ZC; font-weight: 300; src: url(b.woff) }');
  AssertEquals('both rules are collected', 2, FStyle.FontFaceCount);
  AssertEquals('bold reads as 700', 700, FStyle.FontFaces[0].Weight);
  AssertTrue('and italic as a slant',
    FStyle.FontFaces[0].Style = fnItalic);
  AssertEquals('a number is taken as it is', 300,
    FStyle.FontFaces[1].Weight);
  AssertTrue('with no slant', FStyle.FontFaces[1].Style = fnNormal);
end;


procedure TTestSVGFontFaceRules.TestAFormatAfterTheUrlIsNotPartOfIt;

begin
  LoadStyle('@font-face { font-family: ZC;'
    + ' src: url(fonts/zc.woff) format("woff") }');
  AssertEquals('one source', 1, Length(FStyle.FontFaces[0].Sources));
  AssertEquals('the format hint stays out of the url', 'fonts/zc.woff',
    FStyle.FontFaces[0].Sources[0]);
end;


procedure TTestSVGFontFaceRules.TestAQuotedUrlLosesItsQuotes;

begin
  LoadStyle('@font-face { font-family: ZC; src: url("a name.woff") }');
  AssertEquals('the quotes are not part of the name', 'a name.woff',
    FStyle.FontFaces[0].Sources[0]);
end;


procedure TTestSVGFontFaceRules.TestEverySourceIsKeptInOrder;

begin
  LoadStyle('@font-face { font-family: ZC; src: url(a.woff) format("woff"),'
    + ' url(b.otf) format("opentype"), url(c.ttf) }');
  AssertEquals('all three are kept', 3,
    Length(FStyle.FontFaces[0].Sources));
  AssertEquals('the first stays first', 'a.woff',
    FStyle.FontFaces[0].Sources[0]);
  AssertEquals('then the second', 'b.otf', FStyle.FontFaces[0].Sources[1]);
  AssertEquals('then the third', 'c.ttf', FStyle.FontFaces[0].Sources[2]);
end;


procedure TTestSVGFontFaceRules.TestARuleWithoutASourceIsDropped;

begin
  LoadStyle('@font-face { font-family: ZC }'
    + '@font-face { src: url(a.woff) }');
  AssertEquals('a rule with no file, and one with no name, declare nothing',
    0, FStyle.FontFaceCount);
end;


procedure TTestSVGFontFaceRules.TestADocumentWithoutAStyleSheetDeclaresNoFonts;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">'
    + '<text>a</text></svg>');
  FStyle.LoadDocument(FDocument);
  AssertEquals('no stylesheet declares no font', 0, FStyle.FontFaceCount);
end;


procedure TTestSVGFontFaceRules.TestUnloadForgetsTheRules;

begin
  LoadStyle('@font-face { font-family: ZC; src: url(a.woff) }');
  AssertEquals('the rule is there', 1, FStyle.FontFaceCount);
  FStyle.Unload;
  AssertEquals('and gone once the document is', 0, FStyle.FontFaceCount);
end;


{ TSVGOneVisitHistory }

constructor TSVGOneVisitHistory.Create(const aHRef: String);

begin
  inherited Create;
  FHRef := aHRef;
end;


function TSVGOneVisitHistory.WasVisited(const aHRef,
  aBaseURI: String): Boolean;

begin
  if aBaseURI = '' then ;
  Result := SameText(Trim(aHRef), FHRef);
end;


{ TTestSVGClipProperty }

procedure TTestSVGClipProperty.TearDown;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGClipProperty.ClipOf(const aDeclaration: String): TSVGClipShape;

var
  lImage, lSheet: String;

begin
  lSheet := '';
  if Pos(':', aDeclaration) > 0 then
    begin
    lSheet := '<style type="text/css">#i { ' + aDeclaration + ' }</style>';
    lImage := '<image id="i" x="10" y="20" width="100" height="60"/>';
    end
  else
    lImage := '<image id="i" x="10" y="20" width="100" height="60" '
            + aDeclaration + '/>';
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="200" height="200">'
    + lSheet + lImage + '</svg>');
  FStyle := TSVGStyleResolver.Create;
  FStyle.LoadDocument(FDocument);
  Result := FStyle.ComputeStyleOf(FDocument.ElementByID('i')).Clip;
end;


procedure TTestSVGClipProperty.TestAutoIsTheInitialValue;

begin
  AssertFalse('a document that sets no clip leaves the viewport alone',
    ClipOf('').Shaped);
  AssertFalse('and so does the auto keyword', ClipOf('clip="auto"').Shaped);
end;


procedure TTestSVGClipProperty.TestCommasSeparateTheSides;

var
  lClip: TSVGClipShape;

begin
  lClip := ClipOf('clip="rect(1,2,3,4)"');
  AssertTrue('rect() gives a rectangle', lClip.Shaped);
  AssertEquals('top', 1.0, lClip.Top.Offset.Value, 0.0001);
  AssertEquals('left', 4.0, lClip.Left.Offset.Value, 0.0001);
end;


procedure TTestSVGClipProperty.TestWhitespaceSeparatesTheSides;

var
  lClip: TSVGClipShape;

begin
  lClip := ClipOf('clip="rect(1 2 3 4)"');
  AssertTrue('rect() without commas gives a rectangle', lClip.Shaped);
  AssertEquals('top', 1.0, lClip.Top.Offset.Value, 0.0001);
  AssertEquals('left', 4.0, lClip.Left.Offset.Value, 0.0001);
end;


procedure TTestSVGClipProperty.TestTheSidesAreReadTopRightBottomLeft;

var
  lClip: TSVGClipShape;

begin
  lClip := ClipOf('clip="rect(1, 2, 3, 4)"');
  AssertEquals('top comes first', 1.0, lClip.Top.Offset.Value, 0.0001);
  AssertEquals('then right', 2.0, lClip.Right.Offset.Value, 0.0001);
  AssertEquals('then bottom', 3.0, lClip.Bottom.Offset.Value, 0.0001);
  AssertEquals('then left', 4.0, lClip.Left.Offset.Value, 0.0001);
end;


procedure TTestSVGClipProperty.TestOneSideMayBeAuto;

var
  lClip: TSVGClipShape;

begin
  lClip := ClipOf('clip="rect(auto, 2, auto, 4)"');
  AssertTrue('the top is auto', lClip.Top.IsAuto);
  AssertFalse('the right is not', lClip.Right.IsAuto);
  AssertTrue('the bottom is auto', lClip.Bottom.IsAuto);
  AssertEquals('and the left keeps its offset', 4.0,
    lClip.Left.Offset.Value, 0.0001);
end;


procedure TTestSVGClipProperty.TestAStylesheetSetsIt;

var
  lClip: TSVGClipShape;

begin
  lClip := ClipOf('clip: rect(5, 6, 7, 8)');
  AssertTrue('a rule sets the clip as well as an attribute', lClip.Shaped);
  AssertEquals('top', 5.0, lClip.Top.Offset.Value, 0.0001);
end;


procedure TTestSVGClipProperty.TestTooFewSidesLeaveTheValueAlone;

begin
  AssertFalse('three sides are not a rectangle',
    ClipOf('clip="rect(1,2,3)"').Shaped);
  AssertFalse('and neither are five',
    ClipOf('clip="rect(1,2,3,4,5)"').Shaped);
end;


procedure TTestSVGClipProperty.TestAMissingBracketLeavesTheValueAlone;

begin
  AssertFalse('an unclosed rect() is not a rectangle',
    ClipOf('clip="rect(1,2,3,4"').Shaped);
  AssertFalse('and neither is a bare list',
    ClipOf('clip="1,2,3,4"').Shaped);
end;


procedure TTestSVGClipProperty.TestItIsNotInherited;

var
  lStyle: TSVGComputedStyle;

begin
  FreeAndNil(FStyle);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="200" height="200">'
    + '<g clip="rect(1,2,3,4)">'
    + '<image id="i" x="0" y="0" width="10" height="10"/></g></svg>');
  FStyle := TSVGStyleResolver.Create;
  FStyle.LoadDocument(FDocument);
  lStyle := FStyle.ComputeStyleOf(FDocument.ElementByID('i'));
  AssertFalse('a clip on the parent does not reach the child',
    lStyle.Clip.Shaped);
end;


procedure TTestSVGClipProperty.TestEachSideMovesInFromTheSideItFaces;

var
  lClip: TSVGRect;

begin
  lClip := ClipOf('clip="rect(1, 2, 3, 4)"').Narrow(
    TSVGRect.Create(10, 20, 110, 80), TSVGLengthContext.Default);
  AssertEquals('the top moves down from the top', 21.0, lClip.Top, 0.0001);
  AssertEquals('the right moves in from the right', 108.0,
    lClip.Right, 0.0001);
  AssertEquals('the bottom moves up from the bottom', 77.0,
    lClip.Bottom, 0.0001);
  AssertEquals('the left moves in from the left', 14.0, lClip.Left, 0.0001);
end;


procedure TTestSVGClipProperty.TestAnAutoSideStaysOnTheViewport;

var
  lClip: TSVGRect;

begin
  lClip := ClipOf('clip="rect(auto, 2, auto, 4)"').Narrow(
    TSVGRect.Create(10, 20, 110, 80), TSVGLengthContext.Default);
  AssertEquals('the top stays on the viewport', 20.0, lClip.Top, 0.0001);
  AssertEquals('the bottom stays too', 80.0, lClip.Bottom, 0.0001);
  AssertEquals('the right still moves in', 108.0, lClip.Right, 0.0001);
end;


procedure TTestSVGClipProperty.TestCrossedSidesClipEverythingAway;

var
  lClip: TSVGRect;

begin
  lClip := ClipOf('clip="rect(60, 80, 1, 90)"').Narrow(
    TSVGRect.Create(10, 20, 110, 80), TSVGLengthContext.Default);
  AssertTrue('sides that cross leave nothing to draw in', lClip.IsEmpty);
end;


procedure TTestSVGClipProperty.TestAutoClipsToTheViewportItself;

var
  lClip: TSVGRect;

begin
  lClip := ClipOf('').Narrow(
    TSVGRect.Create(10, 20, 110, 80), TSVGLengthContext.Default);
  AssertEquals('left', 10.0, lClip.Left, 0.0001);
  AssertEquals('top', 20.0, lClip.Top, 0.0001);
  AssertEquals('right', 110.0, lClip.Right, 0.0001);
  AssertEquals('bottom', 80.0, lClip.Bottom, 0.0001);
end;


initialization
  RegisterTest('cascade', TTestSVGCascade);
  RegisterTest('cascade', TTestSVGFontFaceRules);
  RegisterTest('cascade', TTestSVGClipProperty);
end.
