{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the faces read from the font elements of a document.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgsvgfont;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test, FpcUnit.Registry,
     svggoldens, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.svgfont;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit, testregistry, svggoldens, fpsvg.types,
     fpsvg.dom, fpsvg.read, fpsvg.svgfont;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGFontElement = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FFace: TSVGFontFace;
    // Reads a document and takes the face of its first font element.
    procedure LoadSource(const aText: String);
    // The outline of the glyph for a code point, at the given size.
    function OutlineOf(aCodePoint: Cardinal; aSize: Double): TSVGPath;
  protected
    procedure TearDown; override;
  published
    procedure TestTheFamilyIsRead;
    procedure TestTheGlyphsAreFound;
    procedure TestACodePointOutsideTheFaceIsNotFound;
    procedure TestTheAdvanceOfAGlyphIsRead;
    procedure TestAGlyphWithoutAnAdvanceTakesTheFontOne;
    procedure TestTheEmIsRead;
    procedure TestAFaceWithoutAnEmIsAThousand;
    procedure TestTheAscentAndDescentAreRead;
    procedure TestALigatureIsKept;
    procedure TestTheLongestGlyphOfARunWins;
    procedure TestAGlyphWithoutAUnicodeIsKeptUnderItsName;
    procedure TestAGlyphNamingNothingIsLeftOut;
    procedure TestTheOutlineIsScaledToTheSize;
    procedure TestTheOutlineIsTurnedTheWayUserSpaceRuns;
    procedure TestAGlyphWithoutAnOutlineDrawsNothing;
    procedure TestTheFontElementsOfADocumentAreFound;
    procedure TestTheWeightAndStyleOfAFaceAreRead;
    procedure TestAFaceThatNamesNoWeightIsANormalOne;
    procedure TestTheProviderAnswersWithADeclaredFace;
    procedure TestTheProviderPassesOnAFamilyItHasNot;
    procedure TestADeclaredFaceAnswersEveryWeightAskedOfIt;
    procedure TestAFaceDeclaredTwiceAtOneWeightKeepsTheFirst;
    procedure TestAFamilyMayDeclareAFacePerWeight;
    procedure TestAnItalicRequestTakesTheItalicFace;
    procedure TestAFamilyMayDeclareAFacePerVariant;
    procedure TestAFamilyWithoutTheSlantAskedForIsPassedOver;
    procedure TestTheFirstFamilyAnsweringTheSlantWins;
    procedure TestADeclaredFaceStandsInWhereNoFamilyAnswers;
    procedure TestAnObliqueRequestPrefersTheObliqueFace;
    procedure TestAnObliqueRequestTakesAnItalicFaceWhenThatIsAll;
    procedure TestTheVariantOutranksTheWeight;
    procedure TestASlantIsFurtherOffThanAnyWeight;
    procedure TestAWeightWithNoFaceOfItsOwnTakesTheNearest;
    procedure TestAPairIsSetCloserByWhatTheKernSays;
    procedure TestAKernNamesItsGlyphsByNameToo;
    procedure TestAKernOverARangeReachesEveryPairInIt;
    procedure TestAKernOverAWildcardReachesEveryDigit;
    procedure TestAPairTheFaceDoesNotKernIsLeftAlone;
    procedure TestACodePointWithNoGlyphDrawsTheMissingOne;
    procedure TestAFaceWithoutAMissingGlyphAnswersNothing;
    procedure TestACodePointOutsideTheRangeIsPassedOn;
    procedure TestAVerticalPairIsReadAndKeptApartFromTheOthers;
    procedure TestAFaceWithNoVerticalPairsKernsNoneOfThem;
    procedure TestAGlyphStatingNoVerticalAdvanceMovesThePenAnEm;
    procedure TestTheVerticalOriginIsHalfAnAdvanceAcrossAndAnAscentUp;
    procedure TestAGlyphStatingItsOwnVerticalAdvanceIsGivenIt;
    procedure TestAGlyphStatingItsOwnVerticalOriginIsGivenIt;
    procedure TestAGlyphStatingNoneTakesTheVerticalAdvanceOfTheFont;
    procedure TestAGlyphStatingNoneTakesTheVerticalOriginOfTheFont;
    procedure TestAFaceSaysWhetherItDrawsSmallCapitals;
  end;

implementation

const
  Delta = 1e-9;

// A font that states the values for text running down the page, both on
// the font itself and on the first glyph.
function VerticalFont: String;

begin
  Result := '<font horiz-adv-x="500" vert-adv-y="900" vert-origin-x="150"'
    + ' vert-origin-y="700" id="f">'
    + '<font-face font-family="Probe" units-per-em="1000" ascent="800"'
    + ' descent="-200"/>'
    + '<glyph unicode="A" horiz-adv-x="600" vert-adv-y="1200"'
    + ' vert-origin-x="400" vert-origin-y="750" d="M0 0H500V1000H0Z"/>'
    + '<glyph unicode="B" d="M0 0H100V200H0Z"/>'
    + '</font>';
end;


// A document holding one font element, with whatever the caller puts in it.
function Doc(const aFont: String): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" width="100" '
    + 'height="100"><defs>' + aFont + '</defs></svg>';
end;


// A font of two glyphs: a square em and a narrow bar.
function TwoGlyphFont: String;

begin
  Result := '<font horiz-adv-x="500" id="f">'
    + '<font-face font-family="Probe" units-per-em="1000" ascent="800"'
    + ' descent="-200"/>'
    + '<glyph unicode="A" horiz-adv-x="600" d="M0 0H500V1000H0Z"/>'
    + '<glyph unicode="B" d="M0 0H100V200H0Z"/>'
    + '</font>';
end;


procedure TTestSVGFontElement.LoadSource(const aText: String);

var
  lFonts: TSVGFontElementArray;

begin
  FreeAndNil(FFace);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(aText);
  lFonts := SVGFontElementsOf(FDocument);
  AssertTrue('the document holds a font element', Length(lFonts) > 0);
  FFace := TSVGFontFace.Create(lFonts[0]);
end;


function TTestSVGFontElement.OutlineOf(aCodePoint: Cardinal;
  aSize: Double): TSVGPath;

var
  lFont: ISVGFont;

begin
  lFont := TSVGDocumentFont.Create(FFace, aSize);
  Result := TSVGPath.Create;
  lFont.GetGlyphOutline(lFont.GetGlyphIndex(aCodePoint), Result);
end;


procedure TTestSVGFontElement.TearDown;

begin
  FreeAndNil(FFace);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


// A font of three glyphs that kerns some of them.
function KerningFont: String;

begin
  Result := '<font horiz-adv-x="500">'
    + '<font-face font-family="Kerned" units-per-em="1000"/>'
    + '<glyph unicode="A" glyph-name="ay" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="V" glyph-name="vee" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="1" glyph-name="one" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="2" glyph-name="two" d="M0 0H500V500H0Z"/>'
    + '<hkern u1="A" u2="V" k="80"/>'
    + '<hkern g1="vee" g2="ay" k="40"/>'
    + '</font>';
end;


procedure TTestSVGFontElement.TestAPairIsSetCloserByWhatTheKernSays;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(KerningFont));
  lFont := TSVGDocumentFont.Create(FFace, 100);
  // Eighty of a thousand-unit em, drawn at a hundred.
  AssertEquals('the pair is set eight closer', 8.0,
    lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('A')),
      lFont.GetGlyphIndex(Ord('V'))), Delta);
end;


procedure TTestSVGFontElement.TestAKernNamesItsGlyphsByNameToo;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(KerningFont));
  lFont := TSVGDocumentFont.Create(FFace, 100);
  AssertEquals('a pair given by its glyph names is read as well', 4.0,
    lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('V')),
      lFont.GetGlyphIndex(Ord('A'))), Delta);
end;


procedure TTestSVGFontElement.TestAKernOverARangeReachesEveryPairInIt;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc('<font horiz-adv-x="500">'
    + '<font-face font-family="Ranged" units-per-em="1000"/>'
    + '<glyph unicode="1" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="2" d="M0 0H500V500H0Z"/>'
    + '<hkern u1="U+0030-0039" u2="U+0030-0039" k="50"/></font>'));
  lFont := TSVGDocumentFont.Create(FFace, 100);
  AssertEquals('a pair inside the range is kerned', 5.0,
    lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('1')),
      lFont.GetGlyphIndex(Ord('2'))), Delta);
end;


procedure TTestSVGFontElement.TestAKernOverAWildcardReachesEveryDigit;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc('<font horiz-adv-x="500">'
    + '<font-face font-family="Wild" units-per-em="1000"/>'
    + '<glyph unicode="1" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="2" d="M0 0H500V500H0Z"/>'
    + '<hkern u1="U+003?" u2="U+003?" k="60"/></font>'));
  lFont := TSVGDocumentFont.Create(FFace, 100);
  AssertEquals('the question mark matches any digit of the range', 6.0,
    lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('1')),
      lFont.GetGlyphIndex(Ord('2'))), Delta);
end;


procedure TTestSVGFontElement.TestAPairTheFaceDoesNotKernIsLeftAlone;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(KerningFont));
  lFont := TSVGDocumentFont.Create(FFace, 100);
  AssertEquals('a pair the face says nothing about is left alone', 0.0,
    lFont.GetGlyphKerning(lFont.GetGlyphIndex(Ord('1')),
      lFont.GetGlyphIndex(Ord('2'))), Delta);
end;


procedure TTestSVGFontElement.TestACodePointWithNoGlyphDrawsTheMissingOne;

var
  lFont: ISVGFont;
  lPath: TSVGPath;

begin
  LoadSource(Doc('<font horiz-adv-x="500">'
    + '<font-face font-family="Boxed" units-per-em="1000"/>'
    + '<missing-glyph horiz-adv-x="400" d="M0 0H400V600H0Z"/>'
    + '<glyph unicode="A" d="M0 0H500V500H0Z"/></font>'));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertTrue('a code point the face has no glyph for still resolves',
    lFont.GetGlyphIndex(Ord('Z')) <> 0);
  AssertEquals('and takes the room the missing glyph asks for', 4.0,
    lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('Z'))), Delta);
  lPath := TSVGPath.Create;
  try
    AssertTrue('and draws it', lFont.GetGlyphOutline(
      lFont.GetGlyphIndex(Ord('Z')), lPath));
    AssertFalse('so something is drawn', lPath.IsEmpty);
  finally
    lPath.Free;
  end;
end;


procedure TTestSVGFontElement.TestAFaceWithoutAMissingGlyphAnswersNothing;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(TwoGlyphFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertFalse('this face draws nothing for a code point it has no glyph for',
    FFace.HasMissingGlyph);
  AssertEquals('so the code point is passed on to another face', 0,
    Integer(lFont.GetGlyphIndex(Ord('Z'))));
end;


procedure TTestSVGFontElement.TestACodePointOutsideTheRangeIsPassedOn;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc('<font horiz-adv-x="500">'
    + '<font-face font-family="Ascii" units-per-em="1000"'
    + ' unicode-range="U+0-7F"/>'
    + '<missing-glyph horiz-adv-x="400" d="M0 0H400V600H0Z"/>'
    + '<glyph unicode="A" d="M0 0H500V500H0Z"/></font>'));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertTrue('a code point inside the range resolves, drawn or missing',
    lFont.GetGlyphIndex(Ord('Z')) <> 0);
  // Outside the range the face does not answer, even though it would draw
  // the missing glyph for a code point inside the range.
  AssertEquals('one outside it is passed on to another face', 0,
    Integer(lFont.GetGlyphIndex($20AC)));
end;


procedure TTestSVGFontElement.TestAVerticalPairIsReadAndKeptApartFromTheOthers;

begin
  LoadSource(Doc('<font horiz-adv-x="500">'
    + '<font-face font-family="Stacked" units-per-em="1000"/>'
    + '<glyph unicode="A" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="V" d="M0 0H500V500H0Z"/>'
    + '<hkern u1="A" u2="V" k="80"/>'
    + '<vkern u1="A" u2="V" k="30"/></font>'));
  AssertEquals('the pair along the line is read', 1, FFace.KernCount);
  AssertEquals('and the one down the column apart from it', 1,
    FFace.VerticalKernCount);
  AssertEquals('each keeps its own value', 80.0,
    FFace.Kerning(FFace.IndexOf(Ord('A')), FFace.IndexOf(Ord('V'))), Delta);
  AssertEquals('and neither answers for the other', 30.0,
    FFace.VerticalKerning(FFace.IndexOf(Ord('A')),
      FFace.IndexOf(Ord('V'))), Delta);
end;


procedure TTestSVGFontElement.TestAFaceWithNoVerticalPairsKernsNoneOfThem;

begin
  LoadSource(Doc(KerningFont));
  AssertEquals('a face without a vertical pair has none', 0,
    FFace.VerticalKernCount);
  AssertEquals('and sets nothing closer down a column', 0.0,
    FFace.VerticalKerning(FFace.IndexOf(Ord('A')),
      FFace.IndexOf(Ord('V'))), Delta);
end;


procedure TTestSVGFontElement.TestTheWeightAndStyleOfAFaceAreRead;

begin
  LoadSource(Doc('<font horiz-adv-x="500">'
    + '<font-face font-family="Heavy" font-weight="bold"'
    + ' font-style="italic"/>'
    + '<glyph unicode="A" d="M0 0H500V500H0Z"/></font>'));
  AssertEquals('the weight the face gives is read', 700, FFace.Weight);
  AssertTrue('and so is the slant', FFace.Style = fnItalic);
end;


procedure TTestSVGFontElement.TestAFaceThatNamesNoWeightIsANormalOne;

begin
  LoadSource(Doc(TwoGlyphFont));
  AssertEquals('a face without a weight is a normal one', 400,
    FFace.Weight);
  AssertTrue('and an upright one', FFace.Style = fnNormal);
end;


procedure TTestSVGFontElement.TestTheProviderAnswersWithADeclaredFace;

var
  lImpl: TSVGDocumentFontProvider;
  lProvider: ISVGFontProvider;
  lFont: ISVGFont;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(TwoGlyphFont));
  lImpl := TSVGDocumentFontProvider.Create(nil);
  lProvider := lImpl;
  lImpl.AddDocument(FDocument);
  lFont := lProvider.ResolveFont(TSVGFontRequest.Create('Probe', 10));
  AssertNotNull('the face the document declares resolves', TObject(lFont));
  AssertEquals('and it is the one that was declared', 'Probe',
    lFont.GetFontName);
end;


procedure TTestSVGFontElement.TestTheProviderPassesOnAFamilyItHasNot;

var
  lImpl: TSVGDocumentFontProvider;
  lProvider: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(TwoGlyphFont));
  lImpl := TSVGDocumentFontProvider.Create(nil);
  lProvider := lImpl;
  lImpl.AddDocument(FDocument);
  // There is no provider behind this one, so a family it has no face for
  // is answered by nobody.
  AssertNull('a family the document does not declare is passed on',
    TObject(lProvider.ResolveFont(TSVGFontRequest.Create('Elsewhere', 10))));
end;


procedure TTestSVGFontElement.TestADeclaredFaceAnswersEveryWeightAskedOfIt;

var
  lImpl: TSVGDocumentFontProvider;
  lProvider: ISVGFontProvider;
  lRequest: TSVGFontRequest;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(TwoGlyphFont));
  lImpl := TSVGDocumentFontProvider.Create(nil);
  lProvider := lImpl;
  lImpl.AddDocument(FDocument);
  // A family that is declared answers every weight asked of it, and this
  // one has a single face to answer with.
  lRequest := TSVGFontRequest.Create('Probe', 10);
  lRequest.Weight := 900;
  AssertNotNull('a bold request is answered by the only face there is',
    TObject(lProvider.ResolveFont(lRequest)));
end;


procedure TTestSVGFontElement.TestAFaceDeclaredTwiceAtOneWeightKeepsTheFirst;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(TwoGlyphFont
    + '<font horiz-adv-x="900"><font-face font-family="Probe"/>'
    + '<glyph unicode="A" d="M0 0H900V900H0Z"/></font>'));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('a repeat of the same weight and slant is dropped', 1,
    lProvider.FaceCount);
  AssertEquals('and it is the face that was declared first', 6.0,
    lHeld.ResolveFont(TSVGFontRequest.Create('Probe', 10)).GetGlyphAdvance(
      lHeld.ResolveFont(TSVGFontRequest.Create('Probe', 10))
        .GetGlyphIndex(Ord('A'))), Delta);
end;


procedure TTestSVGFontElement.TestTheFamilyIsRead;

begin
  LoadSource(Doc(TwoGlyphFont));
  AssertEquals('the family comes from the font-face element', 'Probe',
    FFace.Family);
end;


procedure TTestSVGFontElement.TestTheGlyphsAreFound;

begin
  LoadSource(Doc(TwoGlyphFont));
  AssertEquals('both glyphs are read', 2, FFace.GlyphCount);
  AssertTrue('the first is found by its code point',
    FFace.IndexOf(Ord('A')) > 0);
  AssertTrue('and so is the second', FFace.IndexOf(Ord('B')) > 0);
  AssertTrue('and they are not the same glyph',
    FFace.IndexOf(Ord('A')) <> FFace.IndexOf(Ord('B')));
end;


procedure TTestSVGFontElement.TestACodePointOutsideTheFaceIsNotFound;

begin
  LoadSource(Doc(TwoGlyphFont));
  AssertEquals('a code point the face has no glyph for is not found', 0,
    Integer(FFace.IndexOf(Ord('Z'))));
end;


procedure TTestSVGFontElement.TestTheAdvanceOfAGlyphIsRead;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(TwoGlyphFont));
  // Six hundred of a thousand-unit em, drawn at ten.
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertEquals('the advance is the share of the em it is written as', 6.0,
    lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('A'))), Delta);
end;


procedure TTestSVGFontElement.TestAGlyphWithoutAnAdvanceTakesTheFontOne;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(TwoGlyphFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertEquals('the advance of the font element is used instead', 5.0,
    lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('B'))), Delta);
end;


procedure TTestSVGFontElement.TestTheEmIsRead;

begin
  LoadSource(Doc(TwoGlyphFont));
  AssertEquals('the em is the one the face gives', 1000, FFace.UnitsPerEm);
end;


procedure TTestSVGFontElement.TestAFaceWithoutAnEmIsAThousand;

begin
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Bare"/>'
    + '<glyph unicode="A" d="M0 0H500V500H0Z"/></font>'));
  AssertEquals('a face without an em is read as a thousand', 1000,
    FFace.UnitsPerEm);
end;


procedure TTestSVGFontElement.TestTheAscentAndDescentAreRead;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(TwoGlyphFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertEquals('the ascent is scaled to the size', 8.0, lFont.GetAscent,
    Delta);
  // The attribute is negative and the descent is reported as a depth.
  AssertEquals('the descent is turned the right way up', 2.0,
    lFont.GetDescent, Delta);
end;


procedure TTestSVGFontElement.TestALigatureIsKept;

var
  lCodes: TSVGCodePointArray;
  lCount: Integer;

begin
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Lig"/>'
    + '<glyph unicode="A" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="fi" horiz-adv-x="600" d="M0 0H500V500H0Z"/></font>'));
  AssertEquals('the glyph covering two characters is kept', 2,
    FFace.GlyphCount);
  lCodes := TSVGCodePointArray.Create(Ord('f'), Ord('i'), Ord('A'));
  AssertTrue('and the two are set with it',
    FFace.IndexOfRun(lCodes, 0, lCount) <> 0);
  AssertEquals('taking both of them', 2, lCount);
  AssertTrue('while a character of its own is a glyph too',
    FFace.IndexOfRun(lCodes, 2, lCount) <> 0);
  AssertEquals('taking just the one', 1, lCount);
end;


procedure TTestSVGFontElement.TestTheLongestGlyphOfARunWins;

var
  lCodes: TSVGCodePointArray;
  lCount: Integer;
  lShort, lLong: Cardinal;

begin
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Lig"/>'
    + '<glyph unicode="f" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="fi" d="M0 0H500V500H0Z"/>'
    + '<glyph unicode="ffl" d="M0 0H500V500H0Z"/></font>'));
  lCodes := TSVGCodePointArray.Create(Ord('f'), Ord('f'), Ord('l'));
  lLong := FFace.IndexOfRun(lCodes, 0, lCount);
  AssertEquals('the three characters are set with one glyph', 3, lCount);
  lCodes := TSVGCodePointArray.Create(Ord('f'), Ord('i'));
  lShort := FFace.IndexOfRun(lCodes, 0, lCount);
  AssertEquals('and the two with another', 2, lCount);
  AssertTrue('which is not the same glyph', lShort <> lLong);
end;


procedure TTestSVGFontElement.TestAGlyphWithoutAUnicodeIsKeptUnderItsName;

begin
  // An altGlyph reaches a glyph by its id, and such a glyph covers no
  // character at all.
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Named"/>'
    + '<glyph id="swash" d="M0 0H500V500H0Z"/></font>'));
  AssertEquals('a glyph covering no character is kept', 1,
    FFace.GlyphCount);
  AssertTrue('and is found by its id', FFace.IndexOfName('swash') <> 0);
  AssertEquals('but to no code point', 0, FFace.IndexOf(Ord('A')));
end;


procedure TTestSVGFontElement.TestAGlyphNamingNothingIsLeftOut;

begin
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Named"/>'
    + '<glyph d="M0 0H500V500H0Z"/></font>'));
  AssertEquals('a glyph nothing can ask for is left out', 0,
    FFace.GlyphCount);
end;


procedure TTestSVGFontElement.TestTheOutlineIsScaledToTheSize;

var
  lPath: TSVGPath;

begin
  LoadSource(Doc(TwoGlyphFont));
  // The glyph is a rectangle 500 wide in a thousand-unit em, so at a size
  // of twenty it is ten wide.
  lPath := OutlineOf(Ord('A'), 20);
  try
    AssertEquals('the outline has the segments of the rectangle', 5,
      lPath.SegmentCount);
    AssertEquals('the far corner is half the size across', 10.0,
      lPath.ControlBounds.Right, Delta);
  finally
    lPath.Free;
  end;
end;


procedure TTestSVGFontElement.TestTheOutlineIsTurnedTheWayUserSpaceRuns;

var
  lPath: TSVGPath;

begin
  LoadSource(Doc(TwoGlyphFont));
  // The design grid has y running up from the baseline and user space has
  // it running down, so the glyph sits above the origin.
  lPath := OutlineOf(Ord('A'), 20);
  try
    AssertEquals('the top of the glyph is a whole size above the origin',
      -20.0, lPath.ControlBounds.Top, Delta);
    AssertEquals('and its foot sits on the origin', 0.0,
      lPath.ControlBounds.Bottom, Delta);
  finally
    lPath.Free;
  end;
end;


procedure TTestSVGFontElement.TestAGlyphWithoutAnOutlineDrawsNothing;

var
  lFont: ISVGFont;
  lPath: TSVGPath;

begin
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Sp"/>'
    + '<glyph unicode=" " horiz-adv-x="278"/></font>'));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  lPath := TSVGPath.Create;
  try
    AssertFalse('a glyph with no d draws nothing',
      lFont.GetGlyphOutline(lFont.GetGlyphIndex(Ord(' ')), lPath));
    AssertTrue('and leaves the path it was given empty', lPath.IsEmpty);
  finally
    lPath.Free;
  end;
end;


procedure TTestSVGFontElement.TestTheFontElementsOfADocumentAreFound;

var
  lFonts: TSVGFontElementArray;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(TwoGlyphFont
    + '<font><font-face font-family="Second"/></font>'));
  lFonts := SVGFontElementsOf(FDocument);
  AssertEquals('both font elements are found', 2, Length(lFonts));
  AssertEquals('the first gives its family', 'Probe',
    SVGFontFamilyOf(lFonts[0]));
  AssertEquals('and so does the second', 'Second',
    SVGFontFamilyOf(lFonts[1]));
end;

// A face of the family Probe, with a square em whose width says which of
// them answered, and the weight and slant the caller specifies.
function NamedFaceOf(const aFamily: String; aAdvance: Integer;
  const aDescriptors: String): String;

begin
  Result := '<font horiz-adv-x="' + IntToStr(aAdvance) + '">'
    + '<font-face font-family="' + aFamily + '" units-per-em="1000"'
    + ' ascent="800" descent="-200"' + aDescriptors + '/>'
    + '<glyph unicode="A" d="M0 0H500V1000H0Z"/></font>';
end;


function FaceOf(aAdvance: Integer; const aDescriptors: String): String;

begin
  Result := NamedFaceOf('Probe', aAdvance, aDescriptors);
end;


// The advance of the A that answers a request for a list of families,
// which says which declared face came back.
function AdvanceOfList(aProvider: ISVGFontProvider; const aFamilies: String;
  aStyle: TSVGFontStyle): Double;

var
  lRequest: TSVGFontRequest;
  lFont: ISVGFont;

begin
  lRequest := TSVGFontRequest.Create(aFamilies, 1000);
  lRequest.Style := aStyle;
  lFont := aProvider.ResolveFont(lRequest);
  if lFont = nil then
    Exit(-1);
  Result := lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('A')));
end;


// The advance of the A that a request is answered with, which says which
// declared face came back.
function AdvanceOf(aProvider: ISVGFontProvider; aWeight: Integer;
  aStyle: TSVGFontStyle): Double;

var
  lRequest: TSVGFontRequest;
  lFont: ISVGFont;

begin
  lRequest := TSVGFontRequest.Create('Probe', 1000);
  lRequest.Weight := aWeight;
  lRequest.Style := aStyle;
  lFont := aProvider.ResolveFont(lRequest);
  if lFont = nil then
    Exit(-1);
  Result := lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('A')));
end;


procedure TTestSVGFontElement.TestAFamilyMayDeclareAFacePerWeight;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, ' font-weight="400"') + FaceOf(700, ' font-weight="700"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('both faces of the family are held', 2, lProvider.FaceCount);
  AssertEquals('the regular request takes the regular face', 400,
    AdvanceOf(lHeld, 400, fnNormal), Delta);
  AssertEquals('and the bold one takes the bold face', 700,
    AdvanceOf(lHeld, 700, fnNormal), Delta);
end;


// The advance of the A that a request of a variant is answered with.
function AdvanceOfVariant(aProvider: ISVGFontProvider; aWeight: Integer;
  aStyle: TSVGFontStyle; aVariant: TSVGFontVariant): Double;

var
  lRequest: TSVGFontRequest;
  lFont: ISVGFont;

begin
  lRequest := TSVGFontRequest.Create('Probe', 1000);
  lRequest.Weight := aWeight;
  lRequest.Style := aStyle;
  lRequest.Variant := aVariant;
  lFont := aProvider.ResolveFont(lRequest);
  if lFont = nil then
    Exit(-1);
  Result := lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('A')));
end;


procedure TTestSVGFontElement.TestAFamilyMayDeclareAFacePerVariant;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, '')
    + FaceOf(500, ' font-variant="small-caps"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('both faces of the family are held', 2, lProvider.FaceCount);
  AssertEquals('a plain request takes the plain face', 400,
    AdvanceOfVariant(lHeld, 400, fnNormal, fvNormal), Delta);
  AssertEquals('and one asking for small capitals takes the face that '
    + 'draws them', 500,
    AdvanceOfVariant(lHeld, 400, fnNormal, fvSmallCaps), Delta);
end;


procedure TTestSVGFontElement.TestAFamilyWithoutTheSlantAskedForIsPassedOver;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    NamedFaceOf('One', 400, ' font-style="italic"')
    + NamedFaceOf('Two', 500, ' font-style="oblique"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('the italic of the first family does not answer an oblique '
    + 'request, so the oblique of the second does', 500,
    AdvanceOfList(lHeld, 'One,Two', fnOblique), Delta);
end;


procedure TTestSVGFontElement.TestTheFirstFamilyAnsweringTheSlantWins;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    NamedFaceOf('One', 400, ' font-style="italic"')
    + NamedFaceOf('Two', 500, ' font-style="oblique"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('an italic request takes the italic of the first family',
    400, AdvanceOfList(lHeld, 'One,Two', fnItalic), Delta);
  AssertEquals('and an upright one is answered by neither, so the face the '
    + 'document declares first stands in', 400,
    AdvanceOfList(lHeld, 'One,Two', fnNormal), Delta);
end;


procedure TTestSVGFontElement.TestADeclaredFaceStandsInWhereNoFamilyAnswers;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    NamedFaceOf('One', 400, ' font-style="italic"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('with no other family and no fonts under it, the face the '
    + 'document wrote is kept', 400,
    AdvanceOfList(lHeld, 'One', fnOblique), Delta);
end;


procedure TTestSVGFontElement.TestAnObliqueRequestPrefersTheObliqueFace;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, ' font-style="italic"')
    + FaceOf(500, ' font-style="oblique"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('an oblique request takes the oblique face', 500,
    AdvanceOf(lHeld, 400, fnOblique), Delta);
  AssertEquals('and an italic one the italic face', 400,
    AdvanceOf(lHeld, 400, fnItalic), Delta);
end;


procedure TTestSVGFontElement.TestAnObliqueRequestTakesAnItalicFaceWhenThatIsAll;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, '') + FaceOf(500, ' font-style="italic"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('a slanted face stands closer than an upright one', 500,
    AdvanceOf(lHeld, 400, fnOblique), Delta);
end;


procedure TTestSVGFontElement.TestTheVariantOutranksTheWeight;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, ' font-weight="400"')
    + FaceOf(500, ' font-weight="700" font-variant="small-caps"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('the variant is settled before the weight, so the face that '
    + 'draws small capitals wins the weight it is not', 500,
    AdvanceOfVariant(lHeld, 400, fnNormal, fvSmallCaps), Delta);
end;


procedure TTestSVGFontElement.TestAnItalicRequestTakesTheItalicFace;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, ' font-weight="400"')
    + FaceOf(500, ' font-weight="400" font-style="italic"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('an upright request takes the upright face', 400,
    AdvanceOf(lHeld, 400, fnNormal), Delta);
  AssertEquals('and a slanted one takes the slanted face', 500,
    AdvanceOf(lHeld, 400, fnItalic), Delta);
end;


procedure TTestSVGFontElement.TestASlantIsFurtherOffThanAnyWeight;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  // The upright face is three hundred nearer in weight, and the slanted
  // one is still chosen: a face of the wrong slant is never the nearest.
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(400, ' font-weight="400"')
    + FaceOf(700, ' font-weight="700" font-style="italic"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('the slanted face resolves a slanted request', 700,
    AdvanceOf(lHeld, 400, fnItalic), Delta);
end;


procedure TTestSVGFontElement.TestAWeightWithNoFaceOfItsOwnTakesTheNearest;

var
  lProvider: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(
    FaceOf(300, ' font-weight="300"') + FaceOf(800, ' font-weight="800"')));
  lProvider := TSVGDocumentFontProvider.Create(nil);
  lHeld := lProvider;
  lProvider.AddDocument(FDocument);
  AssertEquals('400 is nearer the light face', 300,
    AdvanceOf(lHeld, 400, fnNormal), Delta);
  AssertEquals('700 is nearer the heavy one', 800,
    AdvanceOf(lHeld, 700, fnNormal), Delta);
  AssertEquals('and 900 is nearer it still', 800,
    AdvanceOf(lHeld, 900, fnNormal), Delta);
end;


procedure TTestSVGFontElement.TestAGlyphStatingNoVerticalAdvanceMovesThePenAnEm;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(TwoGlyphFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertEquals('a font stating none moves the pen a whole em', 10.0,
    lFont.GetGlyphVerticalAdvance(lFont.GetGlyphIndex(Ord('A'))), Delta);
end;


procedure TTestSVGFontElement.TestTheVerticalOriginIsHalfAnAdvanceAcrossAndAnAscentUp;

var
  lFont: ISVGFont;
  lX, lY: Double;

begin
  LoadSource(Doc(TwoGlyphFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  lFont.GetGlyphVerticalOrigin(lFont.GetGlyphIndex(Ord('A')), lX, lY);
  // Six hundred of a thousand-unit em drawn at ten, so half of it is three.
  AssertEquals('the glyph steps back half of its own advance', -3.0,
    lX, Delta);
  AssertEquals('and hangs from the ascent of the face', 8.0, lY, Delta);
  lFont.GetGlyphVerticalOrigin(lFont.GetGlyphIndex(Ord('B')), lX, lY);
  AssertEquals('a glyph of another width steps back half of that',
    -2.5, lX, Delta);
end;


procedure TTestSVGFontElement.TestAGlyphStatingItsOwnVerticalAdvanceIsGivenIt;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(VerticalFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertEquals('the glyph is given the advance it states', 12.0,
    lFont.GetGlyphVerticalAdvance(lFont.GetGlyphIndex(Ord('A'))), Delta);
end;


procedure TTestSVGFontElement.TestAGlyphStatingItsOwnVerticalOriginIsGivenIt;

var
  lFont: ISVGFont;
  lX, lY: Double;

begin
  LoadSource(Doc(VerticalFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  lFont.GetGlyphVerticalOrigin(lFont.GetGlyphIndex(Ord('A')), lX, lY);
  AssertEquals('the glyph steps back by the value it states', -4.0, lX, Delta);
  AssertEquals('and hangs from the value it states', 7.5, lY, Delta);
end;


procedure TTestSVGFontElement.TestAGlyphStatingNoneTakesTheVerticalAdvanceOfTheFont;

var
  lFont: ISVGFont;

begin
  LoadSource(Doc(VerticalFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  AssertEquals('the font answers for a glyph that states none', 9.0,
    lFont.GetGlyphVerticalAdvance(lFont.GetGlyphIndex(Ord('B'))), Delta);
end;


procedure TTestSVGFontElement.TestAGlyphStatingNoneTakesTheVerticalOriginOfTheFont;

var
  lFont: ISVGFont;
  lX, lY: Double;

begin
  LoadSource(Doc(VerticalFont));
  lFont := TSVGDocumentFont.Create(FFace, 10);
  lFont.GetGlyphVerticalOrigin(lFont.GetGlyphIndex(Ord('B')), lX, lY);
  // Half the advance of the glyph would stand it back 2.5, so this is the
  // font answering and not the default.
  AssertEquals('the font answers for a glyph that states none', -1.5, lX, Delta);
  AssertEquals('and hangs it from the value the font states', 7.0, lY, Delta);
end;

procedure TTestSVGFontElement.TestAFaceSaysWhetherItDrawsSmallCapitals;

begin
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Plain"'
    + ' units-per-em="1000"/><glyph unicode="a" d="M0 0H500V500H0Z"/>'
    + '</font>'));
  AssertFalse('a face saying nothing draws none of its own',
    TSVGDocumentFont.Create(FFace, 10).GetSmallCaps);
  LoadSource(Doc('<font horiz-adv-x="500"><font-face font-family="Caps"'
    + ' font-variant="small-caps" units-per-em="1000"/>'
    + '<glyph unicode="a" d="M0 0H500V500H0Z"/></font>'));
  AssertTrue('and one saying so draws them itself',
    TSVGDocumentFont.Create(FFace, 10).GetSmallCaps);
end;


initialization
  RegisterTest('svgfont', TTestSVGFontElement);

end.
