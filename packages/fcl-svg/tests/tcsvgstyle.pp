{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for colour parsing, the property model and computed values.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgstyle;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, FpcUnit.Test, FpcUnit.Registry, fpsvg.types,
     fpsvg.dom, fpsvg.read, fpsvg.style;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpcunit, testregistry, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.style;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGColorGrammar = class(TTestCase)
  private
    // Fails unless aText parses to the given 8-bit components.
    procedure AssertParses(const aMessage, aText: String;
      aRed, aGreen, aBlue, aAlpha: Byte);
  published
    procedure TestShortHex;
    procedure TestShortHexWithAlpha;
    procedure TestLongHex;
    procedure TestLongHexWithAlpha;
    procedure TestHexIsCaseInsensitive;
    procedure TestMalformedHexIsRejected;
    procedure TestRgbWithNumbers;
    procedure TestRgbWithPercentages;
    procedure TestRgbaAlphaIsAFraction;
    procedure TestACSS2SystemNameIsAColour;
    procedure TestASystemNameIsNotCaseSensitive;
    procedure TestASystemNameCanBeChosen;
    procedure TestResetPutsTheSystemNamesBack;
    procedure TestChoosingAnUnknownNameChangesNothing;
    procedure TestTheFallbackPaletteHangsTogether;
    procedure TestRgbClampsOutOfRange;
    procedure TestRgbWithoutClosingParenthesisIsRejected;
    procedure TestRgbWithTrailingGarbageIsRejected;
    procedure TestNamedColours;
    procedure TestNamedColoursAreCaseInsensitive;
    procedure TestGreyAndGrayBothResolve;
    procedure TestUnknownNameIsRejected;
    procedure TestEmptyIsRejected;
    procedure TestWhitespaceIsTolerated;
  end;

  TTestSVGPaintGrammar = class(TTestCase)
  private
    FDocument: TSVGDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNoneIsNoPaint;
    procedure TestColourPaint;
    procedure TestCurrentColorTakesTheGivenColour;
    procedure TestUrlResolvesAgainstTheDocument;
    procedure TestUnresolvableUrlIsRejected;
    procedure TestGarbageIsRejected;
  end;

  TTestSVGPropertyModel = class(TTestCase)
  published
    procedure TestNameRoundTrip;
    procedure TestUnknownNameIsRejected;
    procedure TestInheritedPropertySet;
    procedure TestInitialValues;
    procedure TestInheritResetsNonInheritedProperties;
    procedure TestInheritKeepsInheritedProperties;
    procedure TestDisplayAndPaintPredicates;
  end;

implementation

{ TTestSVGColorGrammar }

procedure TTestSVGColorGrammar.AssertParses(const aMessage, aText: String;
  aRed, aGreen, aBlue, aAlpha: Byte);

var
  lColor: TSVGColor;

begin
  AssertTrue(aMessage + ' (parses)', lColor.TryParse(aText));
  AssertEquals(aMessage + ' (red)', aRed * 257, lColor.Red);
  AssertEquals(aMessage + ' (green)', aGreen * 257, lColor.Green);
  AssertEquals(aMessage + ' (blue)', aBlue * 257, lColor.Blue);
  AssertEquals(aMessage + ' (alpha)', aAlpha * 257, lColor.Alpha);
end;


procedure TTestSVGColorGrammar.TestACSS2SystemNameIsAColour;

begin
  // These names are system dependent by definition. All that matters here
  // is that they parse to a colour instead of failing the declaration.
  AssertParses('Window is the paper a window is drawn on', 'Window',
    255, 255, 255, 255);
  AssertParses('WindowText is what is written on it', 'WindowText',
    0, 0, 0, 255);
  AssertParses('and ThreeDFace is the face of a control', 'ThreeDFace',
    212, 208, 200, 255);
end;


procedure TTestSVGColorGrammar.TestASystemNameIsNotCaseSensitive;

begin
  AssertParses('however it is written', 'wInDoWtExT', 0, 0, 0, 255);
end;


procedure TTestSVGColorGrammar.TestASystemNameCanBeChosen;

var
  lColor: TSVGColor;

begin
  try
    AssertTrue('the name is one of them',
      SVGSetSystemColor('Window', TSVGColor.FromBytes(1, 2, 3, 255)));
    AssertTrue('and it reads back', SVGSystemColor('Window', lColor));
    AssertEquals('and holds the value it was set to', 1 * 257, lColor.Red);
    AssertParses('and a document parsing the name gets it too', 'Window',
      1, 2, 3, 255);
  finally
    SVGResetSystemColors;
  end;
end;


procedure TTestSVGColorGrammar.TestResetPutsTheSystemNamesBack;

begin
  SVGSetSystemColor('Window', TSVGColor.FromBytes(1, 2, 3, 255));
  SVGResetSystemColors;
  AssertParses('the name has its built-in colour again', 'Window',
    255, 255, 255, 255);
end;


procedure TTestSVGColorGrammar.TestChoosingAnUnknownNameChangesNothing;

var
  lColor: TSVGColor;

begin
  AssertFalse('a name that is not one of them cannot be set',
    SVGSetSystemColor('NoSuchSystemColour', TSVGColor.Black));
  AssertFalse('and reads back as nothing',
    SVGSystemColor('NoSuchSystemColour', lColor));
  AssertFalse('nor does it parse as a colour',
    lColor.TryParse('NoSuchSystemColour'));
end;


// How light a system colour is, used to put the shadows in order.
function ToneOf(const aName: String): Double;

var
  lColor: TSVGColor;

begin
  Result := 0;
  if not SVGSystemColor(aName, lColor) then
    Exit;
  Result := 0.2125 * lColor.Red + 0.7154 * lColor.Green
    + 0.0721 * lColor.Blue;
end;


procedure TTestSVGColorGrammar.TestTheFallbackPaletteHangsTogether;

  procedure AssertSame(const aMessage, aFirst, aSecond: String);
  var
    lOne, lTwo: TSVGColor;
  begin
    SVGSystemColor(aFirst, lOne);
    SVGSystemColor(aSecond, lTwo);
    AssertEquals(aMessage, lOne.Red, lTwo.Red);
    AssertEquals(aMessage, lOne.Green, lTwo.Green);
    AssertEquals(aMessage, lOne.Blue, lTwo.Blue);
  end;

  procedure AssertReadable(const aText, aGround: String);
  begin
    AssertTrue(aText + ' can be read on ' + aGround,
      Abs(ToneOf(aText) - ToneOf(aGround)) > 16000);
  end;

begin
  // No specification gives these values. The tests pin the sense they have
  // to make, not the numbers themselves.
  AssertSame('a button face is a three-d face', 'buttonface', 'threedface');
  AssertSame('a button shadow is a three-d shadow', 'buttonshadow',
    'threedshadow');
  AssertSame('a button highlight is a three-d highlight', 'buttonhighlight',
    'threedhighlight');
  AssertSame('both borders are the same chrome', 'activeborder',
    'inactiveborder');
  AssertTrue('the dark shadow is darker than the shadow',
    ToneOf('threeddarkshadow') < ToneOf('threedshadow'));
  AssertTrue('which is darker than the face',
    ToneOf('threedshadow') < ToneOf('threedface'));
  AssertTrue('and the highlight is lighter than the face',
    ToneOf('threedhighlight') > ToneOf('threedface'));
  AssertReadable('windowtext', 'window');
  AssertReadable('menutext', 'menu');
  AssertReadable('buttontext', 'buttonface');
  AssertReadable('captiontext', 'activecaption');
  AssertReadable('highlighttext', 'highlight');
  AssertReadable('infotext', 'infobackground');
end;


procedure TTestSVGColorGrammar.TestShortHex;

begin
  AssertParses('a three digit hex repeats each digit', '#f00', 255, 0, 0, 255);
  AssertParses('each short digit is multiplied by seventeen', '#abc',
    $AA, $BB, $CC, 255);
end;


procedure TTestSVGColorGrammar.TestShortHexWithAlpha;

begin
  AssertParses('a fourth short digit is the alpha', '#f008', 255, 0, 0, $88);
end;


procedure TTestSVGColorGrammar.TestLongHex;

begin
  AssertParses('a six digit hex is read byte by byte', '#ff8000',
    255, 128, 0, 255);
end;


procedure TTestSVGColorGrammar.TestLongHexWithAlpha;

begin
  AssertParses('an eight digit hex includes the alpha', '#ff800040',
    255, 128, 0, 64);
end;


procedure TTestSVGColorGrammar.TestHexIsCaseInsensitive;

begin
  AssertParses('uppercase hex digits parse', '#FF8000', 255, 128, 0, 255);
end;


procedure TTestSVGColorGrammar.TestMalformedHexIsRejected;

var
  lColor: TSVGColor;

begin
  AssertFalse('a five digit hex is not a colour', lColor.TryParse('#12345'));
  AssertFalse('a non-hex digit is rejected', lColor.TryParse('#gg0000'));
  AssertFalse('a bare hash is rejected', lColor.TryParse('#'));
end;


procedure TTestSVGColorGrammar.TestRgbWithNumbers;

begin
  AssertParses('rgb takes three components', 'rgb(255, 128, 0)',
    255, 128, 0, 255);
  AssertParses('rgb tolerates missing spaces', 'rgb(1,2,3)', 1, 2, 3, 255);
end;


procedure TTestSVGColorGrammar.TestRgbWithPercentages;

begin
  AssertParses('a percentage is taken of 255', 'rgb(100%, 0%, 50%)',
    255, 0, 128, 255);
end;


procedure TTestSVGColorGrammar.TestRgbaAlphaIsAFraction;

begin
  AssertParses('the rgba alpha is a fraction of one', 'rgba(0, 0, 0, 0.5)',
    0, 0, 0, 128);
  AssertParses('a fully opaque rgba parses', 'rgba(1, 2, 3, 1)', 1, 2, 3, 255);
end;


procedure TTestSVGColorGrammar.TestRgbClampsOutOfRange;

begin
  AssertParses('components above the range are clamped', 'rgb(300, -20, 0)',
    255, 0, 0, 255);
end;


procedure TTestSVGColorGrammar.TestRgbWithoutClosingParenthesisIsRejected;

var
  lColor: TSVGColor;

begin
  AssertFalse('an unclosed rgb is rejected', lColor.TryParse('rgb(1,2,3'));
  AssertFalse('rgb without an argument list is rejected',
    lColor.TryParse('rgb'));
end;


procedure TTestSVGColorGrammar.TestRgbWithTrailingGarbageIsRejected;

var
  lColor: TSVGColor;

begin
  AssertFalse('text after the closing parenthesis is rejected',
    lColor.TryParse('rgb(1,2,3) blue'));
end;


procedure TTestSVGColorGrammar.TestNamedColours;

begin
  AssertParses('red is a keyword', 'red', 255, 0, 0, 255);
  AssertParses('rebeccapurple is not an SVG 1.1 keyword, but blue is',
    'blue', 0, 0, 255, 255);
  AssertParses('the first name in the table resolves', 'aliceblue',
    $F0, $F8, $FF, 255);
  AssertParses('the last name in the table resolves', 'yellowgreen',
    $9A, $CD, $32, 255);
end;


procedure TTestSVGColorGrammar.TestNamedColoursAreCaseInsensitive;

begin
  AssertParses('a keyword in capitals resolves', 'RED', 255, 0, 0, 255);
  AssertParses('a keyword in mixed case resolves', 'CornflowerBlue',
    $64, $95, $ED, 255);
end;


procedure TTestSVGColorGrammar.TestGreyAndGrayBothResolve;

begin
  AssertParses('gray resolves', 'gray', $80, $80, $80, 255);
  AssertParses('grey resolves to the same colour', 'grey',
    $80, $80, $80, 255);
end;


procedure TTestSVGColorGrammar.TestUnknownNameIsRejected;

var
  lColor: TSVGColor;

begin
  AssertFalse('an unknown keyword is rejected', lColor.TryParse('nosuchcolour'));
end;


procedure TTestSVGColorGrammar.TestEmptyIsRejected;

var
  lColor: TSVGColor;

begin
  AssertFalse('an empty string is not a colour', lColor.TryParse(''));
  AssertFalse('whitespace alone is not a colour', lColor.TryParse('  '));
end;


procedure TTestSVGColorGrammar.TestWhitespaceIsTolerated;

begin
  AssertParses('surrounding whitespace is ignored', '  red  ',
    255, 0, 0, 255);
  AssertParses('whitespace inside rgb is ignored', 'rgb( 1 , 2 , 3 )',
    1, 2, 3, 255);
end;


{ TTestSVGPaintGrammar }

procedure TTestSVGPaintGrammar.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<defs><linearGradient id="grad"/></defs></svg>');
end;


procedure TTestSVGPaintGrammar.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGPaintGrammar.TestNoneIsNoPaint;

var
  lPaint: TSVGPaint;

begin
  AssertTrue('none parses', lPaint.TryParse('none', TSVGColor.Black, FDocument));
  AssertTrue('none draws nothing', lPaint.Kind = spNone);
end;


procedure TTestSVGPaintGrammar.TestColourPaint;

var
  lPaint: TSVGPaint;

begin
  AssertTrue('a colour parses as paint',
    lPaint.TryParse('#00ff00', TSVGColor.Black, FDocument));
  AssertTrue('the paint is a colour', lPaint.Kind = spColor);
  AssertEquals('the colour is kept', $FFFF, lPaint.Color.Green);
end;


procedure TTestSVGPaintGrammar.TestCurrentColorTakesTheGivenColour;

var
  lPaint: TSVGPaint;

begin
  AssertTrue('currentColor parses',
    lPaint.TryParse('currentColor', TSVGColor.FromBytes(0, 0, 255, 255),
      FDocument));
  AssertTrue('currentColor yields a colour', lPaint.Kind = spColor);
  AssertEquals('currentColor takes the colour it was given', $FFFF,
    lPaint.Color.Blue);
end;


procedure TTestSVGPaintGrammar.TestUrlResolvesAgainstTheDocument;

var
  lPaint: TSVGPaint;

begin
  AssertTrue('a url pointing to a defined element parses',
    lPaint.TryParse('url(#grad)', TSVGColor.Black, FDocument));
end;


procedure TTestSVGPaintGrammar.TestUnresolvableUrlIsRejected;

var
  lPaint: TSVGPaint;

begin
  AssertFalse('a url pointing to nothing is rejected',
    lPaint.TryParse('url(#missing)', TSVGColor.Black, FDocument));
end;


procedure TTestSVGPaintGrammar.TestGarbageIsRejected;

var
  lPaint: TSVGPaint;

begin
  AssertFalse('a word that is not a colour is rejected',
    lPaint.TryParse('sideways', TSVGColor.Black, FDocument));
end;


{ TTestSVGPropertyModel }

procedure TTestSVGPropertyModel.TestNameRoundTrip;

var
  P, lParsed: TSVGProperty;

begin
  for P := Low(TSVGProperty) to High(TSVGProperty) do
    begin
    AssertTrue('the name of every property parses back',
      TryStrToSVGProperty(SVGPropertyName(P), lParsed));
    AssertTrue('every property round-trips through its name', lParsed = P);
    end;
end;


procedure TTestSVGPropertyModel.TestUnknownNameIsRejected;

var
  lParsed: TSVGProperty;

begin
  AssertFalse('an unknown property name is rejected',
    TryStrToSVGProperty('nosuchproperty', lParsed));
end;


procedure TTestSVGPropertyModel.TestInheritedPropertySet;

begin
  AssertTrue('fill inherits', SVGPropertyInherits(prFill));
  AssertTrue('stroke-width inherits', SVGPropertyInherits(prStrokeWidth));
  AssertTrue('visibility inherits', SVGPropertyInherits(prVisibility));
  AssertFalse('opacity does not inherit', SVGPropertyInherits(prOpacity));
  AssertFalse('display does not inherit', SVGPropertyInherits(prDisplay));
end;


procedure TTestSVGPropertyModel.TestInitialValues;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := TSVGComputedStyle.Initial;
  AssertTrue('the initial fill is a colour', lStyle.Fill.Kind = spColor);
  AssertEquals('the initial fill is black', 0, lStyle.Fill.Color.Red);
  AssertEquals('the initial fill is opaque', $FFFF, lStyle.Fill.Color.Alpha);
  AssertTrue('the initial stroke is none', lStyle.Stroke.Kind = spNone);
  AssertEquals('the initial stroke width is one', 1, lStyle.Pen.Width, 1e-9);
  AssertEquals('the initial opacity is one', 1, lStyle.Opacity, 1e-9);
  AssertTrue('the initial fill rule is nonzero', lStyle.FillRule = frNonZero);
  AssertTrue('the initial visibility is visible',
    lStyle.Visibility = svVisible);
end;


procedure TTestSVGPropertyModel.TestInheritResetsNonInheritedProperties;

var
  lParent, lChild: TSVGComputedStyle;

begin
  lParent := TSVGComputedStyle.Initial;
  lParent.Opacity := 0.5;
  lParent.Display := sdNone;
  lChild := lParent.Inherit;
  AssertEquals('opacity resets to its initial value in the child', 1,
    lChild.Opacity, 1e-9);
  AssertTrue('display resets to its initial value in the child',
    lChild.Display = sdInline);
end;


procedure TTestSVGPropertyModel.TestInheritKeepsInheritedProperties;

var
  lParent, lChild: TSVGComputedStyle;

begin
  lParent := TSVGComputedStyle.Initial;
  lParent.Fill := TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255));
  lParent.Pen.Width := 4;
  lParent.Visibility := svHidden;
  lChild := lParent.Inherit;
  AssertEquals('the child inherits the fill colour', $FFFF,
    lChild.Fill.Color.Red);
  AssertEquals('the child inherits the stroke width', 4,
    lChild.Pen.Width, 1e-9);
  AssertTrue('the child inherits visibility', lChild.Visibility = svHidden);
end;


procedure TTestSVGPropertyModel.TestDisplayAndPaintPredicates;

var
  lStyle: TSVGComputedStyle;

begin
  lStyle := TSVGComputedStyle.Initial;
  AssertTrue('the initial style is displayed', lStyle.IsDisplayed);
  AssertTrue('the initial style paints', lStyle.IsPainted);
  lStyle.Visibility := svHidden;
  AssertTrue('a hidden element is still displayed', lStyle.IsDisplayed);
  AssertFalse('a hidden element does not paint', lStyle.IsPainted);
  lStyle := TSVGComputedStyle.Initial;
  lStyle.Display := sdNone;
  AssertFalse('display none is not displayed', lStyle.IsDisplayed);
  AssertFalse('display none does not paint', lStyle.IsPainted);
end;


initialization
  RegisterTest('style', TTestSVGColorGrammar);
  RegisterTest('style', TTestSVGPaintGrammar);
  RegisterTest('style', TTestSVGPropertyModel);
end.
