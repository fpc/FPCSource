{
    Tests for the colour routines of fpimage: construction, operators,
    comparison, alpha blending, gray conversion and HTML colours.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tccolor;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests;

type
  TTestColor = class(TTestCase)
  private
    procedure ParseInvalidHtml;
  published
    procedure TestFPColorWithoutAlphaIsOpaque;
    procedure TestFPColorKeepsAllFourChannels;
    procedure TestEqualityComparesAllChannels;
    procedure TestBitwiseOperatorsWorkPerChannel;
    procedure TestCompareColorsIsZeroForEqualColors;
    procedure TestCompareColorsOrdersRedGreenBlueAlpha;
    procedure TestAlphaBlendWithOpaqueTopGivesTop;
    procedure TestAlphaBlendWithTransparentTopGivesBottom;
    procedure TestAlphaBlendOnTransparentBottomGivesTop;
    procedure TestAlphaBlendHalfOverOpaque;
    procedure TestAlphaBlendNearlyOpaqueStillBlends;
    procedure TestAlphaBlendKeepsColorOfTwoEqualTranslucentColors;
    procedure TestAlphaBlendOfTwoTranslucentColors;
    procedure TestCalculateGrayOfBlackAndWhite;
    procedure TestCalculateGrayUsesLumaWeights;
    procedure TestCalculateGrayOfGrayIsThatGray;
    procedure TestHtmlLongForm;
    procedure TestHtmlShortForm;
    procedure TestHtmlNamesIgnoreCase;
    procedure TestHtmlAllSixteenNames;
    procedure TestHtmlInvalidStringsAreRejected;
    procedure TestHtmlToFPColorRaisesOnInvalid;
    procedure TestHtmlToFPColorDefGivesDefault;
    procedure TestColorConstantsMatchTheirHtmlNames;
    procedure TestPrimaryColorConstants;
    procedure TestLightAndDarkGreenDiffer;
    procedure TestTransparentConstantIsTransparent;
  end;

implementation

procedure TTestColor.ParseInvalidHtml;

begin
  HtmlToFPColor('notacolor');
end;


procedure TTestColor.TestFPColorWithoutAlphaIsOpaque;

var
  lColor: TFPColor;

begin
  lColor := FPColor(1, 2, 3);
  AssertEquals('Red is kept', 1, lColor.Red);
  AssertEquals('Green is kept', 2, lColor.Green);
  AssertEquals('Blue is kept', 3, lColor.Blue);
  AssertEquals('A colour made without alpha is opaque', alphaOpaque, lColor.Alpha);
end;


procedure TTestColor.TestFPColorKeepsAllFourChannels;

var
  lColor: TFPColor;

begin
  lColor := FPColor($1111, $2222, $3333, $4444);
  AssertEquals('Red is kept', $1111, lColor.Red);
  AssertEquals('Green is kept', $2222, lColor.Green);
  AssertEquals('Blue is kept', $3333, lColor.Blue);
  AssertEquals('Alpha is kept', $4444, lColor.Alpha);
end;


procedure TTestColor.TestEqualityComparesAllChannels;

var
  lBase: TFPColor;

begin
  lBase := FPColor(10, 20, 30, 40);
  AssertTrue('A colour equals itself', lBase = FPColor(10, 20, 30, 40));
  AssertFalse('A different red makes colours differ', lBase = FPColor(11, 20, 30, 40));
  AssertFalse('A different green makes colours differ', lBase = FPColor(10, 21, 30, 40));
  AssertFalse('A different blue makes colours differ', lBase = FPColor(10, 20, 31, 40));
  AssertFalse('A different alpha makes colours differ', lBase = FPColor(10, 20, 30, 41));
end;


procedure TTestColor.TestBitwiseOperatorsWorkPerChannel;

var
  lA, lB: TFPColor;

begin
  lA := FPColor($F0F0, $FF00, $0F0F, $1234);
  lB := FPColor($FF00, $0FF0, $00FF, $FFFF);
  AssertColorsEqual('and works per channel', FPColor($F000, $0F00, $000F, $1234), lA and lB);
  AssertColorsEqual('or works per channel', FPColor($FFF0, $FFF0, $0FFF, $FFFF), lA or lB);
  AssertColorsEqual('xor works per channel', FPColor($0FF0, $F0F0, $0FF0, $EDCB), lA xor lB);
end;


procedure TTestColor.TestCompareColorsIsZeroForEqualColors;

begin
  AssertEquals('Equal colours compare as 0', 0, CompareColors(FPColor(1, 2, 3, 4), FPColor(1, 2, 3, 4)));
end;


procedure TTestColor.TestCompareColorsOrdersRedGreenBlueAlpha;

begin
  AssertTrue('Red decides first', CompareColors(FPColor(1, 9, 9, 9), FPColor(2, 0, 0, 0)) < 0);
  AssertTrue('Green decides when red is equal', CompareColors(FPColor(1, 3, 0, 0), FPColor(1, 2, 9, 9)) > 0);
  AssertTrue('Blue decides when red and green are equal', CompareColors(FPColor(1, 2, 3, 9), FPColor(1, 2, 4, 0)) < 0);
  AssertTrue('Alpha decides last', CompareColors(FPColor(1, 2, 3, 5), FPColor(1, 2, 3, 4)) > 0);
  AssertTrue('Channels compare as unsigned words', CompareColors(FPColor($FFFF, 0, 0, 0), FPColor(0, 0, 0, 0)) > 0);
end;


procedure TTestColor.TestAlphaBlendWithOpaqueTopGivesTop;

begin
  AssertColorsEqual('An opaque top colour hides the bottom',
    colRed, AlphaBlend(colBlue, colRed));
end;


procedure TTestColor.TestAlphaBlendWithTransparentTopGivesBottom;

begin
  AssertColorsEqual('A transparent top colour shows the bottom',
    colBlue, AlphaBlend(colBlue, FPColor($FFFF, 0, 0, 0)));
end;


procedure TTestColor.TestAlphaBlendOnTransparentBottomGivesTop;

var
  lTop: TFPColor;

begin
  lTop := FPColor($FFFF, 0, 0, $8000);
  AssertColorsEqual('On a transparent bottom the top colour stays as it is',
    lTop, AlphaBlend(FPColor(0, 0, $FFFF, 0), lTop));
end;


procedure TTestColor.TestAlphaBlendHalfOverOpaque;

begin
  AssertColorsEqual('Half-transparent white over opaque black gives opaque mid gray',
    FPColor($8000, $8000, $8000, alphaOpaque),
    AlphaBlend(colBlack, FPColor($FFFF, $FFFF, $FFFF, $8000)), 1);
end;


procedure TTestColor.TestAlphaBlendNearlyOpaqueStillBlends;

begin
  AssertColorsEqual('White at alpha $FFF0 over black lets a little black through',
    FPColor($FFF0, $FFF0, $FFF0, alphaOpaque),
    AlphaBlend(colBlack, FPColor($FFFF, $FFFF, $FFFF, $FFF0)), 1);
end;


procedure TTestColor.TestAlphaBlendKeepsColorOfTwoEqualTranslucentColors;

begin
  AssertColorsEqual('Half red over half red stays pure red, at three quarters coverage',
    FPColor($FFFF, 0, 0, $C000),
    AlphaBlend(FPColor($FFFF, 0, 0, $8000), FPColor($FFFF, 0, 0, $8000)), 2);
end;


procedure TTestColor.TestAlphaBlendOfTwoTranslucentColors;

begin
  AssertColorsEqual('Half blue over half red: one third red, two thirds blue, three quarters coverage',
    FPColor($5555, 0, $AAAA, $C000),
    AlphaBlend(FPColor($FFFF, 0, 0, $8000), FPColor(0, 0, $FFFF, $8000)), 2);
end;


procedure TTestColor.TestCalculateGrayOfBlackAndWhite;

begin
  AssertEquals('Black is gray 0', 0, CalculateGray(colBlack));
  AssertEquals('White is gray $FFFF', $FFFF, CalculateGray(colWhite));
end;


procedure TTestColor.TestCalculateGrayUsesLumaWeights;

begin
  AssertTrue('Red gives 0.299 of full scale', Abs(CalculateGray(colRed) - 19595) <= 2);
  AssertTrue('Green gives 0.587 of full scale', Abs(CalculateGray(colGreen) - 38469) <= 2);
  AssertTrue('Blue gives 0.114 of full scale', Abs(CalculateGray(colBlue) - 7471) <= 2);
end;


procedure TTestColor.TestCalculateGrayOfGrayIsThatGray;

var
  lValue: Integer;

begin
  lValue := 0;
  while lValue <= $FFFF do
    begin
    AssertTrue(Format('Gray %d keeps its level', [lValue]),
      Abs(CalculateGray(FPColor(lValue, lValue, lValue)) - lValue) <= 1);
    Inc(lValue, 4369);
    end;
end;


procedure TTestColor.TestHtmlLongForm;

var
  lColor: TFPColor;

begin
  AssertTrue('#rrggbb is accepted', TryHtmlToFPColor('#FF8001', lColor));
  AssertColorsEqual('#rrggbb gives each byte doubled into a word', FPColor($FFFF, $8080, $0101), lColor);
  AssertTrue('Lower case hex digits are accepted', TryHtmlToFPColor('#ff8001', lColor));
  AssertColorsEqual('Lower case gives the same colour', FPColor($FFFF, $8080, $0101), lColor);
end;


procedure TTestColor.TestHtmlShortForm;

var
  lColor: TFPColor;

begin
  AssertTrue('#rgb is accepted', TryHtmlToFPColor('#F80', lColor));
  AssertColorsEqual('#rgb doubles each digit', FPColor($FFFF, $8888, $0000), lColor);
end;


procedure TTestColor.TestHtmlNamesIgnoreCase;

var
  lColor: TFPColor;

begin
  AssertTrue('A name in mixed case is accepted', TryHtmlToFPColor('NaVy', lColor));
  AssertColorsEqual('navy is half blue', FPColor(0, 0, $8080), lColor);
end;


procedure TTestColor.TestHtmlAllSixteenNames;

const
  cNames: array[0..15] of String = ('white', 'silver', 'gray', 'black', 'red',
    'maroon', 'yellow', 'olive', 'lime', 'green', 'aqua', 'teal', 'blue',
    'navy', 'fuchsia', 'purple');
  cValues: array[0..15] of LongWord = ($FFFFFF, $C0C0C0, $808080, $000000,
    $FF0000, $800000, $FFFF00, $808000, $00FF00, $008000, $00FFFF, $008080,
    $0000FF, $000080, $FF00FF, $800080);

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to High(cNames) do
    begin
    AssertTrue('The name ' + cNames[I] + ' is known', TryHtmlToFPColor(cNames[I], lColor));
    AssertColorsEqual('The value of ' + cNames[I],
      RGB8((cValues[I] shr 16) and $FF, (cValues[I] shr 8) and $FF, cValues[I] and $FF), lColor);
    end;
end;


procedure TTestColor.TestHtmlInvalidStringsAreRejected;

var
  lColor: TFPColor;

begin
  AssertFalse('An empty string is rejected', TryHtmlToFPColor('', lColor));
  AssertFalse('Five hex digits are rejected', TryHtmlToFPColor('#12345', lColor));
  AssertFalse('Non-hex digits are rejected', TryHtmlToFPColor('#12345G', lColor));
  AssertFalse('An unknown name is rejected', TryHtmlToFPColor('orangered', lColor));
  AssertFalse('A bare hex string is rejected', TryHtmlToFPColor('FF0000', lColor));
end;


procedure TTestColor.TestHtmlToFPColorRaisesOnInvalid;

begin
  AssertRaises('An invalid HTML colour raises', EConvertError, @ParseInvalidHtml);
end;


procedure TTestColor.TestHtmlToFPColorDefGivesDefault;

var
  lColor: TFPColor;

begin
  AssertColorsEqual('An invalid string gives the default',
    colYellow, HtmlToFPColorDef('bogus', lColor, colYellow));
  AssertColorsEqual('A valid string gives its colour',
    colRed, HtmlToFPColorDef('#FF0000', lColor, colYellow));
end;


procedure TTestColor.TestColorConstantsMatchTheirHtmlNames;

  procedure Check(const aName: String; const aConstant: TFPColor);

  var
    lHtml: TFPColor;

  begin
    lHtml := HtmlToFPColor(aName);
    AssertEquals('Red byte of col' + aName + ' matches the HTML colour', lHtml.Red shr 8, aConstant.Red shr 8);
    AssertEquals('Green byte of col' + aName + ' matches the HTML colour', lHtml.Green shr 8, aConstant.Green shr 8);
    AssertEquals('Blue byte of col' + aName + ' matches the HTML colour', lHtml.Blue shr 8, aConstant.Blue shr 8);
  end;

begin
  Check('Black', colBlack);
  Check('White', colWhite);
  Check('Red', colRed);
  Check('Blue', colBlue);
  Check('Yellow', colYellow);
  Check('Gray', colGray);
  Check('Silver', colSilver);
  Check('Maroon', colMaroon);
  Check('Olive', colOlive);
  Check('Navy', colNavy);
  Check('Purple', colPurple);
  Check('Teal', colTeal);
  Check('Fuchsia', colFuchsia);
  Check('Aqua', colAqua);
  Check('Lime', colLime);
end;


procedure TTestColor.TestPrimaryColorConstants;

begin
  AssertColorsEqual('colGreen is full green', FPColor(0, $FFFF, 0), colGreen);
  AssertColorsEqual('colCyan is green and blue', FPColor(0, $FFFF, $FFFF), colCyan);
  AssertColorsEqual('colMagenta is red and blue', FPColor($FFFF, 0, $FFFF), colMagenta);
  AssertColorsEqual('colDkGray is a quarter gray', FPColor($4000, $4000, $4000), colDkGray);
end;


procedure TTestColor.TestLightAndDarkGreenDiffer;

begin
  AssertFalse('colLtGreen and colDkGreen are different colours', colLtGreen = colDkGreen);
  AssertTrue('colLtGreen is lighter than colDkGreen', colLtGreen.Green > colDkGreen.Green);
  AssertColorsEqual('colLtGreen is the HTML lightgreen', RGB8(144, 238, 144), colLtGreen);
end;


procedure TTestColor.TestTransparentConstantIsTransparent;

begin
  AssertEquals('colTransparent has alpha 0', alphaTransparent, colTransparent.Alpha);
  AssertEquals('alphaOpaque is the full word', $FFFF, alphaOpaque);
end;


initialization
  RegisterTest('color', TTestColor);
end.
