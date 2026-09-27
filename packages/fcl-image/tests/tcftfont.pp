{
    Tests for text drawn with FreeType fonts (ftfont) on an image canvas,
    using examples/DejaVuLGCSans.ttf; ignored when libfreetype is missing.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcftfont;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests,
     fpcanvas, fpimgcanv, ftfont;

type
  TTestFreeTypeFont = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FCanvas: TFPImageCanvas;
    FFont: TFreeTypeFont;
    // The painted pixels, right and bottom included; Right is -1 when nothing is painted.
    function InkBounds: TRect;
    // Number of distinct colours among the pixels.
    function InkColors: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTextIsDrawnAtItsBaseline;
    procedure TestTextHasTheFontColor;
    procedure TestTextWidthGrowsWithTheText;
    procedure TestTextWidthGrowsWithTheSize;
    procedure TestTextExtentMatchesTheInk;
    procedure TestWithoutAntiAliasingOnlyTwoColors;
    procedure TestUnicodeTextIsDrawn;
    procedure TestRotatedTextIsTallerThanWide;
    procedure TestTheTextConventionsStartAsBefore;
    procedure TestTextMetricsComeFromTheFace;
    procedure TestATopOriginDrawsTheAscenderBelowY;
    procedure TestATopOriginFollowsTheRotation;
    procedure TestFontMeasureUsesTheFontMetrics;
    procedure TestOrientationAndAngleFollowEachOther;
    procedure TestTextRectAlignsTheText;
    procedure TestTextRectPlacesTheLayout;
    procedure TestTextRectClipsToItsRect;
    procedure TestTextRectClipsWhereTheTransformationPutsIt;
    procedure TestTextRectBreaksWords;
    procedure TestTextRectEndsWithAnEllipsis;
    procedure TestTextRectUnderlinesThePrefix;
    procedure TestTextRectFillsAnOpaqueBackground;
    procedure TestTextFitInfoCountsTheCharactersThatFit;
  end;

implementation

procedure TTestFreeTypeFont.SetUp;

var
  lFile: String;

begin
  inherited SetUp;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas := TFPImageCanvas.Create(FImage);
  lFile := ExampleFile('DejaVuLGCSans.ttf');
  if not FileExists(lFile) then
    Fail('Font not found: ' + lFile + ' (run from packages/fcl-image)');
  try
    InitEngine;
    FFont := TFreeTypeFont.Create;
    FCanvas.Font := FFont;
    FFont.Name := lFile;
    FFont.Size := 20;
    FFont.FPColor := colBlack;
    FCanvas.TextWidth('x');
  except
    on E: Exception do
      Ignore('FreeType is not available: ' + E.Message);
  end;
end;


procedure TTestFreeTypeFont.TearDown;

begin
  FreeAndNil(FCanvas);
  FreeAndNil(FFont);
  FreeAndNil(FImage);
  inherited TearDown;
end;


function TTestFreeTypeFont.InkBounds: TRect;

var
  lX, lY: Integer;

begin
  Result := Rect(MaxInt, MaxInt, -1, -1);
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if not (FImage[lX, lY] = colWhite) then
        begin
        if lX < Result.Left then
          Result.Left := lX;
        if lX > Result.Right then
          Result.Right := lX;
        if lY < Result.Top then
          Result.Top := lY;
        if lY > Result.Bottom then
          Result.Bottom := lY;
        end;
end;


function TTestFreeTypeFont.InkColors: Integer;

begin
  Result := CountColors(FImage);
end;


procedure TTestFreeTypeFont.TestTextIsDrawnAtItsBaseline;

var
  lInk: TRect;

begin
  FCanvas.TextOut(10, 50, 'Hxg');
  lInk := InkBounds;
  AssertTrue('The text paints pixels', lInk.Right >= 0);
  AssertTrue('The ink starts near x', (lInk.Left >= 9) and (lInk.Left <= 14));
  AssertTrue('Capitals rise above the baseline', lInk.Top < 50 - 10);
  AssertTrue('Capitals are less than the size above the baseline', lInk.Top >= 50 - 20);
  AssertTrue('A descender goes below the baseline', lInk.Bottom > 50);
  AssertTrue('but not by more than half the size', lInk.Bottom <= 50 + 10);
end;


procedure TTestFreeTypeFont.TestTextHasTheFontColor;

var
  lX, lY: Integer;
  lFound: Boolean;

begin
  FFont.FPColor := colBlue;
  FFont.AntiAliased := False;
  FCanvas.TextOut(10, 50, 'HHH');
  lFound := False;
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if not (FImage[lX, lY] = colWhite) then
        begin
        AssertColorsEqual('Every ink pixel has the font colour', colBlue, FImage[lX, lY]);
        lFound := True;
        end;
  AssertTrue('There is ink', lFound);
end;


procedure TTestFreeTypeFont.TestTextWidthGrowsWithTheText;

var
  lA, lB, lAB: Integer;

begin
  AssertTrue('W is wider than i', FCanvas.TextWidth('W') > FCanvas.TextWidth('i'));
  lA := FCanvas.TextWidth('H');
  lB := FCanvas.TextWidth('o');
  lAB := FCanvas.TextWidth('Ho');
  AssertTrue('The width of two letters is about the sum of their widths', Abs(lAB - (lA + lB)) <= 2);
  AssertTrue('The height of text is positive', FCanvas.TextHeight('Hg') > 0);
end;


procedure TTestFreeTypeFont.TestTextWidthGrowsWithTheSize;

var
  lSmall, lLarge: Integer;

begin
  FFont.Size := 10;
  lSmall := FCanvas.TextWidth('Hello');
  FFont.Size := 20;
  lLarge := FCanvas.TextWidth('Hello');
  AssertTrue('Twice the size is about twice as wide', Abs(lLarge - 2 * lSmall) <= 3);
end;


procedure TTestFreeTypeFont.TestTextExtentMatchesTheInk;

var
  lSize: TSize;
  lInk: TRect;

begin
  FCanvas.TextOut(10, 50, 'Hello');
  lSize := FCanvas.TextExtent('Hello');
  lInk := InkBounds;
  AssertTrue('The extent is at least as wide as the ink', lSize.cx >= lInk.Right - lInk.Left + 1);
  AssertTrue('The extent is not much wider than the ink', lSize.cx <= lInk.Right - lInk.Left + 1 + 6);
  AssertTrue('The extent is at least as high as the ink', lSize.cy >= lInk.Bottom - lInk.Top + 1);
end;


procedure TTestFreeTypeFont.TestWithoutAntiAliasingOnlyTwoColors;

begin
  FFont.AntiAliased := False;
  FCanvas.TextOut(10, 50, 'Hello world');
  AssertEquals('Without anti-aliasing there are only the background and the font colour', 2, InkColors);
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  FFont.AntiAliased := True;
  FCanvas.TextOut(10, 50, 'Hello world');
  AssertTrue('With anti-aliasing there are shades in between', InkColors > 2);
end;


procedure TTestFreeTypeFont.TestUnicodeTextIsDrawn;

var
  lText: UnicodeString;

begin
  lText := UTF8Decode('Ωß€');
  FCanvas.TextOut(10, 50, lText);
  AssertTrue('Non-ASCII text paints pixels', InkBounds.Right >= 0);
  AssertTrue('Non-ASCII text has a width', FCanvas.TextWidth(lText) > 0);
end;


procedure TTestFreeTypeFont.TestRotatedTextIsTallerThanWide;

var
  lInk: TRect;

begin
  FImage.Free;
  FImage := CreateSolidImage(120, 200, colWhite);
  FCanvas.Image := FImage;
  FFont.Angle := Pi / 2;
  FCanvas.TextOut(60, 150, 'Hello');
  lInk := InkBounds;
  AssertTrue('Rotated text paints pixels', lInk.Right >= 0);
  AssertTrue('Text turned a quarter is taller than wide', (lInk.Bottom - lInk.Top) > (lInk.Right - lInk.Left));
end;


procedure TTestFreeTypeFont.TestTheTextConventionsStartAsBefore;

begin
  AssertTrue('An image canvas draws text from the baseline', FCanvas.TextOrigin = toBaseline);
  AssertTrue('and measures its ink', FCanvas.TextMeasure = tmInk);
end;


procedure TTestFreeTypeFont.TestTextMetricsComeFromTheFace;

var
  lMetrics: TFPTextMetric;
  lInk: TRect;

begin
  AssertTrue('The FreeType font has metrics', FCanvas.GetTextMetrics(lMetrics));
  AssertTrue('The ascender is most of the 26.7 pixel size', (lMetrics.Ascender >= 18) and (lMetrics.Ascender <= 27));
  AssertTrue('The descender is a small part of it', (lMetrics.Descender >= 2) and (lMetrics.Descender <= 10));
  AssertEquals('The height is ascender plus descender', lMetrics.Ascender + lMetrics.Descender, lMetrics.Height);
  FCanvas.TextOut(10, 50, 'Hxg');
  lInk := InkBounds;
  AssertTrue('Capitals stay below the ascender', lInk.Top >= 50 - lMetrics.Ascender);
  AssertTrue('Descenders stay above the descender', lInk.Bottom <= 50 + lMetrics.Descender);
end;


procedure TTestFreeTypeFont.TestATopOriginDrawsTheAscenderBelowY;

var
  lMetrics: TFPTextMetric;
  lBaseline, lTop: TRect;

begin
  FCanvas.GetTextMetrics(lMetrics);
  FCanvas.TextOut(10, 10 + lMetrics.Ascender, 'Hxg');
  lBaseline := InkBounds;
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  FCanvas.TextOrigin := toTop;
  FCanvas.TextOut(10, 10, 'Hxg');
  lTop := InkBounds;
  AssertTrue('With toTop the text is drawn as from the baseline one ascender lower',
    (lTop.Left = lBaseline.Left) and (lTop.Top = lBaseline.Top) and
    (lTop.Right = lBaseline.Right) and (lTop.Bottom = lBaseline.Bottom));
  AssertTrue('so it stays below y', lTop.Top >= 10);
end;


procedure TTestFreeTypeFont.TestATopOriginFollowsTheRotation;

var
  lMetrics: TFPTextMetric;
  lBaseline, lTop: TRect;

begin
  FImage.Free;
  FImage := CreateSolidImage(120, 200, colWhite);
  FCanvas.Image := FImage;
  FFont.Orientation := 900;
  FCanvas.GetTextMetrics(lMetrics);
  FCanvas.TextOut(40 + lMetrics.Ascender, 150, 'Hello');
  lBaseline := InkBounds;
  FImage.Free;
  FImage := CreateSolidImage(120, 200, colWhite);
  FCanvas.Image := FImage;
  FCanvas.TextOrigin := toTop;
  FCanvas.TextOut(40, 150, 'Hello');
  lTop := InkBounds;
  AssertTrue('Text turned a quarter moves its baseline one ascender to the right',
    (lTop.Left = lBaseline.Left) and (lTop.Top = lBaseline.Top) and
    (lTop.Right = lBaseline.Right) and (lTop.Bottom = lBaseline.Bottom));
  AssertTrue('so it stays right of x', lTop.Left >= 40);
end;


procedure TTestFreeTypeFont.TestFontMeasureUsesTheFontMetrics;

var
  lMetrics: TFPTextMetric;

begin
  FCanvas.GetTextMetrics(lMetrics);
  AssertTrue('By ink a lowercase x is lower than Hg', FCanvas.TextHeight('x') < FCanvas.TextHeight('Hg'));
  FCanvas.TextMeasure := tmFont;
  AssertEquals('By font an x is the font height', lMetrics.Height, FCanvas.TextHeight('x'));
  AssertEquals('and so is Hg', lMetrics.Height, FCanvas.TextHeight('Hg'));
  AssertEquals('By font the width is the advance, which adds up', 2 * FCanvas.TextWidth('o'), FCanvas.TextWidth('oo'));
  AssertEquals('TextExtent follows TextMeasure', lMetrics.Height, FCanvas.TextExtent('x').cy);
end;


procedure TTestFreeTypeFont.TestOrientationAndAngleFollowEachOther;

begin
  FFont.Orientation := 900;
  AssertEquals('Orientation 900 turns the text a quarter', Pi / 2, FFont.Angle, 1e-9);
  FFont.Angle := Pi / 6;
  AssertEquals('Angle Pi/6 is Orientation 300', 300, FFont.Orientation);
  AssertEquals('and keeps its exact value', Pi / 6, FFont.Angle, 1e-12);
end;


// Returns a text style with the fields that TextRect tests change.
function Style(aAlignment: TAlignment; aLayout: TFPTextLayout): TFPTextStyle;

begin
  Result := Default(TFPTextStyle);
  Result.Alignment := aAlignment;
  Result.Layout := aLayout;
  Result.SingleLine := True;
  Result.Clipping := True;
end;


procedure TTestFreeTypeFont.TestTextRectAlignsTheText;

var
  lInk: TRect;

begin
  FCanvas.RectangleMode := rmExclude;
  FCanvas.TextRect(Rect(10, 10, 190, 60), 0, 0, 'Hi', Style(taRightJustify, ftlTop));
  lInk := InkBounds;
  AssertTrue('Right aligned text ends at the right of the rect', (lInk.Right <= 189) and (lInk.Right >= 184));
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  FCanvas.TextRect(Rect(10, 10, 190, 60), 0, 0, 'Hi', Style(taCenter, ftlTop));
  lInk := InkBounds;
  AssertTrue('Centred text is centred in the rect', Abs((lInk.Left + lInk.Right) div 2 - 100) <= 3);
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  FCanvas.TextRect(Rect(10, 10, 190, 60), 40, 20, 'Hi', Style(taLeftJustify, ftlTop));
  lInk := InkBounds;
  AssertTrue('Left and top aligned text starts at X', (lInk.Left >= 40) and (lInk.Left <= 43));
  AssertTrue('and below Y', lInk.Top >= 20);
end;


procedure TTestFreeTypeFont.TestTextRectPlacesTheLayout;

var
  lInk: TRect;
  lMetrics: TFPTextMetric;

begin
  FCanvas.RectangleMode := rmExclude;
  FCanvas.GetTextMetrics(lMetrics);
  FCanvas.TextRect(Rect(10, 10, 190, 90), 0, 0, 'Hg', Style(taLeftJustify, ftlBottom));
  lInk := InkBounds;
  AssertTrue('Bottom laid out text stays in the rect', lInk.Bottom <= 89);
  AssertTrue('and sits at its bottom', lInk.Bottom >= 89 - lMetrics.Descender);
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  FCanvas.TextRect(Rect(10, 10, 190, 90), 0, 0, 'Hg', Style(taLeftJustify, ftlCenter));
  lInk := InkBounds;
  AssertTrue('Centred text lies around the middle of the rect', (lInk.Top < 50) and (lInk.Bottom > 50));
end;


procedure TTestFreeTypeFont.TestTextRectClipsToItsRect;

var
  lInk: TRect;

begin
  FCanvas.RectangleMode := rmExclude;
  FCanvas.TextRect(Rect(10, 10, 40, 60), 10, 10, 'A wide text', Style(taLeftJustify, ftlTop));
  lInk := InkBounds;
  AssertTrue('The text is drawn', lInk.Right >= 0);
  AssertTrue('but not beyond the rect', lInk.Right <= 39);
  FCanvas.TextOut(100, 80, 'x');
  AssertTrue('Afterwards the canvas no longer clips', InkBounds.Right > 100);
  AssertFalse('and Clipping is off again', FCanvas.Clipping);
end;


procedure TTestFreeTypeFont.TestTextRectClipsWhereTheTransformationPutsIt;

var
  lInk: TRect;

begin
  FCanvas.RectangleMode := rmExclude;
  FCanvas.Translate(50, 0);
  FCanvas.TextRect(Rect(10, 10, 40, 60), 10, 10, 'A wide text', Style(taLeftJustify, ftlTop));
  FCanvas.ResetTransform;
  lInk := InkBounds;
  AssertTrue('The translated text is drawn', lInk.Right >= 0);
  AssertTrue('inside the translated rect', (lInk.Left >= 60) and (lInk.Right <= 89));
end;


procedure TTestFreeTypeFont.TestTextRectBreaksWords;

var
  lStyle: TFPTextStyle;
  lMetrics: TFPTextMetric;
  lInk: TRect;

begin
  FCanvas.RectangleMode := rmExclude;
  FCanvas.GetTextMetrics(lMetrics);
  lStyle := Style(taLeftJustify, ftlTop);
  lStyle.SingleLine := False;
  lStyle.Wordbreak := True;
  FCanvas.TextRect(Rect(10, 0, 110, 100), 10, 0, 'one two three four', lStyle);
  lInk := InkBounds;
  AssertTrue('The words are broken over more lines', lInk.Bottom - lInk.Top > lMetrics.Height);
  AssertTrue('within the width of the rect', lInk.Right <= 109);
end;


procedure TTestFreeTypeFont.TestTextRectEndsWithAnEllipsis;

var
  lStyle: TFPTextStyle;
  lInk: TRect;

begin
  FCanvas.RectangleMode := rmExclude;
  lStyle := Style(taLeftJustify, ftlTop);
  lStyle.Clipping := False;
  lStyle.EndEllipsis := True;
  FCanvas.TextRect(Rect(10, 10, 90, 60), 10, 10, 'A text much wider than its rect', lStyle);
  lInk := InkBounds;
  AssertTrue('The shortened text fits the rect without clipping', lInk.Right <= 89);
  AssertTrue('and fills most of it', lInk.Right >= 60);
end;


procedure TTestFreeTypeFont.TestTextRectUnderlinesThePrefix;

var
  lStyle: TFPTextStyle;
  lPlain, lPrefixed: TRect;

begin
  FCanvas.RectangleMode := rmExclude;
  lStyle := Style(taLeftJustify, ftlTop);
  FCanvas.TextRect(Rect(10, 10, 190, 90), 10, 10, 'File', lStyle);
  lPlain := InkBounds;
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  lStyle.ShowPrefix := True;
  FCanvas.TextRect(Rect(10, 10, 190, 90), 10, 10, '&File', lStyle);
  lPrefixed := InkBounds;
  AssertEquals('The & is not drawn', lPlain.Right, lPrefixed.Right);
  AssertTrue('The prefixed character is underlined below the text', lPrefixed.Bottom > lPlain.Bottom);
end;


procedure TTestFreeTypeFont.TestTextRectFillsAnOpaqueBackground;

var
  lStyle: TFPTextStyle;

begin
  FCanvas.RectangleMode := rmExclude;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.FPColor := colYellow;
  lStyle := Style(taLeftJustify, ftlTop);
  lStyle.Opaque := True;
  FCanvas.TextRect(Rect(10, 10, 190, 90), 20, 20, 'Hi', lStyle);
  AssertTrue('The block of the text is filled with the brush', FImage[21, 21] = colYellow);
  AssertTrue('only the block', FImage[15, 15] = colWhite);
end;


procedure TTestFreeTypeFont.TestTextFitInfoCountsTheCharactersThatFit;

begin
  FCanvas.TextMeasure := tmFont;
  AssertEquals('Three characters fit in the width of Hel', 3, FCanvas.TextFitInfo('Hello', FCanvas.TextWidth('Hel')));
  AssertEquals('Nothing fits in no width', 0, FCanvas.TextFitInfo('Hello', 0));
  AssertEquals('All fit in a wide space', 5, FCanvas.TextFitInfo('Hello', 1000));
end;


initialization
  RegisterTest('ftfont', TTestFreeTypeFont);
end.
