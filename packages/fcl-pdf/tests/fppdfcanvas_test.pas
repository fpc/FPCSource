{
    Tests for TPDFCanvas: the content stream and the resources it writes
    for the drawing methods of TFPCustomCanvas.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit fppdfcanvas_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  {$ifdef fptest}
  TestFramework,
  {$else}
  fpcunit, testregistry,
  {$endif}
  fpimage, fpcanvas, fpimgcanv, fppdf, fppdfcanvas;

type
  TTestPDFCanvas = class(TTestCase)
  private
    FDocument: TPDFDocument;
    FPage: TPDFPage;
    FCanvas: TPDFCanvas;
    FOutput: String;
    // The document as written so far, line ends as LF.
    function Output: String;
    // Fails unless the output holds aText.
    procedure AssertHas(const aMessage, aText: String);
    // Returns the number of times aNeedle occurs in aText.
    function Occurrences(const aNeedle, aText: String): Integer;
    procedure FloodFillCanvas;
    procedure XorLine;
    procedure ReadPixel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheCanvasHasTheSizeOfThePage;
    procedure TestALineFlipsTheYAxis;
    procedure TestThePenSetsColourWidthCapsAndJoins;
    procedure TestPenStylesAreDashes;
    procedure TestAFillCoversThePixels;
    procedure TestAThickRectangleStaysInside;
    procedure TestAnEllipseIsFourCurves;
    procedure TestEllipseModeInsideShrinksTheOutline;
    procedure TestPolygonsFillEvenOddByDefault;
    procedure TestPolygonsCanFillNonZero;
    procedure TestAnArcIsANativeCurve;
    procedure TestAPieStartsAtItsCentre;
    procedure TestAChordClosesItsArc;
    procedure TestARoundRectHasFourCurvedCorners;
    procedure TestAHatchClipsAndDrawsLines;
    procedure TestAPatternBrushTilesAMaskedImage;
    procedure TestAnImageCoversItsPixels;
    procedure TestTheViewerSmoothsAScaledImage;
    procedure TestAnInterpolationResamplesTheImage;
    procedure TestTranslucentColoursUseAGraphicsState;
    procedure TestPmNotInvertsWithTheDifferenceBlend;
    procedure TestPageReadingNeedsTheShadow;
    procedure TestAnXorLineIsAShadowPatch;
    procedure TestFloodFillFillsTheRegion;
    procedure TestFloodFillUpToABorder;
    procedure TestANewPageStartsAWhiteShadow;
    procedure TestTextUsesAStandardFont;
    procedure TestTextMetricsComeFromTheFont;
    procedure TestATopOriginMovesTheBaseline;
    procedure TestRotatedTextUsesATextMatrix;
    procedure TestAnUnderlineIsARectangle;
    procedure TestClippingClipsEveryShape;
    procedure TestAGradientIsAShading;
    procedure TestTextRectClipsItsText;
    procedure TestATrueTypeFontIsEmbedded;
  end;

implementation

procedure TTestPDFCanvas.SetUp;

begin
  inherited SetUp;
  FDocument := TPDFDocument.Create(nil);
  FDocument.Options := [poPageOriginAtTop];
  FDocument.StartDocument;
  FPage := FDocument.Pages.AddPage;
  FPage.PaperType := ptA4;
  FDocument.Sections.AddSection.AddPage(FPage);
  FCanvas := TPDFCanvas.Create(FDocument, FPage);
  FOutput := '';
end;


procedure TTestPDFCanvas.TearDown;

begin
  FreeAndNil(FCanvas);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestPDFCanvas.Output: String;

var
  lStream: TStringStream;

begin
  if FOutput = '' then
    begin
    lStream := TStringStream.Create('');
    try
      FDocument.SaveToStream(lStream);
      FOutput := StringReplace(lStream.DataString, #13#10, #10, [rfReplaceAll]);
    finally
      lStream.Free;
    end;
    end;
  Result := FOutput;
end;


procedure TTestPDFCanvas.AssertHas(const aMessage, aText: String);

begin
  AssertTrue(aMessage + ': the output holds "' + aText + '"', Pos(aText, Output) > 0);
end;


function TTestPDFCanvas.Occurrences(const aNeedle, aText: String): Integer;

var
  lPos: Integer;

begin
  Result := 0;
  lPos := Pos(aNeedle, aText);
  while lPos > 0 do
    begin
    Inc(Result);
    lPos := Pos(aNeedle, aText, lPos + 1);
    end;
end;


procedure TTestPDFCanvas.FloodFillCanvas;

begin
  FCanvas.FloodFill(5, 5);
end;


procedure TTestPDFCanvas.XorLine;

begin
  FCanvas.Pen.Mode := pmXor;
  FCanvas.Line(1, 1, 5, 5);
end;


procedure TTestPDFCanvas.ReadPixel;

begin
  FCanvas.Colors[1, 1];
end;


procedure TTestPDFCanvas.TestTheCanvasHasTheSizeOfThePage;

begin
  AssertEquals('An A4 page is 595 points wide', 595, FCanvas.Width);
  AssertEquals('and 842 points high', 842, FCanvas.Height);
  AssertTrue('The page measures in points', FPage.UnitOfMeasure = uomPixels);
  AssertTrue('The canvas draws text from the baseline', FCanvas.TextOrigin = toBaseline);
  AssertTrue('and measures by the font', FCanvas.TextMeasure = tmFont);
end;


procedure TTestPDFCanvas.TestALineFlipsTheYAxis;

begin
  FCanvas.Line(10, 10, 50, 20);
  AssertHas('The line starts at its first point, counted from the top', '10 832 m');
  AssertHas('and goes to its second point', '50 822 l');
  AssertHas('and is stroked', #10'S'#10);
end;


procedure TTestPDFCanvas.TestThePenSetsColourWidthCapsAndJoins;

begin
  FCanvas.Pen.FPColor := colRed;
  FCanvas.Pen.Width := 3;
  FCanvas.Pen.EndCap := pecFlat;
  FCanvas.Pen.JoinStyle := pjsMiter;
  FCanvas.Line(10, 10, 50, 20);
  AssertHas('The pen colour strokes', '1 0 0 RG');
  AssertHas('with the pen width', '3 w');
  AssertHas('pjsMiter is a miter join', '0 j');
  AssertHas('pecFlat is a butt cap', '0 J');
end;


procedure TTestPDFCanvas.TestPenStylesAreDashes;

begin
  FCanvas.Pen.Style := psDash;
  FCanvas.Line(10, 10, 50, 20);
  FCanvas.Pen.Style := psDot;
  FCanvas.Line(10, 10, 50, 20);
  AssertHas('psDash draws 3 and skips 1', '[3 1] 0 d');
  AssertHas('psDot draws 1 and skips 1', '[1 1] 0 d');
  AssertHas('Dashes have flat ends', '0 J');
end;


procedure TTestPDFCanvas.TestAFillCoversThePixels;

begin
  FCanvas.Brush.FPColor := colBlue;
  FCanvas.FillRect(10, 20, 30, 40);
  AssertHas('The brush colour fills', '0 0 1 rg');
  AssertHas('from the outer edge of the first pixel', '9.500 822.500 m');
  AssertHas('to the outer edge of the last pixel', '30.5 801.5 l');
  AssertHas('with the nonzero rule', 'h'#10'f'#10);
end;


procedure TTestPDFCanvas.TestAThickRectangleStaysInside;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := 5;
  FCanvas.Rectangle(10, 10, 50, 50);
  AssertHas('A width of 5 is centred 2 pixels inside the bounds', '12 830 m');
  AssertHas('on every side', '48 794 l');
end;


procedure TTestPDFCanvas.TestAnEllipseIsFourCurves;


begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Ellipse(100, 100, 140, 120);
  AssertEquals('An ellipse outline is four Bezier curves', 4, Occurrences(' c'#10, Output));
  AssertHas('from 3 o''clock on the pixel centres', '140 732 m');
  AssertHas('It is stroked', 'h'#10'S'#10);
end;


procedure TTestPDFCanvas.TestEllipseModeInsideShrinksTheOutline;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := 11;
  FCanvas.EllipseMode := emInside;
  FCanvas.Ellipse(100, 100, 140, 120);
  AssertHas('emInside moves the outline in by half the width', '135 732 m');
end;


procedure TTestPDFCanvas.TestPolygonsFillEvenOddByDefault;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Polygon([Point(10, 10), Point(50, 10), Point(30, 40)]);
  AssertHas('A polygon fills by the even-odd rule by default', 'h'#10'f*'#10);
end;


procedure TTestPDFCanvas.TestPolygonsCanFillNonZero;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.PolygonNonZeroWindingRule := True;
  FCanvas.Polygon([Point(10, 10), Point(50, 10), Point(30, 40)]);
  AssertEquals('With PolygonNonZeroWindingRule it fills by the nonzero rule', 0, Occurrences('f*', Output));
  AssertHas('and fills', 'h'#10'f'#10);
end;


procedure TTestPDFCanvas.TestAnArcIsANativeCurve;


begin
  FCanvas.Arc(10, 10, 50, 50, 0, 90 * 16);
  AssertEquals('A quarter arc is one Bezier curve', 1, Occurrences(' c'#10, Output));
  AssertEquals('without line segments', 0, Occurrences(' l'#10, Output));
  AssertHas('It starts at 3 o''clock', '50 812 m');
end;


procedure TTestPDFCanvas.TestAPieStartsAtItsCentre;

begin
  FCanvas.RadialPie(10, 10, 50, 50, 0, 90 * 16);
  AssertHas('The pie starts at its centre', '30 812 m');
  AssertHas('and goes to the start of its arc', '50 812 l');
end;


procedure TTestPDFCanvas.TestAChordClosesItsArc;


begin
  FCanvas.Chord(10, 10, 50, 50, 0, 90 * 16);
  AssertEquals('A chord does not go through the centre', 0, Occurrences('30 812 m', Output));
  AssertHas('It starts on the arc', '50 812 m');
end;


procedure TTestPDFCanvas.TestARoundRectHasFourCurvedCorners;


begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.RoundRect(10, 10, 50, 40, 11, 11);
  AssertEquals('Each corner is a curve', 4, Occurrences(' c'#10, Output));
  AssertHas('The top-right corner starts below the top by its radius of 5', '50 827 m');
end;


procedure TTestPDFCanvas.TestAHatchClipsAndDrawsLines;


begin
  FCanvas.Brush.Style := bsHorizontal;
  FCanvas.Brush.FPColor := colBlue;
  FCanvas.HashWidth := 10;
  FCanvas.FillRect(0, 0, 39, 39);
  AssertHas('The hatch clips to the shape', 'W n');
  AssertHas('and strokes in the brush colour', '0 0 1 RG');
  AssertHas('A line lies on each multiple of HashWidth', '-1 842 m'#10'40 842 l');
  AssertEquals('five lines cross the 40 rows from -1 to 40', 5, Occurrences(' m'#10, Output) - 1);
end;


procedure TTestPDFCanvas.TestAPatternBrushTilesAMaskedImage;

begin
  FCanvas.Brush.Style := bsPattern;
  FCanvas.FillRect(0, 0, 10, 10);
  AssertHas('The pattern is drawn as an image', '/I0 Do');
  AssertTrue('Its clear bits are transparent', poUseImageTransparency in FDocument.Options);
  AssertHas('through a soft mask', '/SMask');
end;


procedure TTestPDFCanvas.TestAnImageCoversItsPixels;

var
  lImage: TFPMemoryImage;

begin
  lImage := TFPMemoryImage.Create(2, 1);
  try
    lImage.Colors[0, 0] := colRed;
    lImage.Colors[1, 0] := colLime;
    FCanvas.Draw(10, 20, lImage);
    AssertHas('The image covers its pixels from the bottom-left corner', '2 0 0 1 9.500 821.500 cm');
    AssertHas('as an image of its own size', '/Width 2');
  finally
    lImage.Free;
  end;
end;


procedure TTestPDFCanvas.TestTheViewerSmoothsAScaledImage;

var
  lImage: TFPMemoryImage;

begin
  lImage := TFPMemoryImage.Create(2, 1);
  try
    FCanvas.StretchDraw(10, 20, 20, 10, lImage);
    AssertHas('Without an Interpolation the image is scaled', '20 0 0 10 9.500 812.500 cm');
    AssertHas('by the viewer, smoothed', '/Interpolate true');
  finally
    lImage.Free;
  end;
end;


procedure TTestPDFCanvas.TestAnInterpolationResamplesTheImage;

var
  lImage: TFPMemoryImage;
  lBox: TFPBoxInterpolation;

begin
  lImage := TFPMemoryImage.Create(2, 1);
  lBox := TFPBoxInterpolation.Create;
  try
    FCanvas.Interpolation := lBox;
    FCanvas.StretchDraw(10, 20, 4, 2, lImage);
    FCanvas.Interpolation := nil;
    AssertHas('The image is resampled to the destination size', '/Width 4');
    AssertTrue('and not smoothed again', Pos('/Interpolate', Output) = 0);
  finally
    lBox.Free;
    lImage.Free;
  end;
end;


procedure TTestPDFCanvas.TestTranslucentColoursUseAGraphicsState;

begin
  FCanvas.DrawingMode := dmAlphaBlend;
  FCanvas.Brush.FPColor := FPColor($FFFF, 0, 0, $8000);
  FCanvas.FillRect(10, 20, 30, 40);
  FCanvas.FillRect(40, 20, 60, 40);
  AssertHas('A translucent fill sets a graphics state', '/GS0 gs');
  AssertHas('with the alpha of the brush', '/ca 0.500');
  AssertEquals('The state is shared by fills of the same alpha', 0, Occurrences('/GS1', Output));
end;


procedure TTestPDFCanvas.TestPmNotInvertsWithTheDifferenceBlend;

begin
  FCanvas.Pen.Mode := pmNot;
  FCanvas.Line(10, 10, 50, 20);
  AssertHas('pmNot strokes in white', '1 1 1 RG');
  AssertHas('with the difference blend', '/BM /Difference');
end;


procedure TTestPDFCanvas.TestPageReadingNeedsTheShadow;

begin
  AssertException('FloodFill without Shadow raises', EPDFCanvas, @FloodFillCanvas);
  AssertException('pmXor without Shadow raises', EPDFCanvas, @XorLine);
  FCanvas.Pen.Mode := pmCopy;
  AssertException('Reading a pixel without Shadow raises', EPDFCanvas, @ReadPixel);
end;


procedure TTestPDFCanvas.TestAnXorLineIsAShadowPatch;


begin
  FCanvas.Shadow := True;
  FCanvas.Pen.FPColor := colRed;
  FCanvas.Pen.Mode := pmXor;
  FCanvas.Line(10, 10, 12, 10);
  AssertEquals('An xor line is not stroked', 0, Occurrences(#10'S'#10, Output));
  AssertHas('but drawn as an image of the changed pixels', '3 0 0 1 9.500 831.500 cm');
  AssertTrue('The shadow holds red xor white', FCanvas.Colors[11, 10] = colCyan);
end;


procedure TTestPDFCanvas.TestFloodFillFillsTheRegion;

begin
  FCanvas.Shadow := True;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(10, 10, 20, 20);
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.FPColor := colRed;
  FCanvas.FloodFill(15, 15);
  AssertHas('The region is filled run by run inside the outline', '10.500 831.500 m');
  AssertHas('up to the right edge of its last pixel', '19.5 831.5 l');
  AssertTrue('The shadow is filled', FCanvas.Colors[15, 15] = colRed);
  AssertEquals('up to the outline', 0, FCanvas.Colors[10, 15].Red);
end;


procedure TTestPDFCanvas.TestFloodFillUpToABorder;

begin
  FCanvas.Shadow := True;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.FPColor := colBlue;
  FCanvas.Rectangle(10, 10, 20, 20);
  FCanvas.Pen.FPColor := colBlack;
  FCanvas.Line(12, 15, 18, 15);
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.FPColor := colRed;
  FCanvas.FloodFill(15, 12, colBlue, ffBorder);
  AssertTrue('ffBorder fills over the black line', FCanvas.Colors[15, 15] = colRed);
  AssertTrue('up to the blue border', FCanvas.Colors[10, 15] = colBlue);
end;


procedure TTestPDFCanvas.TestANewPageStartsAWhiteShadow;

var
  lPage: TPDFPage;

begin
  FCanvas.Shadow := True;
  FCanvas.Brush.FPColor := colRed;
  FCanvas.FillRect(0, 0, 10, 10);
  lPage := FDocument.Pages.AddPage;
  lPage.PaperType := ptA5;
  FDocument.Sections[0].AddPage(lPage);
  FCanvas.Page := lPage;
  AssertTrue('A new page starts with a white shadow', FCanvas.Colors[5, 5] = colWhite);
  AssertEquals('and the size of the page', 420, FCanvas.Width);
end;


procedure TTestPDFCanvas.TestTextUsesAStandardFont;

begin
  FCanvas.TextOut(5, 30, 'Hello');
  FCanvas.Font.Name := 'Times New Roman';
  FCanvas.Font.Bold := True;
  FCanvas.TextOut(5, 60, 'Bold');
  AssertHas('Without a name the font is Helvetica', '/BaseFont /Helvetica');
  AssertHas('A bold Times font is Times-Bold', '/BaseFont /Times-Bold');
  AssertHas('The text is written at 10 points', '/F0 10 Tf');
  AssertHas('on its baseline', '5 812 TD');
  AssertHas('as a string', '(Hello) Tj');
end;


procedure TTestPDFCanvas.TestTextMetricsComeFromTheFont;

var
  lMetrics: TFPTextMetric;

begin
  AssertEquals('Hello in Helvetica 10 is 22.79 points wide', 23, FCanvas.TextWidth('Hello'));
  AssertEquals('Helvetica 10 is 9.25 points from descender to ascender', 9, FCanvas.TextHeight('Hello'));
  AssertTrue('The font has metrics', FCanvas.GetTextMetrics(lMetrics));
  AssertEquals('It rises 7.18 points above the baseline', 7, lMetrics.Ascender);
  AssertEquals('and goes 2.07 points below it', 2, lMetrics.Descender);
  AssertEquals('UTF-8 text is measured in WinAnsi', FCanvas.TextWidth('e'), FCanvas.TextWidth('é'));
end;


procedure TTestPDFCanvas.TestATopOriginMovesTheBaseline;

begin
  FCanvas.TextOrigin := toTop;
  FCanvas.TextOut(5, 10, 'a');
  AssertHas('With toTop the baseline is one ascender below y', '5 825 TD');
end;


procedure TTestPDFCanvas.TestRotatedTextUsesATextMatrix;

begin
  FCanvas.Font.Orientation := 900;
  FCanvas.TextOut(5, 30, 'Up');
  AssertHas('Orientation turns the text counter-clockwise', '-0 1 -1 -0 5 812 Tm');
end;


procedure TTestPDFCanvas.TestAnUnderlineIsARectangle;

begin
  FCanvas.Font.Underline := True;
  FCanvas.TextOut(5, 30, 'Hello');
  AssertHas('The underline is placed and sized by the font metrics', '0 -1.25 22.788 0.5 re f');
end;


procedure TTestPDFCanvas.TestClippingClipsEveryShape;


begin
  FCanvas.ClipRect := Rect(20, 30, 60, 70);
  FCanvas.Line(0, 0, 100, 100);
  FCanvas.Clipping := True;
  FCanvas.Line(0, 0, 100, 100);
  FCanvas.Ellipse(0, 0, 100, 100);
  AssertHas('The clip lies on the outer edges of the ClipRect pixels', '19.500 812.500 m');
  AssertEquals('Each part of each clipped shape is clipped, the unclipped line is not', 3, Occurrences('W n', Output));
end;


procedure TTestPDFCanvas.TestAGradientIsAShading;

begin
  FCanvas.GradientFill(Rect(10, 20, 110, 60), colRed, colBlue, gdHorizontal);
  AssertHas('The gradient is an axial shading', '/ShadingType 2');
  AssertHas('from the first to the last column', '/Coords [10 822 110 822]');
  AssertHas('from the start to the end colour', '/C0 [1 0 0]');
  AssertHas('painted inside the rectangle', 'W n'#10'/Sh0 sh');
end;


procedure TTestPDFCanvas.TestTextRectClipsItsText;

begin
  FCanvas.TextRect(Rect(10, 10, 60, 30), 12, 12, 'Hello', FCanvas.TextStyle);
  AssertHas('TextRect clips to its rect', '9.500 832.500 m');
  AssertHas('and writes from the top at Y', '12 823 TD');
  AssertFalse('Afterwards the canvas no longer clips', FCanvas.Clipping);
end;


procedure TTestPDFCanvas.TestATrueTypeFontIsEmbedded;

var
  lFile: String;

begin
  lFile := ExpandFileName('..' + PathDelim + 'examples' + PathDelim + 'fonts' + PathDelim + 'DejaVuSans.ttf');
  if not FileExists(lFile) then
    Ignore('Font not found: ' + lFile);
  FCanvas.Font.Name := lFile;
  AssertTrue('A TrueType font is measured by its advances', FCanvas.TextWidth('Hello') > 20);
  FCanvas.TextOut(5, 30, 'Hello');
  AssertHas('and embedded in the document', '/FontFile2');
end;


initialization
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFCanvas{$ifdef fptest}.Suite{$endif});
end.
