{
    Tests for the PostScript output of pscanvas: the document structure
    written by TPostScript and the operators TPostScriptCanvas writes.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpscanvas;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     types, fpcanvas, fpimgcanv, pscanvas;

type
  TTestPostScript = class(TTestCase)
  private
    FDoc: TPostScript;
    FStream: TStringStream;
    procedure BeginTwice;
    procedure BeginWithoutStream;
    procedure FloodFillCanvas;
    procedure XorLine;
    procedure CustomFill;
    // Returns the number of times aNeedle occurs in aText.
    function Occurrences(const aNeedle, aText: String): Integer;
    // The text written so far.
    function Output: String;
    // Fails unless the output holds aText.
    procedure AssertHas(const aMessage, aText: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheDocumentStartsWithTheHeader;
    procedure TestTheDocumentEndsWithTheLastPage;
    procedure TestTheTitleAndCreatorAreWritten;
    procedure TestTheBoundingBoxFollowsTheSize;
    procedure TestANewPageStartsTheNextPage;
    procedure TestStartingTwiceRaises;
    procedure TestStartingWithoutStreamRaises;
    procedure TestALineFlipsTheYAxis;
    procedure TestARectangleVisitsItsFourCorners;
    procedure TestAFilledRectangleIsFilled;
    procedure TestTextIsWrittenAsAString;
    procedure TestTextEscapesParentheses;
    procedure TestThePenColorIsWrittenAsFractions;
    procedure TestALineUsesThePenColor;
    procedure TestNumbersUseAPointWhateverTheLocale;
    procedure TestAPolygonIsStrokedAndFilled;
    procedure TestARadialPieIsDrawn;
    procedure TestAFillUsesTheBrushColor;
    procedure TestDestroyingAnOpenDocumentEndsIt;
    procedure TestLineToStrokesItsSegment;
    procedure TestATranslatedPolylineIsTranslatedOnce;
    procedure TestTextUsesTheFontColor;
    procedure TestEllipseModeInsideKeepsTheStrokeInside;
    procedure TestAThickRectangleStaysInside;
    procedure TestPolyBezierUsesCurveto;
    procedure TestTheLineCapAndJoinFollowThePen;
    procedure TestPenStylesAreDashed;
    procedure TestAHatchFillsInsideTheShape;
    procedure TestTheHatchOrigin;
    procedure TestAPatternAndAnImageAreTiled;
    procedure TestAGradientIsAShading;
    procedure TestClippingClipsEveryShape;
    procedure TestTextSelectsACoreFont;
    procedure TestTextIsWrittenInLatin1;
    procedure TestTextMetricsComeFromTheFont;
    procedure TestTextOrientationRotates;
    procedure TestTextIsUnderlinedAndStruckThrough;
    procedure TestDrawWritesTheImagePixels;
    procedure TestStretchDrawWithoutInterpolationScalesInPostScript;
    procedure TestStretchDrawResamplesWithTheInterpolation;
    procedure TestCopyRectCopiesAnotherCanvas;
    procedure TestAlphaBlendMasksTransparentPixels;
    procedure TestAnArcIsANativeCurve;
    procedure TestPageReadingNeedsTheShadow;
    procedure TestDestinationFreePenModesAreNative;
    procedure TestTheShadowIsReadBack;
    procedure TestAnXorLineIsAShadowPatch;
    procedure TestAnAlphaBlendFillIsAShadowPatch;
    procedure TestFloodFillFillsTheTracedRegion;
    procedure TestFloodFillKeepsTheHoles;
    procedure TestANewPageClearsTheShadow;
    procedure TestARelativeBrushImageTilesFromTheShape;
    procedure TestPolygonsFollowTheWindingRule;
  end;

implementation

procedure TTestPostScript.SetUp;

begin
  inherited SetUp;
  FStream := TStringStream.Create('');
  FDoc := TPostScript.Create(nil);
  FDoc.Stream := FStream;
end;


procedure TTestPostScript.TearDown;

begin
  FreeAndNil(FDoc);
  FreeAndNil(FStream);
  inherited TearDown;
end;


procedure TTestPostScript.BeginTwice;

begin
  FDoc.BeginDoc;
  FDoc.BeginDoc;
end;


procedure TTestPostScript.BeginWithoutStream;

begin
  FDoc.Stream := nil;
  FDoc.BeginDoc;
end;


function TTestPostScript.Output: String;

begin
  Result := FStream.DataString;
end;


procedure TTestPostScript.AssertHas(const aMessage, aText: String);

begin
  AssertTrue(aMessage + ': the output holds "' + aText + '"', Pos(aText, Output) > 0);
end;


procedure TTestPostScript.TestTheDocumentStartsWithTheHeader;

begin
  FDoc.BeginDoc;
  FDoc.EndDoc;
  AssertEquals('The document starts with the PostScript comment', 1, Pos('%!PS-Adobe-3.0', Output));
  AssertHas('The first page', '%%Page: 1 1');
end;


procedure TTestPostScript.TestTheDocumentEndsWithTheLastPage;

begin
  FDoc.BeginDoc;
  FDoc.EndDoc;
  AssertHas('A page is shown', 'showpage');
  AssertHas('The page count', '%%Pages: 1');
end;


procedure TTestPostScript.TestTheTitleAndCreatorAreWritten;

begin
  FDoc.Title := 'Test title';
  FDoc.Creator := 'The test';
  FDoc.BeginDoc;
  FDoc.EndDoc;
  AssertHas('The title', '%%Title: Test title');
  AssertHas('The creator', '%%Creator: The test');
end;


procedure TTestPostScript.TestTheBoundingBoxFollowsTheSize;

begin
  FDoc.Width := 400;
  FDoc.Height := 300;
  FDoc.BeginDoc;
  FDoc.EndDoc;
  AssertHas('The bounding box has the width and height of the document', '%%BoundingBox: 0 0 400 300');
end;


procedure TTestPostScript.TestANewPageStartsTheNextPage;

begin
  FDoc.BeginDoc;
  FDoc.NewPage;
  AssertEquals('The page number', 2, FDoc.PageNumber);
  FDoc.EndDoc;
  AssertHas('The second page', '%%Page: 2 2');
  AssertHas('The page count', '%%Pages: 2');
end;


procedure TTestPostScript.TestStartingTwiceRaises;

begin
  AssertRaises('Starting a document that is already started raises', Exception, @BeginTwice);
end;


procedure TTestPostScript.TestStartingWithoutStreamRaises;

begin
  AssertRaises('Starting without a stream raises', Exception, @BeginWithoutStream);
end;


procedure TTestPostScript.TestALineFlipsTheYAxis;

begin
  FDoc.Height := 792;
  FDoc.BeginDoc;
  FDoc.Canvas.Line(10, 20, 30, 40);
  AssertHas('A line starts at x and height-y', '10 772 moveto');
  AssertHas('and ends there too', '30 752 lineto');
  AssertHas('and is stroked', 'stroke');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestARectangleVisitsItsFourCorners;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Brush.Style := bsClear;
  FDoc.Canvas.Rectangle(10, 20, 110, 70);
  AssertHas('Top left', '10.000 772.000 moveto');
  AssertHas('Top right', '110.000 772.000 lineto');
  AssertHas('Bottom right', '110.000 722.000 lineto');
  AssertHas('Bottom left', '10.000 722.000 lineto');
  AssertHas('The path is closed', 'closepath');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAFilledRectangleIsFilled;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.FillRect(10, 20, 110, 70);
  AssertHas('A filled rectangle is filled', 'fill');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextIsWrittenAsAString;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.TextOut(5, 10, 'Hello');
  AssertHas('Text is shown', '(Hello) show');
  AssertHas('with its baseline at its place', '5 782 translate');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextEscapesParentheses;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.TextOut(5, 10, 'a(b)c\');
  AssertHas('Parentheses and backslashes are escaped in a PostScript string', '(a\(b\)c\\) show');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestThePenColorIsWrittenAsFractions;

var
  lPen: TPSPen;
  lWords: TStringList;
  lText: String;
  lAt: Integer;
  lSettings: TFormatSettings;

begin
  lSettings := DefaultFormatSettings;
  lSettings.DecimalSeparator := '.';
  lPen := TPSPen.Create;
  lWords := TStringList.Create;
  try
    lPen.FPColor := colRed;
    lPen.Width := 2;
    lText := lPen.AsString;
    lWords.Delimiter := ' ';
    lWords.StrictDelimiter := True;
    lWords.DelimitedText := Trim(lText);
    lAt := lWords.IndexOf('setrgbcolor');
    AssertTrue('The pen sets an RGB colour', lAt >= 3);
    AssertEquals('Red is 1', 1, StrToFloat(lWords[lAt - 3], lSettings), 0.001);
    AssertEquals('Green is 0', 0, StrToFloat(lWords[lAt - 2], lSettings), 0.001);
    AssertEquals('Blue is 0', 0, StrToFloat(lWords[lAt - 1], lSettings), 0.001);
    lAt := lWords.IndexOf('setlinewidth');
    AssertTrue('The pen sets the line width', lAt >= 1);
    AssertEquals('The line width is the pen width', 2, StrToFloat(lWords[lAt - 1], lSettings), 0.001);
  finally
    lWords.Free;
    lPen.Free;
  end;
end;


procedure TTestPostScript.TestALineUsesThePenColor;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Pen.FPColor := colRed;
  FDoc.Canvas.Line(10, 20, 30, 40);
  AssertHas('A red line sets the colour before stroking', 'setrgbcolor');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestNumbersUseAPointWhateverTheLocale;

var
  lSeparator: Char;

begin
  lSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := ',';
  try
    FDoc.BeginDoc;
    FDoc.Canvas.Pen.FPColor := FPColor($8000, 0, 0);
    FDoc.Canvas.Line(1, 1, 5, 5);
    FDoc.EndDoc;
  finally
    DefaultFormatSettings.DecimalSeparator := lSeparator;
  end;
  AssertHas('A colour component is written with a point', '0.5000 0.0000 0.0000 setrgbcolor');
end;


procedure TTestPostScript.TestAPolygonIsStrokedAndFilled;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Polygon([Point(10, 10), Point(50, 10), Point(30, 40)]);
  AssertHas('The polygon starts at its first point', '10 782 moveto');
  AssertHas('and goes to its last point', '30 752 lineto');
  AssertHas('The outline is closed', 'closepath');
  AssertHas('and filled', 'fill');
  AssertHas('and stroked', 'stroke');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestARadialPieIsDrawn;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.RadialPie(10, 10, 50, 50, 0, 90 * 16);
  AssertHas('The pie starts at its centre', 'newpath 30.0 762.0 moveto');
  AssertHas('with a native arc', '20.000 20.000 scale 0 0 1 0.000 90.000 arc setmatrix' + LineEnding + 'closepath');
  AssertHas('and is filled', 'fill');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAFillUsesTheBrushColor;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Brush.FPColor := colBlue;
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.FillRect(Rect(10, 10, 20, 20));
  AssertHas('The fill sets the brush colour', 'gsave 0.0000 0.0000 1.0000 setrgbcolor fill grestore');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestDestroyingAnOpenDocumentEndsIt;

begin
  FDoc.BeginDoc;
  FreeAndNil(FDoc);
  AssertHas('Destroying an open document writes its last page', 'showpage');
  AssertHas('and the page count', '%%Pages: 1');
end;


procedure TTestPostScript.TestLineToStrokesItsSegment;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Pen.FPColor := colRed;
  FDoc.Canvas.MoveTo(10, 10);
  FDoc.Canvas.LineTo(50, 20);
  AssertHas('LineTo sets the pen colour', '1.0000 0.0000 0.0000 setrgbcolor');
  AssertHas('and strokes the segment from the pen position', 'newpath 10 782 moveto 50 772 lineto stroke');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestATranslatedPolylineIsTranslatedOnce;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Translate(100, 0);
  FDoc.Canvas.Polyline([Point(10, 10), Point(20, 30)]);
  AssertHas('The polyline starts at its translated first point', 'newpath 110 782 moveto');
  AssertHas('and goes to its translated second point', '120 762 lineto');
  AssertFalse('The translation is applied once', Pos('210 ', Output) > 0);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextUsesTheFontColor;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Font.FPColor := colBlue;
  FDoc.Canvas.TextOut(5, 10, 'Blue');
  AssertHas('Text is shown in the font colour', '0.0000 0.0000 1.0000 setrgbcolor' + LineEnding + 'gsave 5 782 translate');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestEllipseModeInsideKeepsTheStrokeInside;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Pen.Width := 10;
  FDoc.Canvas.Ellipse(10, 10, 110, 60);
  AssertHas('emCentered strokes on the bounds', '60.000 757.000 translate 50.000 25.000 scale');
  FDoc.Canvas.EllipseMode := emInside;
  FDoc.Canvas.Ellipse(10, 10, 110, 60);
  AssertHas('emInside strokes the pen width less one, halved, inside the bounds', '60.000 757.000 translate 45.500 20.500 scale');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAThickRectangleStaysInside;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Rectangle(10, 10, 110, 60);
  AssertHas('A rectangle of width 1 is stroked on its bounds',
    '10.000 782.000 moveto 110.000 782.000 lineto 110.000 732.000 lineto 10.000 732.000 lineto');
  FDoc.Canvas.Pen.Width := 10;
  FDoc.Canvas.Rectangle(10, 10, 110, 60);
  AssertHas('A thick rectangle is stroked inside its bounds, like on the pixel canvases',
    '14.500 777.500 moveto 105.500 777.500 lineto 105.500 736.500 lineto 14.500 736.500 lineto');
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.FillRect(10, 10, 110, 60);
  AssertHas('A fill covers the whole bounds',
    '10.000 782.000 moveto 110.000 782.000 lineto 110.000 732.000 lineto 10.000 732.000 lineto' + LineEnding +
    'closepath' + LineEnding + 'gsave');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestPolyBezierUsesCurveto;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.PolyBezier([Point(10, 10), Point(20, 30), Point(40, 30), Point(50, 10),
                          Point(60, 0), Point(80, 0), Point(90, 10)], False, True);
  AssertHas('The curve starts at the first point', 'newpath' + LineEnding + '10 782 moveto');
  AssertHas('The first curve is one curveto', '20 762 40 762 50 782 curveto');
  AssertHas('The second curve goes on from the end of the first', '60 792 80 792 90 782 curveto');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTheLineCapAndJoinFollowThePen;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Pen.EndCap := pecSquare;
  FDoc.Canvas.Pen.JoinStyle := pjsBevel;
  FDoc.Canvas.Line(10, 10, 50, 10);
  AssertHas('pecSquare is line cap 2 and pjsBevel line join 2', '2 setlinecap [] 0 setdash 2 setlinejoin');
  AssertFalse('A line is not closed, so its caps show', Pos('lineto closepath stroke', Output) > 0);
  FDoc.Canvas.Pen.EndCap := pecFlat;
  FDoc.Canvas.Pen.JoinStyle := pjsMiter;
  FDoc.Canvas.Line(10, 20, 50, 20);
  AssertHas('pecFlat is line cap 0 and pjsMiter line join 0', '0 setlinecap [] 0 setdash 0 setlinejoin');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestPenStylesAreDashed;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Pen.Style := psDash;
  FDoc.Canvas.Line(10, 10, 50, 10);
  AssertHas('psDash ($EEEEEEEE) is 3 on, 1 off', '[3 1] 0 setdash');
  FDoc.Canvas.Pen.Style := psDashDot;
  FDoc.Canvas.Line(10, 20, 50, 20);
  AssertHas('psDashDot ($E4E4E4E4) is 3 on, 2 off, 1 on, 2 off', '[3 2 1 2] 0 setdash');
  FDoc.Canvas.Pen.Style := psPattern;
  FDoc.Canvas.Pen.Pattern := $0FFFFF00;
  FDoc.Canvas.Line(10, 30, 50, 30);
  AssertHas('A pattern starting with gaps starts the dash at its first drawn bit', '[20 12] 28 setdash');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAHatchFillsInsideTheShape;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Brush.Style := bsCross;
  FDoc.Canvas.Brush.FPColor := colRed;
  FDoc.Canvas.HashWidth := 8;
  FDoc.Canvas.FillRect(10, 10, 110, 60);
  AssertHas('The hatch is clipped to the shape', 'gsave clip pathbbox');
  AssertHas('in the brush colour', '1.0000 0.0000 0.0000 setrgbcolor 1 setlinewidth');
  AssertHas('with lines HashWidth apart', '/HW 8 def');
  AssertHas('horizontal lines', '{ HW mul HOY exch sub dup HLlx exch moveto HUrx exch lineto } for');
  AssertHas('and vertical lines', '{ HW mul HOX add dup HLly moveto HUry lineto } for');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTheHatchOrigin;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Brush.Style := bsHorizontal;
  FDoc.Canvas.FillRect(10, 20, 110, 60);
  AssertHas('hoDefault hatches a rectangle from its corner', '/HOX 10 def /HOY 772 def');
  FDoc.Canvas.Ellipse(10, 20, 110, 60);
  AssertHas('and an ellipse from the canvas origin', '/HOX 0 def /HOY 792 def');
  FDoc.Canvas.HatchOrigin := hoShape;
  FDoc.Canvas.Ellipse(30, 40, 110, 60);
  AssertHas('hoShape hatches an ellipse from its corner', '/HOX 30 def /HOY 752 def');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAPatternAndAnImageAreTiled;

var
  lPattern: TBrushPattern;
  lTile: TFPMemoryImage;
  I: Integer;

begin
  for I := 0 to High(lPattern) do
    lPattern[I] := $F0F0F0F0;
  lTile := TFPMemoryImage.Create(2, 1);
  try
    lTile.Colors[0, 0] := colRed;
    lTile.Colors[1, 0] := colBlue;
    FDoc.BeginDoc;
    FDoc.Canvas.Brush.Style := bsPattern;
    FDoc.Canvas.Brush.Pattern := lPattern;
    FDoc.Canvas.FillRect(10, 10, 110, 60);
    AssertHas('The pattern is a 32x32 mask, most significant bit first', '/HPat <F0F0F0F0');
    AssertHas('tiled with imagemask', '32 32 true [32 0 0 -32 0 32] {HPat} imagemask');
    FDoc.Canvas.Brush.Style := bsImage;
    FDoc.Canvas.Brush.Image := lTile;
    FDoc.Canvas.FillRect(10, 10, 110, 60);
    AssertHas('The image is written as RGB', '/HImg <FF00000000FF> def /HIW 2 def /HIH 1 def');
    AssertHas('and tiled with colorimage', '{HImg} false 3 colorimage');
    FDoc.Canvas.Brush.Image := nil;
    FDoc.EndDoc;
  finally
    lTile.Free;
  end;
end;


procedure TTestPostScript.TestAGradientIsAShading;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.GradientFill(Rect(10, 20, 110, 60), colRed, colBlue, gdHorizontal);
  AssertHas('The gradient covers the pixels of the rectangle',
    'gsave newpath 9.5 772.5 moveto 110.5 772.5 lineto 110.5 731.5 lineto 9.5 731.5 lineto closepath clip');
  AssertHas('gdHorizontal runs from the left to the right column', '/Coords [10.0 772.0 110.0 772.0]');
  AssertHas('from the start to the end colour',
    '/C0 [1.0000 0.0000 0.0000] /C1 [0.0000 0.0000 1.0000] /N 1 >> >> shfill grestore');
  FDoc.Canvas.GradientFill(Rect(10, 20, 110, 60), colRed, colBlue, gdVertical);
  AssertHas('gdVertical runs from the top to the bottom row', '/Coords [10.0 772.0 10.0 732.0]');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestClippingClipsEveryShape;

var
  lBefore: Integer;
  lClipped: string;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.ClipRect := Rect(20, 30, 60, 70);
  FDoc.Canvas.Line(0, 0, 100, 100);
  AssertFalse('Without Clipping nothing is clipped', Pos('closepath clip newpath', Output) > 0);
  FDoc.Canvas.Clipping := True;
  FDoc.Canvas.Brush.Style := bsClear;
  lBefore := Length(Output);
  FDoc.Canvas.Line(0, 0, 100, 100);
  FDoc.Canvas.Ellipse(0, 0, 100, 100);
  FDoc.Canvas.TextOut(5, 5, 'x');
  FDoc.Canvas.Clipping := False;
  AssertHas('With Clipping a shape is clipped to the outer edges of the ClipRect pixels',
    'gsave newpath 19.5 762.5 moveto 60.5 762.5 lineto 60.5 721.5 lineto 19.5 721.5 lineto closepath clip newpath');
  lClipped := Copy(Output, lBefore + 1, MaxInt);
  AssertEquals('Each of the three shapes is clipped', 3, Occurrences('closepath clip newpath', lClipped));
  AssertEquals('Each clip is restored', Occurrences('gsave', lClipped), Occurrences('grestore', lClipped));
  FDoc.EndDoc;
end;


// Returns a Width x 1 image of the colours aColors.
function RowImage(const aColors: array of TFPColor): TFPMemoryImage;

var
  I: Integer;

begin
  Result := TFPMemoryImage.Create(Length(aColors), 1);
  for I := 0 to High(aColors) do
    Result.Colors[I, 0] := aColors[I];
end;


procedure TTestPostScript.TestTextSelectsACoreFont;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.TextOut(5, 10, 'a');
  AssertHas('Without a name or size the font is Helvetica at 10 points', '/FPC-Helvetica findfont 10 scalefont setfont');
  AssertHas('re-encoded for ISO 8859-1', '/FPC-Helvetica /Helvetica findfont');
  FDoc.Canvas.Font.Name := 'Times New Roman';
  FDoc.Canvas.Font.Size := 12;
  FDoc.Canvas.Font.Bold := True;
  FDoc.Canvas.TextOut(5, 10, 'a');
  AssertHas('A bold Times font is Times-Bold', '/FPC-Times-Bold findfont 12 scalefont setfont');
  FDoc.Canvas.Font.Name := 'DejaVu Sans Mono';
  FDoc.Canvas.Font.Italic := True;
  FDoc.Canvas.TextOut(5, 10, 'a');
  AssertHas('A bold italic mono font is Courier-BoldOblique', '/FPC-Courier-BoldOblique findfont');
  FDoc.Canvas.Font.Name := 'Helvetica-Oblique';
  FDoc.Canvas.TextOut(5, 10, 'a');
  AssertHas('A core font name is used as is', '/FPC-Helvetica-Oblique findfont');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextIsWrittenInLatin1;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.TextOut(5, 10, 'caf'#$C3#$A9);
  AssertHas('UTF-8 text is shown in ISO 8859-1, escaped in octal', '(caf\351) show');
  FDoc.Canvas.TextOut(5, 10, 'x'#$E2#$82#$AC'y');
  AssertHas('A character outside ISO 8859-1 becomes a question mark', '(x?y) show');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextMetricsComeFromTheFont;

var
  lSize: TSize;

begin
  FDoc.BeginDoc;
  AssertEquals('Hello in Helvetica 10 is 22.78 points wide', 23, FDoc.Canvas.TextWidth('Hello'));
  AssertEquals('Helvetica 10 is 9.25 points from descender to ascender', 9, FDoc.Canvas.TextHeight('Hello'));
  FDoc.Canvas.Font.Name := 'Courier';
  FDoc.Canvas.Font.Size := 20;
  lSize := FDoc.Canvas.TextExtent('abc');
  AssertEquals('Courier characters are 0.6 em wide', 36, lSize.cx);
  AssertEquals('Courier 20 is 15.72 points from descender to ascender', 16, lSize.cy);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextOrientationRotates;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Font.Orientation := 300;
  FDoc.Canvas.TextOut(5, 10, 'a');
  AssertHas('Orientation in tenths of a degree rotates the text', 'gsave 5 782 translate' + LineEnding + '30.0 rotate');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTextIsUnderlinedAndStruckThrough;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Font.Underline := True;
  FDoc.Canvas.Font.StrikeThrough := True;
  FDoc.Canvas.TextOut(5, 10, 'Hello');
  AssertHas('The underline is placed and sized by the font metrics', '0 -1.25 22.78 0.50 rectfill');
  AssertHas('The strike-through line is at half the x-height', '0 2.37 22.78 0.50 rectfill');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestDrawWritesTheImagePixels;

var
  lImage: TFPMemoryImage;

begin
  lImage := RowImage([colRed, colLime]);
  try
    FDoc.BeginDoc;
    FDoc.Canvas.Draw(10, 20, lImage);
    AssertHas('The image covers its pixels', 'gsave 9.5 771.5 translate 2 1 scale');
    AssertHas('at its own size', '/Width 2 /Height 1 /BitsPerComponent 8 /ImageMatrix [2 0 0 -1 0 1]');
    AssertHas('without smoothing', '/Interpolate false');
    AssertHas('with its pixels in hexadecimal', 'FF000000FF00' + LineEnding + '> grestore');
    FDoc.EndDoc;
  finally
    lImage.Free;
  end;
end;


procedure TTestPostScript.TestStretchDrawWithoutInterpolationScalesInPostScript;

var
  lImage: TFPMemoryImage;

begin
  lImage := RowImage([colRed, colLime]);
  try
    FDoc.BeginDoc;
    FDoc.Canvas.StretchDraw(10, 20, 20, 10, lImage);
    AssertHas('The image is scaled over the destination', 'gsave 9.5 762.5 translate 20 10 scale');
    AssertHas('from its own pixels', '/Width 2 /Height 1');
    AssertHas('smoothed by the PostScript interpreter', '/Interpolate true');
    FDoc.EndDoc;
  finally
    lImage.Free;
  end;
end;


procedure TTestPostScript.TestStretchDrawResamplesWithTheInterpolation;

var
  lImage: TFPMemoryImage;
  lInterpolation: TFPBoxInterpolation;

begin
  lImage := RowImage([colRed, colLime]);
  lInterpolation := TFPBoxInterpolation.Create;
  try
    FDoc.BeginDoc;
    FDoc.Canvas.Interpolation := lInterpolation;
    FDoc.Canvas.StretchDraw(10, 20, 4, 2, lImage);
    FDoc.Canvas.Interpolation := nil;
    AssertHas('The image is resampled to the destination size', '/Width 4 /Height 2');
    AssertHas('by the interpolation', 'FF0000FF000000FF0000FF00' + LineEnding + 'FF0000FF000000FF0000FF00');
    AssertHas('and not smoothed again', '/Interpolate false');
    FDoc.EndDoc;
  finally
    lInterpolation.Free;
    lImage.Free;
  end;
end;


procedure TTestPostScript.TestCopyRectCopiesAnotherCanvas;

var
  lImage: TFPMemoryImage;
  lCanvas: TFPImageCanvas;
  lRaised: Boolean;

begin
  lImage := RowImage([colRed, colLime, colBlue, colWhite]);
  lCanvas := TFPImageCanvas.Create(lImage);
  try
    FDoc.BeginDoc;
    FDoc.Canvas.CopyRect(10, 20, lCanvas, Rect(1, 0, 2, 0));
    AssertHas('The copied rectangle is drawn as an image', 'gsave 9.5 771.5 translate 2 1 scale');
    AssertHas('with the pixels of the source canvas', '00FF000000FF' + LineEnding + '> grestore');
    lRaised := False;
    try
      FDoc.Canvas.CopyRect(10, 20, FDoc.Canvas, Rect(1, 0, 2, 0));
    except
      on EPostScriptCanvas do
        lRaised := True;
    end;
    AssertTrue('Copying from the PostScript canvas raises EPostScriptCanvas', lRaised);
    FDoc.EndDoc;
  finally
    lCanvas.Free;
    lImage.Free;
  end;
end;


procedure TTestPostScript.TestAlphaBlendMasksTransparentPixels;

var
  lImage: TFPMemoryImage;

begin
  lImage := RowImage([colRed, colTransparent]);
  try
    FDoc.BeginDoc;
    FDoc.Canvas.Draw(10, 20, lImage);
    AssertFalse('dmOpaque draws every pixel', Pos('/ImageType 4', Output) > 0);
    FDoc.Canvas.DrawingMode := dmAlphaBlend;
    FDoc.Canvas.Draw(10, 20, lImage);
    AssertHas('dmAlphaBlend masks the image with a key colour', '/Interpolate false /MaskColor [0 0 0]');
    AssertHas('written for the transparent pixels', 'FF0000000000' + LineEnding + '> grestore');
    FDoc.EndDoc;
  finally
    lImage.Free;
  end;
end;


procedure TTestPostScript.TestAnArcIsANativeCurve;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Arc(10, 10, 50, 50, 0, 90 * 16);
  AssertHas('An arc is a scaled unit circle arc on the pixel centres',
    'newpath' + LineEnding + 'matrix currentmatrix 30.000 762.000 translate 20.000 20.000 scale 0 0 1 0.000 90.000 arc setmatrix'
    + LineEnding + 'stroke');
  AssertFalse('without line segments', Pos('lineto', Output) > 0);
  FDoc.Canvas.Arc(10, 10, 50, 50, 90 * 16, -180 * 16);
  AssertHas('A negative length runs clockwise', '0 0 1 90.000 -90.000 arcn');
  FDoc.Canvas.Arc(0, 0, 200, 100, 45 * 16, 45 * 16);
  AssertHas('On an ellipse the rays keep their geometric angles', '0 0 1 63.435 90.000 arc');
  FDoc.Canvas.Arc(10, 10, 50, 50, 30 * 16, 360 * 16);
  AssertHas('A full turn stays a full turn', '0 0 1 30.000 390.000 arc');
  FDoc.EndDoc;
end;


procedure TTestPostScript.FloodFillCanvas;

begin
  FDoc.Canvas.FloodFill(5, 5);
end;


procedure TTestPostScript.XorLine;

begin
  FDoc.Canvas.Pen.Mode := pmXor;
  FDoc.Canvas.Line(1, 1, 5, 5);
end;


procedure TTestPostScript.CustomFill;

begin
  FDoc.Canvas.DrawingMode := dmCustom;
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.FillRect(1, 1, 5, 5);
end;


function TTestPostScript.Occurrences(const aNeedle, aText: String): Integer;

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


procedure TTestPostScript.TestPageReadingNeedsTheShadow;

begin
  FDoc.BeginDoc;
  AssertException('FloodFill without Shadow raises', EPostScriptCanvas, @FloodFillCanvas);
  AssertException('pmXor without Shadow raises', EPostScriptCanvas, @XorLine);
  FDoc.Canvas.Pen.Mode := pmCopy;
  AssertException('dmCustom without Shadow raises', EPostScriptCanvas, @CustomFill);
  FDoc.Canvas.DrawingMode := dmOpaque;
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestDestinationFreePenModesAreNative;

var
  lStrokes: Integer;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Pen.FPColor := colRed;
  FDoc.Canvas.Pen.Mode := pmNotCopy;
  FDoc.Canvas.Line(1, 1, 5, 5);
  AssertHas('pmNotCopy strokes with the inverted pen colour', '0.0000 1.0000 1.0000 setrgbcolor 0 setlinewidth');
  FDoc.Canvas.Pen.Mode := pmWhite;
  FDoc.Canvas.Line(1, 1, 5, 5);
  AssertHas('pmWhite strokes in white', '1.0000 1.0000 1.0000 setrgbcolor 0 setlinewidth');
  lStrokes := Occurrences('stroke', Output);
  FDoc.Canvas.Pen.Mode := pmNop;
  FDoc.Canvas.Line(1, 1, 5, 5);
  FDoc.Canvas.Rectangle(1, 1, 5, 5);
  AssertEquals('pmNop strokes nothing', lStrokes, Occurrences('stroke', Output));
  AssertEquals('and leaves the pen style alone', Ord(psSolid), Ord(FDoc.Canvas.Pen.Style));
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestTheShadowIsReadBack;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Shadow := True;
  AssertTrue('The shadow starts as a white page', FDoc.Canvas.Colors[5, 5] = colWhite);
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.Brush.FPColor := colRed;
  FDoc.Canvas.FillRect(0, 0, 10, 10);
  AssertTrue('A fill reaches the shadow', FDoc.Canvas.Colors[5, 5] = colRed);
  AssertTrue('outside the fill the page stays white', FDoc.Canvas.Colors[12, 5] = colWhite);
  FDoc.Canvas.CopyRect(20, 0, FDoc.Canvas, Rect(4, 4, 5, 4));
  AssertHas('CopyRect from the canvas copies the shadow pixels', 'FF0000FF0000' + LineEnding + '> grestore');
  AssertTrue('into the shadow', FDoc.Canvas.Colors[21, 0] = colRed);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAnXorLineIsAShadowPatch;

var
  lBefore: Integer;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Shadow := True;
  FDoc.Canvas.Pen.FPColor := colRed;
  FDoc.Canvas.Pen.Mode := pmXor;
  lBefore := Length(Output);
  FDoc.Canvas.Line(10, 10, 12, 10);
  AssertFalse('An xor line is not stroked', Pos('stroke', Copy(Output, lBefore + 1, MaxInt)) > 0);
  AssertHas('but written as an image of the changed pixels', 'gsave 9.5 781.5 translate 3 1 scale');
  AssertHas('red xor white is cyan', '00FFFF00FFFF00FFFF' + LineEnding + '> grestore');
  FDoc.Canvas.Line(13, 8, 15, 10);
  AssertHas('A patch with unchanged pixels masks them out', '<< /ImageType 4 ');
  AssertTrue('and the shadow holds the result', FDoc.Canvas.Colors[11, 10] = colCyan);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestAnAlphaBlendFillIsAShadowPatch;

var
  lColor: TFPColor;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Shadow := True;
  FDoc.Canvas.DrawingMode := dmAlphaBlend;
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.Brush.FPColor := FPColor($FFFF, 0, 0, $8000);
  FDoc.Canvas.FillRect(0, 0, 2, 2);
  AssertHas('A translucent fill is written as an image', 'gsave -0.5 789.5 translate 3 3 scale');
  AssertFalse('not as a fill', Pos('fill', Output) > 0);
  lColor := FDoc.Canvas.Colors[1, 1];
  AssertEquals('blended with the white page: red stays full', $FFFF, lColor.Red);
  AssertTrue('and green is about half', Abs(lColor.Green - $7FFF) < $200);
  FDoc.Canvas.Brush.FPColor := colBlue;
  FDoc.Canvas.FillRect(0, 0, 2, 2);
  AssertHas('An opaque colour stays a native fill', '0.0000 0.0000 1.0000 setrgbcolor fill');
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestFloodFillFillsTheTracedRegion;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Shadow := True;
  FDoc.Canvas.Brush.Style := bsClear;
  FDoc.Canvas.Rectangle(10, 10, 20, 20);
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.Brush.FPColor := colRed;
  FDoc.Canvas.FloodFill(15, 15);
  AssertHas('The fill path runs along the pixel edges inside the outline',
    '10.5 781.5 moveto 19.5 781.5 lineto 19.5 772.5 lineto 10.5 772.5 lineto closepath');
  AssertHas('and is filled with the brush', '1.0000 0.0000 0.0000 setrgbcolor fill');
  AssertTrue('The shadow is filled', FDoc.Canvas.Colors[15, 15] = colRed);
  AssertEquals('up to the outline', 0, FDoc.Canvas.Colors[10, 15].Red);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestFloodFillKeepsTheHoles;

var
  lBefore: Integer;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Shadow := True;
  FDoc.Canvas.Brush.Style := bsClear;
  FDoc.Canvas.Rectangle(10, 10, 30, 30);
  FDoc.Canvas.Rectangle(15, 15, 25, 25);
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.Brush.FPColor := colRed;
  lBefore := Length(Output);
  FDoc.Canvas.FloodFill(12, 12);
  AssertEquals('A ring is traced as its outer outline and its hole', 2,
    Occurrences('moveto', Copy(Output, lBefore + 1, MaxInt)) - 1);
  AssertTrue('The hole stays unfilled', FDoc.Canvas.Colors[20, 20] = colWhite);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestANewPageClearsTheShadow;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Shadow := True;
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.Brush.FPColor := colRed;
  FDoc.Canvas.FillRect(0, 0, 10, 10);
  FDoc.NewPage;
  AssertTrue('A new page starts with a white shadow', FDoc.Canvas.Colors[5, 5] = colWhite);
  FDoc.EndDoc;
end;


procedure TTestPostScript.TestARelativeBrushImageTilesFromTheShape;

var
  lImage: TFPMemoryImage;

begin
  lImage := RowImage([colRed, colBlue]);
  try
    FDoc.BeginDoc;
    FDoc.Canvas.Brush.Style := bsImage;
    FDoc.Canvas.Brush.Image := lImage;
    FDoc.Canvas.FillRect(10, 20, 30, 40);
    AssertHas('The brush image tiles from the canvas origin', 'HLlx 0 sub HIW div floor cvi');
    FDoc.Canvas.RelativeBrushImage := True;
    FDoc.Canvas.FillRect(10, 20, 30, 40);
    AssertHas('or from the shape with RelativeBrushImage', 'HLlx 10 sub HIW div floor cvi');
    FDoc.Canvas.Brush.Image := nil;
    FDoc.EndDoc;
  finally
    lImage.Free;
  end;
end;


procedure TTestPostScript.TestPolygonsFollowTheWindingRule;

var
  lBefore: Integer;

begin
  FDoc.BeginDoc;
  FDoc.Canvas.Brush.Style := bsSolid;
  FDoc.Canvas.Polygon([Point(10, 10), Point(50, 10), Point(30, 40)]);
  AssertHas('A polygon fills by the even-odd rule by default', 'setrgbcolor eofill grestore');
  FDoc.Canvas.Brush.Style := bsCross;
  FDoc.Canvas.Polygon([Point(10, 10), Point(50, 10), Point(30, 40)]);
  AssertHas('and clips a hatch by it', 'gsave eoclip pathbbox');
  FDoc.Canvas.PolygonNonZeroWindingRule := True;
  FDoc.Canvas.Brush.Style := bsSolid;
  lBefore := Length(Output);
  FDoc.Canvas.Polygon([Point(10, 10), Point(50, 10), Point(30, 40)]);
  AssertTrue('PolygonNonZeroWindingRule fills by the non-zero rule',
    Pos('setrgbcolor fill grestore', Copy(Output, lBefore + 1, MaxInt)) > 0);
  FDoc.Canvas.PolygonNonZeroWindingRule := False;
  lBefore := Length(Output);
  FDoc.Canvas.FillRect(10, 10, 20, 20);
  AssertTrue('Other shapes keep the plain fill', Pos('setrgbcolor fill grestore', Copy(Output, lBefore + 1, MaxInt)) > 0);
  FDoc.EndDoc;
end;


initialization
  RegisterTest('pscanvas', TTestPostScript);
end.
