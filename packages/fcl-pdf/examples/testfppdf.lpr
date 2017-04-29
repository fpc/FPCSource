{ This program generates a multi-page PDF document and tests various
  functionality on each of the pages.

  You can also specify to generate single pages by using the -p <n>
  command line parameter.
     eg:   testfppdf -p 1
           testfppdf -p 2

  Use -h to see more command line parameter options.
}
program testfppdf;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  {$ifdef unix}cwstring,{$endif}  // required for UnicodeString handling.
  classes,
  sysutils,
  custapp,
  fpimage,
  fpreadjpeg,
  fppdf,
  fpparsettf,
  fpttf,
  typinfo;

type

  TPDFTestApp = class(TCustomApplication)
  private
    FPage: integer;
    FRawJPEG,
    FImageCompression,
    FTextCompression,
    FFontCompression: boolean;
    FNoFontEmbedding: boolean;
    FSubsetFontEmbedding: boolean;
    FDoc: TPDFDocument;
    function    SetUpDocument: TPDFDocument;
    procedure   SaveDocument(D: TPDFDocument);
    procedure   EmptyPage;
    procedure   SimpleText(D: TPDFDocument; APage: integer);
    procedure   SimpleLinesRaw(D: TPDFDocument; APage: integer);
    procedure   SimpleLines(D: TPDFDocument; APage: integer);
    procedure   SimpleImage(D: TPDFDocument; APage: integer);
    procedure   SimpleShapes(D: TPDFDocument; APage: integer);
    procedure   AdvancedShapes(D: TPDFDocument; APage: integer);
    procedure   SampleMatrixTransform(D: TPDFDocument; APage: integer);
    procedure   SampleLandscape(D: TPDFDocument; APage: integer);
    procedure   TextInABox(const APage: TPDFPage; const AX, AY: TPDFFloat; const APointSize: integer; const ABoxColor: TARGBColor; const AFontName: string; const AText: UTF8String);
  protected
    procedure   DoRun; override;
  public
    procedure   WriteHelp;
  end;


var
  Application: TPDFTestApp;

const
  cPageCount: integer = 8;

function TPDFTestApp.SetUpDocument: TPDFDocument;
var
  P: TPDFPage;
  S: TPDFSection;
  i: integer;
  lPageCount: integer;
  lOpts: TPDFOptions;
begin
  Result := TPDFDocument.Create(Nil);
  Result.Infos.Title := Application.Title;
  Result.Infos.Author := 'Graeme Geldenhuys';
  Result.Infos.Producer := 'fpGUI Toolkit 1.4.1';
  Result.Infos.ApplicationName := ApplicationName;
  Result.Infos.CreationDate := Now;

  lOpts := [poPageOriginAtTop];
  if FSubsetFontEmbedding then
    Include(lOpts, poSubsetFont);
  if FNoFontEmbedding then
  begin
    Include(lOpts, poNoEmbeddedFonts);
    Exclude(lOpts, poSubsetFont);
  end;
  if FFontCompression then
    Include(lOpts, poCompressFonts);
  if FTextCompression then
    Include(lOpts,poCompressText);
  if FImageCompression then
    Include(lOpts,poCompressImages);
  if FRawJPEG then
    Include(lOpts,poUseRawJPEG);
  Result.Options := lOpts;

  Result.StartDocument;
  S := Result.Sections.AddSection; // we always need at least one section
  lPageCount := cPageCount;
  if FPage <> -1 then
    lPageCount := 1;
  for i := 1 to lPageCount do
  begin
    P := Result.Pages.AddPage;
    P.PaperType := ptA4;
    P.UnitOfMeasure := uomMillimeters;
    S.AddPage(P); // Add the Page to the Section
  end;
end;

procedure TPDFTestApp.SaveDocument(D : TPDFDocument);
var
  F: TFileStream;
begin
  F := TFileStream.Create('test.pdf',fmCreate);
  try
    D.SaveToStream(F);
    Writeln('Document used ',D.ObjectCount,' PDF objects/commands');
  finally
    F.Free;
  end;
end;

procedure TPDFTestApp.EmptyPage;
var
  D: TPDFDocument;
begin
  D := SetupDocument;
  try
    SaveDocument(D);
  finally
    D.Free;
  end;
end;

{ all units of measure are in millimeters }
procedure TPDFTestApp.SimpleText(D: TPDFDocument; APage: integer);
var
  P : TPDFPage;
  FtTitle, FtText1, FtText2: integer;
  FtWaterMark: integer;
begin
  P := D.Pages[APage];

  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');
  FtText1 := D.AddFont('FreeSans.ttf', 'FreeSans');
  FtText2 := D.AddFont('Times-BoldItalic');
  FtWaterMark := D.AddFont('Helvetica-Bold');

  { Page title }
  P.SetFont(FtTitle, 23);
  P.SetColor(clBlack, false);
  P.WriteText(25, 20, 'Sample Text');

  P.SetFont(FtWaterMark, 120);
  P.SetColor(clWaterMark, false);
  P.WriteText(55, 190, 'Sample', 45);

  // -----------------------------------
  // Write text using PDF standard fonts
  P.SetFont(FtTitle, 12);
  P.SetColor(clBlue, false);
  P.WriteText(25, 50, '(25mm,50mm) Helvetica: The quick brown fox jumps over the lazy dog.');
  P.SetColor(clBlack, false);
  P.WriteText(25, 57, 'Click the URL:  http://www.freepascal.org');
  P.AddExternalLink(54, 58, 49, 5, 'http://www.freepascal.org', false);

  // strike-through text
  P.WriteText(25, 64, 'Strike-Through text', 0, false, true);

  // strike-through text
  P.WriteText(65, 64, 'Underlined text', 0, true);

  // rotated text
  P.SetColor(clBlue, false);
  P.WriteText(25, 100, 'Rotated text at 30 degrees', 30);

  P.SetFont(ftText2,16);
  P.SetColor($C00000, false);
  P.WriteText(50, 100, '(50mm,100mm) Times-BoldItalic: Big text at absolute position');


  // -----------------------------------
  // TrueType testing purposes
  P.SetFont(FtText1, 13);
  P.SetColor(clBlack, false);

  P.WriteText(15, 120, 'Languages: English: Hello, World!');
  P.WriteText(40, 130, 'Greek: Γειά σου κόσμος');
  P.WriteText(40, 140, 'Polish: Witaj świecie');
  P.WriteText(40, 150, 'Portuguese: Olá mundo');
  P.WriteText(40, 160, 'Russian: Здравствуйте мир');
  P.WriteText(40, 170, 'Vietnamese: Xin chào thế giới');

  P.WriteText(15, 185, 'Box Drawing: ╠ ╣ ╦ ╩ ├ ┤ ┬ ┴');

  P.WriteText(15, 200, 'Typography: “What’s wrong?”');
  P.WriteText(40, 210, '£17.99 vs £17·99');
  P.WriteText(40, 220, '€17.99 vs €17·99');
  P.WriteText(40, 230, 'OK then…    (êçèûÎÐð£¢ß)  \\//{}()#<>');

  P.WriteText(25, 280, 'B субботу двадцать третьего мая приезжает твоя любимая теща.');

  { draw a rectangle around the text }
  TextInABox(P, 25, 255, 23, clRed, 'FreeSans', '“Text in a Box gyj?”');

  { lets make a hyperlink more prominent }
  TextInABox(P, 100, 255, 12, clMagenta, 'FreeSans', 'http://www.freepascal.org');
  P.AddExternalLink(99, 255, 49, 5, 'http://www.freepascal.org', false);
end;

procedure TPDFTestApp.SimpleLinesRaw(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  lPt1, lPt2: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, False);
  P.WriteText(25, 20, 'Sample Line Drawing (DrawLine)');

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsSolid, 1);
  lPt1.X := 30;   lPt1.Y := 100;
  lPt2.X := 150;  lPt2.Y := 150;
  P.DrawLine(lPt1, lPt2, 1);

  P.SetColor(clBlue, True);
  P.SetPenStyle(ppsDash, 1);
  lPt1.X := 50;   lPt1.Y := 70;
  lPt2.X := 180;  lPt2.Y := 100;
  P.DrawLine(lPt1, lPt2, 1);

  { we can also use coordinates directly, without TPDFCoord variables }

  P.SetColor(clRed, True);
  P.SetPenStyle(ppsDashDot, 1);
  P.DrawLine(40, 140, 160, 80, 1);

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsDashDotDot, 1);
  P.DrawLine(60, 50, 60, 120, 1);

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsDot, 1);
  P.DrawLine(10, 80, 130, 130, 1);
end;

procedure TPDFTestApp.SimpleLines(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  TsThinBlack, TsThinBlue, TsThick, TsThinRed, TsThinBlackDot: Integer;
  lPt1, lPt2: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  P.WriteText(25, 20, 'Sample Line Drawing (DrawLineStyle)');

  // write the text at position 100 mm from left and 120 mm from top
  TsThinBlack := D.AddLineStyleDef(1, clBlack, ppsSolid);
  TsThinBlue := D.AddLineStyleDef(1, clBlue, ppsDash);
  TsThinRed := D.AddLineStyleDef(1, clRed, ppsDashDot);
  TsThick := D.AddLineStyleDef(1, clBlack, ppsDashDotDot);
  TsThinBlackDot := D.AddLineStyleDef(1, clBlack, ppsDot);

  lPt1.X := 30;   lPt1.Y := 100;
  lPt2.X := 150;  lPt2.Y := 150;
  P.DrawLineStyle(lPt1, lPt2, tsThinBlack);

  lPt1.X := 50;   lPt1.Y := 70;
  lPt2.X := 180;  lPt2.Y := 100;
  P.DrawLineStyle(lPt1, lPt2, tsThinBlue);

  { we can also use coordinates directly, without TPDFCoord variables }

  P.DrawLineStyle(40, 140, 160, 80, tsThinRed);
  P.DrawLineStyle(60, 50, 60, 120, tsThick);
  P.DrawLineStyle(10, 80, 130, 130, tsThinBlackDot);
end;

procedure TPDFTestApp.SimpleImage(D: TPDFDocument; APage: integer);
Var
  P: TPDFPage;
  FtTitle: integer;
  IDX: Integer;
  W, H: Integer;
begin
  P := D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  P.WriteText(25, 20, 'Sample Image Support');

  P.SetFont(FtTitle,10);
  P.SetColor(clBlack, false);

  IDX := D.Images.AddFromFile('poppy.jpg',False);
  W := D.Images[IDX].Width;
  H := D.Images[IDX].Height;
  { full size image }
  P.DrawImageRawSize(25, 130, W, H, IDX);  // left-bottom coordinate of image
  P.WriteText(145, 90, '[Full size (defined in pixels)]');

  { quarter size image }
  P.DrawImageRawSize(25, 190, W shr 1, H shr 1, IDX); // could also have used: Integer(W div 2), Integer(H div 2)
  P.WriteText(85, 180, '[Quarter size (defined in pixels)]');
  { rotated image }
  P.DrawImageRawSize(150, 190, W shr 1, H shr 1, IDX, 30);

  { scalled image to 2x2 centimeters }
  P.DrawImage(25, 230, 20.0, 20.0, IDX); // left-bottom coordinate of image
  P.WriteText(50, 220, '[2x2 cm scaled image]');
  { rotatedd image }
  P.DrawImage(120, 230, 20.0, 20.0, IDX, 30);
end;

procedure TPDFTestApp.SimpleShapes(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  lPt1: TPDFCoord;
  lPoints: array of TPDFCoord;
  i: integer;
  lLineWidth: TPDFFloat;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack);
  P.WriteText(25, 20, 'Basic Shapes');

  // ========== Rectangles ============

  { PDF origin coordinate is Bottom-Left. }
  lPt1.X := 30;
  lPt1.Y := 75;
  P.SetColor($c00000, true);
  P.SetColor(clLtGray, false);
  P.DrawRect(lPt1.X, lPt1.Y, 40, 20, 3, true, true);

  lPt1.X := 20;
  lPt1.Y := 65;
  P.SetColor(clBlue, true);
  P.SetColor($ffff80, false); // pastel yellow
  P.DrawRect(lPt1.X, lPt1.Y, 40, 20, 1, true, true);

  P.SetPenStyle(ppsDashDot);
  P.SetColor(clBlue, true);
  P.DrawRect(110, 75, 40, 20, 1, false, true);

  P.SetPenStyle(ppsDash);
  P.SetColor($37b344, true);  // some green color
  P.DrawRect(100, 70, 40, 20, 2, false, true);

  P.SetPenStyle(ppsSolid);
  P.SetColor($c00000, true);
  P.DrawRect(90, 65, 40, 20, 4, false, true);

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, true);
  P.DrawRect(170, 75, 30, 15, 1, false, true, 30);


  // ========== Rounded Rectangle ===========
  lPt1.X := 30;
  lPt1.Y := 120;
  P.SetColor($c00000, true);
  P.SetColor(clLtGray, false);
  P.DrawRoundedRect(lPt1.X, lPt1.Y, 40, 20, 5, 2, true, true);

  lPt1.X := 20;
  lPt1.Y := 110;
  P.SetColor(clBlue, true);
  P.SetColor($ffff80, false); // pastel yellow
  P.DrawRoundedRect(lPt1.X, lPt1.Y, 40, 20, 2.4, 1, true, true);

  P.SetPenStyle(ppsDashDot);
  P.SetColor(clBlue, true);
  P.DrawRoundedRect(110, 120, 40, 20, 1.5, 1, false, true);

  P.SetPenStyle(ppsDash);
  P.SetColor($37b344, true);  // some green color
  P.DrawRoundedRect(100, 115, 40, 20, 3, 2, false, true);

  P.SetPenStyle(ppsSolid);
  P.SetColor($c00000, true);
  P.DrawRoundedRect(90, 110, 40, 20, 5, 3, false, true);

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, true);
  P.DrawRoundedRect(170, 120, 30, 15, 5, 1, false, true, 30);


  // ========== Ellipses ============

  P.SetPenStyle(ppsSolid);
  P.SetColor($c00000, True);
  P.DrawEllipse(60, 150, -40, 20, 3, False, True);

  lPt1.X := 60;
  lPt1.Y := 150;
  P.SetColor(clBlue, true);
  P.SetColor($ffff80, false); // pastel yellow
  P.DrawEllipse(lPt1, 10, 10, 1, True, True);

  P.SetPenStyle(ppsDashDot);
  P.SetColor($b737b3, True);
  P.DrawEllipse(73, 150, 10, 20, 1, False, True);

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, True);
  P.DrawEllipse(170, 150, 30, 15, 1, False, True, 30);

  // ========== Lines Pen Styles ============

  lLineWidth := 1;

  P.SetPenStyle(ppsSolid, lLineWidth);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 170, 70, 170, lLineWidth);

  P.SetPenStyle(ppsDash, lLineWidth);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 175, 70, 175, lLineWidth);

  P.SetPenStyle(ppsDot, lLineWidth);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 180, 70, 180, lLineWidth);

  P.SetPenStyle(ppsDashDot, lLineWidth);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 185, 70, 185, lLineWidth);

  P.SetPenStyle(ppsDashDotDot, lLineWidth);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 190, 70, 190, lLineWidth);


  // ========== Line Attribute ============

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, True);
  P.DrawLine(100, 170, 140, 170, 0.2);
  P.DrawLine(100, 175, 140, 175, 0.3);
  P.DrawLine(100, 180, 140, 180, 0.5);
  P.DrawLine(100, 185, 140, 185, 1);

  P.SetColor(clRed, True);
  P.DrawLine(100, 190, 140, 190, 2);

  P.SetColor($37b344, True);
  P.DrawLine(100, 195, 140, 195, 3);

  P.SetColor(clBlue, True);
  P.DrawLine(100, 200, 140, 200, 4);

  P.SetColor($b737b3, True);
  P.DrawLine(100, 205, 140, 205, 5);


  // ========== PolyLines and Polygons ============
  P.Matrix.SetYTranslation(70);
  P.Matrix.SetXTranslation(20);

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, true);
  P.DrawRect(0, 10, 50, -50, 1, false, true);

  P.SetColor($c00000, true);
  P.ResetPath;
  SetLength(lPoints, 10);
  for i := 0 to 9 do
  begin
    lPoints[i].X := Random(50);
    lPoints[i].Y := Random(50) + 10.5;
  end;
  P.DrawPolyLine(lPoints, 1);
  P.StrokePath;


  P.Matrix.SetXTranslation(80);
  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, true);
  P.DrawRect(0, 10, 50, -50, 1, false, true);

  P.SetColor($ffff80, false); // pastel yellow
  P.SetColor(clBlue, true);
  P.ResetPath;
  P.DrawPolygon(lPoints, 1);
  P.FillStrokePath;

  p.SetPenStyle(ppsSolid);
  P.SetFont(FtTitle, 8);
  P.SetColor(clBlack, false);
  P.WriteText(0, 8, 'Fill using the nonzero winding number rule');


  P.Matrix.SetXTranslation(140);
  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, true);
  P.DrawRect(0, 10, 50, -50, 1, false, true);

  P.SetColor($ffff80, false); // pastel yellow
  P.SetColor(clBlue, true);
  P.ResetPath;
  P.DrawPolygon(lPoints, 1);
  P.FillEvenOddStrokePath;

  p.SetPenStyle(ppsSolid);
  P.SetFont(FtTitle, 8);
  P.SetColor(clBlack, false);
  P.WriteText(0, 8, 'Fill using the even-odd rule');
end;

{ Each curve uses the exact same four coordinates, just with different CubicCurveToXXX
  method calls. I also use the page Maxtix Y-Translation to adjust the coordinate
  system before I draw each curve. I could also refactor each curves drawing
  code into a single parametised procedure - simply to show that each of the
  curves really do use the same code and coordinates. }
procedure TPDFTestApp.AdvancedShapes(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  lPt1, lPt2, lPt3, lPt4: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack);
  P.WriteText(25, 20, 'Advanced Drawing');

  // ========== Cubic Bezier curve ===========

  // PDF c operator curve ===========
  lPt1 := PDFCoord(75, 70);
  lPt2 := PDFCoord(78, 40);
  lPt3 := PDFCoord(100, 35);
  lPt4 := PDFCoord(140, 60);

  p.SetColor(clBlack, true);
  p.SetPenStyle(ppsSolid);
  p.MoveTo(lPt1);
  p.CubicCurveTo(lPt2, lPt3, lPt4, 1);
  // for fun, lets draw the control points as well
  P.SetColor(clLtGray, True);
  P.SetColor(clLtGray, false);
  P.DrawEllipse(lPt2.X-0.5, lPt2.Y, 1, 1, 1, True, True);
  P.DrawEllipse(lPt3.X-0.8, lPt3.Y, 1, 1, 1, True, True);
  P.SetPenStyle(ppsDot);
  P.DrawLine(lPt1, lPt2, 1);
  P.DrawLine(lPt3, lPt4, 1);

  p.SetPenStyle(ppsSolid);
  P.SetFont(FtTitle, 8);
  P.SetColor(clBlack, false);
  P.WriteText(lPt1.X+1, lPt1.Y, '(current point)');
  p.WriteText(lPt2.X+1, lPt2.Y, '(x1, y1)');
  p.WriteText(lPt3.X+1, lPt3.Y, '(x2, y2)');
  p.WriteText(lPt4.X+1, lPt4.Y, '(xTo, yTo)');

  P.SetFont(FtTitle, 10);
  P.WriteText(20, 50, 'CubicCurveTo(...)');


  // PDF v operator curve ===========
  P.Matrix.SetYTranslation(220);

  p.SetColor(clBlack, true);
  p.SetPenStyle(ppsSolid);
  p.MoveTo(lPt1);
  p.CubicCurveToV(lPt3, lPt4, 1);
  // for fun, lets draw the control points as well
  P.SetColor(clLtGray, True);
  P.SetColor(clLtGray, false);
  P.DrawEllipse(lPt3.X-0.8, lPt3.Y, 1, 1, 1, True, True);
  P.SetPenStyle(ppsDot);
  P.DrawLine(lPt3, lPt4, 1);

  p.SetPenStyle(ppsSolid);
  P.SetFont(FtTitle,8);
  P.SetColor(clBlack, false);
  P.WriteText(lPt1.X+1, lPt1.Y, '(current point)');
  p.WriteText(lPt3.X+1, lPt3.Y, '(x2, y2)');
  p.WriteText(lPt4.X+1, lPt4.Y, '(xTo, yTo)');

  P.SetFont(FtTitle, 10);
  P.WriteText(20, 50, 'CubicCurveToV(...)');


  // PDF y operator curve ===========
  P.Matrix.SetYTranslation(140);

  p.SetColor(clBlack, true);
  p.SetPenStyle(ppsSolid);
  p.MoveTo(lPt1);
  p.CubicCurveToY(lPt2, lPt4, 1);
  // for fun, lets draw the control points as well
  P.SetColor(clLtGray, True);
  P.SetColor(clLtGray, false);
  P.DrawEllipse(lPt2.X-0.5, lPt2.Y, 1, 1, 1, True, True);
  P.SetPenStyle(ppsDot);
  P.DrawLine(lPt1, lPt2, 1);

  p.SetPenStyle(ppsSolid);
  P.SetFont(FtTitle,8);
  P.SetColor(clBlack, false);
  P.WriteText(lPt1.X+1, lPt1.Y, '(current point)');
  p.WriteText(lPt2.X+1, lPt2.Y, '(x1, y1)');
  p.WriteText(lPt4.X+1, lPt4.Y, '(xTo, yTo)');

  P.SetFont(FtTitle, 10);
  P.WriteText(20, 50, 'CubicCurveToY(...)');
end;

procedure TPDFTestApp.SampleMatrixTransform(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;

  procedure OutputSample;
  var
    b: boolean;
  begin
    b := P.Matrix._11 = -1;
    P.SetFont(FtTitle, 10);
    P.WriteText(10, 10, 'Matrix transform: ' + BoolToStr(b, True));
    P.DrawLine(0, 0, 100, 100, 1);
    P.WriteText(100, 100, '(line end point)');
  end;

begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack);
  P.WriteText(75, 20, 'Matrix Transform');

  OutputSample;

  // enables Cartesian coordinate system for the page
  P.Matrix.SetYScalation(1);
  P.Matrix.SetYTranslation(0);

  OutputSample;
end;

procedure TPDFTestApp.SampleLandscape(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;

    function PaperTypeToString(AEnum: TPDFPaperType): string;
    begin
      result := GetEnumName(TypeInfo(TPDFPaperType), Ord(AEnum));
    end;

    function PixelsToMM(AValue: integer): integer;
    begin
      Result := Round((AValue / 72) * 25.4);
    end;

begin
  P:=D.Pages[APage];
  P.Orientation := ppoLandscape;

  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica');

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack);
  P.WriteText(25, 20, 'Landscape Page');

  P.SetFont(FtTitle, 12);
  P.WriteText(100, 80, 'Page PaperType:');
  P.WriteText(145, 80, PaperTypeToString(P.PaperType));

  P.WriteText(100, 90, 'Page Size:');
  P.WriteText(145, 90, Format('%d x %d  (pixels)', [P.Paper.W, P.Paper.H]));
  P.WriteText(145, 95, Format('%d x %d  (mm)', [PixelsToMM(P.Paper.W), PixelsToMM(P.Paper.H)]));
end;

procedure TPDFTestApp.TextInABox(const APage: TPDFPage; const AX, AY: TPDFFloat; const APointSize: integer;
    const ABoxColor: TARGBColor; const AFontName: string; const AText: UTF8String);
var
  lFontIdx: integer;
  lFC: TFPFontCacheItem;
  lHeight: single;
  lDescenderHeight: single;
  lTextHeightInMM: single;
  lWidth: single;
  lTextWidthInMM: single;
  lDescenderHeightInMM: single;
  i: integer;
begin
  for i := 0 to APage.Document.Fonts.Count-1 do
  begin
    if APage.Document.Fonts[i].Name = AFontName then
    begin
      lFontIdx := i;
      break;
    end;
  end;
  APage.SetFont(lFontIdx, APointSize);
  APage.SetColor(clBlack, false);
  APage.WriteText(AX, AY, AText);

  lFC := gTTFontCache.Find(AFontName, False, False);
  if not Assigned(lFC) then
    raise Exception.Create(AFontName + ' font not found');

  lHeight := lFC.TextHeight(AText, APointSize, lDescenderHeight);
  { convert the Font Units to mm as our PDFPage.UnitOfMeasure is set to mm. }
  lTextHeightInMM := (lHeight * 25.4) / gTTFontCache.DPI;
  lDescenderHeightInMM := (lDescenderHeight * 25.4) / gTTFontCache.DPI;

  lWidth := lFC.TextWidth(AText, APointSize);
  { convert the Font Units to mm as our PDFPage.UnitOfMeasure is set to mm. }
  lTextWidthInMM := (lWidth * 25.4) / gTTFontCache.DPI;

  { adjust the Y coordinate for the font Descender, because
    WriteText() draws on the baseline. Also adjust the TextHeight
    because CapHeight doesn't take into account the Descender. }
  APage.SetColor(ABoxColor, true);
  APage.DrawRect(AX, AY+lDescenderHeightInMM, lTextWidthInMM,
      lTextHeightInMM+lDescenderHeightInMM, 1, false, true);
end;

{ TPDFTestApp }

procedure TPDFTestApp.DoRun;

  Function BoolFlag(C : Char;ADefault : Boolean) : Boolean;
  Var
    V : Integer;
  begin
    Result:=ADefault;
    if HasOption(C, '') then
      begin
      v := StrToIntDef(GetOptionValue(C,''),-1);
      if Not (V in [0,1]) then
        Raise Exception.Create('Error in -'+C+' parameter. Valid range is 0-1.');
      Result:=(v=1);
      end
  end;

var
  ErrorMsg: String;
begin
  StopOnException:=True;
  inherited DoRun;
  // quick check parameters
  ErrorMsg := CheckOptions('hp:f:t:i:j:ns', '');
  if ErrorMsg <> '' then
  begin
    WriteLn('ERROR:  ' + ErrorMsg);
    Writeln('');
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', '') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FPage := -1;
  if HasOption('p', '') then
  begin
    FPage := StrToInt(GetOptionValue('p', ''));
    if (FPage < 1) or (FPage > cPageCount) then
    begin
      Writeln(Format('Error in -p parameter. Valid range is 1-%d.', [cPageCount]));
      Writeln('');
      Terminate;
      Exit;
    end;
  end;

  FNoFontEmbedding := HasOption('n', '');
  FSubsetFontEmbedding := HasOption('s', '');
  FFontCompression := BoolFlag('f',true);
  FTextCompression := BoolFlag('t',False);
  FImageCompression := BoolFlag('i',False);
  FRawJPEG:=BoolFlag('j',False);

  gTTFontCache.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  gTTFontCache.BuildFontCache;

  FDoc := SetupDocument;
  try
    FDoc.FontDirectory := 'fonts';

    if FPage = -1 then
    begin
      SimpleText(FDoc, 0);
      SimpleShapes(FDoc, 1);
      AdvancedShapes(FDoc, 2);
      SimpleLines(FDoc, 3);
      SimpleLinesRaw(FDoc, 4);
      SimpleImage(FDoc, 5);
      SampleMatrixTransform(FDoc, 6);
      SampleLandscape(FDoc, 7);
    end
    else
    begin
      case FPage of
        1:  SimpleText(FDoc, 0);
        2:  SimpleShapes(FDoc, 0);
        3:  AdvancedShapes(FDoc, 0);
        4:  SimpleLines(FDoc, 0);
        5:  SimpleLinesRaw(FDoc, 0);
        6:  SimpleImage(FDoc, 0);
        7:  SampleMatrixTransform(FDoc, 0);
        8:  SampleLandscape(FDoc, 0);
      end;
    end;

    SaveDocument(FDoc);
  finally
    FDoc.Free;
  end;

  // stop program loop
  Terminate;
end;

procedure TPDFTestApp.WriteHelp;
begin
  writeln('Usage:');
  writeln('    -h          Show this help.');
  writeln(Format(
          '    -p <n>      Generate only one page. Valid range is 1-%d.' + LineEnding +
          '                If this option is not specified, then all %0:d pages are' + LineEnding +
          '                generated.', [cPageCount]));
  writeln('    -n          If specified, no fonts will be embedded.');
  writeln('    -s          If specified, subset TTF font embedding will occur.');
  writeln('    -f <0|1>    Toggle embedded font compression. A value of 0' + LineEnding +
          '                disables compression. A value of 1 enables compression.' + LineEnding +
          '                If -n is specified, this option is ignored.');
  writeln('    -t <0|1>    Toggle text compression. A value of 0' + LineEnding +
          '                disables compression. A value of 1 enables compression.');
  writeln('    -i <0|1>    Toggle image compression. A value of 0' + LineEnding +
          '                disables compression. A value of 1 enables compression.');
  writeln('    -j <0|1>    Toggle use of JPEG. A value of 0' + LineEnding +
          '                disables use of JPEG images. A value of 1 writes jpeg file as-is');
  writeln('');
end;


begin
  Randomize;
  Application := TPDFTestApp.Create(nil);
  Application.Title := 'fpPDF Test Application';
  Application.Run;
  Application.Free;
end.
