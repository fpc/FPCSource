{ This program generates a multi-page PDF document and tests various
  functionality on each of the 5 pages.

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
  fpparsettf;

type

  TPDFTestApp = class(TCustomApplication)
  private
    Fpg: integer;
    FRawJPEG,
    FImageCompression,
    FTextCompression,
    FFontCompression: boolean;
    FDoc: TPDFDocument;
    function    SetUpDocument: TPDFDocument;
    procedure   SaveDocument(D: TPDFDocument);
    procedure   EmptyPage;
    procedure   SimpleText(D: TPDFDocument; APage: integer);
    procedure   SimpleLinesRaw(D: TPDFDocument; APage: integer);
    procedure   SimpleLines(D: TPDFDocument; APage: integer);
    procedure   SimpleImage(D: TPDFDocument; APage: integer);
    procedure   SimpleShapes(D: TPDFDocument; APage: integer);
    procedure   SampleMatrixTransform(D: TPDFDocument; APage: integer);
  protected
    procedure   DoRun; override;
  public
    procedure   WriteHelp;
  end;


var
  Application: TPDFTestApp;


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
  Result.Infos.Producer := 'fpGUI Toolkit 0.8';
  Result.Infos.ApplicationName := ApplicationName;
  Result.Infos.CreationDate := Now;

  lOpts := [];
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
  lPageCount := 6;
  if Fpg <> -1 then
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
  FtTitle, FtText1, FtText2, FtText3: integer;
begin
  P := D.Pages[APage];

  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica', clRed);
  FtText1 := D.AddFont('FreeSans.ttf', 'FreeSans', clGreen); // TODO: this color value means nothing - not used at all
  FtText2 := D.AddFont('Times-BoldItalic', clBlack);
  // FtText3 := D.AddFont('arial.ttf', 'Arial', clBlack);
  FtText3 := FtText1; // to reduce font dependecies, but above works too if you have arial.ttf available

  { Page title }
  P.SetFont(FtTitle, 23);
  P.SetColor(clBlack, false);
  P.WriteText(25, 20, 'Sample Text');

  // -----------------------------------
  // Write text using PDF standard fonts
  P.SetFont(FtTitle, 12);
  P.SetColor(clBlue, false);
  P.WriteText(25, 50, '(25mm,50mm) Helvetica: The quick brown fox jumps over the lazy dog.');

  P.SetFont(ftText2,16);
  P.SetColor($c00000, false);
  P.WriteText(60, 100, '(60mm,100mm) Times-BoldItalic: Big text at absolute position');

  // -----------------------------------
  // TrueType testing purposes
  P.SetFont(ftText3, 13);
  P.SetColor(clBlack, false);

  P.WriteText(15, 120, 'Languages: English: Hello, World!');
  P.WriteText(40, 130, 'Greek: Γειά σου κόσμος');
  P.WriteText(40, 140, 'Polish: Witaj świecie');
  P.WriteText(40, 150, 'Portuguese: Olá mundo');
  P.WriteText(40, 160, 'Russian: Здравствуйте мир');
  P.WriteText(40, 170, 'Vietnamese: Xin chào thế giới');

  P.SetFont(ftText1, 13);
  P.WriteText(15, 185, 'Box Drawing: ╠ ╣ ╦ ╩ ├ ┤ ┬ ┴');

  P.WriteText(15, 200, 'Typography: “What’s wrong?”');
  P.WriteText(40, 210, '£17.99 vs £17·99');
  P.WriteText(40, 220, '€17.99 vs €17·99');
  P.WriteText(40, 230, 'OK then…    êçèûÎÐð£¢ß');

  P.WriteText(25, 280, 'B субботу двадцать третьего мая приезжает твоя любимая теща.');
end;

procedure TPDFTestApp.SimpleLinesRaw(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  lPt1, lPt2: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica', clBlack);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, False);
  P.WriteText(25, 20, 'Sample Line Drawing (DrawLine)');

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsSolid);
  lPt1.X := 30;   lPt1.Y := 100;
  lPt2.X := 150;  lPt2.Y := 150;
  P.DrawLine(lPt1, lPt2, 0.2);

  P.SetColor(clBlue, True);
  P.SetPenStyle(ppsDash);
  lPt1.X := 50;   lPt1.Y := 70;
  lPt2.X := 180;  lPt2.Y := 100;
  P.DrawLine(lPt1, lPt2, 0.1);

  { we can also use coordinates directly, without TPDFCoord variables }

  P.SetColor(clRed, True);
  P.SetPenStyle(ppsDashDot);
  P.DrawLine(40, 140, 160, 80, 1);

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsDashDotDot);
  P.DrawLine(60, 50, 60, 120, 1.5);

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsDot);
  P.DrawLine(10, 80, 130, 130, 0.5);
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
  FtTitle := D.AddFont('Helvetica', clRed);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  P.WriteText(25, 20, 'Sample Line Drawing (DrawLineStyle)');

  // write the text at position 100 mm from left and 120 mm from top
  TsThinBlack := D.AddLineStyleDef(0.2, clBlack, ppsSolid);
  TsThinBlue := D.AddLineStyleDef(0.1, clBlue, ppsDash);
  TsThinRed := D.AddLineStyleDef(1, clRed, ppsDashDot);
  TsThick := D.AddLineStyleDef(1.5, clBlack, ppsDashDotDot);
  TsThinBlackDot := D.AddLineStyleDef(0.5, clBlack, ppsDot);

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
  FtTitle := D.AddFont('Helvetica', clBlack);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  P.WriteText(25, 20, 'Sample Image Support');

  P.SetFont(FtTitle,10);
  P.SetColor(clBlack, false);

  IDX := D.Images.AddFromFile('poppy.jpg',False);
  W := D.Images[IDX].Width;
  H := D.Images[IDX].Height;
  { scalled down image (small) }
  P.DrawImage(25, 100, W div 2, H div 2, IDX); // left-bottom coordinate of image
  P.WriteText(90, 75, '[Scaled image]');

  { large image }
  P.DrawImage(35, 190, W, H, IDX);  // left-bottom coordinate of image
  P.WriteText(160, 150, '[Default size]');
end;

procedure TPDFTestApp.SimpleShapes(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  lPt1: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica', clBlack);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack);
  P.WriteText(25, 20, 'Basic Shapes');

  // ========== Rectangles ============

  { PDF origin coordinate is Bottom-Left, and we want to use Image Coordinate of Top-Left }
  lPt1.X := 30;
  lPt1.Y := 60+20; // origin + height
  P.SetColor(clRed, true);
  P.SetColor($37b344, false); // some green color
  P.DrawRect(lPt1.X, lPt1.Y, 40, 20, 3, true, true);

  lPt1.X := 20;
  lPt1.Y := 50+20; // origin + height
  P.SetColor(clBlue, true);
  P.SetColor($b737b3, false); // some purple color
  P.DrawRect(lPt1.X, lPt1.Y, 40, 20, 1, true, true);

  P.SetPenStyle(ppsDashDot);
  P.SetColor(clBlue, true);
  P.DrawRect(110, 70+20 {origin+height}, 40, 20, 1, false, true);

  P.SetPenStyle(ppsDash);
  P.SetColor($37b344, true);  // some green color
  P.DrawRect(100, 60+20 {origin+height}, 40, 20, 2, false, true);

  P.SetPenStyle(ppsSolid);
  P.SetColor($b737b3, true);  // some purple color
  P.DrawRect(90, 50+20 {origin+height}, 40, 20, 4, false, true);


  // ========== Ellipses ============

  P.SetPenStyle(ppsSolid);
  P.SetColor($c00000, True);
  P.DrawEllipse(60, 150, -40, 20, 3, False, True);

  lPt1.X := 60;
  lPt1.Y := 150;
  P.SetColor(clBlue, true);
  P.SetColor($b737b3, false); // some purple color
  P.DrawEllipse(lPt1, 10, 10, 1, True, True);

  P.SetPenStyle(ppsDashDot);
  P.SetColor($b737b3, True);
  P.DrawEllipse(140, 150, 35, 20, 1, False, True);


  // ========== Lines Pen Styles ============

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 200, 70, 200, 1);

  P.SetPenStyle(ppsDash);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 210, 70, 210, 1);

  P.SetPenStyle(ppsDot);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 220, 70, 220, 1);

  P.SetPenStyle(ppsDashDot);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 230, 70, 230, 1);

  P.SetPenStyle(ppsDashDotDot);
  P.SetColor(clBlack, True);
  P.DrawLine(30, 240, 70, 240, 1);


  // ========== Line Attribute ============

  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, True);
  P.DrawLine(100, 170, 140, 170, 0.2);
  P.DrawLine(100, 180, 140, 180, 0.3);
  P.DrawLine(100, 190, 140, 190, 0.5);
  P.DrawLine(100, 200, 140, 200, 1);

  P.SetColor(clRed, True);
  P.DrawLine(100, 210, 140, 210, 2);

  P.SetColor($37b344, True);
  P.DrawLine(100, 220, 140, 220, 3);

  P.SetColor(clBlue, True);
  P.DrawLine(100, 230, 140, 230, 4);

  P.SetColor($b737b3, True);
  P.DrawLine(100, 240, 140, 240, 5);
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
  FtTitle := D.AddFont('Helvetica', clBlack);

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
  v: integer;

begin
  StopOnException:=True;
  inherited DoRun;
  // quick check parameters
  ErrorMsg := CheckOptions('hp:f:t:i:j:', '');
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

  Fpg := -1;
  if HasOption('p', '') then
  begin
    Fpg := StrToInt(GetOptionValue('p', ''));
    if (Fpg < 1) or (Fpg > 5) then
    begin
      Writeln('Error in -p parameter. Valid range is 1-5.');
      Writeln('');
      Terminate;
      Exit;
    end;
  end;

  FFontCompression := BoolFlag('f',true);
  FTextCompression := BoolFlag('t',False);
  FImageCompression := BoolFlag('i',False);
  FRawJPEG:=BoolFlag('j',False);

  FDoc := SetupDocument;
  try
    FDoc.FontDirectory := 'fonts';

    if Fpg = -1 then
    begin
      SimpleText(FDoc, 0);
      SimpleShapes(FDoc, 1);
      SimpleLines(FDoc, 2);
      SimpleLinesRaw(FDoc, 3);
      SimpleImage(FDoc, 4);
      SampleMatrixTransform(FDoc, 5);
    end
    else
    begin
      case Fpg of
        1:  SimpleText(FDoc, 0);
        2:  SimpleShapes(FDoc, 0);
        3:  SimpleLines(FDoc, 0);
        4:  SimpleLinesRaw(FDoc, 0);
        5:  SimpleImage(FDoc, 0);
        6:  SampleMatrixTransform(FDoc, 0);
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
  writeln('    -p <n>      Generate only one page. Valid range is 1-5.' + LineEnding +
          '                If this option is not specified, then all 5 pages are' + LineEnding +
          '                generated.');
  writeln('    -f <0|1>    Toggle embedded font compression. A value of 0' + LineEnding +
          '                disables compression. A value of 1 enables compression.');
  writeln('    -t <0|1>    Toggle text compression. A value of 0' + LineEnding +
          '                disables compression. A value of 1 enables compression.');
  writeln('    -i <0|1>    Toggle image compression. A value of 0' + LineEnding +
          '                disables compression. A value of 1 enables compression.');
  writeln('    -j <0|1>    Toggle use of JPEG. A value of 0' + LineEnding +
          '                disables use of JPEG images. A value of 1 writes jpeg file as-is');
  writeln('');
end;



begin
  Application := TPDFTestApp.Create(nil);
  Application.Title := 'fpPDF Test Application';
  Application.Run;
  Application.Free;
end.
