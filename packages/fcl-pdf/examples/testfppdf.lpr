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
  {$ifdef unix}cwstring,{$endif}
  classes, sysutils, custapp, fpimage, fpreadjpeg, fppdf, fpparsettf;

type

  TPDFTestApp = class(TCustomApplication)
  private
    Fpg: integer;
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
  Result.Options := lOpts;

  Result.StartDocument;
  S := Result.Sections.AddSection; // we always need at least one section
  lPageCount := 5;
  if Fpg <> -1 then
    lPageCount := 1;
  for i := 1 to lPageCount do
  begin
    P := Result.Pages.AddPage;
    P.PaperType := ptA4;
    P.UnitOfMeasure := uomMillimeters;
    S.AddPage(P);
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
  lPt1: TPDFCoord;
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
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Sample Text');

  // Write text using PDF standard fonts

  P.SetFont(FtTitle, 12);
  P.SetColor(clBlue, false);
  lPt1 := P.Matrix.Transform(25, 50);
  P.WriteText(lPt1.X, lPt1.Y, '(25mm,50mm) Helvetica: The quick brown fox jumps over the lazy dog.');

  P.SetFont(ftText2,16);
  P.SetColor($c00000, false);
  lPt1 := P.Matrix.Transform(60, 100);
  P.WriteText(lPt1.X, lPt1.Y, '(60mm,100mm) Times-BoldItalic: Big text at absolute position');

  // TrueType testing purposes

  P.SetFont(ftText3, 13);
  P.SetColor(clBlack, false);

  lPt1 := P.Matrix.Transform(15, 120);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Languages: English: Hello, World!');

  lPt1 := P.Matrix.Transform(40, 130);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Greek: Γειά σου κόσμος');

  lPt1 := P.Matrix.Transform(40, 140);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Polish: Witaj świecie');

  lPt1 := P.Matrix.Transform(40, 150);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Portuguese: Olá mundo');

  lPt1 := P.Matrix.Transform(40, 160);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Russian: Здравствулте мир');

  lPt1 := P.Matrix.Transform(40, 170);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Vietnamese: Xin chào thế giới');


  P.SetFont(ftText1, 13);
  lPt1 := P.Matrix.Transform(15, 185);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Box Drawing: ╠ ╣ ╦ ╩ ├ ┤ ┬ ┴');

  lPt1 := P.Matrix.Transform(15, 200);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'Typography: “What’s wrong?”');
  lPt1 := P.Matrix.Transform(40, 210);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, '£17.99 vs £17·99');
  lPt1 := P.Matrix.Transform(40, 220);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, '€17.99 vs €17·99');
  lPt1 := P.Matrix.Transform(40, 230);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'OK then…    êçèûÎÐð£¢ß');

  lPt1 := P.Matrix.Transform(25, 280);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'B субботу двадцать третьего мая приезжает твоя любимая теща.');

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
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Sample Line Drawing (DrawLine)');

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsSolid);
  lPt1 := P.Matrix.Transform(30, 100);
  lPt2 := P.Matrix.Transform(150, 150);
  P.DrawLine(lPt1, lPt2, 0.2);

  P.SetColor(clBlue, True);
  P.SetPenStyle(ppsDash);
  lPt1 := P.Matrix.Transform(50, 70);
  lPt2 := P.Matrix.Transform(180, 100);
  P.DrawLine(lPt1, lPt2, 0.1);

  P.SetColor(clRed, True);
  P.SetPenStyle(ppsDashDot);
  lPt1 := P.Matrix.Transform(40, 140);
  lPt2 := P.Matrix.Transform(160, 80);
  P.DrawLine(lPt1, lPt2, 1);

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsDashDotDot);
  lPt1 := P.Matrix.Transform(60, 50);
  lPt2 := P.Matrix.Transform(60, 120);
  P.DrawLine(lPt1, lPt2, 1.5);

  P.SetColor(clBlack, True);
  P.SetPenStyle(ppsDot);
  lPt1 := P.Matrix.Transform(10, 80);
  lPt2 := P.Matrix.Transform(130, 130);
  P.DrawLine(lPt1, lPt2, 0.5);
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
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Sample Line Drawing (DrawLineStyle)');

  // write the text at position 100 mm from left and 120 mm from top
  TsThinBlack := D.AddLineStyleDef(0.2, clBlack, ppsSolid);
  TsThinBlue := D.AddLineStyleDef(0.1, clBlue, ppsDash);
  TsThinRed := D.AddLineStyleDef(1, clRed, ppsDashDot);
  TsThick := D.AddLineStyleDef(1.5, clBlack, ppsDashDotDot);
  TsThinBlackDot := D.AddLineStyleDef(0.5, clBlack, ppsDot);

  lPt1 := P.Matrix.Transform(30, 100);
  lPt2 := P.Matrix.Transform(150, 150);
  P.DrawLineStyle(lPt1, lPt2, tsThinBlack);

  lPt1 := P.Matrix.Transform(50, 70);
  lPt2 := P.Matrix.Transform(180, 100);
  P.DrawLineStyle(lPt1, lPt2, tsThinBlue);

  lPt1 := P.Matrix.Transform(40, 140);
  lPt2 := P.Matrix.Transform(160, 80);
  P.DrawLineStyle(lPt1, lPt2, tsThinRed);

  lPt1 := P.Matrix.Transform(60, 50);
  lPt2 := P.Matrix.Transform(60, 120);
  P.DrawLineStyle(lPt1, lPt2, tsThick);

  lPt1 := P.Matrix.Transform(10, 80);
  lPt2 := P.Matrix.Transform(130, 130);
  P.DrawLineStyle(lPt1.X, lPt1.Y, lPt2.X, lPt2.Y, tsThinBlackDot);  { just to test the other overloaded version too. }
end;

procedure TPDFTestApp.SimpleImage(D: TPDFDocument; APage: integer);
Var
  P: TPDFPage;
  FtTitle: integer;
  IDX: Integer;
  W, H: Integer;
  lPt1: TPDFCoord;
begin
  P := D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica', clBlack);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Sample Image Support');

  P.SetFont(FtTitle,10);
  P.SetColor(clBlack, false);

  IDX := D.Images.AddFromFile('poppy.jpg',False);
  W := D.Images[IDX].Width;
  H := D.Images[IDX].Height;
  { scalled down image (small) }
  lPt1 := P.Matrix.Transform(25, 100); // left-bottom coordinate of image
  P.DrawImage(lPt1.X, lPt1.Y, W div 2, H div 2, IDX);
  lPt1 := P.Matrix.Transform(90, 75);
  P.WriteText(lPt1.X, lPt1.Y, '[Scaled image]');


  { large image }
  lPt1 := P.Matrix.Transform(35, 190);  // left-bottom coordinate of image
  P.DrawImage(lPt1.X, lPt1.Y, W, H, IDX);
  lPt1 := P.Matrix.Transform(160, 150);
  P.WriteText(lPt1.X, lPt1.Y, '[Default size]');
end;

procedure TPDFTestApp.SimpleShapes(D: TPDFDocument; APage: integer);
Var
  P : TPDFPage;
  FtTitle: integer;
//  FtText: integer;
  lPt1, lPt2, lPt3: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('Helvetica', clBlack);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack);
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Basic Shapes');

  // ========== Rectangles ============

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 30;
  { PDF origin coordinate is Bottom-Left, and we want to use Image coordinate of Top-Left }
  lPt1.Y := 60+20; // origin + height
  lPt2 := P.Matrix.Transform(lPt1);
  P.SetColor(clRed, true);
  P.SetColor($37b344, false); // some green color
  P.DrawRect(lPt2.X, lPt2.Y, 40, 20, 3, true, true);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 20;
  { we need the Top-Left coordinate }
  lPt1.Y := 50+20; // origin + height
  lPt2 := P.Matrix.Transform(lPt1);
  P.SetColor(clBlue, true);
  P.SetColor($b737b3, false); // some purple color
  P.DrawRect(lPt2.X, lPt2.Y, 40, 20, 1, true, true);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 110;
  { PDF origin coordinate is Bottom-Left, and we want to use Image cooridanet of Top-Left }
  lPt1.Y := 70+20; // origin + height
  lPt2 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsDashDot);
  P.SetColor(clBlue, true);
  P.DrawRect(lPt2.X, lPt2.Y, 40, 20, 1, false, true);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 100;
  { PDF origin coordinate is Bottom-Left, and we want to use Image cooridanet of Top-Left }
  lPt1.Y := 60+20; // origin + height
  lPt2 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsDash);
  P.SetColor($37b344, true);  // some green color
  P.DrawRect(lPt2.X, lPt2.Y, 40, 20, 2, false, true);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 90;
  { we need the Top-Left coordinate }
  lPt1.Y := 50+20; // origin + height
  lPt2 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsSolid);
  P.SetColor($b737b3, true);  // some purple color
  P.DrawRect(lPt2.X, lPt2.Y, 40, 20, 4, false, true);


  // ========== Ellipses ============

  { Transform the origin point to the Cartesian coordinate system. }
  lPt2 := P.Matrix.Transform(60, 150);
  P.SetPenStyle(ppsSolid);
  P.SetColor($c00000, True);
  P.DrawEllipse(lPt2.X, lPt2.Y, -40, 20, 3, False, True);

  P.SetColor(clBlue, true);
  P.SetColor($b737b3, false); // some purple color
  P.DrawEllipse(lPt2, 10, 10, 1, True, True);
(*
  P.DrawRect(mmToPDF(lPt2.X), mmToPDF(lPt2.Y), 2, 2, 1, False, True);
  FtText := D.AddFont('helvetica-8', clBlack);
  P.SetFont(ftText,8);
  P.SetColor(clblack);
  P.WriteText(mmtoPDF(100), GetPaperHeight-mmToPDF(105),'^---(origin point)');
*)

  { Transform the origin point to the Cartesian coordinate system. }
  lPt2 := P.Matrix.Transform(140, 150);
  P.SetPenStyle(ppsDashDot);
  P.SetColor($b737b3, True);
  P.DrawEllipse(lPt2, 35, 20, 1, False, True);


  // ========== Lines Pen Styles ============

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 30;
  lPt1.Y := 200;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 70;
  lPt1.Y := 200;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 1);

  lPt1.X := 30;
  lPt1.Y := 210;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 70;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsDash);
  P.SetColor(clBlack, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 1);

  lPt1.X := 30;
  lPt1.Y := 220;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 70;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsDot);
  P.SetColor(clBlack, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 1);

  lPt1.X := 30;
  lPt1.Y := 230;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 70;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsDashDot);
  P.SetColor(clBlack, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 1);

  lPt1.X := 30;
  lPt1.Y := 240;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 70;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsDashDotDot);
  P.SetColor(clBlack, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 1);


  // ========== Line Attribute ============


  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 100;
  lPt1.Y := 170;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetPenStyle(ppsSolid);
  P.SetColor(clBlack, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 0.2);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 100;
  lPt1.Y := 180;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 0.3);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 100;
  lPt1.Y := 190;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 0.5);

  { Transform the origin point to the Cartesian coordinate system. }
  lPt1.X := 100;
  lPt1.Y := 200;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 1);

  lPt1.X := 100;
  lPt1.Y := 210;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetColor(clRed, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 2);

  lPt1.X := 100;
  lPt1.Y := 220;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetColor($37b344, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 3);

  lPt1.X := 100;
  lPt1.Y := 230;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetColor(clBlue, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 4);

  lPt1.X := 100;
  lPt1.Y := 240;
  lPt2 := P.Matrix.Transform(lPt1);
  lPt1.X := 140;
  lPt3 := P.Matrix.Transform(lPt1);
  P.SetColor($b737b3, True);
  P.DrawLine(lPt2.X, lPt2.Y, lPt3.X, lPt3.Y, 5);
end;


{ TPDFTestApp }

procedure TPDFTestApp.DoRun;
var
  ErrorMsg: String;
  v: integer;
begin
  inherited DoRun;
  // quick check parameters
  ErrorMsg := CheckOptions('hp:f:', '');
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

  FFontCompression := True;
  if HasOption('f', '') then
  begin
    v := StrToInt(GetOptionValue('f', ''));
    if (v <> 0) and (v <> 1) then
    begin
      Writeln('Error in -f parameter. Valid range is 0-1.');
      Writeln('');
      Terminate;
      Exit;
    end;
    FFontCompression := (v = 1);
  end;

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
    end
    else
    begin
      case Fpg of
        1:  SimpleText(FDoc, 0);
        2:  SimpleShapes(FDoc, 0);
        3:  SimpleLines(FDoc, 0);
        4:  SimpleLinesRaw(FDoc, 0);
        5:  SimpleImage(FDoc, 0);
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
  writeln('');
end;



begin
  Application := TPDFTestApp.Create(nil);
  Application.Title := 'fpPDF Test Application';
  Application.Run;
  Application.Free;
end.

