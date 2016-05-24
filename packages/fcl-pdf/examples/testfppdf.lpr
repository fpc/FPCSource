{ This program generatesa multi-page PDF document and tests various
  functionality on each of the 5 pages. }
{$mode objfpc}
{$H+}
program testfppdf;

uses
  classes, sysutils, fpimage, fpreadjpeg, freetype, fppdf;

Function SetUpDocument : TPDFDocument;
Var
  P : TPDFPage;
  S : TPDFSection;
  i: integer;
begin
  Result:=TPDFDocument.Create(Nil);
  Result.Infos.Title := 'Test Document';
  Result.Infos.Author := ApplicationName;
  Result.Infos.Producer:='fpGUI Toolkit 0.8';
  Result.Infos.ApplicationName:='pdf_demo';
  Result.Infos.CreationDate:=Now;
  Result.StartDocument;
  S:=Result.Sections.AddSection; // we always need at least one section
  for i := 1 to 5 do
  begin
    P:=Result.Pages.AddPage;
    P.PaperType := ptA4;
    P.UnitOfMeasure := uomMillimeters;
    S.AddPage(P);
  end;
end;

Procedure SaveDocument(D : TPDFDocument);
Var
  F : TFileStream;
begin
  F:=TFileStream.Create('test.pdf',fmCreate);
  try
    D.SaveToStream(F);
    Writeln('Document used ',D.ObjectCount,' PDF objects/commands');
  finally
    F.Free;
  end;
end;

Procedure EmptyPage;
Var
  D : TPDFDocument;
begin
  D:=SetupDocument;
  try
    SaveDocument(D);
  finally
    D.Free;
  end;
end;


{ all units of measure are in millimeters }
Procedure SimpleText(D: TPDFDocument; APage: integer);
Var
  P : TPDFPage;
  FtTitle, FtText1, FtText2: integer;
  lPt1: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('helvetica-12', clRed);
  FtText1 := D.AddFont('FreeSans.ttf', 'FreeSans-12', clGreen); // TODO: this color value means nothing - not used at all
  FtText2 := D.AddFont('times-8', clGreen);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Sample Text');

  // Write text using FreeSans font
  P.SetFont(ftText1,12);
  P.SetColor(clBlack, false);
  P.WriteText(25, P.GetPaperHeight-70, '(25mm,70mm) FreeSans: 0oO 1lL - wêreld çèûÎÐð£¢ß');
  lPt1 := P.Matrix.Transform(25, 76);
  P.WriteText(lPt1.X, lPt1.Y, '(25mm,76mm) - FreeSans font');

  P.WriteUTF8Text(25, P.GetPaperHeight-200, 'Hello Graeme *'#$E2#$95#$AC'*'#$C3#$A4); // 0xE2 0x95 0xAC is UTF-8 for ╬   and   0xC3 0xA4 is UTF-8 for ä
  lPt1 := P.Matrix.Transform(25, 210);
  P.WriteUTF8Text(lPt1.X, lPt1.Y, 'В субботу двадцать третьего мая приезжает твоя любимая теща.');

  // Write text using Helvetica font
  P.SetFont(ftText2,12);
  P.SetColor(clBlue, false);
  lPt1 := P.Matrix.Transform(25, 50);
  P.WriteText(lPt1.X, lPt1.Y, '(25mm,50mm) - Times: 0oO 1lL - wêreld çèûÎÐð£¢ß');
  P.SetFont(ftText2,16);
  P.SetColor($c00000, false);
  lPt1 := P.Matrix.Transform(75, 100);
  P.WriteText(lPt1.X, lPt1.Y, '(75mm,100mm) - Big text at absolute position');
end;

Procedure SimpleLinesRaw(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  lPt1, lPt2: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('helvetica-12', clBlack);

  { Page title }
  P.SetFont(FtTitle,23);
  P.SetColor(clBlack, false);
  lPt1 := P.Matrix.Transform(25, 20);
  P.WriteText(lPt1.X, lPt1.Y, 'Sample Line Drawing (DrawLine)');

  P.SetColor(clBlack,False); // clblue
  P.SetPenStyle(ppsSolid);
  lPt1 := P.Matrix.Transform(30, 100);
  lPt2 := P.Matrix.Transform(150, 150);
  P.DrawLine(lPt1, lPt2, 0.2);
  P.SetColor($0000FF,False); // clblue
  P.SetPenStyle(ppsDash);
  lPt1 := P.Matrix.Transform(50, 70);
  lPt2 := P.Matrix.Transform(180, 100);
  P.DrawLine(lPt1, lPt2, 0.1);
  P.SetColor($FF0000,False); // clRed
  P.SetPenStyle(ppsDashDot);
  lPt1 := P.Matrix.Transform(40, 140);
  lPt2 := P.Matrix.Transform(160, 80);
  P.DrawLine(lPt1, lPt2, 1);
  P.SetColor(clBlack,False); // clBlack
  P.SetPenStyle(ppsDashDotDot);
  lPt1 := P.Matrix.Transform(60, 50);
  lPt2 := P.Matrix.Transform(60, 120);
  P.DrawLine(lPt1, lPt2, 1.5);
  P.SetColor(clBlack,False); // clBlack
  P.SetPenStyle(ppsDot);
  lPt1 := P.Matrix.Transform(10, 80);
  lPt2 := P.Matrix.Transform(130, 130);
  P.DrawLine(lPt1, lPt2, 0.5);
end;

Procedure SimpleLines(D: TPDFDocument; APage: integer);
var
  P: TPDFPage;
  FtTitle: integer;
  TsThinBlack, TsThinBlue, TsThick, TsThinRed, TsThinBlackDot: Integer;
  lPt1, lPt2: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('helvetica-12', clRed);

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

Procedure SimpleImage(D: TPDFDocument; APage: integer);
Var
  P: TPDFPage;
  FtTitle: integer;
  IDX: Integer;
  W, H: Integer;
  lPt1: TPDFCoord;
begin
  P := D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('helvetica-12', clBlack);

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

Procedure SimpleShapes(D: TPDFDocument; APage: integer);
Var
  P : TPDFPage;
  FtTitle: integer;
//  FtText: integer;
  lPt1, lPt2, lPt3: TPDFCoord;
begin
  P:=D.Pages[APage];
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle := D.AddFont('helvetica-12', clBlack);

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

Var
  D: TPDFDocument;
begin
  D := SetupDocument;
  try
    D.FontDirectory := ExtractFIlePath(Paramstr(0))+'fonts'+PathDelim;

    SimpleText(D, 0);
    SimpleShapes(D, 1);
    SimpleLines(D, 2);
    SimpleLinesRaw(D, 3);
    SimpleImage(D, 4);

    SaveDocument(D);
  finally
    D.Free;
  end;
end.

