{$mode objfpc}{$h+}
program Drawing;

uses classes, sysutils,
     FPImage, FPCanvas, FPImgCanv,
     FPWritePNG, FPReadPNG,
     ftfont;

const
  MyColor : TFPColor = (Red: $7FFF; Green: $0000; Blue: $FFFF; Alpha: alphaOpaque);

procedure DoDraw;
var canvas : TFPcustomCAnvas;
    ci, image : TFPCustomImage;
    writer : TFPCustomImageWriter;
    reader : TFPCustomImageReader;
    ff : string;
    afont : TFreeTypeFont;
begin
  image := TFPMemoryImage.Create (16,16);
  ci := TFPMemoryImage.Create (20,20);
  Canvas := TFPImageCanvas.Create (image);
  Writer := TFPWriterPNG.Create;
  reader := TFPReaderPNG.Create;
  with TFPWriterPNG(Writer) do
    begin
    indexed := false;
    wordsized := true;
    UseAlpha := false;
    GrayScale := false;
    end;
  try
    if paramcount > 0 then
      ff := paramstr(1)
    else
      ff := 'arial';
    ci.LoadFromFile ('test.png', reader);
    with Canvas as TFPImageCanvas do
      begin
      height := 30;
      width := 60;
      with brush do
        begin
        color := colBlue;
        style := bsSolid;
        end;
      rectangle(0,0, 20,29);
      with pen do
        begin
        color := colLtGray;
        end;
      Line (0,18, 59,18);
      afont := TFreeTypeFont.Create;
      with afont do
        begin
        name := ff;
        fontindex := 0;
        size := 12;
        color := colWhite;
        AntiAliased := True;
        resolution := 96;
        end;
      font := afont;
      writeln ('Outputting texts');
      // TextOut (20,30, 'Font: '+font.name);
      font.color := colLtGray;
      //TextOut (40,80, 'Meer van dit, veel meer...');
      writeln (Gettextwidth('correct?'));
      writeln (Gettextheight('correct?'));
      TextOut (5,17, 'correct?');
      with colors[6,7] do
        writeln ('color 6,7 = ',red,',',green,',',blue);
      aFont.antialiased := False;
      afont.angle := -0.523598;
      font.color := colLtGray;
      //TextOut (40,100, 'Meer van dit, veel meer...');
      font.color := colRed;
      font.size := 24;
      aFont.Angle := PI / 2.4;
      font.color := colGreen;
      //TextOut (100,240, 'HOERA !');
      font.size := 26;
      aFont.Angle := aFont.Angle + (pi / 90);
      font.color := colBlue;
      //TextOut (250,240, 'HOERA !');
      font.size := 28;
      aFont.Angle := aFont.Angle + (pi / 90);
      font.color := colRed;
      //TextOut (400,240, 'HOERA !');
      writeln ('Text written');
{      brush.color := colYellow;
      brush.Style := bsSolid;
      rectangle (60,0, 130,40);

      pen.color := colSilver;
      pen.mode := pmCopy;
      pen.style := psSolid;
      pen.width := 1;
      brush.color := MyColor;
      pen.color := colBlue;
      Rectangle (0,160, 120,200);

      brush.style := bsDiagCross;
      brush.color := colGreen;
      HashWidth := 10;
      pen.color := colSilver;
      Rectangle (150,50, 250,150);

      writeln ('Saving to inspect !');
}      end;
    image.SaveToFile ('DrawTest.png', writer);
  finally
    Canvas.Free;
    image.Free;
    writer.Free;
    ci.free;
    reader.Free;
  end;
end;

begin
  // DefaultFontPath := 'c:\winnt\fonts\';
  DoDraw;

end.
