{$mode objfpc}{$h+}
program Drawing;

uses classes, sysutils,
     FPImage, FPCanvas, FPImgCanv, ftFont,
     FPWritePNG, FPReadPNG;

const
  MyColor : TFPColor = (Red: $7FFF; Green: $0000; Blue: $FFFF; Alpha: alphaOpaque);

procedure DoDraw;
var canvas : TFPcustomCAnvas;
    ci, image : TFPCustomImage;
    writer : TFPCustomImageWriter;
    reader : TFPCustomImageReader;
    f : TFreeTypeFont;
begin
  image := TFPMemoryImage.Create (100,100);
  ci := TFPMemoryImage.Create (20,20);
  Canvas := TFPImageCanvas.Create (image);
  Writer := TFPWriterPNG.Create;
  reader := TFPReaderPNG.Create;
  with TFPWriterPNG(Writer) do
    begin
    indexed := false;
    wordsized := false;
    UseAlpha := false;
    GrayScale := false;
    end;
  try
//    ci.LoadFromFile ('test.png', reader);
    with Canvas as TFPImageCanvas do
      begin
      pen.mode := pmCopy;
      pen.style := psSolid;
      pen.width := 1;
      pen.FPColor := colred;
      with pen.FPColor do
        red := red div 4;
      Ellipse (10,10, 90,90);

      pen.style := psDashDot;
      pen.FPColor := colred;
      HashWidth := 10;
      Ellipse (10,10, 90,90);

      with pen.FPColor do
        begin
        red := red div 2;
        green := red div 4;
        blue := green;
        end;
      pen.style := psSolid;
      RelativeBrushImage := true;
{
      brush.image := ci;
      brush.style := bsimage;
      with brush.FPColor do
        green := green div 2;
      Ellipse (11,11, 89,89);
}

      brush.style := bsSolid;
      brush.FPColor := MyColor;
      pen.style := psSolid;
      pen.width := 3;
      pen.FPColor := colSilver;
      ellipse (30,35, 70,65);

      pen.width := 1;
      pen.FPColor := colCyan;
      ellipseC (50,50, 1,1);

      InitEngine;
      F:=TFreeTypeFont.Create;
      F.Angle:=0.15;
      Font:=F;
//      Font.Name:='/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf';
      Font.Name:='/home/michael/Documents/arial.ttf';
      Font.Size:=10;
      Font.FPColor:=colWhite;
//      Font.Orientation:=900;
      
      Canvas.TextOut(10,90,'o');
      end;
      writeln ('Saving to inspect !');
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
//  DefaultFontPath := '/usr/share/fonts/truetype/ttf-dejavu/';
  DoDraw;

end.
