program demogradient;

uses
  Types, FPImage, FPCanvas, FPImgCanv, FPWriteBMP;

var
  img: TFPMemoryImage;
  canv: TFPCustomCanvas;

begin
  img := TFPMemoryImage.Create(256, 128);
  try
    canv := TFPImageCanvas.Create(img);
    try
      canv.GradientFill(Rect(0, 0, 128, img.Height), colRed, colYellow, gdVertical);
      canv.GradientFill(Rect(128, 0, 256, img.Height), colRed, colYellow, gdHorizontal);
      img.SaveToFile('test.bmp');
    finally
      canv.Free;
    end;
  finally
    img.Free;
  end;
end.
