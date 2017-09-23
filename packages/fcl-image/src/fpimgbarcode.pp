unit fpimgbarcode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcanvas, fpimage, types, fpbarcode;


Function DrawBarCode(Img : TFPCustomImage; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;
Function DrawBarCode(Img : TFPCustomImage; Rect : TRect; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;

implementation

uses
  FPImgCanv;

Function DrawBarCode(Img : TFPCustomImage; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;

Var
  T : TRect;

begin
  T.Left:=0;
  T.Top:=0;
  T.Right:=Img.Width-1;
  T.Bottom:=Img.Height-1;
  Result:=DrawBarCode(Img,T,S,E,aWidth,aWeight);
end;

Function DrawBarCode(Img : TFPCustomImage; Rect : TRect; S : String; E : TBarcodeEncoding; aWidth : Integer = 1; AWeight : Double = 2.0) : Boolean;

Var
  Cnv : TFPImageCanvas;
  BWT : TBarWidthArray;
  i: integer;
  xOffset: integer;
  w, h: integer;
  BarRect : TRect;
  BP : TBarParams;
  Data : TBarTypeArray;

begin
  BWT:=CalcBarWidths(E,aWidth,aWeight);
  Data:=StringToBarTypeArray(S,E);
  Cnv:=TFPImageCanvas.Create(Img);
  try
    xOffset := 0;
    Cnv.Brush.FPColor := colWhite;
    Cnv.Brush.Style:=bsSolid;
    Cnv.FillRect(Rect);
    Cnv.Pen.Width := 1;
    for i:=0 to Length(Data)-1 do
      begin
      BP:=BarTypeToBarParams(Data[i]);
      case BP.c of
        bcBlack : Cnv.Pen.FPColor := colBlack;
        bcWhite : Cnv.Pen.FPColor := colWhite;
      end;
      W:=BWT[BP.w];
      Cnv.Brush.FPColor:=Cnv.Pen.FPColor;
      H:=Rect.Bottom-Rect.Top;
      if BP.h=bhTwoFifth then
        H:=H*2 div 5;
      BarRect.Left:=Rect.Left+xOffset;
      BarRect.Top:=Rect.Top;
      BarRect.Bottom:=Rect.Top+H;
      BarRect.Right:=BarRect.Left + W-1;
      Cnv.FillRect(BarRect);
      xOffset:=xOffset + W;
      end;
  finally
    Cnv.Free;
  end;
end;


end.

