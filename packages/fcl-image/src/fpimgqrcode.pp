{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by Michael Van Canneyt, member of the Free Pascal development team

    fpImage QR code drawing algorithm.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpimgqrcode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpImage, fpqrcodegen;

type

  { TImageQRCodeGenerator }

  TImageQRCodeGenerator = Class(TQRCodeGenerator)
  private
    FOrigin: TPoint;
    FPixelSize: Integer;
  Public
    Constructor Create; override;
    Procedure Draw(Img : TFPCustomImage);
    // overrides Origin.
    Procedure SaveToFile(const AFileName : String; aBorder : Integer = 0);
    Property PixelSize : Integer Read FPixelSize Write FPixelSize default 2;
    Property Origin : TPoint Read FOrigin Write FOrigin;
  end;

Procedure DrawQRCode(Img : TFPCustomImage; QRCode : TQRBuffer; aOrigin: TPoint; PixelSize : Byte = 1);

implementation

Procedure DrawQRCode(Img : TFPCustomImage; QRCode : TQRBuffer; aOrigin: TPoint; PixelSize : Byte = 1);

Var
  X,Y,PH,PV,PX,PY,S : Word;
  col : TFPColor;

begin
  PY:=aOrigin.Y;
  S:=QRGetSize(QRCode);
//  Writeln('Size ',S);
  if S=0 then
      exit;
  For Y:=0 to S-1 do
    begin
    PX:=aOrigin.X;
    For X:=0 to S-1 do
      begin
      if QRgetModule(QRCode,X,Y) then
        begin
        Col:=colBlack;
//        Write('##');
        end
      else
        begin
        Col:=colWhite;
//        Write('  ');
        end;
      For pV:=0 to PixelSize-1 do
        For pH:=0 to PixelSize-1 do
          Img.Colors[PX+PH,PY+PV]:=col;
      Inc(PX,PixelSize);
      end;
//    Writeln;
    Inc(PY,PixelSize);
    end;
end;

{ TImageQRCodeGenerator }

constructor TImageQRCodeGenerator.Create;
begin
  inherited Create;
  FPixelSize:=2;
end;

procedure TImageQRCodeGenerator.Draw(Img: TFPCustomImage);
begin
  DrawQRCode(Img,Bytes,FOrigin,PixelSize);
end;

procedure TImageQRCodeGenerator.SaveToFile(const AFileName: String; aBorder: Integer);


Var
  Img : TFPCustomImage;
  D,S,X,Y : Word;


begin
  S:=Size;
  if S=0 then exit;
  D:=PixelSize*S;
  Img:=TFPCompactImgGray8Bit.Create(D+aBorder*2,D+aBorder*2);
  try
    For X:=0 to D+(aBorder*2)-1 do
      For Y:=1 to aBorder do
        begin
        Img[X,Y-1]:=colWhite;
        Img[X,D+(aBorder*2)-Y]:=colWhite;
        end;
    For Y:=aBorder to D+aBorder-1 do
      For X:=1 to aBorder do
        begin
        Img[X-1,Y]:=colWhite;
        Img[D+(aBorder*2)-X,Y]:=colWhite;
        end;
    Origin:=Point(aBorder,aBorder);
    Draw(Img);
    Img.SaveToFile(aFileName);
  finally
    Img.Free;
  end;
end;

end.

