{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{$mode objfpc}{$h+}
unit FPWriteBMP;

interface

uses FPImage, classes, sysutils;

type

  TFPWriterBMP = class (TFPCustomImageWriter)
  private
    FBytesPerPixel : Byte;
    procedure SetColorSize (AValue : Byte);
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    Property BytesPerPixel : Byte Read FBytesPerPixel Write SetColorSize;
  end;


implementation

uses BMPcomn;

Function FPColorToRGB(Const Color : TFPColor) : TColorRGB;

begin
  With Result,Color do
    begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    end;
end;

Function FPColorToRGBA(Const Color : TFPColor) : TColorRGBA;

begin
  With Result,Color do
    begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    A:=(Alpha and $FF00) shr 8;
    end;
end;

constructor TFPWriterBMP.create;
begin
  inherited create;
  FBytesPerPixel:=3;
end;

procedure TFPWriterBMP.SetColorSize (AValue : byte);
begin
  if (AValue>4) then
    AValue:=4;
  if (AValue<1) then
    AValue:=1;
  FBytesPerPixel:=AValue;
end;

function TFPWriterBMP.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;

var
  BFH:TBitMapFileHeader;
  BFI:TBitMapInfoHeader;

begin
  Result:=False;
  with BFI do
    begin
    Size:=sizeof(TBitMapInfoHeader);
    Width:=Img.Width;
    Height:=Img.Height;
    Planes:=1;
    BitCount:=BytesPerPixel SHL 3;
    Compression:=0;
    SizeImage:=Width*Height;
    XPelsPerMeter:=100;
    YPelsPerMeter:=100;
    ClrUsed:=0; // No palette yet.
    ClrImportant:=0;
    end;
  with BFH do
    begin
    bfType:=BMmagic;//'BM'
    bfOffset:=sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader);
    bfReserved:=0;
    bfSize:=bfOffset+BFI.SizeImage*BytesPerPixel;
    end;
  Stream.seek(0,soFromBeginning);
  Stream.Write(bfh,sizeof(TBitMapFileHeader));
  Stream.Write(bfi,sizeof(TBitMapInfoHeader));
  Result:=true;
end;

procedure TFPWriterBMP.InternalWrite (Stream:TStream; Img:TFPCustomImage);

var
  Row,Col,nBpLine,WriteSize:Integer;
  aLine: PByte;
  S : Integer;

begin
  If Not (BytesPerPixel in [3,4]) then
    Raise FPImageException.Create('Only 24 or 32 bit images are currently supported.');
  SaveHeader(Stream,Img);
  nBpLine:=Img.Width*BytesPerPixel;
  WriteSize:=(nBpLine+3) AND $FFFFFFFC; //BMP needs evry line 4Bytes aligned
  GetMem(aLine,(Img.Width+1)*BytesPerPixel);//3 extra byte for BMP 4Bytes alignement.
  Try
    for Row:=Img.Height-1 downto 0 do
      begin
      Case BytesPerPixel of
        3 : for Col:=0 to img.Width-1 do
              PColorRGB(aLine)[Col]:=FPColorToRGB(img.colors[Col,Row]);
        4 : for Col:=0 to img.Width-1 do
              PColorRGBA(aLine)[Col]:=FPColorToRGBA(img.colors[Col,Row]);
      end;
      Stream.Write(aLine[0],WriteSize);
      end;
  Finally
    FreeMem(aLine);
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('BMP Format', 'bmp', TFPWriterBMP);
end.
