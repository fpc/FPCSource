{*****************************************************************************}
{
    $Id$
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
{
$Log$
Revision 1.6  2004-02-20 23:52:49  michael
+ Added support for 32-bit writing. Standard is still 24 bit.

Revision 1.5  2003/09/09 11:28:23  mazen
* fixing copyright section in the file header

Revision 1.4  2003/09/08 14:08:48  mazen
- all common defintions are now included into bmpcomn unit
- removed erronous code (causing exception)

Revision 1.3  2003/09/08 10:38:56  luk
- removed debug info
* prevented exceptions when using non indexed images

Revision 1.2  2003/09/04 22:29:43  luk
* correct color conversion (prevent range check errors)

Revision 1.1  2003/09/04 12:02:21  mazen
+ fpwritebmp.pas renamed to fpwritebmp.pp

Revision 1.1  2003/09/04 08:44:32  mazen
+ Adds support of writing BMP files

}
