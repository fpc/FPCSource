{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    BMP writer implementation.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPWriteBMP;

interface

uses FPImage, classes, sysutils;

type
   
   TBitMapFileHeader = record
      bfType:word;              // is always 19778 : 'BM'
      bfSize:longint;           // Filesize
      bfReserved:longint;
      bfOffset:longint;         // Offset of image data
   end;

   TBitMapInfoHeader = record
      Size:longint;
      Width:longint;
      Height:longint;
      Planes:word;
      BitCount:word;
      Compression:longint;
      SizeImage:longint;
      XPelsPerMeter:Longint;
      YPelsPerMeter:Longint;
      ClrUsed:longint;
      ClrImportant:longint;
   end;
  
  TFPWriterBMP = class (TFPCustomImageWriter)
    private
      BytesPerPixel:Integer;
      procedure SetColorSize (AValue : byte);
    protected
      procedure InternalWrite (Stream:TStream; Img:TFPCustomImage); override;
    public
      constructor Create; override;
  end;


implementation

constructor TFPWriterBMP.create;
begin
  inherited create;
  BytesPerPixel := 3
end;

procedure TFPWriterBMP.SetColorSize (AValue : byte);
begin
  if AValue >= 3
  then
    BytesPerPixel := 3
  else if AValue = 0
  then
    BytesPerPixel := 1
  else
    BytesPerPixel := AValue;
end;

procedure TFPWriterBMP.InternalWrite (Stream:TStream; Img:TFPCustomImage);
  function SaveHeader(stream:TStream):boolean;
    var
      BFH:TBitMapFileHeader;
      BFI:TBitMapInfoHeader;
    begin
      SaveHeader := false;
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
          ClrUsed:=0;
          ClrImportant:=0;
        end;
      with BFH do
        begin
          bfType:=19778;//'BM'
          bfOffset:=sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader);
          bfReserved:=0;
          bfSize:=bfOffset+BFI.SizeImage*BytesPerPixel;
        end;
      stream.seek(0,soFromBeginning);
      stream.Write(bfh,sizeof(TBitMapFileHeader));
      stream.Write(bfi,sizeof(TBitMapInfoHeader));
      if(bfi.bitCount = 8)
      then
        begin
//          stream.Write(Palet, bfh.bfOffset - 54);
        end;
      SaveHeader := true;
    end;
  type
    TPixel=packed record
      B,G,R:Byte;
    end;
  var
    Row,Coulumn,nBpLine,WriteSize:Integer;
    aColor:TFPcolor;
{$IFDEF UseDynArray}
    aLine:ARRAY OF TPixel;
{$ELSE UseDynArray}
    aLine:^TPixel;
{$ENDIF UseDynArray}
  begin
    SaveHeader(Stream);
    nBpLine:=Img.Width*SizeOf(TPixel);
    WriteSize:=(nBpLine+3)AND $FFFFFFFC;//BMP needs evry line 4Bytes aligned
{$IFDEF UseDynArray}
    SetLength(aLine,Img.Width+1);//3 extra byte for BMP 4Bytes alignement.
{$ELSE UseDynArray}
    GetMem(aLine,(Img.Width+1)*SizeOf(TPixel));//3 extra byte for BMP 4Bytes alignement.
{$ENDIF UseDynArray}
    for Row:=img.Height-1 downto 0 do
      begin
        for Coulumn:=0 to img.Width-1 do
          with aLine[Coulumn],aColor do
            begin
              //aColor:=img.Palette.Color[img.Pixels[Coulumn,Row]]; // Will raise exception when image doesn't have palette
              aColor := img.colors[Coulumn,Row];
              R:=(Red and $FF00) shr 8;  // Use only the high byte to convert the color
              G:=(Green and $FF00) shr 8;
              B:=(Blue and $FF00) shr 8;
            end;
        Stream.Write(aLine{$IFNDEF UseDynArray}^{$ENDIF UseDynArray},WriteSize);
      end;
{$IFNDEF UseDynArray}
    FreeMem(aLine,(Img.Width+1)*SizeOf(TPixel));
{$ENDIF UseDynArray}
  end;

initialization
  ImageHandlers.RegisterImageWriter ('BMP Format', 'bmp', TFPWriterBMP);
end.
{
$Log$
Revision 1.3  2003-09-08 10:38:56  luk
- removed debug info
* prevented exceptions when using non indexed images

Revision 1.2  2003/09/04 22:29:43  luk
* correct color conversion (prevent range check errors)

Revision 1.1  2003/09/04 12:02:21  mazen
+ fpwritebmp.pas renamed to fpwritebmp.pp

Revision 1.1  2003/09/04 08:44:32  mazen
+ Adds support of writing BMP files

}
