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
unit FPReadBMP;

interface

uses FPImage, classes, sysutils;

type
  TFPReaderBMP = class (TFPCustomImageReader)
    private
      BytesPerPixel:Integer;
    protected
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

implementation

uses BMPcomn;

constructor TFPReaderBMP.create;
begin
  inherited create;
end;

destructor TFPReaderBMP.Destroy;
begin
  inherited destroy;
end;

procedure TFPReaderBMP.InternalRead(Stream:TStream; Img:TFPCustomImage);
  var
    BFI:TBitMapInfoHeader;
    Row,Column,nBpLine,ReadSize:Integer;
    aColor:TFPcolor;
    palette: ARRAY OF TFPcolor;
    aLine:ARRAY OF TColorRGB;
    bLine:ARRAY OF TColorRGBA;
    mLine: array of Byte;
    function MakeFpColor(RGBA: TColorRGBA):TFPcolor;
    begin
      with Result, RGBA do begin
        Red := (R shl 8) or R;
        Green := (G shl 8) or G;
        Blue := (B shl 8) or B;
        alpha := AlphaOpaque;
      end;
    end;
    procedure SetupRead(nPalette, nRowBits: Integer);
    var
      ColInfo: ARRAY OF TColorRGBA;
      i: Integer;
    begin
      if nPalette > 0 then begin
        SetLength(palette, nPalette);
        SetLength(ColInfo, nPalette);
        if BFI.ClrUsed > 0 then
          Stream.Read(ColInfo[0], BFI.ClrUsed*SizeOf(TColorRGBA))
        else if nPalette > 0 then
          Stream.Read(ColInfo[0], nPalette*SizeOf(TColorRGBA));
      end else
        if BFI.ClrUsed > 0 then { Skip palette }
          Stream.Position := Stream.Position + BFI.ClrUsed*SizeOf(TColorRGBA);
      for i := 0 to High(ColInfo) do
        palette[i] := MakeFpColor(ColInfo[i]);
      ReadSize := ((nRowBits + 31) div 32) shl 2;
    end;
  begin
    Stream.Read(BFI,SizeOf(BFI));
    { This will move past any junk after the BFI header }
    Stream.Position := Stream.Position - SizeOf(BFI) + BFI.Size;
    with BFI do
      begin
        Img.Width:=Width;
        Img.Height:=Height;
      end;
    if BFI.BitCount = 1 then begin
      { Monochrome }
      SetupRead(2, Img.Width);
      SetLength(mLine, ReadSize);
      for Row:=Img.Height-1 downto 0 do begin
        Stream.Read(mLine[0],ReadSize);
        for Column:=0 to Img.Width-1 do
          if ((mLine[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
            img.colors[Column,Row] := Palette[1]
          else
            img.colors[Column,Row] := Palette[0];
       end;
    end else if BFI.BitCount = 4 then begin
      SetupRead(16, Img.Width*4);
      SetLength(mLine, ReadSize);
      for Row:=img.Height-1 downto 0 do begin
        Stream.Read(mLine[0],ReadSize);
        for Column:=0 to img.Width-1 do
          img.colors[Column,Row] := Palette[(mLine[Column div 2] shr (((Column+1) and 1)*4)) and $0f];
       end;
    end else if BFI.BitCount = 8 then begin
      SetupRead(256, Img.Width*8);
      SetLength(mLine, ReadSize);
      for Row:=img.Height-1 downto 0 do begin
        Stream.Read(mLine[0],ReadSize);
        for Column:=0 to img.Width-1 do
          img.colors[Column,Row] := Palette[mLine[Column]];
       end;
    end else if BFI.BitCount = 16 then begin
      raise Exception.Create('16 bpp bitmaps not supported');
{Treating the 24bit BMP files}
    end else if BFI.BitCount=24 then
      begin
        SetupRead(0, Img.Width*8*3);
        SetLength(aLine,ReadSize);//3 extra byte for BMP 4Bytes alignement.
        for Row:=img.Height-1 downto 0 do
          begin
            Stream.Read(aLine[0],ReadSize);
            for Column:=0 to img.Width-1 do
              with aLine[Column],aColor do
                begin
{Use only the high byte to convert the color}
                  Red := (R shl 8) + R;
                  Green := (G shl 8) + G;
                  Blue := (B shl 8) + B;
                  alpha := AlphaOpaque;
                  img.colors[Column,Row]:=aColor;
                end;
          end;
      end
    else if BFI.BitCount=32 then
      begin
        SetupRead(0, Img.Width*8*4);
        SetLength(bLine,ReadSize);
        for Row:=img.Height-1 downto 0 do
          begin
            Stream.Read(bLine[0],ReadSize);
            for Column:=0 to img.Width-1 do
              img.colors[Column,Row]:=MakeFpColor(bLine[Column])
          end;
      end;
  end;

function  TFPReaderBMP.InternalCheck (Stream:TStream) : boolean;
  var
    BFH:TBitMapFileHeader;
  begin
    stream.Read(BFH,SizeOf(BFH));
    with BFH do
      if bfType<>BMmagic
      then
        InternalCheck:=False
      else { Do not check size to allow multiple bitmaps per stream }
        InternalCheck:=True;
end;

initialization
  ImageHandlers.RegisterImageReader ('BMP Format', 'bmp', TFPReaderBMP);
end.
{
$Log$
Revision 1.6  2004-02-15 20:59:06  michael
+ Patch from Colin Western

Revision 1.5  2003/09/30 14:17:05  luk
* better color conversion (White didn't stay white)

Revision 1.4  2003/09/30 06:17:38  mazen
- all common defintions are now included into bmpcomn unit

Revision 1.3  2003/09/15 11:39:01  mazen
* fixed InternalRead method to load BMP files.
  But still too long to load images.

Revision 1.2  2003/09/09 11:26:59  mazen
+ setting image attributes when loading images
* fixing copyright section in the file header

Revision 1.1  2003/09/08 14:10:10  mazen
+ adding support for loading bmp images

}
