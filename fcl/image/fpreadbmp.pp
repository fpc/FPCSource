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
  var
    Row,Coulumn,nBpLine,ReadSize:Integer;
    aColor:TFPcolor;
{$IFDEF UseDynArray}
    aLine:ARRAY OF TColorRGB;
{$ELSE UseDynArray}
    aLine:^TColorRGB;
{$ENDIF UseDynArray}
  begin
    Stream.Read(BFI,SizeOf(BFI));
    with BFI do
      begin
        Img.Width:=Width;
        Img.Height:=Height;
        BytesPerPixel:=BitCount SHR 3;
      end;
    if BytesPerPixel=1
    then
      begin
//        stream.read(Palet, bfh.bfOffset - 54);
      end
{Treating the 24bit BMP files}
    else
      begin
        nBpLine:=Img.Width*SizeOf(TColorRGB);
        ReadSize:=(nBpLine+3)AND $FFFFFFFC;//BMP needs evry line 4Bytes aligned
{$IFDEF UseDynArray}
        SetLength(aLine,Img.Width+1);//3 extra byte for BMP 4Bytes alignement.
{$ELSE UseDynArray}
        GetMem(aLine,(Img.Width+1)*SizeOf(TColorRGB));//3 extra byte for BMP 4Bytes alignement.
{$ENDIF UseDynArray}
        for Row:=img.Height-1 downto 0 do
          begin
            for Coulumn:=0 to img.Width-1 do
              with aLine[Coulumn],aColor do
                begin
{Use only the high byte to convert the color}
                  Red:=R shl 8;
                  Green:=G shl 8;
                  Blue:=B shl 8;
                  alpha:=AlphaOpaque;
                  img.colors[Coulumn,Row]:=aColor;
                end;
            Stream.Read(aLine{$IFNDEF UseDynArray}^{$ENDIF UseDynArray},ReadSize);
          end;
      end;
{$IFNDEF UseDynArray}
        FreeMem(aLine,(Img.Width+1)*SizeOf(TColorRGB));
{$ENDIF UseDynArray}
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
      else if Stream.Size<>bfSize
      then
        InternalCheck:=False
      else
        InternalCheck:=True;
end;

initialization
  ImageHandlers.RegisterImageReader ('BMP Format', 'bmp', TFPReaderBMP);
end.
{
$Log$
Revision 1.4  2003-09-30 06:17:38  mazen
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
