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
unit FPReadBMP;

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

const
  WhiteSpace = ' '#8#10#13;

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
      if(bitCount = 8)
      then
        begin
//          stream.read(Palet, bfh.bfOffset - 54);
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
                    img.colors[Coulumn,Row]:=aColor;
                  end;
              Stream.Write(aLine{$IFNDEF UseDynArray}^{$ENDIF UseDynArray},ReadSize);
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
Revision 1.1  2003-09-08 14:10:10  mazen
+ adding support for loading bmp images

}
