{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG reader/writer common code.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit BMPcomn;

interface

const
{BMP magic word is always 19778 : 'BM'}
  BMmagic=19778;
type

   TBitMapFileHeader = record
{File type}
      bfType:word;
{File size in bytes}
      bfSize:longint;
      bfReserved:longint;
{Offset of image data}
      bfOffset:longint;
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
  
    TColorRGB=packed record
      B,G,R:Byte;
    end;
implementation

end.
