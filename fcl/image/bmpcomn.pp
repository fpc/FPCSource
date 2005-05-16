{*****************************************************************************}
{
    $Id: bmpcomn.pp,v 1.5 2005/02/14 17:13:12 peter Exp $
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP reader/writer common code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{$mode objfpc}{$h+}
unit BMPcomn;

interface

const
{BMP magic word is always 19778 : 'BM'}
  BMmagic=19778;
type

   TBitMapFileHeader = packed record
{00+02 :File type}
      bfType:word;
{02+04 :File size in bytes}
      bfSize:longint;
{06+04 : Reserved}
      bfReserved:longint;
{10+04 : Offset of image data : size if the file hieder + the info header}
      bfOffset:longint;
   end;
   PBitMapFileHeader = ^TBitMapFileHeader;

   TBitMapInfoHeader = packed record
{14+04 : Size of the bitmap info header : sould be 40=$28}
      Size:longint;
{18+04 : Image width in pixels}
      Width:longint;
{22+04 : Image height in pixels}
      Height:longint;
{26+02 : Number of image planes : should be 1 always}
      Planes:word;
{28+02 : Color resolution : Number of bits per pixel (1,4,8,24)}
      BitCount:word;
{30+04 : Compression Type}
      Compression:longint;
{34+04 : Size of compressed image : should be 0 if no compression}
      SizeImage:longint;
{38+04 : Horizontal resolution in pixel/meter}
      XPelsPerMeter:Longint;
{42+04 : Vertical resolution in pixel/meter}
      YPelsPerMeter:Longint;
{46+04 : Number of coros used}
      ClrUsed:longint;
{50+04 : Number of imprtant colors used : usefull for displaying on VGA256}
      ClrImportant:longint;
   end;
   PBitMapInfoHeader = ^TBitMapInfoHeader;

   TColorRGB=packed record
     B,G,R:Byte;
   end;
   PColorRGB = ^TColorRGB;

   TColorRGBA=packed record
   case Boolean of
      False:(B,G,R,A:Byte);
      True:(RGB:TColorRGB);
   end;
   PColorRGBA = ^TColorRGBA;

{54+?? : Color map : Lenght of color map is 4 bytes + the rest until the beginning of image data fixed in BFH.bfOffset}
    TColorMap=TColorRGBA;

implementation

end.
{
$Log: bmpcomn.pp,v $
Revision 1.5  2005/02/14 17:13:12  peter
  * truncate log

}
