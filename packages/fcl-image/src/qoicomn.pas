{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    QOI reader/writer common code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit qoicomn;
{$ENDIF FPC_DOTTEDUNITS}

interface

type  PQoiHeader = ^TQoiHeader;
      TQoiHeader = packed record
         magic  : array [0..3] of AnsiChar; { magic bytes 'qoif' }
         width  : dword;                { image width in pixels (BE)}
         height : dword;                { image height in pixels (BE)}
         channels   : byte;             { 3 = RGB, 4 = RGBA }
         colorspace : byte;             { 0 = sRGB with linear alpha }
                                        { 1 = all channels linear }
     end;


type  PQoiPixel = ^TQoiPixel;
      TQoiPixel = packed record
         r,g,b,a : byte;
      end;

const qoChannelRGB  = 3;
      qoChannelRGBA = 4;

function QoiPixelIndex (px : TQoiPixel):dword;

implementation

function QoiPixelIndex (px : TQoiPixel):dword;
begin
     QoiPixelIndex:= (dword(px.r)*3+dword(px.g)*5+dword(px.b)*7+dword(px.a)*11) and 63;
end;


end.
