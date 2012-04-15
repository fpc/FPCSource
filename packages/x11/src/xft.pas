{ Original C header:
 * include/X11/Xft/Xft.h.  Generated from Xft.h.in by configure.  */
/*
 * Copyright © 2000 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Keith Packard not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Keith Packard makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *}
{
    Initial Pascal headers obtained from:
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Headers extended and modified by Felipe Monteiro de Carvalho in 2011

    Description:
      Xft interface functions
}
unit xft;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, X, XLib, Xutil, ctypes, fontconfig, xrender;

//#include FT_FREETYPE_H
//#include <fontconfig/fontconfig.h>
//#include <X11/extensions/Xrender.h>

const
  {$IF Defined(DARWIN)}
    libXft = 'libXft.dylib';
    {$linklib Xft}
  {$ELSE}
    libXft = 'libXft.so';
  {$IFEND}

{* Current Xft version number, set from version in the Xft configure.ac file. *}
  XFT_MAJOR = 2;
  XFT_MINOR = 2;
  XFT_REVISION = 0;

  XFT_VERSION = ((XFT_MAJOR * 10000) + (XFT_MINOR * 100) + (XFT_REVISION));
  XftVersion = XFT_VERSION;

  XFT_CORE = 'core';
  XFT_RENDER	=	'render';
  XFT_XLFD	=	'xlfd';
  XFT_MAX_GLYPH_MEMORY ='maxglyphmemory';
  XFT_MAX_UNREF_FONTS =	'maxunreffonts';
  
type
// extern FT_Library	_XftFTlibrary;

  TXftFontInfo = record end;

  TPicture = longword;

  TXftDraw = record end;
  PXftDraw = ^TXftDraw;

  TXftFont = record
   ascent   : cint;
   descent  : cint;
   height   : cint;
   max_advance_width : cint;
   ptr1     : PFcCharSet;
   ptr2     : PFcPattern;
  end;
  PXftFont = ^TXftFont;

  TXftColor = record
    pixel : culong;
    color : TXRenderColor;
  end;
              
function  XftDrawCreate(display : PXDisplay; win : TXID; vis : PVisual; colorm : longint) : PXftDraw; cdecl; external libXft;
procedure XftDrawChange(xftd : PXftDraw; win : TXID); cdecl; external libXft;
procedure XftDrawDestroy(draw : PXftDraw); cdecl; external libXft;
function  XftDrawPicture(draw : PXftDraw) : TPicture; cdecl; external libXft;
function  XftFontOpenName(display : PXDisplay; scr : integer; par3 : PChar) : PXftFont; cdecl; external libXft;
procedure XftFontClose(display : PXDisplay; fnt : PXftFont); cdecl; external libXft;
procedure XftDrawStringUtf8(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external libXft;
procedure XftDrawString8(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external libXft;
procedure XftDrawString16(draw : PXftDraw; var col : TXftColor; fnt : PXftFont; x,y : integer; txt : PChar; len : integer); cdecl; external libXft;
procedure XftTextExtentsUtf8(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external libXft;
procedure XftTextExtents8(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external libXft;
procedure XftTextExtents16(display : PXDisplay; fnt : PXftFont; txt : PChar; len : integer; var extents : TXGlyphInfo); cdecl; external libXft;
//function XftGlyphExists(display : PXDisplay; fnt : PXftFont; ch : integer) : longbool; cdecl; external libXft;
//procedure XftDrawSetClipRectangles(draw : PXftDraw; xorigin, yorigin : integer; rect : PXRectangle; rnum : integer); cdecl; external libXft;
procedure XftDrawSetClip(draw : PXftDraw; rg : TRegion); cdecl; external libXft;
function  XftListFonts(display : PXDisplay; screen : integer; params : array of const) : PFcFontSet; cdecl; external libXft;
function  XftNameUnparse(pat : PFcPattern; dest : PChar; destlen : integer) : boolean; cdecl; external libXft;
procedure FcFontSetDestroy(fsp : PFcFontSet); cdecl; external libXft;

implementation

end.

