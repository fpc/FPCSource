(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba PCX decoder

	Copyright 2003-2004 by Dave Murphy.

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Library General Public
	License as published by the Free Software Foundation; either
	version 2 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Library General Public License for more details.

	You should have received a copy of the GNU Library General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
	MA 02110-1301, USA.

	Please report all bugs and problems through the bug tracker at
	"http://sourceforge.net/tracker/?group_id=114505&atid=668551".

  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$



*)

{$ifdef GBA_INTERFACE}
type
  pcx_header = packed record
    manufacturer: byte;
    version: byte;
    encoding: byte;
    bpp: byte;
    x1,y1: u16;
    x2,y2: u16;
    hres: u16;
    vres: u16;
    palette: array [0..47] of byte;
    reserved: byte;
    color_planes: byte;
    BytesPerLine: u16;
    PaletteType: u16;
    dummy: array [0..57] of byte;
  end;

procedure DecodePCX(const PCXBuffer: pu8; ScreenAddr, Palette: pu16); cdecl; external;

{$endif GBA_INTERFACE}
