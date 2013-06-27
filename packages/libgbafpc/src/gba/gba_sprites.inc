(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba sprite definitions

	Copyright 2003-2006 by Dave Murphy.

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
  OBJATTR = record
    attr0: u16;
    attr1: u16;
    attr2: u16;
    dummy: u16;
  end; 
  POBJATTR = ^OBJATTR;

  SpriteEntry = record
    attribute: array [0..2] of u16;
    dummy: u16;
  end;
  PSpriteEntry = ^SpriteEntry;

  OBJAFFINE = record
    dummy0: array [0..2] of u16;
    pa: s16;
    dummy1: array [0..2] of u16;
    pb: s16;
    dummy2: array [0..2] of u16;
    pc: s16;
    dummy3: array [0..2] of u16;
    pd: s16;
  end;
  POBJAFFINE = ^OBJAFFINE;


const
  OAM           : POBJATTR  =	pointer($07000000);
  OBJ_BASE_ADR		          = pointer(VRAM + $10000);
  SPRITE_GFX    : pu16      = pointer(VRAM + $10000);
  BITMAP_OBJ_BASE_ADR	      = pointer(VRAM + $14000);

type
  SPRITE_SHAPES = integer;

const  
  SQUARE: SPRITE_SHAPES = 0;
  WIDE: SPRITE_SHAPES = 1; 
  TALL: SPRITE_SHAPES = 2;
  
  
function OBJ_SHAPE(m: integer): integer; inline;

// Sprite Attribute 0
function OBJ_Y(m: integer): integer; inline;

const
  OBJ_ROT_SCALE_ON  = (1 shl 8);
  OBJ_DISABLE = (1 shl 9);
  OBJ_DOUBLE  = (1 shl 9);

function OBJ_MODE(m: integer): integer; inline;

const
  OBJ_MOSAIC    = (1 shl 12);
  OBJ_256_COLOR = (1 shl 13);
  OBJ_16_COLOR  = (0 shl 13);

  ATTR0_MOSAIC			    = (1 shl 12);
  ATTR0_COLOR_256			  = (1 shl 13);
  ATTR0_COLOR_16			  = (0 shl 13);
  ATTR0_NORMAL			    = (0 shl 8);
  ATTR0_ROTSCALE			  = (1 shl 8);
  ATTR0_DISABLED			  = (2 shl 8);
  ATTR0_ROTSCALE_DOUBLE = (3 shl 8);

{$define  ATTR0_SQUARE	:= OBJ_SHAPE(SQUARE)}
{$define  ATTR0_WIDE		:= OBJ_SHAPE(WIDE)}
{$define  ATTR0_TALL		:= OBJ_SHAPE(TALL)}



// Sprite Attribute 1
function OBJ_X(m: integer): integer; inline;

function OBJ_ROT_SCALE(m: integer): integer; inline;

const
  OBJ_HFLIP			= (1 shl 12);
  OBJ_VFLIP			= (1 shl 13);


function ATTR1_ROTDATA(n: integer): integer; inline;

const
  ATTR1_FLIP_X          = (1 shl 12);
  ATTR1_FLIP_Y          = (1 shl 13);
  ATTR1_SIZE_8          = (0 shl 14);
  ATTR1_SIZE_16         = (1 shl 14);
  ATTR1_SIZE_32         = (2 shl 14);
  ATTR1_SIZE_64         = (3 shl 14);



function OBJ_SIZE(m: integer): integer; inline;
// Sprite Attribute 2
function OBJ_CHAR(m: integer): integer; inline;
function OBJ_PRIORITY(m: integer): integer; inline;
function OBJ_PALETTE(m: integer): integer; inline;

function ATTR2_PRIORITY(n: integer): integer; inline;
function ATTR2_PALETTE(n: integer): integer; inline;

{$define OBJ_TRANSLUCENT  := OBJ_MODE(1)}
{$define OBJ_OBJWINDOW    := OBJ_MODE(2)}
{$define OBJ_SQUARE       := OBJ_SHAPE(0)}
{$define OBJ_WIDE         := OBJ_SHAPE(1)}
{$define OBJ_TALL         := OBJ_SHAPE(2)}

//---------------------------------------------------------------------------------
type
  SPRITE_SIZECODE = integer;
const		
  Sprite_8x8  : SPRITE_SIZECODE = 0;
  Sprite_16x16: SPRITE_SIZECODE = 1;
  Sprite_32x32: SPRITE_SIZECODE = 2;
  Sprite_64x64: SPRITE_SIZECODE = 3;
  Sprite_16x8 : SPRITE_SIZECODE = 4;
  Sprite_32x8 : SPRITE_SIZECODE = 5;
  Sprite_32x16: SPRITE_SIZECODE = 6;
  Sprite_64x32: SPRITE_SIZECODE = 7;
  Sprite_8x16 : SPRITE_SIZECODE = 8;
  Sprite_8x32 : SPRITE_SIZECODE = 9;
  Sprite_16x32: SPRITE_SIZECODE = 10;
  Sprite_32x64: SPRITE_SIZECODE = 11;

{$endif GBA_INTERFACE}

{$ifdef GBA_IMPLEMENTATION}
function OBJ_SHAPE(m: integer): integer; inline;
begin		
  OBJ_SHAPE := ((m) shl 14);
end;

// Sprite Attribute 0
function OBJ_Y(m: integer): integer; inline;
begin
  OBJ_Y := ((m) and $00ff);
end;

function OBJ_MODE(m: integer): integer; inline;
begin
  OBJ_MODE := ((m) shl 10);
end;

// Sprite Attribute 1
function OBJ_X(m: integer): integer; inline;
begin
  OBJ_X := ((m) and $01ff);
end;

function OBJ_ROT_SCALE(m: integer): integer; inline;
begin
  OBJ_ROT_SCALE := ((m) shl 9);
end;

function ATTR1_ROTDATA(n: integer): integer; inline;
begin
  ATTR1_ROTDATA := ((n) shl 9);  // note: overlaps with flip flags
end;

function OBJ_SIZE(m: integer): integer; inline;
begin
  OBJ_SIZE := ((m) shl 14);
end;

// Sprite Attribute 2
function OBJ_CHAR(m: integer): integer; inline;
begin
  OBJ_CHAR := ((m) and $03ff);
end;

function OBJ_PRIORITY(m: integer): integer; inline;
begin
  OBJ_PRIORITY := ((m) shl 10);
end;

function OBJ_PALETTE(m: integer): integer; inline;
begin
  OBJ_PALETTE := ((m) shl 12);
end;

function ATTR2_PRIORITY(n: integer): integer; inline;
begin     
  ATTR2_PRIORITY := ((n) shl 10);
end;

function ATTR2_PALETTE(n: integer): integer; inline;
begin      
  ATTR2_PALETTE := ((n) shl 12);
end;

{$endif GBA_IMPLEMENTATION}
