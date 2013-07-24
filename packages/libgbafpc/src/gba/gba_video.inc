(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba video definitions

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

const
  BG_COLORS       : pu16 = pointer($05000000);	// Background color table
  BG_PALETTE      : pu16 = pointer($05000000);	// Background color table

  OBJ_COLORS      : pu16 = pointer($05000200);	// Sprite color table
  SPRITE_PALETTE  : pu16 = pointer($05000200);	// Sprite color table

  REG_DISPCNT     : pu16 = pointer(REG_BASE + $00);

type
  LCDC_BITS = integer;
const
	MODE_0 : LCDC_BITS =	0;	(* BG Mode 0 *)
	MODE_1 : LCDC_BITS =	1;	(* BG Mode 1 *)
	MODE_2 : LCDC_BITS =	2;	(* BG Mode 2 *)
	MODE_3 : LCDC_BITS =	3;	(* BG Mode 3 *)
	MODE_4 : LCDC_BITS =	4;	(* BG Mode 4 *)
	MODE_5 : LCDC_BITS =	5;	(* BG Mode 5 *)

	BACKBUFFER : LCDC_BITS =	(1 shl 4);		(* buffer display select			*)
	OBJ_1D_MAP : LCDC_BITS =	(1 shl 6);		(* sprite 1 dimensional mapping	*)
	LCDC_OFF : LCDC_BITS =	(1 shl 7);		(* LCDC OFF						*)
	BG0_ON : LCDC_BITS =	(1 shl 8);		(* enable background 0			*)
	BG1_ON : LCDC_BITS =	(1 shl 9);		(* enable background 1			*)
	BG2_ON : LCDC_BITS =	(1 shl 10);	(* enable background 2			*)
	BG3_ON : LCDC_BITS =	(1 shl 11);	(* enable background 3			*)
	OBJ_ON : LCDC_BITS =	(1 shl 12);	(* enable sprites					*)
	WIN0_ON : LCDC_BITS =	(1 shl 13);	(* enable window 0				*)
	WIN1_ON : LCDC_BITS =	(1 shl 14);	(* enable window 1				*)
	OBJ_WIN_ON : LCDC_BITS =	(1 shl 15);	(* enable obj window				*)

	BG0_ENABLE : LCDC_BITS =	(1 shl 8);		(* enable background 0	*)
	BG1_ENABLE : LCDC_BITS =	(1 shl 9); 	(* enable background 1	*)
	BG2_ENABLE : LCDC_BITS =	(1 shl 10); 	(* enable background 2	*)
	BG3_ENABLE : LCDC_BITS =	(1 shl 11);		(* enable background 3	*)
	OBJ_ENABLE : LCDC_BITS =	(1 shl 12); 	(* enable sprites			*)
	WIN0_ENABLE : LCDC_BITS =	(1 shl 13);	(* enable window 0		*)
	WIN1_ENABLE : LCDC_BITS =	(1 shl 14);	(* enable window 1		*)
	OBJ_WIN_ENABLE : LCDC_BITS =	(1 shl 15); 	(* enable obj window		*)

	BG_ALL_ON : LCDC_BITS =	(1 shl 8) or (1 shl 9) or (1 shl 10) or (1 shl 11); 	    (* All Backgrounds on.		*)
	BG_ALL_ENABLE : LCDC_BITS =	(1 shl 8) or (1 shl 9) or (1 shl 10) or (1 shl 11);	    (* All Backgrounds enabled.	*)

const
  REG_DISPSTAT	: pu16 = pointer(REG_BASE + $04);

//---------------------------------------------------------------------------------
// LCDC Interrupt bits
//---------------------------------------------------------------------------------

type
  LCDC_IRQ = integer;
const
	LCDC_VBL_FLAG  : LCDC_IRQ = (1 shl 0);
	LCDC_HBL_FLAG  : LCDC_IRQ = (1 shl 1);
	LCDC_VCNT_FLAG  : LCDC_IRQ = (1 shl 2);
	LCDC_VBL  : LCDC_IRQ = (1 shl 3);
	LCDC_HBL  : LCDC_IRQ = (1 shl 4);
	LCDC_VCNT  : LCDC_IRQ = (1 shl 5);


function VCOUNT(m: integer): u32; inline; 


const
  REG_VCOUNT	: pu16 = pointer(REG_BASE + $06);
  BGCTRL		  : pu16 = pointer(REG_BASE + $08);
  REG_BG0CNT  : pu16 = pointer(REG_BASE + $08);
  REG_BG1CNT  : pu16 = pointer(REG_BASE + $0a);
  REG_BG2CNT  : pu16 = pointer(REG_BASE + $0c);
  REG_BG3CNT  : pu16 = pointer(REG_BASE + $0e);

type
  bg_scroll = record
    x: u16;
    y: u16;
  end;
  Pbg_scroll = ^bg_scroll;

const
  BG_OFFSET : pbg_scroll = pointer(REG_BASE + $10);

  REG_BG0HOFS		: pu16 = pointer(REG_BASE + $10);	// BG 0 H Offset
  REG_BG0VOFS		: pu16 = pointer(REG_BASE + $12);	// BG 0 V Offset
  REG_BG1HOFS		: pu16 = pointer(REG_BASE + $14);	// BG 1 H Offset
  REG_BG1VOFS		: pu16 = pointer(REG_BASE + $16);	// BG 1 V Offset
  REG_BG2HOFS		: pu16 = pointer(REG_BASE + $18);	// BG 2 H Offset
  REG_BG2VOFS		: pu16 = pointer(REG_BASE + $1a);	// BG 2 V Offset
  REG_BG3HOFS		: pu16 = pointer(REG_BASE + $1c);	// BG 3 H Offset
  REG_BG3VOFS		: pu16 = pointer(REG_BASE + $1e);	// BG 3 V Offset

  REG_BG2PA	: ps16 = pointer(REG_BASE + $20);
  REG_BG2PB	: ps16 = pointer(REG_BASE + $22);
  REG_BG2PC	: ps16 = pointer(REG_BASE + $24);
  REG_BG2PD	: ps16 = pointer(REG_BASE + $26);
  REG_BG2X	: ps32 = pointer(REG_BASE + $28);
  REG_BG2Y	: ps32 = pointer(REG_BASE + $2c);
  REG_BG3PA	: ps16 = pointer(REG_BASE + $30);
  REG_BG3PB	: ps16 = pointer(REG_BASE + $32);
  REG_BG3PC	: ps16 = pointer(REG_BASE + $34);
  REG_BG3PD	: ps16 = pointer(REG_BASE + $36);
  REG_BG3X	: ps32 = pointer(REG_BASE + $38);
  REG_BG3Y	: ps32 = pointer(REG_BASE + $3c);

function BG_SIZE(m: integer): integer; inline;

type
  BG_CTRL_BITS = integer;
const
	BG_MOSAIC      : BG_CTRL_BITS =	(1 shl 6);		  (* enable background mosaic			*)
	BG_16_COLOR    : BG_CTRL_BITS =	(0 shl 7);		(* background uses 16 color tiles		*)
	BG_256_COLOR   : BG_CTRL_BITS =	(1 shl 7);		(* background uses 256 color tiles	*)
	BG_WRAP			: BG_CTRL_BITS =	  (1 shl 13);	  (* background wraps when scrolling	*)
	
  BG_SIZE_0		: BG_CTRL_BITS = (0 shl 14);	(* Map Size 256x256	*)
  BG_SIZE_1		: BG_CTRL_BITS = (1 shl 14);	(* Map Size 512x256	*)
  BG_SIZE_2		: BG_CTRL_BITS = (2 shl 14);	(* Map Size 256x512	*)
  BG_SIZE_3		: BG_CTRL_BITS = (3 shl 14);	(* Map Size 512x512	*)


function CHAR_BASE(m: integer): integer; inline;
function BG_TILE_BASE(m: integer): integer; inline;
function CHAR_BASE_ADR(m: integer): pointer; inline;
function CHAR_BASE_BLOCK(m: integer): pointer; inline;
function MAP_BASE_ADR(m: integer): pointer; inline;
function SCREEN_BASE_BLOCK(m: integer): pointer; inline;
function SCREEN_BASE(m: integer): integer; inline;
function BG_MAP_BASE(m: integer): integer; inline;

//alternate names for char and screen base
function TILE_BASE(m: integer): integer; inline;
function TILE_BASE_ADR(m: integer): pointer; inline;
function MAP_BASE(m: integer): integer; inline;
function BG_PRIORITY(m: integer): integer; inline;
function CHAR_PALETTE(m: integer): integer; inline;

(*---------------------------------------------------------------------------------
	CHAR_BASE_ADR() is the direct equivalent to old PATRAM(),
	giving the base address of a chr bank.
	These macros pinpoint the base address of a single tile.
---------------------------------------------------------------------------------*)
function PATRAM4(x, tn: integer): pu32; inline;
function PATRAM8(x, tn: integer): pu32; inline;
function SPR_VRAM(tn: integer): pu32; inline;

(*---------------------------------------------------------------------------------
	MAP_BASE_ADR() only gives the beginning of a map.
	Each cell of a text map can be accessed using 3D array notation:

	MAP[page][y][x]
---------------------------------------------------------------------------------*)
type
  NAMETABLE = array [0..31, 0..31] of u16;
  PNAMETABLE = ^NAMETABLE;
const
  MAP : PNAMETABLE = pointer($06000000);

(*---------------------------------------------------------------------------------
	width and height of a GBA text map can (and probably should)
	be controlled separately.
---------------------------------------------------------------------------------*)
const
  BG_WID_32 = 0 shl 14;
  BG_WID_64 = 1 shl 14;
  BG_HT_32  = 0 shl 14;
  BG_HT_64  = 2 shl 14;
//---------------------------------------------------------------------------------
// Symbolic names for the rot/scale map sizes
//---------------------------------------------------------------------------------
  
  ROTBG_SIZE_16  = 0 shl 14;
  ROTBG_SIZE_32  = 1 shl 14;
  ROTBG_SIZE_64  = 2 shl 14;
  ROTBG_SIZE_128 = 3 shl 14;

  TEXTBG_SIZE_256x256    = 0 shl 14;
  TEXTBG_SIZE_512x256    = 1 shl 14;
  TEXTBG_SIZE_256x512    = 2 shl 14;
  TEXTBG_SIZE_512x512    = 3 shl 14;

  ROTBG_SIZE_128x128    = 0 shl 14;
  ROTBG_SIZE_256x256    = 1 shl 14;
  ROTBG_SIZE_512x512    = 2 shl 14;
  ROTBG_SIZE_1024x1024  = 3 shl 14;

//---------------------------------------------------------------------------------
// Framebuffers for mode 3 and 5
//---------------------------------------------------------------------------------
type
  MODE3_LINE = array [0..239] of u16;
  PMODE3_LINE = ^MODE3_LINE;
  MODE5_LINE = array [0..159] of u16;
  PMODE5_LINE = ^MODE5_LINE;
  
const
  MODE3_FB : PMODE3_LINE = pointer($06000000);
  MODE5_FB : PMODE5_LINE = pointer($06000000);
  MODE5_BB : PMODE5_LINE = pointer($0600A000);

  REG_WIN0H	  : pu16 = pointer(REG_BASE + $40);
  REG_WIN1H	  : pu16 = pointer(REG_BASE + $42);
  REG_WIN0V	  : pu16 = pointer(REG_BASE + $44);
  REG_WIN1V	  : pu16 = pointer(REG_BASE + $46);
  REG_WININ	  : pu16 = pointer(REG_BASE + $48);
  REG_WINOUT	: pu16 = pointer(REG_BASE + $4A);

  REG_MOSAIC	: pu16 = pointer(REG_BASE + $4c);

  REG_BLDCNT		: pu16 = pointer(REG_BASE + $50);
  REG_BLDALPHA	: pu16 = pointer(REG_BASE + $52);
  REG_BLDY		  : pu16 = pointer(REG_BASE + $54);

//---------------------------------------------------------------------------------
// Helper macros
//---------------------------------------------------------------------------------
procedure SetMode(mode: integer); inline;
function RGB5(const r, g, b: word): word;inline;
function RGB8(const r, g, b: word): word;inline;

const
  SCREEN_WIDTH = 240;
  SCREEN_HEIGHT = 160;

{$endif GBA_INTERFACE}



{$ifdef GBA_IMPLEMENTATION}
function VCOUNT(m: integer): u32; inline; 
begin
  VCOUNT := m shl 8; 
end;

function BG_SIZE(m: integer): integer; inline;
begin
  BG_SIZE := ((m shl 14));
end;

function CHAR_BASE(m: integer): integer; inline;
begin
  CHAR_BASE := ((m) shl 2);
end;

function BG_TILE_BASE(m: integer): integer; inline;
begin		
  BG_TILE_BASE := ((m) shl 2);
end;

function CHAR_BASE_ADR(m: integer): pointer; inline;
begin
  CHAR_BASE_ADR := pointer(VRAM + ((m) shl 14));
end;

function CHAR_BASE_BLOCK(m: integer): pointer; inline;
begin
  CHAR_BASE_BLOCK := pointer(VRAM + ((m) shl 14));
end;

function MAP_BASE_ADR(m: integer): pointer; inline;
begin		
  MAP_BASE_ADR := pointer(VRAM + ((m) shl 11));
end;

function SCREEN_BASE_BLOCK(m: integer): pointer; inline;
begin		
  SCREEN_BASE_BLOCK := pointer(VRAM + ((m) shl 11));
end;

function SCREEN_BASE(m: integer): integer; inline;
begin
  SCREEN_BASE := ((m) shl 8);
end;

function BG_MAP_BASE(m: integer): integer; inline;
begin
  BG_MAP_BASE := ((m) shl 8);
end;

//alternate names for char and screen base
function TILE_BASE(m: integer): integer; inline;
begin
  TILE_BASE := ((m) shl 2);
end;

function TILE_BASE_ADR(m: integer): pointer; inline;
begin
  TILE_BASE_ADR := pointer(VRAM + ((m) shl 14));
end;

function MAP_BASE(m: integer): integer; inline;
begin
  MAP_BASE := ((m) shl 8);
end;

function BG_PRIORITY(m: integer): integer; inline;
begin
  BG_PRIORITY := (m);
end;

function CHAR_PALETTE(m: integer): integer; inline;
begin
  CHAR_PALETTE := ((m) shl 12);
end;

(*---------------------------------------------------------------------------------
	CHAR_BASE_ADR() is the direct equivalent to old PATRAM(),
	giving the base address of a chr bank.
	These macros pinpoint the base address of a single tile.
---------------------------------------------------------------------------------*)
function PATRAM4(x, tn: integer): pu32; inline;
begin
  PATRAM4 := (pu32($6000000 or (((x) shl 14) + ((tn) shl 5)) ));
end;

function PATRAM8(x, tn: integer): pu32; inline;
begin
  PATRAM8 := (pu32($6000000 or (((x) shl 14) + ((tn) shl 6)) ));
end;

function SPR_VRAM(tn: integer): pu32; inline;
begin
  SPR_VRAM := (pu32(VRAM or $10000 or ((tn) shl 5)));
end;

procedure SetMode(mode: integer); inline;
begin	
  REG_DISPCNT^ := mode
end;

function RGB5(const r, g, b: word): word;inline;
begin
  RGB5 := ((r) or ((g) shl 5) or ((b) shl 10));
end;

function RGB8(const r, g, b: word): word;inline;
begin
  RGB8 := ( (((b) shr 3) shl 10) or (((g) shr 3) shl 5) or ((r) shr 3) );
end;

{$endif GBA_IMPLEMENTATION}

