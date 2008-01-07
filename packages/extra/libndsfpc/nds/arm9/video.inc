(*
  $Id: video.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)

{$ifndef ARM9}
  {$error Video is only available on the ARM9}
{$endif ARM9}

{$ifdef NDS_IMPLEMENTATION}
function RGB15(r, g, b: cint): cuint16; 
begin
  RGB15 := ((r) or ((g) shl 5) or ((b) shl 10));
end;
  
function RGB5(r, g, b: cint): cuint16;
begin
  RGB5 := ((r) or ((g) shl 5) or ((b) shl 10));
end;

function RGB8(r, g, b: cint): cuint8;
begin
  RGB8 := (((r) shr 3) or (((g) shr 3) shl 5) or (((b) shr 3) shl 10));
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  SCREEN_HEIGHT = 192;
  SCREEN_WIDTH  = 256;
  
//	Vram Control
  VRAM_CR			: pcuint32 = pointer($04000240);
  VRAM_A_CR		: pcuint8 = pointer($04000240);
  VRAM_B_CR		: pcuint8 = pointer($04000241);
  VRAM_C_CR		: pcuint8 = pointer($04000242);
  VRAM_D_CR		: pcuint8 = pointer($04000243);
  VRAM_E_CR		: pcuint8 = pointer($04000244);
  VRAM_F_CR		: pcuint8 = pointer($04000245);
  VRAM_G_CR		: pcuint8 = pointer($04000246);
  WRAM_CR			: pcuint8 = pointer($04000247);
  VRAM_H_CR		: pcuint8 = pointer($04000248);
  VRAM_I_CR		: pcuint8 = pointer($04000249);

  VRAM_ENABLE	= (1 shl 7);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function  VRAM_OFFSET(n: cint): cint; inline;	
begin
  VRAM_OFFSET := ((n) shl 3);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
type
  VRAM_A_TYPE = cint;
const 
  VRAM_A_LCD	              : VRAM_A_TYPE = 0;
  VRAM_A_MAIN_BG            : VRAM_A_TYPE = 1;
  VRAM_A_MAIN_BG_0x06000000  : VRAM_A_TYPE = 1 or (0 shl 3);
  VRAM_A_MAIN_BG_0x06020000  : VRAM_A_TYPE = 1 or (1 shl 3);
  VRAM_A_MAIN_BG_0x06040000  : VRAM_A_TYPE = 1 or (2 shl 3);
  VRAM_A_MAIN_BG_0x06060000  : VRAM_A_TYPE = 1 or (3 shl 3);
  VRAM_A_MAIN_SPRITE        : VRAM_A_TYPE = 2;
  VRAM_A_MAIN_SPRITE_0x06400000  : VRAM_A_TYPE = 2;
  VRAM_A_MAIN_SPRITE_0x06420000  : VRAM_A_TYPE = 2 or (1 shl 3);  
  VRAM_A_TEXTURE            : VRAM_A_TYPE = 3;
  VRAM_A_TEXTURE_SLOT0      : VRAM_A_TYPE = 3 or (0 shl 3);
  VRAM_A_TEXTURE_SLOT1      : VRAM_A_TYPE = 3 or (1 shl 3);
  VRAM_A_TEXTURE_SLOT2      : VRAM_A_TYPE = 3 or (2 shl 3);
  VRAM_A_TEXTURE_SLOT3      : VRAM_A_TYPE = 3 or (3 shl 3);

type
  VRAM_B_TYPE = cint;
const
  VRAM_B_LCD                : VRAM_B_TYPE = 0;
  VRAM_B_MAIN_BG            : VRAM_B_TYPE = 1 or (1 shl 3);
  VRAM_B_MAIN_BG_0x06000000  : VRAM_B_TYPE = 1 or (0 shl 3);
  VRAM_B_MAIN_BG_0x06020000  : VRAM_B_TYPE = 1 or (1 shl 3);
  VRAM_B_MAIN_BG_0x06040000  : VRAM_B_TYPE = 1 or (2 shl 3);
  VRAM_B_MAIN_BG_0x06060000  : VRAM_B_TYPE = 1 or (3 shl 3);
  VRAM_B_MAIN_SPRITE        : VRAM_B_TYPE = 2 or (1 shl 3);
  VRAM_B_MAIN_SPRITE_0x06400000  : VRAM_B_TYPE = 2;
  VRAM_B_MAIN_SPRITE_0x06420000  : VRAM_B_TYPE = 2 or (1 shl 3);
  VRAM_B_TEXTURE            : VRAM_B_TYPE = 3 or (1 shl 3);
  VRAM_B_TEXTURE_SLOT0      : VRAM_B_TYPE = 3 or (0 shl 3);
  VRAM_B_TEXTURE_SLOT1      : VRAM_B_TYPE = 3 or (1 shl 3);
  VRAM_B_TEXTURE_SLOT2      : VRAM_B_TYPE = 3 or (2 shl 3);
  VRAM_B_TEXTURE_SLOT3      : VRAM_B_TYPE = 3 or (3 shl 3);


type
  VRAM_C_TYPE = cint;
const
  VRAM_C_LCD                : VRAM_C_TYPE = 0;
  VRAM_C_MAIN_BG            : VRAM_C_TYPE = 1 or (2 shl 3);
  VRAM_C_MAIN_BG_0x06000000  : VRAM_C_TYPE = 1 or (0 shl 3);
  VRAM_C_MAIN_BG_0x06020000  : VRAM_C_TYPE = 1 or (1 shl 3);
  VRAM_C_MAIN_BG_0x06040000  : VRAM_C_TYPE = 1 or (2 shl 3);
  VRAM_C_MAIN_BG_0x06060000  : VRAM_C_TYPE = 1 or (3 shl 3);
  VRAM_C_ARM7               : VRAM_C_TYPE = 2;
  VRAM_C_ARM7_0x06000000     : VRAM_C_TYPE = 2;
  VRAM_C_ARM7_0x06020000     : VRAM_C_TYPE = 2 or (1 shl 3);  
  VRAM_C_SUB_BG             : VRAM_C_TYPE = 4;
  VRAM_C_SUB_BG_0x06200000   : VRAM_C_TYPE = 4 or (0 shl 3);
  VRAM_C_SUB_BG_0x06220000   : VRAM_C_TYPE = 4 or (1 shl 3);
  VRAM_C_SUB_BG_0x06240000   : VRAM_C_TYPE = 4 or (2 shl 3);
  VRAM_C_SUB_BG_0x06260000   : VRAM_C_TYPE = 4 or (3 shl 3);
  VRAM_C_TEXTURE            : VRAM_C_TYPE = 3 or (2 shl 3);
  VRAM_C_TEXTURE_SLOT0      : VRAM_C_TYPE = 3 or (0 shl 3);
  VRAM_C_TEXTURE_SLOT1      : VRAM_C_TYPE = 3 or (1 shl 3);
  VRAM_C_TEXTURE_SLOT2      : VRAM_C_TYPE = 3 or (2 shl 3);
  VRAM_C_TEXTURE_SLOT3      : VRAM_C_TYPE = 3 or (3 shl 3);


type
  VRAM_D_TYPE = cint;
const
  VRAM_D_LCD                : VRAM_D_TYPE = 0;
  VRAM_D_MAIN_BG            : VRAM_D_TYPE = 1 or (3 shl 3);
  VRAM_D_MAIN_BG_0x06000000  : VRAM_D_TYPE = 1 or (0 shl 3);
  VRAM_D_MAIN_BG_0x06020000  : VRAM_D_TYPE = 1 or (1 shl 3);
  VRAM_D_MAIN_BG_0x06040000  : VRAM_D_TYPE = 1 or (2 shl 3);
  VRAM_D_MAIN_BG_0x06060000  : VRAM_D_TYPE = 1 or (3 shl 3);
  VRAM_D_ARM7               : VRAM_D_TYPE = 2 or (1 shl 3);
  VRAM_D_ARM7_0x06000000     : VRAM_D_TYPE = 2;
  VRAM_D_ARM7_0x06020000     : VRAM_D_TYPE = 2 or (1 shl 3);  
  VRAM_D_SUB_SPRITE         : VRAM_D_TYPE = 4;
  VRAM_D_TEXTURE            : VRAM_D_TYPE = 3 or (3 shl 3);
  VRAM_D_TEXTURE_SLOT0      : VRAM_D_TYPE = 3 or (0 shl 3);
  VRAM_D_TEXTURE_SLOT1      : VRAM_D_TYPE = 3 or (1 shl 3);
  VRAM_D_TEXTURE_SLOT2      : VRAM_D_TYPE = 3 or (2 shl 3);
  VRAM_D_TEXTURE_SLOT3      : VRAM_D_TYPE = 3 or (3 shl 3);

type
  VRAM_E_TYPE = cint;
const
  VRAM_E_LCD                : VRAM_E_TYPE = 0;
  VRAM_E_MAIN_BG            : VRAM_E_TYPE = 1;
  VRAM_E_MAIN_SPRITE        : VRAM_E_TYPE = 2;
  VRAM_E_TEX_PALETTE        : VRAM_E_TYPE = 3;
  VRAM_E_BG_EXT_PALETTE     : VRAM_E_TYPE = 4;
  VRAM_E_OBJ_EXT_PALETTE    : VRAM_E_TYPE = 5;

type
  VRAM_F_TYPE = cint;
const
  VRAM_F_LCD                : VRAM_F_TYPE = 0;
  VRAM_F_MAIN_BG            : VRAM_F_TYPE = 1;
  VRAM_F_MAIN_SPRITE        : VRAM_F_TYPE = 2;
  VRAM_F_MAIN_SPRITE_0x06000000  : VRAM_F_TYPE = 2;
  VRAM_F_MAIN_SPRITE_0x06004000  : VRAM_F_TYPE = 2 or (1 shl 3);
  VRAM_F_MAIN_SPRITE_0x06010000  : VRAM_F_TYPE = 2 or (2 shl 3);
  VRAM_F_MAIN_SPRITE_0x06014000  : VRAM_F_TYPE = 2 or (3 shl 3); 
  VRAM_F_TEX_PALETTE        : VRAM_F_TYPE = 3;
  VRAM_F_BG_EXT_PALETTE     : VRAM_F_TYPE = 4;
  VRAM_F_OBJ_EXT_PALETTE    : VRAM_F_TYPE = 5;

type
  VRAM_G_TYPE = cint;
const
  VRAM_G_LCD                : VRAM_G_TYPE = 0;
  VRAM_G_MAIN_BG            : VRAM_G_TYPE = 1;
  VRAM_G_MAIN_SPRITE        : VRAM_G_TYPE = 2;
  VRAM_G_MAIN_SPRITE_0x06000000  : VRAM_G_TYPE = 2;
  VRAM_G_MAIN_SPRITE_0x06004000  : VRAM_G_TYPE = 2 or (1 shl 3);
  VRAM_G_MAIN_SPRITE_0x06010000  : VRAM_G_TYPE = 2 or (2 shl 3);
  VRAM_G_MAIN_SPRITE_0x06014000  : VRAM_G_TYPE = 2 or (3 shl 3);    
  VRAM_G_TEX_PALETTE        : VRAM_G_TYPE = 3;
  VRAM_G_BG_EXT_PALETTE     : VRAM_G_TYPE = 4;
  VRAM_G_OBJ_EXT_PALETTE    : VRAM_G_TYPE = 5;

type
  VRAM_H_TYPE = cint;
const
  VRAM_H_LCD                : VRAM_H_TYPE = 0;
  VRAM_H_SUB_BG             : VRAM_H_TYPE = 1;
  VRAM_H_SUB_BG_EXT_PALETTE : VRAM_H_TYPE = 2;

type
  VRAM_I_TYPE = cint;
const
  VRAM_I_LCD                    : VRAM_I_TYPE = 0;
  VRAM_I_SUB_BG                 : VRAM_I_TYPE = 1;
  VRAM_I_SUB_SPRITE             : VRAM_I_TYPE = 2;
  VRAM_I_SUB_SPRITE_EXT_PALETTE : VRAM_I_TYPE = 3;

type
  TPalette = array [0..255] of cuint16;
  TExtPalette = array [0..15] of TPalette;
  PPalette = ^TPalette;
  PExtPalette = ^TExtPalette;

var
  VRAM_E_EXT_PALETTE : PExtPalette absolute VRAM_E;
  VRAM_F_EXT_PALETTE : PExtPalette absolute VRAM_F;
  VRAM_G_EXT_PALETTE : PExtPalette absolute VRAM_G;
  VRAM_H_EXT_PALETTE : PExtPalette absolute VRAM_H;

function vramSetMainBanks(a: VRAM_A_TYPE; b: VRAM_B_TYPE; c: VRAM_C_TYPE; d: VRAM_D_TYPE): cuint32; cdecl; external;
procedure vramRestoreMainBanks(vramTemp: cuint32); cdecl; external;

procedure vramSetBankA(a: VRAM_A_TYPE); cdecl; external;
procedure vramSetBankB(b: VRAM_B_TYPE); cdecl; external;
procedure vramSetBankC(c: VRAM_C_TYPE); cdecl; external;
procedure vramSetBankD(d: VRAM_D_TYPE); cdecl; external;
procedure vramSetBankE(e: VRAM_E_TYPE); cdecl; external;
procedure vramSetBankF(f: VRAM_F_TYPE); cdecl; external;
procedure vramSetBankG(g: VRAM_G_TYPE); cdecl; external;
procedure vramSetBankH(h: VRAM_H_TYPE); cdecl; external;
procedure vramSetBankI(i: VRAM_I_TYPE); cdecl; external;


// Display control registers
const
  DISPLAY_CR       : pcuint32 = pointer($04000000);
  SUB_DISPLAY_CR   : pcuint32 = pointer($04001000);

  MODE_0_2D      = $10000;
  MODE_1_2D      = $10001;
  MODE_2_2D      = $10002;
  MODE_3_2D      = $10003;
  MODE_4_2D      = $10004;
  MODE_5_2D      = $10005;

// main display only
  MODE_6_2D      = $10006;
  MODE_FIFO      = (3 shl 16);

  ENABLE_3D      = (1 shl 3);

  DISPLAY_BG0_ACTIVE    = (1 shl 8);
  DISPLAY_BG1_ACTIVE    = (1 shl 9);
  DISPLAY_BG2_ACTIVE    = (1 shl 10);
  DISPLAY_BG3_ACTIVE    = (1 shl 11);
  DISPLAY_SPR_ACTIVE    = (1 shl 12);
  DISPLAY_WIN0_ON       = (1 shl 13);
  DISPLAY_WIN1_ON       = (1 shl 14);
  DISPLAY_SPR_WIN_ON    = (1 shl 15);


// Main display only
  MODE_0_3D   = (MODE_0_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);
  MODE_1_3D   = (MODE_1_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);
  MODE_2_3D   = (MODE_2_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);
  MODE_3_3D   = (MODE_3_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);
  MODE_4_3D   = (MODE_4_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);
  MODE_5_3D   = (MODE_5_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);
  MODE_6_3D   = (MODE_6_2D or DISPLAY_BG0_ACTIVE or ENABLE_3D);

  MODE_FB0    = ($00020000);
  MODE_FB1    = ($00060000);
  MODE_FB2	  = ($000A0000);
  MODE_FB3	  = ($000E0000);

  DISPLAY_SPR_HBLANK	  = (1 shl 23);

  DISPLAY_SPR_1D_LAYOUT	= (1 shl 4);

  DISPLAY_SPR_1D				  = (1 shl 4);
  DISPLAY_SPR_2D				  = (0 shl 4);
  DISPLAY_SPR_1D_BMP			= (4 shl 4);
  DISPLAY_SPR_2D_BMP_128	= (0 shl 4);
  DISPLAY_SPR_2D_BMP_256	= (2 shl 4);


  DISPLAY_SPR_1D_SIZE_32		  = (0 shl 20);
  DISPLAY_SPR_1D_SIZE_64		  = (1 shl 20);
  DISPLAY_SPR_1D_SIZE_128		  = (2 shl 20);
  DISPLAY_SPR_1D_SIZE_256		  = (3 shl 20);
  DISPLAY_SPR_1D_BMP_SIZE_128	= (0 shl 22);
  DISPLAY_SPR_1D_BMP_SIZE_256	= (1 shl 22);


  DISPLAY_SPR_EXT_PALETTE		  = (1 shl 31);
  DISPLAY_BG_EXT_PALETTE		  = (1 shl 30);

  DISPLAY_SCREEN_OFF     = (1 shl 7);
{$endif NDS_INTERFACE}
  
  
{$ifdef NDS_IMPLEMENTATION}
function DISPLAY_CHAR_BASE(n: cuint32): cuint32; inline;
begin
  DISPLAY_CHAR_BASE := ((n) and 7) shl 24;
end;

function DISPLAY_SCREEN_BASE(n: cuint32): cuint32; inline;
begin 
  DISPLAY_SCREEN_BASE := ((n) and 7) shl 27;
end;	 
	 
procedure videoSetMode(mode: cuint32); inline;  
begin
  DISPLAY_CR^ := mode;
end;

procedure videoSetModeSub(mode: cuint32); inline;  
begin
  SUB_DISPLAY_CR^ := mode;
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  BRIGHTNESS     : pcuint16 = pointer($0400006C);
  SUB_BRIGHTNESS : pcuint16 = pointer($0400106C);

  BGCTRL			: pcuint16 = pointer($04000008);
  BG0_CR		  : pcuint16 = pointer($04000008);
  BG1_CR		  : pcuint16 = pointer($0400000A);
  BG2_CR		  : pcuint16 = pointer($0400000C);
  BG3_CR		  : pcuint16 = pointer($0400000E);

  BGCTRL_SUB		: pcuint16 = pointer($04001008);
  SUB_BG0_CR    : pcuint16 = pointer($04001008);
  SUB_BG1_CR    : pcuint16 = pointer($0400100A);
  SUB_BG2_CR    : pcuint16 = pointer($0400100C);
  SUB_BG3_CR    : pcuint16 = pointer($0400100E);

  BG_256_COLOR   = (1 shl 7);
  BG_16_COLOR    = (0);

  BG_MOSAIC_ON   = (1 shl 6);
  BG_MOSAIC_OFF  = (0);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function BG_PRIORITY(n: cint): cint; inline;
begin
  BG_PRIORITY := (n);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  BG_PRIORITY_0  = (0);
  BG_PRIORITY_1  = (1);
  BG_PRIORITY_2  = (2);
  BG_PRIORITY_3  = (3);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function BG_TILE_BASE(base: cint): cint; inline; 
begin 
  BG_TILE_BASE := ((base) shl 2);
end;

function BG_MAP_BASE(base: cint): cint; inline;  
begin
  BG_MAP_BASE := ((base) shl 8);
end;

function BG_BMP_BASE(base: cint): cint; inline;  
begin
  BG_BMP_BASE := ((base) shl 8);
end;


function BG_MAP_RAM(base: cint): cint; inline;  
begin
  BG_MAP_RAM := (((base) * $800) + $06000000);
end;

function BG_MAP_RAM_SUB(base: cint): cint; inline; 
begin
  BG_MAP_RAM_SUB := (((base) * $800) + $06200000);
end;


function BG_TILE_RAM(base: cint): cint; inline; 
begin
  BG_TILE_RAM := (((base) * $4000) + $06000000);
end;

function BG_TILE_RAM_SUB(base: cint): cint; inline; 
begin
  BG_TILE_RAM_SUB := (((base) * $4000) + $06200000);
end;


function BG_BMP_RAM(base: cint): cint; inline;  
begin
  BG_BMP_RAM := (((base) * $4000) + $06000000);
end;

function BG_BMP_RAM_SUB(base: cint): cint; inline; 
begin
  BG_BMP_RAM_SUB := (((base) * $4000) + $06200000);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  BG_WRAP_OFF    = (0);
  BG_WRAP_ON     = (1 shl 13);

  BG_32x32       = (0 shl 14);
  BG_64x32       = (1 shl 14);
  BG_32x64       = (2 shl 14);
  BG_64x64       = (3 shl 14);

  BG_RS_16x16    = (0 shl 14);
  BG_RS_32x32    = (1 shl 14);
  BG_RS_64x64    = (2 shl 14);
  BG_RS_128x128  = (3 shl 14);

  BG_BMP8_128x128 = (BG_RS_16x16 or BG_256_COLOR);
  BG_BMP8_256x256 = (BG_RS_32x32 or BG_256_COLOR);
  BG_BMP8_512x256 = (BG_RS_64x64 or BG_256_COLOR);
  BG_BMP8_512x512 = (BG_RS_128x128 or BG_256_COLOR);
  BG_BMP8_1024x512 = (1 shl 14);
  BG_BMP8_512x1024 = 0;

  BG_BMP16_128x128 = (BG_RS_16x16 or BG_256_COLOR or (1 shl 2));
  BG_BMP16_256x256 = (BG_RS_32x32 or BG_256_COLOR or (1 shl 2));
  BG_BMP16_512x256 = (BG_RS_64x64 or BG_256_COLOR or (1 shl 2));
  BG_BMP16_512x512 = (BG_RS_128x128 or BG_256_COLOR or (1 shl 2));

  BG_PALETTE_SLOT0 = 0;
  BG_PALETTE_SLOT1 = 0;
  BG_PALETTE_SLOT2 = (1 shl 13);
  BG_PALETTE_SLOT3 = (1 shl 13);

type
  bg_scroll = record
    x: cuint16;
    y: cuint16;
  end;
  Tbg_scroll = bg_scroll;
  Pbg_scroll = ^bg_scroll;

  bg_rotation = record
    xdx: cuint16;
    xdy: cuint16;
    ydx: cuint16;
    ydy: cuint16;
    centerX: cuint32;
    centerY: cuint32;
  end;
  Tbg_rotation = bg_rotation;
  Pbg_rotation = ^bg_rotation;

  bg_attribute = record
    control: array [0..3] of cuint16;
    scroll: array [0..3] of bg_scroll;
    bg2_rotation: bg_rotation;
    bg3_rotation: bg_rotation;
  end;
  Tbg_attribute = bg_attribute;
  Pbg_attribute = ^bg_attribute;


const
	BACKGROUND     : Pbg_attribute = pointer($04000008);
	BACKGROUND_SUB : Pbg_attribute = pointer($04001008);

  BG_OFFSET      : Pbg_scroll = pointer($04000010);

  BG0_X0         : pcuint16 = pointer($04000010);
  BG0_Y0         : pcuint16 = pointer($04000012);
  BG1_X0         : pcuint16 = pointer($04000014);
  BG1_Y0         : pcuint16 = pointer($04000016);
  BG2_X0         : pcuint16 = pointer($04000018);
  BG2_Y0         : pcuint16 = pointer($0400001A);
  BG3_X0         : pcuint16 = pointer($0400001C);
  BG3_Y0         : pcuint16 = pointer($0400001E);

  BG2_XDX        : pcuint16 = pointer($04000020);
  BG2_XDY        : pcuint16 = pointer($04000022);
  BG2_YDX        : pcuint16 = pointer($04000024);
  BG2_YDY        : pcuint16 = pointer($04000026);
  BG2_CX         : pcuint32 = pointer($04000028);
  BG2_CY         : pcuint32 = pointer($0400002C);

  BG3_XDX        : pcuint16 = pointer($04000030);
  BG3_XDY        : pcuint16 = pointer($04000032);
  BG3_YDX        : pcuint16 = pointer($04000034);
  BG3_YDY        : pcuint16 = pointer($04000036);
  BG3_CX         : pcuint32 = pointer($04000038);
  BG3_CY         : pcuint32 = pointer($0400003C);

  SUB_BG0_X0     : pcuint16 = pointer($04001010);
  SUB_BG0_Y0     : pcuint16 = pointer($04001012);
  SUB_BG1_X0     : pcuint16 = pointer($04001014);
  SUB_BG1_Y0     : pcuint16 = pointer($04001016);
  SUB_BG2_X0     : pcuint16 = pointer($04001018);
  SUB_BG2_Y0     : pcuint16 = pointer($0400101A);
  SUB_BG3_X0     : pcuint16 = pointer($0400101C);
  SUB_BG3_Y0     : pcuint16 = pointer($0400101E);

  SUB_BG2_XDX    : pcuint16 = pointer($04001020);
  SUB_BG2_XDY    : pcuint16 = pointer($04001022);
  SUB_BG2_YDX    : pcuint16 = pointer($04001024);
  SUB_BG2_YDY    : pcuint16 = pointer($04001026);
  SUB_BG2_CX     : pcuint32 = pointer($04001028);
  SUB_BG2_CY     : pcuint32 = pointer($0400102C);

  SUB_BG3_XDX    : pcuint16 = pointer($04001030);
  SUB_BG3_XDY    : pcuint16 = pointer($04001032);
  SUB_BG3_YDX    : pcuint16 = pointer($04001034);
  SUB_BG3_YDY    : pcuint16 = pointer($04001036);
  SUB_BG3_CX     : pcuint32 = pointer($04001038);
  SUB_BG3_CY     : pcuint32 = pointer($0400103C);

// Window 0
  WIN0_X0        : pcuint8 = pointer($04000041);
  WIN0_X1        : pcuint8 = pointer($04000040);
  WIN0_Y0        : pcuint8 = pointer($04000045);
  WIN0_Y1        : pcuint8 = pointer($04000044);

// Window 1
  WIN1_X0        : pcuint8 = pointer($04000042);
  WIN1_X1        : pcuint8 = pointer($04000043);
  WIN1_Y0        : pcuint8 = pointer($04000047);
  WIN1_Y1        : pcuint8 = pointer($04000046);

  WIN_IN         : pcuint16 = pointer($04000048);
  WIN_OUT        : pcuint16 = pointer($0400004A);

// Window 0
  SUB_WIN0_X0    : pcuint8 = pointer($04001041);
  SUB_WIN0_X1    : pcuint8 = pointer($04001040);
  SUB_WIN0_Y0    : pcuint8 = pointer($04001045);
  SUB_WIN0_Y1    : pcuint8 = pointer($04001044);

// Window 1
  SUB_WIN1_X0    : pcuint8 = pointer($04001042);
  SUB_WIN1_X1    : pcuint8 = pointer($04001043);
  SUB_WIN1_Y0    : pcuint8 = pointer($04001047);
  SUB_WIN1_Y1    : pcuint8 = pointer($04001046);

  SUB_WIN_IN     : pcuint16 = pointer($04001048);
  SUB_WIN_OUT    : pcuint16 = pointer($0400104A);

  MOSAIC_CR      : pcuint16 = pointer($0400004C);
  SUB_MOSAIC_CR  : pcuint16 = pointer($0400104C);

  BLEND_CR       : pcuint16 = pointer($04000050);
  BLEND_AB       : pcuint16 = pointer($04000052);
  BLEND_Y        : pcuint16 = pointer($04000054);

  SUB_BLEND_CR   : pcuint16 = pointer($04001050);
  SUB_BLEND_AB   : pcuint16 = pointer($04001052);
  SUB_BLEND_Y    : pcuint16 = pointer($04001054);

  BLEND_NONE          = (0 shl 6);
  BLEND_ALPHA         = (1 shl 6);
  BLEND_FADE_WHITE    = (2 shl 6);
  BLEND_FADE_BLACK    = (3 shl 6);

  BLEND_SRC_BG0       = (1 shl 0);
  BLEND_SRC_BG1       = (1 shl 1);
  BLEND_SRC_BG2       = (1 shl 2);
  BLEND_SRC_BG3       = (1 shl 3);
  BLEND_SRC_SPRITE    = (1 shl 4);
  BLEND_SRC_BACKDROP  = (1 shl 5);

  BLEND_DST_BG0       = (1 shl 8);
  BLEND_DST_BG1       = (1 shl 9);
  BLEND_DST_BG2       = (1 shl 10);
  BLEND_DST_BG3       = (1 shl 11);
  BLEND_DST_SPRITE    = (1 shl 12);
  BLEND_DST_BACKDROP  = (1 shl 13);

// Display capture control

  REG_DISPCAPCNT: pcuint32 = pointer($04000064);
  REG_DISP_MMEM_FIFO: pcuint32 = pointer($04000068);

  DCAP_ENABLE    = (1 shl 31);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function DCAP_MODE(n: cint): cint; inline;   
begin
  DCAP_MODE := (((n) and 3) shl 29);
end;

function DCAP_DST(n: cint): cint; inline;    
begin
  DCAP_DST := (((n) and 3) shl 26);
end;

function DCAP_SRC(n: cint): cint; inline;    
begin
  DCAP_SRC := (((n) and 3) shl 24);
end;

function DCAP_SIZE(n: cint): cint; inline;   
begin
  DCAP_SIZE := (((n) and 3) shl 20);
end;

function DCAP_OFFSET(n: cint): cint; inline; 
begin
  DCAP_OFFSET := (((n) and 3) shl 18);
end;

function DCAP_BANK(n: cint): cint; inline;   
begin
  DCAP_BANK := (((n) and 3) shl 16);
end;

function DCAP_B(n: cint): cint; inline;      
begin
  DCAP_B := (((n) and $1F) shl 8);
end;

function DCAP_A(n: cint): cint; inline;      
begin
  DCAP_A := (((n) and $1F) shl 0);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
// 3D core control
const
  GFX_CONTROL           : pcuint16 = pointer($04000060);

  GFX_FIFO              : pcuint32 = pointer($04000400);
  GFX_STATUS            : pcuint32 = pointer($04000600);
  GFX_COLOR             : pcuint32 = pointer($04000480);

  GFX_VERTEX10          : pcuint32 = pointer($04000490);
  GFX_VERTEX_XY         : pcuint32 = pointer($04000494);
  GFX_VERTEX_XZ         : pcuint32 = pointer($04000498);
  GFX_VERTEX_YZ         : pcuint32 = pointer($0400049C);
  GFX_VERTEX_DIFF       : pcuint32 = pointer($040004A0);

  GFX_VERTEX16          : pcuint32 = pointer($0400048C);
  GFX_TEX_COORD         : pcuint32 = pointer($04000488);
  GFX_TEX_FORMAT        : pcuint32 = pointer($040004A8);
  GFX_PAL_FORMAT        : pcuint32 = pointer($040004AC);

  GFX_CLEAR_COLOR       : pcuint32 = pointer($04000350);
  GFX_CLEAR_DEPTH       : pcuint16 = pointer($04000354);

  GFX_LIGHT_VECTOR      : pcuint32 = pointer($040004C8);
  GFX_LIGHT_COLOR       : pcuint32 = pointer($040004CC);
  GFX_NORMAL            : pcuint32 = pointer($04000484);

  GFX_DIFFUSE_AMBIENT   : pcuint32 = pointer($040004C0);
  GFX_SPECULAR_EMISSION : pcuint32 = pointer($040004C4);
  GFX_SHININESS         : pcuint32 = pointer($040004D0);

  GFX_POLY_FORMAT       : pcuint32 = pointer($040004A4);
  GFX_ALPHA_TEST        : pcuint16 = pointer($04000340);

  GFX_BEGIN             : pcuint32 = pointer($04000500);
  GFX_END               : pcuint32 = pointer($04000504);
  GFX_FLUSH             : pcuint32 = pointer($04000540);
  GFX_VIEWPORT          : pcuint32 = pointer($04000580);
  GFX_TOON_TABLE        : pcuint16 = pointer($04000380);
  GFX_EDGE_TABLE        : pcuint16 = pointer($04000330);
  GFX_FOG_COLOR         : pcuint32 = pointer($04000358);
  GFX_FOG_OFFSET        : pcuint32 = pointer($0400035C);
  GFX_FOG_TABLE         : pcuint8 = pointer($04000360);
  GFX_BOX_TEST          : pcint32 = pointer($040005C0);
  GFX_POS_TEST          : pcint32 = pointer($040005C4);
  GFX_POS_RESULT        : pcint32 = pointer($04000620);
{$endif NDS_INTERFACE}
  
{$ifdef NDS_IMPLEMENTATION}
function GFX_BUSY: boolean; inline;
begin
	GFX_BUSY := GFX_STATUS^ and (1 shl 27) <> 0;
end;
{$endif NDS_IMPLEMENTATION}


{$ifdef NDS_INTERFACE}
const
  GFX_VERTEX_RAM_USAGE	: pcuint16 = pointer($04000606);
  GFX_POLYGON_RAM_USAGE	: pcuint16 = pointer($04000604);

  GFX_CUTOFF_DEPTH		  : pcuint16 = pointer($04000610);

// Matrix processor control

  MATRIX_CONTROL		: pcuint32 = pointer($04000440);
  MATRIX_PUSH			  : pcuint32 = pointer($04000444);
  MATRIX_POP        : pcuint32 = pointer($04000448);
  MATRIX_SCALE		  : pcint32 = pointer($0400046C);
  MATRIX_TRANSLATE	: pcint32 = pointer($04000470);
  MATRIX_RESTORE		: pcuint32 = pointer($04000450);
  MATRIX_STORE		  : pcuint32 = pointer($0400044C);
  MATRIX_IDENTITY		: pcuint32 = pointer($04000454);
  MATRIX_LOAD4x4		: pcint32 = pointer($04000458);
  MATRIX_LOAD4x3		: pcint32 = pointer($0400045C);
  MATRIX_MULT4x4		: pcint32 = pointer($04000460);
  MATRIX_MULT4x3		: pcint32 = pointer($04000464);
  MATRIX_MULT3x3		: pcint32 = pointer($04000468);

//matrix operation results
  MATRIX_READ_CLIP   : pcint32 = pointer($04000640);
  MATRIX_READ_VECTOR : pcint32 = pointer($04000680);
  POINT_RESULT       : pcint32 = pointer($04000620);
  VECTOR_RESULT      : pcuint16 = pointer($04000630);
{$endif NDS_INTERFACE}

{$ifdef NDS_INTERFACE}
function RGB15(r, g, b: cint): cuint16; 
function RGB5(r, g, b: cint): cuint16;
function RGB8(r, g, b: cint): cuint8;
function  VRAM_OFFSET(n: cint): cint; inline;	
function DISPLAY_CHAR_BASE(n: cuint32): cuint32; inline;
function DISPLAY_SCREEN_BASE(n: cuint32): cuint32; inline;
procedure videoSetMode(mode: cuint32); inline;  
procedure videoSetModeSub(mode: cuint32); inline;  
function BG_PRIORITY(n: cint): cint; inline;
function BG_TILE_BASE(base: cint): cint; inline; 
function BG_MAP_BASE(base: cint): cint; inline;  
function BG_BMP_BASE(base: cint): cint; inline;  
function BG_MAP_RAM(base: cint): cint; inline;  
function BG_MAP_RAM_SUB(base: cint): cint; inline; 
function BG_TILE_RAM(base: cint): cint; inline; 
function BG_TILE_RAM_SUB(base: cint): cint; inline; 
function BG_BMP_RAM(base: cint): cint; inline;  
function BG_BMP_RAM_SUB(base: cint): cint; inline; 
function DCAP_MODE(n: cint): cint; inline;   
function DCAP_DST(n: cint): cint; inline;    
function DCAP_SRC(n: cint): cint; inline;    
function DCAP_SIZE(n: cint): cint; inline;   
function DCAP_OFFSET(n: cint): cint; inline; 
function DCAP_BANK(n: cint): cint; inline;   
function DCAP_B(n: cint): cint; inline;      
function DCAP_A(n: cint): cint; inline;      
function GFX_BUSY: boolean; inline;
{$endif NDS_INTERFACE}
