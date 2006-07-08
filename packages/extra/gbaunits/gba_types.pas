(*
  gba_types.pas 18/06/2006 4.38.46
  ------------------------------------------------------------------------------
  This lib is a raw porting of libgba library for gba (you can find it at
  http://www.devkitpro.org).
  
  As this is a direct port from c, I'm pretty sure that something could not work
  as you expect. I am even more sure that this code could be written better, so 
  if you think that I have made some mistakes or you have some better 
  implemented functions, let me know [francky74 (at) gmail (dot) com]
  Enjoy!

  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
  
  Copyright (C) 2006  Francesco Lombardi
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
  ------------------------------------------------------------------------------
*)
unit gba_types;
{$i def.inc}
interface

type
  (* Unsigned types *)
  u8  = byte;
  u16 = word;
  u32 = dword;
  pu8 = ^u8;
  pu16 = ^u16;
  pu32 = ^u32;
  
  (* Signed types *)
  s8  = shortint;
  s16 = smallint;
  s32 = longint;
  ps8 = ^s8;
  ps16 = ^s16;
  ps32 = ^s32;

  BGAffineSource = record
    x: longint;
    y: longint;
    tX: smallint;
    tY: smallint;
    sX: smallint;
    sY: smallint;
    theta: smallint;
  end;
  TBgAffineSource = BGAffineSource;
  PBgAffineSource = ^TBgAffineSource;
   
  BGAffineDest = record
    pa: smallint;
    pb: smallint;
    pc: smallint;
    pd: smallint;
    x: longint;
    y: longint;
  end;
  TBgAffineDest = BGAffineDest;
  PBgAffineDest = ^TBgAffineDest;

  ObjAffineSource = record
    sX: smallint;
    sY: smallint;
    theta: word;
  end;
  TObjAffineSource = ObjAffineSource;
  PObjAffineSource = ^TObjAffineSource;

  ObjAffineDest = record
    pa: smallint;
    pb: smallint;
    pc: smallint;
    pd: smallint;
  end;
  TObjAffineDest = ObjAffineDest;
  PObjAffineDest = ^TObjAffineDest;

  BUP = record
    SrcNum: word;				  // Source Data Byte Size
    SrcBitNum: byte;			  // 1 Source Data Bit Number
    DestBitNum: byte;			  // 1 Destination Data Bit Number
    DestOffset: dword;		  // Number added to Source Data
    DestOffset0_On: dword;	// Flag to add/not add Offset to 0 Data
  end;
  TBUP = BUP;
  PBUP = ^TBUP;

  FIXED = longint;
  TFixed = FIXED;
  
  COLOR = word;
  TColor = COLOR;

  
  MultiBootParam = record 
    reserved1: array [0..4] of dword;
    handshake_data: byte;
    padding: byte;
    handshake_timeout: word;
    probe_count: byte;
    client_data: array [0..2] of byte;
    palette_data: byte;
    response_bit: byte;
    client_bit: byte;
    reserved2: byte;
    boot_srcp: ^byte;
    boot_endp: ^byte;
    masterp: ^byte;
    reserved3: array [0..2] of ^byte;
    system_work2: array [0..3] of dword;
    sendflag: byte;
    probe_target_bit: byte;
    check_wait: byte;
    server_type: byte;
  end;
  TMultiBootParam = MultiBootParam;
  PMultiBootParam = ^TMultiBootParam;
  
  MULTIBOOT_MODES = ( MODE32_NORMAL, MODE16_MULTI, MODE32_2MHZ);
  TMultiBootModes = MULTIBOOT_MODES;
  
  DMA_MODES = (
    FILL    = (1 shl 24),
    COPY16  = (0 shl 26),
    COPY32  = (1 shl 26)
  );
  TDMAModes = DMA_MODES;

  KEYPAD_BITS = (
    KEY_A       = (1 shl 0),
    KEY_B       = (1 shl 1),
    KEY_SELECT  = (1 shl 2),
    KEY_START   = (1 shl 3),	
    KEY_RIGHT   = (1 shl 4),
    KEY_LEFT    = (1 shl 5),
    KEY_UP      = (1 shl 6),
    KEY_DOWN    = (1 shl 7),
    KEY_R       = (1 shl 8),
    KEY_L       = (1 shl 9),

    KEYIRQ_ENABLE = (1 shl 14),
    KEYIRQ_OR     = (0 shl 15),
    KEYIRQ_AND    = (1 shl 15),
    DPAD          = integer(KEY_UP) or integer(KEY_DOWN) or integer(KEY_LEFT) or integer(KEY_RIGHT)
  );
  TKeyPadBits = KEYPAD_BITS;
  
  OBJATTR = record
    attr0: word;
    attr1: word;
    attr2: word;
    dummy: word;
  end;
  TObjAttr = OBJATTR;
  PObjAttr = ^TObjAttr;

  OBJAFFINE = record
    dummy0: array [0..2] of word;
    pa: smallint;
    dummy1: array [0..2] of word;
    pb: smallint;
    dummy2: array [0..2] of word;
    pc: smallint;
    dummy3: array [0..2] of word;
    pd: smallint;
  end;
  TObjAffine = OBJAFFINE;
  PObjAffine = ^TObjAffine;
  
  bg_scroll = record
    x: word;
    y: word;
  end;
  TBgScroll = bg_scroll;
  PBgScroll = ^TBgScroll;  


implementation


end.
