(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba bios calls

	Copyright 2003-2005 by Dave Murphy.

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
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
	USA.

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
//---------------------------------------------------------------------------------
// Reset Functions
//---------------------------------------------------------------------------------
type
  RESTART_FLAG = integer;
const
  ROM_RESTART: RESTART_FLAG = 0; 	(* Restart from RAM entry point. *)
	RAM_RESTART: RESTART_FLAG	= 1;	(* restart from ROM entry point *)



procedure SoftReset(RestartFlag: RESTART_FLAG);cdecl; external;

type
  RESET_FLAG = integer;
  RESET_FLAGS = RESET_FLAG;
const
	RESET_EWRAM    : RESET_FLAGS  = (1 shl 0);	(* Clear 256K on-board WRAM			*)
	RESET_IWRAM    : RESET_FLAGS  = (1 shl 1);	(* Clear 32K in-chip WRAM				*)
	RESET_PALETTE  : RESET_FLAGS  = (1 shl 2);	(* Clear Palette						*)
	RESET_VRAM     : RESET_FLAGS  = (1 shl 3);	(* Clear VRAM							*)
	RESET_OAM      : RESET_FLAGS  = (1 shl 4);	(* Clear OAM							*)
	RESET_SIO      : RESET_FLAGS  = (1 shl 5);	(* Switches to general purpose mode	*)
	RESET_SOUND    : RESET_FLAGS  = (1 shl 6);	(* Reset Sound registers				*)
	RESET_OTHER    : RESET_FLAGS  = (1 shl 7);	(* all other registers				*)


procedure RegisterRamReset(ResetFlags: integer); cdecl; external;

//---------------------------------------------------------------------------------
// Interrupt functions
//---------------------------------------------------------------------------------

procedure Halt(); assembler; inline;

procedure Stop(); assembler; inline;

//---------------------------------------------------------------------------------
function BiosCheckSum(): u32; assembler; inline;

//---------------------------------------------------------------------------------
// Math functions
//---------------------------------------------------------------------------------
function Divi(Number, Divisor: s32): s32; cdecl; external;
function DivMod(Number, Divisor: s32): s32; cdecl; external;
function DivAbs(Number, Divisor: s32): u32; cdecl; external;
function DivArm(Divisor, Number: s32): s32; cdecl; external;
function DivArmMod(Divisor, Number: s32): s32; cdecl; external;
function DivArmAbs(Divisor, Number: s32): u32; cdecl; external;
function Sqrt(X: u32): u16; cdecl; external;

function ArcTan(Tn: s16): s16; cdecl; external;
function ArcTan2(X, Y: s16): u16; cdecl; external;

procedure CpuSet(const source: pointer; dest: pointer; mode: u32); cdecl; external;
procedure CpuFastSet(const source: pointer; dest: pointer; mode: u32); cdecl; external;

procedure IntrWait(ReturnFlag, IntFlag: u32); cdecl; external;

procedure VBlankIntrWait(); assembler; inline; 
{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}
//---------------------------------------------------------------------------------
// Interrupt functions
//---------------------------------------------------------------------------------

procedure Halt(); assembler; inline;
asm 
  SWI SC_ARM_2
end;

procedure Stop(); assembler; inline;
asm 
  SWI SC_ARM_3
end;

//---------------------------------------------------------------------------------
function BiosCheckSum(): u32; assembler; inline;
asm
	SWI	#0xd0000
  mov r0,#0
end;

procedure VBlankIntrWait(); assembler; inline; 
asm 
  SWI SC_ARM_5
end;	
{$endif GBA_IMPLEMENTATION}

