(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba base macros, included by all libgba files

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
const
  VRAM      = $06000000;
  IWRAM     = $03000000;
  EWRAM     = $02000000;
  EWRAM_END = $02040000;
  SRAM      = $0E000000;
  REG_BASE  = $04000000;




(* No thumb mode in fpc and it can't handle such preprocessor code *)
{
#if	defined	( __thumb__ )
#define	SystemCall(Number)	 __asm ("SWI	  "#Number"\n" :::  "r0", "r1", "r2", "r3")
#else
#define	SystemCall(Number)	 __asm ("SWI	  "#Number"	<< 16\n" :::"r0", "r1", "r2", "r3")
#endif
}
// SysCall conversion
const
  SC_ARM_2 = (2 shl 16);
  SC_ARM_3 = (3 shl 16);
  SC_ARM_5 = (5 shl 16);
  SC_ARM_28 = (28 shl 16);
  SC_ARM_29 = (29 shl 16);
  SC_ARM_30 = (30 shl 16);
  SC_ARM_40 = (40 shl 16);
  SC_ARM_41 = (41 shl 16);

{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}
function BIT(number: integer): integer; inline;
begin
  BIT := (1 shl (number));
end;
{$endif GBA_IMPLEMENTATION}


(* No attribute handling in fpc *)
{
#define IWRAM_CODE	__attribute__((section(".iwram"), long_call))
#define EWRAM_CODE	__attribute__((section(".ewram"), long_call))

#define IWRAM_DATA	__attribute__((section(".iwram")))
#define EWRAM_DATA	__attribute__((section(".ewram")))
#define EWRAM_BSS	__attribute__((section(".sbss")))
#define ALIGN(m)	__attribute__((aligned (m)))
}


{$ifdef GBA_INTERFACE}
//---------------------------------------------------------------------------------
// modes for DMA and CPU(Fast)Set
//---------------------------------------------------------------------------------
type
  DMA_MODES = integer;
const
	FILL   : DMA_MODES = (1 shl 24);
	COPY16 : DMA_MODES = (0 shl 26);
	COPY32 : DMA_MODES = (1 shl 26);
{$endif GBA_INTERFACE}


{$ifdef GBA_INTERFACE}
function BIT(number: integer): integer; inline;
{$endif GBA_INTERFACE}

