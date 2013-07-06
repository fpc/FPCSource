(*
  $Id$
  ------------------------------------------------------------------------------
  Header file for libgba interrupt handling

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
//  IntFn = procedure;
  IntFn = procedure of object;
  TVoidFunctionPointer = procedure of object;

  IntTable = record
    handler: IntFn;
    mask: cuint32;
  end;
  PIntTable = ^IntTable;


const
  MAX_INTS = 15;
//  INT_VECTOR  : IntFn = pointer($03007ffc);   // BIOS Interrupt vector
var
  INT_VECTOR  : IntFn absolute $03007ffc;   // BIOS Interrupt vector

  REG_IME     : pu16 = pointer(REG_BASE + $208);  // Interrupt Master Enable
  REG_IE      : pu16 = pointer(REG_BASE + $200);  // Interrupt Enable
  REG_IF      : pu16 = pointer(REG_BASE + $202);  // Interrupt Request


type
  irqMASKS = integer;
  irqMASK  = irqMASKS;
const
  IRQ_VBLANK :  irqMASK = (1 shl  0);
  IRQ_HBLANK :  irqMASK = (1 shl  1);
  IRQ_VCOUNT :  irqMASK = (1 shl  2);
  IRQ_TIMER0 :  irqMASK = (1 shl  3);
  IRQ_TIMER1 :  irqMASK = (1 shl  4);
  IRQ_TIMER2 :  irqMASK = (1 shl  5);
  IRQ_TIMER3 :  irqMASK = (1 shl  6);
  IRQ_SERIAL :  irqMASK = (1 shl  7);
  IRQ_DMA0   :  irqMASK = (1 shl  8);
  IRQ_DMA1   :  irqMASK = (1 shl  9);
  IRQ_DMA2   :  irqMASK = (1 shl 10);
  IRQ_DMA3   :  irqMASK = (1 shl 11);
  IRQ_KEYPAD :  irqMASK = (1 shl 12);
  IRQ_GAMEPAK:  irqMASK = (1 shl 13);


var
//  IntrTable: array [0..MAX_INTS-1] of IntTable; cvar; // cvar used in order to gain access from assembly unit
  IntrTable: array [0..0] of IntTable; cvar; external;         

procedure irqInit(); cdecl; external;

procedure irqSet(mask: irqMASK; func: TVoidFunctionPointer); cdecl; external;
procedure irqSet(mask: irqMASK; func: pointer); cdecl; external;
procedure irqEnable(mask: cint); cdecl; external;
procedure irqDisable(mask: cint); cdecl; external;

procedure IntrMain(); cdecl; external;
{$endif GBA_INTERFACE}
