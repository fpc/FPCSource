(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba input functions

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
  REG_KEYINPUT  : pu16 = pointer(REG_BASE + $130);  // Key Input
  REG_KEYCNT		: pu16 = pointer(REG_BASE + $132);  // Key Control

type
  KEYPAD_BITS = integer;
const
  KEY_A: KEYPAD_BITS       = (1 shl 0);
  KEY_B: KEYPAD_BITS       = (1 shl 1);
  KEY_SELECT: KEYPAD_BITS  = (1 shl 2);
  KEY_START: KEYPAD_BITS   = (1 shl 3);	
  KEY_RIGHT: KEYPAD_BITS   = (1 shl 4);
  KEY_LEFT: KEYPAD_BITS    = (1 shl 5);
  KEY_UP: KEYPAD_BITS      = (1 shl 6);
  KEY_DOWN: KEYPAD_BITS    = (1 shl 7);
  KEY_R: KEYPAD_BITS       = (1 shl 8);
  KEY_L: KEYPAD_BITS       = (1 shl 9);

  KEYIRQ_ENABLE: KEYPAD_BITS = (1 shl 14);
  KEYIRQ_OR: KEYPAD_BITS     = (0 shl 15);
  KEYIRQ_AND: KEYPAD_BITS    = (1 shl 15);
  DPAD: KEYPAD_BITS          = (1 shl 6) or (1 shl 7) or (1 shl 5) or (1 shl 4);


procedure scanKeys(); cdecl; external;

function keysDown(): u16; cdecl; external;
function keysDownRepeat(): u16; cdecl; external;
function keysUp(): u16; cdecl; external;
function keysHeld(): u16; cdecl; external;

procedure setRepeat(SetDelay, SetRepeat: integer); cdecl; external;
{$endif GBA_INTERFACE}

