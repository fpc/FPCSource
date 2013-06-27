(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba timer definitions

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
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
	MA 02111-1301 USA.

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
  REG_TM0CNT		: pu32 = pointer(REG_BASE + $100);
  REG_TM0CNT_L	: pu16 = pointer(REG_BASE + $100);
  REG_TM0CNT_H	: pu16 = pointer(REG_BASE + $102);

  REG_TM1CNT		: pu32 = pointer(REG_BASE + $104);
  REG_TM1CNT_L	: pu16 = pointer(REG_BASE + $104);
  REG_TM1CNT_H	: pu16 = pointer(REG_BASE + $106);

  REG_TM2CNT		: pu32 = pointer(REG_BASE + $108);
  REG_TM2CNT_L	: pu16 = pointer(REG_BASE + $108);
  REG_TM2CNT_H	: pu16 = pointer(REG_BASE + $10a);

  REG_TM3CNT		: pu32 = pointer(REG_BASE + $10c);
  REG_TM3CNT_L	: pu16 = pointer(REG_BASE + $10c);
  REG_TM3CNT_H	: pu16 = pointer(REG_BASE + $10e);


{$define TIMER_COUNT  := BIT(2)}
{$define TIMER_IRQ    := BIT(6)}
{$define TIMER_START  := BIT(7)}

{$endif GBA_INTERFACE}
