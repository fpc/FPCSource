(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba bios compression functions

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

type
  BUP = record
    SrcNum: u16;				// Source Data Byte Size
    SrcBitNum: u8;			// 1 Source Data Bit Number
    DestBitNum: u8;			// 1 Destination Data Bit Number
    {u32 }DestOffset: 0..31;		// Number added to Source Data
    {u32 }DestOffset0_On: 0..1;	// Flag to add/not add Offset to 0 Data
  end;
  PBUP = ^BUP;

//---------------------------------------------------------------------------------
// Decompression functions
//---------------------------------------------------------------------------------
procedure BitUnPack(source: pointer; dest: pointer; _bup: PBUP); cdecl; external;
procedure LZ77UnCompWram(source, dest: pointer); cdecl; external;
procedure LZ77UnCompVram(source, dest: pointer); cdecl; external;
procedure HuffUnComp(source, dest: pointer); cdecl; external;
procedure RLUnCompWram(source, dest: pointer); cdecl; external;
procedure RLUnCompVram(source, dest: pointer); cdecl; external;
procedure Diff8bitUnFilterWram(source, dest: pointer); cdecl; external;
procedure Diff8bitUnFilterVram(source, dest: pointer); cdecl; external;
procedure Diff16bitUnFilter(source, dest: pointer); cdecl; external;

{$endif GBA_INTERFACE}
