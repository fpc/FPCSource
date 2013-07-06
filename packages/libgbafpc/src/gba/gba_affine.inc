(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba bios affine functions

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
//---------------------------------------------------------------------------------
type
  t_BGAffineSource = record 
    x: s32;				
    y: s32;
    tX: s16;
    tY: s16;
    sX: s16;
    sY: s16;
    theta: u16;
  end; 
  BGAffineSource = t_BGAffineSource;
  PBGAffineSource = ^BGAffineSource;

  t_BGAffineDest = record
    pa: s16;
    pb: s16;
    pc: s16;
    pd: s16;
    x: s32;	
    y: s32;	
  end;
  BGAffineDest = t_BGAffineDest;
  PBGAffineDest = ^BGAffineDest;


  t_ObjAffineSource = record
    sX: s16;
    sY: s16;
    theta: u16;
  end;
  ObjAffineSource = t_ObjAffineSource;
  PObjAffineSource = ^ObjAffineSource;

  t_ObjAffineDest = record
    pa: s16;
    pb: s16;
    pc: s16;
    pd: s16;
  end;
  ObjAffineDest = t_ObjAffineDest;
  PObjAffineDest = ^ObjAffineDest;


procedure ObjAffineSet(source: PObjAffineSource; dest: pointer; num, offset: s32); cdecl; external;
procedure BgAffineSet(source: PBGAffineSource; dest: PBGAffineDest; num: s32); cdecl; external;

{$endif GBA_INTERFACE}

