(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba multiboot definitions

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
	 MA 02110-1301,	USA.

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
  MultiBootParam = record
    reserved1: array [0..4] of u32;
    handshake_data: u8;
    padding: u8;
    handshake_timeout: u16;
    probe_count: u8;
    client_data: array [0..2] of u8;
    palette_data: u8;
    response_bit: u8;
    client_bit: u8;
    reserved2: u8;
    boot_srcp: pu8;
    boot_endp: pu8;
    masterp: pu8;
    reserved3: array [0..2] of pu8;
    system_work2: array [0..3] of u32;
    sendflag: u8;
    probe_target_bit: u8;
    check_wait: u8;
    server_type: u8;
  end;
  PMultiBootParam = ^MultiBootParam;

  MULTIBOOT_MODES = (MODE32_NORMAL, MODE16_MULTI, MODE32_2MHZ);

function MultiBoot(mp: PMultiBootParam; mode: u32): u32; cdecl; external;
{$endif GBA_INTERFACE}
