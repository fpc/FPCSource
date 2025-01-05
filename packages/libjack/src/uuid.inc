{
    Copyright (C) 2013 Paul Davis
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software 
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

}

{$ifndef __jack_uuid_h__}
{$define __jack_uuid_h__}

//#include <jack/types.h>

//#ifdef __cplusplus
//extern "C" {
//#endif

const
  JACK_UUID_SIZE = 36;
  JACK_UUID_STRING_SIZE = (JACK_UUID_SIZE+1); { includes trailing null }
  JACK_UUID_EMPTY_INITIALIZER = 0;

function jack_client_uuid_generate: jack_uuid_t; cdecl; external libjack;
function jack_port_uuid_generate (port_id: uint32_t): jack_uuid_t; cdecl; external libjack;

function jack_uuid_to_index (u: jack_uuid_t): uint32_t; cdecl; external libjack;

function jack_uuid_compare (u1, u2: jack_uuid_t): cint; cdecl; external libjack;
procedure jack_uuid_copy (dst: Pjack_uuid_t; src: jack_uuid_t); cdecl; external libjack;
procedure jack_uuid_clear (dst: Pjack_uuid_t); cdecl; external libjack;
function jack_uuid_parse (const buf: PChar; dst: Pjack_uuid_t): cint; cdecl; external libjack;
procedure jack_uuid_unparse (src: jack_uuid_t; buf: PChar {[JACK_UUID_STRING_SIZE]}); cdecl; external libjack;
function jack_uuid_empty (src: jack_uuid_t): cint; cdecl; external libjack;

//#ifdef __cplusplus
//} /* namespace */
//#endif

{$endif __jack_uuid_h__}

