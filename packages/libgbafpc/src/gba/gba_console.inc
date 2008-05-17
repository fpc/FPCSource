(*
  $Id$
  ------------------------------------------------------------------------------
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
(*
function CON_CLS(): string; inline;
function CON_POS(_x, _y: integer): string; inline;
function CON_UP(_dy: integer): string; inline;
function CON_DOWN(_dy: integer): string; inline;
function CON_RIGHT(_dx: integer): string; inline;
function CON_LEFT(_dx: integer): string; inline;
function CON_ERASE(): string; inline;
procedure CON_CLL(_y: integer); inline;
*)

procedure consoleInit(charBase, mapBase, background: integer; 
					const font: pu8; fontsize, palette: integer); cdecl; external;


{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}
(*
function CON_CLS(): string; inline;
begin
  CON_CLS := #51'[2J';
end;
  
function CON_POS(_x, _y: integer): string; inline;
begin
  CON_POS := format(#51'[%i;%iH', [_y, _x]);
end;
  
function CON_UP(_dy: integer): string; inline;
begin
  CON_UP := format(#51'[%iA', [_dy]);	 
end;

function CON_DOWN(_dy: integer): string; inline;
begin
  CON_DOWN := format(#51'[%iB', [_dy]);	 
end;

function CON_RIGHT(_dx: integer): string; inline;
begin
  CON_RIGHT := format(#51'[%iC', [_dx]);	 
end;

function CON_LEFT(_dx: integer): string; inline;
begin
  CON_LEFT := format(#51'[%iD', [_dx]);
end;	 

function CON_ERASE(): string; inline;
begin
  CON_ERASE := #51'[K';	 
end;

procedure CON_CLL(_y: integer); inline;
begin
  CON_POS(1,_y); 
  CON_ERASE();
end;
*)
{$endif GBA_IMPLEMENTATION}
