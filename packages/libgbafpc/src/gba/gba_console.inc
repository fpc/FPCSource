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
// Make console interaction a little less eye-crossing

function CON_CLS(): PAnsiChar; inline;
function CON_ERASE(): PAnsiChar; inline;
function CON_POS(_x, _y: cint): PAnsiChar; inline;
function CON_UP(_dy: cint): PAnsiChar; inline;
function CON_DOWN(_dy: cint): PAnsiChar; inline;
function CON_RIGHT(_dx: cint): PAnsiChar; inline;
function CON_LEFT(_dx: cint): PAnsiChar; inline;
function CON_CLL(_y: cint): PAnsiChar; inline;

procedure consoleInit(charBase, mapBase, background: cint;
					const font: pcuint8; fontsize, palette: cint); cdecl; external;
procedure consoleDemoInit(); cdecl; external;

{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}
function CON_CLS(): PAnsiChar; inline;
begin
 CON_CLS := #27'[2J';
end;

function CON_ERASE(): PAnsiChar; inline;
begin
  CON_ERASE := #27'[K';
end;

function CON_POS(_x, _y: cint): PAnsiChar; inline;
begin
  CON_POS := PAnsiChar(#27'[' + (GBAIntToStr(_y)) +';' + (GBAIntToStr(_x)) +  'H');
end;

function CON_UP(_dy: cint): PAnsiChar; inline;
begin
  CON_UP := PAnsiChar(#27'[' + GBAIntToStr(_dy) +'A');
end;

function CON_DOWN(_dy: cint): PAnsiChar; inline;
begin
	CON_DOWN := PAnsiChar(#27'[' + GBAIntToStr(_dy) + 'B');
end;

function CON_RIGHT(_dx: cint): PAnsiChar; inline;
begin
  CON_RIGHT := PAnsiChar(#27'[' + GBAIntToStr(_dx) + 'C');
end;

function CON_LEFT(_dx: cint): PAnsiChar; inline;
begin
  CON_LEFT := PAnsiChar(#27'[' + GBAIntToStr(_dx) + 'D');
end;

function CON_CLL(_y: cint): PAnsiChar; inline;
begin
  CON_CLL := PAnsiChar(concat(CON_POS(1, _y), CON_ERASE()));
end;
{$endif GBA_IMPLEMENTATION}
