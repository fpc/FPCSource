(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba mbv2 functions

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
//---------------------------------------------------------------------------------
// Don't Use these function names
//---------------------------------------------------------------------------------
procedure mbv2_dprintf(str: pchar; args: array of const); cdecl; external;
procedure mbv2_dfprintf(fp: integer; str: pchar; args: array of const); cdecl; external;
function mbv2_dputchar(c: integer): integer; cdecl; external;
function mbv2_dgetch(): integer; cdecl; external;
function mbv2_dkbhit(): integer; cdecl; external;

function mbv2_dfopen(const _file: pchar; const _type: pchar): integer; cdecl; external;
function mbv2_dfclose	(fp: integer): integer; cdecl; external;
function mbv2_dfgetc(fp: integer): integer; cdecl; external;
function mbv2_dfputc(ch, fp: integer): integer; cdecl; external;
procedure mbv2_drewind(fp: integer); cdecl; external;

//---------------------------------------------------------------------------------
// Use these function names instead
// these will be repeated for VBA & Xcomms
//---------------------------------------------------------------------------------
procedure dprintf(str: pchar; args: array of const); inline;
procedure dfprintf(fp: integer; str: pchar; args: array of const); inline;
function dputchar(c: integer): integer; inline;
function dgetch(): integer; inline;
function dkbhit(): integer; inline;
function dfopen(const _file: pchar; const _type: pchar): integer; inline;
function dfclose	(fp: integer): integer; inline;
function dfgetc(fp: integer): integer; inline;
function dfputc(ch, fp: integer): integer; inline;
procedure drewind(fp: integer); inline;


const
  __DOUTBUFSIZE	= 256;
  __FINBUFSIZE	= 256;  //Must be a multiple of 2! (ex: 32,64,128,256,512..)
  __KINBUFSIZE	= 64;   //Must be a multiple of 2! (ex: 32,64,128,256,512..)
  __ESCCHR		= 27;

  __ESC_NADA				= 0;
  __ESC_ESCCHR			= 1;
  __ESC_FOPEN				= 2;
  __ESC_FCLOSE			= 3;
  __ESC_FGETC				= 4;
  __ESC_FPUTC				= 5;
  __ESC_REWIND			= 6;
  __ESC_FPUTC_PROCESSED	= 7;	// PC side add CR before LF if DOS machine
  __ESC_KBDCHR 			= 8;

function __dputchar (c: integer): integer; cdecl; external;
{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}
//---------------------------------------------------------------------------------
// Use these function names instead
// these will be repeated for VBA & Xcomms
//---------------------------------------------------------------------------------
procedure dprintf(str: pchar; args: array of const); inline;
begin
  mbv2_dprintf(str, args); 
end;

procedure dfprintf(fp: integer; str: pchar; args: array of const); inline;
begin
  mbv2_dfprintf(fp, str, args); 
end;

function dputchar(c: integer): integer; inline;
begin
  dputchar := mbv2_dputchar(c);
end;

function dgetch(): integer; inline;
begin
  dgetch := mbv2_dgetch();
end;

function dkbhit(): integer; inline;
begin
  dkbhit := mbv2_dkbhit();
end;


function dfopen(const _file: pchar; const _type: pchar): integer; inline;
begin
  dfopen := mbv2_dfopen(_file, _type);
end;

function dfclose	(fp: integer): integer; inline;
begin
  dfclose := mbv2_dfclose(fp);
end;

function dfgetc(fp: integer): integer; inline;
begin
  dfgetc := mbv2_dfgetc(fp);
end;

function dfputc(ch, fp: integer): integer; inline;
begin
  dfputc := mbv2_dfputc(ch, fp); 
end;

procedure drewind(fp: integer); inline;
begin
  mbv2_drewind(fp);
end;
{$endif GBA_IMPLEMENTATION}
