(*
  $Id: helper.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
    Copyright (C) 2007  Francesco Lombardi
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)

{$ifdef NDS_INTERFACE}
type
  time_t = cint;
  ptime_t = ^time_t;

  Ptm = ^tm;
  tm = record
    tm_sec: cint;
    tm_min: cint;
    tm_hour: cint;
    tm_mday: cint;
    tm_mon: cint;
    tm_year: cint;
    tm_wday: cint;
    tm_yday: cint;
    tm_isdst: cint;
  end;


(* Some libc functions *)
//function printf(format: Pchar; args: array of const): longint; cdecl; external;
function printf(format: Pchar): longint; cdecl; varargs; external;
//function sprintf(s: Pchar; format: Pchar; args: array of const): longint; cdecl; external;
function sprintf(s: Pchar; format: Pchar): longint; varargs; cdecl; external;
//function iprintf(format: Pchar; args: array of const): longint; cdecl; external;
function iprintf(format: Pchar): longint; varargs; cdecl; external;
//function scanf(format: Pchar; args: array of const): longint; cdecl; external;
function scanf(format: Pchar): longint; cdecl; varargs; external;
//function sscanf(s: Pchar; format: Pchar; args: array of const): longint; cdecl; external;
function sscanf(s: Pchar; format: Pchar): longint; cdecl; varargs; external;
function strcmp(s1: Pchar; s2: Pchar): longint; cdecl; external;

function malloc(size: integer): pointer; cdecl; external;
function realloc(ptr: pointer; size: integer): pointer; cdecl; external;
procedure free(ptr: pointer); cdecl; external;
function memcpy(dest: pointer; src: pointer; n: integer): pointer; cdecl; external;

function gmtime(timer: ptime_t): ptm; cdecl; external;
function time(timer: ptime_t): time_t; cdecl; external;

type
  TSort = function (const a, b: pointer): integer;
procedure qsort(__base: pointer; __nmemb: integer; __size: integer; __compar: TSort); cdecl; external;

function __errno: pcint; cdecl; export;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function __errno: pcint;  cdecl; export;
begin

end;
{$endif NDS_IMPLEMENTATION}

