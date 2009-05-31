(*
  $Id$
  ------------------------------------------------------------------------------
  This lib is a raw porting of tonclib library for gba (you can find it at
  http://user.chem.tue.nl/jakvijn/index.htm).
  
  As this is a direct port from c, I'm pretty sure that something could not work
  as you expect. I am even more sure that this code could be written better, so 
  if you think that I have made some mistakes or you have some better 
  implemented functions, let me know [francky74 (at) gmail (dot) com]
  Enjoy!

  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
  
  Copyright (C) 2006  Francesco Lombardi
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA

  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

  
*)


{$ifdef GBA_INTERFACE}
{$H+}

procedure memset16(dest: pointer; hw: word; hwcount: dword); cdecl; external;
procedure memcpy16(dest: pointer; const src: pointer; hwcount: dword); cdecl; external;

procedure memset32(dest: pointer; wd: dword; wcount: dword); cdecl; external;
procedure memcpy32(dest: pointer; const src: pointer; wcount: dword); cdecl; external;

function printf(format: Pchar; args: array of const): longint; cdecl; external;
function printf(format: Pchar): longint; cdecl; varargs; external;
function sprintf(s: Pchar; format: Pchar; args: array of const): longint; cdecl; external;
function sprintf(s: Pchar; format: Pchar): longint; varargs; cdecl; external;
function iprintf(format: Pchar; args: array of const): longint; cdecl; external;
function iprintf(format: Pchar): longint; varargs; cdecl; external;
function scanf(format: Pchar; args: array of const): longint; cdecl; external;
function scanf(format: Pchar): longint; cdecl; varargs; external;
function sscanf(s: Pchar; format: Pchar; args: array of const): longint; cdecl; external;
function sscanf(s: Pchar; format: Pchar): longint; cdecl; varargs; external;

function malloc(size: integer): pointer; cdecl; external;
function realloc(ptr: pointer; size: integer): pointer; cdecl; external;
procedure free(ptr: pointer); cdecl; external;
function memcpy(dest: pointer; src: pointer; n: integer): pointer; cdecl; external;

(* libc file handling types and routines *)
type
  _FILE = record
    firstCluster: cuint;
    length: cuint;
    curPos: cuint;
    curClus: cuint;			// Current cluster to read from
    curSect: integer;			// Current sector within cluster
    curByte: integer;			// Current byte within sector
    readBuffer: array [0..511] of byte;	// Buffer used for unaligned reads
    appClus: cuint;			// Cluster to append to
    appSect: integer;			// Sector within cluster for appending
    appByte: integer;			// Byte within sector for appending
    read: boolean;	// Can read from file
    write: boolean;	// Can write to file
    append: boolean;// Can append to file
    inUse: boolean;	// This file is open
    dirEntSector: cuint;	// The sector where the directory entry is stored
    dirEntOffset: integer;	// The offset within the directory sector
  end;
  P_FILE = ^_FILE;

const
   SEEK_SET = 0;
   SEEK_CUR = 1;
   SEEK_END = 2;

function fopen(filename: Pchar; modes: Pchar): P_FILE; cdecl; external;
function fread(ptr: pointer; size: longint; n: longint; stream: P_FILE): longint; cdecl; external;
function fwrite(ptr: pointer; size: longint; n: longint; s: P_FILE): longint; cdecl; external;
function ftell(stream: P_FILE): longint; cdecl; external;
function fseek(stream: P_FILE; off: longint; whence: longint): longint; cdecl; external;
function fclose(stream: P_FILE): longint; cdecl; external;

(* Directory iterator for mantaining state between dir* calls *)
type
  DIR_ITER = record
    device: cint;
    dirStruct: pointer;
  end;
  PDIR_ITER = ^DIR_ITER;

  stat = packed record
    st_dev: qword;
    __pad1: word;
    __align_pad1: word;
    st_ino: dword;
    st_mode : dword;
    st_nlink : dword;
    st_uid : dword;
    st_gid : dword;
    st_rdev : qword;
    __pad2 : word;
    __align_pad2 : word;
    st_size : longint;
    st_blksize : longint;
    st_blocks : longint;
    st_atime : longint;
    __unused1 : dword;
    st_mtime : longint;
    __unused2 : dword;
    st_ctime : longint;
    __unused3 : dword;
    __unused4 : dword;
    __unused5 : dword;
  end;
  Pstat = ^stat;

const
  S_IFMT        = $F000;
  S_IFDIR       = $4000;
  S_IFCHR       = $2000;
  S_IFBLK       = $6000;
  S_IFREG       = $8000;
  S_IFIFO       = $1000;
  S_IFLNK       = $A000;
  S_IFSOCK      = $C000;

  S_ISUID       = $800;
  S_ISGID       = $400;
  S_ISVTX       = $200;
  S_IREAD       = $100;
  S_IWRITE      = $80;
  S_IEXEC       = $40;

function diropen(const path: pchar): PDIR_ITER; cdecl; external;
function dirreset(dirState: PDIR_ITER): cint; cdecl; external;
function dirnext(dirState: PDIR_ITER; filename: pchar; filestat: Pstat): cint; cdecl; external;
function dirclose(dirState: PDIR_ITER): cint; cdecl; external;

procedure DebugPrint(s: string); assembler; inline;
function GBAIntToStr(i: integer): string;

{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}

// memory handling routines
// these are in ASM and optimized; use when possible
{$l core_asm.o}

procedure DebugPrint(s: string); assembler; inline;
asm
  mov r0,s
  swi #0xff0000
end['r0'];

function GBAIntToStr(i: integer): string;
var
  s: string;
begin
  str(i, s);
  GBAIntToStr := s;
end;



{$endif GBA_IMPLEMENTATION}
