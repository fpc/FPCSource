{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Free Pascal development team

    DOS related defines for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  h68kdos_comline = record
    case boolean of
      true: ( len: byte; buffer: array[0..255] of char; );
      false: ( pstr: shortstring; )
  end;

type
  h68kdos_psp = record
    env: pchar;
    _exit: pointer;
    ctrlc: pointer;
    errexit: pointer;
    comline: ^h68kdos_comline;
    handle: array[0..11] of byte;
    bss: pointer;
    heap: pointer;
    stack: pointer;
    usp: pointer;
    ssp: pointer;
    sr: word;
    abort_sr: word;
    abort_ssp: pointer;
    trap10: pointer;
    trap11: pointer;
    trap12: pointer;
    trap13: pointer;
    trap14: pointer;
    osflg: dword;
    reserve_1: array[0..27] of byte;
    exe_path: array[0..67] of char;
    exe_name: array[0..23] of char;
    reserve_2: array[0..35] of byte;
  end;

procedure h68kdos_exit; noreturn; syscall $ff00;
function h68kdos_write(fileno: word; buffer: pointer; len: longint): longint; syscall $ff40;
function h68kdos_malloc(size: longint): pointer; syscall $ff48;
function h68kdos_mfree(ptr: pointer): longint; syscall $ff49;
function h68kdos_setblock(ptr: pointer; size: longint): longint; syscall $ff4a;
procedure h68kdos_exit2(code: smallint); noreturn; syscall $ff4c;
