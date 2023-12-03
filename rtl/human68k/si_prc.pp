{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team

    System Entry point for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

interface

{$.define FPC_HUMAN68K_USE_TINYHEAP}

implementation

{$include h68kdos.inc}

var
  stacktop: pointer; public name '__stktop';
  stklen: longint; external name '__stklen';
{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
  initial_heap_start: pointer; external name '__initial_heap_start';
  initial_heap_end: pointer; external name '__initial_heap_end';
{$endif FPC_HUMAN68K_USE_TINYHEAP}

var
  h68k_startup: Th68kdos_startup; public name '_h68k_startup';
  h68k_psp: Ph68kdos_psp; public name '_h68k_psp';

procedure PascalMain; external name 'PASCALMAIN';
procedure PascalStart(const startparams: Ph68kdos_startup); noreturn; forward;

{ this function must be the first in this unit which contains code }
procedure _FPC_proc_start; assembler; nostackframe; noreturn; public name '_start';
asm
  move.l  a1,sp
  add.l   stklen,sp
  movem.l a0-a5,-(sp)
  move.l sp,a0
  jbsr PascalStart
end;

procedure PascalStart(const startparams: Ph68kdos_startup); noreturn;
var
  bss_start: pbyte;
  blocksize: longint;
begin
  with startparams^ do
    begin
      { clear BSS }
      bss_start:=pbyte(pdword(@mcb[$30])^);
      fillchar(bss_start^,bss_end-bss_start,0);

      h68k_psp:=pointer(@mcb[$10]);
{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
      blocksize:=bss_end-pointer(h68k_psp)+stklen+heapsize;
{$else FPC_HUMAN68K_USE_TINYHEAP}
      blocksize:=bss_end-pointer(h68k_psp)+stklen;
{$endif FPC_HUMAN68K_USE_TINYHEAP}
      h68kdos_setblock(h68k_psp,blocksize);
      stacktop:=bss_end+stklen;
{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
      initial_heap_start:=stacktop;
      initial_heap_end:=initial_heap_start+heapsize;
{$endif FPC_HUMAN68K_USE_TINYHEAP}
    end;

  h68k_startup:=startparams^;

  PASCALMAIN;
end;

procedure _FPC_proc_halt(_ExitCode: longint); noreturn; public name '_haltproc';
begin
  h68kdos_exit2(_ExitCode);
end;

end.
