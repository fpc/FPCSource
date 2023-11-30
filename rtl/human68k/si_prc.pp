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

implementation

{$include h68kdos.inc}

var
  stacktop: pointer; public name '__stktop';
  stklen: longint; external name '__stklen';

type
  Th68k_startup = record
    mcb: pbyte;
    bss_end: pbyte;
    comm: pbyte;
    environ: pbyte;
    entry: pbyte;
    intr: pbyte;
  end;
  Ph68k_startup = ^Th68k_startup;

var
  h68k_startup: Th68k_startup; public name '_h68k_startup';

procedure PascalMain; external name 'PASCALMAIN';
procedure PascalStart(const startparams: Ph68k_startup); noreturn; forward;

{ this function must be the first in this unit which contains code }
procedure _FPC_proc_start; assembler; nostackframe; noreturn; public name '_start';
asm
  movem.l a0-a5,-(sp)
  move.l sp,a0
  jbsr PascalStart
end;

procedure PascalStart(const startparams: Ph68k_startup); noreturn;
var
  bss_start: pbyte;
begin
  with startparams^ do
    begin
      { clear BSS }
      bss_start:=pbyte(pdword(@mcb[30])^);
      fillchar(bss_start^,bss_end-bss_start,0);
    end;

  h68k_startup:=startparams^;

  PASCALMAIN;
end;

procedure _FPC_proc_halt(_ExitCode: longint); noreturn; public name '_haltproc';
begin
  h68kdos_exit2(_ExitCode);
end;

end.
