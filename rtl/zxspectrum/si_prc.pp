{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by Free Pascal development team

    This file contains startup code for the ZX Spectrum

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

{$SMARTLINK OFF}

interface

implementation

{$GOTO ON}

var
  FPC_SAVE_IY: word; external name 'FPC_SAVE_IY';
  fpc_stackarea_start: word; external name '__fpc_stackarea_start';
  fpc_stackarea_end: word; external name '__fpc_stackarea_end';

procedure PascalMain; external name 'PASCALMAIN';

{ this *must* always remain the first procedure with code in this unit }
procedure _start; assembler; nostackframe; public name 'start';
label
  bstart,bend,loop;
asm
    { init the stack }
    ld sp, offset fpc_stackarea_end

    { zero the .bss section }
    ld bc, offset bstart
    ld hl, offset bend
    scf
    ccf
    sbc hl, bc
    ld a, 0
loop:
    ld (bc), a
    inc bc
    dec hl
    cp a, l
    jr NZ, loop
    cp a, h
    jr NZ, loop

    { save IY (must be done after zeroing the .bss section) }
    ld (FPC_SAVE_IY), iy

    { ready to run the main program }
    jp PASCALMAIN

    { When using the SDCC-SDLDZ80 linker, the first object module defines the
      order of areas (sections). Since this module contains the startup code,
      it is loaded first, so we define all the sections we use in the proper
      order. }
    area '_DATA'
    area '_BSS'
bstart:
    area '_BSSEND'
bend:
    area '_HEAP'
    area '_STACK'
    area '_CODE'
end;

end.
