{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function fpc_setjmp(var S : jmp_buf) : shortint;assembler;[Public, alias : 'FPC_SETJMP'];nostackframe;compilerproc;
  asm
    push ix
    ld ix, 0
    add ix,sp

    ld l, (ix+4)  { (S)   }
    ld h, (ix+5)  { (S+1) }

    { save caller ix }
    ld c, (ix)
    ld b, (ix+1)
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl

    { save caller sp (i.e. what its value was, right before the call instrunction) }
    ld iy, 4
    add iy, sp
    push iy
    pop bc
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl

    { save ret address }
    ld c, (ix+2)
    ld b, (ix+3)
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl

    ld l, 0

    pop ix
  end;


procedure fpc_longjmp(var S : jmp_buf;value : shortint);assembler;[Public, alias : 'FPC_LONGJMP'];nostackframe;compilerproc;
  asm
    push ix
    ld ix, 0
    add ix, sp

    ld d, (ix+6)  { (value) }

    ld l, (ix+4)  { (S)   }
    ld h, (ix+5)  { (S+1) }

    { restore ix }
    ld c, (hl)
    inc hl
    ld b, (hl)
    inc hl
    push bc
    pop ix

    { restore sp }
    ld c, (hl)
    inc hl
    ld b, (hl)
    inc hl
    push bc
    pop iy
    ld sp, iy

    { restore pc }
    ld c, (hl)
    inc hl
    ld b, (hl)
    inc hl
    { prepare the new return address, will be popped by the ret instruction }
    push bc

    { return result }
    ld l, d
  end;


