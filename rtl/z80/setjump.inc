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

    ld iy, 0
    add iy, sp
    push iy
    pop bc
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
    ld c, (hl)
    inc hl
    ld b, (hl)
    inc hl
    push bc
    pop iy
    ld sp, iy

    ld l, d

    pop ix
  end;


