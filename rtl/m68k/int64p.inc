{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifndef FPC_SYSTEM_HAS_MUL_LONGINT_TO_INT64}
{$define FPC_SYSTEM_HAS_MUL_LONGINT_TO_INT64}
function fpc_mul_longint_to_int64(f1,f2 : longint) : int64;[public,alias: 'FPC_MUL_LONGINT_TO_INT64']; assembler; nostackframe; compilerproc;
asm
    tst.l    d0
    beq.s    @xit0
    tst.l    d1
    beq.s    @xit0

    movem.l  d2-d5,-(sp)
    move.l   d0,d5

    eor.l    d1,d5
    move.l   d0,d2
    bpl.s    @pos0

    neg.l    d2
    neg.l    d0

@pos0:
    move.l   d1,d4
    bpl.s    @pos1

    neg.l    d4
    neg.l    d1

@pos1:
    move.l   d0,d3
    swap     d4
    swap     d3

    mulu.w   d1,d0
    mulu.w   d3,d1
    mulu.w   d4,d2
    mulu.w   d4,d3

    swap     d0
    clr.l    d4
    add.w    d1,d0
    addx.l   d4,d3
    add.w    d2,d0
    addx.l   d4,d3

    clr.w    d1
    clr.w    d2
    swap     d1
    swap     d2

    add.l    d3,d1
    swap     d0
    add.l    d2,d1

    tst.l    d5
    bpl.s    @xit

    neg.l    d0
    negx.l   d1

@xit:
    movem.l  (sp)+,d2-d5
    exg.l    d0,d1
    rts

@xit0:
    clr.l    d0
    clr.l    d1
end;
{$endif FPC_SYSTEM_HAS_MUL_LONGINT_TO_INT64}
