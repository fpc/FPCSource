{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
           Set_Jmp/Long_jmp
 **********************************************************************}

{$warning Fix register handling in case of nostackframe }

Function fpc_SetJmp (Var S : Jmp_buf) : longint;assembler;nostackframe;[Public, alias : 'FPC_SETJMP'];compilerproc;
asm
  // Temporarily store a0 into d0
  move.l  a0,d0
  // load S to a0
  move.l 4(sp), a0
  // save a0 (now in d0) to offset 40
  move.l d0, 40(a0)
  // save return address (PC) and pop S off the stack
  move.l (sp)+, d0
  move.l d0,(sp)
  move.l d0, 8(a0)
  // save FP
{$if defined(amiga)}
  move.l a5, (a0)
{$else}
  move.l a6, (a0)
{$endif}
  // save SP
  move.l sp, d0
  // 4 bytes already popped, 4 to go.
  addq.l #4, d0
  move.l d0, 4(a0)
  // save a1
  move.l a1,d0
  move.l d0,44(a0)
  // save a2
  move.l a2,d0
  move.l d0,48(a0)
  // save a3
  move.l a3,d0
  move.l d0,52(a0)
  // save a4
  move.l a4,d0
  move.l d0,56(a0)
  // save a5
  move.l a5,d0
  move.l d0,60(a0)
  // save d1..d7
  move.l d1, 12(a0)
  move.l d2, 16(a0)
  move.l d3, 20(a0)
  move.l d4, 24(a0)
  move.l d5, 28(a0)
  move.l d6, 32(a0)
  move.l d7, 36(a0)
  // restore a0
  move.l 40(a0), a0

  // return 0
  clr.l d0
end;

Procedure fpc_longJmp (Var S : Jmp_buf; value : longint); assembler;nostackframe;[Public, alias : 'FPC_LONGJMP'];compilerproc;
asm
  // load S to a0
  move.l 4(sp),a0
  // Restore address registers
  // restore a1
  move.l 44(a0),d0
  move.l d0,a1
  // restore a2
  move.l 48(a0),d0
  move.l d0,a2
  // restore a3
  move.l 52(a0),d0
  move.l d0,a3
  // restore a4
  move.l 56(a0),d0
  move.l d0,a4
  // restore a5
  move.l 60(a0),d0
  move.l d0,a5
  // restore d1..d7
  move.l 12(a0),d1
  move.l 16(a0),d2
  move.l 20(a0),d3
  move.l 24(a0),d4
  move.l 28(a0),d5
  move.l 32(a0),d6
  move.l 36(a0),d7

  // load value to d0
  move.l 8(sp),d0
  // Save temporarily into d1 slot
  move.l d0,12(a0)
  // restore FP
{$if defined(amiga)}
  move.l (a0), a5
{$else}
  move.l (a0), a6
{$endif}
  // restore SP
  move.l 4(a0), sp
  // jump to PC
  move.l 8(a0),d0
  move.l d0,-(sp)
  // restore a0
  move.l 40(a0),d0
  move.l d0,-(sp)
  // restore return value, now at 12(a0)
  move.l 12(a0),d0
  // restore a0 from stack
  move.l (sp)+,a0
  // new return pc is at (sp), so we can call rts
  rts
end;
