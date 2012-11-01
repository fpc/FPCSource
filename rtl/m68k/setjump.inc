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

Function fpc_SetJmp (Var S : Jmp_buf) : longint;assembler;nostackframe;[Public, alias : 'FPC_SETJMP'];compilerproc;
asm
  // load S to a0
  move.l 4(sp), a0
  // save return address (PC)
  move.l (sp), d0
  move.l d0, 8(a0)
  // save FP
  move.l a6, (a0)
  // save SP
  move.l sp, d0
  addq.l #8, d0
  move.l d0, 4(a0)
  // return 0
  clr.l d0
end;

Procedure fpc_longJmp (Var S : Jmp_buf; value : longint); assembler;nostackframe;[Public, alias : 'FPC_LONGJMP'];compilerproc;
asm
  // load S to a0
  move.l 4(sp),a0
  // load value to d0
  move.l 8(sp),d0
  // restore FP
  move.l (a0), a6
  // restore SP
  move.l 4(a0), sp
  // jump to PC
  move.l 8(a0),a0
  jmp (a0)
end;
