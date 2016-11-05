{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2016 by the Free Pascal development team.

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

Function fpc_SetJmp (Var S : Jmp_buf) : longint;assembler;stdcall;nostackframe;[Public, alias : 'FPC_SETJMP'];compilerproc;
asm
  // load S to a0
  move.l 4(sp),a0

  // Save nonvolatile registers
{$if defined(amiga)}
  movem.l d2-d7/a2-a4/a6,12(a0)    { amiga uses a5 as fp }
{$else}
  movem.l d2-d7/a2-a5,12(a0)
{$endif}

{$if defined(fpu68881)}
  fmovem.x  fp2-fp7,52(a0)
{$elseif defined(fpucoldfire)}
  fmovem.d  fp2-fp7,52(a0)
{$endif}

  // save return address (PC) and pop S off the stack
  move.l (sp)+,d0
  move.l d0,(sp)
  move.l d0,8(a0)
  // save FP
  move.l fp,(a0)
  // save SP
  move.l sp,d0
  // 4 bytes already popped, 4 to go.
  addq.l #4,d0
  move.l d0,4(a0)

  // return 0
  clr.l d0
end;

Procedure fpc_longJmp (Var S : Jmp_buf; value : longint); assembler;stdcall;nostackframe;[Public, alias : 'FPC_LONGJMP'];compilerproc;
asm
  // load S to a0
  move.l 4(sp),a0
  // load 'value' to d0
  move.l 8(sp),d0
  // don't return zero
  bne @valueok
  moveq.l #1,d0
@valueok:
  // restore FP
  move.l (a0),fp
  // restore SP
  move.l 4(a0),sp
  // jump to PC
  move.l 8(a0),-(sp)

  // Restore registers
{$if defined(amiga)}
  movem.l 12(a0),d2-d7/a2-a4/a6    { amiga uses a5 as fp }
{$else}
  movem.l 12(a0),d2-d7/a2-a5
{$endif}

{$if defined(fpu68881)}
  fmovem.x  52(a0),fp2-fp7
{$elseif defined(fpucoldfire)}
  fmovem.d  52(a0),fp2-fp7
{$endif}
  // new return pc is at (sp)
end;
