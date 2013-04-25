{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function fpc_SetJmp (Var S : Jmp_buf) : smallint;assembler;nostackframe;[Public, alias : 'FPC_SETJMP']; compilerproc;
asm
  mov ax, bp
  mov di, sp
  push bp
  mov bp, sp

  mov bx, ss:[bp + 4]
  mov word [bx + Jmp_buf.bp], ax
  mov word [bx + Jmp_buf.sp], di
  mov di, word ss:[di]
  mov word [bx + Jmp_buf.pc], di

  xor ax, ax
  pop bp
end;


Procedure fpc_longJmp (Var S : Jmp_buf; value : smallint); assembler;nostackframe;[Public, alias : 'FPC_LONGJMP']; compilerproc;
asm
  push bp
  mov bp, sp

  mov bx, ss:[bp + 6]
  mov ax, ss:[bp + 4]
  test ax, ax
  jnz @@L1
  inc ax
@@L1:
  mov dx, word [bx + Jmp_buf.pc]
  mov bp, word [bx + Jmp_buf.bp]
  mov sp, word [bx + Jmp_buf.sp]
  // we should also clear the fpu
  // fninit no must be done elsewhere PM
  // or we should reset the control word also
  jmp dx
end;
