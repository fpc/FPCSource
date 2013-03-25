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
  xchg ax, bx
  mov word [bx + Jmp_buf.bx], ax
  mov word [bx + Jmp_buf.si], si
  mov word [bx + Jmp_buf.di], di
  mov word [bx + Jmp_buf.bp], bp
  mov di, sp
  add di, 2
  mov word [bx + Jmp_buf.sp], di
  mov di, word [di - 2]
  mov word [bx + Jmp_buf.pc], di
  mov di, word [bx + Jmp_buf.di]
  mov bx, ax
  xor ax, ax
end;


Procedure fpc_longJmp (Var S : Jmp_buf; value : longint); assembler;nostackframe;[Public, alias : 'FPC_LONGJMP']; compilerproc;
asm
end;
