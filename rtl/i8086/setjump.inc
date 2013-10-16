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
  mov si, sp

  mov bx, ss:[si + 2 + extra_param_offset]  // S
  mov word [bx + Jmp_buf.bp], bp
  mov ax, word ss:[si]
  mov word [bx + Jmp_buf.ip], ax
{$ifdef FPC_X86_CODE_FAR}
  mov ax, word ss:[si + 2]
  mov word [bx + Jmp_buf.cs], ax
{$endif FPC_X86_CODE_FAR}
  lea ax, [si + 4 + extra_param_offset]
  mov word [bx + Jmp_buf.sp], ax

  xor ax, ax
end;


Procedure fpc_longJmp (Var S : Jmp_buf; value : smallint); assembler;nostackframe;[Public, alias : 'FPC_LONGJMP']; compilerproc;
asm
  mov si, sp

  mov bx, ss:[si + 4 + extra_param_offset]  // S
  mov ax, ss:[si + 2 + extra_param_offset]  // value
  test ax, ax
  jnz @@L1
  inc ax
@@L1:
  mov bp, word [bx + Jmp_buf.bp]
  mov sp, word [bx + Jmp_buf.sp]
  // we should also clear the fpu
  // fninit no must be done elsewhere PM
  // or we should reset the control word also
{$ifdef FPC_X86_CODE_NEAR}
  jmp word [bx + Jmp_buf.ip]
{$else FPC_X86_CODE_NEAR}
  // the inline asm doesn't support jmp far yet, so we use db for now
  // jmp far [bx + Jmp_buf.ip]
  db 0FFh, 06Fh, Jmp_buf.ip
{$endif FPC_X86_CODE_NEAR}
end;
