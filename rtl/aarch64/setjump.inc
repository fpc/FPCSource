{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp/Longjmp implementation for AArch64

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


 **********************************************************************}

function fpc_setjmp(var S: jmp_buf): longint; assembler; [public, alias:'FPC_SETJMP']; nostackframe; compilerproc;
  asm
    mov x3, sp
    stp x19, x20, [x0, #jmp_buf.x19]
    stp x21, x22, [x0, #jmp_buf.x21]
    stp x23, x24, [x0, #jmp_buf.x23]
    stp x25, x26, [x0, #jmp_buf.x25]
    stp x27, x28, [x0, #jmp_buf.x27]
    stp x29, x30, [x0, #jmp_buf.x29]
    str x3,       [x0, #jmp_buf.xsp]

    stp d8,  d9,  [x0, #jmp_buf.d8]
    stp d10, d11, [x0, #jmp_buf.d10]
    stp d12, d13, [x0, #jmp_buf.d12]
    stp d14, d15, [x0, #jmp_buf.d14]

    mov x0, xzr
  end;


procedure fpc_longjmp(var S: jmp_buf ;value: longint); assembler; [public, alias:'FPC_LONGJMP']; nostackframe; compilerproc;
  asm
    ldr x3,       [x0, #jmp_buf.xsp]

    ldp x19, x20, [x0, #jmp_buf.x19]
    ldp x21, x22, [x0, #jmp_buf.x21]
    ldp x23, x24, [x0, #jmp_buf.x23]
    ldp x25, x26, [x0, #jmp_buf.x25]
    ldp x27, x28, [x0, #jmp_buf.x27]
    ldp x29, x30, [x0, #jmp_buf.x29]

    mov sp, x3

    ldp d8,  d9,  [x0, #jmp_buf.d8]
    ldp d10, d11, [x0, #jmp_buf.d10]
    ldp d12, d13, [x0, #jmp_buf.d12]
    ldp d14, d15, [x0, #jmp_buf.d14]

    mov x0, x1
  end;


