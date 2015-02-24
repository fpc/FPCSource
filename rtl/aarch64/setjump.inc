{
    This file is part of the Free Pascal run time library.

    * Copyright (c) 2014 The NetBSD Foundation, Inc.
    * All rights reserved.
    *
    * This code is derived from software contributed to The NetBSD Foundation
    * by Matt Thomas of 3am Software Foundry.
    *
    * Redistribution and use in source and binary forms, with or without
    * modification, are permitted provided that the following conditions
    * are met:
    * 1. Redistributions of source code must retain the above copyright
    *    notice, this list of conditions and the following disclaimer.
    * 2. Redistributions in binary form must reproduce the above copyright
    *    notice, this list of conditions and the following disclaimer in the
    *    documentation and/or other materials provided with the distribution.
    *
    * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
    * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
    * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    * POSSIBILITY OF SUCH DAMAGE.

 **********************************************************************}

{ modified from original NetBSD version by removing the sigprocmask related
  code }

const
  _JB_MAGIC_AARCH64_SETJMP: int64 = $1234434576533430;

function fpc_setjmp(var S:jmp_buf):longint;assembler;[public,alias:'FPC_SETJMP'];nostackframe;compilerproc;
  asm
    adrp x7, _JB_MAGIC_AARCH64_SETJMP@gotpage
    ldr x7, [x7, _JB_MAGIC_AARCH64_SETJMP@gotpageoff]
    ldr x7, [x7]

    mov x3, sp
    stp x7, x3,  [x0, #jmp_buf.magic]

    stp x19, x20, [x0, #jmp_buf.x19]
    stp x21, x22, [x0, #jmp_buf.x21]
    stp x23, x24, [x0, #jmp_buf.x23]
    stp x25, x26, [x0, #jmp_buf.x25]
    stp x27, x28, [x0, #jmp_buf.x27]
    stp x29, x30, [x0, #jmp_buf.x29]

    mrs x5, tpidr_el0
    str x5, [x0, #jmp_buf.tpidr]

    stp d8,  d9,  [x0, #jmp_buf.d8]
    stp d10, d11, [x0, #jmp_buf.d10]
    stp d12, d13, [x0, #jmp_buf.d12]
    stp d14, d15, [x0, #jmp_buf.d14]

    mov x0, xzr
  end;


procedure fpc_setjmp_error;
  begin
    { better option than 216? }
    HandleErrorAddrFrameInd(216,get_pc_addr,get_frame);
  end;


procedure fpc_longjmp(var S:jmp_buf;value:longint);assembler;[public,alias:'FPC_LONGJMP'];nostackframe;compilerproc;
  asm
    adrp x7, _JB_MAGIC_AARCH64_SETJMP@gotpage
    ldr x7, [x7, _JB_MAGIC_AARCH64_SETJMP@gotpageoff]
    ldr x7, [x7]
    ldp x2, x3, [x0, #jmp_buf.magic]
    cmp x2, x7
    b.ne        .Lbotch

    ldp x4,  x5,  [x0, #jmp_buf.x29]
    cbz x3, .Lbotch
    cbz x4, .Lbotch
    cbz x5, .Lbotch

    mov sp, x3
    mov x29, x4
    mov x30, x5

    ldp x19, x20, [x0, #jmp_buf.x19]
    ldp x21, x22, [x0, #jmp_buf.x21]
    ldp x23, x24, [x0, #jmp_buf.x23]
    ldp x25, x26, [x0, #jmp_buf.x25]
    ldp x27, x28, [x0, #jmp_buf.x27]

    ldr x5, [x0, #jmp_buf.tpidr]
    msr tpidr_el0, x5

    ldp d8,  d9,  [x0, #jmp_buf.d8]
    ldp d10, d11, [x0, #jmp_buf.d10]
    ldp d12, d13, [x0, #jmp_buf.d12]
    ldp d14, d15, [x0, #jmp_buf.d14]

    mov x0, x1
    ret

    { validation failed, die die die. }
  .Lbotch:
    { build stack frame }
    stp x29, x30, [sp, #-16]
    bl fpc_setjmp_error
    b .Lbotch
  end;


