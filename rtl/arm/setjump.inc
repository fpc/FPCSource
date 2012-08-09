{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];nostackframe; compilerproc;
  asm
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    {$if defined(CPUARMV3) or defined(CPUARMV4) or defined(CPUARMV5)}
    fstmiax r0!, {d8-d15}
    // according to the ARM Developer Suite Assembler Guide Version 1.2
    // fstmiad increases the address register always by 2n+1 words, so fix this
    sub r0,r0,#4
    {$else}
    fstmiad r0!, {d8-d15}
    {$endif}
    {$endif}

    {$if defined(CPUCORTEXM3) or defined(CPUARMV7M)}
    stmia	r0!, {v1-v6, sl, fp}
    mov	   r2, sp
    stmia	r0!, {r2, lr}

    mov     r0,#0
    mov     pc,lr
    {$else}
    stmia   r0,{v1-v6, sl, fp, sp, lr}
    mov     r0,#0
    bx      lr
    {$endif}
  end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; compilerproc;
  asm
    {$if defined(CPUCORTEXM3) or defined(CPUARMV7M)}
    mov     ip, r0
    movs    r0, r1
    it eq
    moveq   r0, #1
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    fldmiad ip!, {d8-d15}
    {$endif}
    ldmia   ip!, {v1-v6, sl, fp}
    ldr     sp, [ip]
    add     ip, ip, #4
    ldr     pc, [ip]
    {$else}
    mov     ip, r0
    movs    r0, r1
    moveq   r0, #1
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    {$if defined(CPUARMV3) or defined(CPUARMV4) or defined(CPUARMV5)}
    fldmiax ip!, {d8-d15}
    // according to the ARM Developer Suite Assembler Guide Version 1.2
    // increases fldmiax the address register always by 2n+1 words, so fix this
    sub ip,ip,#4
    {$else}
    fldmiad ip!, {d8-d15}
    {$endif}
    {$endif}
    ldmia   ip,{v1-v6, sl, fp, sp, pc}
    {$endif}
  end;


