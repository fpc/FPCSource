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
    vstmia r0!, {d8-d15}
    // according to the ARM Developer Suite Assembler Guide Version 1.2
    // fstmiad increases the address register always by 2n+1 words, so fix this
    sub r0,r0,#4
    {$else}
    vstmia r0!, {d8-d15}
    {$endif}
    {$endif}

    {$if defined(CPUTHUMB2)}
    stmia	r0!, {v1-v6, sl, fp}
    mov	        r2, sp
    stmia	r0!, {r2, lr}

    mov     r0,#0
    mov     pc,lr
    {$elseif defined(CPUTHUMB)}
    stmia	r0!,{v1-v4}
    mov         r1,v5
    str         r1,[r0]
    mov         r1,v6
    str         r1,[r0,#4]
    mov         r1,sl
    str         r1,[r0,#8]
    mov         r1,fp
    str         r1,[r0,#12]
    mov         r1,sp
    str         r1,[r0,#16]
    mov         r1,lr
    str         r1,[r0,#20]
    mov         r0,#0
    bx          lr
    {$else}
    stmia   r0,{v1-v6, sl, fp, sp, lr}
    mov     r0,#0
    {$ifdef CPUARM_HAS_BX}
    bx      lr
    {$else}
    mov pc,lr
    {$endif}
    {$endif}
  end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; compilerproc;
  asm
    {$if defined(CPUTHUMB2)}
    mov     ip, r0
    movs    r0, r1
    it eq
    moveq   r0, #1
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    vldmia ip!, {d8-d15}
    {$endif}
    ldmia   ip!, {v1-v6, sl, fp}
    ldr     sp, [ip]
    add     ip, ip, #4
    ldr     pc, [ip]

    {$elseif defined(CPUTHUMB)}
    mov     r3, r0
    movs    r0, r1
    bne     .L1
    mov     r0, #1
.L1:
    ldmia   r3!,{v1-v4}
    ldr     r2,[r3]
    mov     v5,r2
    ldr     r2,[r3,#4]
    mov     v6,r2
    ldr     r2,[r3,#8]
    mov     sl,r2
    ldr     r2,[r3,#12]
    mov     fp,r2
    ldr     r2,[r3,#16]
    mov     sp,r2
    ldr     r2,[r3,#20]
    mov     pc,r2

    {$else}
    mov     ip, r0
    movs    r0, r1
    moveq   r0, #1
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    {$if defined(CPUARMV3) or defined(CPUARMV4) or defined(CPUARMV5)}
    vldmia ip!, {d8-d15}
    // according to the ARM Developer Suite Assembler Guide Version 1.2
    // increases fldmiax the address register always by 2n+1 words, so fix this
    sub ip,ip,#4
    {$else}
    vldmia ip!, {d8-d15}
    {$endif}
    {$endif}
    ldmia   ip,{v1-v6, sl, fp, sp, pc}
    {$endif}
  end;


