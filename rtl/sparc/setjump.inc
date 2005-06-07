{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{#define ENV(base,reg) [%base + (reg * 4)]
#define ST_FLUSH_WINDOWS 3
#define RW_FP [%fp + 0x48]
}

procedure longjmp(var s : jmp_buf;value:longint);assembler;nostackframe;[Public,alias:'FPC_LONGJMP'];
  asm
    // Store our arguments in global registers so we can still
    // use them while unwinding frames and their register windows.

    ld [%o0+4], %g3         // Cache target FP in register %g3.
    mov %o0, %g1            // s in %g1
    orcc %o1, %g0, %g2      // value in %g2
    be,a .L0                // Branch if zero; else skip delay slot.
    mov 1, %g2              // Delay slot only hit if zero: VAL = 1.
.L0:
    xor %fp, %g3, %o0
    add %fp, 512, %o1
    andncc %o0, 4095, %o0
    bne .Lthread
    cmp %o1, %g3
    bl .Lthread

    // Now we will loop, unwinding the register windows up the stack
    // until the restored %fp value matches the target value in %g3.

.Lloop:
    cmp %fp, %g3            // Have we reached the target frame?
    bl,a .Lloop             // Loop while current fp is below target.
    restore                 // Unwind register window in delay slot.
    be,a .Lfound            // Better have hit it exactly.
    ld [%g1], %o0           // Delay slot: extract target SP.

.Lthread:
    {
     * Do a "flush register windows trap".  The trap handler in the
     * kernel writes all the register windows to their stack slots, and
     * marks them all as invalid (needing to be sucked up from the
     * stack when used).  This ensures that all information needed to
     * unwind to these callers is in memory, not in the register
     * windows.
    }

    ta      3
    mov     %g1,%o1         // use %o1, since %g1 will be destroyed by the call below

    ld      [%o1], %fp      // Set saved SP on restore below.
    sub     %fp, 64, %sp    // Allocate a register frame.
    st      %g3, [%fp+48]   // Set saved FP on restore below.

    ld      [%o1+8], %o7    // Set return PC.

    retl
    restore %g2, 0, %o0     // Restore values from above register frame.

.Lfound:
    // We have unwound register windows so %fp matches the target.
    mov %o0, %sp            // OK, install new SP.

.Lsp_ok:
    ld [%g1+8], %o0         // Extract target return PC.
    jmp %o0+8               // Return there.
    mov %g2, %o0            // Delay slot: set return value.
  end;


function setjmp(var S:jmp_buf):longint;assembler;nostackframe;[Public,alias:'FPC_SETJMP'];
  asm
    // We don't create a stackframe so we can save PC,SP and FP of the caller
    st      %o7, [%o0+8]
    st      %sp, [%o0]
    st      %fp, [%o0+4]

    ld      [%o0+8], %o7
    mov     %g0, %o0
  end;

