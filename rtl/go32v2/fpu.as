/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
/* Translated from tasm to GAS by C. Sandmann */
/* One comment displaced to get it compiled by as.exe directly  !!! */
/* by Pierre Muller */

/* This routine assumes DS == SS since [ESI] coding shorter than [EBP] coding */

        .global __detect_80387          /* direct from the Intel manual */
__detect_80387:                         /* returns 1 if 387 (or more), else 0 */
        pushl   %esi
        pushl   %eax                    /* Dummy work area on stack */
        movl    %esp,%esi
        fninit
        movw    $0x5a5a,(%esi)
        fnstsw  (%esi)
        cmpb    $0,(%esi)
        jne     Lno_387

        fnstcw  (%esi)
        movl    (%esi),%eax             /* Only ax significant */
        andl    $0x103f,%eax
        cmpl    $0x3f,%eax
        jne     Lno_387

        fld1
        fldz
/*      fdiv                               GAS encodes this as 0xdcf1 !! BUG */
        .byte   0xde,0xf9
        fld     %st
        fchs
        fcompp
        fstsw   (%esi)
        movzwl  (%esi),%eax             /* Clears upper %eax */
        sahf
        je      Lno_387
        fninit                          /* 387 present, initialize. */
        fnstcw  (%esi)
        wait
        andw    $0x0fffa,(%esi)
/* enable invalid operation exception */
        fldcw   (%esi)
        movw    $1,%ax
        jmp     Lexit
Lno_387:
        xorl    %eax,%eax
Lexit:
        popl    %esi                    /* Fix stack first */
        popl    %esi
        ret
