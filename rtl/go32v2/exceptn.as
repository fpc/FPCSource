/* Copyright (C) 1994, 1995 Charles Sandmann (sandmann@clio.rice.edu)
 * This file maybe freely distributed and modified as long as copyright remains.
 */

  EAX = 0
  EBX = 4
  ECX = 8
  EDX = 12
  ESI = 16
  EDI = 20
  EBP = 24
  ESP = 28
  EIP = 32
  EFLAGS = 36
  CS = 40
  DS = 42
  ES = 44
  FS = 46
  GS = 48
  SS = 50
  ERRCODE = 52
  EXCEPNO = 56
  PREVEXC = 60
/* Length 64 bytes plus non-used FPU */
        .data
        .balign 8
        .comm   exception_stack, 8000

        .text
        .balign 16,,7
   .macro EXCEPTION_ENTRY number
        pushl   \number
        jmp     exception_handler
   .endm

        .global ___djgpp_exception_table
___djgpp_exception_table:
EXCEPTION_ENTRY $18
EXCEPTION_ENTRY $19
EXCEPTION_ENTRY $0
EXCEPTION_ENTRY $1
EXCEPTION_ENTRY $2
EXCEPTION_ENTRY $3
EXCEPTION_ENTRY $4
EXCEPTION_ENTRY $5
EXCEPTION_ENTRY $6
EXCEPTION_ENTRY $7
EXCEPTION_ENTRY $8
EXCEPTION_ENTRY $9
EXCEPTION_ENTRY $10
EXCEPTION_ENTRY $11
EXCEPTION_ENTRY $12
EXCEPTION_ENTRY $13
EXCEPTION_ENTRY $14
EXCEPTION_ENTRY $15
EXCEPTION_ENTRY $16
EXCEPTION_ENTRY $17

/*      This code is called any time an exception occurs in the 32 bit protected
;*      mode code.  The exception number is pushed on the stack.  This is called
;*      on a locked stack with interrupts disabled.  Don't try to terminate.
;*
;*      [   *   |   SS  ]       * Don't modify
;*      [      ESP      ]
;*      [    EFLAGS     ]
;*      [   *   |   CS  ]       * Don't modify
;*      [      EIP      ]
;*      [   ERR CODE    ]
;*      [   *   |RET CS*]       * Don't modify
;*      [   RET EIP*    ]       * Don't modify
;*      [  EXCEPTION #  ]       (And later EBP)
;*/
/* ;WARNING WARNING WARNING
   ;The mechanism for passing signals between the debugger
   ;and the debuggee relies on the *exact* instructions between
   ;EXCEPTION_ENTRY($13) above and "cmpb $0, forced" instruction
   ;below!  These instructions are stored in forced_test[] buffer
   ;in src/debug/common/dbgcom.c.  Do NOT change anything between
   ;these two instructions, or you will break signal support in
   ;the debuggers!!  */
exception_handler:
        pushl   %ebx
        pushl   %ds
        .byte   0x2e                            /* CS: */
        cmpb    $0, forced
        je      not_forced
        call    limitFix
        .byte   0x2e                            /* CS: */
        movzbl  forced,%ebx
        movl    %ebx,8(%esp)                    /* replace EXCEPNO */
        cmpb    $0x75, %bl
        jne     not_forced
        movzwl    ___djgpp_fpu_state,%ebx
        movl    %ebx,20(%esp)                   /* set ERRCODE to FPU state */
not_forced:
        movw    %cs:___djgpp_our_DS, %ds
        movl    $0x10000, forced                /* its zero now, flag inuse */
        movl    $exception_state, %ebx
        popl    DS(%ebx)
        popl    EBX(%ebx)
        popl    EXCEPNO(%ebx)
        movl    %esi, ESI(%ebx)
        movl    %edi, EDI(%ebx)
        movl    %ebp, EBP(%ebx)
        movl    %eax, EAX(%ebx)
        movl    %ecx, ECX(%ebx)
        movl    %edx, EDX(%ebx)
        movw    %es, ES(%ebx)
        movw    %fs, FS(%ebx)
        movw    %gs, GS(%ebx)
        movl    ___djgpp_exception_state_ptr, %eax
        movl    %eax, PREVEXC(%ebx)

/* Stack clean at this point, DS:[EBX] points to exception_state, all
   register information saved.  Now get the info on stack. */

        pushl   %ebp
        movl    %esp, %ebp      /* load ebp with stack for easy access */

        movl    12(%ebp), %eax
        movl    %eax, ERRCODE(%ebx)
        movl    16(%ebp), %eax
        movl    %eax, EIP(%ebx)
        movl    20(%ebp), %eax
        movw    %ax, CS(%ebx)
        movl    24(%ebp), %eax
        movl    %eax, EFLAGS(%ebx)
        andb    $0xfe, %ah                      /* Clear trace flag */
        movl    %eax, 24(%ebp)                  /* and restore on stack */

        movl    28(%ebp), %eax
        movl    %eax, ESP(%ebx)
        movl    32(%ebp), %eax
        movw    %ax, SS(%ebx)

        movl    $dpmi_exception_proc1, 16(%ebp)         /* where to return */
        movw    %cs, 20(%ebp)

/* Change to our local stack on return from exception (maybe stack exception) */
        movw    %ds, %ax
        cmpb    $12,EXCEPNO(%ebx)               /* Stack fault ? */
        je      1f
        cmpw    %ax,32(%ebp)
        je      stack_ok
        .byte   0x2e                            /* CS: */
        movw    ___djgpp_ds_alias,%di
        cmpw    %di,32(%ebp)    /* if it's DS alias, switch to normal DS */
        jne     1f
        movw    %ax,32(%ebp)
        jmp     stack_ok
1:      movl    $exception_stack+8000, 28(%ebp)
        movw    %ax, 32(%ebp)
stack_ok:
/* Now copy the exception structure to the new stack before returning */
        movw    %ax, %es
        movl    %ebx,%esi
        movl    28(%ebp), %edi
        subl    $92, %edi                       /* 64 plus extra for longjmp */
        movl    %edi, 28(%ebp)
        movl    %edi, ___djgpp_exception_state_ptr
        movl    $16, %ecx
        cld
        rep
        movsl

        movl    EAX(%ebx), %eax                         /* restore regs */
        movl    ESI(%ebx), %esi
        movl    EDI(%ebx), %edi
        movl    ECX(%ebx), %ecx
        movw    ES(%ebx), %es
        popl    %ebp
        pushl   EBX(%ebx)
        pushl   DS(%ebx)
        movb    $0, forced+2                            /* flag non-use */
        popl    %ds
        popl    %ebx
        lret

/* Code to fix fake exception, EBX destroyed.  Note, app_DS may == our_DS! */
        .balign 16,,7
limitFix:
        pushl   %eax
        pushl   %ecx
        pushl   %edx
        .byte   0x2e                            /* CS: */
        movl    ___djgpp_app_DS, %ebx           /* avoid size prefix */
        .byte   0x2e                            /* CS: */
        movl    ds_limit, %edx
        movl    %edx, %ecx
        shrl    $16, %ecx
        movw    $0x0008, %ax
        int     $0x31                           /* Set segment limit */
        popl    %edx
        popl    %ecx
        popl    %eax
        ret

/* This local routine preprocesses a return request to the C code.  It checks
   to make sure the DS & SS are set OK for C code.  If not, it sets them up */
        .balign 16,,7
dpmi_exception_proc1:
        cld
        .byte   0x2e                            /* CS: !!! */
        movw    ___djgpp_our_DS, %bx            /* to be sure */
        movw    %bx, %ds
        movw    %bx, %es
        /* Note: SS:ESP should be set properly by exception routine */
        jmp     ___djgpp_exception_processor

/*      This code is called by a user routine wishing to save an interrupt
;*      state.  It will return with a clean stack, our DS,ES,SS.
;*      Minor bug: uses static exception_state for a short window without
;*      interrupts guaranteed disabled.
;*
;*      [    EFLAGS     ]
;*      [   *   |   CS  ]
;*      [      EIP      ]
;*      [  CALLING EIP  ]
;*/

        .balign 16,,7
        .globl  ___djgpp_save_interrupt_regs
___djgpp_save_interrupt_regs:
        pushl   %esi
        pushl   %ds
        movw    %cs:___djgpp_our_DS, %ds
        movl    $exception_state, %esi
        popl    DS(%esi)                /* Trashes ES but OK */
        popl    ESI(%esi)
        movl    %edi, EDI(%esi)
        movl    %ebp, EBP(%esi)
        movl    %eax, EAX(%esi)
        movl    %ebx, EBX(%esi)
        movl    %ecx, ECX(%esi)
        movl    %edx, EDX(%esi)
        popl    %edx                    /* Save calling EIP */
        popl    EIP(%esi)
        popl    %eax
        movw    %ax,CS(%esi)            /* Don't pop, nukes DS */
        popl    EFLAGS(%esi)
        movl    %esp, ESP(%esi)
        movw    %es, ES(%esi)
        movw    %fs, FS(%esi)
        movw    %gs, GS(%esi)
        movw    %ss, SS(%esi)
        movl    ___djgpp_exception_state_ptr, %eax
        movl    %eax, PREVEXC(%esi)
        cld
        movw    %ds, %ax
        movw    %ax, %es
        movw    %ss, %bx
        cmpw    %ax, %bx                        /* is SS = DS ? */
        je      Lss_ok
        movw    %ax, %ss                        /* set new SS:ESP */
        movl    $exception_stack+8000, %esp
Lss_ok: subl    $92, %esp               /* 64 plus extra for longjmp */
        movl    %esp, %edi
        movl    $16, %ecx
        movl    %edi, ___djgpp_exception_state_ptr
        rep
        movsl                                   /* Copy structure to stack */
        jmp     *%edx                           /* A "return" */

        .balign 8               /* We will touch this; it must be locked */
        .global ___djgpp_hw_lock_start
___djgpp_hw_lock_start:
        /* src/debug/common/dbgcom.c knows that `ds_limit' is stored
           4 bytes before `forced' and relies on that.  Do NOT change that! */
ds_limit:                       .long   0
forced:                         .long   0
        .global ___djgpp_cbrk_count
___djgpp_cbrk_count:            .long   0
        .global ___djgpp_timer_countdown
___djgpp_timer_countdown:       .long   0
        .global ___djgpp_our_DS
___djgpp_our_DS:                .word   0
        .global ___djgpp_app_DS
___djgpp_app_DS:                .word   0
        .global ___djgpp_dos_sel
___djgpp_dos_sel:               .word   0
        .global ___djgpp_hwint_flags
___djgpp_hwint_flags:           .word   0
        .global ___djgpp_sigint_key
___djgpp_sigint_key:            .word   0       /* scan code and kb status */
        .global ___djgpp_sigint_mask
___djgpp_sigint_mask:           .word   0       /* kb status mask */
        .global ___djgpp_sigquit_key
___djgpp_sigquit_key:           .word   0
        .global ___djgpp_sigquit_mask
___djgpp_sigquit_mask:          .word   0
        .global ___djgpp_old_kbd
___djgpp_old_kbd:               .long   0,0
        .global ___djgpp_old_timer
___djgpp_old_timer:             .long   0,0
        .global ___djgpp_exception_state_ptr
___djgpp_exception_state_ptr:   .long   0
exception_state:                .space  64
        .global ___djgpp_ds_alias
___djgpp_ds_alias:              .word   0       /* used in dpmi/api/d0303.s (alloc rmcb) */

        .global ___djgpp_fpu_state
___djgpp_fpu_state:             .word   0
        .balign 16,,7
        .global ___djgpp_npx_hdlr
___djgpp_npx_hdlr:
        fnstsw  ___djgpp_fpu_state
        fnclex
        pushl   %eax
        xorl    %eax,%eax
        outb    %al,$0x0f0
        movb    $0x20,%al
        outb    %al,$0x0a0
        outb    %al,$0x020
        movb    $0x75,%al
hw_to_excp:
        call    ___djgpp_hw_exception
        popl    %eax
        sti
        iret

        .balign 16,,7
        .global ___djgpp_kbd_hdlr
___djgpp_kbd_hdlr:
        pushl   %eax
        pushl   %ebx
        pushl   %ds
        .byte   0x2e                            /* CS: */
        testb   $1, ___djgpp_hwint_flags        /* Disable? */
        jne     Lkbd_chain
        movw    %cs:___djgpp_dos_sel, %ds       /* Conventional mem selector */
/*      movw    $0x7021,0xb0f00         */      /* Test code - write to mono */
/* Check Keyboard status bits */
        movb    0x417,%ah                       /* Get KB status byte */
        testb   $1,%ah
        je      6f
        orb     $2,%ah  /* If RShift is set, set LShift as well */
6:
        inb     $0x60,%al                       /* Read the scan code */
99:
        movb    %ah,%bh                         /* Save KB status */
        andb    %cs:___djgpp_sigint_mask, %ah   /* Mask off irrelevant bits */
        cmpw    %cs:___djgpp_sigint_key, %ax    /* Test for SIGINT */
        jne     7f
        movb    $0x79,%bh                       /* SIGINT */
        jmp     98f
7:
        movb    %bh,%ah                         /* Restore KB status */
        andb    %cs:___djgpp_sigquit_mask, %ah  /* Mask off irrelevant bits */
        cmpw    %cs:___djgpp_sigquit_key, %ax   /* Test for SIGQUIT*/
        jne     Lkbd_chain
        movb    $0x7a,%bh                       /* SIGQUIT */
/* Clear interrupt, (later: remove byte from controller?)
        movb    $0x20,%al
        outb    %al,$0x020      */
98:
        movb    %bh,%al
        call    ___djgpp_hw_exception
Lkbd_chain:
        popl    %ds
        popl    %ebx
        popl    %eax
        ljmp    %cs:___djgpp_old_kbd

        .balign 16,,7
        .global ___djgpp_kbd_hdlr_pc98
___djgpp_kbd_hdlr_pc98:
        pushl   %eax
        pushl   %ebx
        pushl   %ds
        .byte   0x2e                            /* CS: */
        testb   $1, ___djgpp_hwint_flags        /* Disable? */
        jne     Lkbd_chain
/* Check CTRL state */
        movw    %cs:___djgpp_dos_sel, %ds       /* Conventional mem selector */
        movb    0x053a,%al                      /* Get KB status byte */
        /* Convert PC98 style status byte to PC/AT style */
        movb    %al,%ah
        andb    $0x09,%ah       /* GRPH(=ALT), SHIFT(=R-Shift) */
        testb   $0x02,%al
        je      981f
        orb     $0x40,%ah       /* CAPS(=Caps Lock) */
981:    testb   $0x10,%al
        je      982f
        orb     $0x04,%ah       /* CTRL(=Ctrl) */
982:    testb   $0x01,%al
        je      983f
        orb     $0x02,%ah       /* SHIFT(=L-Shift) */
983:    testb   $0x04,%al
        je      984f
        orb     $0x20,%ah       /* KANA(=NUM Lock) */
984:    inb     $0x41,%al                       /* Read the scan code */
        jmp     99b

        .balign 16,,7
        .global ___djgpp_timer_hdlr
___djgpp_timer_hdlr:
        .byte   0x2e                            /* CS: */
        cmpl    $0,___djgpp_timer_countdown
        je      4f
        pushl   %ds
        movw    %cs:___djgpp_ds_alias, %ds
        decl    ___djgpp_timer_countdown
        popl    %ds
        jmp     3f
4:
        pushl   %eax
        movb    $0x78,%al
        call    ___djgpp_hw_exception
        popl    %eax
3:
        .byte   0x2e                            /* CS: */
        testb   $4, ___djgpp_hwint_flags        /* IRET or chain? */
        jne     2f
        ljmp    %cs:___djgpp_old_timer
2:
        pushl   %eax
        movb    $0x20,%al                       /* EOI the interrupt */
        outb    %al,$0x020
        popl    %eax
        iret

        /* On entry ES is the DS alias selector */
        .balign 16,,7
        .global ___djgpp_cbrk_hdlr              /* A RMCB handler for 0x1b */
___djgpp_cbrk_hdlr:
        cld
        lodsl                                   /* EAX = DS:[esi] CS:IP */
        movl    %eax, %es:0x2a(%edi)            /* store in structure */
        lodsl                                   /* AX = FLAGS */
        movw    %ax, %es:0x20(%edi)
        addw    $6, %es:0x2e(%edi)              /* Adjust RM SP */
        movb    $0x1b,%al

        .byte   0x2e                            /* CS: */
        testb   $2, ___djgpp_hwint_flags        /* Count, don't kill */
        jne     1f

        call    ___djgpp_hw_exception
        iret
1:
        incl    %es:___djgpp_cbrk_count
        iret

        .global ___djgpp_i24                    /* Int 24 handler if needed */
        .global ___djgpp_iret                   /* Int 23 handler if needed */
___djgpp_i24:
        movb    $3,%al
___djgpp_iret:
        iret

/* Code to stop execution ASAP, EAX destroyed.  Make DS/ES/SS invalid.
   Fake exception value is passed in AL and moved into the "forced" variable.
   This is used to convert a HW interrupt into something we can transfer
   control away from via longjmp or exit(), common with SIGINT, SIGFPE, or
   if we want EIP information on timers. */

        .balign 16,,7
        .global ___djgpp_hw_exception
___djgpp_hw_exception:
        .byte   0x2e                            /* CS: */
        cmpl    $0, forced                      /* Already flagged? */
        jne     already_forced
        pushl   %ebx
        pushl   %ecx
        pushl   %edx
        pushl   %ds
        movw    %cs:___djgpp_our_DS, %ds
        movl    ___djgpp_app_DS, %ebx           /* avoid size prefix */
        lsl     %ebx, %ecx
        movl    %ecx, ds_limit                  /* Save current limit */
        movb    %al, forced                     /* Indicate a fake exception */
        xorl    %ecx, %ecx
        movw    $0xfff, %dx                     /* 4K limit is null page ! */
        movw    $0x0008, %ax
        int     $0x31                           /* Set segment limit */
5:      popl    %ds
        popl    %edx
        popl    %ecx
        popl    %ebx
already_forced:
        ret

        .global ___djgpp_hw_lock_end
___djgpp_hw_lock_end:
        ret                                     /* LD does weird things */
