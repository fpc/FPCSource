#
#  This file is part of the Free Pascal run time library.
#  Copyright (c) 2011 by Marcus Sackrow
#
#  Startup code for AROS/i386 RTL
#
#  See the file COPYING.FPC, included in this distribution,
#  for details about the copyright.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY;without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

# AROS Startup Code
   .text
   .align 4
   .section .aros.startup, "ax"
   .globl _start
   .globl start
   .globl _haltproc
   .globl haltproc
_start:
start:

    /* Save the exec library base */
    movl     12(%esp),%eax
    movl     %eax,_ExecBase

    /* Save the command line pointer length to CommandLineLen */
    movl     8(%esp),%eax
    movl     %eax,CommandLineLen

    /* Save the command line pointer to CommandLine */
    movl     4(%esp),%eax
    movl     %eax,CommandLine

    /* save all registers */
    pushal

    /* get the pointer to current stack */
    movl     _ExecBase,%eax
    pushl    %eax
    pushl    $0
    movl     -196(%eax),%eax    /* FindTask(nil) */
    call     *%eax
    addl     $8,%esp

    movl     64(%eax),%ecx      /* SPUpper */
    subl     60(%eax),%ecx      /* SPLower */

/* Uncomment the symbol line below to force system stack use,
   and do not attempt to reallocate stack if the system-provided
   stack is smaller than the user specified */
# FORCE_USE_SYSTEM_STACK:

.ifndef FORCE_USE_SYSTEM_STACK
    /* Check if we need a new stack
       Only allocate a new stack if the system-provided
       stack is smaller than the one set compile time */
    cmpl     __stklen,%ecx
    jl       _allocStack
.endif

    movl     %ecx,__stklen      /* Store the new stack size */
    xorl     %eax,%eax
    movl     %eax,StackAreaPtr  /* Clear the stackAreaPtr for exit test */
    jmp      _noAllocStack

_allocStack:
    /* Allocating new stack */
    movl     _ExecBase,%eax
    pushl    %eax
    pushl    $0                 /* MEMF_ANY */
    pushl    __stklen
    movl     -456(%eax),%eax    /* AllocVec() */
    call     *%eax
    addl     $12,%esp

    testl    %eax,%eax
    je       __exit
    movl     %eax,StackAreaPtr

    /* Setting up StackSwap structure, and do the StackSwap */
    lea      StackSwapStruct,%ecx
    movl     %eax,(%ecx)            /* Bottom of the stack */
    addl     __stklen,%eax
    movl     %eax,4(%ecx)           /* Top of the stack */
    movl     %eax,8(%ecx)           /* Initial stackpointer */
    movl     _ExecBase,%eax
    pushl    %eax
    lea      StackSwapArgs,%ebx
    pushl    %ebx
    lea      _initProc,%ebx
    pushl    %ebx
    pushl    %ecx
    movl     -536(%eax),%eax        /* NewStackSwap() */
    call     *%eax
    addl     $16,%esp
    jmp      _afterMain

_noAllocStack:
    call     _initProc

_afterMain:
    /* check if we have a StackArea to free */
    movl     StackAreaPtr,%eax
    testl    %eax,%eax
    je       __exit

_freeStack:
    /* Freeing up stack area */
    movl     _ExecBase,%eax
    pushl    %eax
    pushl    StackAreaPtr
    movl     -460(%eax),%eax        /* FreeVec() */
    call     *%eax
    addl     $8,%esp

__exit:
    /* get back all registers */
    popal
    /* get returncode */
    movl     operatingsystem_result,%eax
    /* bye bye */
    ret

    /* This function is getting called from NewStackSwap() or
       as standalone if we don't do stackswap */
_initProc:
    pushal
    /* Save stack pointer */
    movl     %esp,STKPTR

    /* call the main function */
    call     PASCALMAIN

    /* entry to stop the program */
_haltproc:
haltproc:
    /* restore the old stackPtr and return */
    movl     STKPTR,%esp
    popal
    ret

  /*----------------------------------------------------*/

    .bss
    .global CommandLineLen      # byte length of command line
    .global CommandLine         # comandline as PChar
    .global STKPTR              # Used to terminate the program, initial SP
    .global _ExecBase           # exec library base
    .align 4

CommandLine:    .skip   4
CommandLineLen: .skip   4
STKPTR:         .skip   4
_ExecBase:      .skip   4

StackAreaPtr:    .skip   4
StackSwapStruct: .skip   12
StackSwapArgs:   .skip   32
