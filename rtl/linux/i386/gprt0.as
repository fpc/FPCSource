#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman
#   members of the Free Pascal development team.
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY;without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#**********************************************************************}
#
# Linux ELF startup code with profiling support for Free Pascal
# Note: Needs linking with -lgmon and -lc
#

        .file   "gprt1.as"
        .text
        .globl _start
        .type _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx
        movl    %esp,%ebx               /* Points to the arguments */
        movl    %ecx,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,operatingsystem_parameter_envp    /* Move the environment pointer */
        movl    %ecx,operatingsystem_parameter_argc    /* Move the argument counter    */
        movl    %ebx,operatingsystem_parameter_argv    /* Move the argument pointer    */

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        pushl   $_etext                 /* Initialize gmon */
        pushl   $_start
        call    monstartup
        addl    $8,%esp
        pushl   $_mcleanup
        call    atexit
        addl    $4,%esp

        /* Save initial stackpointer */
        movl    %esp,__stkptr

        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
_haltproc2:             # GAS <= 2.15 bug: generates larger jump if a label is exported
        movzwl  operatingsystem_result,%ebx
        pushl   %ebx
        call    exit                    /* call libc exit, this will */
                                        /* write the gmon.out */
        movl    $252,%eax                /* exit_group */
        popl    %ebx
        int     $0x80
        jmp     _haltproc2

.data
___fpucw:
        .long   0x1332

.bss
        .type   __stkptr,@object
        .size   __stkptr,4
        .global __stkptr
__stkptr:
        .skip   4

        .type operatingsystem_parameters,@object
        .size operatingsystem_parameters,12
operatingsystem_parameters:
        .skip 3*4

        .global operatingsystem_parameter_envp
        .global operatingsystem_parameter_argc
        .global operatingsystem_parameter_argv
        .set operatingsystem_parameter_envp,operatingsystem_parameters+0
        .set operatingsystem_parameter_argc,operatingsystem_parameters+4
        .set operatingsystem_parameter_argv,operatingsystem_parameters+8
