#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2004 by Michael Van Canneyt, Peter Vreman,
#   & Daniel Mantione, members of the Free Pascal development team.
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
# Linux ELF startup code for Free Pascal
#
# The code in this file is the default startup code, it is used unless
# libc is linked in, profiling is enabled or you are compiling a shared
# library.
#
#
# Stack layout at program start:
#
#         nil
#         envn
#         ....
#         ....           ENVIRONMENT VARIABLES
#         env1
#         env0
#         nil
#         argn
#         ....
#         ....           COMMAND LINE OPTIONS
#         arg1
#         arg0
#         argc <--- esp
#

        .file   "prt0.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx                    /* Get argc in ecx */
        movl    %esp,%ebx               /* Esp now points to the arguments */
        leal    4(%esp,%ecx,4),%eax     /* The start of the environment is: esp+4*eax+4 */
        andl    $0xfffffff8,%esp        /* Align stack */

        leal    operatingsystem_parameters,%edi
        stosl   /* Move the environment pointer */
        xchg    %ecx,%eax
        stosl   /* Move the argument counter    */
        xchg    %ebx,%eax
        stosl   /* Move the argument pointer    */


        fninit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

#        /* Initialize gs for thread local storage */
#        movw    %ds,%ax
#        movw    %ax,%gs

        /* Save initial stackpointer */
        movl    %esp,__stkptr

        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
_haltproc2:             # GAS <= 2.15 bug: generates larger jump if a label is exported
        movl    $252,%eax                /* exit_group */
        movzwl  operatingsystem_result,%ebx
        int     $0x80
        movl    $1,%eax                /* exit */
        movzwl  operatingsystem_result,%ebx
        int     $0x80
        jmp     _haltproc2

.data
        .type   __fpucw,@object
        .size   __fpucw,4
        .global __fpucw
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

//.section .threadvar,"aw",@nobits
        .comm   ___fpc_threadvar_offset,4
.section .note.GNU-stack,"",%progbits
