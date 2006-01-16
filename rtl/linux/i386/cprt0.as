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
# Linux ELF startup code for Free Pascal
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

        .file   "cprt0.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx                    /* Get argc in ecx */
        movl    %esp,%ebx               /* Esp now points to the arguments */
        leal    4(%esp,%ecx,4),%eax     /* The start of the environment is: esp+4*eax+8 */
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,operatingsystem_parameter_envp    /* Move the environment pointer */
        movl    %ecx,operatingsystem_parameter_argc    /* Move the argument counter    */
        movl    %ebx,operatingsystem_parameter_argv    /* Move the argument pointer    */

        movl    %eax,__environ          /* libc environ */

        pushl   %eax
        pushl   %ebx
        pushl   %ecx

        call    __libc_init             /* init libc */
        movzwl  __fpu_control,%eax
        pushl   %eax
        call    __setfpucw
        popl    %eax
        pushl   $_fini
        call    atexit
        popl    %eax
        call    _init

        popl    %eax
        popl    %eax

        /* Save initial stackpointer */
        movl    %esp,__stkptr

        xorl    %ebp,%ebp
        call    PASCALMAIN              /* start the program */

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
_haltproc2:             # GAS <= 2.15 bug: generates larger jump if a label is exported
        movzwl  operatingsystem_result,%ebx
        pushl   %ebx
        call    exit
        xorl    %eax,%eax
        incl    %eax                    /* eax=1, exit call */
        popl    %ebx
        int     $0x80
        jmp     _haltproc2

.data

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
