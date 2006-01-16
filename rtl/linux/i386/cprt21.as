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

        .file   "prt1.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */

        popl    %esi
        movl    %eax,%edi

        movl    %esp,%ebx               /* Points to the arguments */
        movl    %esi,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,operatingsystem_parameter_envp    /* Move the environment pointer */
        movl    %esi,operatingsystem_parameter_argc    /* Move the argument counter    */
        movl    %ebx,operatingsystem_parameter_argv    /* Move the argument pointer    */

        xorl    %ebp,%ebp
        pushl   %edi
        pushl   %esp
        pushl   %edx
        pushl   $_fini_dummy
        pushl   $_init_dummy
        pushl   %ebx
        pushl   %esi
        pushl   $main
        call    __libc_start_main
        hlt

/* fake main routine which will be run from libc */
main:
        /* save return address */
        popl    %eax
        movl    %eax,___fpc_ret
        movl    %ebx,___fpc_ret_ebx
        movl    %ebp,___fpc_ret_ebp
        pushl   %eax

        /* Save initial stackpointer */
        movl    %esp,__stkptr

        /* start the program */
        xorl    %ebp,%ebp
        call    PASCALMAIN
        hlt

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        movzwl    operatingsystem_result,%eax

        movl    ___fpc_ret,%edx         /* return to libc */
        movl    ___fpc_ret_ebp,%ebp
        movl    ___fpc_ret_ebx,%ebx
        push    %edx
_init_dummy:
_fini_dummy:
        ret

.data
        .align  4

___fpc_ret:                             /* return address to libc */
        .long   0
___fpc_ret_ebx:
        .long   0
___fpc_ret_ebp:
        .long   0

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
