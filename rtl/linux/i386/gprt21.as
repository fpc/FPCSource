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

        movl    %edi,%eax
        xorl    %ebp,%ebp
        pushl   %eax
        pushl   %esp
        pushl   %edx
        pushl   $_fini_dummy
        pushl   $_init_dummy
        pushl   %ebx
        pushl   %esi
        pushl   $cmain
        call    __libc_start_main
        hlt

/* fake main routine which will be run from libc */
cmain:
        /* save return address */
        popl    %eax
        movl    %eax,___fpc_ret
        movl    %ebx,___fpc_ret_ebx
        movl    %esi,___fpc_ret_esi
        movl    %edi,___fpc_ret_edi
        pushl   %eax

        call    __gmon_start__

        /* Save initial stackpointer */
        movl    %esp,__stkptr

        /* start the program */
        call    PASCALMAIN
        hlt

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        movzwl    operatingsystem_result,%eax

        movl    ___fpc_ret,%edx         /* return to libc */
        movl    ___fpc_ret_ebx,%ebx
        movl    ___fpc_ret_esi,%esi
        movl    ___fpc_ret_edi,%edi
        push    %edx
_init_dummy:
_fini_dummy:
        ret

        .globl  __gmon_start__
        .type   __gmon_start__,@function
__gmon_start__:
        pushl   %ebp
        movl    __monstarted,%eax
        leal    0x1(%eax),%edx
        movl    %esp,%ebp
        movl    %edx,__monstarted
        testl   %eax,%eax
        jnz     .Lnomonstart
        pushl   $etext                  /* Initialize gmon */
        pushl   $_start
        call    monstartup
        addl    $8,%esp
        pushl   $_mcleanup
        call    atexit
        addl    $4,%esp
.Lnomonstart:
        movl   %ebp,%esp
        popl   %ebp
        ret

.data
        .align  4

___fpc_ret:                             /* return address to libc */
        .long   0
___fpc_ret_ebx:
        .long   0
___fpc_ret_esi:
        .long   0
___fpc_ret_edi:
        .long   0

.bss
        .lcomm __monstarted,4

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

