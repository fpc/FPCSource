#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2013 by Yury Sidorov and other
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
# Shared library startup code for Free Pascal. Android-i386 target.
#

.file   "dllprt0.as"
.text
        .globl  FPC_SHARED_LIB_START
        .type   FPC_SHARED_LIB_START,@function
FPC_SHARED_LIB_START:
        pushl   %ebp
        movl    %esp,%ebp
        /* Align the stack to a 16 byte boundary */
        andl $~15, %esp

        /* Save ebx */
        pushl   %ebx

        /* GOT init */
        call    fpc_geteipasebx
        addl    $_GLOBAL_OFFSET_TABLE_,%ebx

        /* Save initial stackpointer */
        movl    __stkptr@GOT(%ebx),%eax
        movl    %esp,(%eax)

        /* Get environment info from libc */
        movl    environ@GOT(%ebx),%eax
        movl    (%eax),%eax
        /* Check if environment is NULL */
        test    %eax,%eax
        jne     env_ok
        movl    EmptyEnv@GOT(%ebx),%eax
env_ok:
        movl    operatingsystem_parameter_envp@GOT(%ebx),%edx
        movl    %eax,(%edx)

        /* Restore ebx */
        popl    %ebx

        /* Call main */
        call    PASCALMAIN@PLT
        /* Call library init */
        call    FPC_LIB_INIT_ANDROID@PLT

        leave
        ret

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        /* GOT init */
        call    fpc_geteipasebx
        addl    $_GLOBAL_OFFSET_TABLE_,%ebx
        /* Jump to libc exit(). _haltproc has the same declaration as exit. */
        jmp     exit@PLT

/* --------------------------------------------------------- */
.data
        .comm __stkptr,4
        .comm operatingsystem_parameter_envp,4
operatingsystem_parameter_argc:
        .global operatingsystem_parameter_argc
        .long 1
operatingsystem_parameter_argv:
        .global operatingsystem_parameter_argv
        .long EmptyCmdLine
EmptyCmdLine:
        .long EmptyCmdStr
EmptyCmdStr:
        .ascii "\0"
EmptyEnv:
        .long 0
        .long 0
        .long 0

/* --------------------------------------------------------- */
      	.section .init_array, "aw"
	      .long FPC_SHARED_LIB_START
