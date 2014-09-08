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

        /* Save initial stackpointer */
        movl    %esp,__stkptr

        /* Get environment info from libc */
        movl    environ,%eax
        /* Check if environment is NULL */
        test    %eax,%eax
        jne     env_ok
        leal    EmptyEnv,%eax
env_ok:
        movl    %eax,operatingsystem_parameter_envp

        /* Register exit handler. It is called only when the main process terminates */
        leal    FPC_LIB_EXIT,%eax
        pushl   %eax
        call    atexit
        addl    $4,%esp

        /* call main and exit normally */
        call    PASCALMAIN
        leave
        ret

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movzwl  operatingsystem_result,%ebx
        pushl   %ebx
        /* Call libc exit() */
        call    exit

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
