#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2018 by Yuriy Sydorov and other
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
# Shared library startup code for Free Pascal. Android-ARM target.
#

.file   "dllprt0.as"
.text
.align 2
        .globl  FPC_SHARED_LIB_START
        .type   FPC_SHARED_LIB_START,#function
FPC_SHARED_LIB_START:
        stp	x29,x30,[sp, #-16]!

        /* Save initial stackpointer */
        adrp x14,:got:__stkptr
        ldr	x14,[x14,#:got_lo12:__stkptr]
        mov x13,sp
        str x13,[x14]

        /* Get environment info from libc */
        adrp x14,:got:environ
        ldr	x14,[x14,#:got_lo12:environ]
        ldr x13,[x14]
        /* Check if environment is NULL */
        cmp x13,#0
        b.ne .Lenvok

        adrp x14,:got:EmptyEnv
        ldr	x13,[x14,#:got_lo12:EmptyEnv]

.Lenvok:
        adrp x14,:got:operatingsystem_parameter_envp
        ldr	x14,[x14,#:got_lo12:operatingsystem_parameter_envp]
        str x13,[x14]

        /* Call main */
        bl FPC_LIB_MAIN_ANDROID
        /* Call library init */
        bl FPC_LIB_INIT_ANDROID

        ldp	x29,x30,[sp], #16
        ret

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        /* Simply call libc exit(). _haltproc has the same declaration as exit. */
        bl exit

/* --------------------------------------------------------- */
.data
        .comm __stkptr,8
        .comm operatingsystem_parameter_envp,8
operatingsystem_parameter_argc:
        .global operatingsystem_parameter_argc
        .long 1
operatingsystem_parameter_argv:
        .global operatingsystem_parameter_argv
        .quad EmptyCmdLine
EmptyCmdLine:
        .quad EmptyCmdStr
EmptyCmdStr:
        .ascii "\0"
EmptyEnv:
        .quad 0
        .quad 0
        .quad 0

/* --------------------------------------------------------- */
      	.section .init_array, "aw"
	      .quad FPC_SHARED_LIB_START
