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
# Shared library startup code for Free Pascal. Android-ARM target.
#

.file   "dllprt0.as"
.text
        .globl  FPC_SHARED_LIB_START
        .type   FPC_SHARED_LIB_START,#function
FPC_SHARED_LIB_START:
        mov ip, sp
        stmfd sp!,{fp, ip, lr, pc}
        sub fp, ip, #4

        /* Get GOT */
        ldr r3,.L_GOT1
.LPIC1:
        add r3,pc,r3

        /* Save initial stackpointer */
        ldr ip,.L__stkptr
        ldr ip,[r3, ip]
        str sp,[ip]

        /* Get environment info from libc */
        ldr ip,.Lenviron
        ldr ip,[r3, ip]
        ldr r0,[ip]
        /* Check if environment is NULL */
        cmp r0,#0
        ldreq r0,.LEmptyEnv
        ldreq r0,[r3, r0]
        ldr ip,.Loperatingsystem_parameter_envp
        ldr ip,[r3, ip]
        str r0,[ip]

        /* Call main */
        blx PASCALMAIN
        /* Call library init */
        blx FPC_LIB_INIT_ANDROID

        ldmea fp, {fp, sp, pc}

.L_GOT1:
        .long _GLOBAL_OFFSET_TABLE_-.LPIC1-8
.L__stkptr:
        .word __stkptr(GOT)
.Lenviron:
        .word environ(GOT)
.LEmptyEnv:
        .word EmptyEnv(GOT)
.Loperatingsystem_parameter_envp:
        .word operatingsystem_parameter_envp(GOT)

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        .globl  _haltproc_eabi
        .type   _haltproc_eabi,#function
_haltproc_eabi:
        /* Simply call libc exit(). _haltproc has the same declaration as exit. */
        blx exit

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
