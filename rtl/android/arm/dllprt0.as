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
        ldr r4,.L_GOT1
.LPIC1:
        add r4,pc,r4

        /* Save initial stackpointer */
        ldr ip,.L__stkptr
        ldr ip,[r4, ip]
        str sp,[ip]

        /* Get environment info from libc */
        ldr ip,.Lenviron
        ldr ip,[r4, ip]
        ldr r0,[ip]
        /* Check if environment is NULL */
        cmp r0,#0
        ldreq r0,.LEmptyEnv
        ldreq r0,[r4, r0]
        ldr ip,.Loperatingsystem_parameter_envp
        ldr ip,[r4, ip]
        str r0,[ip]
        
        /* Register exit handler. It is called only when the main process terminates */
        ldr r0,.LFPC_LIB_EXIT
        ldr r0,[r4, r0]
        blx atexit

        /* call main and exit normally */
        blx PASCALMAIN
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
.LFPC_LIB_EXIT:
        .word FPC_LIB_EXIT(GOT)

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        .globl  _haltproc_eabi
        .type   _haltproc_eabi,#function
_haltproc_eabi:
        ldr r0,.Loperatingsystem_result
.LPIC10:
        add r0,pc,r0
        ldr r0,[r0]
        /* Call libc exit() */
        blx exit

.Loperatingsystem_result:
        .long operatingsystem_result-.LPIC10-8

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
