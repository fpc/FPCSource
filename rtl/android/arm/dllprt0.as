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

        /* Save initial stackpointer */
        ldr ip,=__stkptr
        str sp,[ip]

        /* Get environment info from libc */
        ldr ip,=environ
        ldr r0,[ip]
        /* Check if environment is NULL */
        cmp r0,#0
        ldreq r0,=EmptyEnv
        ldr ip,=operatingsystem_parameter_envp
        str r0,[ip]
        
        /* Register exit handler. It is called only when the main process terminates */
        ldr r0,=FPC_LIB_EXIT
        blx atexit

        /* call main and exit normally */
        blx PASCALMAIN
        ldmea fp, {fp, sp, pc}

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        .globl  _haltproc_eabi
        .type   _haltproc_eabi,#function
_haltproc_eabi:
        ldr r0,=operatingsystem_result
        ldr r0,[r0]
        /* Go to libc exit() */
        ldr ip,=exit
        bx ip

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
