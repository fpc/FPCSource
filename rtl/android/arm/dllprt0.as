/*
 * This file is part of the Free Pascal run time library.
 * Copyright (c) 2011 by Thomas Schatzl,
 * member of the Free Pascal development team.
 *
 * Startup code for shared libraries, ARM version.
 *
 * See the file COPYING.FPC, included in this distribution,
 * for details about the copyright.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

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
        ldr r5,[ip]
        ldr ip,=operatingsystem_parameter_envp
        str r5,[ip]
        
        /* Register exit handler. It is called only when the main process terminates */
        ldr r0,=FPC_LIB_EXIT
        bl atexit

        /* call main and exit normally */
        bl PASCALMAIN
        ldmdb fp, {fp, sp, pc}

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
        b exit

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

/* --------------------------------------------------------- */
      	.section .init_array, "aw"
	      .long FPC_SHARED_LIB_START
