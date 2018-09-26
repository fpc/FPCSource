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
        .globl  _startlib
        .type   _startlib,function
_startlib:
        .globl  FPC_SHARED_LIB_START
        .type   FPC_SHARED_LIB_START,function
FPC_SHARED_LIB_START:
        sw x1, -4(x2)
        sw x8, -8(x2)
        addi x8, x2, 0
        addi x2, x2, -8

        /* a1 contains argc, a2 contains argv and a3 contains envp */
        lui x15, %hi(operatingsystem_parameter_argc)
        addi x15,x15,%lo(operatingsystem_parameter_argc)
        sw a1, (x15)

        lui x15, %hi(operatingsystem_parameter_argv)
        addi x15,x15,%lo(operatingsystem_parameter_argv)
        sw a2, (x15)

        lui x15, %hi(operatingsystem_parameter_envp)
        addi x15,x15,%lo(operatingsystem_parameter_envp)
        sw a3, (x15)

        /* save initial stackpointer */
        lui x15, %hi(__stklen)
        addi x15,x15,%lo(__stklen)
        sw x2, (x15)

        /* call main and exit normally */
        jal x1, PASCALMAIN
        lw x8, -8(x8)
        lw x1, -4(x8)

        jalr x0, x1

        .globl  _haltproc
        .type   _haltproc,function
_haltproc:
        /* reload exitcode */
        lui x10, %hi(operatingsystem_result)
        addi x10,x10,%lo(operatingsystem_result)
        addi x17, x0, 248
        scall
        jal x0, _haltproc

.data

        .type operatingsystem_parameters,object
        .size operatingsystem_parameters,12
operatingsystem_parameters:
        .skip 3*4
        .global operatingsystem_parameter_envp
        .global operatingsystem_parameter_argc
        .global operatingsystem_parameter_argv
        .set operatingsystem_parameter_envp,operatingsystem_parameters+0
        .set operatingsystem_parameter_argc,operatingsystem_parameters+4
        .set operatingsystem_parameter_argv,operatingsystem_parameters+8

.bss

        .comm __stkptr,4

