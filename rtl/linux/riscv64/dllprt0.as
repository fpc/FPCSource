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
        addi sp, sp, -16
        sd ra, 8(sp)
        sd x8, 0(sp)
        addi x8, sp, 16

        /* a0 contains argc, a1 contains argv and a2 contains envp */
1:auipc	x8,%pcrel_hi(operatingsystem_parameter_argc)
	sw	a0,%pcrel_lo(1b)(x8)
1:auipc	x8,%pcrel_hi(operatingsystem_parameter_argv)
	sd	a1,%pcrel_lo(1b)(x8)
1:auipc	x8,%pcrel_hi(operatingsystem_parameter_envp)
	sd	a2,%pcrel_lo(1b)(x8)

        /* save initial stackpointer */
1:auipc	x8,%pcrel_hi(__stklen)
	sd	sp,%pcrel_lo(1b)(x8)

        /* call main and exit normally */
1:auipc	x8,%pcrel_hi(PASCALMAIN)
        jalr ra, x8, %pcrel_lo(1b)

        ld x8, 0(x8)
        ld ra, 8(x8)
        addi sp, sp, 16

        jalr x0, ra

        .globl  _haltproc
        .type   _haltproc,function
_haltproc:
1:auipc	x8,%pcrel_hi(operatingsystem_result)
	lbu	x1,%pcrel_lo(1b)(x8)
	addi	x17, x0, 94
	ecall
        jal x0, _haltproc

.data

        .type operatingsystem_parameters,object
        .size operatingsystem_parameters, 24
operatingsystem_parameters:
        .skip 3 * 8
        .global operatingsystem_parameter_argc
        .global operatingsystem_parameter_argv
        .global operatingsystem_parameter_envp
        .set operatingsystem_parameter_argc, operatingsystem_parameters+0
        .set operatingsystem_parameter_argv, operatingsystem_parameters+8
        .set operatingsystem_parameter_envp, operatingsystem_parameters+16

.bss

        .comm __stkptr,8

