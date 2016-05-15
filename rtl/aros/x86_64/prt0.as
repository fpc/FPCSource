#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2015 by Marcus Sackrow
#
#   AROS x86_64 startup code
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY;without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#**********************************************************************}

# AROS startup code
	.text
	.align 8
        .section .aros.startup, "ax"
        .globl start
        .globl _start
	.type _start,@function	
_start:
start:
# save registers, suggested by
# System V Application Binary Interface
# AMD64 Architecture Processor Support
# 0.99.6 page 21 Fig. 3.4
        push	%rbp
        push	%rbx
        push	%r12
        push	%r13
        push	%r14
        push	%r15
        movq	%rdx, _ExecBase
        movq	%rsp, STKPTR
# todo: stack change (see i386)
        callq	PASCALMAIN
	jmpq	_haltproc

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
	movslq	operatingsystem_result,%rax
	mov	STKPTR, %rsp
	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbx
	pop	%rbp
        ret

	.data
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
        data_start = __data_start

.bss
		.type	STKPTR,@object
		.size	STKPTR,8
		.global	STKPTR
STKPTR:	.skip	8

		.type	_ExecBase,@object
		.size	_ExecBase,8
		.global	_ExecBase
_ExecBase:	.skip	8
