/*
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2009 by Michael Van Canneyt and David Zhang

    Startup code for elf32-mipsel

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

	.section ".text"
	.align 4
	.global _start
	.type _start,#function
_start:
   0:   03a0f021        move    s8,sp
   4:   2401fff8        li      at,-8
   8:   03a1e824        and     sp,sp,at
   c:   27bdffe0        addiu   sp,sp,-32
  10:   3c17003d        lui     s7,0x3d
  14:   26f70900        addiu   s7,s7,2304
  18:   2401fff8        li      at,-8
  1c:   02e1b824        and     s7,s7,at
  20:   26f7ffe0        addiu   s7,s7,-32
  24:   8fc40000        lw      a0,0(s8)
  28:   3c050000        lui     a1,0x0
  2c:   aca40000        sw      a0,0(a1)
  30:   27c50004        addiu   a1,s8,4
  34:   3c060000        lui     a2,0x0
  38:   acc50000        sw      a1,0(a2)
  3c:   24860001        addiu   a2,a0,1
  40:   00063080        sll     a2,a2,0x2
  44:   00c53021        addu    a2,a2,a1
  48:   3c070000        lui     a3,0x0
  4c:   0c000000        jal     0 <__start>
  50:   ace60000        sw      a2,0(a3)
  54:   00000000        nop

  	/* Terminate the stack frame, and reserve space for functions to
     	   drop their arguments.  */
	mov	%g0, %fp
	sub	%sp, 6*4, %sp

  	/* Extract the arguments and environment as encoded on the stack.  The
     	   argument info starts after one register window (16 words) past the SP.  */
	ld	[%sp+22*4], %o2
	sethi	%hi(operatingsystem_parameter_argc),%o1
	or	%o1,%lo(operatingsystem_parameter_argc),%o1
	st	%o2, [%o1]

	add	%sp, 23*4, %o0
	sethi	%hi(operatingsystem_parameter_argv),%o1
	or	%o1,%lo(operatingsystem_parameter_argv),%o1
	st	%o0, [%o1]

	/* envp=(argc+1)*4+argv */
	inc     %o2
	sll     %o2, 2, %o2
	add	%o2, %o0, %o2
	sethi	%hi(operatingsystem_parameter_envp),%o1
	or	%o1,%lo(operatingsystem_parameter_envp),%o1
	st	%o2, [%o1]

    /* Save initial stackpointer */
	sethi	%hi(__stkptr),%o1
	or	%o1,%lo(__stkptr),%o1
	st	%sp, [%o1]

  	/* Call the user program entry point.  */
  	call	PASCALMAIN
  	nop

.globl  _haltproc
.type   _haltproc,@function
_haltproc:
        li      v0,4001
        lui     a0,0x0
        lw      a0,0(a0)
        syscall
        b       _haltproc
        nop

	.size _start, .-_start

        .comm __stkptr,4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4
