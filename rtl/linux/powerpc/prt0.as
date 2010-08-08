/* Startup code for programs linked with GNU libc.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

        .section ".text"
        .globl  _start
_start:
	mr 	26,1
	/* Set up an initial stack frame, and clear the LR.  */
	clrrwi	1,1,4
	li	0,0
	stwu	1,-16(1)
	mtlr	0
	stw	0,0(1)
	lwz     3,0(26)       /* get argc */
        lis 	11,operatingsystem_parameter_argc@ha
	stw 	3,operatingsystem_parameter_argc@l(11);

	addi    4,26,4        /* get argv */
        lis 	11,operatingsystem_parameter_argv@ha
	stw 	4,operatingsystem_parameter_argv@l(11);

	addi    27,3,1        /* calculate argc + 1 into r27 */
        slwi    27,27,2       /* calculate (argc + 1) * sizeof(char *) into r27 */
        add     5,4,27        /* get address of env[0] */
	lis 	11,operatingsystem_parameter_envp@ha
	stw 	5,operatingsystem_parameter_envp@l(11);

    lis 	11,__stkptr@ha
	stw 	1,__stkptr@l(11);

	bl	PASCALMAIN

	b	_haltproc

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        li      0,1	      /* exit call */
        sc
        b	_haltproc

	/* Define a symbol for the first piece of initialized data.  */
	.section ".data"
	.globl	__data_start
__data_start:
data_start:

.text
        .comm __stkptr,4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

.section .note.GNU-stack,"",%progbits
