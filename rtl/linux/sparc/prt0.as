/* Startup code for elf32-sparc
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson <richard@gnu.ai.mit.edu>, 1997.

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

	.align 4
	.global _dynamic_start
	.type _dynamic_start,#function
_dynamic_start:
        /* TODO: need to set __dl_fini here */
        b _start

	.align 4
	.global _start
	.type _start,#function
_start:
  	/* Terminate the stack frame, and reserve space for functions to
     	   drop their arguments.  */
	mov	%g0, %fp
	sub	%sp, 6*4, %sp

.ifdef PIC
        /* Set %l7 to _GLOBAL_OFFSET_TABLE value */
        sethi %hi(_GLOBAL_OFFSET_TABLE_-8),%l7
        or %l7,%lo(_GLOBAL_OFFSET_TABLE_-4),%l7
        call FPC_GETGOT
        nop
.endif
  	/* Extract the arguments and environment as encoded on the stack.  The
     	   argument info starts after one register window (16 words) past the SP.  */
	ld	[%sp+22*4], %o2
	sethi	%hi(operatingsystem_parameter_argc),%o1
	or	%o1,%lo(operatingsystem_parameter_argc),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
	st	%o2, [%o1]

	add	%sp, 23*4, %o0
	sethi	%hi(operatingsystem_parameter_argv),%o1
	or	%o1,%lo(operatingsystem_parameter_argv),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
	st	%o0, [%o1]

	/* envp=(argc+1)*4+argv */
	inc     %o2
	sll     %o2, 2, %o2
	add	%o2, %o0, %o2
	sethi	%hi(operatingsystem_parameter_envp),%o1
	or	%o1,%lo(operatingsystem_parameter_envp),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
	st	%o2, [%o1]

        /* Save initial stackpointer */
	sethi	%hi(__stkptr),%o1
	or	%o1,%lo(__stkptr),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
	st	%sp, [%o1]

  	/* Call the user program entry point.  */
  	call	PASCALMAIN
  	nop
	/* Die very horribly if main returns.  */
	unimp

.globl  _haltproc
.type   _haltproc,@function
_haltproc:
        /* TODO: need to check whether __dl_fini is non-zero and call the function pointer in case */

	mov	188, %g1		/* "exit_group" system call */
	ta	0x10			/* dot the system call */
	nop				/* delay slot */
	/* Die very horribly if exit returns.  */
	unimp

	.size _start, .-_start

        .comm __stkptr,4
        .comm __dl_fini,4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4
