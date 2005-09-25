/*
*/
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
   
.macro LOAD_64BIT_VAL ra, value 
	addis           \ra, 0, \value@highest
	ori             \ra,\ra,\value@higher
	sldi            \ra,\ra,32
	oris            \ra,\ra,\value@h
	ori             \ra,\ra,\value@l	
.endm

.macro FUNCTION_PROLOG fn
	.section	".text"
	.align	2
	.globl	\fn
	.section	".opd", "aw"
	.align	3
	\fn:
	.quad	.\fn, .TOC.@tocbase, 0
	.previous
	.size	\fn, 24
	.type	\fn, @function	
	.globl	.\fn
.\fn:
.endm

.macro FUNCTION_EPILOG fn
	.long	0
	.byte	0, 12, 0, 0, 0, 0, 0, 0
	.type	.\fn, @function
	.size	.\fn,.-.\fn
.endm

.macro PRINTMSG msg len
	lis	4, \msg@highest
	ori	4, 4, \msg@higher
	sldi	4, 4, 32
	oris	4, 4, \msg@h
	ori	4, 4, \msg@l	
	li	5, \len	
	li	0,4
	li	3,1
	sc
.endm
	/* 
	cprt0 pascal entry
	*/
FUNCTION_PROLOG _start

	mr 	26, 1
	/* Set up an initial stack frame, and clear the LR */
	clrrdi  1, 1, 5       /* align r1 */
	li      0, 0          
	stdu    1,-48(1)      
	mtlr    0             
	std     0, 0(1)       /* r1 = pointer to NULL value */

	/* store argument count (= 0(r1) )*/
	ld      3, 0(26)
	LOAD_64BIT_VAL 10, operatingsystem_parameter_argc
	stw     3, 0(10)
	/* calculate argument vector address and store (= 8(r1) + 8 ) */
	addi    4, 26, 8
	LOAD_64BIT_VAL 10, operatingsystem_parameter_argv
	std     4, 0(10)
	/* store environment pointer (= argv + (argc+1)* 8 ) */
	addi    5, 3, 1
	sldi    5, 5, 3
	add     5, 4, 5
	LOAD_64BIT_VAL 10, operatingsystem_parameter_envp
	std     5, 0(10)
	
	bl	.__libc_init_first
	nop
	
	lis	3, _dl_fini@highest
	ori	3, 3, _dl_fini@higher
	sldi	3,3,32
	oris	3, 3, _dl_fini@h
	ori	3, 3, _dl_fini@l

	bl      .PASCALMAIN
	nop
	ori     0, 0, 0

	/* directly jump to exit procedure, not via the function pointer */
	b       _haltproc
	
	.align  3

	.global ._haltproc
	.section        ".opd", "aw"
	.align 3
._haltproc:
	.quad   _haltproc, .TOC.@tocbase, 0
	.previous
	.size ._haltproc, 24
	.global _haltproc

_haltproc:
	/* exit call */
	li      0, 1
	sc
	b       _haltproc

	/* Define a symbol for the first piece of initialized data.  */
	.section ".data"
	.globl  __data_start
__data_start:
data_start:
	.globl  ___fpc_brk_addr	/* heap management */
	.type   ___fpc_brk_addr, @object
	.size   ___fpc_brk_addr, 4
___fpc_brk_addr:
	.long   0

.text
	.comm operatingsystem_parameter_argc, 4
	.comm operatingsystem_parameter_argv, 8
	.comm operatingsystem_parameter_envp, 8
	.comm operatingsystem_parameter_auxp, 8
	.comm operatingsystem_parameter_exitp, 8

/*
*/
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

.macro LOAD_64BIT_VAL ra, value
        addis           \ra, 0, \value@highest
        ori             \ra,\ra,\value@higher
        sldi            \ra,\ra,32
        oris            \ra,\ra,\value@h
        ori             \ra,\ra,\value@l
.endm

.macro FUNCTION_PROLOG fn
        .section        ".text"
        .align  2
        .globl  \fn
        .section        ".opd", "aw"
        .align  3
        \fn:
        .quad   .\fn, .TOC.@tocbase, 0
        .previous
        .size   \fn, 24
        .type   \fn, @function
        .globl  .\fn
.\fn:
.endm

.macro FUNCTION_EPILOG fn
        .long   0
        .byte   0, 12, 0, 0, 0, 0, 0, 0
        .type   .\fn, @function
        .size   .\fn,.-.\fn
.endm

.macro PRINTMSG msg len
        lis     4, \msg@highest
        ori     4, 4, \msg@higher
        sldi    4, 4, 32
        oris    4, 4, \msg@h
        ori     4, 4, \msg@l
        li      5, \len
        li      0,4
        li      3,1
        sc
.endm
        /*
        cprt0 pascal entry
        */
FUNCTION_PROLOG _start

        mr      26, 1
        /* Set up an initial stack frame, and clear the LR */
        clrrdi  1, 1, 5       /* align r1 */
        li      0, 0
        stdu    1,-48(1)
        mtlr    0
        std     0, 0(1)       /* r1 = pointer to NULL value */

        /* store argument count (= 0(r1) )*/
        ld      3, 0(26)
        LOAD_64BIT_VAL 10, operatingsystem_parameter_argc
        stw     3, 0(10)
        /* calculate argument vector address and store (= 8(r1) + 8 ) */
        addi    4, 26, 8
        LOAD_64BIT_VAL 10, operatingsystem_parameter_argv
        std     4, 0(10)
        /* store environment pointer (= argv + (argc+1)* 8 ) */
        addi    5, 3, 1
        sldi    5, 5, 3
        add     5, 4, 5
        LOAD_64BIT_VAL 10, operatingsystem_parameter_envp
        std     5, 0(10)

        bl      .__libc_init_first
        nop

        lis     3, _dl_fini@highest
        ori     3, 3, _dl_fini@higher
        sldi    3,3,32
        oris    3, 3, _dl_fini@h
        ori     3, 3, _dl_fini@l

        bl      .PASCALMAIN
        nop
        ori     0, 0, 0

        /* directly jump to exit procedure, not via the function pointer */
        b       _haltproc

        .align  3

        .global ._haltproc
        .section        ".opd", "aw"
        .align 3
._haltproc:
        .quad   _haltproc, .TOC.@tocbase, 0
        .previous
        .size ._haltproc, 24
        .global _haltproc

_haltproc:
        /* exit call */
        li      0, 1
        sc
        b       _haltproc

        /* Define a symbol for the first piece of initialized data.  */
        .section ".data"
        .globl  __data_start
__data_start:
data_start:
        .globl  ___fpc_brk_addr /* heap management */
        .type   ___fpc_brk_addr, @object
        .size   ___fpc_brk_addr, 4
___fpc_brk_addr:
        .long   0

.text
        .comm operatingsystem_parameter_argc, 4
        .comm operatingsystem_parameter_argv, 8
        .comm operatingsystem_parameter_envp, 8
        .comm operatingsystem_parameter_auxp, 8
        .comm operatingsystem_parameter_exitp, 8

