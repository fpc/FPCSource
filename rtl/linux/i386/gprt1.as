#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1993,97 by Michael Van Canneyt and Peter Vreman
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
# Linux ELF startup code with profiling support for Free Pascal
# Note: Needs linking with -lgmon and -lc
#

	.file	"gprt1.as"
	.text
	.globl _start
	.type _start,@function
_start:
	/* First locate the start of the environment variables */
	popl	%ecx
	movl	%esp,%ebx  		/* Points to the arguments */
	movl	%ecx,%eax
	incl	%eax
	shll	$2,%eax
	addl	%esp,%eax
	andl	$0xfffffff8,%esp	/* Align stack */

	movl	%eax,U_SYSLINUX_ENVP	/* Move the environment pointer */
	movl	%ecx,U_SYSLINUX_ARGC	/* Move the argument counter    */
	movl	%ebx,U_SYSLINUX_ARGV	/* Move the argument pointer	*/

	pushl	$_etext			/* Initialize gmon */
	pushl	$_start
	call	monstartup
	addl	$8,%esp
	pushl	$_mcleanup 
	call	atexit
	addl	$4,%esp
	
	call	PASCALMAIN

	.globl _haltproc
	.type _haltproc,@function
_haltproc:
	xorl	%ebx,%ebx		/* load and save exitcode */
	movw	U_SYSLINUX_EXITCODE,%bx
	pushl	%ebx
	
	call	exit			/* call libc exit, this will */
					/* write the gmon.out */
					
	movl	$1,%eax			/* exit call */
	popl	%ebx
	int	$0x80
	jmp	_haltproc

.data
	.align	4
	
	.globl	___brk_addr		/* heap management */
	.type	___brk_addr,@object
	.size	___brk_addr,4
___fpc_brk_addr:
	.long	0
	
	.globl	__curbrk		/* necessary for libc */
	.type	__curbrk,@object
	.size	__curbrk,4
__curbrk:
	.long	0
	