#
#   $Id: prt0_10.as,v 1.1 2004/01/04 01:13:23 marco Exp $
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Marco van de Voort, Michael Van Canneyt
#                                                  and Peter Vreman
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
# NetBSD standard (static) ELF/i386 startup code for Free Pascal
#


	.file	"prt0.s"
	.version	"01.01"
gcc2_compiled.:
.globl __progname

.section	.rodata
.LC0:
	.ascii "\0"
.data
	.align 4
	.type	 __progname,@object
	.size	 __progname,4
__progname:
	.long .LC0
.globl __ps_strings
	.align 4
	.type	 __ps_strings,@object
	.size	 __ps_strings,4
__ps_strings:
	.long 0
	.align 4
___fpucw:
        .long   0x1332

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

#APP
	
	.text
	.align	4
	.globl	__start
	.globl	_start
_start:
__start:
	pushl	%ebx			# ps_strings
	pushl	%ecx			# obj
	pushl	%edx			# cleanup
	movl	12(%esp),%eax
	leal	20(%esp,%eax,4),%ecx
	leal	16(%esp),%edx
	pushl	%ecx
	pushl	%edx
	pushl	%eax
	call	___start

#NO_APP
.text
	.align 4
.globl ___start
	.type	 ___start,@function
___start:
	pushl %ebp
	movl %esp,%ebp
	movl 16(%ebp),%eax
	movl %eax,environ
	movl %eax,U_SYSBSD_ENVP
	movl 8(%ebp),%eax
	movl %eax,U_SYSBSD_ARGC
	movl 12(%ebp),%eax
	movl %eax,U_SYSBSD_ARGV
	movl (%eax),%edx
	movl %edx,__progname
	testl %edx,%edx
	je .L2
	pushl $47
	movl __progname,%eax
	pushl %eax
	call _strrchr
	addl $8,%esp
	movl %eax,%eax
	movl %eax,__progname
	cmpl $0,__progname
	jne .L3
	movl 12(%ebp),%eax
	movl (%eax),%edx
	movl %edx,__progname
	jmp .L2
	.align 4
.L3:
	incl __progname
.L4:
.L2:
	cmpl $0,28(%ebp)
	je .L5
	movl 28(%ebp),%eax
	movl %eax,__ps_strings
.L5:
#	pushl $_fini
#	call atexit
#	addl $4,%esp
#	call _init
# copied from linux

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        xorl    %ebp,%ebp

	call main
	pushl %eax
	jmp  _haltproc
        
.p2align 2,0x90
.globl _haltproc
.type _haltproc,@function

_haltproc:
           mov $1,%eax  
           movzwl U_SYSBSD_EXITCODE,%ebx
           pushl %ebx
           call _actualsyscall
           addl  $4,%esp
           jmp   _haltproc

_actualsyscall:
         int $0x80
         jb .LErrorcode
         xor %ebx,%ebx
         ret
.LErrorcode:
         mov %eax,%ebx
         mov $-1,%eax
         ret
        .p2align 2,0x90 	

.Lfe1:
	.size	 ___start,.Lfe1-___start
	.align 4
	.type	 _strrchr,@function
_strrchr:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	movl 12(%ebp),%eax
	movb %al,-1(%ebp)
	movl $0,-8(%ebp)
	.align 4
.L7:
	movl 8(%ebp),%eax
	movb (%eax),%dl
	cmpb -1(%ebp),%dl
	jne .L10
	movl 8(%ebp),%eax
	movl %eax,-8(%ebp)
.L10:
	movl 8(%ebp),%eax
	cmpb $0,(%eax)
	jne .L9
	movl -8(%ebp),%edx
	movl %edx,%eax
	jmp .L6
	.align 4
.L11:
.L9:
	incl 8(%ebp)
	jmp .L7
	.align 4
.L8:
.L6:
	leave
	ret


.Lfe2:
	.size	 _strrchr,.Lfe2-_strrchr
	.comm	environ,4,4

# This section is needed for NetBSD to recognize a NetBSD binary as such.
# otherwise it will be startup in Linux emulation mode.

.section ".note.netbsd.ident","a"
.p2align 2

.long 7
.long 4
# ELF NOTE TYPE NETBSD TAG
.long 1
.ascii "NetBSD\0\0"
.long 199905
