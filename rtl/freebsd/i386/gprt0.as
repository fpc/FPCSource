#
#   $Id: gprt0.as,v 1.3 2004/07/03 21:50:30 daniel Exp $
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
# FreeBSD ELF startup code for Free Pascal for dynamical linking to libc
# with profiling support.
#

	.file	"gpcrt1.c"
	.version	"01.01"
gcc2_compiled.:
.section	.note.ABI-tag,"a",@progbits
	.p2align 2
	.type	 abitag,@object
	.size	 abitag,24
abitag:
	.long 8
	.long 4
	.long 1
	.byte	 0x46,0x72,0x65,0x65,0x42,0x53,0x44,0x0
	.long 470000
.globl __progname
	.section	.rodata
.LC0:
	.byte	 0x0
.data
	.p2align 2
	.type	 __progname,@object
	.size	 __progname,4
__progname:
	.long .LC0

        .align  4
___fpucw:
        .long   0x1332

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

.text
	.p2align 2,0x90
.globl _start
		.type		 _start,@function
_start:
#APP
	movl %edx,%edx
#NO_APP
	pushl %ebp
	movl %esp,%ebp
	subl $12,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 4(%ebp),%ebx
	movl %ebx,operatingsystem_parameter_argc
	leal 12(%ebp,%ebx,4),%esi
	leal 8(%ebp),%eax
	movl %eax,operatingsystem_parameter_argv
	movl %eax,-4(%ebp)
	movl %esi,operatingsystem_parameter_envp
	movl %esi,environ
	movl $_DYNAMIC,%ecx
	testl %ebx,%ebx
	jle .L3
	movl 8(%ebp),%eax
	testl %eax,%eax
	je .L3
	movl %eax,__progname
	cmpb $0,(%eax)
	je .L3
	.p2align 2,0x90
.L7:
	cmpb $47,(%eax)
	jne .L6
	leal 1(%eax),%edi
	movl %edi,__progname
.L6:
	incl %eax
	cmpb $0,(%eax)
	jne .L7
.L3:
	testl %ecx,%ecx
	je .L10
	addl $-12,%esp
	pushl %edx
	call atexit
	addl $16,%esp
.L10:
	addl $-12,%esp
	pushl $_mcleanup
	call atexit
	addl $-12,%esp
	pushl $_fini
	call atexit
	addl $32,%esp
	addl $-8,%esp
	pushl $etext
	pushl $eprol
	call monstartup
	call _init

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw
        xorl    %ebp,%ebp
	call  main
	pushl %eax
	jmp  _haltproc

.globl _haltproc
.type _haltproc,@function
_haltproc:
           movzwl operatingsystem_result,%ebx
           pushl %ebx
	   call  exit
           mov $1,%eax  
           movzwl operatingsystem_result,%ebx
	   pushl %ebx
           call .Lactualsyscall
           addl  $4,%esp
           jmp   _haltproc

.Lactualsyscall:
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
		.size		 _start,.Lfe1-_start
#APP
	.text
	eprol:
	.previous
#NO_APP
	.comm	environ,4,4
	.globl	_DYNAMIC
	.weak	_DYNAMIC
	.ident	"GCC: (GNU) c 2.95.4 20020320 [FreeBSD]"

.bss
        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4
