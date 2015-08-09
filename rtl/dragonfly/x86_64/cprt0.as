#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Marco van de Voort, Pierre Mueller
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
# DragonFly ELF startup code for Free Pascal for dynamical linking to libc.
#
# To avoid needing a "COMPAT" system, patch the constant following the 
# "DragonFly" field in the abitag to the relevant ABI number 

	.file	"crt1.c"
	.section	.note.ABI-tag,"a",@progbits
	.align 4
	.type	abitag, @object
	.size	abitag, 28
abitag:
	.long	10
	.long	4
	.long	1
	.string	"DragonFly"
	.align	4
	.long	400000
.globl __progname
	.section	.rodata
.LC0:
	.string	""
	.data
	.align 8
	.type	__progname, @object
	.size	__progname, 8
__progname:
	.quad	.LC0
	.text
	.p2align 4,,15
.globl _start
	.type	_start, @function
_start:
.LFB5:
	pushq	%rbp
.LCFI0:
	movq	%rsp, %rbp
.LCFI1:
	subq	$48, %rsp
.LCFI2:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	%eax, -28(%rbp)
	movl	%eax, operatingsystem_parameter_argc(%rip)
	movq	-40(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -24(%rbp)
	movq	%rax, operatingsystem_parameter_argv(%rip)
	movq	-40(%rbp), %rdx
	addq	$16, %rdx
	movl	-28(%rbp), %eax
	cltq
	salq	$3, %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, operatingsystem_parameter_envp(%rip)
	movq	%rax, environ(%rip)
	cmpl	$0, -28(%rbp)
	jle	.L2
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L2
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, __progname(%rip)
	movq	__progname(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.L5
.L6:
	movq	-8(%rbp), %rax
	movzbl	(%rax), %eax
	cmpb	$47, %al
	jne	.L7
	movq	-8(%rbp), %rax
	addq	$1, %rax
	movq	%rax, __progname(%rip)
.L7:
	addq	$1, -8(%rbp)
.L5:
	movq	-8(%rbp), %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	jne	.L6
.L2:
	movl	$_DYNAMIC, %eax
	testq	%rax, %rax
	je	.L9
	movq	-48(%rbp), %rdi
	call	atexit
.L9:
	call	_init_tls
        call    _rtld_call_init
	movl	$_fini, %edi
	call	atexit
	call	_init
#	movq	-16(%rbp), %rdx
#	movq	-24(%rbp), %rsi
#	movl	-28(%rbp), %edi
	xorq   %rbp,%rbp  	
	call	main
	movl	%eax, %edi
	call	exit
.LFE5:
	.size	_start, .-_start
.weak __error
.type	__error, @function
__error:
.LFB9:
	
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%fs:0, %rdx
	movq	errno@gottpoff(%rip), %rax
	addq	%rdx, %rax
	popq	%rbp
	ret
	
.LFE9:
	.size	__error, .-__error

.bss
        .type   __stkptr,@object
        .size   __stkptr,8
        .global __stkptr
__stkptr:
        .skip   8

        .type operatingsystem_parameters,@object
        .size operatingsystem_parameters,24
operatingsystem_parameters:
        .skip 3*8

        .global operatingsystem_parameter_envp
        .global operatingsystem_parameter_argc
        .global operatingsystem_parameter_argv
        .set operatingsystem_parameter_envp,operatingsystem_parameters+0
        .set operatingsystem_parameter_argc,operatingsystem_parameters+8
        .set operatingsystem_parameter_argv,operatingsystem_parameters+16

	.comm	environ,8,8
	.weak	_DYNAMIC
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	"zR"
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.uleb128 0x1
	.byte	0x3
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.long	.LFB5
	.long	.LFE5-.LFB5
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI0-.LFB5
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE1:
	.ident	"[DragonFly]"
