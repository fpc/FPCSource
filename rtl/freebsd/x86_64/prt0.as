	.file	"crt1.c"
#APP
	.ident	"$FreeBSD: src/lib/csu/common/crtbrand.c,v 1.4 2003/10/17 15:43:13 peter Exp $"
#NO_APP
	.section	.note.ABI-tag,"a",@progbits
	.p2align 2
	.type	abitag, @object
	.size	abitag, 24
abitag:
	.long	8
	.long	4
	.long	1
	.string	"FreeBSD"
	.long	502110
	.section	.rodata
.LC0:
	.string	""
.globl __progname
	.data
	.p2align 3
	.type	__progname, @object
	.size	__progname, 8
__progname:
	.quad	.LC0
	.text
	.p2align 2,,3
.globl _start
	.type	_start, @function
_start:
.LFB9:
	pushq	%rbp
.LCFI0:
	movq	%rsp, %rbp
.LCFI1:
	subq	$48, %rsp
.LCFI2:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, -20(%rbp)
	movl	%eax, operatingsystem_parameter_argc
	movq	-8(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -32(%rbp)
	movq    %rax, operatingsystem_parameter_argv
	movl	-20(%rbp), %eax
	cltq
	salq	$3, %rax
	addq	-8(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -40(%rbp)
	movq    %rax, operatingsystem_parameter_envp
	movq	-40(%rbp), %rax
	movq	%rax, environ(%rip)
	movq    %rax,environ
	cmpl	$0, -20(%rbp)
	jle	.L5
	movq	-32(%rbp), %rax
	cmpq	$0, (%rax)
	je	.L5
	movq	-32(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, __progname(%rip)
	movq	__progname(%rip), %rax
	movq	%rax, -48(%rbp)
.L6:
	movq	-48(%rbp), %rax
	cmpb	$0, (%rax)
	jne	.L9
	jmp	.L5
.L9:
	movq	-48(%rbp), %rax
	cmpb	$47, (%rax)
	jne	.L8
	movq	-48(%rbp), %rax
	incq	%rax
	movq	%rax, __progname(%rip)
.L8:
	leaq	-48(%rbp), %rax
	incq	(%rax)
	jmp	.L6
.L5:
# 	movl	$_DYNAMIC, %eax
# 	testq	%rax, %rax
# 	je	.L11
# 	movq	-16(%rbp), %rdi
# 	call	atexit
.L11:
# 	movl	$_fini, %edi
# 	call	atexit
#	call	_init
#	movq	-40(%rbp), %rdx    #env
#	movq	-32(%rbp), %rsi  #argv
#	movl	-20(%rbp), %edi  # argc
	xorq    %rbp,%rbp
	call	main
	movl	%eax, %edi
	call	exit
.LFE9:
	.size	_start, .-_start
#APP
	.ident	"$FreeBSD: src/lib/csu/amd64/crt1.c,v 1.13 2003/04/30 19:27:07 peter Exp $"
#NO_APP
	.comm	environ,8,8
	.weak	_DYNAMIC
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	""
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.p2align 3
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.quad	.LFB9
	.quad	.LFE9-.LFB9
	.byte	0x4
	.long	.LCFI0-.LFB9
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xd
	.uleb128 0x6
	.p2align 3
.LEFDE1:
	.ident	"GCC: (GNU) 3.3.3 [FreeBSD] 20031106"
