	.file	"crt0.c"
	.globl	__progname
	.section	.rodata
.LC0:
	.string	""
	.data
	.align 8
	.type	__progname, @object
	.size	__progname, 8
__progname:
	.quad	.LC0
#APP
	 .text				
	.align	8			
	.globl	__start			
	.globl	_start			
_start:					
__start:				
	movq	%rbx,%r9		
	movq	%rcx,%r8		
	movq	%rdx,%rcx		
	movq	(%rsp),%rdi		
	leaq	16(%rsp,%rdi,8),%rdx	
	leaq	8(%rsp),%rsi		
	subq	$8,%rsp			
	andq	$~15,%rsp		
	addq	$8,%rsp			
	jmp	___start		

#NO_APP
	.text
	.globl	___start
	.type	___start, @function
___start:
.LFB9:
	pushq	%rbp
.LCFI0:
	movq	%rsp, %rbp
.LCFI1:
	subq	$64, %rsp
.LCFI2:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	%r8, -56(%rbp)
	movq	%r9, -64(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, environ(%rip)
	movq    %rax,operatingsystem_parameter_envp(%rip)
	movq	-32(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -8(%rbp)
	cmpq	$0, -8(%rbp)
	je	.L2
	movq	-8(%rbp), %rdi
	movl	$47, %esi
	call	_strrchr
	movq	%rax, __progname(%rip)
	movq	__progname(%rip), %rax
	testq	%rax, %rax
	jne	.L4
	movq	-8(%rbp), %rax
	movq	%rax, __progname(%rip)
	jmp	.L6
.L4:
	movq	__progname(%rip), %rax
	addq	$1, %rax
	movq	%rax, __progname(%rip)
.L6:
	movq	$__progname_storage, -16(%rbp)
	jmp	.L7
.L8:
	movq	__progname(%rip), %rcx
	movzbl	(%rcx), %edx
	movq	-16(%rbp), %rax
	movb	%dl, (%rax)
	addq	$1, -16(%rbp)
	leaq	1(%rcx), %rax
	movq	%rax, __progname(%rip)
.L7:
	movq	__progname(%rip), %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	je	.L9
	movl	$__progname_storage+255, %eax
	cmpq	%rax, -16(%rbp)
	jb	.L8
.L9:
	movq	-16(%rbp), %rax
	movb	$0, (%rax)
	movq	$__progname_storage, __progname(%rip)
.L2:
	movl	$_mcleanup, %edi
	call	atexit
	movl	$_etext, %eax
	movq	%rax, %rsi
	movl	$_eprol, %eax
	movq	%rax, %rdi
	call	monstartup
	movl	$0, %eax
	call	__init
	movq	environ(%rip), %rdx
	movq	-32(%rbp), %rsi
	movl	-20(%rbp), %edi
	movq    %rdi,operatingsystem_parameter_argc(%rip)
	movq    %rsi,operatingsystem_parameter_argv(%rip)
	movl	$0, %eax
	call	main
	# movl	%eax, %edi
	# call	exit
	jmp _haltproc
        .p2align 2,0x90

.globl _haltproc
.type _haltproc,@function

_haltproc:
           movq $1,%rax
           movzwq operatingsystem_result(%rip),%rbx
           pushq   %rbx
           call .Lactualsyscall
           addq  $8,%rsp
           jmp   _haltproc

.Lactualsyscall:
         int $0x80
         jb .LErrorcode
         xor %rbx,%rbx
         ret
.LErrorcode:
         movq  %rax,%rbx
         movq  $-1,%rax
.LFE9:
	.size	___start, .-___start
	.type	_strrchr, @function
_strrchr:
.LFB10:
	pushq	%rbp
.LCFI3:
	movq	%rsp, %rbp
.LCFI4:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	$0, -8(%rbp)
.L13:
	movq	-24(%rbp), %rdx
	movzbl	(%rdx), %eax
	cmpb	-25(%rbp), %al
	jne	.L14
	movq	-24(%rbp), %rax
	movq	%rax, -8(%rbp)
.L14:
	movq	-24(%rbp), %rdx
	movzbl	(%rdx), %eax
	testb	%al, %al
	jne	.L16
	movq	-8(%rbp), %rax
	movq	%rax, -16(%rbp)
	jmp	.L12
.L16:
	addq	$1, -24(%rbp)
	jmp	.L13
.L12:
	movq	-16(%rbp), %rax
	leave
	ret
.LFE10:
	.size	_strrchr, .-_strrchr
#APP
	  .text
	_eprol:
#NO_APP
	.comm	environ,8,8
	.comm	__progname_storage,256,32
        .comm   operatingsystem_parameter_envp,8,8
        .comm   operatingsystem_parameter_argc,8,8
        .comm   operatingsystem_parameter_argv,8,8
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
	.long	.LFB9
	.long	.LFE9-.LFB9
	.uleb128 0x0
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
	.align 8
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.long	.LFB10
	.long	.LFE10-.LFB10
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI3-.LFB10
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI4-.LCFI3
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE3:
	.ident	"GCC: (GNU) 4.2.1 20070719 "
