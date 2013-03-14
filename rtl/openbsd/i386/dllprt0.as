	.section ".note.openbsd.ident", "a"
	.p2align 2
	.long	8
	.long	4
	.long	1
	.ascii "OpenBSD\0"
	.long	0
	.previous
	.file	"crt0.c"
gcc2_compiled.:
	.globl	__progname
	.section	.rodata
.LC0:
	.string	""
	.section	.data.rel.local,"aw",@progbits
	.align 4
	.type	__progname, @object
	.size	__progname, 4
__progname:
	.long	.LC0
#APP
	.text
	.align  4
	.globl  __start
	.globl  _start
_start:
__start:
	pushl	%ebx			#ps_strings
	pushl   %ecx                    # obj
	pushl   %edx                    # cleanup
	movl    12(%esp),%eax
	leal    20(%esp,%eax,4),%ecx
	leal    16(%esp),%edx
	pushl   %ecx
	pushl   %edx
	pushl   %eax
	call    ___start 
#NO_APP
	.text
	.globl	___start
	.type	___start, @function
___start:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$36, %esp
	call	__i686.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	environ@GOT(%ebx), %edx
	movl	16(%ebp), %eax
	movl	%eax, (%edx)
	movl	operatingsystem_parameter_envp@GOT(%ebx), %edx
	movl	16(%ebp), %eax
	movl	%eax, (%edx)
	movl	operatingsystem_parameter_argc@GOT(%ebx), %edx
	movl	8(%ebp), %eax
	movl	%eax, (%edx)
	movl	operatingsystem_parameter_argv@GOT(%ebx), %edx
	movl	12(%ebp), %eax
	movl	%eax, (%edx)
	movl	12(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -8(%ebp)
	cmpl	$0, -8(%ebp)
	je	.L2
	movl	$47, 4(%esp)
	movl	-8(%ebp), %eax
	movl	%eax, (%esp)
	call	_strrchr
	movl	%eax, %edx
	movl	__progname@GOT(%ebx), %eax
	movl	%edx, (%eax)
	movl	__progname@GOT(%ebx), %eax
	movl	(%eax), %eax
	testl	%eax, %eax
	jne	.L4
	movl	__progname@GOT(%ebx), %edx
	movl	-8(%ebp), %eax
	movl	%eax, (%edx)
	jmp	.L6
.L4:
	movl	__progname@GOT(%ebx), %eax
	movl	(%eax), %eax
	leal	1(%eax), %edx
	movl	__progname@GOT(%ebx), %eax
	movl	%edx, (%eax)
.L6:
	movl	__progname_storage@GOT(%ebx), %eax
	movl	%eax, -12(%ebp)
	jmp	.L7
.L8:
	movl	__progname@GOT(%ebx), %eax
	movl	(%eax), %ecx
	movzbl	(%ecx), %edx
	movl	-12(%ebp), %eax
	movb	%dl, (%eax)
	addl	$1, -12(%ebp)
	leal	1(%ecx), %edx
	movl	__progname@GOT(%ebx), %eax
	movl	%edx, (%eax)
.L7:
	movl	__progname@GOT(%ebx), %eax
	movl	(%eax), %eax
	movzbl	(%eax), %eax
	testb	%al, %al
	je	.L9
	movl	__progname_storage@GOT(%ebx), %eax
	leal	255(%eax), %eax
	cmpl	%eax, -12(%ebp)
	jb	.L8
.L9:
	movl	-12(%ebp), %eax
	movb	$0, (%eax)
	movl	__progname@GOT(%ebx), %edx
	movl	__progname_storage@GOT(%ebx), %eax
	movl	%eax, (%edx)
.L2:
	call	__init@PLT
	movl	environ@GOT(%ebx), %eax
	movl	(%eax), %eax
	movl	%eax, 8(%esp)
	movl	12(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	main@PLT
#	pushl environ
#	pushl %esi
#	pushl 8(%ebp)
	finit
	fwait
	fldcw ___fpucw
	xorl  %ebp,%ebp
	call main
	pushl %eax
	call exit
        .p2align 2,0x90

#	movl	%eax, (%esp)
#	call	exit@PLT
	.size	___start, .-___start

.globl _haltproc
.type _haltproc,@function

_haltproc:
	mov $1,%eax
	call	__i686.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl operatingsystem_result@GOT(%ebx), %esi
        movzwl (%esi),%edx
        pushl %edx
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
	.type	_strrchr, @function
_strrchr:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$12, %esp
	movl	12(%ebp), %eax
	movb	%al, -9(%ebp)
	movl	$0, -4(%ebp)
.L13:
	movl	8(%ebp), %edx
	movzbl	(%edx), %eax
	cmpb	-9(%ebp), %al
	jne	.L14
	movl	8(%ebp), %eax
	movl	%eax, -4(%ebp)
.L14:
	movl	8(%ebp), %edx
	movzbl	(%edx), %eax
	testb	%al, %al
	jne	.L16
	movl	-4(%ebp), %eax
	movl	%eax, -8(%ebp)
	jmp	.L12
.L16:
	addl	$1, 8(%ebp)
	jmp	.L13
.L12:
	movl	-8(%ebp), %eax
	leave
	ret
	.size	_strrchr, .-_strrchr
	.comm	environ,4,4
	.comm	operatingsystem_parameter_envp,4,4
	.comm	operatingsystem_parameter_argc,4,4
	.comm	operatingsystem_parameter_argv,4,4
	.comm	__progname_storage,256,32
	.ident	"GCC: (GNU) 4.2.1 20070719 "
	.section	.gnu.linkonce.t.__i686.get_pc_thunk.bx,"ax",@progbits
	.globl	__i686.get_pc_thunk.bx
	.hidden	__i686.get_pc_thunk.bx
	.type	__i686.get_pc_thunk.bx, @function
__i686.get_pc_thunk.bx:
	movl	(%esp), %ebx
	ret
