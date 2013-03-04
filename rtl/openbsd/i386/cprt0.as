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
.data
	.align 32
	.type	 rcsid , @object
	.size	rcsid , 58
rcsid:
	.string	"$OpenBSD: crt0.c,v 1.11 2003/06/27 22:30:38 deraadt Exp $"
.globl __progname
.section	.rodata
.LC0:
	.string	""
.data
	.align 4
	.type	 __progname , @object
	.size	__progname , 4
__progname:
	.long .LC0
.global __progname_storage
	.type __progname_storage, @ object
	.size  __progname_storage, 256

        .align  4
___fpucw:
        .long   0x1332

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

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
	.align 4
.globl ___start
	.type	___start , @function
___start:
	pushl %ebp
	movl %esp,%ebp
	subl $16,%esp
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%esi
	movl 16(%ebp),%eax
	movl %eax,environ
	movl %eax,operatingsystem_parameter_envp
	movl (%esi),%ebx
	testl %ebx,%ebx
	je .L3
	addl $-8,%esp
	pushl $47
	pushl %ebx
	call _strrchr
	movl %eax,__progname
	addl $16,%esp
	testl %eax,%eax
	jne .L4
	movl %ebx,__progname
	jmp .L5
	.p2align 4,,7
.L4:
	incl %eax
	movl %eax,__progname
.L5:
	movl $__progname_storage,%edx
	jmp .L12
	.p2align 4,,7
.L9:
	movb (%eax),%al
	movb %al,(%edx)
	incl __progname
	incl %edx
.L12:
	movl __progname,%eax
	cmpb $0,(%eax)
	je .L7
	cmpl $__progname_storage+255,%edx
	jb .L9
.L7:
	movb $0,(%edx)
	movl $__progname_storage,__progname
.L3:
	call __init
	subl $16,%esp
	pushl %eax
	movl 8(%ebp),%eax
	movl %eax,operatingsystem_parameter_argc
	movl %esi,operatingsystem_parameter_argv
	popl %eax
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

.globl _haltproc
.type _haltproc,@function

_haltproc:
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

	.size	___start , . - ___start
	.align 4
	.type	_strrchr , @function
_strrchr:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%eax
	movb 12(%ebp),%bl
	xorl %ecx,%ecx
	.p2align 4,,7
.L14:
	movb (%eax),%dl
	cmpb %bl,%dl
	jne .L17
	movl %eax,%ecx
.L17:
	testb %dl,%dl
	je .L16
	incl %eax
	jmp .L14
	.p2align 4,,7
.L16:
	movl %ecx,%eax
	popl %ebx
	leave
	ret
	.size	_strrchr , . - _strrchr
	.comm	environ,4,4
	.comm	__progname_storage,256,32
        .comm   operatingsystem_parameter_envp,4,4
        .comm   operatingsystem_parameter_argc,4,4
        .comm   operatingsystem_parameter_argv,4,4

