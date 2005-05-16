#
#   $Id: cprt0.as,v 1.2 2004/01/04 01:13:23 marco Exp $
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
# NetBSD standard (shared) ELF/i386 startup code for Free Pascal
#

	.file	"crt0.c"
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
	.long 0x1332	
        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

#APP
	.weak _DYNAMIC
	
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
	movl	%eax,U_SYSTEM_ARGC
	movl	%edx,U_SYSTEM_ARGV
	call	___start

#NO_APP
.text
	.align 4
.globl ___start
	.type	 ___start,@function
___start:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	call .L12
.L12:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L12],%ebx
	movl 12(%ebp),%esi
	movl 16(%ebp),%edx
	movl 28(%ebp),%edi
	movl environ@GOT(%ebx),%eax
	movl %edx,(%eax)
	movl %edx,U_SYSTEM_ENVP
	movl __progname@GOT(%ebx),%edx
	movl (%esi),%eax
	movl %eax,(%edx)
	testl %eax,%eax
	je .L6
	pushl $47
	movl __progname@GOT(%ebx),%eax
	pushl (%eax)
	call _strrchr@PLT
	movl %eax,%edx
	movl __progname@GOT(%ebx),%eax
	movl %edx,(%eax)
	addl $8,%esp
	testl %edx,%edx
	jne .L7
	movl __progname@GOT(%ebx),%edx
	movl (%esi),%eax
	movl %eax,(%edx)
	jmp .L6
	.align 4
.L7:
	movl __progname@GOT(%ebx),%eax
	incl %edx
	movl %edx,(%eax)
.L6:
	testl %edi,%edi
	je .L9
	movl __ps_strings@GOT(%ebx),%eax
	movl %edi,(%eax)
.L9:
	cmpl $0,_DYNAMIC@GOT(%ebx)
	je .L10
	pushl 24(%ebp)
	pushl 20(%ebp)
	call _rtld_setup@PLT
	addl $8,%esp
.L10:
	pushl _fini@GOT(%ebx)
	call atexit@PLT
	call _init@PLT
#	movl environ@GOT(%ebx),%eax
#	pushl (%eax)
#	pushl %esi
#	pushl 8(%ebp)
#	call main@PLT

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

//        xorl    %ebp,%ebp               /* Detect main from nested */
                                        /* procs/unwinding? */
	pushl (%eax)
	pushl (%esi)
	pushl 8(%ebp)
        call main
        pushl %eax
	pushl %eax
	call exit@PLT

.p2align 2,0x90
.globl _haltproc
.type _haltproc,@function

_haltproc:
           mov $1,%eax
           movzwl U_SYSTEM_EXITCODE,%ebx
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
	subl $4,%esp
	pushl %esi
	movl 8(%ebp),%eax
	movb 12(%ebp),%cl
	movb %cl,-1(%ebp)
	xorl %esi,%esi
	.align 4
.L14:
	movb (%eax),%dl
	cmpb -1(%ebp),%dl
	jne .L17
	movl %eax,%esi
.L17:
	testb %dl,%dl
	je .L16
	incl %eax
	jmp .L14
	.align 4
.L16:
	movl %esi,%eax
	movl -8(%ebp),%esi
	leave
	ret
.Lfe2:
	.size	 _strrchr,.Lfe2-_strrchr
.section	.rodata
	.align 32
.LC1:
	.ascii "Corrupt Obj_Entry pointer in GOT\0"
	.align 32
.LC2:
	.ascii "Dynamic linker version mismatch\0"
.text
	.align 4
.globl _rtld_setup
	.type	 _rtld_setup,@function
_rtld_setup:
	pushl %ebp
	movl %esp,%ebp
	pushl %esi
	pushl %ebx
	call .L35
.L35:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L35],%ebx
	movl 12(%ebp),%esi
	testl %esi,%esi
	je .L22
	cmpl $-716130182,(%esi)
	je .L21
.L22:
	pushl $33
	leal .LC1@GOTOFF(%ebx),%eax
	pushl %eax
	pushl $2
	pushl $4
	call __syscall@PLT
	pushl $1
	pushl $1
	call __syscall@PLT
	addl $24,%esp
	.align 4
.L21:
	cmpl $1,4(%esi)
	je .L28
	pushl $32
	leal .LC2@GOTOFF(%ebx),%eax
	pushl %eax
	pushl $2
	pushl $4
	call __syscall@PLT
	pushl $1
	pushl $1
	call __syscall@PLT
	addl $24,%esp
	.align 4
.L28:
	pushl 8(%ebp)
	call atexit@PLT
	leal -8(%ebp),%esp
	popl %ebx
	popl %esi
	leave
	ret
.Lfe3:
	.size	 _rtld_setup,.Lfe3-_rtld_setup
#APP
	.weak dlopen ; dlopen = _dlopen
	.weak dlclose ; dlclose = _dlclose
	.weak dlsym ; dlsym = _dlsym
	.weak dlerror ; dlerror = _dlerror
	.weak dladdr ; dladdr = _dladdr
#NO_APP
	.align 4
.globl _dlopen
	.type	 _dlopen,@function
_dlopen:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	call .L40
.L40:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L40],%ebx
	movl __mainprog_obj@GOT(%ebx),%eax
	movl (%eax),%eax
	testl %eax,%eax
	je .L37
	pushl 12(%ebp)
	pushl 8(%ebp)
	movl 80(%eax),%eax
	call *%eax
	jmp .L38
	.align 4
.L37:
	xorl %eax,%eax
.L38:
	movl -4(%ebp),%ebx
	leave
	ret
.Lfe4:
	.size	 _dlopen,.Lfe4-_dlopen
	.align 4
.globl _dlclose
	.type	 _dlclose,@function
_dlclose:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	call .L45
.L45:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L45],%ebx
	movl __mainprog_obj@GOT(%ebx),%eax
	movl (%eax),%eax
	testl %eax,%eax
	je .L42
	pushl 8(%ebp)
	movl 92(%eax),%eax
	call *%eax
	jmp .L43
	.align 4
.L42:
	movl $-1,%eax
.L43:
	movl -4(%ebp),%ebx
	leave
	ret
.Lfe5:
	.size	 _dlclose,.Lfe5-_dlclose
	.align 4
.globl _dlsym
	.type	 _dlsym,@function
_dlsym:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	call .L50
.L50:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L50],%ebx
	movl __mainprog_obj@GOT(%ebx),%eax
	movl (%eax),%eax
	testl %eax,%eax
	je .L47
	pushl 12(%ebp)
	pushl 8(%ebp)
	movl 84(%eax),%eax
	call *%eax
	jmp .L48
	.align 4
.L47:
	xorl %eax,%eax
.L48:
	movl -4(%ebp),%ebx
	leave
	ret
.Lfe6:
	.size	 _dlsym,.Lfe6-_dlsym
.section	.rodata
	.align 32
.LC3:
	.ascii "Dynamic linker interface not available\0"
.text
	.align 4
.globl _dlerror
	.type	 _dlerror,@function
_dlerror:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	call .L55
.L55:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L55],%ebx
	movl __mainprog_obj@GOT(%ebx),%eax
	movl (%eax),%eax
	testl %eax,%eax
	je .L52
	movl 88(%eax),%eax
	call *%eax
	jmp .L53
	.align 4
.L52:
	leal .LC3@GOTOFF(%ebx),%eax
.L53:
	movl -4(%ebp),%ebx
	leave
	ret
.Lfe7:
	.size	 _dlerror,.Lfe7-_dlerror
	.align 4
.globl _dladdr
	.type	 _dladdr,@function
_dladdr:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	call .L60
.L60:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L60],%ebx
	movl __mainprog_obj@GOT(%ebx),%eax
	movl (%eax),%eax
	testl %eax,%eax
	je .L57
	pushl 12(%ebp)
	pushl 8(%ebp)
	movl 96(%eax),%eax
	call *%eax
	jmp .L58
	.align 4
.L57:
	movl $-1,%eax
.L58:
	movl -4(%ebp),%ebx
	leave
	ret
.Lfe8:
	.size	 _dladdr,.Lfe8-_dladdr
	.comm	environ,4,4
	.comm	__mainprog_obj,4,4



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
