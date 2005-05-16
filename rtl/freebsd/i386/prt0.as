#
#   $Id: prt0.as,v 1.4 2004/07/03 21:50:30 daniel Exp $
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
# FreeBSD standard (static) ELF startup code for Free Pascal
#

        .file   "prt1.as"
        .version        "01.01"
gcc2_compiled.:
.globl __progname
.section        .rodata
.LC0:
        .ascii "\0"
.data
        .p2align 2
        .type    __progname,@object
        .size    __progname,4
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
        .p2align 2
.globl _start
        .type    _start,@function
_start:
        pushl %ebp
        movl %esp,%ebp
        pushl %edi
        pushl %esi
        pushl %ebx
#APP
        movl %edx,%edx
#NO_APP
        leal 8(%ebp),%edi
        movl %edi,operatingsystem_parameter_argv
        mov -4(%edi),%eax
        movl %eax,operatingsystem_parameter_argc
        movl 4(%ebp),%ebx
        leal 12(%ebp,%ebx,4),%esi
        movl %esi,operatingsystem_parameter_envp
        movl %esi,environ
        testl %ebx,%ebx
        jle .L2
        movl 8(%ebp),%eax
        testl %eax,%eax
        je .L2
        movl %eax,__progname
        cmpb $0,(%eax)
        je .L2
        .p2align 2,0x90
.L6:
        cmpb $47,(%eax)
        jne .L5
        leal 1(%eax),%ecx
        movl %ecx,__progname
.L5:
        incl %eax
        cmpb $0,(%eax)
        jne .L6
.L2:
.L9:

# copied from linux

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        xorl    %ebp,%ebp

        call main
        pushl %eax
        jmp   _haltproc
        
.p2align 2,0x90
.globl _haltproc
.type _haltproc,@function

_haltproc:
           mov $1,%eax  
           movzwl operatingsystem_result,%ebx
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
        .size    _start,.Lfe1-_start
        .comm   environ,4,4
        .weak   _DYNAMIC
        .ident  "GCC: (GNU) 2.7.2.1"

.bss
        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4
	
