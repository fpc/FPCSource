#
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
# FreeBSD standard (shared) ELF startup code for Free Pascal
#
# FreeBSD RunTime dynamic loader only
# calls the functions specified by -init
# as functions without parameters
# Thus, it is not possible to retrieve argc, argv and envp
# for libraries.
# On x86_64 CPU, using weak for the operatingsystem_parameter_XXX
# allows to bind to main program parameters,
# but this does not seem to work for i386 loader.
 
        .file   "dllprt0.as"
        .version        "01.01"
gcc2_compiled.:
.section        .rodata
.LC0:
        .ascii "\0"
.data
        .p2align 2
	.globl __progname
        .type    __progname,@object
        .size    __progname,4
__progname:
        .long .LC0
        .align  4
        .type   __fpucw,@object
        .size   __fpucw,4
        .global __fpucw
___fpucw:
        .long   0x1332

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4

.text
	.p2align 2,,3
	.globl FPC_LIB_START
	.type FPC_LIB_START,@function
FPC_LIB_START:
	.globl FPC_SHARED_LIB_START
	.type FPC_SHARED_LIB_START,@function
FPC_SHARED_LIB_START:
        movb    $1,operatingsystem_islibrary

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        /* xorl    %ebp,%ebp */

	call	PASCALMAIN@PLT
	ret

        pushl %eax
        jmp   _haltproc
        
.p2align 2,0x90
.globl _haltproc
.type _haltproc,@function

_haltproc:
	.globl FPC_SHARED_LIB_EXIT
	.type FPC_SHARED_LIB_EXIT,@function
FPC_SHARED_LIB_EXIT:
        call    FPC_LIB_EXIT@PLT
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
        .comm   environ,4,4
        .weak   _DYNAMIC
        .ident  "GCC: (GNU) 2.7.2.1"


__stkptr:
        .skip   4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

//.section .threadvar,"aw",@nobits
        .comm   ___fpc_threadvar_offset,4
