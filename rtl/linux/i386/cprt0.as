#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman
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
# Linux ELF startup code for Free Pascal
#

        .file   "prt1.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx
        movl    %esp,%ebx               /* Points to the arguments */
        movl    %ecx,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,U_SYSTEM_ENVP    /* Move the environment pointer */
        movl    %ecx,U_SYSTEM_ARGC    /* Move the argument counter    */
        movl    %ebx,U_SYSTEM_ARGV    /* Move the argument pointer    */

        movl    %eax,__environ          /* libc environ */

        pushl   %eax
        pushl   %ebx
        pushl   %ecx

        call    __libc_init             /* init libc */
        movzwl  __fpu_control,%eax
        pushl   %eax
        call    __setfpucw
        addl    $4,%esp
        pushl   $_fini
        call    atexit
        addl    $4,%esp
        call    _init

        popl    %eax
        popl    %eax

        xorl    %ebp,%ebp
        call    PASCALMAIN              /* start the program */

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        xorl    %ebx,%ebx               /* load and save exitcode */
        movw    U_SYSTEM_EXITCODE,%bx
        pushl   %ebx

        call    exit                    /* call libc exit, this will */
                                        /* write the gmon.out */

        movl    $1,%eax                 /* exit call */
        popl    %ebx
        int     $0x80
        jmp     _haltproc

.data
        .align  4

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0


#
# $Log$
# Revision 1.3  2002-09-07 16:01:20  peter
#   * old logs removed and tabs fixed
#
