#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1998-2000 by Michael Van Canneyt and Peter Vreman
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

        movl    %eax,U_SYSLINUX_ENVP    /* Move the environment pointer */
        movl    %ecx,U_SYSLINUX_ARGC    /* Move the argument counter    */
        movl    %ebx,U_SYSLINUX_ARGV    /* Move the argument pointer    */

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
        movw    U_SYSLINUX_EXITCODE,%bx
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

        .globl  __curbrk                /* necessary for libc */
        .type   __curbrk,@object
        .size   __curbrk,4
__curbrk:
        .long   0

        .globl  __environ
        .type   __environ,@object
        .size   __environ,4
__environ:
        .long   0

#
# $Log$
# Revision 1.2  2000-01-07 16:32:28  daniel
#   * copyright 2000 added
#
# Revision 1.1  1999/11/08 23:07:48  peter
#   * removed aout entries
#
# Revision 1.3  1998/11/04 10:16:25  peter
#   + xorl ebp,ebp to indicate end of backtrace
#
# Revision 1.2  1998/10/14 21:28:46  peter
#   * initialize fpu so sigfpe is finally generated for fpu errors
#
# Revision 1.1  1998/08/12 19:16:09  peter
#   + loader including libc init and exit
#
#
