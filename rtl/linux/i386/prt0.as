#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael Van Canneyt
#   member of the Free Pascal development team.
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
# Linux a.out startup code for Free Pascal
#

        .file "prt0.as"
        .text
        .globl  __entry
__entry:
        movl    8(%esp),%eax            /* Move the environment pointer */
        movl    4(%esp),%ebx            /* Move the argument pointer */
        movl    (%esp),%ecx             /* Move the argument counter */

        movl    %eax,U_SYSLINUX_ENVP
        movl    %ebx,U_SYSLINUX_ARGV
        movl    %ecx,U_SYSLINUX_ARGC

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movl    $1,%eax                 /* exit call */
        xorl    %ebx,%ebx
        movw    U_SYSLINUX_EXITCODE,%bx
        int     $0x80
        jmp     _haltproc

.data
        .align  4
___fpucw:
        .long   0x1332

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

#
# $Log$
# Revision 1.5  1998-11-04 10:16:28  peter
#   + xorl ebp,ebp to indicate end of backtrace
#
# Revision 1.4  1998/10/14 21:28:49  peter
#   * initialize fpu so sigfpe is finally generated for fpu errors
#
# Revision 1.3  1998/08/08 14:42:11  peter
#   * added missing ___fpc_sbrk and logs
#
#
