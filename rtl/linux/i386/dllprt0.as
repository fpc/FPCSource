#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2001 by Peter Vreman
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
# Linux ELF shared library startup code for Free Pascal
#

        .file   "dllprt0.as"
        .text
        .globl  _startlib
        .type   _startlib,@function
_startlib:
        .globl  FPC_LIB_START
        .type   FPC_LIB_START,@function
FPC_LIB_START:
        pushl   %ebp
        movl    %esp,%ebp

        movl    8(%ebp),%eax
        movl    12(%ebp),%ecx
        movl    16(%ebp),%edx

        movl    %eax,U_SYSTEM_ARGC    /* Move the argument counter    */
        movl    %ecx,U_SYSTEM_ARGV    /* Move the argument pointer    */
        movl    %edx,U_SYSTEM_ENVP    /* Move the environment pointer */

        movb    $1,U_SYSTEM_ISLIBRARY

        call    PASCALMAIN

        leave
        ret

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movl    $1,%eax                 /* exit call */
        xorl    %ebx,%ebx
        movw    U_SYSTEM_EXITCODE,%bx
        int     $0x80
        jmp     _haltproc

#
# $Log$
# Revision 1.2  2002-09-07 16:01:20  peter
#   * old logs removed and tabs fixed
#
