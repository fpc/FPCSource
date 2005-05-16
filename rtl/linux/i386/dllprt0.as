#
#   $Id: dllprt0.as,v 1.3 2004/07/03 21:50:31 daniel Exp $
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

        movl    %edx,operatingsystem_parameter_envp    /* Move the environment pointer */
        movl    %eax,operatingsystem_parameter_argc    /* Move the argument counter    */
        movl    %ecx,operatingsystem_parameter_argv    /* Move the argument pointer    */

        movb    $1,U_SYSTEM_ISLIBRARY

        call    PASCALMAIN

        leave
        ret

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
_haltproc2:             # GAS <= 2.15 bug: generates larger jump if a label is exported
        xorl    %eax,%eax
        incl    %eax                    /* eax=1, exit call */
        movzwl  operatingsystem_result,%ebx
        int     $0x80
        jmp     _haltproc2

.bss
        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

#
# $Log: dllprt0.as,v $
# Revision 1.3  2004/07/03 21:50:31  daniel
#   * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
#     longer necessary
#
# Revision 1.2  2002/09/07 16:01:20  peter
#   * old logs removed and tabs fixed
#
