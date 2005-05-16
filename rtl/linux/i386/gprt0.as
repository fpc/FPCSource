#
#   $Id: gprt0.as,v 1.4 2004/07/03 21:50:31 daniel Exp $
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
# Linux ELF startup code with profiling support for Free Pascal
# Note: Needs linking with -lgmon and -lc
#

        .file   "gprt1.as"
        .text
        .globl _start
        .type _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx
        movl    %esp,%ebx               /* Points to the arguments */
        movl    %ecx,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,operatingsystem_parameter_envp    /* Move the environment pointer */
        movl    %ecx,operatingsystem_parameter_argc    /* Move the argument counter    */
        movl    %ebx,operatingsystem_parameter_argv    /* Move the argument pointer    */

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        pushl   $_etext                 /* Initialize gmon */
        pushl   $_start
        call    monstartup
        addl    $8,%esp
        pushl   $_mcleanup
        call    atexit
        addl    $4,%esp

        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
_haltproc2:             # GAS <= 2.15 bug: generates larger jump if a label is exported
        movzwl  operatingsystem_result,%ebx
        pushl   %ebx
        call    exit                    /* call libc exit, this will */
                                        /* write the gmon.out */
        xorl    %eax,%eax
        incl    %eax                    /* eax=1, exit call */
        popl    %ebx
        int     $0x80
        jmp     _haltproc2

.data
___fpucw:
        .long   0x1332

.bss
        .type   ___fpc_brk_addr,@object
        .comm   ___fpc_brk_addr,4        /* heap management */

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

#
# $Log: gprt0.as,v $
# Revision 1.4  2004/07/03 21:50:31  daniel
#   * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
#     longer necessary
#
# Revision 1.3  2002/09/07 16:01:20  peter
#   * old logs removed and tabs fixed
#
