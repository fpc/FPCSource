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
        popl    %esi
        movl    %eax,%edi

        movl    %esp,%ebx               /* Points to the arguments */
        movl    %esi,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,U_SYSTEM_ENVP    /* Move the environment pointer */
        movl    %esi,U_SYSTEM_ARGC    /* Move the argument counter    */
        movl    %ebx,U_SYSTEM_ARGV    /* Move the argument pointer    */

        movl    %edi,%eax
        xorl    %ebp,%ebp
        pushl   %eax
        pushl   %esp
        pushl   %edx
        pushl   $_fini_dummy
        pushl   $_init_dummy
        pushl   %ebx
        pushl   %esi
        pushl   $main
        call    __libc_start_main
        hlt

/* fake main routine which will be run from libc */
        .globl  cmain
        .type   cmain,@object
cmain:
        /* save return address */
        popl    %eax
        movl    %eax,___fpc_ret
        movl    %ebx,___fpc_ret_ebx
        movl    %esi,___fpc_ret_esi
        movl    %edi,___fpc_ret_edi
        pushl   %eax

        /* start the program */
        call    PASCALMAIN
        hlt

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        xorl    %eax,%eax               /* load and save exitcode */
        movw    U_SYSTEM_EXITCODE,%ax

        movl    ___fpc_ret,%edx         /* return to libc */
        movl    ___fpc_ret_ebx,%ebx
        movl    ___fpc_ret_esi,%esi
        movl    ___fpc_ret_edi,%edi
        push    %edx
_init_dummy:
_fini_dummy:
        ret

        .globl  __gmon_start__
        .type   __gmon_start__,@object
__gmon_start__:
        pushl   %ebp
        movl    __monstarted,%eax
        leal    0x1(%eax),%edx
        movl    %esp,%ebp
        movl    %edx,__monstarted
        testl   %eax,%eax
        jnz     .Lnomonstart
        pushl   $etext                  /* Initialize gmon */
        pushl   $_start
        call    monstartup
        addl    $8,%esp
        pushl   $_mcleanup
        call    atexit
        addl    $4,%esp
.Lnomonstart:
        movl   %ebp,%esp
        popl   %ebp
        ret

.data
        .align  4

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

___fpc_ret:                             /* return address to libc */
        .long   0
___fpc_ret_ebx:
        .long   0
___fpc_ret_esi:
        .long   0
___fpc_ret_edi:
        .long   0

.bss
        .lcomm __monstarted,4

#
# $Log$
# Revision 1.4  2002-09-07 16:01:20  peter
#   * old logs removed and tabs fixed
#
