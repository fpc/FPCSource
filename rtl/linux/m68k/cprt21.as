|
|   $Id: cprt21.as,v 1.1.2.5 2002/02/28 22:44:44 pierre Exp $
|   This file is part of the Free Pascal run time library.
|   Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman
|   members of the Free Pascal development team.
|
|   See the file COPYING.FPC, included in this distribution,
|   for details about the copyright.
|
|   This program is distributed in the hope that it will be useful,
|   but WITHOUT ANY WARRANTY;without even the implied warranty of
|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
|
|**********************************************************************}
|
| Linux ELF startup code for Free Pascal
|

        .file   "prt1.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */
        move.l    (%sp)+,%d3
        move.l    %d0,%d4

        move.l    %sp,%d1               /* Points to the arguments */
        move.l    %d3,%d0
        addq.l    #1,%d0
        lsl.l     #2,%d0
        add.l     %sp,%d0

        move.l    %sp,%d7
        and.l     #0xfffffff8,%d7        /* Align stack */
        move.l    %d7,%sp

        move.l    %d0,U_SYSLINUX_ENVP    /* Move the environment pointer */
        move.l    %d3,U_SYSLINUX_ARGC    /* Move the argument counter    */
        move.l    %d1,U_SYSLINUX_ARGV    /* Move the argument pointer    */

        move.l   #0,%fp                  /* Zero frame pointer to end call stack */

|
|       Start of args for __libc_start_main
|
|
        move.l   %d4,-(%sp)
        move.l   %sp,-(%sp)
        move.l   %a1,-(%sp)
        pea.l    _fini_dummy
        pea.l    _init_dummy
        move.l   %d1,-(%sp)
        move.l   %d3,-(%sp)
        pea.l    main
        jsr      __libc_start_main
        trap     #0

/* fake main routine which will be run from libc */
main:
        /* save return address */
        move.l    (%sp)+,%d0
        move.l    %d0,___fpc_ret
        move.l    %d1,___fpc_ret_d1
        move.l    %fp,___fpc_ret_fp
        move.l    %d0,-(%sp)

        /* start the program */
        move.l   #0,%fp
        jsr      PASCALMAIN

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        eor.l    %d0,%d0               /* load and save exitcode */
        move.w   U_SYSLINUX_EXITCODE,%d0

        move.l    ___fpc_ret,%d3         /* return to libc */
        move.l    ___fpc_ret_fp,%fp
        move.l    ___fpc_ret_d1,%d1
        move.l    %d3,-(%sp)
_init_dummy:
_fini_dummy:
        rts

.data
        .align  4

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

___fpc_ret:                             /* return address to libc */
        .long   0
___fpc_ret_d1:
        .long   0
___fpc_ret_fp:
        .long   0


|
| $Log: cprt21.as,v $
| Revision 1.1.2.5  2002/02/28 22:44:44  pierre
|  + add some comments
|
| Revision 1.1.2.4  2001/08/02 21:23:59  pierre
|  * fix the args to __libc_start_main
|
| Revision 1.1.2.3  2001/07/30 20:05:07  pierre
|  * fix a assembler syntax eror in last commit
|
| Revision 1.1.2.2  2001/07/30 16:18:53  pierre
|  * converted from i386 code
|
| Revision 1.1.2.2  2001/06/04 18:04:32  peter
|   * use own dummies for _init and _fini
|
| Revision 1.1.2.1  2001/02/14 22:18:45  pierre
|  * fix Sebastian's problem with HeapTrace
|
| Revision 1.1  2000/07/13 06:30:55  michael
| + Initial import
|
| Revision 1.3  2000/01/07 16:41:42  daniel
|   * copyright 2000
|
| Revision 1.2  2000/01/07 16:32:28  daniel
|   * copyright 2000 added
|
| Revision 1.1  1999/05/03 21:29:36  peter
|   + glibc 2.1 support
|
| Revision 1.3  1998/11/04 10:16:25  peter
|   + xorl fp,fp to indicate end of backtrace
|
| Revision 1.2  1998/10/14 21:28:46  peter
|   * initialize fpu so sigfpe is finally generated for fpu errors
|
| Revision 1.1  1998/08/12 19:16:09  peter
|   + loader including libc init and exit
|
|
