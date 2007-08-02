|
|   $Id: dllprt0.as,v 1.1.2.4 2001/08/01 13:26:17 pierre Exp $
|   This file is part of the Free Pascal run time library.
|   Copyright (c) 2001 by Pierre Muller
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
| Linux m68k ELF shared library startup code for Free Pascal
|
        .file "dllprt0.as"
	.text
        .globl  _startlib
        .type   _startlib,@function
_startlib:
        .globl  FPC_LIB_START
        .type   FPC_LIB_START,@function
FPC_LIB_START:
|
|       The args and envs are not tested yet
|
        move.l   (%sp)+, %d0
        lea      4(%sp,%d0*4),%a0
        move.l   %a0, U_SYSLINUX_ENVP
        move.l   %sp,U_SYSLINUX_ARGV
        move.l   %d0,U_SYSLINUX_ARGC
        jsr      PASCALMAIN
        rts

        .globl  _haltproc
        .type   _haltproc,@function
haltproc:
        moveq.l   #1,%d0
        move.w    U_SYSLINUX_EXITCODE,%d1
        trap      #0
        bra       _haltproc


        .data
	.align  4
	.globl  ___fpc_brk_addr
___fpc_brk_addr:
        .long   0

|
| $Log: dllprt0.as,v $
| Revision 1.1.2.4  2001/08/01 13:26:17  pierre
|  * syntax adapted to GNU as
|
| Revision 1.1.2.3  2001/07/13 15:13:47  pierre
|  + add and fix some comments
|
| Revision 1.1.2.2  2001/07/13 15:04:35  pierre
|  * correct assembler error
|
| Revision 1.1.2.1  2001/07/13 15:03:02  pierre
|  + New file converted from i386 version
|
|

