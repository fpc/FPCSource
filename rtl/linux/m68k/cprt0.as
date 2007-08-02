|
|   $Id: cprt0.as,v 1.1.2.2 2001/08/01 13:26:17 pierre Exp $
|   This file is part of the Free Pascal run time library.
|   Copyright (c) 2001 by Free Pascal Core Team
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
| Linux m68k ELF startup code for linking with C lib for Free Pascal
|
        .file   "cprt0.as"
	.text
        .globl  _start
        .type   _start,@function
_start:
        .globl  __entry
        .type   __entry,@function
__entry:
        move.l   8(%sp),%d0
        move.l   %d0,U_SYSLINUX_ENVP
        move.l   %d0,__environ
        move.l   4(%sp),%d0
        move.l   %d0,U_SYSLINUX_ARGV
        move.l   (%sp),%d0
        move.l   %d0,U_SYSLINUX_ARGC
|       The arguments should be in correct order for
|       calling __libc_init
|       This code is untested for now PM
        jsr     __libc_init
|       insert _fini in atexit chain
        move.l   _fini,-(%sp)
        jsr      atexit
        addq.l   #4,%sp
|       call _init function
        jsr      _init

        jsr      PASCALMAIN

|       Used by System_exit procedure
        .globl  _haltproc
_haltproc:
|       Call C exit function
        move.w   U_SYSLINUX_EXITCODE,%d1
        move.l   %d1,-(%sp)
        jsr      exit
        moveq.l  #1,%d0
        move.l   (%sp)+,%d1
        trap     #0
        bra      _haltproc


|       Is this still needed ??
|        .data
|        .align	4
|        .globl	___fpc_brk_addr
|___fpc_brk_addr:
|       .long	0


|
| $Log: cprt0.as,v $
| Revision 1.1.2.2  2001/08/01 13:26:17  pierre
|  * syntax adapted to GNU as
|
| Revision 1.1.2.1  2001/07/13 15:29:32  pierre
|  first version of cprt0.as
|
|

