#   $Id$
/* Startup code for elf32-sparc
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson <richard@gnu.ai.mit.edu>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <sysdep.h>


	.section ".text"
	.align 4
	.global _start
	.type _start,#function
_start:

  /* Terminate the stack frame, and reserve space for functions to
     drop their arguments.  */
	mov	%g0, %fp
	sub	%sp, 6*4, %sp

  /* Extract the arguments and environment as encoded on the stack.  The
     argument info starts after one register window (16 words) past the SP.  */
	ld	[%sp+22*4], %o1
	add	%sp, 23*4, %o2

  /* Load the addresses of the user entry points.  */
	sethi	%hi(main), %o0
	sethi	%hi(fpc_initialize), %o3
	sethi	%hi(fpc_finalize), %o4
	or	%o0, %lo(main), %o0
	or	%o3, %lo(fpc_initialize), %o3
	or	%o4, %lo(fpc_finalize), %o4

  /* When starting a binary via the dynamic linker, %g1 contains the
     address of the shared library termination function, which will be
     registered with atexit().  If we are statically linked, this will
     be NULL.  */
  mov	%g1, %o5

  /* Call the user program entry point.  */
  call	PASCALMAIN
  nop

  /* Die very horribly if exit returns.  */
	unimp

	.size _start, .-_start

# $Log$
# Revision 1.4  2003-06-02 22:03:37  mazen
# *making init and fini symbols compatible FPC code by
#  changing  _init ==> fpc_initialize
#  and _fini ==> fpc_finalize
#
# Revision 1.3  2002/11/18 19:03:46  mazen
# * start code of gcc adapted for FPC
#
