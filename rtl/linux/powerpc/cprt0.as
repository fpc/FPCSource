/*
  $Id: cprt0.as,v 1.9 2004/09/02 18:57:37 marco Exp $
*/
/* Startup code for programs linked with GNU libc.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

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

        .section ".text"
        .globl  _start
_start:
	mr 	26,1
	/* Set up an initial stack frame, and clear the LR.  */
	clrrwi	1,1,4
	li	0,0
	stwu	1,-16(1)
	mtlr	0
	stw	0,0(1)

        lis 	11,operatingsystem_parameter_argc@ha
	stw 	3,operatingsystem_parameter_argc@l(11);

        lis 	11,operatingsystem_parameter_argv@ha
	stw 	4,operatingsystem_parameter_argv@l(11);

	lis 	11,operatingsystem_parameter_envp@ha
	stw 	5,operatingsystem_parameter_envp@l(11);

	/* init libc, parameters are already setup at this point */
	bl	__libc_init_first

	/* install finalization code handler */
	lis	3,_fini@ha
	addi	3,3,_fini@l
	bl	PASCALMAIN

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        li      0,1	         /* exit call */
	lis	3,operatingsystem_result@h
	stw	3,operatingsystem_result@l(3)
        sc
        b	_haltproc

	/* Define a symbol for the first piece of initialized data.  */
	.section ".data"
	.globl	__data_start
__data_start:
data_start:
        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

.text
        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

/*
  $Log: cprt0.as,v $
  Revision 1.9  2004/09/02 18:57:37  marco
   * fixed argc<->argv

  Revision 1.8  2004/08/18 14:26:50  karoly
    * quick fix to make it compile

  Revision 1.7  2004/07/03 21:50:31  daniel
    * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
      longer necessary

  Revision 1.6  2004/01/04 17:28:03  florian
    * clean up

  Revision 1.5  2004/01/04 17:12:28  florian
    * arg* and envp handling fixed

  Revision 1.4  2003/12/28 20:08:53  florian
    * initial code

  Revision 1.3  2002/09/07 16:01:20  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/07/26 17:09:44  florian
    * log fixed

  Revision 1.1  2002/07/26 17:07:11  florian
    + dummy implementation to test the makefile
*/