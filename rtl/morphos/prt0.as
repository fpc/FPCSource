/*
  $Id$
*/
/*
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Karoly Balogh for Genesi Sarl. 

   Thanks for Martin 'MarK' Kuchinka <kuchinka@volny.cz>
   for his help.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/
	.section	".text"
	.globl	_start
	.align	4
_start:
	stwu	1,-16(1)
	mflr	0
	stw	0,20(1)

	/* Get ExecBase */
	lwz	3,4(0)
	lis	12,_ExecBase@ha
	stw	3,_ExecBase@l(12)

	/* ARGC & ARGV and ENVP STUFF MISSING!!! */
	/* AFAIK there is no such thing as ENVP on MorphOS, just like */
	/* on AmigaOS. One must use dos.library calls to query environment */
	/* variables, so this should be handled by pascal code inside */
	/* System unit. */

	bl	PASCALMAIN

	lwz	0,20(1)
	addi	1,1,16
	mtlr	0
	blr

	.globl	_ExecBase
	.globl	SysBase
	.align	4
SysBase:
_ExecBase:
	.long	0

	/* This is needed to be a proper MOS ABox executable */
	/* This symbol _MUST NOT_ be stripped out from the executable */
   /* or else... */
	.globl	__abox__
	.align 	4
__abox__:
	.long 1

/*
  $Log$
  Revision 1.1  2004-03-16 10:29:22  karoly
   * first implementation of some startup code for MOS

*/
