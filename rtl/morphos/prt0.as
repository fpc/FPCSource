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
	stwu	1,-12(1)
	mflr	0
	stw	13,8(1)
	stw	0,16(1)
	mr		13,1

	/* Get ExecBase */
	li		3,4
	lwz	3,0(3)
	lis	4,_ExecBase@ha
	stw	3,_ExecBase@l(4)

	/* ARGC & ARGV and ENVP STUFF MISSING!!! */
	/* AFAIK there is no such thing as ENVP on MorphOS, just like */
	/* on AmigaOS. One must use dos.library calls to query environment */
	/* variables, so this should be handled by pascal code inside */
	/* System unit. */

	bl	PASCALMAIN

	lwz	0,16(1)
   mtlr  0
   lwz   13,8(1)
	lwz   1,0(1)
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
  Revision 1.3  2004-04-09 02:58:15  karoly
   * typo fixed.

  Revision 1.1  2004/03/16 10:29:22  karoly
   * first implementation of some startup code for MOS

*/
