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
	stwu 1,-64(1)
	mflr 0
	stw 29,52(1)
	stw 30,56(1)
	stw 31,60(1)
	stw 0,68(1)

	/* Save Stackpointer */
	lis 4,OriginalStkPtr@ha
	stw 1,OriginalStkPtr@l(4)

	/* Get ExecBase */
	lwz 3,4(0)
	lis 4,_ExecBase@ha
	stw 3,_ExecBase@l(4)

	/* ARGC & ARGV and ENVP STUFF MISSING!!! */
	/* AFAIK there is no such thing as ENVP on MorphOS, just like */
	/* on AmigaOS. One must use dos.library calls to query environment */
	/* variables, so this should be handled by pascal code inside */
	/* System unit. */

	bl	PASCALMAIN

	.globl	_haltproc
_haltproc:
	/* Restore Stackpointer */	
	lis 4,OriginalStkPtr@ha
	lwz 1,OriginalStkPtr@l(4)

	lwz 11,0(1)
	lwz 0,4(11)
	mtlr 0
	lwz 29,-12(11)
	lwz 30,-8(11)
	lwz 31,-4(11)
	mr 1,11
	blr

	.globl	_ExecBase
	.globl	SysBase
	.align	4
SysBase:
_ExecBase:
	.long	0

   .globl	OriginalStkPtr
	.align	4
OriginalStkPtr:
	.long	0

	/* This is needed to be a proper MOS ABox executable */
	/* This symbol _MUST NOT_ be stripped out from the executable */
   /* or else... */
	.globl	__abox__
	.type	 __abox__,@object
	.size	 __abox__,4
__abox__:
	.long 1

/*
  $Log$
  Revision 1.6  2004-05-01 15:08:57  karoly
    + haltproc added, saving/restoring stackpointer added

  Revision 1.5  2004/04/21 03:24:55  karoly
   * rewritten to be similar to GCC startup code

  Revision 1.4  2004/04/09 04:02:43  karoly
   * abox id symbol fixed

  Revision 1.3  2004/04/09 02:58:15  karoly
   * typo fixed.

  Revision 1.1  2004/03/16 10:29:22  karoly
   * first implementation of some startup code for MOS

*/
