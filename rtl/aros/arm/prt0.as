/* 
  This file is part of the Free Pascal run time library.
  Copyright (c) 2016 by Marcus Sackrow

  Startup code for AROS/ARM RTL

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY;without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  */
  
	.text
	.globl _start
	.type _start,#function
_start:
	push {r0-r12,r14}
	ldr ip,=_Backjump
	str lr,[ip]
	/* ExecBase */
	ldr ip,=_ExecBase
	str r5, [ip]  /* Execbase seems to be in r5 ;-)*/

	/* Save initial stackpointer*/
	ldr ip,=__stkptr
	str sp,[ip]
	
	bl PASCALMAIN

	/*mov r0,#71
	bl _RawCharS
	mov r0,#10
	bl _RawCharS*/

	.globl  _haltproc
	.type   _haltproc,#function
_haltproc:
	
	ldr ip,=__stkptr
	ldr sp,[ip]

	pop {r0-r12,r14}

	/* exitcode should be r0*/
	ldr ip,=operatingsystem_result
	ldr r0,[ip]

	ldr ip,=_Backjump
	ldr lr,[ip]
	bx lr
	

	/* Define a symbol for the first piece of initialized data.  */
	.data
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
	data_start = __data_start

.bss
  .comm __stkptr,4
	.comm _ExecBase,4
	.comm _Backjump,4
