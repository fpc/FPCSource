{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    ARM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        mov    r0,fp
end ['R0'];


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
(*!!!!!!
        movl    framebp,%eax
        orl     %eax,%eax
        jz      .Lg_a_null
        movl    4(%eax),%eax
.Lg_a_null:
*)
end ['R0'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
(*!!!!!
        movl    framebp,%eax
        orl     %eax,%eax
        jz      .Lgnf_null
        movl    (%eax),%eax
.Lgnf_null:
*)
end ['R0'];


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Longint;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        mov    r0,sp
end ['R0'];


{
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);assembler;

* void *memset (dstpp, c, len) */

asm
	mov	a4, a1
	cmp	a2, $8		// at least 8 bytes to do?
	blt	.Fillchar2
	orr	a3, a3, a3, lsl $8
	orr	a3, a3, a3, lsl $16
.Fillchar0:
	tst	a4, $3		// aligned yet?
	strneb	a3, [a4], $1
	subne	a2, a2, $1
	bne	.Fillchar0
	mov	ip, a3
.Fillchar1:
	cmp	a2, $8		// 8 bytes still to do?
	blt	.Fillchar2
	stmia	a4!, {a3, ip}
	sub	a2, a2, $8
	cmp	a2, $8		// 8 bytes still to do?
	blt	.Fillchar2
	stmia	a4!, {a3, ip}
	sub	a2, a2, $8
	cmp	a2, $8		// 8 bytes still to do?
	blt	.Fillchar2
	stmia	a4!, {a3, ip}
	sub	a2, a2, $8
	cmp	a2, $8		// 8 bytes still to do?
	stmgeia	a4!, {a3, ip}
	subge	a2, a2, $8
	bge	.Fillchar1
.Fillchar2:
	movs	a2, a2		// anything left?
	RETINSTR(moveq,pc,lr)	// nope
	rsb	a2, a2, $7
	add	pc, pc, a2, lsl $2
	mov	r0, r0
	strb	a3, [a4], $1
	strb	a3, [a4], $1
	strb	a3, [a4], $1
	strb	a3, [a4], $1
	strb	a3, [a4], $1
	strb	a3, [a4], $1
	strb	a3, [a4], $1
	RETINSTR(mov,pc,lr)
end;

}



{
  $Log$
  Revision 1.2  2003-09-03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.1  2003/08/21 16:41:54  florian
    * empty dummy files
    + [long|set]jmp implemented
}
