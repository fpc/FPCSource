/ code2.s (emx+fpk) -- Copyright (c) 1992-1996 by Eberhard Mattes
/                      Changed for FPK-Pascal in 1998 by Dani‰l Mantione.
/					   This code is _not_ under the Library GNU Public
/ 					   License, because the original is not. See copying.emx
/	 				   for details. You should have received it with this
/		 			   product, write the author if you haven't.

		.globl  DosGetMessage
		.globl  _msgseg32

_msgseg32:
		.byte   0xff
		.asciz  "MSGSEG32"
		.byte   0x01, 0x80, 0x00, 0x00
		.long   L_tab

		.align  2, 0x90

DosGetMessage:
		PROFILE_NOFRAME
		popl    %ecx                    /* return address */
		pushl   $_msgseg32
		pushl   %ecx
		jmp     _DOSCALLS$$_DOSTRUEGETMESSAGE$POINTER$PINSERTTABLE$LONGINT$PCHAR$LONGINT$LONGINT$PCHAR$LONGINT

L_tab:  .short  0x0000
        .short  0xffff
