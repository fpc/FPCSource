/ prt1.s (emx+fpk) -- Made from crt2.s,
/					  Copyright (c) 1990-1996 by Eberhard Mattes.
/					  Portions Copyright (c) 1997 by Dani‰l Mantione

		.globl  __entry1
		.globl  _environ
		.globl	_envc
		.globl	_argv
		.globl	_argc

		.text

__entry1:
		popl    %esi
		xorl    %ebp, %ebp
		leal    (%esp), %edi
		movl    %edi,_environ
		call    L_ptr_tbl
		mov		%ecx,_envc
		mov		%edi,_argv
		call    L_ptr_tbl
		mov		%ecx,_argc
		jmp     *%esi

L_ptr_tbl:
		xorl    %eax, %eax
		movl    $-1, %ecx
1:      incl    %ecx
		scasl
		jne     1b
		ret

		.data

		.comm   _environ,	4
		.comm   _envc,		4
		.comm   _argv,		4
		.comm   _argc,		4
