/ prt1.s (emx+fpk) -- Made from crt2.s and dos.s,
/                                         Copyright (c) 1990-1999-2000 by Eberhard Mattes.
/                     Changed for Free Pascal in 1997 Daniel Mantione.
/                                         This code is _not_ under the Library GNU Public
/                                         License, because the original is not. See copying.emx
/                                         for details. You should have received it with this
/                                         product, write the author if you haven't.

                .globl  __entry1
                .globl  _environ
                .globl  _envc
                .globl  _argv
                .globl  _argc

                .text

__entry1:
                popl    %esi
                cld
                xorl    %ebp, %ebp
                leal    (%esp), %edi      /* argv[] */
                movl    %edi,_environ
                call    L_ptr_tbl
                movl    %ecx,_envc
                movl    %edi,_argv
                call    L_ptr_tbl
                movl    %ecx,_argc
                jmp     *%esi

L_ptr_tbl:
                xorl    %eax, %eax
                movl    $-1, %ecx
1:              incl    %ecx
                scasl
                jne     1b
                ret

/ In executables created with emxbind, the call to _dos_init will
/ be fixed up at load time to _emx_init of emx.dll.  Under DOS,
/ this dummy is called instead as there is no fixup.  This module
/ must be linked statically to avoid having two fixups for the
/ same location.

                .globl  __dos_init
                .globl  __dos_syscall

__dos_init:
                ret     $4

                .align  2, 0x90

__dos_syscall:
                int     $0x21
                ret

                .data

                .comm   _environ,       4
                .comm   _envc,          4
                .comm   _argv,          4
                .comm   _argc,          4
