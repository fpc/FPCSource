/ emx_386/dosinit.s (emx+gcc) -- Copyright (c) 1994-1996 by Eberhard Mattes

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
