{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    Program startup

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   TCdeclProcedure = procedure; cdecl;
function atexit(proc:TCdeclProcedure):longint;cdecl;external 'c' name 'atexit';
procedure _cleanup;cdecl;external 'c' name '_cleanup';
procedure _DYNAMIC;cdecl;external 'c' name '_DYNAMIC';
procedure __fpstart;cdecl;external 'c' name '__fpstart';
procedure PascalMain;cdecl;external name 'PASCALMAIN';

procedure _start;assembler;nostackframe;public name '_start';
asm
        // Terminate the stack frame, and reserve space for functions to
        //   drop their arguments.
        mov     %g0, %fp
        sub     %sp, 6*4, %sp

        // Extract the arguments and environment as encoded on the stack.  The
        //   argument info starts after one register window (16 words) past the SP.
        ld      [%sp+22*4], %o2
        sethi   %hi(argc),%o1
        or      %o1,%lo(argc),%o1
        st      %o2, [%o1]

        add     %sp, 23*4, %o0
        sethi   %hi(argv),%o1
        or      %o1,%lo(argv),%o1
        st      %o0, [%o1]

        // envp=(argc+1)*4+argv
        inc     %o2
        sll     %o2, 2, %o2
        add     %o2, %o0, %o2
        sethi   %hi(envp),%o1
        or      %o1,%lo(envp),%o1
        st      %o2, [%o1]

        // Check to see if there is an _cleanup() function linked in, and if
        // so, register it with atexit() as the last thing to be run by
        // atexit().
        sethi   %hi(_cleanup), %o0
        or      %o0, %lo(_cleanup), %o0
        cmp     %o0,%g0
        be      .L1
        nop
        call    atexit
        nop
.L1:

        // Now check to see if we have an _DYNAMIC table, and if so then
        // we need to register the function pointer previously in %edx, but
        // now conveniently saved on the stack as the argument to pass to
        // atexit().
        sethi   %hi(_DYNAMIC), %o0
        or      %o0, %lo(_DYNAMIC), %o0
        cmp     %o0,%g0
        be      .L2
        nop
        call    atexit
        nop
.L2:

        // Register _fini() with atexit().  We will take care of calling _init()
        // directly.
        //
        //      sethi   %hi(_fini), %o0
        //      or      %o0, %lo(_fini), %o0
        //      call    atexit

        // Call _init(argc, argv, environ), _fpstart(argc, argv, environ), and
        // main(argc, argv, environ).
        ld      [%sp+22*4], %o0
        add     %sp, 23*4, %o1
        add     %o0, 1, %o2
        sll     %o2, 2, %o2
        add     %o2, %o1, %o2


        // call    __fpstart
        // nop

        call    PASCALMAIN
        nop

        // Die very horribly if exit returns
        unimp
end;


