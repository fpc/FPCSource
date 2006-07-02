These subdirectories contain the object files for ctest.c compiled with
their respective GCC compilers.

Each time a new test is done on a compiler, it should be put in the
following list, so as to determine which compiler versions can be
used by Free Pascal for interfacing to C modules.

Tested compilers (OK)
---------------------
go32v2 : 2.95.3 20010315/djgpp (release)
Win32 :  2.95.3-5 (cygwin special)
Amiga :  GCC 2.91.66 
 (long long/double support is buggy in this version of GCC, so it cannot be used)
Linux-m68k :  GCC 2.95.4 and 3.0.4
Linux-i386 : GCC 2.95.4 20011002 (Debian prerelease)

NetBSD-m68k : GCC 2.95.3 on NetBSD elf 1.6

Macos-powerpc : MrC C Compiler 4.1.0f1c1 for MPW (dont know yet if it
    can be used with FPC, but at least ctest.c compiles)

wince 4.21 : GCC 3.3.3
