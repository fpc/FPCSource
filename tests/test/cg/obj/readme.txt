These subdirectories contain the object files for ctest.c compiled with
their respective GCC compilers.

Each time a new test is done on a compiler, it should be put in the
following list, so as to determine which compiler versions can be
used by Free Pascal for interfacing to C modules.

Tested compilers (OK)
---------------------
go32v2 : 2.95.3 20010315/djgpp (release)
         tcext6, cpp*.cpp complied with GCC version 3.4.4
Win32 :  2.95.3-5 (cygwin special)
Amiga :  GCC 2.91.66
 (long long/double support is buggy in this version of GCC, so it cannot be used)
Linux-m68k :  GCC 2.95.4 and 3.0.4
Linux-i386 : GCC 2.95.4 20011002 (Debian prerelease)

NetBSD-m68k : GCC 2.95.3 on NetBSD elf 1.6

Macos-powerpc : MrC C Compiler 4.1.0f1c1 for MPW (dont know yet if it
    can be used with FPC, but at least ctest.c compiles)

wince 4.21 : GCC 3.3.3

Solaris-i386 : gcc (GCC) 3.4.3 (csl-sol210-3_4-20050802)
Solaris-x86_64 : gcc (GCC) 3.4.3 (csl-sol210-3_4-20050802) with -m64 option

Freebsd-x86_64 : gcc (GCC) 4.2.1 20070719  [FreeBSD]

OpenBSD-i386 : gcc (GCC) 4.2.1 20070719 
NetBSD-i386 : gcc (GCC) 4.1.3 20080704 prerelease (NetBSD nb2 20081120)
FreeBSD-i386 : gcc (GCC) 4.2.1 20070719  [FreeBSD] 8.2-RELEASE
Linux-sparc : gcc (Debian 4.3.2-1.1) 4.3.2
OpenBSD-x86_64 : gcc (GCC) 4.2.1 20070719 
NetBSD-x86_64 : gcc (GCC) 4.1.3 20080704 prerelease (NetBSD nb2 20081120)
Linux-arm-gnueabihf : gcc version 4.6.3 (Debian 4.6.3-8+rpi1)
Linux-mipsel : gcc (Debian 4.4.5-8) 4.4.5
Linux-mips : gcc (Debian 4.4.5-8) 4.4.5


Android-arm    : GCC 4.7
Android-i386   : GCC 4.7
Android-mipsel : GCC 4.7
haiku-i386 : gcc 2.95.3-haiku-100818
