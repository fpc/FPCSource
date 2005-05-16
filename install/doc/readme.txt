
                            Free Pascal Compiler

                               Version 2.0.0


****************************************************************************
* Introduction
****************************************************************************

Please also read the platform specific README file, if it exists.

This package contains a freeware 32-bit/64-bit pascal compiler for several
platforms. The language and the runtime library are almost compatible with
Turbo Pascal 7.0 and recent Delphi releases. There is also partial support
for the Macintosh pascal dialect.

Free Pascal 2.0.0 is currently available for the following platforms:
- Linux-i386
- Linux-powerpc
- Linux-sparc
- Linux-x86_64 (amd64)
- Win32 (Win95/98/Me/XP/2000 and WinNT)
- OS/2-i386 (OS/2 Warp v3.0, 4.0, WarpServer for e-Business and eComStation)
- FreeBSD i386
- Mac OS X/Darwin for PowerPC
- Mac OS (classic) for PowerPC
- Netware-i386
- NetwLibc-i386 (LibC is preferred API under newer Netware versions)

More platforms will be supported in future 1.9.x beta releases.
Because release building is quite time intensive, we decide to start the
2.0 beta series only with the most important targets. If you want to change
this and create and maintain 2.0 beta releases for other platforms and
targets, feel free to contact us, e-mail addresses are listed below.

There are other platforms which are more or less working, but there is
currently no maintainer for them and thus we cannot provide adequate
support. DOS (i386) using the GO32v2 dos extender is one such example,
other examples would include other BSD variants, etc. If you want to change
this and create and maintain versions for other platforms and targets, feel
free to contact us, e-mail addresses are listed below.

****************************************************************************
* Features
****************************************************************************

- high speed compiler
- fully 32 or 64-bit code
- language features:
  - almost fully compatible with Borland Pascal and Borland Delphi
  - ansi strings
  - wide strings
  - exception support
  - RTTI support
  - procedure overloading
  - operator overloading
  - COM, CORBA and raw interfaces support
  - dynamic array support
  - variant support
  - inlining
- code optimizer:
  - peephole optimizer (80x86 only)
  - jump optimizer
  - loading of variables into registers
  - assembler level dataflow analyzer (80x86 only)
  - stack frame eliminations
  - sophisticated register allocator
- integrated BASM (built-in assembler) parser
  - supports ATT syntax used by GNU C
  - supports Intel syntax used by Turbo Pascal (80x86-only)
- can compile code into assembler source code for these assemblers:
  - GNU Assembler (GAS)
  - Netwide assembler (Nasm)
  - Microsoft Assembler/Turbo Assembler (Masm/Tasm)
  - Watcom assembler (wasm)
- can call external C code
- smartlinking (not yet supported under Mac OS X)
- support for the GNU debugger
- IDE (currently for GO32v2, Linux, FreeBSD, OS/2 and Win32 only; not all
  platforms include debugger integrated in IDE)
- can create binaries running natively under both DOS and OS/2 (EMX version)


****************************************************************************
* Minimum requirements
****************************************************************************

i386, x86_64, PowerPC or Sparc processor
Win32:
 - Win95/98/Me/2000/XP or WinNT
 - 16 MB RAM
OS/2:
 - OS/2 Warp v3.0 with one of late fixpaks - FP 35 should be fine,
   OS/2 Warp v4.0 with FP 5 and above, WSeB, MCP or any eComStation version
   (OS/2 2.0/2.1 may work partly, but not supported)
Linux:
 - system running a 2.0.x kernel
FreeBSD:
- FreeBSD 4.x system or 5.x system that has COMPAT_4 system (which is
  default)
Mac OS X:
 - Mac OS X 10.1 and higher (10.0 may also work, but is untested)
Mac OS (classic)
 - Mac OS 9.2 has been tested, should probably also work from 7.5.3 and up.

****************************************************************************
* Quick start - Win32
****************************************************************************

Download the distribution package (fpc-2.0.0.i386-win32.exe) and run it
- it is a self-extracting installer, so just follow the instructions
to install it. Don't forget to set the path as mentioned by the install
program.

To test the compiler, change to the demo directory of the compiler
and type
        fpc hello
        hello


****************************************************************************
* Quick start - OS/2 / DOS
****************************************************************************

Download distribution archive (os2200.zip for OS/2 or dos196.zip for
GO32v2) and unzip it into a temporary directory.

Start the install program INSTALL.EXE and follow the instructions.

Don't forget to set the path as mentioned by the install program.

To test the compiler, change to the demo directory of the compiler
and type
        fpc hello
        hello


****************************************************************************
* Quick start - Linux/FreeBSD
****************************************************************************

Download fpc-2.0.0.<cpu>-<os>.tar and untar into a temporary directory.

Start the install script with ./install.sh and follow the instructions.

To test the compiler, change to the demo directory of the compiler
and type
        fpc hello
        hello


****************************************************************************
* Documentation
****************************************************************************

The documentation is available as HTML pages, PDF, PS, and text although the
recommended format is pdf. These are all available on
ftp://ftp.freepascal.org/fpc/docs

NB that there is at present no FPC specific documentation for the Win32
system functions. There is a note in the ftp /doc explaining where
the MS help file for this can be obtained.


****************************************************************************
* Suggestions, Help, Bug reporting, snapshots,  ...
****************************************************************************

Suggestions, Help ...
---------------------
e-mail: fpc-devel@lists.freepascal.org (bugs, developer related questions)
e-mail: fpc-pascal@lists.freepascal.org (general pascal related questions)

Both these adresses are for mailing lists. If you're not subscribed,
be sure to mention this fact when sending questions to these lists,
so that people sending answers know about it and send you a copy.
Information about available lists and subscription can be found
on http://lists.freepascal.org/mailman/listinfo

WWW: http://www.freepascal.org
FTP: ftp://ftp.freepascal.org/fpc
(several mirrors exist, see website for links)

Additional information about mailing lists, news, future plans etc.
can be found on the web site.

SNAPSHOTS & SOURCES
-------------------
One of the features of FPC is the snapshots. These are made daily or weekly
from the developers' latest versions of the source. Snapshots are available
for GO32v2, Win32, OS/2, Linux and Netware versions. The latest snapshots
are in: ftp... /fpc/snapshot/ in appropriately named .zip/tar files.

You will also normally find in the snapshot archive file a readme, with
a note about the latest included changes. It is quite common, though it doesn't
always happen, that when a bug is reported it is fixed and a fixed version
can be obtained the NEXT day in the appropriate snapshot.... yes really!

Also on the ftp site you'll find a /dist directory, with the latest
distributed releases, a /docs directory, and a /source directory, in
which every night at about 0100 GMT the latest source generated by the
developers during the day & evening before is exported from CVS
into ZIP file fpc.zip.


Making your own snapshots
-------------------------
By downloading the /source files (makefiles are included)
it is possible to to make your own version of the fpc compiler/rtl
and to modify it. You are of course free to do this
so long as you observe the licence conditions. In order to make the
compiler/rtl & ides in a resonable time (eg <30 minutes) you'll need at least
32M of physical memory (64M is better) memory and at least a 200 Mhz processor
and at least 100 Mbytes of free disk space. You'll also need
some knowledge of making files & programming... it is not
difficult but it isn't easy either!

REPORTING BUGS
----------------
If you find a bug in the released version, you may want to try a snapshot
(see SNAPSHOTS above) to see if it has been fixed before reporting it to
the fpc-devel mailing list.

If you find a fault or 'feature' in a release, please report it either using
the bug reporting interface available on our WWW pages (see above), or to
the fpc-devel mailing list. PLEASE INCLUDE ALSO A COMPILABLE CODE FRAGMENT
which can be used to reproduce the problem (or a link to larger archive if
it cannot reproduced with small example), and state the version eg Win32,
GO32v2, and the date of the compiler etc on which you noticed the problem
& any other useful info so the developers can reproduce the problem,
otherwise they may not be willing/able to fix it.


****************************************************************************
* License
****************************************************************************

The compiler and most utilities and executables distributed in this package
fall under the GPL, for more information read the file COPYING.

Some specific utilities and programs come under the license described in
COPYING.DJ, COPYING.EMX and COPYING.RSX.

The documentation, unless otherwise noted, is distributed as free
text, and is distributed under the GNU Library General Public
License.

The runtime library, package libraries, free component library, and
other libraries which are used to create executables by the compiler
come under a modified GNU Library General Public license. Additional
information about the library license is found in COPYING.FPC.

NOTE: OS/2 version of the installer uses the library UNZIP32.DLL from
      Info-ZIP. Info-ZIP's software (Zip, UnZip and related utilities)
      is free and can be obtained as source code or executables from
      Internet/WWW sites, including http://www.info-zip.org.
