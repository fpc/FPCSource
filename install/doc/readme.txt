
                            Free Pascal Compiler

                        Version 1.9.0 aka 2.0.0-Beta1


****************************************************************************
* Introduction
****************************************************************************

*** WARNING *** WARNING *** WARNING ***
This a beta release and it still contains some known bugs. However, it would
be very useful if you could test your projects with it and tell us when
you find bugs. You can submit bug reports on the website or mail them to
bugrep@freepascal.org.
*** WARNING *** WARNING *** WARNING ***

This package contains a freeware 32-bit pascal compiler for 386+. The language
and the runtime library are almost compatible with TP 7.0 and recent Delphi
releases.

Free Pascal 1.9.0 is currently available for the following platforms:
- Linux (i386), both aout and elf
- Win32 (Win32s, Win95/98/Me/XP/2000 and WinNT)
- FreeBSD i386

More platforms will be supported in future 1.9.x beta releases.
Because release building is quite time intensive, we decide to start the
2.0 beta series only with the most important targets. If you want to change
this and create and maintain 2.0 beta releases for other platforms and
targets, feel free to contact us, E-Mail addresses are listened below.

****************************************************************************
* Features
****************************************************************************

- high speed compiler
- fully 32-bit code
- language features:
  - almost fully compatible with Borland Pascal and Borland Delphi
  - ansi strings
  - exception support
  - RTTI support
  - procedure overloading
  - operator overloading
  - COM, CORBA and raw interfaces support
  - dyn. array support
  - variant support
- code optimizer:
  - peephole optimizer
  - loading of variables into registers
  - assembler level dataflow analyzer
  - stack frame eliminations
  - sophisticated register allocator
- integrated BASM (built-in assembler) parser
  - supports ATT syntax used by GNU C
  - supports Intel syntax used by Turbo Pascal
- can compile code into assembler source code for these assemblers:
  - GNU Assembler (GAS)
  - Netwide assembler (Nasm)
  - Microsoft Assembler/Turbo Assembler (Masm/Tasm)
- can call external C code
- smartlinking
- support for the GNU debugger
- IDE (currently for GO32v2, Linux and Win32 only, in beta testing phase)
- can create binaries running natively under both DOS and OS/2 (EMX version)


****************************************************************************
* Requirements
****************************************************************************

386 processor
Win32:
 - Win95/98/Me/2000/XP or WinNT
 - 8 MB RAM (16+ MB recommended)
Linux:
 - system running a 2.0.x kernel

****************************************************************************
* Quick start
****************************************************************************

Win32:
Download w32190.zip and unzip it
into a temporary directory.

Start the install program INSTALL.EXE and follow the instructions.

Don't forget to set the path as mentioned by the install program.

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
e-mail: fpc-devel@lists.freepascal.org (bugs, developer related qs)
e-mail: fpc-pascal@lists.freepascal.org (general pascal related qs)

Both these adresses are for mailing lists. If you're not subscribed,
be sure to mention this fact when sending questions to these lists,
so that people sending answers know about it and send you a copy.
Information about available lists and subscription can be found
on http://lists.freepascal.org/mailman/listinfo

www: http://www.freepascal.org
ftp: ftp://ftp.freepascal.org/fpc
(several mirrors exist, see website for links)

Additional information about mailing lists, news, future plans etc.
can be found on the web site.

SNAPSHOTS & SOURCES
-------------------
One of the features of FPC is the snapshots. These are made daily or weekly
from the developers' latest versions of the source. Snapshots are available
for the GO32v2, Win32, OS/2 and Linux versions of the compiler/rtl. Snapshots
are also available for the go32v2 & Win32 IDEs, and for FV, FCL, GTK and
utils for GO32v2 and Win32. The latest snapshots are in: ftp... /fpc/snapshot/
in appropriately named .zip/tar files.

You will also normally find in the snapshot archive file a readme, with
a note about the latest included changes. It is quite common, though it doesn't
always happen, that when a bug is reported it is fixed and a fixed version
can be obtained the NEXT day in the appropriate snapshot.... yes really!

Also on the ftp site you'll find a /dist directory, with the latest
distributed releases, a /docs directory, and a /source directory, in
which every night at about 0100 GMT the latest source generated by the
developers during the day & evening before is exported from CVS
into ZIP files eg compiler.zip, rtl.zip, base.zip etc.


Making your own snapshots
-------------------------
By downloading the /source files (makefiles are included)
it is possible to to make your own version of the fpc compiler/rtl
and to modify it. You are of course free to do this
so long as you observe the licence conditions. In order to make the
compiler/rtl & ides in a resonable time (eg <30 minutes) you'll need at least
32M of physical memory (64M is better) memory and at least a 200 Mhz processor
and at least 20 Mbytes of free disk space. You'll also need
some knowledge of making files & programming... it is not
difficult but it isn't easy either!

REPORTING BUGS
----------------
If you find a bug in the released version, you may want to try a snapshot
(see SNAPSHOTS above) to see if it has been fixed before reporting it to
the fpc-devel mailing list.

If you find a fault or 'feature' in a release, please report it
to the fpc-devel mailing list. PLEASE SEND ALSO A SMALL EXTRACT OF THE SOURCE
CODE which caused the problem, and state the version eg Win32, GO32v2,
and the date of the compiler etc on which you noticed the problem & any other
useful info so the developers can reproduce the problem, otherwise they may
not be willing/able to fix it.


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
