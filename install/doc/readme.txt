
                            Free Pascal Compiler

                               Version 1.0.6


****************************************************************************
* Introduction
****************************************************************************

This package contains a freeware 32-bit pascal compiler for 386+. The language
and the runtime library are more or less compatible to TP 7.0. Some Delphi
additions have also been implemented like classes, exceptions, ansistrings
and rtti.

Free Pascal is currently available for the following platforms:
- DOS, via the DJ Delorie's GO32V2 Dos extenders
- Linux (i386), both aout and elf
- OS/2 & DOS, via the EMX extender
- Win32 (Win32s, Win95/98 and WinNT)
- Sun Solaris i386
- BeOS i386

Older version of the compiler (0.99.5) is also available on:
- Commodore Amiga
- Atari ST

More platforms will be supported in the future.

****************************************************************************
* Features
****************************************************************************

- high speed compiler
- fully 32-bit code
- language features:
  - almost fully compatible with Borland Pascal
  - partially compatible with Borland Delphi
  - ansi strings
  - exception support
  - RTTI support
  - procedure overloading
  - operator overloading
- code optimizer:
  - peephole optimizer
  - loading of variables into registers
  - assembler level dataflow analyzer
  - stack frame eliminations
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
- IDE (currently for GO32v2 and Win32 only, in beta testing phase)
- can create binaries running natively under both DOS and OS/2 (EMX version)


****************************************************************************
* Requirements (Intel version)
****************************************************************************

386 processor
DOS (extender GO32v2):
 - DOS 3.3
 - 4 MB RAM (8+ MB recommended)
 - hard disk with free space of 8 MB
 - DMPI server (CWSDPMI is delivered in the go32v2 distribution)
Win32:
 - Win95/98 or WinNT
 - 8 MB RAM (16+ MB recommended)
OS/2 and DOS (extender EMX):
 - either DOS 5.0 and above
 or OS/2 v2.x and above
 - 3 MB RAM (8+ MB recommended) for DOS
 or 8 MB (12 or more MB recommended depending on OS version) for OS/2
 - EMX or RSX (for DPMI) runtime package (part of OS/2 distribution)


****************************************************************************
* Quick start
****************************************************************************

Download dos106.zip (version for DOS) or w32106.zip (version for
Win9x/NT) or os2106.zip (EMX version - for OS/2 and DOS) and unzip it
into a temporary directory.

Start the install program INSTALL.EXE and follow the instructions.

Don't forget to set the path as mentioned by the install program.

To test the compiler, change to the demo directory of the compiler
and type
        ppc386 hello
        hello


****************************************************************************
* The packages of the distribution
****************************************************************************

All standard packages contain a part that is specific for the target platform
and a few files which are target independent. All files are also available
as separate files to reduce file size if the default file is too big.

dos106.zip specific:
----------------------
  basego32.zip    contains a DOS (Go32V2) compiler, runtime library and
                  additional files
  asldgo32.zip    contains additional GNU utilities which are necessary:
      AS 2.9.1 for Go32V2
      LD 2.9.1 for Go32V2
      AR 2.8.1 for Go32V2
      Strip 2.8.1 for Go32V2
  makego32.zip    contains additional GNU utilities which might be useful to
                  compile the run time library:
      Make 3.79.1
      RM 3.16
      CP 3.16
      MV 3.16
      PWD 3.16
      GInstall 3.16
      GDate 3.16
      GEcho 3.16
      UPX 1.20
  gdbgo32.zip     contains the GNU Debugger 4.18 with pascal support for Go32V2

w32106.zip specific:
----------------------
  basew32.zip     contains a Win32 compiler, runtime library and
                  additional files
  asldw32.zip     contains additional GNU utilities from MinGW32 which are
                  necessary:
      AS 2.9.5 for Win32
      LD 2.9.5 for Win32
      AR 2.9.5 for Win32
      Strip 2.9.5 for Win32
      WindRes 2.9.5 for Win32
      DLLTool 2.9.5 for Win32
  makew32.zip     contains additional GNU utilities which might be useful to
                  compile the run time library:
      Make 3.79.1
      RM 3.16
      CP 3.16
      MV 3.16
      PWD 3.16
      GInstall 3.16
      GDate 3.16
      GEcho 3.16
      UPX 1.20
  gdbw32.zip      contains the GNU Debugger 4.18 with pascal support for Win32

os2106.zip specific:
----------------------
  baseemx.zip     contains an EMX (OS/2 and DOS) compiler, runtime library and
                  additional files
  asldemx.zip     contains additional GNU utilities which are necessary:
      AS 2.9.1 for EMX
      LD for EMX
      EMXBIND 0.9d
      AR 2.9.1 for EMX
      NM 2.9.1 for EMX
      GASP 1.2 for EMX
      ObjCopy 2.9.1 for EMX
      ObjDump 2.9.1 for EMX
      Strip 2.9.1 for EMX
      RANLIB 2.9.1 for EMX
  utilemx.zip     contains additional GNU utilities which might be useful to
                  compile the run time library:
      Make 3.76.1
      RM 3.13
      CP 3.13
      MV 3.13
      ChMod 3.13
      PWD 1.12
      Install 3.13
      Date 1.12
      Echo 1.12
  gdbemx.zip      contains the GNU Debugger 4.16 for EMX,
                  PMGDB (Presentation Manager add-on for GDB) and GPROF 2.9.1

common files in dos106.zip, w32106.zip and os2106.zip:
------------------------------------------------------------
  demo.zip        contains some demo files
  doc-pdf.zip     contains the documentation in PDF format
  doc-html.zip    contains the documentation in HTML format
  install.exe     installation program
  install.dat     installation data
  readme.txt      this readme file
  whatsnew.txt    what's been changed

Optional source package src106.zip:
-------------------------------------
  basesrc.zip     contains the basic Makefiles needed for the source tree
  compsrc.zip     contains the compiler sources
  rtlsrc.zip      contains the runtime library sources
  fclsrc.zip      contains the Free Component Library sources
  pkgssrc         contains the packages (various units) sources
  utilssrc.zip    contains the Utilities sources
  instsrc.zip     contains the installer sources
  docsrc.zip      contains the TeX sources of the doc


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

The programs and sources come under the GPL, for more informations read
the file COPYING. Additional informations about the runtime library license
are found in COPYING.FPC. Some utilities and programs come under the license
described in COPYING.DJ or COPYING.EMX

NOTE: OS/2 version of the installer uses the library UNZIP32.DLL from
      Info-ZIP. Info-ZIP's software (Zip, UnZip and related utilities)
      is free and can be obtained as source code or executables from
      Internet/WWW sites, including http://www.cdrom.com/pub/infozip/ .
