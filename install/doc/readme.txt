
                            Free Pascal Compiler

                               Version 0.99.14


****************************************************************************
* Intro
****************************************************************************

This package contains a freeware 32-bit pascal compiler for 386+. The language
and the runtime library are more or less compatible to TP 7.0. Some Delphi
additions have also been implemented like exceptions and rtti.

Free Pascal is currently available for the following platforms:
- DOS, via the DJ Delorie's GO32V1 and GO32V2 Dos extenders
- Linux (i386), both aout and elf
- OS/2 & DOS, via the EMX extender
- Win32 (Win32s, Win95/98 and WinNT)
- Commodore Amiga
- Atari ST

More platforms will be supported in the future.


****************************************************************************
* Features
****************************************************************************

- high speed compiler
- fully 32-bit code
- code optimizer:
  - peephole optimizer
  - loading of variables into registers
  - assembler level dataflow analyzer
  - stack frame eliminations
- language features:
  - almost fully compatible with Borland Pascal
  - long strings, ansi strings
  - partially compatible with Borland Delphi
  - procedure overloading
  - operator overloading
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
- cross-platform API
- IDE (currently for GO32v2 and Win32 only)
- can create binaries running natively under both DOS and OS/2 (EMX version)


****************************************************************************
* Requirements
****************************************************************************

386 processor
DOS (extender GO32v2):
 - DOS 3.3
 - 4 MB RAM (8+ MB recommended)
 - hard disk with free space of 8 MB
 - DMPI server (CWSDPMI is delivered in the go32v2 distro)
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
The current version is only an evaluation version.
****************************************************************************

Quick start
-----------
Download dos09912.zip (version for DOS) or w3209912.zip (version for
Win9x/NT) or os209912.zip (EMX version - for OS/2 and DOS) and unzip it
into a temporary directory.

Start the install program INSTALL.EXE and follow the instructions.

Don't forget to set the path as mentioned by the install program.

To test the compiler, change to the demo directory of the compiler
and type
        ppc386 hello          or          ppos2 hello    (for EMX version)
        hello


****************************************************************************
* The packages of the distribution
****************************************************************************

All standard packages contain a part that is specific for the target platform
and a few files which are target independent. All files are also available
as separate files to reduce file size if the default file is too big.

dos09912.zip specific:
----------------------
  basego32.zip    contains a DOS (Go32V2) compiler, runtime library and
                  additional files
  asldgo32.zip    contains additional GNU utilities which are necessary:
      AS 2.9.1 for Go32V2
      LD 2.9.1 for Go32V2
      AR 2.8.1 for Go32V2
      Strip 2.8.1 for Go32V2
  utilgo32.zip    contains additional GNU utilities which might be useful to
                  compile the run time library:
      Make 3.76.1
      RM 3.16
      CP 3.16
      MV 3.16
      PWD 3.16
      GInstall 3.16
      GDate 3.16
      GEcho 3.16
      UPX 0.94
  gdbgo32.zip     contains the GNU Debugger 4.16 for Go32V2

w3209912.zip specific:
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
  utilw32.zip     contains additional GNU utilities which might be useful to
                  compile the run time library:
      Make 3.76.1
      RM 3.16
      CP 3.16
      MV 3.16
      PWD 3.16
      GInstall 3.16
      GDate 3.16
      GEcho 3.16
      UPX 0.94
  gdbw32.zip      contains the GNU Debugger 4.16.1 for Win32

os209912.zip specific:
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

common files in dos09912.zip, w3209912.zip and os209912.zip:
------------------------------------------------------------
  demo.zip        contains some demo files
  doc-html.zip    contains the documentation in HTML format
  docs-ps.zip     contains the documentation in PostScript
  install.exe     installation program
  install.dat     installation data
  readme.txt      this readme file
  whatsnew.txt    what's been changed

Optional source package src09912.zip:
-------------------------------------
  pp09912s.zip    contains the compiler sources
  rl09912s.zip    contains the runtime library sources
  doc160s.zip     contains the TeX sources of the doc


****************************************************************************
* Documentation
****************************************************************************

The documentation is available as HTML pages.
The documentation "home page" is doc\fpctoc.htm (FPC Table Of Contents).

The documentation in PostScript is available at the ftp server.


****************************************************************************
* Suggestions, Help, Bugs ...
****************************************************************************

Suggestions, Help ...
---------------------
e-mail: fpc-devel@vekoll.saturnus.vein.hu
WWW: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/
(several mirrors exist)
FTP: ftp://tflily.fys.kuleuven.ac.be/pub/fpc
Additional informations about mailing lists etc. can be found on the
web site.


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
