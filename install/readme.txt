
                            Free Pascal Compiler

                               Version 0.99.10


****************************************************************************
* Intro
****************************************************************************

This package contains a freeware pascal compiler for 386+. The language
and the runtime library are less or more compatible to TP 7.0. Some Delphi
additions have also been implemented like exceptions and rtti.


****************************************************************************
* Requirements
****************************************************************************

386 processor
DOS:
 - DOS 3.3
 - 640 kB RAM
 - hard disk with 8 MB free space
 - 4mb of memory (8+ MB recommended)
 - DMPI server (CWSDPMI is delivered in the go32v2 distro)
Win32:
 - Win95 or WinNT
 - 8mb of memory (16+ MB recommended)


****************************************************************************
The current version is only an evaluation version.
****************************************************************************

Quick start
-----------
Download dos09910.zip and unzip it into a temporary directory.

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

dos09910.zip specific:
----------------------
  basego32.zip    contains a DOS (Go32V2) compiler, run time library and
                  additional files.
  asldgo32.zip    contains additional GNU utilities which are necessary:
      AS 2.8.1 for Go32V2
      LD 2.8.1 for Go32V2
      AR 2.8.1 for Go32V2
      Strip 2.8.1 for Go32V2
  gdbgo32.zip     contains the GNU Debugger 4.16 for Go32V2

w3209910.zip specific:
----------------------
  basew32.zip     contains a Win32 compiler, run time library and
                  additional files.
  asldw32.zip     contains additional GNU utilities from MinGW32 which are
                  necessary:
      AS 980119 for Win32
      LD 980119 for Win32
      AR 980119 for Win32
      Strip 980119 for Win32
  gdbw32.zip      contains the GNU Debugger 4.16.1 for Win32

common files in dos09910.zip and w3209910.zip:
----------------------------------------------
  gnuutils.zip    contains additional GNU utilities which are neccessary to
                  compile the run time library:
      Make 3.76.1
      RM 3.16
      CP 3.16
      MV 3.16
      PWD 3.16
      GInstall 3.16
  demo.zip        contains some demo files
  docs-htm.zip    contains the documentatio in HTML format
  docs-ps.zip     contains the documentaion in post script
  install.exe     installation program
  install.dat     installation data
  readme.txt      this readme file
  whatsnew.txt    what's been changed

Optional source package src09910.zip:
-------------------------------------
  pp09910s.zip    contains the compiler sources
  rl09910s.zip    contains the run time library sources
  doc120s.zip     contains the TeX sources of the doc


****************************************************************************
* Documentation
****************************************************************************

The documentations are available as HTML pages.
The documentation "home page" is doc\fpctoc.htm (FPC Table Of Contents).

The documentation as post script is available at the ftp server.


****************************************************************************
* Suggestions, Help, Bugs ...
****************************************************************************

Suggestions, Help ...
---------------------
EMail: ba2395@fen.baynet.de
WWW: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/
Additional informations about mailing lists etc. can be found on the
web site.


****************************************************************************
* License
****************************************************************************

The programs and sources come under the GPL, for more informations read
the file COPYING. Additional informations about the runtime library license
are found in COPYING.FPC. Some utilities and programs come under the license
described in COPYING.DJ

