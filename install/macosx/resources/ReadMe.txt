
                            Free Pascal Compiler

                        Version 1.9.3 aka 2.0.0-Beta2.5


***************************************************
Introduction
***************************************************

*** WARNING *** WARNING *** WARNING ***
This a beta release and it still contains some known bugs. However, it would be very useful if you could test your projects with it and tell us when you find bugs. You can submit bug reports on the website.
*** WARNING *** WARNING *** WARNING ***

This package contains a freeware 32-bit pascal compiler for 386+ and PowerPC. The language and the runtime library are almost compatible with TP 7.0 and recent Delphi releases.

Free Pascal 1.9.3 is currently available for the following platforms:
- Linux-i386, both aout and elf
- Linux-powerpc
- Dos (i386), using the Go32v2 dos extender
- Win32 (Win32s, Win95/98/Me/XP/2000 and WinNT)
- FreeBSD i386
- Mac OS X/Darwin for PowerPC

More platforms will be supported in future 1.9.x beta releases. Because release building is quite time intensive, we decide to start the 2.0 beta series only with the most important targets. If you want to change this and create and maintain 2.0 beta releases for other platforms and targets, feel free to contact us, E-Mail addresses are listed below.

***************************************************
Features
***************************************************
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
  - dynamic array support
  - variant support
- code optimizer:
  - sophisticated register allocator
- integrated BASM (built-in assembler) parser
  - supports ATT syntax used by GNU C
  - supports Intel syntax used by Turbo Pascal
- can compile code into assembler source code for these assemblers:
  - GNU Assembler (GAS)
- can call external C code
- smartlinking (not yet supported under Mac OS X)
- support for the GNU debugger


***************************************************
Minimum requirements
***************************************************

- Mac OS X 10.1 or higher (10.0 may also work, but is untested)
- The BSD subsystem and Mac OS X Developer Tools must be installed

***************************************************
Quick start
***************************************************

As there is currently no IDE (Integrated Development Environment) available for the Free Pascal Compiler (nor a plug-in to integrate it into Project Builder or XCode), you currently have to use Terminal to compile your files.

After installing this package, you can write your source code in your favorite editor (including Project Builder and XCode). To compile something, go to the correct directory in Terminal and type

        fpc name_of_your_source_file.pas


***************************************************
Documentation
***************************************************

The documentation is installed in PDF format in
/Developer/Documentation/Free Pascal Compiler

***************************************************
Suggestions, Help, Bug reporting, snapshots,  ...
***************************************************

Suggestions, Help ...
---------------------
e-mail: fpc-devel@lists.freepascal.org (bugs, developer related questions)
e-mail: fpc-pascal@lists.freepascal.org (general pascal related questions)

Both these adresses are for mailing lists. If you're not subscribed, be sure to mention this fact when sending questions to these lists, so that people sending answers know about it and send you a copy. Information about available lists and subscription can be found on http://lists.freepascal.org/mailman/listinfo

www: http://www.freepascal.org
ftp: ftp://ftp.freepascal.org/fpc
(several mirrors exist, see website for links)

Additional information about mailing lists, news, future plans etc.
can be found on the web site.

SNAPSHOTS & SOURCES
-------------------
One of the features of FPC is the snapshots. These are made daily or weekly from the developers' latest versions of the source. Snapshots are available for the GO32v2, Win32, OS/2 and Linux versions of the compiler/rtl. Snapshots are also available for the go32v2 & Win32 IDEs, and for FV, FCL, GTK and utils for GO32v2 and Win32. The latest snapshots are in: ftp... /fpc/snapshot/ in appropriately named .zip/tar files.

You will also normally find in the snapshot archive file a readme, with a note about the latest included changes. It is quite common, though it doesn't always happen, that when a bug is reported it is fixed and a fixed version can be obtained the NEXT day in the appropriate snapshot.... yes really!

Also on the ftp site you'll find a /dist directory, with the latest distributed releases, a /docs directory, and a /source directory, in which every night at about 0100 GMT the latest source generated by the developers during the day & evening before is exported from CVS into ZIP files eg compiler.zip, rtl.zip, base.zip etc.

Note: Currently snapshots are not yet automatically generated for Mac OS X, but that may change in the future.


REPORTING BUGS
----------------
If you find a bug in the released version, you may want to try a snapshot (see SNAPSHOTS above) to see if it has been fixed before reporting it to the fpc-devel mailing list.

If you find a fault or 'feature' in a release, please report it to the fpc-devel mailing list. PLEASE SEND ALSO A SMALL EXTRACT OF THE SOURCE CODE which caused the problem, and state the version eg Win32, GO32v2, and the date of the compiler etc on which you noticed the problem & any other useful info so the developers can reproduce the problem, otherwise they may not be willing/able to fix it.


***************************************************
License
***************************************************

The compiler and most utilities and executables distributed in this package fall under the GPL, for more information read the file COPYING.

Some specific utilities and programs come under the license described in COPYING.DJ, COPYING.EMX and COPYING.RSX.

The documentation, unless otherwise noted, is distributed as free text, and is distributed under the GNU Library General Public License.

The runtime library, package libraries, free component library, and other libraries which are used to create executables by the compiler come under a modified GNU Library General Public license. Additional information about the library license is found in COPYING.FPC.


