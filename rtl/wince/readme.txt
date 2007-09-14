WinCE port
==========

WinCE port is quite complete and usable. The port was started and maintained by Yury Sidorov. Oliver (Oro06) ported WinCE API headers.

Status
------
* The 2.1.x compiler has compiler support WinCE.
* ARM and i386 (old WinCE emulator) CPUs are supported.
* The following platforms are supported: 
  * Pocket PC 2002 – WinCE version: 3.0 
  * Pocket PC 2003 – WinCE version: 4.20 
  * Pocket PC 2003 Second Edition – WinCE version: 4.21 
* Base units are complete.
* Windows unit is almost complete. Delphi compatible declarations is not ready.

Building
--------
* You need cross binutils for arm-wince, get them fromftp://ftp.freepascal.org/pub/fpc/contrib/cross/arm-wince-binutils.zip for Win32.
* Extract them to some dir in the path on your machine.
* Get the 2.1 source repository from SVN: http://www.freepascal.org/develop.html#svn
* Go to fpc/compiler and execute: 
  make cycle CPU_TARGET=arm OS_TARGET=wince

You should end with the units compiled to fpc/rtl/units/arm-wince and a ppccrossarm.exe in fpc/compiler. Copy them to locations fitting your fpc installation.

WinCE port notes
----------------
* chdir procedure always produces an error (WinCE does not support setting of current directory).
* All file/dir paths must be absolute (started with \).
* WinCE is unicode OS. All string parameters to API calls must be PWideChar.
* WinCE does not have support for environment strings.
* WinCE does not have support for console applications by default. But you can install console support by yourself. Please note that FPC creates GUI applications for WinCE target by default. To create console application you should use -WC compiler switch or put {$APPTYPE CONSOLE} directive to source code.<br>To enable console in WinCE install one of the following programs:

  - PocketCMD by SymbolicTools. It is recommended solution. Get it here: http://www.symbolictools.de/public/pocketconsole/applications/PocketCMD

  - PPC Command Shell from Microsoft Windows Mobile Developer Power Toys. Get it here: http://www.microsoft.com/downloads/details.aspx?FamilyID=74473fd6-1dcc-47aa-ab28-6a2b006edfe9&displaylang=en

PPC Command Shell have less features than PocketCMD. Also it have some issues. One of them - a new console window is opened even if an application is started from a console command prompt.

Links
-----
* WinCE Port page at Free Pascal Wiki: http://www.freepascal.org/wiki/index.php/WinCE_port
* Useful WinCE info: http://www.rainer-keuchel.de/documents.html

Contacts
--------
Write any questions regarding WinCE port to Yury Sidorov yury_sidorov@mail.ru
