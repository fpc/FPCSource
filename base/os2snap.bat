@echo off

rem ***  $Id$

rem *** Batch file for creating of FPC snapshot for OS/2.
rem *** FPCDIR variable must be set to your base FPC directory and
rem *** must _not_ contain forward slashes (only backslashes allowed).
rem *** Please, note, that you need to have enough space for environment
rem *** variables to run this batch (don't try to run it under Norton
rem *** Commander or similar programs under DOS with COMMAND.COM as shell).
rem *** Your compiler (PPC386.EXE per default) and AS.EXE must be somewhere
rem *** on PATH (unless you set path to them explicitly using FPCTOOLS
rem *** variable, which must end with \ if present).
rem *** One of the following parameters may be specified: rtl, compiler,
rem *** both, cycle and snapshot - "snapshot" being the default, optionally
rem *** followed by second parameter "debug" causing debugging symbols
rem *** not to be stripped from the created compiler (parameter _must_ be
rem *** in lowercase to be recognized correctly, unless running under 4dos).
rem *** Meaning of parameters:
rem ***  rtl .......... RTL only, no snapshot created
rem ***  compiler ..... compiler only, no snapshot created
rem ***  both ......... both RTL and compiler, no snapshot created
rem ***  snapshot ..... both RTL and compiler, snapshot _is_ created
rem ***  cycle ........ RTL and compiler compiled, the resulting compiler
rem ***                 is then copied to /BIN/OS2 (backing up previous
rem ***                 version to ppos2.old) and used to compile both RTL and
rem ***                 compiler again, and then finally snapshot is created
rem *** PPC386.EXE is used for the compilation (for the first one only with
rem *** "cycle"), unless a different compiler name (e.g. PPOS2.EXE)
rem *** is specified in COMPILER variable.
rem *** Environment variable OTHEROPTS may be used to specify additional
rem *** switches (e.g. setting level of verbosity, etc.).

echo *"Makefile" for OS/2:

echo *Setting up environment ...

rem Check whether FPCDIR exists
if %FPCDIR%. == . goto ErrorDir
if exist %FPCDIR% goto DirOK
if exist %FPCDIR%\. goto DirOK
if exist %FPCDIR%\makefile goto DirOK
if exist %FPCDIR%\SOURCE\makefile goto DirOK
if exist %FPCDIR%\SOURCE\COMPILER\pp.pas goto DirOK
goto ErrorDir

:DirOK

rem Set path to source files
if exist %FPCDIR%\SOURCE goto SrcExists
if exist %FPCDIR%\SOURCE\. goto SrcExists
if exist %FPCDIR%\SOURCE\makefile goto SrcExists
set FPCSRC=%FPCDIR%
goto SetOpts

:SrcExists
set FPCSRC=%FPCDIR%\SOURCE

:SetOpts

rem Common options for OS/2 target
set OS2OPT=-TOS2 %OTHEROPTS%
rem Stack size for the compiler
set STACKOPT=-Cs64500
rem Options for OS/2 compiler
set OS2COPT=%OS2OPT% %STACKOPT%
rem Stripping sybols
set STRIPDEBUG=-Xs
rem Options and paths for the OS/2 RTL
set OS2RTL=-FU%FPCSRC%/RTL/OS2 %OS2OPT% %FPCSRC%/RTL/OS2/
rem Options and paths for the OS/2 RTL parts compiled from common sources
set OS2RTLC=-FU%FPCSRC%/RTL/OS2 %OS2OPT% %FPCSRC%/RTL/INC/
rem Options and paths for the OS/2 RTL parts compiled from processor dependent sources
set OS2RTLP=-FU%FPCSRC%/RTL/OS2 %OS2OPT% %FPCSRC%/RTL/I386/
rem Options and paths for the OS/2 RTL parts for Object Pascal extensions
set OS2RTLO=-FU%FPCSRC%/RTL/OS2 %OS2OPT% %FPCSRC%/RTL/OBJPAS/
rem Default compiler for the first compilation
set CYCLE=0
if .%COMPILER% == . goto SetCompiler
goto PrgFound
:SetCompiler
set COMPILER=PPC386.EXE

:PrgFound

echo *Searching for tools ...

if %FPCTOOLS%. == . goto SetupTools
goto ToolsOK

:SetupTools
if exist %FPCDIR%\BIN\%COMPILER% goto Tools1
if exist %FPCDIR%\BIN\%COMPILER%.EXE goto Tools1
goto NoTools1
:Tools1
if exist %FPCDIR%\BIN\AS.EXE goto Tools1OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory!
goto NoTools1
:Tools1OK
set FPCTOOLS=%FPCDIR%\BIN\
goto ToolsOK
:NoTools1
if exist %FPCDIR%\BIN\GO32V2\%COMPILER% goto Tools2
if exist %FPCDIR%\BIN\GO32V2\%COMPILER%.EXE goto Tools2
goto NoTools2
:Tools2
if exist %FPCDIR%\BIN\GO32V2\AS.EXE goto Tools2OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory!
goto NoTools2
:Tools2OK
set FPCTOOLS=%FPCDIR%\BIN\GO32V2\
goto ToolsOK
:NoTools2
if exist %FPCDIR%\BIN\GO32V1\%COMPILER% goto Tools3
if exist %FPCDIR%\BIN\GO32V1\%COMPILER%.EXE goto Tools3
goto NoTools3
:Tools3
if exist %FPCDIR%\BIN\GO32V1\AS.EXE goto Tools3OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory!
goto NoTools3
:Tools3OK
set FPCTOOLS=%FPCDIR%\BIN\GO32V1\
goto ToolsOK
:NoTools3
if exist %FPCDIR%\BIN\OS2\%COMPILER% goto Tools4
if exist %FPCDIR%\BIN\OS2\%COMPILER%.EXE goto Tools4
goto NoTools4
:Tools4
if exist %FPCDIR%\BIN\OS2\AS.EXE goto Tools4OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory!
goto NoTools4
:Tools4OK
set FPCTOOLS=%FPCDIR%\BIN\OS2\
goto ToolsOK
:NoTools4
echo *Warning: Cannot locate your %COMPILER% and AS.EXE, make sure they're on PATH!

:ToolsOK

echo *Checking parameters
set PARAMS=%1
if .%PARAMS% == . set PARAMS=snapshot
if %2. == debug set STRIPDEBUG=
if %@EVAL[0] == 0 goto Shl1
goto Cmd1
:Shl1
set PARAMS=%@LOWER[%PARAMS%]
if .%@LOWER[%2] == .debug set STRIPDEBUG=
:Cmd1
if %PARAMS% == clean goto CleanRTL
if %PARAMS% == both goto CleanRTL
if %PARAMS% == snapshot goto CleanRTL
if %PARAMS% == rtl goto CleanRTL
if %PARAMS% == cycle goto CleanRTL
if %PARAMS% == compiler goto CleanCompiler
echo *Error: Unknown parameter - %PARAMS%
goto End

:CleanRTL
if %@eval[0] == 0 goto JPCleanRTL
echo *Cleaning up the RTL (error messages are OK here) ...
del %FPCSRC%\RTL\OS2\*.ppo
del %FPCSRC%\RTL\OS2\*.oo2
del %FPCSRC%\RTL\OS2\ppas.bat
del %FPCSRC%\RTL\OS2\ppas.cmd
del %FPCSRC%\RTL\OS2\link.res
goto ContCleanRTL
:JPCleanRTL
echo *Cleaning up the RTL ...
del %FPCSRC%\RTL\OS2\*.ppo >& nul
del %FPCSRC%\RTL\OS2\*.oo2 >& nul
del %FPCSRC%\RTL\OS2\ppas.bat >& nul
del %FPCSRC%\RTL\OS2\ppas.cmd >& nul
del %FPCSRC%\RTL\OS2\link.res >& nul
:ContCleanRTL
if %PARAMS% == rtl goto Branches
:CleanCompiler
if %@eval[0] == 0 goto JPCleanComp
echo *Cleaning up the compiler (error messages are OK here) ...
del %FPCSRC%\COMPILER\*.ppo
del %FPCSRC%\COMPILER\*.oo2
del %FPCSRC%\COMPILER\pp
del %FPCSRC%\COMPILER\pp.exe
del %FPCSRC%\COMPILER\ppos2.exe
del %FPCSRC%\COMPILER\ppas.bat
del %FPCSRC%\COMPILER\ppas.cmd
del %FPCSRC%\COMPILER\link.res
goto ContCleanComp
:JPCleanComp
echo *Cleaning up the compiler ...
del %FPCSRC%\COMPILER\*.ppo >& nul
del %FPCSRC%\COMPILER\*.oo2 >& nul
del %FPCSRC%\COMPILER\pp >& nul
del %FPCSRC%\COMPILER\pp.exe >& nul
del %FPCSRC%\COMPILER\ppos2.exe >& nul
del %FPCSRC%\COMPILER\ppas.bat >& nul
del %FPCSRC%\COMPILER\ppas.cmd >& nul
del %FPCSRC%\COMPILER\link.res >& nul
:ContCleanComp
if %PARAMS% == compiler goto Branches
if %PARAMS% == both goto Branches
:CleanSnapshot
if %@eval[0] == 0 goto JPCleanSnap
echo *Deleting the old snapshot (error messages are OK here) ...
del %FPCSRC%\snap-os2.zip
goto ContCleanSnap
:JPCleanSnap
echo *Deleting the old snapshot ...
del %FPCSRC%\snap-os2.zip >& nul
:ContCleanSnap
if %PARAMS% == clean goto End

:Branches
if %PARAMS% == both goto RTL1
if %PARAMS% == snapshot goto RTL1
if %PARAMS% == compiler goto Compiler
if %PARAMS% == rtl goto RTL1
if %PARAMS% == cycle goto RTL1
echo *Error: Unknown parameter - %PARAMS%
goto End

:RTL1
echo *Assembling the helpers ...
%FPCDIR%\BIN\OS2\as -o %FPCSRC%/RTL/OS2/prt0.oo2 %FPCSRC%/RTL/OS2/prt0.as
%FPCDIR%\BIN\OS2\as -o %FPCSRC%/RTL/OS2/prt1.oo2 %FPCSRC%/RTL/OS2/prt1.as
%FPCDIR%\BIN\OS2\as -o %FPCSRC%/RTL/OS2/code2.oo2 %FPCSRC%/RTL/OS2/code2.as
%FPCDIR%\BIN\OS2\as -o %FPCSRC%/RTL/OS2/code3.oo2 %FPCSRC%/RTL/OS2/code3.as
echo *Compiling the system unit ...
%FPCTOOLS%%COMPILER% -Sg -Us %OS2RTL%SYSOS2.PAS
echo *Compiling unit Objects ...
%FPCTOOLS%%COMPILER% %OS2RTLC%OBJECTS.PP
echo *Compiling unit Strings ...
%FPCTOOLS%%COMPILER% %OS2RTLC%STRINGS.PP
echo *Compiling unit HeapTrace ...
%FPCTOOLS%%COMPILER% %OS2RTLC%HEAPTRC.PP
echo *Compiling unit CPU ...
%FPCTOOLS%%COMPILER% %OS2RTLP%CPU.PP
echo *Compiling unit MMX ...
%FPCTOOLS%%COMPILER% %OS2RTLP%MMX.PP
echo *Compiling unit TypInfo ...
%FPCTOOLS%%COMPILER% %OS2RTLO%TYPINFO.PP
echo *Compiling unit DosCalls ...
%FPCTOOLS%%COMPILER% %OS2RTL%DOSCALLS.PAS
echo *Compiling unit DOS ...
%FPCTOOLS%%COMPILER% %OS2RTL%DOS.PAS
echo *Compiling unit CRT ...
%FPCTOOLS%%COMPILER% %OS2RTL%CRT.PAS
echo *Compiling unit Printer ...
%FPCTOOLS%%COMPILER% %OS2RTL%PRINTER.PAS
echo *Compiling unit SysUtils ...
%FPCTOOLS%%COMPILER% %OS2RTLO%SYSUTILS.PP
echo *Compiling unit Math ...
%FPCTOOLS%%COMPILER% %OS2RTLO%MATH.PP
echo *Compiling unit UComplex ...
%FPCTOOLS%%COMPILER% %OS2RTLC%UCOMPLEX.PP
echo *Compiling unit GetOpts ...
%FPCTOOLS%%COMPILER% %OS2RTLC%GETOPTS.PP
echo *Compiling unit KbdCalls ...
%FPCTOOLS%%COMPILER% %OS2RTL%KBDCALLS.PAS
echo *Compiling unit MouCalls ...
%FPCTOOLS%%COMPILER% %OS2RTL%MOUCALLS.PAS
echo *Compiling unit VioCalls ...
%FPCTOOLS%%COMPILER% %OS2RTL%VIOCALLS.PAS
echo *Compiling unit Ports ...
%FPCTOOLS%%COMPILER% %OS2RTL%PORTS.PAS
echo *Compiling PM units ...
%FPCTOOLS%%COMPILER% %OS2RTL%PMWIN.PAS
%FPCTOOLS%%COMPILER% %OS2RTL%PMBITMAP.PAS
%FPCTOOLS%%COMPILER% %OS2RTL%PMGPI.PAS
echo *Compiling MMOS2 units ...
%FPCTOOLS%%COMPILER% %OS2RTL%DIVE.PAS

if %PARAMS% == rtl goto End

:Compiler
echo *Compiling the compiler itself ...
%FPCTOOLS%%COMPILER% %OS2COPT% %STRIPDEBUG% -FE%FPCSRC%/COMPILER -Fu%FPCSRC%/COMPILER -dGDB -dI386 %FPCSRC%/COMPILER/PP.PAS
:Comp2
ren %FPCSRC%\COMPILER\pp.exe ppos2.exe
if exist %FPCSRC%\COMPILER\ppos2.exe goto OKCompiler
if exist %FPCSRC%\COMPILER\ppas.bat goto PPasBat
if exist %FPCSRC%\COMPILER\ppas.bat goto PPasCmd
echo *Error: Compiler wasn't compiled!!
goto End

:PPasBat
echo *Automatic binding failed, trying again ...
call %FPCSRC%\COMPILER\ppas.bat
del %FPCSRC%\COMPILER\ppas.bat
goto Comp2
goto PPas

:PPasCmd
echo *Automatic binding failed, trying again ...
call %FPCSRC%\COMPILER\ppas.cmd
del %FPCSRC%\COMPILER\ppas.cmd
goto Comp2

:OKCompiler

if %PARAMS% == compiler goto End
if %PARAMS% == both goto End
if %PARAMS% == cycle goto Cycle
goto CheckEnv

:Cycle

rem Another loop?
if %CYCLE% == 2 goto CheckEnv
echo *Backing up previous compiler version ...
copy %FPCDIR%\BIN\OS2\ppos2.exe %FPCDIR%\BIN\OS2\ppos2.old
echo *Copying the newly created compiler to %FPCDIR%\BIN\OS2 ...
copy %FPCSRC%\COMPILER\ppos2.exe %FPCDIR%\BIN\OS2
if %CYCLE% == 1 goto Cycle2
set COMPILER=PPOS2.EXE
set CYCLE=1
goto SetupTools

:Cycle2
set CYCLE=2
goto SetupTools

:CheckEnv

if %@EVAL[0] == 0 goto Pack
echo *Warning: Packing in this environment might fail.
echo *You should press Ctrl-Break now if the current drive is different from that
echo *of %FPCDIR%; otherwise press any key to continue.
pause>nul
cd %FPCSRC%

:Pack
echo *Packing the snapshot ...
if %@EVAL[0] == 0 goto SHL2
goto Cmd2
:Shl2
pushd
cdd %FPCSRC%
:Cmd2

rem ZIP.EXE must be on the PATH
zip -9 -r snap-os2.zip compiler\ppos2.exe rtl\os2\*.ppo rtl\os2\*.oo2
if exist snap-os2.zip goto ZipOK
echo *Error: The ZIP file hasn't been created!!
:ZipOK
if %@EVAL[0] == 0 popd

echo *Done.

goto End
:ErrorDir
echo *Error: Environment variable FPCDIR must point to your base FPC directory!!!
goto End


  $Log$
  Revision 1.5  2000-01-02 16:38:51  hajny
    + unit Ports added

  Revision 1.3  1999/10/01 09:00:21  hajny
    + PMGPI and DIVE added

  Revision 1.2  1999/09/15 07:31:47  hajny
    + some units added, OTHEROPTS variable support



:End
