@echo off

rem ***  $Id$

rem *** Batch file for creating of FPC snapshot for OS/2.
rem *** FPCDIR variable must be set to your base FPC directory and
rem *** must _not_ contain forward slashes (only backslashes allowed).
rem *** Your compiler (PPOS2.EXE per default) and AS.EXE must be somewhere
rem *** on PATH (unless you set path to them explicitly using FPCTOOLS
rem *** variable, which must end with \ if present). However, be sure which
rem *** version of AS.EXE, etc. gets called if several such files exist.
rem *** One of the following parameters may be specified: rtl, compiler,
rem *** both, cycle and snapshot ("snapshot" being the default), optionally
rem *** followed by a second parameter "debug" (causing debugging symbols
rem *** not to be stripped from the created compiler), or "release" (code
rem *** optimization, debug info stripped out). Parameters _must_ be in
rem *** lowercase to be recognized correctly, unless running under 4os2).
rem *** Meaning of parameters:
rem ***  rtl .......... RTL only, _no_ snapshot created
rem ***  compiler ..... compiler only, _no_ snapshot created
rem ***  both ......... both RTL and compiler, _no_ snapshot created
rem ***  snapshot ..... both RTL and compiler, snapshot _is_ created
rem ***  cycle ........ RTL and compiler compiled, the resulting compiler
rem ***                 is then copied to %FPCTOOLS% (BIN\OS2 by default)
rem ***                 backing up possible previous version to ppos2.x),
rem ***                 the whole procedure is started again (RTL is compiled
rem ***                 with the new compiler version this time) and after
rem ***                 another cycle (to make sure the new compiler works
rem ***                 correctly) the snapshot is finally created
rem *** PPOS2.EXE is used for the compilation, unless a different compiler name
rem *** is specified in FPCCOMPILER variable. In any case, the compiler should
rem *** reside in the same directory as the other required tools (AS.EXE,
rem *** LD.EXE, etc.).
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

rem Set path to the source files
if exist %FPCDIR%\SOURCE goto SrcExists
if exist %FPCDIR%\SOURCE\. goto SrcExists
if exist %FPCDIR%\SOURCE\makefile goto SrcExists
if exist %FPCDIR%\SOURCE\COMPILER\pp.pas goto SrcExists
set FPCSRC=%FPCDIR%
goto SetOpts

:SrcExists
set FPCSRC=%FPCDIR%\SOURCE

:SetOpts

rem Path to file with options
set OS2OPTF=%FPCSRC%\OS2SNAP.OPT
rem Path for the OS/2 RTL
set OS2RTL=%FPCSRC%\RTL\OS2
rem Path for OS/2 RTL parts compiled from common sources
set OS2RTLC=%FPCSRC%\RTL\INC
rem Path for OS/2 RTL parts compiled from processor dependent sources
set OS2RTLP=%FPCSRC%\RTL\I386
rem Path for OS/2 RTL parts for Object Pascal extensions
set OS2RTLO=%FPCSRC%\RTL\OBJPAS
rem Path to the compiler source
set COMPSPATH=%FPCSRC%\COMPILER
rem Option to skip the default configuration file
set SKIPCFG=-n
rem Common options for OS/2 target
set OS2OPT1=-TOS2
set OS2OPT2=-dGDB
set OS2OPT3=-dI386
set OS2OPT4=-Sg
rem "Release" options (optimizations, strip debug symbols)
set RELEASEOPT1=-Og2p1
set RELEASEOPT2=-Xs
rem "Debug" options (add debug symbols, do all code generation checks)
set DEBUGOPT1=-g
set DEBUGOPT2=-Crtoi
rem Place for debug or release options, empty by default
set CURRENTOPT1=
set CURRENTOPT2=
rem Stack size for the compiler
set STACKOPT=-Cs64500
rem Path to object files
set OS2OBJP=-Fo%OS2RTL%
rem Path to units
set OS2UNITP=-Fu%OS2RTL%
rem Path to compiler units
set COMPUNITP=-Fu%COMPSPATH%
rem Path to compiler include files
set COMPINCP=-Fi%COMPSPATH%
rem Path to compiler object files
set COMPOBJP=-Fo%COMPSPATH%
rem Target path for units
set OS2UNITT=-FU%OS2RTL%
rem Target path for executables
set OS2EXET=-FE%COMPSPATH%
rem Path to include files
set OS2INCP=-Fi%OS2RTL%;%OS2RTLC%;%OS2RTLO%;%OS2RTLP%
rem Default compiler for the first compilation
set CYCLE=0
set COMPILER=%FPCCOMPILER%
if .%FPCCOMPILER% == . goto SetCompiler
goto PrgFound
:SetCompiler
set COMPILER=PPOS2.EXE

:PrgFound

echo *Searching for tools ...

set REALTOOLS=%FPCTOOLS%
if %FPCTOOLS%. == . goto SetupTools
goto ToolsOK

:SetupTools
if exist %FPCDIR%\BIN\OS2\%COMPILER% goto Tools1
if exist %FPCDIR%\BIN\OS2\%COMPILER%.EXE goto Tools1
goto NoTools1
:Tools1
if exist %FPCDIR%\BIN\OS2\AS.EXE goto Tools1OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory!
goto NoTools1
:Tools1OK
set REALTOOLS=%FPCDIR%\BIN\OS2\
goto ToolsOK
:NoTools1
if exist %FPCDIR%\BIN\%COMPILER% goto Tools2
if exist %FPCDIR%\BIN\%COMPILER%.EXE goto Tools2
goto NoTools2
:Tools2
if exist %FPCDIR%\BIN\AS.EXE goto Tools2OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory!
goto NoTools2
:Tools2OK
set REALTOOLS=%FPCDIR%\BIN\
goto ToolsOK
:NoTools2
echo *Warning: Cannot locate your %COMPILER% and AS.EXE, make sure they're on PATH!

:ToolsOK

echo *Checking parameters
set PARAMS=%1
if .%PARAMS% == . set PARAMS=snapshot
if %2. == debug set CURRENTOPT1=%DEBUGOPT1%
if %2. == debug set CURRENTOPT2=%DEBUGOPT2%
if %2. == release set CURRENTOPT1=%RELEASEOPT1%
if %2. == release set CURRENTOPT2=%RELEASEOPT2%
if %@EVAL[0] == 0 goto Shl1
goto Cmd1
:Shl1
set PARAMS=%@LOWER[%PARAMS%]
if .%@LOWER[%2] == .debug set CURRENTOPT1=%DEBUGOPT1%
if .%@LOWER[%2] == .debug set CURRENTOPT2=%DEBUGOPT2%
if .%@LOWER[%2] == .release set CURRENTOPT1=%RELEASEOPT1%
if .%@LOWER[%2] == .release set CURRENTOPT2=%RELEASEOPT2%
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
del %OS2OPTF%
del %OS2RTL%\*.ppo
del %OS2RTL%\*.oo2
del %OS2RTL%\ppas.bat
del %OS2RTL%\ppas.cmd
del %OS2RTL%\link.res
goto ContCleanRTL
:JPCleanRTL
echo *Cleaning up the RTL ...
del %OS2OPTF% >& nul
del %OS2RTL%\*.ppo >& nul
del %OS2RTL%\*.oo2 >& nul
del %OS2RTL%\ppas.bat >& nul
del %OS2RTL%\ppas.cmd >& nul
del %OS2RTL%\link.res >& nul
:ContCleanRTL
if %PARAMS% == rtl goto Branches
:CleanCompiler
if %@eval[0] == 0 goto JPCleanComp
echo *Cleaning up the compiler (error messages are OK here) ...
del %OS2OPTF%
del %COMPSPATH%\*.ppo
del %COMPSPATH%\*.oo2
del %COMPSPATH%\pp
del %COMPSPATH%\pp.exe
del %COMPSPATH%\ppos2.exe
del %COMPSPATH%\ppas.bat
del %COMPSPATH%\ppas.cmd
del %COMPSPATH%\link.res
goto ContCleanComp
:JPCleanComp
echo *Cleaning up the compiler ...
del %OS2OPTF% >& nul
del %COMPSPATH%\*.ppo >& nul
del %COMPSPATH%\*.oo2 >& nul
del %COMPSPATH%\pp >& nul
del %COMPSPATH%\pp.exe >& nul
del %COMPSPATH%\ppos2.exe >& nul
del %COMPSPATH%\ppas.bat >& nul
del %COMPSPATH%\ppas.cmd >& nul
del %COMPSPATH%\link.res >& nul
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
echo *Creating file with all the needed options and paths for RTL ...
echo %SKIPCFG% > %OS2OPTF%
echo %OS2OPT1% >> %OS2OPTF%
echo %OS2OPT2% >> %OS2OPTF%
echo %OS2OPT3% >> %OS2OPTF%
echo %OS2OPT4% >> %OS2OPTF%
echo %OS2OBJP% >> %OS2OPTF%
echo %OS2UNITP% >> %OS2OPTF%
echo %OS2INCP% >> %OS2OPTF%
echo %OS2UNITT% >> %OS2OPTF%
echo -FD%REALTOOLS% >> %OS2OPTF%
if not .%CURRENTOPT1% == . echo %CURRENTOPT1% >> %OS2OPTF%
if not .%CURRENTOPT2% == . echo %CURRENTOPT2% >> %OS2OPTF%
echo *Assembling the helpers ...
%REALTOOLS%\as -o %OS2RTL%\prt0.oo2 %OS2RTL%\prt0.as
%REALTOOLS%\as -o %OS2RTL%\prt1.oo2 %OS2RTL%\prt1.as
%REALTOOLS%\as -o %OS2RTL%\code2.oo2 %OS2RTL%\code2.as
%REALTOOLS%\as -o %OS2RTL%\code3.oo2 %OS2RTL%\code3.as
echo *Compiling the system unit ...
%REALTOOLS%%COMPILER% @%OS2OPTF% -Us %OTHEROPTS% %OS2RTL%\SYSOS2.PAS
echo *Compiling unit Objects ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\OBJECTS.PP
echo *Compiling unit Strings ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\STRINGS.PP
echo *Compiling unit HeapTrace ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\HEAPTRC.PP
echo *Compiling unit CPU ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLP%\CPU.PP
echo *Compiling unit MMX ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLP%\MMX.PP
echo *Compiling unit TypInfo ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLO%\TYPINFO.PP
echo *Compiling unit DosCalls ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\DOSCALLS.PAS
echo *Compiling unit DOS ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\DOS.PAS
echo *Compiling unit CRT ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\CRT.PAS
echo *Compiling unit Printer ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PRINTER.PAS
echo *Compiling unit SysUtils ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLO%\SYSUTILS.PP
echo *Compiling unit Math ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLO%\MATH.PP
echo *Compiling unit UComplex ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\UCOMPLEX.PP
echo *Compiling unit GetOpts ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\GETOPTS.PP
echo *Compiling unit KbdCalls ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\KBDCALLS.PAS
echo *Compiling unit MouCalls ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\MOUCALLS.PAS
echo *Compiling unit VioCalls ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\VIOCALLS.PAS
echo *Compiling unit MonCalls ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\MONCALLS.PAS
echo *Compiling unit Ports ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PORTS.PAS
echo *Compiling PM units ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PMWIN.PAS
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PMBITMAP.PAS
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PMGPI.PAS
echo *Compiling MMOS2 units ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\DIVE.PAS

if %PARAMS% == rtl goto End

:Compiler
echo *Creating file with all the needed options and paths for the compiler ...
echo %SKIPCFG% > %OS2OPTF%
echo %OS2OPT1% >> %OS2OPTF%
echo %OS2OPT2% >> %OS2OPTF%
echo %OS2OPT3% >> %OS2OPTF%
echo %OS2OPT4% >> %OS2OPTF%
echo %OS2OBJP% >> %OS2OPTF%
echo %OS2UNITP% >> %OS2OPTF%
echo -FD%REALTOOLS% >> %OS2OPTF%
echo %COMPUNITP% >> %OS2OPTF%
echo %COMPINCP% >> %OS2OPTF%
echo %COMPOBJP% >> %OS2OPTF%
echo %STACKOPT% >> %OS2OPTF%
echo %OS2EXET% >> %OS2OPTF%
if not .%CURRENTOPT1% == . echo %CURRENTOPT1% >> %OS2OPTF%
if not .%CURRENTOPT2% == . echo %CURRENTOPT2% >> %OS2OPTF%
echo *Compiling the compiler ...
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %COMPSPATH%\PP.PAS
:Comp2
ren %COMPSPATH%\pp.exe ppos2.exe
if exist %COMPSPATH%\ppos2.exe goto OKCompiler
if exist %COMPSPATH%\ppas.bat goto PPasBat
if exist %COMPSPATH%\ppas.cmd goto PPasCmd
echo *Error: The compiler wasn't compiled!!
goto End

:PPasBat
echo *Automatic binding failed, trying again ...
call %COMPSPATH%\ppas.bat
del %COMPSPATH%\ppas.bat
goto Comp2
goto PPas

:PPasCmd
echo *Automatic binding failed, trying again ...
call %COMPSPATH%\ppas.cmd
del %COMPSPATH%\ppas.cmd
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
copy %REALTOOLS%ppos2.exe %REALTOOLS%ppos2.%CYCLE%
echo *Copying the newly created compiler to %REALTOOLS% ...
copy %COMPSPATH%\ppos2.exe %REALTOOLS%.
if %CYCLE% == 1 goto Cycle2
set COMPILER=PPOS2.EXE
set CYCLE=1
goto Cmd1

:Cycle2
set CYCLE=2
goto Cmd1

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
zip -9 -r snap-os2.zip compiler\ppos2.exe rtl\os2\*.ppo rtl\os2\*.oo2 rtl\os2\*.ao2
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
  Revision 1.6  2000-01-16 18:44:21  hajny
    * got rid of PPC386.CFG dependency

  Revision 1.3  1999/10/01 09:00:21  hajny
    + PMGPI and DIVE added

  Revision 1.2  1999/09/15 07:31:49  hajny
    + some units added, OTHEROPTS variable support



:End
