@echo off

rem ***  $Id: os2snap.cmd,v 1.6 2001/06/28 21:09:43 peter Exp $

rem *** Batch file for creating of FPC snapshot for OS/2.
rem *** FPCDIR variable must be set to your base FPC directory and
rem *** must _not_ contain forward slashes (only backslashes allowed).
rem *** Your compiler (PPOS2.EXE per default) and AS.EXE must be somewhere
rem *** on PATH (unless you set path to them explicitly using FPCTOOLS
rem *** variable, which must end with \ if present). However, be sure which
rem *** version of AS.EXE, etc. gets called if several such files exist.
rem *** One of the following parameters may be specified: rtl, compiler,
rem *** both, cycle and snapshot ("snapshot" being the default), optionally
rem *** followed by parameters "debug" (causing debugging symbols not to be
rem *** stripped from the created compiler), "release" (code optimization,
rem *** debug info stripped out), and "verbose" (compiler messages are
rem *** shown; the same can be accomplished with setting environment
rem *** variable DOVERBOSE to 1). Parameters "debug" and "release" are
rem *** mutually exclusive (the later one is used if both are present).
rem *** Parameter "ppas" forces only PPAS script to be created
rem *** by the compiler and called manually afterwards. This might help
rem *** to resolve LD crashes due to low stack (e.g. under WinXX).
rem *** Parameters _must_ be in lowercase to be recognized correctly,
rem *** unless running under 4os2 or compatible.
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
rem *** Environment variable FPCLOG can specify a file for error logging
rem *** (full path needed). Please, note, that the previous contents of
rem *** file will be overwritten each time the batch file is run.
rem *** Environment variable VERBOSEOPT may be used to specify level of
rem *** verbosity used with "verbose" parameter (-va by default, i.e. show
rem *** everything). Another way would be specifying this in OTHEROPTS
rem *** variable (see above) and not using "verbose" at all.
rem *** Environment variable FPCSNAPPATH may be used to specify path, where
rem *** the compiled files should be placed and possibly the ZIP file
rem *** created.

set FPCERRLOG=%FPCLOG%
if .%FPCERRLOG% == . set FPCERRLOG=CON

echo *"Makefile" for OS/2: > %FPCERRLOG%

echo *Setting up environment ... >> %FPCERRLOG%

rem Check whether FPCDIR exists
if .%FPCDIR% == . goto ErrorDir
if exist %FPCDIR% goto DirOK
if exist %FPCDIR%\. goto DirOK
if exist %FPCDIR%\makefile goto DirOK
if exist %FPCDIR%\COMPILER\pp.pas goto DirOK
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
goto SnapDir

:SrcExists
set FPCSRC=%FPCDIR%\SOURCE

:SnapDir
set FPCSNAP=%FPCSNAPPATH%
if .%FPCSNAP% == . set FPCSNAP=%FPCSRC%\SNAPSHOT
if exist %FPCSNAP% goto SnapExists
echo *Creating directories for the snapshot ... >> %FPCERRLOG%
mkdir %FPCSNAP% >> %FPCERRLOG%
if exist %FPCSNAP% goto SnapExists
echo *Error: Cannot create the directory %FPCSNAP%!!
goto End

:SnapExists
if exist %FPCSNAP%\BIN goto BinExists
echo *Creating directories for the snapshot (binaries) ... >> %FPCERRLOG%
mkdir %FPCSNAP%\BIN >> %FPCERRLOG%
if exist %FPCSNAP%\BIN goto BinExists
echo *Error: Cannot create the directory %FPCSNAP%\BIN!!
goto End

:BinExists
set FPCSNAPBIN=%FPCSNAP%\BIN\OS2
if exist %FPCSNAPBIN% goto BinOS2Exists
echo *Creating directories for the snapshot (binaries for OS/2) ... >> %FPCERRLOG%
mkdir %FPCSNAPBIN% >> %FPCERRLOG%
if exist %FPCSNAPBIN% goto BinOS2Exists
echo *Error: Cannot create the directory %FPCSNAPBIN%!!
goto End

:BinOS2Exists
set FPCSNAPMSG=%FPCSNAP%\MSG
if exist %FPCSNAPMSG% goto MsgExists
echo *Creating directories for the snapshot (messages) ... >> %FPCERRLOG%
mkdir %FPCSNAPMSG% >> %FPCERRLOG%
if exist %FPCSNAPMSG% goto MsgExists
echo *Error: Cannot create the directory %FPCSNAPMSG%!!
goto End

:MsgExists
if exist %FPCSNAP%\UNITS goto UnitsExists
echo *Creating directories for the snapshot (units) ... >> %FPCERRLOG%
mkdir %FPCSNAP%\UNITS >> %FPCERRLOG%
if exist %FPCSNAP%\UNITS goto UnitsExists
echo *Error: Cannot create the directory %FPCSNAP%\UNITS!!
goto End

:UnitsExists
if exist %FPCSNAP%\UNITS\OS2 goto UnitsOS2Exists
echo *Creating directories for the snapshot (units for OS/2) ... >> %FPCERRLOG%
mkdir %FPCSNAP%\UNITS\OS2 >> %FPCERRLOG%
if exist %FPCSNAP%\UNITS\OS2 goto UnitsOS2Exists
echo *Error: Cannot create the directory %FPCSNAP%\UNITS\OS2!!
goto End

:UnitsOS2Exists
set FPCSNAPRTL=%FPCSNAP%\UNITS\OS2\RTL
if exist %FPCSNAPRTL% goto OS2RTLExists
echo *Creating directories for the snapshot (units for OS/2 RTL) ... >> %FPCERRLOG%
mkdir %FPCSNAPRTL% >> %FPCERRLOG%
if exist %FPCSNAPRTL% goto OS2RTLExists
echo *Error: Cannot create the directory %FPCSNAPRTL%!!
goto End

:OS2RTLExists
set FPCSNAPDOC=%FPCSNAP%\DOC
if exist %FPCSNAPDOC% goto SetOpts
echo *Creating directories for the snapshot (documentation) ... >> %FPCERRLOG%
mkdir %FPCSNAPDOC% >> %FPCERRLOG%
if exist %FPCSNAPDOC% goto SetOpts
echo *Error: Cannot create the directory %FPCSNAPDOC%!!
goto End

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
rem "Verbose" options
if .%VERBOSEOPT% == . set VERBOSEOPT=-va
set DOVERBOSE=
rem Place for debug or release options, empty by default
set CURRENTOPT1=
set CURRENTOPT2=
rem Stack size for the compiler
rem set STACKOPT=-Cs64500
set STACKOPT=-Cs256000
rem Path to object files
set OS2OBJP=-Fo%OS2RTL%
rem Path to units
set OS2UNITP=-Fu%FPCSNAPRTL%
rem Path to compiler units
set COMPUNITP=-Fu%COMPSPATH%;%COMPSPATH%\i386;%COMPSPATH%\targets
set COMPUNIT386=-Fu%COMPSPATH%\i386
set COMPUNITTRG=-Fu%COMPSPATH%\targets

rem Path to compiler include files
set COMPINCP=-Fi%COMPSPATH%;%COMPSPATH%\i386
set COMPINCP386=-Fu%COMPSPATH%\i386
rem Path to compiler object files


set COMPOBJP=-Fo%COMPSPATH%
rem Target path for RTL units
set OS2UNITT=-FU%FPCSNAPRTL%
rem Fake target path for executables for RTL compilation (path for PPAS)
set OS2UNITE=-FE%FPCSNAPRTL%
rem Target path for executables
set OS2EXET=-FE%FPCSNAPBIN%
rem Path to include files
set OS2INCP=-Fi%OS2RTL%;%OS2RTLC%;%OS2RTLO%;%OS2RTLP%
rem PPAS step disabled by default
set FORCEPPAS=
rem Name of the PPAS script
set PPASNAME=PPAS.CMD
rem Default compiler for the first compilation
set CYCLE=0
set COMPILER=%FPCCOMPILER%
if .%FPCCOMPILER% == . goto SetCompiler
goto PrgFound
:SetCompiler
set COMPILER=PPOS2.EXE

:PrgFound

echo *Searching for tools ... >> %FPCERRLOG%

set REALTOOLS=%FPCTOOLS%
if %FPCTOOLS%. == . goto SetupTools
goto ToolsOK

:SetupTools
if exist %FPCDIR%\BIN\OS2\%COMPILER% goto Tools1
if exist %FPCDIR%\BIN\OS2\%COMPILER%.EXE goto Tools1
goto NoTools1
:Tools1
if exist %FPCDIR%\BIN\OS2\AS.EXE goto Tools1OK
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory! >> %FPCERRLOG%
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
echo *Warning: %COMPILER% found, but AS.EXE isn't in the same directory! >> %FPCERRLOG%
goto NoTools2
:Tools2OK
set REALTOOLS=%FPCDIR%\BIN\
goto ToolsOK
:NoTools2
echo *Warning: Cannot locate your %COMPILER% and AS.EXE, make sure they're on PATH! >> %FPCERRLOG%

:ToolsOK

echo *Checking parameters >> %FPCERRLOG%
set PARAMS=%1
if .%PARAMS% == . set PARAMS=snapshot
if not %@EVAL[0] == 0 goto ParLoop
set PARAMS=%@LOWER[%PARAMS%]
:ParLoop
shift
if %1. == . goto NoPars
if %@EVAL[0] == 0 goto Shl1
if %1 == debug set CURRENTOPT1=%DEBUGOPT1%
if %1 == debug set CURRENTOPT2=%DEBUGOPT2%
if %1 == release set CURRENTOPT1=%RELEASEOPT1%
if %1 == release set CURRENTOPT2=%RELEASEOPT2%
if %1 == verbose set DOVERBOSE=1
if %1 == ppas set FORCEPPAS=1
goto ParLoop
:Shl1
if %@LOWER[%1] == debug set CURRENTOPT1=%DEBUGOPT1%
if %@LOWER[%1] == debug set CURRENTOPT2=%DEBUGOPT2%
if %@LOWER[%1] == release set CURRENTOPT1=%RELEASEOPT1%
if %@LOWER[%1] == release set CURRENTOPT2=%RELEASEOPT2%
if %@LOWER[%1] == verbose set DOVERBOSE=1
if %@LOWER[%1] == ppas set FORCEPPAS=1
goto ParLoop
:NoPars
if %PARAMS% == clean goto CleanRTL
if %PARAMS% == both goto CleanRTL
if %PARAMS% == snapshot goto CleanRTL
if %PARAMS% == rtl goto CleanRTL
if %PARAMS% == cycle goto CleanRTL
if %PARAMS% == compiler goto CleanCompiler
echo *Error: Unknown parameter - %PARAMS% >> %FPCERRLOG%
goto End

:CleanRTL
if %@EVAL[0] == 0 goto JPCleanRTL
echo *Cleaning up the RTL (error messages are OK here) ... >> %FPCERRLOG%
del %OS2OPTF% >> %FPCERRLOG%
del %OS2RTL%\*.ppo >> %FPCERRLOG%
del %OS2RTL%\*.oo2 >> %FPCERRLOG%
del %OS2RTL%\ppas.bat >> %FPCERRLOG%
del %OS2RTL%\ppas.cmd >> %FPCERRLOG%
del %OS2RTL%\link.res >> %FPCERRLOG%
del %FPCSNAPRTL%\*.ppo >> %FPCERRLOG%
del %FPCSNAPRTL%\*.oo2 >> %FPCERRLOG%
del %FPCSNAPRTL%\ppas.bat >> %FPCERRLOG%
del %FPCSNAPRTL%\ppas.cmd >> %FPCERRLOG%
del %FPCSNAPRTL%\link.res >> %FPCERRLOG%
goto ContClRTL
:JPCleanRTL
echo *Cleaning up the RTL ... >> %FPCERRLOG%
del %OS2OPTF% >& nul >> %FPCERRLOG%
del %OS2RTL%\*.ppo >& nul >> %FPCERRLOG%
del %OS2RTL%\*.oo2 >& nul >> %FPCERRLOG%
del %OS2RTL%\ppas.bat >& nul >> %FPCERRLOG%
del %OS2RTL%\ppas.cmd >& nul >> %FPCERRLOG%
del %OS2RTL%\link.res >& nul >> %FPCERRLOG%
del %FPCSNAPRTL%\*.ppo >& nul >> %FPCERRLOG%
del %FPCSNAPRTL%\*.oo2 >& nul >> %FPCERRLOG%
del %FPCSNAPRTL%\ppas.bat >& nul >> %FPCERRLOG%
del %FPCSNAPRTL%\ppas.cmd >& nul >> %FPCERRLOG%
del %FPCSNAPRTL%\link.res >& nul >> %FPCERRLOG%
:ContClRTL
if %PARAMS% == rtl goto Branches
:CleanCompiler
if %@EVAL[0] == 0 goto JPCleanComp
echo *Cleaning up the compiler (error messages are OK here) ... >> %FPCERRLOG%
del %OS2OPTF% >> %FPCERRLOG%
del %COMPSPATH%\*.ppo >> %FPCERRLOG%
del %COMPSPATH%\*.oo2 >> %FPCERRLOG%
del %COMPSPATH%\pp >> %FPCERRLOG%
del %COMPSPATH%\pp.exe >> %FPCERRLOG%
del %COMPSPATH%\ppos2.exe >> %FPCERRLOG%
del %COMPSPATH%\ppas.bat >> %FPCERRLOG%
del %COMPSPATH%\ppas.cmd >> %FPCERRLOG%
del %COMPSPATH%\link.res >> %FPCERRLOG%
del %FPCSNAPBIN%\*.ppo >> %FPCERRLOG%
del %FPCSNAPBIN%\*.oo2 >> %FPCERRLOG%
del %FPCSNAPBIN%\pp >> %FPCERRLOG%
del %FPCSNAPBIN%\pp.exe >> %FPCERRLOG%
del %FPCSNAPBIN%\ppos2.exe >> %FPCERRLOG%
del %FPCSNAPBIN%\ppas.bat >> %FPCERRLOG%
del %FPCSNAPBIN%\ppas.cmd >> %FPCERRLOG%
del %FPCSNAPBIN%\link.res >> %FPCERRLOG%
goto ContClComp
:JPCleanComp
echo *Cleaning up the compiler ... >> %FPCERRLOG%
del %OS2OPTF% >& nul >> %FPCERRLOG%
del %COMPSPATH%\*.ppo >& nul >> %FPCERRLOG%
del %COMPSPATH%\*.oo2 >& nul >> %FPCERRLOG%
del %COMPSPATH%\pp >& nul >> %FPCERRLOG%
del %COMPSPATH%\pp.exe >& nul >> %FPCERRLOG%
del %COMPSPATH%\ppos2.exe >& nul >> %FPCERRLOG%
del %COMPSPATH%\ppas.bat >& nul >> %FPCERRLOG%
del %COMPSPATH%\ppas.cmd >& nul >> %FPCERRLOG%
del %COMPSPATH%\link.res >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\*.ppo >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\*.oo2 >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\pp >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\pp.exe >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\ppos2.exe >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\ppas.bat >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\ppas.cmd >& nul >> %FPCERRLOG%
del %FPCSNAPBIN%\link.res >& nul >> %FPCERRLOG%
:ContClComp
if %PARAMS% == compiler goto Branches
if %PARAMS% == both goto Branches
:CleanSnapshot
if %@EVAL[0] == 0 goto JPCleanSnap
echo *Deleting the old snapshot (error messages are OK here) ... >> %FPCERRLOG%
del %FPCSNAPDOC%\*.txt >> %FPCERRLOG%
del %FPCSNAPDOC%\*.htm* >> %FPCERRLOG%
del %FPCSNAPDOC%\copying.* >> %FPCERRLOG%
del %FPCSNAPMSG%\*.msg >> %FPCERRLOG%
del %FPCSNAP%\baseemx.zip >> %FPCERRLOG%
goto ContClSnap
:JPCleanSnap
echo *Deleting the old snapshot ... >> %FPCERRLOG%
del %FPCSNAP%\baseemx.zip >& nul >> %FPCERRLOG%
del %FPCSNAPDOC%\*.txt >& nul >> %FPCERRLOG%
del %FPCSNAPDOC%\*.htm* >& nul >> %FPCERRLOG%
del %FPCSNAPDOC%\copying.* >& nul >> %FPCERRLOG%
del %FPCSNAPMSG%\*.msg >& nul >> %FPCERRLOG%
:ContClSnap
if %PARAMS% == clean goto End

:Branches
if %PARAMS% == both goto RTL1
if %PARAMS% == snapshot goto RTL1
if %PARAMS% == compiler goto Compiler
if %PARAMS% == rtl goto RTL1
if %PARAMS% == cycle goto RTL1
echo *Error: Unknown parameter - %PARAMS% >> %FPCERRLOG%
goto End

:RTL1
echo *Creating file with all the needed options and paths for RTL ... >> %FPCERRLOG%
echo %SKIPCFG% > %OS2OPTF%
echo %OS2OPT1% >> %OS2OPTF%
echo %OS2OPT2% >> %OS2OPTF%
echo %OS2OPT3% >> %OS2OPTF%
echo %OS2OPT4% >> %OS2OPTF%
echo %OS2OBJP% >> %OS2OPTF%
echo %OS2UNITP% >> %OS2OPTF%
echo %OS2INCP% >> %OS2OPTF%
echo %OS2UNITT% >> %OS2OPTF%
echo %OS2UNITE% >> %OS2OPTF%
echo -FD%REALTOOLS% >> %OS2OPTF%
if not .%CURRENTOPT1% == . echo %CURRENTOPT1% >> %OS2OPTF%
if not .%CURRENTOPT2% == . echo %CURRENTOPT2% >> %OS2OPTF%
if not .%FPCLOG% == . echo -Fe%FPCERRLOG% >> %OS2OPTF%
if not .%FORCEPPAS% == . echo -a >> %OS2OPTF%
if not .%FORCEPPAS% == . echo -s >> %OS2OPTF%
if .%DOVERBOSE% == .1 echo %VERBOSEOPT% >> %OS2OPTF%
if not .%DOVERBOSE% == .1 goto CompS1
echo *Start of basic options used for compilation >> %FPCERRLOG%
type %OS2OPTF% >> %FPCERRLOG%
echo *End of basic options used for compilation >> %FPCERRLOG%
if not .%OTHEROPTS% == . echo *User specified options: %OTHEROPTS% >> %FPCERRLOG%
:CompS1
echo *Assembling the helpers ... >> %FPCERRLOG%
%REALTOOLS%as -o %FPCSNAPRTL%\prt0.oo2 %OS2RTL%\prt0.as >> %FPCERRLOG%
%REALTOOLS%as -o %FPCSNAPRTL%\prt1.oo2 %OS2RTL%\prt1.as >> %FPCERRLOG%
%REALTOOLS%as -o %FPCSNAPRTL%\code2.oo2 %OS2RTL%\code2.as >> %FPCERRLOG%
%REALTOOLS%as -o %FPCSNAPRTL%\code3.oo2 %OS2RTL%\code3.as >> %FPCERRLOG%
echo *Compiling the system unit ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% -Us %OTHEROPTS% %OS2RTL%\SYSOS2.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit ObjPas ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLO%\OBJPAS.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit Objects ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\OBJECTS.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit Strings ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\STRINGS.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit HeapTrace ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\HEAPTRC.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit DosCalls ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\DOSCALLS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit DOS ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\DOS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit CPU ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLP%\CPU.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit MMX ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLP%\MMX.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit SysUtils ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\SYSUTILS.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit TypInfo ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLO%\TYPINFO.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit CRT ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\CRT.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit Printer ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PRINTER.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit Math ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLO%\MATH.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit UComplex ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\UCOMPLEX.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit GetOpts ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\GETOPTS.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit LineInfo ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTLC%\LINEINFO.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit KbdCalls ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\KBDCALLS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit MouCalls ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\MOUCALLS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit VioCalls ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\VIOCALLS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit MonCalls ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\MONCALLS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling unit Ports ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PORTS.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling PM units ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\OS2DEF.PAS
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PMWIN.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PMBITMAP.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\PMGPI.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
echo *Compiling MMOS2 units ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\DIVE.PAS
echo *Compiling API units... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\MOUSE.PP
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\KEYBOARD.PP
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %OS2RTL%\VIDEO.PP
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%
if .%FORCEPPAS% == .1 echo * Deleting the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 del %FPCSNAPRTL%\%PPASNAME% >> %FPCERRLOG%

if %PARAMS% == rtl goto End

:Compiler
echo *Creating file with all the needed options and paths for the compiler ... >> %FPCERRLOG%
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
if not .%FPCLOG% == . echo -Fe%FPCERRLOG% >> %OS2OPTF%
if not .%FORCEPPAS% == . echo -a >> %OS2OPTF%
if not .%FORCEPPAS% == . echo -s >> %OS2OPTF%
if .%DOVERBOSE% == .1 echo %VERBOSEOPT% >> %OS2OPTF%
if not .%DOVERBOSE% == .1 goto CompS2
echo *Start of basic options used for compilation >> %FPCERRLOG%
type %OS2OPTF% >> %FPCERRLOG%
echo *End of basic options used for compilation >> %FPCERRLOG%
if not .%OTHEROPTS% == . echo *User specified options: %OTHEROPTS% >> %FPCERRLOG%
:CompS2
echo *Compiling the compiler ... >> %FPCERRLOG%
%REALTOOLS%%COMPILER% @%OS2OPTF% %OTHEROPTS% %COMPSPATH%\PP.PAS
if .%FORCEPPAS% == .1 echo *Calling the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 call %FPCSNAPBIN%\%PPASNAME% >> %FPCERRLOG%
if .%FORCEPPAS% == .1 echo * Deleting the PPAS script >> %FPCERRLOG%
if .%FORCEPPAS% == .1 del %FPCSNAPBIN%\%PPASNAME% >> %FPCERRLOG%
if .%FORCEPPAS% == .1 del %FPCSNAPBIN%\link.res >> %FPCERRLOG%
:Comp2
ren %FPCSNAPBIN%\pp.exe ppos2.exe >> %FPCERRLOG%
copy %FPCSNAPBIN%\ppos2.exe ppc386.exe >> %FPCERRLOG%
if exist %FPCSNAPBIN%\ppos2.exe goto OKCompiler
if not exist %FPCSNAPBIN%\pp goto C2Cont
if exist %FPCSNAPBIN%\ppas.bat goto PPasBat
if exist %FPCSNAPBIN%\ppas.cmd goto PPasCmd
:C2Cont
echo *Error: The compiler wasn't compiled!! >> %FPCERRLOG%
goto End

:PPasCmd
echo *Automatic binding failed, trying again ... >> %FPCERRLOG%
call %FPCSNAPBIN%\ppas.cmd
del %FPCSNAPBIN%\ppas.cmd >> %FPCERRLOG%
goto Comp2

:PPasBat
echo *Automatic binding failed, trying again ... >> %FPCERRLOG%
call %FPCSNAPBIN%\ppas.bat
del %FPCSNAPBIN%\ppas.bat >> %FPCERRLOG%
goto Comp2

:OKCompiler
if %PARAMS% == compiler goto End
if %PARAMS% == both goto End
if %PARAMS% == cycle goto Cycle
goto CopyFiles

:Cycle

rem Another loop?
if %CYCLE% == 2 goto CopyFiles
echo *Backing up previous compiler version to ppos2.%CYCLE% ... >> %FPCERRLOG%
copy %REALTOOLS%ppos2.exe %REALTOOLS%ppos2.%CYCLE% >> %FPCERRLOG%
echo *Copying the newly created compiler to %REALTOOLS% ... >> %FPCERRLOG%
copy %FPCSNAPBIN%\ppos2.exe %REALTOOLS%. >> %FPCERRLOG%
copy %FPCSNAPBIN%\ppos2.exe %REALTOOLS%ppc386.exe >> %FPCERRLOG%
if %CYCLE% == 1 goto Cycle2
set COMPILER=PPOS2.EXE
set CYCLE=1
goto NoPars

:Cycle2
set CYCLE=2
goto NoPars

:CopyFiles
set FPCSNAPTXT=%FPCSNAPDOC%\snapshot.txt
echo *Copying the documentation ... >> %FPCERRLOG%
copy %FPCSRC%\INSTALL\DOC\*.txt %FPCSNAPDOC% >> %FPCERRLOG%
copy %FPCSRC%\INSTALL\DOC\*.htm* %FPCSNAPDOC% >> %FPCERRLOG%
copy %FPCSRC%\INSTALL\DOC\copying.* %FPCSNAPDOC% >> %FPCERRLOG%
echo *Creating the snapshot readme file ... >> %FPCERRLOG%
echo This is a FPC snapshot for OS/2. It contains compilation of the most current >> %FPCSNAPTXT%
echo developers' sources as of time of its creation. It contains the latest fixes >> %FPCSNAPTXT%
echo but might contain some new bugs as well, since it's less tested than regular >> %FPCSNAPTXT%
echo releases. Please, send your error reports to fpc-devel@lists.freepascal.org >> %FPCSNAPTXT%
echo mailing list (and don't forget to mention the fact you're not subscribed to >> %FPCSNAPTXT%
echo the list in your e-mail, if it's the case). >> %FPCSNAPTXT%
echo The snapshot has the same structure as the release ZIP files, so it may be >> %FPCSNAPTXT%
echo installed using the normal installer (INSTALL.EXE and INSTALL.DAT must be >> %FPCSNAPTXT%
echo in the same directory) or directly unzipped into your FPC tree. >> %FPCSNAPTXT%
echo *Copying the message files ... >> %FPCERRLOG%
copy %COMPSPATH%\msg\*.msg %FPCSNAPMSG% >> %FPCERRLOG%

if %@EVAL[0] == 0 goto Pack
echo *Warning: Packing in this environment might fail. >> %FPCERRLOG%
echo *You should press Ctrl-Break now if the current drive is different from that >> %FPCERRLOG%
echo *of %FPCSNAP%; otherwise press any key to continue. >> %FPCERRLOG%
if not %FPCERRLOG% == CON echo *Warning: Packing in this environment might fail.
if not %FPCERRLOG% == CON echo *You should press Ctrl-Break now if the current drive is different from that
if not %FPCERRLOG% == CON echo *of %FPCDIR%; otherwise press any key to continue.
pause>nul
cd %FPCSNAP%

:Pack
echo *Packing the snapshot ... >> %FPCERRLOG%
if %@EVAL[0] == 0 goto SHL2
goto Cmd2
:Shl2
pushd
cdd %FPCSNAP%
:Cmd2

rem ZIP.EXE must be on the PATH
zip -9 -r baseemx.zip bin\os2\ppc386.exe doc\* msg\* units\os2\rtl\*.ppo units\os2\rtl\*.oo2 units\os2\rtl\*.ao2 >> %FPCERRLOG%
if exist baseemx.zip goto ZipOK
echo *Error: The ZIP file hasn't been created!! >> %FPCERRLOG%
:ZipOK
if %@EVAL[0] == 0 popd

echo *Done. >> %FPCERRLOG%

goto End

:ErrorDir
echo *Error: Environment variable FPCDIR must point to your base FPC directory!!! >> %FPCERRLOG%
goto End


  $Log: os2snap.cmd,v $
  Revision 1.6  2001/06/28 21:09:43  peter
    * latest versions from 1.0.5 branch

  Revision 1.1.2.1  2001/05/15 18:41:14  carl
  * updated to work with FPC v1.0.x

  Revision 1.1  2000/07/14 10:09:29  michael
  + Moved from base

  Revision 1.1  2000/07/13 06:31:26  michael
  + Initial import

  Revision 1.18  2000/06/26 17:31:12  hajny
    * workaround for MS command shell limitation

  Revision 1.17  2000/05/21 16:09:42  hajny
    * os2def.pas added

  Revision 1.16  2000/05/14 16:46:09  hajny
    * cmd.exe compatibility problem fixed

  Revision 1.15  2000/04/03 17:42:46  hajny
    + LineInfo added

  Revision 1.14  2000/03/28 19:30:50  hajny
    * another change of order

  Revision 1.13  2000/03/16 19:43:36  hajny
    * fix for COMMAND.COM, order, etc.

  Revision 1.12  2000/03/12 18:29:40  hajny
    * wrong order corrected

  Revision 1.11  2000/03/12 13:42:00  hajny
    * cosmetic change for easier synchronization

  Revision 1.10  2000/03/12 13:37:24  hajny
    * support for calling PPAS script, compiler stack increased

  Revision 1.9  2000/03/05 19:13:25  hajny
    * new snapshot structure

  Revision 1.8  2000/01/29 16:24:01  hajny
    * logging enhanced, verbose support, error for non-4dos fixed

  Revision 1.7  2000/01/26 22:34:36  hajny
    * support for error logging added

  Revision 1.6  2000/01/16 18:44:21  hajny
    * got rid of PPC386.CFG dependency

  Revision 1.3  1999/10/01 09:00:21  hajny
    + PMGPI and DIVE added

  Revision 1.2  1999/09/15 07:31:49  hajny
    + some units added, OTHEROPTS variable support



:End
