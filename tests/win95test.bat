@echo off
set CONT=
set FPC=
rem if arg1 or arg2 is cont then
rem do not erase old files
rem  Create DATE env
rem  to be able to save a version with the current date
make setdate
call setdate.bat
if "%1"=="cont" goto setcont
if "%2"=="cont" goto setcont
if "%1"=="diffs" goto setdiffs
if "%2"=="diffs" goto setdiffs
set CONT=
goto nocont
:setcont
set CONT=1
if "%1"=="cont" goto doshift
goto nocont
:doshift
shift
:nocont
if "%1"=="" goto go32v2test
if "%1"=="go32v2" goto go32v2test
if "%1"=="win32" goto win32test
echo This batch file allows to test all test sources of the entire directory
echo Use "win95test go32v2" to run the test with ppc386
echo Use "win95test win32" to run the test with ppwin32 (native win32 version)
echo or use "win95test" to run the test first with ppc386 and again with ppwin32
echo "cont" arg can be used to continue a test suite
echo "diffs" can be used to generate diffs to last report
goto end
:go32v2test
set FPC=ppc386
set LONGLOG=go32v2.longlog
if "%CONT%"=="1" goto go32v2cont
echo Test of FPC for > %LONGLOG%
%FPC% -l -iSO >> %LONGLOG%
echo Test of FPC for > log
%FPC% -l -iSO >> log
Echo Stderr output of Make > Make.err
make clean
if not exist units\makefile goto go32v2cont
make -C units OS_TARGET=go32v2
:go32v2cont
set CONT=
set FPC=ppc386
set OPT=-n -Fuunits -gl -Croi
make alltbf
make alltbf
make tbs0to99
make tbs0to99
make tbs100to199
make tbs100to199
make tbs200to299
make tbs200to299
make tbs300to399
make tbs300to399
make allts alltf allto
make allts alltf allto
make alltest
make alltest
make allwebtbf allwebtbs
make allwebtbf allwebtbs
make tbsexec0to99
make tbsexec0to99
make tbsexec100to199
make tbsexec100to199
make tbsexec200to299
make tbsexec200to299
make tbsexec300to399
make tbsexec300to399
make allwebtbsexec
make allwebtbsexec
make alltsexec
make alltsexec
make alltestexec
make alltestexec

cp log go32v2.log
cp go32v2.log go32v2.%DATE%.log
cp go32v2.longlog go32v2.%DATE%.longlog
cp make.err go32v2.%DATE%.make.err
echo Go32v2 fail list of %DATE% > go32v2.%DATE%.fail
cat faillist >> go32v2.%DATE%.fail
:setdiffs
if "%1=="win32" goto setwin32diffs
if exist go32v2.lastfail goto go32diff
goto go32end
:go32diff
if not "%LASTDATE%"=="" goto go32lastset
call getlastgo32v2date.bat
:go32lastset
echo Go32v2 diffs from %LASTDATE% to %DATE% > go32v2.diff_to_last
diff -u faillist go32v2.lastfail >> go32v2.diff_to_last
cp go32v2.diff_to_last go32v2.%DATE%.diff

:go32end
echo set LASTDATE=%DATE% > getlastgo32v2date.bat
cp faillist go32v2.lastfail

rem should we pass the win32 test ?

if "%1"=="go32v2" goto end

rem Start of win32 part of test

:win32test
set FPC=ppwin32
set LONGLOG=win32.longlog
if "%CONT%"=="1" goto win32cont
Echo Stderr output of Make > Make.err
make clean
if not exist units\makefile goto win32nounits
make -C units OS_TARGET=win32
:win32nounits
echo Test of FPC for > %LONGLOG%
%FPC% -l -iSO >> %LONGLOG%
echo Test of FPC for > log
%FPC% -l -iSO >> log
:win32cont
set CONT=
set FPC=ppwin32
set OPT=-n -Fuunits -gl -Croi
make alltbf
make alltbf
make tbs0to99
make tbs0to99
make tbs100to199
make tbs100to199
make tbs200to299
make tbs200to299
make tbs300to399
make tbs300to399
make allts alltf allto
make allts alltf allto
make alltest
make alltest
make allwebtbf allwebtbs
make allwebtbf allwebtbs
make tbsexec0to99
make tbsexec0to99
make tbsexec100to199
make tbsexec100to199
make tbsexec200to299
make tbsexec200to299
make tbsexec300to399
make tbsexec300to399
make allwebtbsexec
make allwebtbsexec
make alltsexec
make alltsexec
make alltestexec
make alltestexec
cp log win32.log
cp win32.log win32.%DATE%.log
cp win32.longlog win32.%DATE%.longlog
cp faillist win32.%DATE%.fail
cp make.err win32.%DATE%.make.err

:setwin32diffs
if exist win32.lastfail goto win32diff
goto win32end
:win32diff
if not "%LASTDATE%"=="" goto win32lastset
call getlastwin32date.bat
:win32lastset
echo Win32 diffs from %LASTDATE% to %DATE% > win32.diff_to_last
diff -u faillist win32.lastfail >> win32.diff_to_last
cp win32.diff_to_last win2.%DATE%.diff

:win32end
echo set LASTDATE=%DATE% > getlastwin32date.bat
cp faillist win32.lastfail
:end
