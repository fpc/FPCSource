@echo off
if "%1"=="" goto go32v2test
if "%1"=="go32v2" goto go32v2test
if "%1"=="win32" goto win32test
echo This batch file allows to test all test sources of the entire directory
echo Use "win95test go32v2" to run the test with ppc386
echo Use "win95test win32" to run the test with ppwin32 (native win32 version)
echo or use "win95test" to run the test first with ppc386 and again with ppwin32
goto end
:go32v2test
set LONGLOG=1
ppc386 -l -iSO >longlog
make clean
make alltbf
make tbs0to99
make tbs100to199
make tbs200to299
make tbs300to399
make tbsexec0to99
make tbsexec100to199
make tbsexec200to299
make tbsexec300to399
make allts alltf allto 
make alltest
make allwebtbf allwebtbs
make allwebtbsexec
make alltsexec
make alltestexec
cp log go32v2.log
cp longlog go32v2.longlog
if "%1"=="go32v2" goto end
:win32test
set LONGLOG=1
set FPC=ppwin32
make clean
%FPC% -l -iSO > longlog
make alltbf
make tbs0to99
make tbs100to199
make tbs200to299
make tbs300to399
make tbsexec0to99
make tbsexec100to199
make tbsexec200to299
make tbsexec300to399
make allts alltf allto 
make alltest
make allwebtbf allwebtbs
make allwebtbsexec
make alltestexec
make alltsexec
cp log win32.log
cp longlog win32.longlog
:end
