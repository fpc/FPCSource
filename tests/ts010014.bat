@echo off
rem
rem Batch file to compile and run ts010014
rem
echo Compiling ts010014...
ppc386 ts010014 >nul
if errorlevel 1 goto comfailed
echo compilation of ts010014 : PASSED
ts010014 >nul
if errorlevel 0 goto runpassed
echo execution of ts010014 : FAILED
goto end
:runpassed
echo execution of ts010014 : PASSED
goto end
:comfailed
echo Compilation of ts010014 : FAILED
:end
