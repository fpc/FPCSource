@echo off
rem
rem Batch file to compile and run ts010100
rem
echo Compiling ts010100...
ppc386 ts010100 >nul
if errorlevel 1 goto comfailed
echo compilation of ts010100 : PASSED
ts010100 >nul
if errorlevel 0 goto runpassed
echo execution of ts010100 : FAILED
goto end
:runpassed
echo execution of ts010100 : PASSED
goto end
:comfailed
echo Compilation of ts010100 : FAILED
:end
