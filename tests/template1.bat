@echo off
rem
rem Batch file to compile and run NAME
rem
echo Compiling NAME...
ppc386 NAME >nul
if errorlevel 1 goto comfailed
echo compilation of NAME : PASSED
NAME >nul
if errorlevel 0 goto runpassed
echo execution of NAME : FAILED
goto end
:runpassed
echo execution of NAME : PASSED
goto end
:comfailed
echo Compilation of NAME : FAILED
:end
