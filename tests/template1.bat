@echo off
rem
rem Batch file to compile and run NAME
rem
echo Compiling NAME...
ppc386 NAME >nul
if errorlevel 1 goto comfailed
echo compilation of NAME : PASSED
name >nul
if errorlevel 1 goto runfailed
echo execution of NAME : PASSED
goto end
:runfailed
echo execution of NAME : FAILED
:comfailed
echo Compilation of NAME : FAILED
:end



