@echo off
rem
rem Batch file to compile NAME. If compilation fails, the test passed.
rem
echo Compiling NAME...
ppc386 NAME >nul
if errorlevel 1 goto compassed
echo Error compilation of NAME : FAILED
goto end
:compassed
echo Error compilation of NAME : PASSED
:end



