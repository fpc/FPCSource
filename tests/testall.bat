@echo off
rem This batch script should compile all tests.
rem All tests which should run and be ok.
for %%f in ( ts*.bat ) do command /c %%f
rem All tests which crash the compiler.
for %%f in ( tf*.bat ) do command /c %%f




  