@echo off
rem
rem Set database
rem 
set DATABASE=testdb.gdb
if not "$1"=="" goto NODB
set DATABASE=%1
:NODB
rem
rem  Choose one of the following:
rem 
set ISQL=isql
rem set ISQL=/opt/interbase/i586_V4.0G/bin/isql
#
# Don't edit after this.
#
echo Creating and filling table FPdev in database %DATABASE%...
echo CREATE DATABASE "%DATABASE%"; >mkdb.sql
type mkdb.sqd >>mkdb.sql
%ISQL% <mkdb.sql >nul
if not errorlevel goto ok
echo Failed.
goto end
:ok
  echo "Done."
:end
