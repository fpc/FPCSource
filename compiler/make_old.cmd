@echo off
rem $id: make.cmd,v $
rem
rem ************************Make batchfile for OS/2****************************
rem * Purpose:      The makefile contains a lot of Unix commands. While it is *
rem *               possible to install for example a bash shell under OS/2   *
rem *               a batch file is much easier.                              *
rem *                                                                         *
rem * Copyright (c) 1998-2000 by Daniel Mantione, developer of Free Pascal         *
rem ***************************************************************************

goto %1

:clean
pushd
\pp\rtl\
del /s *.ppo *.so2 *.oo2 *.ppu *.s *.o *.pp1 *.s1 *.o1>&dev\nul
popd
del *.ppo *.so2 *.oo2 *.ppu *.s *.o *.pp1 *.s1 *.o1>&dev\nul
goto eind

:prtx
pushd
\pp\rtl\os2\
iff not exist prt0.oo2 then
  as -o prt0.oo2 prt0.as
endiff
iff not exist prt1.oo2 then
  as -o prt1.oo2 prt1.as
endiff
iff not exist code2.oo2 then
  as -o code2.oo2 code2.as
endiff
iff not exist code3.oo2 then
  as -o code3.oo2 code3.as
endiff
popd
goto eind

:compiler
call make prtx
iff "%2"=="" then
    ppos2 pp.pas
else
    %2 pp.pas
endiff
goto eind

:remake
call make clean
call make compiler %2
goto eind

:cycle
call make remake %2
move pp.exe pp1.exe
call make remake pp1.exe
move pp.exe pp2.exe
call make remake pp2.exe
move pp.exe pp3.exe
goto eind

$log: make.cmd,v$

:eind
