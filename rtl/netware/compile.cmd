@echo off
REM comile and install fpc rtl for netware under winnt/win2k
REM make seems to have a problem unter nt/w2k
REM
SET DEST=\compiler\fpc\units\netware

REM Compile and install system unit first
del /Q *.ppn
del /Q *.on
ppc386 -di386 -XX -dSYSTEMDEBUG -O3 -Sg -Tnetware -FE. -I../inc -I../i386 -I../objpas ../inc/strings.pp
copy *.ppn %DEST%
copy *.on %DEST%

REM copy the import files
copy nwimp\*.imp %DEST%

REM and build other stuff
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. nwpre.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 dos.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 crt.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 -I../objpas ../objpas/objpas.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 -I../objpas sysutils.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 keyboard.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 mouse.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 video.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 sockets.pp
ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. -I../inc -I../i386 netware.pp

copy *.on %DEST%
copy *.ppn %DEST%