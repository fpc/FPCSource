@echo off
REM comile and install fpc rtl for netware under winnt/win2k
REM make seems to have a problem unter nt/w2k
REM
SET DEST=\compiler\fpc\units\netware

REM Compile and install system unit first
SET INC=-I../inc -I../i386 -I../objpas ../inc/strings.pp
SET OPT=-di386 -XX -O3 -Sg -Tnetware

del /Q *.ppn
del /Q *.on
ppc386 %OPT% -dSYSTEMDEBUG  -FE. %INC%
copy *.ppn %DEST%
copy *.on %DEST%

REM copy the import files
copy nwimp\*.imp %DEST%

REM and build other stuff
#ppc386 -di386 -XX -O3 -Sg -Tnetware -FE. nwpre.pp
asw nwpre.as -o nwpre.on
asw prelude.as -o prelude.on

ppc386 %OPT% %INC% dos.pp
ppc386 %OPT% %INC% crt.pp
ppc386 %OPT% %INC% ../objpas/objpas.pp
ppc386 %OPT% %INC% sysutils.pp
ppc386 %OPT% %INC% keyboard.pp
ppc386 %OPT% %INC% mouse.pp
ppc386 %OPT% %INC% video.pp
ppc386 %OPT% %INC% sockets.pp
ppc386 %OPT% %INC% netware.pp
ppc386 %OPT% %INC% winsock2.pp
ppc386 %OPT% %INC% aio.pp

copy *.on %DEST%
copy *.ppn %DEST%