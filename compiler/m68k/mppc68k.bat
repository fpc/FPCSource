del ..\*.ppu
del *.ppu
ppc386 ..\pp.pas -dm68k -dnora68kmot -dnoag68kmit -dnoag68kmpw -dnoag68kmot -Sg
move pp.exe ppc68k.exe
