@echo off
rem msdos batch file
rem Simply paste a header and footer to the program.
type head.tex >  %1.tex
type %1.pp    >> %1.tex
type foot.tex >> %1.tex
