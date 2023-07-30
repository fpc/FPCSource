PACKAGE_NAME=fpcm
include ../build/Makefile.pkg
ifndef inCygWin
$(BS_UNITDIR):
	$(MKDIRTREE) $(BS_UNITDIR)
bootstrap: $(BS_UNITDIR) fpcmake.inc
	$(FPCFPMAKE) fpcmake.pp $(FPMAKE_SKIP_CONFIG) -Fu$(UNITDIR_FPMAKE_RTL) -FU$(BS_UNITDIR) $(OPT)
endif

regeninc$(EXEEXT): regeninc.pp
ifdef INSTALLEDCOMPILER
	$(FPC) $<
else	
	$(COMPILER) $<
endif

regenerateinc: regeninc$(EXEEXT)
ifdef inUnix
	./regeninc
else
	regeninc$(EXEEXT)
endif	

	
