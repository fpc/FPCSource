# makes the SYSTEM-Unit for OS2
#
# Copyright (c) 1996 by Michael Van Canneyt

#####################################################################
# Start of configurable section. 
# Please note that all these must be set in the main makefile, and 
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

# set the directory where to install the units.
ifndef LIBINSTALLDIR
LIBINSTALLDIR=c:/pp/bin
endif 

# What is the Operating System
ifndef OS_SRC
OS_SRC=os2
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=os2
endif

# What compiler to use ?
ifndef PP
PP=../../ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

ifndef CPU
CPU=i386
endif 

#####################################################################
# End of configurable section. 
# Do not edit after this line.
#####################################################################

# Where are the include files
INC=../inc
PROCINC=../$(CPU)
CFG=../cfg

# Get some defaults for Programs and OSes.
# This will set the following variables :
# inlinux indos COPY REPLACE DEL INSTALL INSTALLEXE MKDIR
# It will also set OPT for cross-compilation, and add required options.
# also checks for config file.
# it expects INC PROCINC to be set !!
include $(CFG)/makefile.cfg

# Get the system independent include file names.
# This will set the following variables : 
# SYSINCNAMES
include $(INC)/makefile.inc
SYSINCDEPS=$(addprefix $(INC)/,$(SYSINCNAMES))

# Get the processor dependent include file names.
# This will set the following variables : 
# CPUINCNAMES
include $(PROCINC)/makefile.cpu
SYSCPUDEPS=$(addprefix $(PROCINC)/,$(CPUINCNAMES))

# Put system unit dependencies together.
SYSDEPS=$(SYSINCDEPS) $(SYSCPUDEPS)

#####################################################################
# System dependent
#####################################################################


PPUEXT = .ppu

OEXT = .obj


.PHONY: all clean install diffs diffclean

all : sysos2$(PPUEXT) prt0$(OEXT) prt1$(OEXT) dosinit$(OEXT) \
        strings$(PPUEXT) dos$(PPUEXT) getopts$(PPUEXT)


getopts$(PPUEXT) : $(PROCINC)/getopts.pp sysos2$(PPUEXT)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts.pp $(REDIR)
	$(DEL) getopts.pp

strings$(PPUEXT) : $(PROCINC)/strings.pp sysos2$(PPUEXT)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings.pp $(REDIR)
	$(DEL) strings.pp

dos$(PPUEXT) : dos.pas strings$(PPUEXT) sysos2$(PPUEXT)
	$(PP) $(OPT) dos.pas $(REDIR)

sysos2$(PPUEXT) : sysos2.pas $(SYSDEPS)
	$(PP) $(OPT) -Us sysos2.pas $(REDIR)

prt0$(OEXT) : prt0.so2
	as -D -o prt0$(OEXT) prt0.as

prt1$(OEXT) : prt1.so2
	as -D -o prt1$(OEXT) prt1.as

dosinit$(OEXT) : dosinit.as
	as -D -o dosinit$(OEXT) dosinit.as

clean:
	-$(DEL) *$(OEXT)
	-$(DEL) *$(PPUEXT)
	-$(DEL) *.dif
	-$(DEL) *.s
	-$(DEL) log

diffclean:
	-$(DEL) *.dif

install: all
	$(MKDIR) $(LIBINSTALLDIR)/os2units
	$(INSTALL) *$(OEXT) *$(PPUEXT) $(LIBINSTALLDIR)/os2units

%.dif : %.pas
	-$(DIFF) $(DIFOPTS) $*.pas $(REFPATH)/os2/$*.pas > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFOPTS) $*.inc $(REFPATH)/os2/$*.inc > $*.dif

%.dif : %.as
	-$(DIFF) $(DIFOPTS) $*.as $(REFPATH)/os2/$*.as > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/os2/makefile > makefile.dif
	

diffs: sysos2.dif dos.dif doscalls.dif os.dif prt0.dif prt1.dif dosinit.dif \
       makefile.dif
