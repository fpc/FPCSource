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

ifeq ($(OS_TARGET),$(OS_SRC))
CROSSCOMPILE=NO
else
CROSSCOMPILE=YES
endif 

# To copy pograms
ifndef COPY 
ifeq ($(DOS),YES)
COPY=copy
else
COPY=cp -f -p
endif
endif

# To delete programs
ifndef DEL 
ifeq ($(DOS),YES)
DEL=del
else
DEL=rm
endif
endif

# To install programs
ifndef INSTALL
ifeq ($(DOS),YES)
INSTALL=copy
else
INSTALL=install -m 644
endif
endif

# To make a directory.
ifndef MKDIR
ifeq ($(DOS),YES)
MKDIR=mkdir
else
MKDIR=install -m 755 -d
endif
endif


#diff program
ifndef REFPATH
REFPATH=/usr/local/fpk/work/new/rtl
endif
ifndef DIFF
DIFF=diff
endif
ifndef DIFFOPTS
DIFFOPTS=-b -c
endif

ifeq ($(CROSSCOMPILE),YES)
OPT:=$(OPT) -dCROSSCOMPILE -TOS2
endif

# Was a config file specified ?
ifdef CFGFILE
OPT:=$(OPT) @$(CFGFILE)
endif

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

sysos2$(PPUEXT) : sysos2.pas $(INC)/systemh.inc $(INC)/system.inc $(INC)/real2str.inc \
	$(INC)/mathh.inc $(PROCINC)/math.inc $(PROCINC)/set.inc $(INC)/innr.inc $(PROCINC)/heap.inc  \
	$(INC)/heaph.inc $(INC)/textrec.inc $(INC)/filerec.inc $(PROCINC)/$(CPU).inc
	$(COPY) $(INC)/systemh.inc .
	$(COPY) $(INC)/system.inc .
	$(COPY) $(PROCINC)/$(CPU).inc .
	$(COPY) $(INC)/real2str.inc .
	$(COPY) $(INC)/mathh.inc .
	$(COPY) $(PROCINC)/math.inc .
	$(COPY) $(PROCINC)/set.inc .
	$(COPY) $(INC)/innr.inc .
	$(COPY) $(PROCINC)/heap.inc .
	$(COPY) $(INC)/heaph.inc .
	$(COPY) $(INC)/textrec.inc .
	$(COPY) $(INC)/filerec.inc .
	$(PP) $(OPT) -Us sysos2.pas $(REDIR)
	$(DEL) systemh.inc
	$(DEL) system.inc
	$(DEL) real2str.inc
	$(DEL) mathh.inc
	$(DEL) math.inc
	$(DEL) set.inc
	$(DEL) innr.inc
	$(DEL) heap.inc
	$(DEL) heaph.inc
	$(DEL) $(CPU).inc
	$(DEL) textrec.inc
	$(DEL) filerec.inc

prt0$(OEXT) : prt0.as
	as -D -o prt0$(OEXT) prt0.as

prt1$(OEXT) : prt1.as
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
