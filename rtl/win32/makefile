# makes the SYSTEM-Unit for Win32
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
OS_SRC=DOS
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=WIN32
endif

# What compiler to use ?
ifndef PP
PP=ppc386
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
OPT:=$(OPT) -dCROSSCOMPILE -TWIN32
endif

# Was a config file specified ?
ifdef CFGFILE
OPT:=$(OPT) @$(CFGFILE)
endif

# os independent depends
SYSTEMDEPS=$(INC)/system.inc $(INC)/systemh.inc $(INC)/mathh.inc $(INC)/real2str.inc \
	$(INC)/heaph.inc $(INC)/innr.inc $(INC)/sstrings.inc $(INC)/file.inc \
	$(INC)/text.inc $(INC)/typefile.inc $(INC)/version.inc $(INC)/filerec.inc \
	$(INC)/textrec.inc $(INC)/objpas.inc $(INC)/objpash.inc \
	$(PROCINC)/math.inc $(PROCINC)/set.inc $(PROCINC)/heap.inc $(PROCINC)/$(CPU).inc

PPUEXT = .ppu

OEXT = .obj


.PHONY: all clean install diffs diffclean

all : syswin32$(PPUEXT) strings$(PPUEXT) getopts$(PPUEXT) \
	base$(PPUEXT)


getopts$(PPUEXT) : $(PROCINC)/getopts.pp syswin32$(PPUEXT)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts.pp $(REDIR)
	$(DEL) getopts.pp

strings$(PPUEXT) : $(PROCINC)/strings.pp syswin32$(PPUEXT)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings.pp $(REDIR)
	$(DEL) strings.pp

dos$(PPUEXT) : dos.pp strings$(PPUEXT) syswin32$(PPUEXT)
	$(PP) $(OPT) dos.pp $(REDIR)

base$(PPUEXT) : base.pp syswin32$(PPUEXT)
	$(PP) $(OPT) base.pp $(REDIR)

syswin32$(PPUEXT) : syswin32.pp $(SYSTEMDEP)
	$(COPY) $(INC)/*.inc $(PROCINC)/*.inc .
	$(PP) $(OPT) -Us -Sg syswin32 $(REDIR)
	$(DEL) systemh.inc system.inc real2str.inc version.inc $(CPU).inc sstrings.inc
	$(DEL) mathh.inc math.inc set.inc innr.inc heap.inc heaph.inc objpash.inc
	$(DEL) filerec.inc textrec.inc file.inc typefile.inc text.inc objpas.inc

clean:
	-$(DEL) *$(OEXT)
	-$(DEL) *$(PPUEXT)
	-$(DEL) *.dif
	-$(DEL) *.s
	-$(DEL) log

diffclean:
	-$(DEL) *.dif

install: all
	$(MKDIR) $(LIBINSTALLDIR)/units
	$(INSTALL) *$(OEXT) *$(PPUEXT) $(LIBINSTALLDIR)/units

%.dif : %.pp
	-$(DIFF) $(DIFOPTS) $*.pp $(REFPATH)/win32/$*.pp > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFOPTS) $*.inc $(REFPATH)/win32/$*.inc > $*.dif

%.dif : %.as
	-$(DIFF) $(DIFOPTS) $*.as $(REFPATH)/win32/$*.as > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/os2/makefile > makefile.dif


diffs: syswin32.dif dos.dif os.dif
