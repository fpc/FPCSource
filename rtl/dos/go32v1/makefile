# Makefile for the DOS Go32v1 Run-time library.
# we need the stupid copies and del because the old FPK Pascal doesn't handle
# ppc386 ..\crt correct..
#
#####################################################################
# Start of configurable section.
# Please note that all these must be set in the main makefile, and
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

# set the directory where to install the units.
ifndef UNITINSTALLDIR
UNITINSTALLDIR=c:\lib\ppc
endif

# set the directory where to install libraries
ifndef LIBINSTALLDIR
LIBINSTALLDIR=c:\lib
endif

# What is the Operating System
ifndef OS_SRC
OS_SRC=GO32V1
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=GO32V1
endif

# What compiler to use ?
# I think ppc386 is better (it's mostly in path) (FK)
ifndef PP
PP=ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

# Where is the ppumove program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

# Where are the include files
INC=../../inc

ifndef CPU
CPU=i386
endif

PROCINC=../../$(CPU)

# Where are the .ppi files.
PPI=../ppi

ifeq ($(OS_TARGET),$(OS_SRC))
CROSSCOMPILE=NO
else
CROSSCOMPILE=YES
endif

# To copy pograms
ifndef COPY
COPY=cp -p
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
INSTALL=cp
else
INSTALL=install -m 644
endif
endif


ifndef MKDIR
ifeq ($(DOS),YES)
MKDIR=mkdir
else
MKDIR=install -m 755 -d
endif
endif

# Check for crosscompile
ifeq ($(CROSSCOMPILE),YES)
OPT:=$(OPT) -dCROSSCOMPILE -T$(OS_TARGET)
endif

# check config file
ifdef CFGFILE
OPT:=$(OPT) @$(CFGFILE)
endif

# to be sure to be able to compile with an older
# compiler version
OPT:=$(OPT) -dFPC

# diff program
ifndef REFPATH
REFPATH=/usr/local/fpk/work/new/rtl
endif
ifndef DIFF
DIFF=diff
endif
ifndef DIFFOPTS
DIFFOPTS=-b -c
endif

# os independent depends
SYSTEMDEPS=$(INC)/system.inc $(INC)/systemh.inc $(INC)/mathh.inc $(INC)/real2str.inc \
	$(INC)/heaph.inc $(INC)/innr.inc $(INC)/sstrings.inc $(INC)/file.inc \
	$(INC)/text.inc $(INC)/typefile.inc $(INC)/version.inc $(INC)/filerec.inc \
	$(INC)/textrec.inc $(INC)/objpas.inc $(INC)/objpash.inc \
	$(PROCINC)/math.inc $(PROCINC)/set.inc $(PROCINC)/heap.inc $(PROCINC)/$(CPU).inc

PPUEXT=.pp1
PPLEXT=.ppl
# At this moment only static libs under go32v2. When shared libs are made then
# we should ask what kind of lib user wants, and then set the correct
# extension... (see linux makefile for example)
LIBEXT=.a1
OEXT=.o1

.PHONY: all clean install diffs diffclean

all : system$(PPUEXT) prt0$(OEXT) crt$(PPUEXT) go32$(PPUEXT) strings$(PPUEXT) \
	dos$(PPUEXT) printer$(PPUEXT) objects$(PPUEXT) \
	mmx$(PPUEXT) cpu$(PPUEXT) \
	mouse$(PPUEXT) fmouse$(PPUEXT) getopts$(PPUEXT) graph$(PPUEXT)

printer$(PPUEXT) : ../printer.pp system$(PPUEXT)
	$(COPY) ../printer.pp .
	$(PP) $(OPT) printer.pp $(REDIR)
	$(DEL) printer.pp

getopts$(PPUEXT) : $(PROCINC)/getopts.pp system$(PPUEXT)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts.pp $(REDIR)
	$(DEL) getopts.pp

graph$(PPUEXT) : ../graph.pp go32$(PPUEXT) system$(PPUEXT) mmx$(PPUEXT) \
	$(PPI)/arc.ppi $(PPI)/colors.ppi $(PPI)/dpmi2raw.ppi $(PPI)/ellipse.ppi \
	$(PPI)/fill.ppi $(PPI)/font.ppi $(PPI)/global.ppi $(PPI)/ibm.ppi \
	$(PPI)/image.ppi $(PPI)/line.ppi $(PPI)/modes.ppi $(PPI)/move.ppi \
	$(PPI)/palette.ppi $(PPI)/pixel.ppi $(PPI)/stdcolor.ppi $(PPI)/text.ppi \
	$(PPI)/triangle.ppi $(PPI)/vesadeb.ppi
	$(COPY) ../graph.pp $(PPI)/*.ppi .
	$(PP) $(OPT) graph $(REDIR)
	$(DEL) graph.pp *.ppi

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) system$(PPUEXT)
	$(COPY) $(PROCINC)/mmx.pp .
	$(PP) $(OPT) mmx.pp $(REDIR)
	$(DEL) mmx.pp

cpu$(PPUEXT) : ../../i386/cpu.pp system$(PPUEXT)
	$(COPY) ../../i386/cpu.pp .
	$(PP) $(OPT) cpu $(REDIR)
	$(DEL) cpu.pp

strings$(PPUEXT) : $(PROCINC)/strings.pp system$(PPUEXT)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings.pp $(REDIR)
	$(DEL) strings.pp

dos$(PPUEXT) : ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	go32$(PPUEXT) system$(PPUEXT) strings$(PPUEXT)
	$(COPY) ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) dos.pp filerec.inc textrec.inc

system$(PPUEXT) : system.pp $(SYSTEMDEPS)
	$(COPY) $(INC)/*.inc $(PROCINC)/*.inc .
	$(PP) $(OPT) -Us -Sg system $(REDIR)
	$(DEL) systemh.inc system.inc real2str.inc version.inc $(CPU).inc sstrings.inc
	$(DEL) mathh.inc math.inc set.inc innr.inc heap.inc heaph.inc objpash.inc
	$(DEL) filerec.inc textrec.inc file.inc typefile.inc text.inc objpas.inc

prt0$(OEXT) : prt0.as
	as -D -o prt0$(OEXT) prt0.as

crt$(PPUEXT) : ../crt.pp $(INC)/textrec.inc go32$(PPUEXT) system$(PPUEXT)
	$(COPY) ../crt.pp $(INC)/textrec.inc .
	$(PP) $(OPT) crt $(REDIR)
	$(DEL) crt.pp textrec.inc

go32$(PPUEXT) : ../go32.pp system$(PPUEXT)
	$(COPY) ../go32.pp .
	$(PP) $(OPT) go32.pp $(REDIR)
	$(DEL) go32.pp

objects$(PPUEXT) : ../objects.pp system$(PPUEXT)
	$(COPY) ../objects.pp .
	$(PP) $(OPT) objects.pp $(REDIR)
	$(DEL) objects.pp

mouse$(PPUEXT) : ../mouse.pp system$(PPUEXT)
	$(COPY) ../mouse.pp .
	$(PP) $(OPT) mouse.pp $(REDIR)
	$(DEL) mouse.pp

fmouse$(PPUEXT) : ../fmouse.pp system$(PPUEXT)
	$(COPY) ../fmouse.pp .
	$(PP) $(OPT) fmouse.pp $(REDIR)
	$(DEL) fmouse.pp

clean:
	-$(DEL) *$(OEXT)
	-$(DEL) *$(PPUEXT)
	-$(DEL) *.dif
	-$(DEL) log
	-$(DEL) *$(ASMEXT)

diffclean:
	-$(DEL) *.dif

install: all
	-$(MKDIR) $(LIBINSTALLDIR)/dosunits
	$(INSTALL) *$(OEXT) $(LIBINSTALLDIR)/dosunits
	$(INSTALL) *$(PPUEXT) $(LIBINSTALLDIR)/dosunits

%.dif : %.pp
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/dos/go32v1/$*.pp > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/dos/go32v1/$*.inc > $*.dif

%.dif : %.as
	-$(DIFF) $(DIFFOPTS) $*.as $(REFPATH)/dos/go32v1/$*.as > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/dos/go32v1/makefile > makefile.dif

diffs: system.dif os.dif makefile.dif prt0.dif
