# Makefile for the DOS Go32v2 Run-time library.
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
OS_SRC=GO32V2
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=GO32V2
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
REFPATH=h:/cvs/rtl
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

PPUEXT=.ppu
PPLEXT=.ppl
# At this moment only static libs under go32v2. When shared libs are made then
# we should ask what kind of lib user wants, and then set the correct
# extension... (see linux makefile for example)
LIBEXT=.a
OEXT=.o

.PHONY: all clean install diffs diffclean

all : system$(PPUEXT) prt0$(OEXT) crt$(PPUEXT) go32$(PPUEXT) strings$(PPUEXT) \
	dos$(PPUEXT) printer$(PPUEXT) objects$(PPUEXT) \
	mouse$(PPUEXT) fmouse$(PPUEXT) getopts$(PPUEXT) graph$(PPUEXT) \
	dpmiexcp$(PPUEXT) exceptn$(OEXT) profile$(PPUEXT) \
	dxeload$(PPUEXT) fpu$(OEXT) emu387$(PPUEXT) mmx$(PPUEXT) cpu$(PPUEXT) \
        objpas$(PPUEXT)

dxeload$(PPUEXT) : system$(PPUEXT)
	$(PP) $(OPT) dxeload $(REDIR)

emu387$(PPUEXT) : system$(PPUEXT) fpu$(OEXT) dxeload$(PPUEXT) dpmiexcp$(PPUEXT)
	$(PP) $(OPT) emu387 $(REDIR)

fpu$(OEXT) : fpu.as
	as -o fpu$(OEXT) fpu.as

printer$(PPUEXT) : ../printer.pp system$(PPUEXT)
	$(COPY) ../printer.pp .
	$(PP) $(OPT) printer $(REDIR)
	$(DEL) printer.pp

getopts$(PPUEXT) : $(PROCINC)/getopts.pp system$(PPUEXT)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts $(REDIR)
	$(DEL) getopts.pp

graph$(PPUEXT) : ../graph.pp mmx$(PPUEXT) go32$(PPUEXT) system$(PPUEXT) \
	$(PPI)/arc.ppi $(PPI)/colors.ppi $(PPI)/dpmi2raw.ppi $(PPI)/ellipse.ppi \
	$(PPI)/fill.ppi $(PPI)/font.ppi $(PPI)/global.ppi $(PPI)/ibm.ppi \
	$(PPI)/image.ppi $(PPI)/line.ppi $(PPI)/modes.ppi $(PPI)/move.ppi \
	$(PPI)/palette.ppi $(PPI)/pixel.ppi $(PPI)/stdcolor.ppi $(PPI)/text.ppi \
	$(PPI)/triangle.ppi $(PPI)/vesadeb.ppi
	$(COPY) ../graph.pp $(PPI)/*.ppi .
	$(PP) $(OPT) graph $(REDIR)
	$(DEL) graph.pp *.ppi

strings$(PPUEXT) : $(PROCINC)/strings.pp system$(PPUEXT)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

dos$(PPUEXT) : ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	go32$(PPUEXT) system$(PPUEXT) strings$(PPUEXT)
	$(COPY) ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) dos.pp filerec.inc textrec.inc

exceptn$(OEXT) : exceptn.as
	as -o exceptn$(OEXT) exceptn.as
#   gcc not need anymore
#	gcc -c -x assembler-with-cpp -o exceptn$(OEXT) exceptn.AS

dpmiexcp$(PPUEXT) : dpmiexcp.pp exceptn$(OEXT)
	$(PP) $(OPT) -Sg dpmiexcp $(REDIR)

profile$(PPUEXT) : profile.pp dpmiexcp$(PPUEXT) go32$(PPUEXT)
	$(PP) $(OPT) profile $(REDIR)

system$(PPUEXT) : system.pp $(SYSTEMDEPS)
	$(COPY) $(INC)/*.inc $(PROCINC)/*.inc .
	$(PP) $(OPT) -dI386 -Us -Sg system $(REDIR)
	$(DEL) systemh.inc system.inc real2str.inc version.inc $(CPU).inc sstrings.inc
	$(DEL) mathh.inc math.inc set.inc innr.inc heap.inc heaph.inc objpash.inc
	$(DEL) filerec.inc textrec.inc file.inc typefile.inc text.inc objpas.inc

prt0$(OEXT) : v2prt0.as
	as -o prt0$(OEXT) v2prt0.as
#   gcc not need anymore
#	gcc -c -x assembler-with-cpp -o prt0$(OEXT) v2prt0.AS

crt$(PPUEXT) : ../crt.pp $(INC)/textrec.inc go32$(PPUEXT) system$(PPUEXT)
	$(COPY) ../crt.pp $(INC)/textrec.inc .
	$(PP) $(OPT) crt $(REDIR)
	$(DEL) crt.pp textrec.inc

go32$(PPUEXT) : ../go32.pp system$(PPUEXT)
	$(COPY) ../go32.pp .
	$(PP) $(OPT) go32 $(REDIR)
	$(DEL) go32.pp

mmx$(PPUEXT) : ../../i386/mmx.pp cpu$(PPUEXT) system$(PPUEXT)
	$(COPY) ../../i386/mmx.pp .
	$(PP) $(OPT) mmx $(REDIR)
	$(DEL) mmx.pp

cpu$(PPUEXT) : ../../i386/cpu.pp system$(PPUEXT)
	$(COPY) ../../i386/cpu.pp .
	$(PP) $(OPT) cpu $(REDIR)
	$(DEL) cpu.pp

objpas$(PPUEXT) : ../../objpas/objpas.pp system$(PPUEXT)
	$(COPY) ../../objpas/objpas.pp .
	$(PP) $(OPT) objpas $(REDIR)
	$(DEL) objpas.pp

objects$(PPUEXT) : ../objects.pp system$(PPUEXT)
	$(COPY) ../objects.pp .
	$(PP) $(OPT) objects $(REDIR)
	$(DEL) objects.pp

mouse$(PPUEXT) : ../mouse.pp system$(PPUEXT)
	$(COPY) ../mouse.pp .
	$(PP) $(OPT) mouse $(REDIR)
	$(DEL) mouse.pp

fmouse$(PPUEXT) : ../fmouse.pp system$(PPUEXT)
	$(COPY) ../fmouse.pp .
	$(PP) $(OPT) fmouse $(REDIR)
	$(DEL) fmouse.pp

libs: libfpc$(LIBEXT)

libfpc.a: all
	$(PPUMOVE) -s -o fpc *.ppu

clean:
	-$(DEL) *$(OEXT) *$(PPUEXT) *.dif log *.s

diffclean:
	-$(DEL) *.dif

install: all
	-$(MKDIR) $(UNITINSTALLDIR)/go32unit
	$(INSTALL) *$(PPUEXT) $(UNITINSTALLDIR)/go32unit
	$(INSTALL) *$(OEXT)   $(UNITINSTALLDIR)/go32unit

libinstall: libs
	$(INSTALL) libfpc$(LIBEXT) $(LIBINSTALLDIR)
	$(INSTALL) *$(PPLEXT) $(UNITINSTALLDIR)

%.dif : %.pp
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/dos/go32v2/$*.pp > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/dos/go32v2/$*.inc > $*.dif

# these must have the lowest priority

%.dif : %.as
	-$(DIFF) $(DIFFOPTS) $*.as $(REFPATH)/dos/go32v2/$*.as > $*.dif

%.dif : %.asm
	-$(DIFF) $(DIFFOPTS) $*.asm $(REFPATH)/dos/go32v2/$*.asm > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/dos/go32v2/makefile > makefile.dif

diffs: system.dif v2prt0.dif dpmiexcp.dif exceptn.dif profile.dif os.dif \
       sbrk16.dif exit16.dif makefile.dif
