#****************************************************************************
#
#                   Copyright (c) 1993,96 by Florian Klaempfl
#
#****************************************************************************
#
# makefile for FPK Pascal DOS directory
#
#####################################################################
# Start of configurable section
#####################################################################

# set here the type of the ppc386 you use:
# Choose from: dos go32v2 linux or os2
OS_SRC=dos

# Set REFPATH if you want to generate diffs to a standard RTL
ifndef REFPATH
REFPATH=/usr/local/fpk/work/new/rtl
endif
ifndef DIFF
DIFF=diff
endif
ifndef DIFFOPTS
DIFFOPTS=-b -c
endif

#######################################################################
# End of configurable section.
# Do not edit after this line.
#######################################################################

# Check operating system.
ifeq ($(OS_SRC),linux)
DOS=NO
else
DOS=YES
endif

# Check copy delete commands.
# You need cp and rm from GNU to handle / as directory separator
ifeq ($(DOS),YES)
COPY=cp
DEL=rm
else
COPY=cp
DEL=rm
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

PPI=ppi

PPFILES = crt.pp \
        dos.pp \
        fmouse.pp \
        go32.pp \
        graph.pp \
        mouse.pp \
        objects.pp \
        printer.pp \
        watch.pp

PPIFILES = arc.ppi \
         colors.ppi \
         dpmi2raw.ppi \
         ellipse.ppi \
         fill.ppi \
         font.ppi \
         global.ppi \
         ibm.ppi \
         image.ppi \
         line.ppi \
         modes.ppi \
         move.ppi \
         palette.ppi \
         pixel.ppi \
         stdcolor.ppi \
         text.ppi \
         triangle.ppi \
         vesadeb.ppi 


# Here we have a conflict between modes.inc and modes.ppi
# that generate the same modes.dif !!
# so we use modes.dii for modes.inc

INCFILES = modes.inc

DIFFFILES = $(patsubst %.pp,%.dif,$(PPFILES)) \
	$(patsubst %.ppi,$(PPI)/%.dif,$(PPIFILES)) \
	$(patsubst %.inc,%.dii,$(INCFILES)) makefile.dif

.PHONY: clean diff diffclean

clean:
	-$(DEL) *.dif
	-$(DEL) *.dii

diffclean:
	-$(DEL) *.dif
	-$(DEL) *.dii
	-$(DEL) $(PPI)/*.dif


%.dii : %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/dos/$*.inc > $*.dii

%.dif : %.pp 
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/dos/$*.pp > $*.dif

%.dif : %.ppi
	-$(DIFF) $(DIFFOPTS) $*.ppi $(REFPATH)/dos/$*.ppi > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/dos/makefile > makefile.dif
	
diffs : $(DIFFFILES)
