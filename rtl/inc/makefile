#****************************************************************************
#
#                   Copyright (c) 1993,96 by Florian Klaempfl
#
#****************************************************************************
#
# makefile for FPKPascal
#
#####################################################################
# Start of configurable section
#####################################################################

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

# Check copy delete commands.
# You need cp from GNU to handle / as directory separator
ifeq ($(DOS),YES)
COPY=cp -p -f
DEL=del
else
COPY=cp -p -f
DEL=rm
endif


PPFILES = complex.pp \
	cpne.pp

INCFILES = heaph.inc \
	innr.inc \
	mathh.inc \
	real2str.inc \
	system.inc  \
	systemh.inc \
        textrec.inc \
	filerec.inc


.PHONY: clean diffclean diffs

all:

clean:
	-$(DEL) *.dif

diffclean:
	-$(DEL) *.dif


%.dif : %.inc 
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/inc/$*.inc > $*.dif

%.dif : %.pp 
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/inc/$*.pp > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/inc/makefile > makefile.dif
	
diffs : $(patsubst %.inc,%.dif,$(INCFILES)) \
	$(patsubst %.pp,%.dif,$(PPFILES)) \
	makefile.dif


