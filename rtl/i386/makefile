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


PPFILES = getopts.pp \
	strings.pp

INCFILES = heap.inc \
	i386.inc \
	math.inc \
	set.inc 

all:

.PHONY: clean diffclean diffs

clean:
	-$(DEL) *.dif

diffclean:
	-$(DEL) *.dif


%.dif : %.inc 
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/i386/$*.inc > $*.dif

%.dif : %.pp 
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/i386/$*.pp > $*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/i386/makefile > makefile.dif
	
diffs : $(patsubst %.inc,%.dif,$(INCFILES)) \
	$(patsubst %.pp,%.dif,$(PPFILES)) \
	makefile.dif
