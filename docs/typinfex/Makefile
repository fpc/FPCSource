#######################################################################
#
# Makefile to compile all examples and convert them to LaTeX
# 
#######################################################################

# Compiler
ifndef FPC
ifdef PP
FPC=$(PP)
endif
endif
ifndef FPC
FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH))))
ifneq ($(FPCPROG),)
FPCPROG:=$(firstword $(FPCPROG))
FPC:=$(shell $(FPCPROG) -PB)
ifneq ($(findstring Error,$(FPC)),)
override FPC=ppc386
endif
else
override FPC=ppc386
endif
endif
override FPC:=$(subst $(SRCEXEEXT),,$(FPC))
override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)
# Unit directory
# UNITDIR=/usr/lib/ppc/0.99.0/linuxunits


# Any options you wish to pass.
PPOPTS=

# Script to convert the programs to LaTeX examples which can be included.
PP2TEX=../pp2tex

# Script to collect all examples in 1 file.
MAKETEX=make1tex

#######################################################################
# No need to edit after this line.
#######################################################################

ifdef UNITDIR
PPOPTS:=$(PPOPTS) -Up$(UNITDIR);
endif

.SUFFIXES: .pp .tex

.PHONY: all tex clean execute

OBJECTS=rttiobj trtti1 trtti2 trtti3 ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 \
        ex10 ex11 ex12 ex13  ex14 ex15 ex16 ex17 ex18
        
# This might not be the same list as objects, since some of the
# tests might be interactive.
TOTEST=trtti1 trtti2 trtti3 ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 \
        ex10 ex11 ex12 ex13  ex14 ex15 ex16 ex17 ex18
        
LOGFILE=$(addsuffix .log, $(TOTEST))
        

TEXOBJECTS=$(addsuffix .tex, $(OBJECTS))

all : $(OBJECTS)

execute: $(LOGFILE)


tex : $(TEXOBJECTS)

onetex : tex
	$(MAKETEX) $(TEXOBJECTS)

clean : 
	rm -f *.o *.s $(OBJECTS) $(TEXOBJECTS) rttiobj.ppu
	rm -f *.ow *.sw *.exe *.dll *.tmp *.log rttiobj.ppw
 
$(OBJECTS): %: %.pp
	$(FPC) $(PPOPTS) $*
	
$(TEXOBJECTS): %.tex: %.pp head.tex foot.tex
	$(PP2TEX) $*

$(LOGFILE): %.log: %.exe 
	$* > $*.log
