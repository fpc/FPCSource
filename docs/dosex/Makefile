#######################################################################
#
# Makefile to compile all examples and convert them to LaTeX
# 
#######################################################################

# Compiler
PP=ppc386

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

.SUFFIXES: .pp .tex .log .exe

.PHONY: all tex clean execute

OBJECTS=ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 \
#        ex15 ex16

TEXOBJECTS=$(addsuffix .tex, $(OBJECTS))

LOGFILE=$(addsuffix .log, $(OBJECTS))


execute: $(LOGFILE)

all : $(OBJECTS)

tex : $(TEXOBJECTS)

onetex : tex
	$(MAKETEX) $(TEXOBJECTS)

clean : 
	rm -f *.o *.s $(OBJECTS) $(TEXOBJECTS)
	rm -f *.ow *.sw *.exe *.dll *.log *.tmp
	
	
$(OBJECTS): %: %.pp
	$(PP) $(PPOPTS) $*
	
$(LOGFILE): %.log: %.exe 
	$* > $*.log
	

$(TEXOBJECTS): %.tex: %.pp head.tex foot.tex
	$(PP2TEX) $*
