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

# Script to convert the C programs to LaTeX examples which can be included.
C2TEX=./c2tex

# Script to collect all examples in 1 file.
MAKETEX=make1tex

#######################################################################
# No need to edit after this line.
#######################################################################

ifdef UNITDIR
PPOPTS:=$(PPOPTS) -Up$(UNITDIR);
endif

.SUFFIXES: .pp .tex

.PHONY: all tex clean

OBJECTS=plsubs psubs 
COBJECTS=ctest ctest2
LIBOBJECT=libsubs.so
TEXOBJECTS=$(addsuffix .tex, $(OBJECTS))
TEXOBJECTS:=$(TEXOBJECTS) subs.tex
CTEXOBJECTS=$(addsuffix .tex, $(COBJECTS))

all : $(LIBOBJECTS) $(OBJECTS) $(COBJECTS)

tex : $(TEXOBJECTS) $(CTEXOBJECTS) subs.tex

onetex : tex
	$(MAKETEX) $(TEXOBJECTS)

clean : 
	rm -f *.o *.s *.so $(OBJECTS) $(COBJECTS) $(TEXOBJECTS) $(CTEXOBJECTS)
	rm -f *.ow *.sw *.exe *.dll
 
$(OBJECTS): %: %.pp $(LIBOBJECT)
	$(FPC) $(PPOPTS) $*

$(TEXOBJECTS): %.tex: %.pp head.tex foot.tex
	$(PP2TEX) $*

$(CTEXOBJECTS): %.tex: %.c head.tex foot.tex
	$(C2TEX) $*

libsubs.so: subs.pp
	$(FPC) $(PPOPTS) $<

ctest: ctest.c libsubs.so
	$(CC) -o ctest ctest.c -lsubs

ctest2: ctest2.c libsubs.so
	$(CC) -o ctest2 ctest2.c -ldl
