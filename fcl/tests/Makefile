#######################################################################
#   Makefile for Free Pascal
#   (C) 1998 Michael van Canneyt
#######################################################################
#
#  Configurable section
#

# What Compiler should we use ?
PP=../../compiler/ppc386

# Where are the Free Pascal units ? (Optional)
UNITDIR = ../linux\;../../rtl/linux


# Any options you wish to pass to the compiler
OPT=

# Where to install the units ?
UNITINSTALLDIR=/usr/lib/fpc/0.99.5/linuxunits

# Where to install the programs ?
BININSTALLDIR=/usr/local/bin

#######################################################################
# End of configurable section. Do not edit below this line.
#######################################################################

.SUFFIXES: .pp .ppu .pas
.PHONY: all install clean units progs

# If nothing special needs doing, then just fill in the names here.
# The rest should be automatic.
#UNITNAMES=
PROGNAMES=fstream mstream list dparser
UNITOBJECTS=$(addsuffix .o, $(UNITNAMES))
UNITFILES=$(addsuffix .ppu, $(UNITNAMES))
PROGSOURCES=$(addsiffix .pp, $(PROGNAMES))
PROGOBJECTS=$(addsuffix .o, $(PROGNAMES))

# Adapt options. Add unit path if needed.
ifdef UNITDIR
override OPT:=$(OPT) -S2 -Up$(UNITDIR)
endif


# Default rule for units
.pp.ppu:
	$(PP) $(OPT) $<

# Default target.
all: $(UNITFILES) $(PROGNAMES)

units: $(UNITFILES)

progs: $(PROGNAMES)

# Default rule for programs
$(PROGNAMES): %:%.pp
	$(PP) $(OPT) $<

#
# Generic install and clean targets
#

install: all
	install -m 755 $(UNITINSTALLDIR)
ifdef UNITNAMES
	install -m 666 $(UNITNAMES) $(UNITINSTALLDIR)
endif
ifdef PROGNAMES
	install -m 755 $(PROGNAMES) $(BININSTALLDIR)
endif

clean:
	rm -f $(UNITOBJECTS) $(UNITFILES) $(PROGNAMES) $(PROGOBJECTS)

# End of makefile.
