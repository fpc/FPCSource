#######################################################################
#   Makefile for Free Pascal
#   (C) 1998 Michael van Canneyt
#######################################################################
#
#  Configurable section
#

# What Compiler should we use ?
PP=ppc386

# Where are the Free Pascal units ? (Optional)
#UNITDIR = c:/pp/units/dos/go32v2

# Processor you are using
CPU=i386
#CPU=m68k

# Any options you wish to pass to the compiler
OPT=

# Where to install the units ?
UNITINSTALLDIR=c:/pp/units/fcl

# Where to install the programs ?
BININSTALLDIR=/pp/bin

# Install program ?
INSTALL=cp

# Mkdir program ?
MKDIR=mkdir

#######################################################################
# End of configurable section. Do not edit below this line.
#######################################################################

.SUFFIXES: .pp .ppu .pas
.PHONY: all install clean units progs

INCDIR=../../inc
CPUDIR=../../$(CPU)

include $(INCDIR)/Makefile.inc

# Set inc
INCFILENAMES=$(addprefix $(INCDIR)/,$(INCNAMES))

# If nothing special needs doing, then just fill in the names here.
# The rest should be automatic.
UNITNAMES=classes
#PROGNAMES=
UNITOBJECTS=$(addsuffix .o, $(UNITNAMES))
UNITFILES=$(addsuffix .ppu, $(UNITNAMES))
PROGSOURCES=$(addsiffix .pp, $(PROGNAMES))
PROGOBJECTS=$(addsuffix .o, $(PROGNAMES))

# Adapt options. Add unit path if needed.
override OPT:=$(OPT) -S2 -I$(INCDIR) -I$(CPUDIR)

ifdef UNITDIR
override OPT:=$(OPT) -Up$(UNITDIR) 
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

classes.ppu: classes.pp $(INCFILENAMES) $(PROCFILENAMES)

#
# Generic install and clean targets
#

install: all
	$(MKDIR) $(UNITINSTALLDIR)
ifdef UNITNAMES
	$(INSTALL) $(UNITNAMES) $(UNITINSTALLDIR)
endif
ifdef PROGNAMES
	$(INSTALL) $(PROGNAMES) $(BININSTALLDIR)
endif

clean:
	rm -f $(UNITOBJECTS) $(UNITFILES) $(PROGNAMES) $(PROGOBJECTS)

# End of makefile.
