# makes the utilities for FPC
#
# Copyright (c) 1996 by Michael Van Canneyt

#####################################################################
# Start of configurable section.
# Please note that all these must be set in the main makefile, and
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

# where are the units ?
ifndef UNITDIR
UNITDIR=../linux
endif

# set the directory where to install the units.
ifndef UNITINSTALLDIR
UNITINSTALLDIR=/usr/lib/ppc/0.99.0/objpas
endif

# What is the Operating System ?
ifndef OS_SRC
OS_SRC=linux
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
endif

# What compiler to use ?
ifndef PP
PP=ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

# Where are the include files ?
INC=../inc
PROCINC=../$(CPU)

# add required options...
override OPT:= $(OPT) -d$(CPU)

# Add unitdir
ifdef UNITDIR
override OPT:=$(OPT) -Up$(UNITDIR)
endif

# To copy pograms
ifndef COPY
ifeq ($(DOS),YES)
COPY=copy
else
COPY=cp -p
endif
endif

# Check delete program
ifndef DEL
ifeq ($(DOS),YES)
DEL=del
else
DEL=rm -f
endif
endif

# To install programs
ifndef INSTALL
ifeq ($(DOS),YES)
INSTALL=copy
else
INSTALL=install
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

# Was a config file specified ?
ifdef CFGFILE
override OPT:=$(OPT) @$(CONFIG)
endif

# determine needed extensions.
PPUEXT=.ppu
OEXT=.o

# Targets
UNITOBJECTS=math.ppu sysutils.ppu

.PHONY : all clean diffs install diffclean

all : $(UNITOBJECTS)

$(UNITOBJECTS): %.ppu: %.pp
	$(PP) $(OPT) $* 

install : all
	$(INSTALL) -m 755 $(UNITOBJECTS) $(UNITINSTALLDIR)

clean:
	-$(DEL) *.o *.s *.ppu
	-$(DEL) $(UNITOBJECTS)

