#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Go32v1 Runtime Library
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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
UNITINSTALLDIR=c:\lib\ppc\go32v1
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
OS_TARGET=GO32V1
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
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
PROCINC=../../$(CPU)
OBJPASDIR=../../objpas
# Where are the .ppi files.
PPI=../ppi

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

# To install files
ifndef INSTALL
ifeq ($(DOS),YES)
INSTALL=copy
else
INSTALL=install -m 644
endif
endif

# To install programs
ifndef INSTALLEXE
ifeq ($(DOS),YES)
INSTALLEXE=copy
else
INSTALLEXE=install -m 755
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

#
# System independent Commandline Options
#

# Cross compiling ?
ifeq ($(OS_TARGET),$(OS_SRC))
CROSSCOMPILE=NO
else
CROSSCOMPILE=YES
endif

# add required options... (-dFPC is required for older versions)
override OPT:= $(OPT) -dFPC -d$(CPU)

# Was a config file specified ?
ifdef CFGFILE
override OPT:=$(OPT) @$(CFGFILE)
endif

# Check for crosscompile
ifeq ($(CROSSCOMPILE),YES)
override OPT:= $(OPT) -dCROSSCOMPILE -T$(OS_TARGET)
endif

#
# System dependent Commandline Options
#


#####################################################################
# System dependent
#####################################################################

# Determine needed extensions
PPUEXT=.pp1
PPLEXT=.ppl
OEXT=.o1
ASMEXT=.s1
LIBEXT=.a1

# Define Linux Units
SYSTEMPPU=system$(PPUEXT)
OBJECTS=strings go32 objpas \
	dos crt objects printer \
	cpu mmx mouse getopts graph \

LOADERS=prt0

#####################################################################
# System independent Makefile
#####################################################################

# OS Independent Depends
SYSTEMDEPS=system.inc systemh.inc mathh.inc real2str.inc \
	heaph.inc innr.inc sstrings.inc file.inc \
	text.inc typefile.inc version.inc filerec.inc \
	textrec.inc

# Processor Dependent Depends
SYSPROCDEPS=math.inc set.inc heap.inc $(CPU).inc

# Add Prefix and Suffixes
OBJLOADERS=$(addsuffix $(OEXT), $(LOADERS))
PPUOBJECTS=$(addsuffix $(PPUEXT), $(OBJECTS))
DSYSTEMDEPS=$(addprefix $(INC)/, $(SYSTEMDEPS))
DSYSPROCDEPS=$(addprefix $(PROCINC)/, $(SYSPROCDEPS))


.PHONY : all install clean \
	 libs libsclean \
	 diffs diffclean \

all : $(OBJLOADERS) $(PPUOBJECTS)

install : all
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(UNITINSTALLDIR)

clean :
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) *.PPS log

#####################################################################
# Files
#####################################################################

#
# Loaders
#

prt0$(OEXT) : prt0.as
	as -D -o prt0$(OEXT) prt0.as

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : system.pp $(DSYSTEMDEPS) $(DSYSPROCDEPS)
	$(COPY) $(DSYSTEMDEPS) .
	$(COPY) $(DSYSPROCDEPS) .
	$(PP) $(OPT) -Us -Sg system.pp $(REDIR)
	$(DEL) $(SYSTEMDEPS)
	$(DEL) $(SYSPROCDEPS)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings.pp $(REDIR)
	$(DEL) strings.pp

go32$(PPUEXT) : ../go32.pp $(SYSTEMPPU)
	$(COPY) ../go32.pp .
	$(PP) $(OPT) go32.pp $(REDIR)
	$(DEL) go32.pp

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(SYSTEMPPU)
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(PP) $(OPT) objpas $(REDIR)
	$(DEL) objpas.pp

#
# System Dependent Units
#


#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	       go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)
	$(COPY) ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) dos.pp filerec.inc textrec.inc

crt$(PPUEXT) : ../crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)
	$(COPY) ../crt.pp $(INC)/textrec.inc .
	$(PP) $(OPT) crt $(REDIR)
	$(DEL) crt.pp textrec.inc

objects$(PPUEXT) : ../objects.pp $(SYSTEMPPU)
	$(COPY) ../objects.pp .
	$(PP) $(OPT) objects.pp $(REDIR)
	$(DEL) objects.pp

printer$(PPUEXT) : ../printer.pp $(SYSTEMPPU)
	$(COPY) ../printer.pp .
	$(PP) $(OPT) printer.pp $(REDIR)
	$(DEL) printer.pp

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/cpu.pp .
	$(PP) $(OPT) cpu.pp $(REDIR)
	$(DEL) cpu.pp

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(PROCINC)/mmx.pp .
	$(PP) $(OPT) mmx.pp $(REDIR)
	$(DEL) mmx.pp

mouse$(PPUEXT) : ../mouse.pp $(SYSTEMPPU)
	$(COPY) ../mouse.pp .
	$(PP) $(OPT) mouse.pp $(REDIR)
	$(DEL) mouse.pp

getopts$(PPUEXT) : $(PROCINC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts.pp $(REDIR)
	$(DEL) getopts.pp

graph$(PPUEXT) : ../graph.pp go32$(PPUEXT) $(SYSTEMPPU) mmx$(PPUEXT) \
	$(PPI)/arc.ppi $(PPI)/colors.ppi $(PPI)/dpmi2raw.ppi $(PPI)/ellipse.ppi \
	$(PPI)/fill.ppi $(PPI)/font.ppi $(PPI)/global.ppi $(PPI)/ibm.ppi \
	$(PPI)/image.ppi $(PPI)/line.ppi $(PPI)/modes.ppi $(PPI)/move.ppi \
	$(PPI)/palette.ppi $(PPI)/pixel.ppi $(PPI)/stdcolor.ppi $(PPI)/text.ppi \
	$(PPI)/triangle.ppi $(PPI)/vesadeb.ppi
	$(COPY) ../graph.pp $(PPI)/*.ppi .
	$(PP) $(OPT) graph $(REDIR)
	$(DEL) graph.pp *.ppi


#####################################################################
# Libs
#####################################################################

libs: all

libsclean : clean
	-$(DEL) *.$(LIBEXT) *$(PPLEXT)

#####################################################################
# Diffs
#####################################################################

%.dif : %.pp
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/dos/go32v1/$*.pp > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/dos/go32v1/$*.inc > $*.dif

%.dif : %.as
	-$(DIFF) $(DIFFOPTS) $*.as $(REFPATH)/dos/go32v1/$*.as > $*.dif

diffclean:
	-$(DEL) *.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/dos/go32v1/makefile > makefile.dif

diffs: system.dif os.dif makefile.dif prt0.dif

#####################################################################
# Distribution
#####################################################################

distclean : clean libsclean diffclean

