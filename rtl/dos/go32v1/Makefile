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
CFG=../../cfg
INC=../../inc
PROCINC=../../$(CPU)
OBJPASDIR=../../objpas
# Where are the .ppi files.
PPI=../ppi

# Get some defaults for Programs and OSes.
# This will set the following variables :
# inlinux indos COPY REPLACE DEL INSTALL INSTALLEXE MKDIR
# It will also set OPT for cross-compilation, and add required options.
# also checks for config file.
# it expects INC PROCINC to be set !!
include $(CFG)/makefile.cfg

# Get the system independent include file names.
# This will set the following variables :
# SYSINCNAMES
include $(INC)/makefile.inc
SYSINCDEPS=$(addprefix $(INC)/,$(SYSINCNAMES))

# Get the processor dependent include file names.
# This will set the following variables :
# CPUINCNAMES
include $(PROCINC)/makefile.cpu
SYSCPUDEPS=$(addprefix $(PROCINC)/,$(CPUINCNAMES))

# Get the PPI file names
# This sets the variable name PPIINCNAMES
include $(PPI)/makefile.ppi
PPIDEPS=$(addprefix $(PPI)/,$(PPIINCNAMES))

# Put system unit dependencies together.
SYSDEPS=$(SYSINCDEPS) $(SYSCPUDEPS)



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
OBJECTS=strings go32 objpas sysutils math\
	dos crt objects printer \
	cpu mmx mouse getopts graph \

LOADERS=prt0

#####################################################################
# System independent Makefile
#####################################################################

# Add Prefix and Suffixes
OBJLOADERS=$(addsuffix $(OEXT), $(LOADERS))
PPUOBJECTS=$(addsuffix $(PPUEXT), $(OBJECTS))


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

$(SYSTEMPPU) : system.pp $(SYSDEPS)
	$(PP) $(OPT) -Us -Sg system.pp $(REDIR)

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

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(SYSTEMPPU) objpas$(PPUEXT) \
                   dos$(PPUEXT)
	$(COPY) $(OBJPASDIR)/sysutils.pp .
	$(PP) $(OPT) -I$(OBJPASDIR) sysutils $(REDIR)
	$(DEL) sysutils.pp

math$(PPUEXT) : $(OBJPASDIR)/math.pp $(SYSTEMPPU) objpas$(PPUEXT) \
                   dos$(PPUEXT)
	$(COPY) $(OBJPASDIR)/math.pp .
	$(PP) $(OPT) -I$(OBJPASDIR) math $(REDIR)
	$(DEL) math.pp
#
# System Dependent Units
#


#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	       go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)
	$(COPY) ../dos.pp .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) dos.pp

crt$(PPUEXT) : ../crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)
	$(COPY) ../crt.pp .
	$(PP) $(OPT) crt $(REDIR)
	$(DEL) crt.pp

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
	$(COPY) $(INC)/objects.pp .
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

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(INC)/getopts.pp .
	$(PP) $(OPT) getopts.pp $(REDIR)
	$(DEL) getopts.pp

PPIFILES:=$(wildcard $(PPI)/*.ppi)
graph$(PPUEXT) : ../graph.pp go32$(PPUEXT) $(SYSTEMPPU) mmx$(PPUEXT) $(PPIFILES)
	$(COPY) ../graph.pp .
	$(PP) $(OPT) -I$(PPI) graph $(REDIR)
	$(DEL) graph.pp


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

#
# $Log$
# Revision 1.1  1998-09-10 14:15:48  peter
#   - renamed makefile to Makefile
#
#