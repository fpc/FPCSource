#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Go32v2 Runtime Library
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

# What is the Operating System
ifndef OS_SOURCE
OS_SOURCE=GO32V2
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=GO32V2
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
CFG=../../cfg
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

# Put system unit dependencies together.
SYSDEPS=$(SYSINCDEPS) $(SYSCPUDEPS)

#####################################################################
# System dependent
#####################################################################

# Define Linux Units
SYSTEMPPU=system$(PPUEXT)
OBJECTS=strings go32 objpas \
	dpmiexcp profile dxeload emu387 \
	dos crt objects printer \
	cpu mmx mouse getopts graph \

LOADERS=prt0 exceptn fpu

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
	$(MKDIR) $(BASEINSTALLDIR)
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

prt0$(OEXT) : v2prt0.as
	as -o prt0$(OEXT) v2prt0.as

exceptn$(OEXT) : exceptn.as
	as -o exceptn$(OEXT) exceptn.as

fpu$(OEXT) : fpu.as
	as -o fpu$(OEXT) fpu.as

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
	-$(PP) $(OPT) objpas.pp $(REDIR)
	$(DEL) objpas.pp

#
# System Dependent Units
#

dpmiexcp$(PPUEXT) : dpmiexcp.pp exceptn$(OEXT) $(SYSTEMPPU)
	$(PP) $(OPT) -Sg dpmiexcp.pp $(REDIR)

profile$(PPUEXT) : profile.pp dpmiexcp$(PPUEXT) go32$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) profile.pp $(REDIR)

dxeload$(PPUEXT) : $(SYSTEMPPU)
	$(PP) $(OPT) dxeload.pp $(REDIR)

emu387$(PPUEXT) : emu387.pp fpu$(OEXT) strings$(PPUEXT) dxeload$(PPUEXT) \
		  dpmiexcp$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) emu387.pp $(REDIR)

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

objects$(PPUEXT) : $(INC)/objects.pp $(INC)/platform.inc objinc.inc $(SYSTEMPPU)
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

libs: all libfpc$(LIBEXT)

libfpc.a:
	$(PPUMOVE) -s -o fpc *.ppu

libsclean : clean
	-$(DEL) *.$(LIBEXT) *$(PPLEXT)

libinstall: libs
	$(INSTALL) libfpc$(LIBEXT) $(LIBINSTALLDIR)
	$(INSTALL) *$(PPLEXT) $(UNITINSTALLDIR)

#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.9  1998-05-22 00:39:36  peter
#   * go32v1, go32v2 recompiles with the new objects
#   * remake3 works again with go32v2
#   - removed some "optimizes" from daniel which were wrong
#
# Revision 1.8	1998/05/06 11:53:40  peter
#   * update
#
#
