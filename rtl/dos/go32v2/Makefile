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
OS_SOURCE=go32v1
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=go32v2
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
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

# Define Go32v2 Units
SYSTEMPPU=system$(PPUEXT)

OBJECTS=strings go32 \
	dpmiexcp profile dxeload emu387 \
	dos crt objects printer \
	cpu mmx mouse getopts graph

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

all : $(OBJLOADERS) $(PPUOBJECTS) objpas

install : all
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(UNITINSTALLDIR)

clean :
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) $(PPAS) link.res log
	-$(DELTREE) *$(SMARTEXT)
	make -C $(OBJPASDIR) clean

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
	$(COMPILER) -Us -Sg system.pp $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(COMPILER) strings.pp $(REDIR)
	$(DEL) strings.pp

go32$(PPUEXT) : ../go32.pp $(SYSTEMPPU)
	$(COPY) ../go32.pp .
	$(COMPILER) go32.pp $(REDIR)
	$(DEL) go32.pp

#
# Delphi Object Model
#

objpas:
	make -C $(OBJPASDIR) UNITTARGETDIR=$(BASEDIR) UNITDIR=$(BASEDIR)

#
# System Dependent Units
#

dpmiexcp$(PPUEXT) : dpmiexcp.pp exceptn$(OEXT) $(SYSTEMPPU)
	$(COMPILER) -Sg dpmiexcp.pp $(REDIR)

profile$(PPUEXT) : profile.pp dpmiexcp$(PPUEXT) go32$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) profile.pp $(REDIR)

dxeload$(PPUEXT) : $(SYSTEMPPU)
	$(COMPILER) dxeload.pp $(REDIR)

emu387$(PPUEXT) : emu387.pp fpu$(OEXT) strings$(PPUEXT) dxeload$(PPUEXT) \
		  dpmiexcp$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) emu387.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : ../dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	       go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)
	$(COPY) ../dos.pp .
	$(COMPILER) dos $(REDIR)
	$(DEL) dos.pp

crt$(PPUEXT) : ../crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)
	$(COPY) ../crt.pp .
	$(COMPILER) crt $(REDIR)
	$(DEL) crt.pp

objects$(PPUEXT) : $(INC)/objects.pp $(INC)/platform.inc objinc.inc $(SYSTEMPPU)
	$(COPY) $(INC)/objects.pp .
	$(COMPILER) objects.pp $(REDIR)
	$(DEL) objects.pp

printer$(PPUEXT) : ../printer.pp $(SYSTEMPPU)
	$(COPY) ../printer.pp .
	$(COMPILER) printer.pp $(REDIR)
	$(DEL) printer.pp

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/cpu.pp .
	$(COMPILER) cpu.pp $(REDIR)
	$(DEL) cpu.pp

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(PROCINC)/mmx.pp .
	$(COMPILER) mmx.pp $(REDIR)
	$(DEL) mmx.pp

mouse$(PPUEXT) : ../mouse.pp $(SYSTEMPPU)
	$(COPY) ../mouse.pp .
	$(COMPILER) mouse.pp $(REDIR)
	$(DEL) mouse.pp

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(INC)/getopts.pp .
	$(COMPILER) getopts.pp $(REDIR)
	$(DEL) getopts.pp

PPIFILES:=$(wildcard $(PPI)/*.ppi)
graph$(PPUEXT) : ../graph.pp go32$(PPUEXT) $(SYSTEMPPU) mmx$(PPUEXT) $(PPIFILES)
	$(COPY) ../graph.pp .
	$(COMPILER) -I$(PPI) graph $(REDIR)
	$(DEL) graph.pp

#####################################################################
# Libs
#####################################################################

staticlib:
	make libsclean
	make all SMARTLINK=YES LIBNAME=fpc LIBTYPE=static

sharedlib:
	@echo Shared Libraries not supported for Go32v2

staticlibinstall: staticlib
	$(MKDIR) $(STATIC_LIBINSTALLDIR)
	$(MKDIR) $(STATIC_UNITINSTALLDIR)
	$(INSTALLEXE) fpc$(STATICLIBEXT) $(STATIC_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(STATIC_UNITINSTALLDIR)

sharedlibinstall: sharedlib

libinstall: staticlibinstall

libsclean : clean
	-$(DEL) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)

#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.2  1998-09-15 12:09:09  peter
#   * merged updates
#
# Revision 1.1.2.1  1998/09/15 12:02:03  peter
#   * updates to get objpas using its own makefile
#
# Revision 1.1	1998/09/10 14:13:28  peter
#   * renamed
#
# Revision 1.16  1998/09/10 10:20:41  florian
#   * fixed typo of previous commit
#
# Revision 1.15  1998/09/10 10:16:52  florian
#   * make staticlib does now a make libclean instead of make clean
#
# Revision 1.14  1998/08/20 08:08:37  pierre
#   * dpmiexcp did not compile with older versions
#     due to the proc to procvar bug
#   * makefile separator problem fixed
#
# Revision 1.13  1998/08/19 10:05:01  peter
#   * fixed for go32v2 staticlib
#
# Revision 1.12  1998/08/05 10:31:05  pierre
#   + added BUGFIX test to be able to compile bugfix branch
#
# Revision 1.11  1998/07/29 15:44:37  michael
#  included sysutils and math.pp as target. They compile now.
#
# Revision 1.10  1998/07/22 21:37:03  michael
# make cycle now works
#
# Revision 1.9	1998/05/22 00:39:36  peter
#   * go32v1, go32v2 recompiles with the new objects
#   * remake3 works again with go32v2
#   - removed some "optimizes" from daniel which were wrong
#
# Revision 1.8	1998/05/06 11:53:40  peter
#   * update
#
#
