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

# Warning: this file contains TAB (#9) characters that are required for
# make. Make sure you use an editor that does not replace TABs with
# spaces, or the makefile won't work anymore after you save.

#####################################################################
# Makefile Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=go32v2
override CPU=i386

# Where are the include files
RTL=..
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)

# Where to place the result files
TARGETDIR=.

# These units belong to the RTL
UNITPREFIX=rtl


#####################################################################
# Own defaults
#####################################################################

# Paths
OBJPASDIR=$(RTL)/objpas
PPI=ppi

# Define Go32v2 Units
SYSTEMUNIT=system

# Loaders
LOADEROBJECTS=prt0 exceptn fpu

# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) strings go32 initc \
	  dpmiexcp profile dxeload emu387 \
	  dos crt objects printer \
	  objpas sysutils math typinfo \
	  cpu mmx getopts heaptrc \
	  msmouse graph


#####################################################################
# Include default makefile
#####################################################################

# test if FPCMAKE is still valid
ifdef FPCMAKE
ifeq ($(strip $(wildcard $(FPCMAKE))),)
FPCDIR=
FPCMAKE=
endif
endif

ifndef FPCDIR
ifdef DEFAULTFPCDIR
FPCDIR=$(DEFAULTFPCDIR)
endif
endif

ifndef FPCMAKE
ifdef FPCDIR
FPCMAKE=$(FPCDIR)/makefile.fpc
else
FPCMAKE=makefile.fpc
endif
endif

override FPCMAKE:=$(strip $(wildcard $(FPCMAKE)))
ifeq ($(FPCMAKE),)
nofpcmake:
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@exit
else
include $(FPCMAKE)
endif


#####################################################################
# Include system unit dependencies
#####################################################################

SYSTEMPPU=$(addsuffix $(PPUEXT),$(SYSTEMUNIT))

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
# Dependencies
#####################################################################

vpath %$(PASEXT) $(INC) $(PROCINC)

#
# Loaders
#

prt0$(OEXT) : v2prt0.as
	$(AS) -o prt0$(OEXT) v2prt0.as

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : system.pp $(SYSDEPS)
	$(COMPILER) -Us -Sg system.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc \
		   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc \
		   $(SYSTEMPPU)

go32$(PPUEXT) : go32.pp $(SYSTEMPPU)

#
# Delphi Object Model
#

include $(OBJPASDIR)/makefile.op


#
# System Dependent Units
#

dpmiexcp$(PPUEXT) : dpmiexcp.pp exceptn$(OEXT) $(SYSTEMPPU)
	$(COMPILER) -Sg dpmiexcp.pp $(REDIR)

initc$(PPUEXT) : initc.pp $(SYSTEMPPU)

profile$(PPUEXT) : profile.pp dpmiexcp$(PPUEXT) go32$(PPUEXT)

dxeload$(PPUEXT) : dxeload.pp $(SYSTEMPPU)

emu387$(PPUEXT) : emu387.pp fpu$(OEXT) strings$(PPUEXT) dxeload$(PPUEXT) \
		  dpmiexcp$(PPUEXT)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	       go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

printer$(PPUEXT) : printer.pp $(SYSTEMPPU)

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)

msmouse$(PPUEXT) : msmouse.pp $(SYSTEMPPU)

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)

graph$(PPUEXT) : graph.pp go32$(PPUEXT) $(SYSTEMPPU) mmx$(PPUEXT) \
		 $(wildcard $(PPI)/*.ppi)
	$(COMPILER) -I$(PPI) graph.pp $(REDIR)

#######################################################################
# Clean targets
#######################################################################

clean: fpc_clean

libsclean: fpc_libsclean

# $Log$
# Revision 1.10  1999-03-12 21:04:45  michael
# +  clean and libsclean added
#
# Revision 1.9  1999/03/09 01:35:54  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
