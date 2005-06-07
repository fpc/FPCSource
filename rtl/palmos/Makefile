#
#   $Id: Makefile,v 1.2 2002/09/07 16:01:25 peter Exp $
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Michael van Canneyt
#
#   Makefile for the Free Pascal PalmOS Runtime Library
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

# What is the Operating System ?
ifndef OS_SOURCE
OS_SOURCE=dos
endif

# What is the target operating system ?
ifndef OS_TARGET
override OS_TARGET=palmos
endif

# What compiler to use ?
ifndef PP
PP=ppc68k
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

# Use smartlinking ?
ifndef SMARTLINK
SMARTLINK=NO
endif

# Name of library ?
# If this is set, all units will be put in the same library.
# If it is empty (default), the units will be left in separate files.
ifndef LIBNAME
LIBNAME=
#LIBNAME=fpc
endif

# Should the library be shared or static (only if LIBNAME is set).
# Set this to 'shared' or 'static' to create a librrary
# Setting this to shared will disable smart linking.
ifndef  LIBTYPE
LIBTYPE=
#LIBTYPE=static
endif

# Where is the PPUMOVE program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################
override OS_TARGET=palmos
override CPU=m68k

#####################################################################
# System independent
#####################################################################

# Where are the include files
RTL=..
CFG=$(RTL)/cfg
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)
OBJPASDIR=$(RTL)/objpas

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

# Check if we need C library.
ifeq ($(LINK_TO_C),YES)
override OPT:=$(OPT) -dCRTLIB
endif

# Define PalmOS units
SYSTEMPPU = syspalm.ppu
OBJECTS=syspalm

PRT=prt0
LOADERAS=$(PRT).as

# Define Loaders
LOADERS=

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

#prt0$(OEXT) : $(LOADERAS)
#	-as $(LOADERAS) -o prt0$(OEXT)

#gprt0$(OEXT) : $(GLOADERAS)
#	-as $(GLOADERAS) -o gprt0$(OEXT)

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syspalm.pp $(SYSLINUXDEPS) $(SYSDEPS)
	$(COMPILER) -Us -Sg syspalm.pp $(REDIR)

# strings$(PPUEXT) : ../template/strings.pp $(SYSTEMPPU)
#         $(COPY) ../template/strings.pp .
#         $(PP) $(OPT) strings $(REDIR)
#         $(DEL) strings.pp

#
# Delphi Object Model
#

# objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(SYSTEMPPU)
#         $(COPY) $(OBJPASDIR)/objpas.pp .
#         $(PP) $(OPT) objpas $(REDIR)
#         $(DEL) objpas.pp

#
# System Dependent Units
#

#
# TP7 Compatible RTL Units
#

#dos$(PPUEXT) : $(DOSDEPS) $(SYSTEMPPU)
#	$(PP) $(OPT) dos $(REDIR)

#crt$(PPUEXT) : crt.pp $(INC)/textrec.inc $(INC)/filerec.inc $(SYSTEMPPU)
#	$(PP) $(OPT) crt $(REDIR)

#objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
#	$(COPY) $(INC)/objects.pp .
#	$(PP) $(OPT) objects $(REDIR)
#	$(DEL) objects.pp

#
# Other RTL Units
#

#####################################################################
# Libs
#####################################################################

staticlib:
	make clean
	make all SMARTLINK=YES LIBNAME=fpc LIBTYPE=static

sharedlib:
	make clean
	make all
	$(PPUMOVE) -o fpc $(SHAREDLIBFILES)

staticlibinstall: staticlib
	$(MKDIR) $(STATIC_LIBINSTALLDIR)
	$(MKDIR) $(STATIC_UNITINSTALLDIR)
	$(INSTALLEXE) libfpc$(STATICLIBEXT) $(STATIC_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(STATIC_UNITINSTALLDIR)

sharedlibinstall: sharedlib
	$(MKDIR) $(SHARED_LIBINSTALLDIR)
	$(MKDIR) $(SHARED_UNITINSTALLDIR)
	$(INSTALLEXE) libfpc$(SHAREDLIBEXT) $(SHARED_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(SHARED_UNITINSTALLDIR)
	ldconfig

libinstall: staticlibinstall sharedlibinstall

libsclean : clean
	-$(DEL) *$(SMARTLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)

#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

