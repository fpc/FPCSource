#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1998-2000 by Michael van Canneyt
#
#   Makefile for the Free Pascal Amiga Runtime Library
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
OS_SOURCE=linux
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=amiga
endif

# use as68k if crosscompiling 
ifneq ($(OS_SOURCE),$(OS_TARGET))
override OPT+= -AGAS
endif

# What is the target processor :
ifndef CPU
#CPU=i386
CPU=m68k
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

# Override extensions
PPUEXT=.ppa
ASMEXT=.asm

# Define Linux Units
SYSTEMPPU=sysamiga$(PPUEXT)
# no compiling :
# getopts : no argv !!
# sysutils : problem with objpas unit ??
OBJECTS=strings exec dos crt printer objects objpas heaptrc

PRT=prt0
LOADERAS=$(PRT).as

# Define Loaders
LOADERS=prt0

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

prt0$(OEXT) : $(LOADERAS)
	-$(AS) $(LOADERAS) -o prt0$(OEXT)

#gprt0$(OEXT) : $(GLOADERAS)
#	-$(AS) $(GLOADERAS) -o gprt0$(OEXT)

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : sysamiga.pas $(SYSLINUXDEPS) $(SYSDEPS)
	$(PP) $(OPT) -ui386 -dm68k -I$(INC) -I$(PROCINC) -Us -Sg sysamiga.pas $(REDIR)

strings$(PPUEXT) : ../template/strings.pp $(SYSTEMPPU)
	$(COPY) ../template/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

exec$(PPUEXT) : exec.pp exec.inc $(SYSTEMPPU)
	$(PP) $(OPT) exec $(REDIR)

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(SYSTEMPPU)
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(PP) $(OPT) -I$(INC) objpas $(REDIR)
	$(DEL) objpas.pp

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp objpas$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(OBJPASDIR)/sysutils.pp .
	$(PP) $(OPT) -I$(INC) sysutils $(REDIR)
	$(DEL) sysutils.pp

#
# System Dependent Units
#

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : $(DOSDEPS) $(SYSTEMPPU)
	$(PP) $(OPT) -I$(INC) dos $(REDIR)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc $(SYSTEMPPU)
	$(PP) $(OPT) -I$(INC) crt $(REDIR)

printer$(PPUEXT) : printer.pp $(SYSTEMPPU)
	$(PP) $(OPT) printer $(REDIR)
	
objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
	$(COPY) $(INC)/objects.pp .
	$(PP) $(OPT) -I$(INC) objects $(REDIR)
	$(DEL) objects.pp

#
# Other RTL Units
#

getopts$(PPUEXT) : $(INC)/getopts.pp strings$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(INC)/getopts.pp .
	$(PP) $(OPT) -I$(INC) getopts $(REDIR)
	$(DEL) getopts.pp
	
heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COPY) $(INC)/heaptrc.pp .
	$(PP) $(OPT) heaptrc $(REDIR)
	$(DEL) heaptrc.pp
	
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

#
# $Log$
# Revision 1.7  2000-01-07 16:32:21  daniel
#   * copyright 2000 added
#
# Revision 1.6  1998/10/22 16:43:54  pierre
#   + added some units
#
# Revision 1.5  1998/10/14 10:49:46  pierre
#   * added some include directory command lines necessary
#   * added some missing units
#   + now compiles completely  with cross-compiler
#
# Revision 1.4  1998/10/13 16:31:14  pierre
#  + added -AGAS for crosscompilers
#
# Revision 1.3  1998/10/13 08:07:01  pierre
#   * uses AS variable (can be set to as68k when crosscompiling)
#   * correct command line for sysamiga.pas
#
# Revision 1.2  1998/10/02 09:25:58  peter
#   * fixed rtl path
#
# Revision 1.1  1998/09/10 14:15:47  peter
#   - renamed makefile to Makefile
#
#