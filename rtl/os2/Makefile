#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal OS/2 Runtime Library
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
OS_SOURCE=go32v2
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=os2
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

# Where is the PPUMOVE program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

# Set this to 'shared' or 'static'
LIBTYPE=shared

# AOUT should be defined in main makefile.
# But you can set it here too.
# AOUT = -DAOUT

# Do you want to link to the C library ?
# Standard it is NO. You can set it to YES to link in th C library.
ifndef LINK_TO_C
LINK_TO_C=NO
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

#####################################################################
# System independent
#####################################################################

# Where are the include files ?
INC=../inc
PROCINC=../$(CPU)
CFG=../cfg
OBJPASDIR=../objpas

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

PPUEXT=.ppo
ASMEXT=.so2
OEXT=.o2

# Define Linux Units
SYSTEMPPU=sysos2$(PPUEXT)
OBJECTS=strings objpas \
        dos crt

LOADERS=prt0 prt1

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
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) log

#####################################################################
# Files
#####################################################################

#
# Loaders
#

prt0$(OEXT) : prt0.as
	as -D -o prt0$(OEXT) prt0.as

prt1$(OEXT) : prt1.as
	as -D -o prt1$(OEXT) prt1.as

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : sysos2.pas $(SYSDEPS)
	$(PP) $(OPT) -Us -Sg sysos2.pas $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

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

dos$(PPUEXT) : dos.pas $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) dos.pas $(REDIR)

crt$(PPUEXT) : crt.pas dos$(PPUEXT)
	$(PP) $(OPT) crt.pas $(REDIR)

#objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
#	 $(COPY) $(INC)/objects.pp .
#	 $(PP) $(OPT) objects $(REDIR)
#	 $(DEL) objects.pp

#
# Other RTL Units
#

#####################################################################
# Libs
#####################################################################

libs	: all libfpc$(LIBEXT)

libfpc.so:
	$(PPUMOVE) -o fpc *.ppu

libfpc.a:
	$(PPUMOVE) -s -o fpc *.ppu

libinstall : libs
	$(INSTALLEXE) libfpc$(LIBEXT) $(LIBINSTALLDIR)
	$(INSTALL) *$(PPLEXT) $(UNITINSTALLDIR)
	ldconfig

libsclean : clean
	-$(DEL) *.a *.so *$(PPLEXT)

#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.1  1998-09-10 14:17:51  peter
#   -renamed
#
#