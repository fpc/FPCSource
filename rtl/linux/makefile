#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Linux Runtime Library
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
OS_TARGET=linux
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

# Check if we need C library.
ifeq ($(LINK_TO_C),YES)
override OPT:=$(OPT) -dCRTLIB
endif

# Define Linux Units
SYSTEMPPU=syslinux$(PPUEXT)
OBJECTS=strings linux objpas sysutils math\
	dos crt objects printer \
	getopts errors sockets graph\

# Extra Syslinux Depends
ifeq ($(LINK_TO_C),YES)
SYSLINUXDEPS=lprt$(OEXT)
else
SYSLINUXDEPS=sysconst.inc systypes.inc syscalls.inc
endif

# Determine on what dos unit should depend.
DOSDEPS = dos.pp $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT)
ifeq ($(LINK_TO_C),YES)
DOSDEPS:=$(DOSDEPS) lprt$(OEXT)
else
DOSDEPS:=$(DOSDEPS) linux$(PPUEXT)
endif

# Which prt to use ?
ifdef AOUT
PRT=prt0
else
PRT=prt1
endif

LOADERAS=$(CPU)/$(PRT).as
CLOADERAS=$(CPU)/c$(PRT).as
GLOADERAS=$(CPU)/g$(PRT).as

# Define Loaders
ifeq ($(LINK_TO_C),NO)
LOADERS=prt0 cprt0 gprt0
else
LOADERS=lprt
endif

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
	-as $(LOADERAS) -o prt0$(OEXT)

cprt0$(OEXT) : $(CLOADERAS)
	-as $(CLOADERAS) -o cprt0$(OEXT)

gprt0$(OEXT) : $(GLOADERAS)
	-as $(GLOADERAS) -o gprt0$(OEXT)

lprt$(OEXT) : lprt.c
	gcc $(AOUT) -c lprt.c -o lprt$(OEXT)

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syslinux.pp $(SYSLINUXDEPS) $(SYSDEPS)
	$(PP) $(OPT) -Us -Sg syslinux.pp $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

linux$(PPUEXT) : linux.pp strings$(PPUEXT) $(INC)/textrec.inc $(INC)/filerec.inc \
		 syscalls.inc systypes.inc sysconst.inc $(SYSTEMPPU)
	$(PP) $(OPT) linux.pp $(REDIR)

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

sockets$(PPUEXT) : sockets.pp $(INC)/textrec.inc $(INC)/filerec.inc \
		   linux$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) sockets.pp $(REDIR)

errors$(PPUEXT) : errors.pp strings$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) errors.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : $(DOSDEPS) $(SYSTEMPPU)
	$(PP) $(OPT) dos $(REDIR)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc $(INC)/filerec.inc linux$(PPUEXT)\
	       $(SYSTEMPPU)
	$(PP) $(OPT) crt $(REDIR)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
	$(COPY) $(INC)/objects.pp .
	$(PP) $(OPT) objects $(REDIR)
	$(DEL) objects.pp

printer$(PPUEXT) : printer.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) printer $(REDIR)

graph$(PPUEXT) : graph.pp linux$(PPUEXT) objects$(PPUEXT)
	$(PP) $(OPT) graph $(REDIR)

#
# Other RTL Units
#

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(INC)/getopts.pp .
	$(PP) $(OPT) getopts $(REDIR)
	$(DEL) getopts.pp

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
# Revision 1.17  1998-08-15 17:08:09  peter
#   + support for cprt0.o
#
#