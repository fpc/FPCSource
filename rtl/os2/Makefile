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

# Warning: this file contains TAB (#9) characters that are required for
# make. Make sure you use an editor that does not replace TABs with
# spaces, or the makefile won't work anymore after you save.

#####################################################################
# Start of configurable section.
# Please note that all these must be set in the main makefile, and
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

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
# LIBTYPE=shared

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
OEXT=.oo2

# Define Linux Units
SYSTEMPPU=sysos2$(PPUEXT)
OBJECTS=strings dos crt objpas sysutils
# objects still not implemented !!

LOADERS=prt0 prt1 code2 code3

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

%$(OEXT) : %.as
	as -o $*$(OEXT) $*.as

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : sysos2.pas $(SYSDEPS)
	$(COMPILER) -Us -Sg sysos2.pas $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(COMPILER) strings $(REDIR)
	$(DEL) strings.pp

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(INC)/except.inc
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(COMPILER) -S2 -I$(OBJPASDIR) objpas $(REDIR)
	$(DEL) objpas.pp

SYSUTILINC = $(wildcard $(OBJPASDIR)/*.inc)

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(SYSUTILINC) filutil.inc disk.inc
	$(COPY) $(OBJPASDIR)/sysutils.pp .
	$(COMPILER) -S2 -I$(OBJPASDIR) sysutils $(REDIR)
	$(DEL) sysutils.pp


#
# System Dependent Units
#

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pas $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) dos.pas $(REDIR)

crt$(PPUEXT) : crt.pas dos$(PPUEXT)
	$(COMPILER) crt.pas $(REDIR)

#objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
#	 $(COPY) $(INC)/objects.pp .
#	 $(COMPILER) $(OPT) objects $(REDIR)
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
# Revision 1.4  1998-11-24 19:51:21  jonas
#   + added warning about TABs
#
# Revision 1.3  1998/10/27 15:14:03  pierre
#   * update sys_os2 so that it compiles with current RTL
#     (do_isdevice might be wrong !! Daniel could you check this !!)
#   * fixed makefile (works from go32v2 compiler)
#     I had to remove library=shared !!
#     (it tried to use ld with -CD !!)
#
# Revision 1.2  1998/10/11 13:46:31  michael
# Sysutils changes
#
# Revision 1.1  1998/09/10 14:17:51  peter
#   -renamed
#
#
