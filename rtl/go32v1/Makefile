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

# Warning: this file contains TAB (#9) characters that are required for
# make. Make sure you use an editor that does not replace TABs with
# spaces, or the makefile won't work anymore after you save.

#####################################################################
# Configuration section
#####################################################################

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
ifndef	LIBTYPE
LIBTYPE=
#LIBTYPE=static
endif


#####################################################################
# Defaults
#####################################################################

# set target and cpu which are required
OS_TARGET=go32v1
CPU=i386

# Where are the include files
RTL=..
CFG=$(RTL)/cfg
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)
OBJPASDIR=$(RTL)/objpas

# Where are the results placed
TARGETDIR=.


#####################################################################
# Include default makefile
#####################################################################

include $(CFG)/makefile.cfg


#####################################################################
# System dependent
#####################################################################

# Define Linux Units
SYSTEMPPU=system$(PPUEXT)
OBJECTS=strings go32 \
	dos crt objects printer \
        objpas sysutils typinfo \
	cpu mmx getopts heaptrc mouse

LOADERS=prt0


#####################################################################
# Include system unit dependencies
#####################################################################

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
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) $(PPAS) link.res log
	-$(DELTREE) *$(SMARTEXT)


#####################################################################
# Files
#####################################################################

#
# Loaders
#

prt0$(OEXT) : prt0.as
	as -o prt0$(OEXT) prt0.as

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : system.pp $(SYSDEPS)
	$(COMPILER) -Us -Sg system.pp $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COMPILER) $(PROCINC)/strings.pp $(REDIR)

go32$(PPUEXT) : go32.pp $(SYSTEMPPU)
	$(COMPILER) go32.pp $(REDIR)

#
# Delphi Object Model
#

include $(OBJPASDIR)/makefile.op


#
# System Dependent Units
#

mouse$(PPUEXT) : mouse.pp $(SYSTEMPPU)
	$(COMPILER) mouse.pp $(REDIR)


#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	       go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) dos.pp $(REDIR)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) crt.pp $(REDIR)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
	$(COMPILER) $(INC)/objects.pp $(REDIR)

printer$(PPUEXT) : printer.pp $(SYSTEMPPU)
	$(COMPILER) printer.pp $(REDIR)

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)
	$(COMPILER) $(PROCINC)/cpu.pp $(REDIR)

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) $(PROCINC)/mmx.pp $(REDIR)

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)
	$(COMPILER) $(INC)/getopts.pp $(REDIR)

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COMPILER) $(INC)/heaptrc $(REDIR)


#####################################################################
# Libs
#####################################################################

staticlib:
	make libsclean
	make all SMARTLINK=YES LIBNAME=fpc LIBTYPE=static

sharedlib:
	@echo Shared Libraries not supported for Go32v1

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
# Revision 1.2  1998-12-28 23:37:38  peter
#   * clean target fix, theres no objpas dir to clean anymore
#
# Revision 1.1  1998/12/21 13:07:02  peter
#   * use -FE
#
# Revision 1.9  1998/11/24 19:49:44  jonas
#   + added warning about TABs
#
# Revision 1.8  1998/10/12 08:36:29  pierre
#  * wrong 'objpas' target in all removed
#
# Revision 1.7  1998/10/11 13:45:04  michael
# + Added disk.inc to sysutils dependencies
#
# Revision 1.6  1998/10/11 12:21:47  michael
# + Further sysutils implementations.
#
# Revision 1.5  1998/10/06 22:10:29  peter
#   + heaptrc
#
# Revision 1.4  1998/10/02 09:25:59  peter
#   * fixed rtl path
#
# Revision 1.3  1998/09/16 16:47:26  peter
#   * merged fixes
#
# Revision 1.1.2.2  1998/09/16 16:17:49  peter
#   * updates to install with go32,win32
#
# Revision 1.2  1998/09/15 12:09:08  peter
#   * merged updates
#
# Revision 1.1.2.1  1998/09/15 12:02:02  peter
#   * updates to get objpas using its own makefile
#
