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
OS_TARGET=go32v2
CPU=i386

# Where are the include files
RTL=../..
CFG=$(RTL)/cfg
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)
OBJPASDIR=$(RTL)/objpas
# Where are the .ppi files.
PPI=../ppi


#####################################################################
# Include default makefile
#####################################################################

include $(CFG)/makefile.cfg


#####################################################################
# System dependent
#####################################################################

# Define Go32v2 Units
SYSTEMPPU=system$(PPUEXT)

OBJECTS=strings go32 \
	dpmiexcp profile dxeload emu387 \
	dos crt objects printer \
	cpu mmx mouse getopts heaptrc graph objpas sysutils

LOADERS=prt0 exceptn fpu


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
	-make -C $(OBJPASDIR) clean


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

# new version need -Sv switch for external variables
dpmiexcp$(PPUEXT) : dpmiexcp.pp exceptn$(OEXT) $(SYSTEMPPU)
	$(COMPILER) -Sg -Sv dpmiexcp.pp $(REDIR)

profile$(PPUEXT) : profile.pp dpmiexcp$(PPUEXT) go32$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) profile.pp $(REDIR)

dxeload$(PPUEXT) : dxeload.pp $(SYSTEMPPU)
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

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COPY) $(INC)/heaptrc.pp .
	$(COMPILER) heaptrc $(REDIR)
	$(DEL) heaptrc.pp

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
	$(INSTALLEXE) *$(STATICLIBEXT) $(STATIC_LIBINSTALLDIR)
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
# Revision 1.10  1998-10-21 16:51:07  pierre
#  * dxeload range check problem solved
#
# Revision 1.9  1998/10/14 07:56:02  pierre
#  * -Sv now necessary for dpmiexcp unit
#
# Revision 1.8  1998/10/12 08:36:31  pierre
#  * wrong 'objpas' target in all removed
#
# Revision 1.7  1998/10/11 13:45:05  michael
# + Added disk.inc to sysutils dependencies
#
# Revision 1.6  1998/10/11 12:21:48  michael
# + Further sysutils implementations.
#
# Revision 1.5  1998/10/06 22:10:30  peter
#   + heaptrc
#
# Revision 1.4  1998/10/02 09:26:00  peter
#   * fixed rtl path
#
# Revision 1.3  1998/09/16 16:47:27  peter
#   * merged fixes
#
# Revision 1.1.2.2  1998/09/16 16:17:50  peter
#   * updates to install with go32,win32
#
# Revision 1.2  1998/09/15 12:09:09  peter
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
