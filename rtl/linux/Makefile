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

# Check if we need C library.
ifeq ($(LINK_TO_C),YES)
override OPT+=-dCRTLIB
endif

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

# Define Linux Units
SYSTEMPPU=syslinux$(PPUEXT)
OBJECTS=strings linux \
	dos crt objects printer \
	getopts heaptrc errors sockets graph objpas sysutils

# Which units may be placed in the shared lib file
SHAREDLIBFILES=syslinux strings linux objpas sysutils math \
	dos crt objects printer \
	getopts errors sockets

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
PPUOBJECTS=$(SYSTEMPPU) $(addsuffix $(PPUEXT), $(OBJECTS))

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
	make -C $(OBJPASDIR) clean

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
	$(COMPILER) -Us -Sg syslinux.pp $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(COMPILER) strings $(REDIR)
	$(DEL) strings.pp

linux$(PPUEXT) : linux.pp strings$(PPUEXT) $(INC)/textrec.inc $(INC)/filerec.inc \
		 syscalls.inc systypes.inc sysconst.inc $(SYSTEMPPU)
	$(COMPILER) linux.pp $(REDIR)

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(INC)/except.inc
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(COMPILER) -S2 -I$(OBJPASDIR) objpas $(REDIR)
	$(DEL) objpas.pp

SYSUTILINC = $(wildcard $(OBJPASDIR)/*.inc)

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(SYSUTILINC) filutil.inc
	$(COPY) $(OBJPASDIR)/sysutils.pp .
	$(COMPILER) -S2 -I$(OBJPASDIR) sysutils $(REDIR)
	$(DEL) sysutils.pp

#
# System Dependent Units
#

sockets$(PPUEXT) : sockets.pp $(INC)/textrec.inc $(INC)/filerec.inc \
		   linux$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) sockets.pp $(REDIR)

errors$(PPUEXT) : errors.pp strings$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) errors.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : $(DOSDEPS) $(SYSTEMPPU)
	$(COMPILER) dos $(REDIR)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) crt $(REDIR)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
	$(COPY) $(INC)/objects.pp .
	$(COMPILER) objects $(REDIR)
	$(DEL) objects.pp

printer$(PPUEXT) : printer.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)
	$(COMPILER) printer $(REDIR)

graph$(PPUEXT) : graph.pp linux$(PPUEXT) objects$(PPUEXT)
	$(COMPILER) graph $(REDIR)

#
# Other RTL Units
#

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(INC)/getopts.pp .
	$(COMPILER) getopts $(REDIR)
	$(DEL) getopts.pp

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COPY) $(INC)/heaptrc.pp .
	$(COMPILER) heaptrc $(REDIR)
	$(DEL) heaptrc.pp


#####################################################################
# Libs
#####################################################################

libs: staticlib sharedlib

staticlib:
	make clean
	make all SMARTLINK=YES LIBNAME=fpc LIBTYPE=static

sharedlib:
	make clean
	make all
	$(PPUMOVE) -ofpc $(SHAREDLIBFILES)

staticlibinstall: staticlib
	$(MKDIR) $(STATIC_LIBINSTALLDIR)
	$(MKDIR) $(STATIC_UNITINSTALLDIR)
	$(INSTALLEXE) libfpc$(STATICLIBEXT) $(STATIC_LIBINSTALLDIR)
	$(INSTALLEXE) libobjpas$(STATICLIBEXT) $(STATIC_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) $(OBJLOADERS) $(STATIC_UNITINSTALLDIR)

sharedlibinstall: sharedlib
	$(MKDIR) $(SHARED_LIBINSTALLDIR)
	$(MKDIR) $(SHARED_UNITINSTALLDIR)
	$(INSTALLEXE) libfpc$(SHAREDLIBEXT) $(SHARED_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) $(OBJLOADERS) $(SHARED_UNITINSTALLDIR)
	$(LDCONFIG)

libinstall: staticlibinstall sharedlibinstall

libsclean : clean
	-$(DEL) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)

#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.6  1998-10-11 12:21:49  michael
# + Further sysutils implementations.
#
# Revision 1.5  1998/10/06 22:10:31  peter
#   + heaptrc
#
# Revision 1.4  1998/10/02 09:26:01  peter
#   * fixed rtl path
#
# Revision 1.3  1998/09/15 12:09:10  peter
#   * merged updates
#
# Revision 1.2  1998/09/11 13:14:01  michael
# many libs and libinstall fixes
#
# Revision 1.1.2.1  1998/09/15 12:02:04  peter
#   * updates to get objpas using its own makefile
#
# Revision 1.1  1998/09/10 14:15:49  peter
#   - renamed makefile to Makefile
#
# Revision 1.24  1998/09/10 09:10:05  michael
# errors when making libs
#
# Revision 1.23  1998/09/07 18:31:53  peter
#   * fixed $smartlibext -> $staticlibext to fix a 'rm *' :(
#
# Revision 1.22  1998/08/21 20:20:12  michael
# + Fixed spaces to tabs.
#
# Revision 1.21  1998/08/21 15:17:00  peter
#   * win32 compiles a bit better, no growheap crash
#
# Revision 1.20  1998/08/18 08:35:06  peter
#   * updated for new lib options
#
# Revision 1.19  1998/08/17 09:30:00  peter
#   * libtype/libname is by default off
#
# Revision 1.18  1998/08/16 10:22:57  michael
# Added library support, and smartlinking
#
# Revision 1.17  1998/08/15 17:08:09  peter
#   + support for cprt0.o
#
#
