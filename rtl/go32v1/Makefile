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
# Makefile Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=go32v1
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

# Define Go32v1 Units
SYSTEMUNIT=system

# Loaders
LOADEROBJECTS=prt0

# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) objpas strings \
	  go32 \
          dos crt objects printer \
	  sysutils math typinfo \
	  cpu mmx getopts heaptrc \
          msmouse

#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
	staticlib sharedlib libsclean \
	staticinstall sharedinstall libinstall \

all: testfpcmake fpc_all

clean: testfpcmake fpc_clean

install: testfpcmake fpc_install

info: testfpcmake fpc_info

staticlib: testfpcmake fpc_staticlib

sharedlib: testfpcmake fpc_sharedlib

libsclean: testfpcmake fpc_libsclean

staticinstall: testfpcmake fpc_staticinstall

sharedinstall: testfpcmake fpc_sharedinstall

libinstall: testfpcmake fpc_libinstall


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
testfpcmake:
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@exit
else
include $(FPCMAKE)
testfpcmake:
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

prt0$(OEXT) : prt0.as
	$(AS) -o prt0$(OEXT) prt0.as

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : system.pp $(SYSDEPS)
	$(COMPILER) -Us -Sg system.pp $(REDIR)

objpas$(PPUEXT): $(OBJPASDIR)/objpas.pp $(INC)/except.inc $(SYSTEMPPU)
	$(COMPILER) -I$(OBJPASDIR) $(OBJPASDIR)/objpas.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc \
		   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc \
		   $(SYSTEMPPU)

#
# System Dependent Units
#

go32$(PPUEXT) : go32.pp objpas$(PPUEXT) $(SYSTEMPPU)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
               go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

printer$(PPUEXT) : printer.pp $(SYSTEMPPU)

#
# Delphi Compatible Units
#

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(wildcard $(OBJPASDIR)/*.inc) \
		    filutil.inc disk.inc objpas$(PPUEXT) dos$(PPUEXT) go32$(PPUEXT)
	$(COMPILER) -I$(OBJPASDIR) $(OBJPASDIR)/sysutils.pp $(REDIR)

typinfo$(PPUEXT): $(OBJPASDIR)/typinfo.pp objpas$(PPUEXT)
	$(COMPILER) -Sg $(OBJPASDIR)/typinfo.pp $(REDIR)

math$(PPUEXT): $(OBJPASDIR)/math.pp objpas$(PPUEXT) sysutils$(PPUEXT)
	$(COMPILER) $(OBJPASDIR)/math.pp $(REDIR)

#
# Other system-independent RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COMPILER) -Sg $(INC)/heaptrc.pp $(REDIR)

#
# Other system-dependent RTL Units
#

msmouse$(PPUEXT) : msmouse.pp $(SYSTEMPPU)

#
# $Log$
# Revision 1.7  1999-06-01 13:23:08  peter
#   * fixes to work with the new makefile
#   * os2 compiles now correct under linux
#
# Revision 1.6  1999/04/08 12:22:52  peter
#   * removed os.inc
#
#
