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
# Makefile Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=os2
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

# Define os2 system Unit name
SYSTEMUNIT=sysos2

# Loaders
LOADEROBJECTS=prt0 prt1 code2 code3

# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) objpas strings \
	  os2def doscalls moncalls kbdcalls viocalls \
	  dos crt objects printer \
	  objpas sysutils math typinfo \
	  cpu mmx getopts heaptrc \


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
# Include default makef
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

vpath %.pas $(INC) $(PROCINC)
vpath %.pp $(INC) $(PROCINC)

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

objpas$(PPUEXT): $(OBJPASDIR)/objpas.pp $(INC)/except.inc $(SYSTEMPPU)
	$(COMPILER) -I$(OBJPASDIR) $(OBJPASDIR)/objpas.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc \
                   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc \
                   $(SYSTEMPPU)

#
# System Dependent Units
#

doscalls$(PPUEXT) : doscalls.pas strings$(PPUEXT) objects$(PPUEXT) $(SYSTEMPPU)

kbdcalls$(PPUEXT) : kbdcalls.pas $(SYSTEMPPU)

moncalls$(PPUEXT) : moncalls.pas strings$(PPUEXT) $(SYSTEMPPU)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pas $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT) \
	       $(SYSTEMPPU)

crt$(PPUEXT) : crt.pas $(INC)/textrec.inc $(SYSTEMPPU)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

printer$(PPUEXT) : printer.pas $(INC)/textrec.inc $(SYSTEMPPU)

#graph$(PPUEXT) : graph.pp

#
# Delphi Compatible Units
#

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(wildcard $(OBJPASDIR)/*.inc) \
                    filutil.inc disk.inc objpas$(PPUEXT)
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

#
# $Log$
# Revision 1.7  1999-06-01 13:23:14  peter
#   * fixes to work with the new makefile
#   * os2 compiles now correct under linux
#
#
