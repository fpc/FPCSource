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

# Warning: this file contains TAB (#9) characters that are required for
# make. Make sure you use an editor that does not replace TABs with
# spaces, or the makefile won't work anymore after you save.

#####################################################################
# Makefile Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=linux
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

# Define Go32v2 Units
SYSTEMUNIT=syslinux


# Define Loaders
ifdef AOUT
PRT=prt0
else
PRT=prt1
endif

LOADERAS=$(CPU)/$(PRT).as
CLOADERAS=$(CPU)/c$(PRT).as
GLOADERAS=$(CPU)/g$(PRT).as

LOADEROBJECTS=prt0 cprt0 gprt0

# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) strings linux \
            dos crt objects printer ports \
            objpas sysutils typinfo math \
            cpu mmx getopts heaptrc \
            errors sockets graph


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

prt0$(OEXT) : $(LOADERAS)
	$(AS) -o prt0$(OEXT) $(LOADERAS)

cprt0$(OEXT) : $(CLOADERAS)
	$(AS) -o cprt0$(OEXT) $(CLOADERAS)

gprt0$(OEXT) : $(GLOADERAS)
	$(AS) -o gprt0$(OEXT) $(GLOADERAS)

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syslinux.pp sysconst.inc systypes.inc syscalls.inc $(SYSDEPS)
	$(COMPILER) -Us -Sg syslinux.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc\
                   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc\
                   $(SYSTEMPPU)

linux$(PPUEXT) : linux.pp strings$(PPUEXT) $(INC)/textrec.inc $(INC)/filerec.inc \
                 syscalls.inc systypes.inc sysconst.inc $(SYSTEMPPU)

#
# Delphi Object Model
#

include $(OBJPASDIR)/makefile.op

#
# System Dependent Units
#

sockets$(PPUEXT) : sockets.pp $(INC)/textrec.inc $(INC)/filerec.inc \
                   linux$(PPUEXT) $(SYSTEMPPU)

errors$(PPUEXT) : errors.pp strings$(PPUEXT) $(SYSTEMPPU)

ipc$(PPUEXT) : ipc.pp linux$(PPUEXT) $(SYSTEMPPU)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT) \
               linux$(PPUEXT) $(SYSTEMPPU)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

printer$(PPUEXT) : printer.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)

ports$(PPUEXT) : ports.pp linux$(PPUEXT) objpas$(PPUEXT)

graph$(PPUEXT) : graph.pp linux$(PPUEXT) objects$(PPUEXT)

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)

#######################################################################
# clean
#######################################################################

clean: fpc_clean

libsclean: fpc_libsclean


#
# $Log$
# Revision 1.19  1999-04-14 09:07:17  peter
#   * fixed strings dependency
#
# Revision 1.18  1999/03/22 13:09:10  peter
#   - ipc unit because it crashes on 0.99.10
#
# Revision 1.17  1999/03/16 00:47:09  peter
#   * makefile.fpc targets start with fpc_
#   * small updates for install scripts
#
# Revision 1.16  1999/03/12 21:07:51  michael
# +  clean and libsclean added
#
# Revision 1.15  1999/03/09 01:35:55  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
