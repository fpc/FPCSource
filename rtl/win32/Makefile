#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Win32 Runtime Library
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
# Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=win32
override CPU=i386

# Where are the include files
RTL=..
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)
WININC=wininc

# Unit dir
UNITDIR=.

# Needed options
ifneq ("$(FPC_VERSION)","0.99.10")
ifndef BROWSER
NEEDOPT=-b-
endif
endif

# Where to place the result files
TARGETDIR=.

# These units belong to the RTL
UNITPREFIX=rtl


#####################################################################
# Own defaults
#####################################################################

# Paths
OBJPASDIR=$(RTL)/objpas

# Define Win32 Units
SYSTEMUNIT=syswin32

# Loaders
LOADEROBJECTS=wprt0 wdllprt0

# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) objpas strings \
	    windows ole2 opengl32 winsock \
            sockets initc \
	    dos crt objects \
	    sysutils typinfo math \
	    cpu mmx getopts heaptrc

# Files used by windows.pp
include $(WININC)/makefile.inc

WINDOWS_SOURCE_FILES=$(addprefix $(WININC)/,$(addsuffix .inc,$(WINDOWS_FILES)))


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

override AS=asw

wprt0$(OEXT) : wprt0.as
	$(AS) -o wprt0$(OEXT) wprt0.as

wdllprt0$(OEXT) : wdllprt0.as
	$(AS) -o wdllprt0$(OEXT) wdllprt0.as


#
# System Units (System, Objpas, Strings)
#

$(SYSTEMPPU) : syswin32.pp win32.inc $(SYSDEPS)
	$(COMPILER) -Us -Sg syswin32.pp $(REDIR)

objpas$(PPUEXT): $(OBJPASDIR)/objpas.pp $(INC)/except.inc $(SYSTEMPPU)
	$(COMPILER) -I$(OBJPASDIR) $(OBJPASDIR)/objpas.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc\
		   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc\
		   $(SYSTEMPPU)

#
# System Dependent Units
#

windows$(PPUEXT) : windows.pp $(WINDOWS_SOURCE_FILES) $(SYSTEMPPU)
	$(COMPILER) -I$(WININC) windows.pp $(REDIR)

ole2$(PPUEXT) : ole2.pp windows$(PPUEXT) $(SYSTEMPPU)

opengl32$(PPUEXT) : opengl32.pp windows$(PPUEXT) $(SYSTEMPPU)

winsock$(PPUEXT) : winsock.pp windows$(PPUEXT) $(SYSTEMPPU)

sockets$(PPUEXT) : sockets.pp windows$(PPUEXT) winsock$(PPUEXT) $(SYSTEMPPU) \
		   $(INC)/sockets.inc $(INC)/socketsh.inc

initc$(PPUEXT) : initc.pp $(SYSTEMPPU)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT) $(SYSTEMPPU)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc $(SYSTEMPPU) objpas$(PPUEXT) dos$(PPUEXT) windows$(PPUEXT)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

#
# Delphi Compatible Units
#

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(wildcard $(OBJPASDIR)/*.inc) \
		    filutil.inc disk.inc objpas$(PPUEXT) dos$(PPUEXT) windows$(PPUEXT)
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
# Revision 1.37  1999-09-28 22:53:27  pierre
#  + InitC unit
#
# Revision 1.36  1999/09/16 13:38:07  peter
#   * windows unit include moved to wininc/
#
#
