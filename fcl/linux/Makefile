#
#   $Id$
#   Copyright (c) 1999 by the Free Pascal Development Team
#
#   Makefile for Free Component Library for Linux
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#


#####################################################################
# Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=linux
override CPU=i386

# Include files
INC=../inc
PROCINC=../$(CPU)

# Where to place the files
TARGETDIR=.

# We need always -S2
NEEDOPT=-S2

# As default make only the units
DEFAULTUNITS=1


#####################################################################
# Real targets
#####################################################################

# INCUNITS is defined in makefile.inc
# They are default units for all platforms.
include $(INC)/Makefile.inc

UNITOBJECTS=$(INCUNITS)
EXEOBJECTS=


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
# Dependencies
#####################################################################

vpath %$(PASEXT) $(INC)

INCFILES=$(addprefix $(INC)/,$(INCNAMES))

classes$(PPUEXT): $(INCFILES) classes$(PASEXT)

inifiles$(PPUEXT): classes$(PPUEXT) inifiles$(PASEXT)

ezcgi$(PPUEXT): ezcgi$(PASEXT) ezcgi.inc

#
# $Log$
# Revision 1.10  1999-04-27 19:13:44  michael
# + Added ezcgi from Michael Hess
#
# Revision 1.9  1999/04/22 10:50:45  peter
#   * set target always
#
# Revision 1.8	1999/04/08 15:58:44  michael
# + INCUNITS added
#
# Revision 1.7	1999/04/08 10:18:59  peter
#   * makefile updates
#
#
