#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for Free Pascal Demos
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
DEFAULTFPCDIR=..


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=eratos qsort hello blackbox magic lines fpctris

# Win32 demo's
ifeq ($(OS_TARGET),win32)
override EXEOBJECTS+=winhello menu
endif

# Go32v2 demo's
ifeq ($(OS_TARGET),go32v2)
override EXEOBJECTS+=mandel samegame
endif

# Linux demo's
ifeq ($(OS_TARGET),linux)
override EXEOBJECTS+=mandel samegame
endif


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

ifeq ($(OS_TARGET),win32)
vpath %$(PASEXT) win32
endif

#################################
# Demo installation for linux 
#

DEMOINSTALLDIR=$(DOCINSTALLDIR)/demo

installdemo:
	$(MKDIR) $(DEMOINSTALLDIR)
	$(COPY) -rf * $(DEMOINSTALLDIR)

#
# $Log$
# Revision 1.6  1999-06-11 12:51:28  peter
#   * updated for linux
#
# Revision 1.5  1999/06/08 18:28:21  peter
#   * fixes for 0.99.12 release
#
# Revision 1.4  1999/05/27 21:36:32  peter
#   * new demo's
#   * fixed mandel for linux
#
# Revision 1.3  1999/05/03 18:04:38  peter
#   * updates
#
#
