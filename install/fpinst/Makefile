#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for Free Pascal Installer
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

NEEDUNITDIR=../../api ../../fv


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=ziptypes unzip

# this hack is needed to solve dup install target
ifdef linux
EXEOBJECTS=installer
else
EXEOBJECTS=install
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

ifdef linux
installer: install.pas
	$(COMPILER) install.pas
endif

#
# $Log$
# Revision 1.5  1999-06-29 22:20:18  peter
#   * updated to use tab pages
#
# Revision 1.4  1999/05/30 11:34:34  peter
#   * updates to generate the installer
#
# Revision 1.3	1999/05/02 14:27:39  peter
#   * update to new standards
#
# Revision 1.2	1999/03/09 01:35:53  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
