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

# Where need we to place the executables/ppu/objects
TARGETDIR=.

# Needed units
NEEDUNITDIR=../$(OS_TARGET)

# We need always -S2
NEEDOPT=-S2


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=stringl dparser fstream mstream list threads testrtf\
           cfgtest testz testz2 xmldump htdump


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
	@exit 1
else
include $(FPCMAKE)
testfpcmake:
endif



#####################################################################
# Dependencies
#####################################################################

#
# $Log$
# Revision 1.7  1999-07-11 22:43:23  michael
# + Added htdump
#
# Revision 1.6  1999/07/09 21:08:16  michael
# + Added ztsream and xml demos
#
# Revision 1.5  1999/05/30 10:46:43  peter
#   * start of tthread for linux,win32
#
# Revision 1.4  1999/04/08 10:19:04  peter
#   * makefile updates
#
#
