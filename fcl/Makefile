#
#   $Id$
#   Copyright (c) 1999 by the Free Pascal Development Team
#
#   Makefile for Free Component Library
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
# Common targets
#####################################################################

.PHONY: all clean install info \
        staticlib sharedlib libsclean \
        staticinstall sharedinstall libinstall \

all: testfpcmake
	$(MAKE) -C $(OS_TARGET) all
        
clean: testfpcmake
	$(MAKE) -C go32v2 clean
	$(MAKE) -C linux clean
	$(MAKE) -C win32 clean
	$(MAKE) -C os2 clean
	$(MAKE) -C tests clean

install: testfpcmake
	$(MAKE) -C $(OS_TARGET) install

info: testfpcmake fpc_info

staticlib: testfpcmake
	$(MAKE) -C $(OS_TARGET) staticlib

sharedlib: testfpcmake
	$(MAKE) -C $(OS_TARGET) sharedlib

libsclean: testfpcmake
	$(MAKE) -C $(OS_TARGET) libsclean

staticinstall: testfpcmake
	$(MAKE) -C $(OS_TARGET) staticinstall

sharedinstall: testfpcmake
	$(MAKE) -C $(OS_TARGET) sharedinstall

libinstall: testfpcmake
	$(MAKE) -C $(OS_TARGET) libinstall


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

.PHONY: tests examples go32v2 linux os win32

#
# Examples
#
examples: tests

tests: all
	$(MAKE) -C tests all


#
# Specific OS
#
go32v2:
	$(MAKE) -C go32v2

linux:
	$(MAKE) -C linux

os2:
	$(MAKE) -C os2

win32:
	$(MAKE) -C win32

#
# $Log$
# Revision 1.3  1999-04-08 10:18:47  peter
#   * makefile updates
#
#
