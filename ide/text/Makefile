#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for Free Pascal Environment
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

# We always need the API and FV
NEEDUNITDIR=../../api ../../fv

# when making a full version include the compiler
ifeq ($(FULL),1)
override NEEDUNITDIR+=../../compiler
override NEEDOPT+=-dBrowserCol -dGDB -Sg
else
override NEEDUNITDIR+=../fake/compiler
endif

# when including debugger include the gdbinterface
ifndef GDBINT
GDBINT=gdbint
endif

ifeq ($(GDB),1)
override NEEDUNITDIR+=../../$(GDBINT)
NEEDLIBDIR+=../../$(GDBINT)/libgdb/$(OS_TARGET)
NEEDOBJDIR+=../../$(GDBINT)/libgdb/$(OS_TARGET)
else
override NEEDUNITDIR+=../fake/gdb
endif


# We need to link to libgcc
NEEDGCCLIB=1

#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=fp


#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
	staticlib sharedlib libsclean \
	staticinstall sharedinstall libinstall \

all: testfpcmake fpc_all

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

ifdef inlinux
LIBDIR=/usr/lib/gcc-lib/i486-linux/2.7.2.3
endif

fp$(EXEEXT): $(wildcard *.pas) $(wildcard *.inc)

gdb:
	make all GDB=1

full:
	make all FULL=1

fullgdb:
	make all FULL=1 GDB=1

# This is necessary because we don't have all units separate in the
# units targets
clean : testfpcmake fpc_cleanall

#
# Installation
#
install: fpc_install
	$(INSTALL) $(wildcard *.pt) $(wildcard *.tdf) readme.txt $(BININSTALLDIR)

#
# Misc
#
clean_compiler:
	make -C ../../compiler clean

#
# $Log$
# Revision 1.17  1999-10-29 14:33:03  pierre
#  + added readme.txt file
#
# Revision 1.16  1999/10/08 15:34:11  pierre
#  * adapted to target dependent libgdb dirs
#
# Revision 1.15  1999/07/06 15:49:54  peter
#   * need gcc lib
#
# Revision 1.14  1999/03/16 00:47:00  peter
#   * makefile.fpc targets start with fpc_
#   * small updates for install scripts
#
# Revision 1.13  1999/03/09 01:35:52  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
