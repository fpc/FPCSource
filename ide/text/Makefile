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
NEEDLIBDIR+=../../$(GDBINT)/libgdb
NEEDOBJDIR+=../../$(GDBINT)/libgdb
else
override NEEDUNITDIR+=../fake/gdb
endif

EXTRAINSTALL=myinstall


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=fp


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
nofpcmake:
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@exit
else
include $(FPCMAKE)
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

#
# Installation
#
myinstall:
	$(INSTALL) $(wildcard *.pt) $(wildcard *.tdf) $(BININSTALLDIR)

#
# Misc
#
clean_compiler:
	make -C ../../compiler clean

#
# $Log$
# Revision 1.13  1999-03-09 01:35:52  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
