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
DEFAULTFPCDIR=..

NEEDUNITDIR=../api ../fv


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=ziptypes unzip
EXEOBJECTS=install

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

#
# $Log$
# Revision 1.2  1999-03-09 01:35:53  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
