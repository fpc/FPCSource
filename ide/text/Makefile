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

# Add this dir also to the path as first
ifdef FULL
NEEDUNITDIR=../../compiler
NEEDOPT=-dBrowserCol
else
NEEDUNITDIR=../fake
endif


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=fp


#####################################################################
# Include default makefile
#####################################################################

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
	@echo
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@echo
	@exit
else
include $(FPCMAKE)
endif


#####################################################################
# Dependencies
#####################################################################

fp$(EXEEXT): $(wildcard *.pas) $(wildcard *.inc)

full:
	make all FULL=1

#
# $Log$
# Revision 1.3  1999-01-21 11:54:09  peter
#   + tools menu
#   + speedsearch in symbolbrowser
#   * working run command
#
# Revision 1.2  1999/01/19 18:21:49  peter
#   * Use FPCDIR and FPCMAKE environment to find makefile.fpc
#   * better install dir setting
#
#
