#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Installation Helpers
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

# Test dir if none specified
ifndef BASEINSTALLDIR
BASEINSTALLDIR=/pptest
endif


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=


#####################################################################
# Common targets
#####################################################################

all:
	@echo
	@echo Possible targets are:
	@echo
	@echo basego32, basego32.zip
	@echo basew32,	basew32.zip
	@echo
	@exit


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

# Directory to the base of the CVS tree
CVSBASE=..
CFG=$(CVSBASE)/rtl/cfg

# Temporary path to pack a file
PACKDIR=$(subst \,/,$(TMP))/pp_tmp

# Use new ppc386
PPNEW=$(BASEDIR)/$(CVSBASE)/compiler/ppc386.exe

# Test dir if none specified
ifndef PACKAGEDIR
PACKAGEDIR=$(BASEDIR)
endif


##########################################################################
# Default target
##########################################################################

.PHONY: all clean basego32 basew32

clean:
	-$(DEL) *.tpu install.exe install.dat

##########################################################################
# Install
##########################################################################

installer:
	$(MAKE) -C $(CVSBASE)/rtl/go32v2 all RELEASE=1
	$(MAKE) -C $(CVSBASE)/api all RELEASE=1
	$(MAKE) -C $(CVSBASE)/fv all RELEASE=1
	$(MAKE) -C fpinst all RELEASE=1


##########################################################################
# Basego32.zip
##########################################################################

basego32:
# create dirs
	$(MKDIR) $(BASEINSTALLDIR)
	$(MKDIR) $(DOCINSTALLDIR)
	$(MKDIR) $(BININSTALLDIR)
	$(MKDIR) $(SOURCEINSTALLDIR)
# readme & whatsnew and docs
	$(COPY) doc/*.txt doc/COPYING* $(DOCINSTALLDIR)
# bingo32 (cwsdpmi,wmemu387.dxe)
	$(COPY) bingo32/* $(BININSTALLDIR)
# source (base)
	$(COPY) $(CVSBASE)/base/Makefile $(CVSBASE)/base/makefile.fpc $(SOURCEINSTALLDIR)
# compiler
	make -C $(CVSBASE)/compiler cycle OS_TARGET=go32v2 RELEASE=1
	make -C $(CVSBASE)/compiler install OS_TARGET=go32v2
# rtl go32v2
	make -C $(CVSBASE)/rtl/go32v2 clean OS_TARGET=go32v2
	make -C $(CVSBASE)/rtl/go32v2 install OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)
# rtl go32v2 libs
	make -C $(CVSBASE)/rtl/go32v2 clean OS_TARGET=go32v2
	make -C $(CVSBASE)/rtl/go32v2 libinstall OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)
# utils
	make -C $(CVSBASE)/utils clean OS_TARGET=go32v2
	make -C $(CVSBASE)/utils install OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)

basego32.zip:
	make basego32 BASEINSTALLDIR=$(PACKDIR)
	cd $(PACKDIR) ; $(ZIPPROG) $(PACKAGEDIR)/basego32.zip * ; cd $(BASEDIR)
	$(DELTREE) $(PACKDIR)


##########################################################################
# basew32.zip
##########################################################################

basew32:
# create dirs
	$(MKDIR) $(BASEINSTALLDIR)
	$(MKDIR) $(DOCINSTALLDIR)
	$(MKDIR) $(BININSTALLDIR)
	$(MKDIR) $(SOURCEINSTALLDIR)
# readme & whatsnew and docs
	$(COPY) doc/*.txt doc/COPYING* $(DOCINSTALLDIR)
# source (base)
	$(COPY) $(CVSBASE)/base/Makefile $(CVSBASE)/base/makefile.fpc $(SOURCEINSTALLDIR)
# readme.txt & whatsnew.txt
	$(MKDIR) $(BASEINSTALLDIR)
	$(COPY) readme.txt whatsnew.txt $(BASEINSTALLDIR)
# compiler, the - is necessary because the files sometimes differ
	-make -C $(CVSBASE)/compiler cycle OS_TARGET=win32 RELEASE=1
	make -C $(CVSBASE)/compiler install OS_TARGET=win32
# rtl
	make -C $(CVSBASE)/rtl/win32 clean OS_TARGET=win32
	make -C $(CVSBASE)/rtl/win32 install OS_TARGET=win32 RELEASE=1 PP=$(PPNEW)
# rtl libs
	make -C $(CVSBASE)/rtl/win32 clean OS_TARGET=win32
	make -C $(CVSBASE)/rtl/win32 libinstall OS_TARGET=win32 RELEASE=1 PP=$(PPNEW)
# utils
	make -C $(CVSBASE)/utils clean OS_TARGET=win32
	make -C $(CVSBASE)/utils install OS_TARGET=win32 RELEASE=1 PP=$(PPNEW)

basew32.zip:
	make basew32 BASEINSTALLDIR=$(PACKDIR)
	$(MKDIR) $(PACKAGEDIR)
# Maybe you need 4dos for it, becuase unix paths with / are used
	cd $(PACKDIR)
	$(ZIPPROG) $(PACKAGEDIR)/basew32.zip *
	cd $(BASEDIR)
	$(DELTREE) $(PACKDIR)


##########################################################################
# Baseemx.zip
##########################################################################

baseemx:
# create dirs
	$(MKDIR) $(BASEINSTALLDIR)
	$(MKDIR) $(DOCINSTALLDIR)
	$(MKDIR) $(BININSTALLDIR)
	$(MKDIR) $(SOURCEINSTALLDIR)
# readme & whatsnew and docs
	$(COPY) doc/*.txt doc/COPYING* $(DOCINSTALLDIR)
# source (base)
	$(COPY) $(CVSBASE)/base/Makefile $(CVSBASE)/base/makefile.fpc $(SOURCEINSTALLDIR)
# compiler
	make -C $(CVSBASE)/compiler cycle OS_TARGET=os2 RELEASE=1
	make -C $(CVSBASE)/compiler install OS_TARGET=os2
# rtl os2
	make -C $(CVSBASE)/rtl/go32v2 clean OS_TARGET=os2
	make -C $(CVSBASE)/rtl/go32v2 install OS_TARGET=os2 RELEASE=1 PP=$(PPNEW)
# rtl go32v2 libs
#	make -C $(CVSBASE)/rtl/go32v2 clean OS_TARGET=go32v2
#	make -C $(CVSBASE)/rtl/go32v2 libinstall OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)
# utils
	make -C $(CVSBASE)/utils clean OS_TARGET=os2
	make -C $(CVSBASE)/utils install OS_TARGET=os2 RELEASE=1 PP=$(PPNEW)

baseemx.zip:
	make baseemx BASEINSTALLDIR=$(PACKDIR)
	cd $(PACKDIR) ; $(ZIPPROG) $(PACKAGEDIR)/baseemx.zip * ; cd $(BASEDIR)
	$(DELTREE) $(PACKDIR)

#
# $Log$
# Revision 1.5  1999-06-11 13:58:35  peter
#   * moved .txt to doc/
#
# Revision 1.4  1999/06/11 13:03:51  peter
#   * os2 update, readme,whatsnew in doc/ dir
# C
#
# Revision 1.3  1999/05/30 11:34:33  peter
#   * updates to generate the installer
#
# Revision 1.2	1999/05/03 18:04:37  peter
#   * updates
#
# Revision 1.1	1998/09/16 16:46:36  peter
#   + updates
#
#
