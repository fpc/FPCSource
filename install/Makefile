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

##########################################################################
# Config
##########################################################################

# Test dir if none specified
ifndef BASEINSTALLDIR
BASEINSTALLDIR=/pptest
endif

# TPC
TPC=otpc

# Use UPX ?
UPX=upx

# Zip
ZIP=zip -D9 -r


##########################################################################
# Defaults
##########################################################################

# Directory to the base of the CVS tree
CVSBASE=..
CFG=$(CVSBASE)/rtl/cfg

# Temporary path to pack a file
PACKDIR=$(subst \,/,$(TMP))/pp_tmp

# Use new ppc386
PPNEW=$(BASEDIR)/$(CVSBASE)/compiler/ppc386.exe


##########################################################################
# Include default makefile
##########################################################################

include $(CFG)/makefile.cfg

# Test dir if none specified
ifndef PACKAGEDIR
PACKAGEDIR=$(BASEDIR)
endif


##########################################################################
# Default target
##########################################################################

.PHONY: all basego32 basew32

all:
	@echo
	@echo Possible targets are:
	@echo
	@echo basego32, basego32.zip
	@echo basew32,	basew32.zip
	@echo
	@exit

clean:
	-$(DEL) *.tpu install.exe install.dat

##########################################################################
# Install
##########################################################################

install.exe: install.pas
	$(TPC) install.pas
ifdef UPX
	-$(UPX) install.exe
endif

installgo32: install.exe
	$(MKDIR) $(PACKAGEDIR)
	$(COPY) install.dos install.dat
	$(COPY) install.exe install.dat $(PACKAGEDIR)
	$(COPY) readme.txt whatsnew.txt $(PACKAGEDIR)

installw32: install.exe
	$(MKDIR) $(PACKAGEDIR)
	$(COPY) install.w32 install.dat
	$(COPY) install.exe install.dat $(PACKAGEDIR)
	$(COPY) readme.txt whatsnew.txt $(PACKAGEDIR)


##########################################################################
# Basego32.zip
##########################################################################

basego32:
# readme.txt & whatsnew.txt
	$(MKDIR) $(BASEINSTALLDIR)
	$(COPY) readme.txt whatsnew.txt $(BASEINSTALLDIR)
# docs
	$(MKDIR) $(DOCINSTALLDIR)
	$(COPY) doc/* $(DOCINSTALLDIR)
# bingo32 (cwsdpmi,wmemu387.dxe)
	$(MKDIR) $(BININSTALLDIR)
	$(COPY) bingo32/* $(BININSTALLDIR)
# compiler
	make -C $(CVSBASE)/compiler cycle OS_TARGET=go32v2 RELEASE=1
	make -C $(CVSBASE)/compiler install OS_TARGET=go32v2
# rtl go32v2
	make -C $(CVSBASE)/rtl/dos/go32v2 clean OS_TARGET=go32v2
	make -C $(CVSBASE)/rtl/dos/go32v2 install OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)
# rtl go32v2 libs
	make -C $(CVSBASE)/rtl/dos/go32v2 clean OS_TARGET=go32v2
	make -C $(CVSBASE)/rtl/dos/go32v2 libinstall OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)
# utils
	make -C $(CVSBASE)/rtl/utils clean OS_TARGET=go32v2
	make -C $(CVSBASE)/rtl/utils install OS_TARGET=go32v2 RELEASE=1 PP=$(PPNEW)
ifdef UPX
	-$(UPX) $(BININSTALLDIR)/*.exe
endif

basego32.zip:
	make basego32 BASEINSTALLDIR=$(PACKDIR)
	cd $(PACKDIR)
	$(ZIP) $(PACKAGEDIR)/basego32.zip *
	cd $(BASEDIR)
	$(DELTREE) $(PACKDIR)


##########################################################################
# basew32.zip
##########################################################################

basew32:
# readme.txt & whatsnew.txt
	$(MKDIR) $(BASEINSTALLDIR)
	$(COPY) readme.txt whatsnew.txt $(BASEINSTALLDIR)
# docs
	$(MKDIR) $(DOCINSTALLDIR)
	$(COPY) doc/* $(DOCINSTALLDIR)
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
	make -C $(CVSBASE)/rtl/utils clean OS_TARGET=win32
	make -C $(CVSBASE)/rtl/utils install OS_TARGET=win32 RELEASE=1 PP=$(PPNEW)

basew32.zip:
	make basew32 BASEINSTALLDIR=$(PACKDIR)
	$(MKDIR) $(PACKAGEDIR)
# Maybe you need 4dos for it, becuase unix paths with / are used
	cd $(PACKDIR)
	$(ZIP) $(PACKAGEDIR)/basew32.zip *
	cd $(BASEDIR)
	$(DELTREE) $(PACKDIR)


#
# $Log$
# Revision 1.1  1998-09-16 16:46:36  peter
#   + updates
#
#
