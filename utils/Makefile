#
#   $Id$
#   Copyright (c) 1999 by the Free Pascal Development Team
#
#   Makefile for Free Pascal Utils
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

EXEOBJECTS=ppudump ppumove ppdep ptop
UNITOBJECTS=

EXTRACLEANUNITS=ppu ptopu

#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
        staticlib sharedlib libsclean \
        staticinstall sharedinstall libinstall \
        extra_clean \
        h2pas_all h2pas_clean h2pas_install \
        
all: testfpcmake fpc_all h2pas_all

clean: testfpcmake fpc_clean h2pas_clean

install: testfpcmake fpc_install h2pas_install

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

#
# PPU Tools
#
ppu$(PPUEXT): ppu.pas

ppudump$(EXEEXT): ppudump.pp ppu$(PPUEXT)

ppumove$(EXEEXT): ppumove.pp ppu$(PPUEXT)

#
# Pascal beautifier
#
ptop$(EXEEXT): ptop.pp ptopu$(PPUEXT)

ptopu$(PPUEXT): ptopu.pp

#
# H2Pas
#
h2pas_all:
	$(MAKE) -C h2pas all

h2pas_clean:
	$(MAKE) -C h2pas clean

h2pas_install:
	$(MAKE) -C h2pas install

#
# $Log$
# Revision 1.1.2.2  1999-07-16 13:40:58  peter
#   + extrainstallunits,extracleanunits
#
# Revision 1.1.2.1  1999/06/18 10:55:36  peter
#   * version fixes
#   * EXTRAUNITS to set extra units that are build and needs to be cleaned
#
# Revision 1.1  1999/05/12 16:11:39  peter
#   * moved
#
# Revision 1.14  1999/05/03 18:03:13  peter
#   * renamed mkdep -> ppdep
#   * removed obsolete units
#   * add .cod files
#
# Revision 1.13  1999/04/02 00:01:49  peter
#   + ptop to exeobjects
#
# Revision 1.12  1999/04/01 22:52:00  peter
#   * update for makefile.fpc
#
# Revision 1.1  1999/03/16 00:50:29  peter
#   + init
#
#
