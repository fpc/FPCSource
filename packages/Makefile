#
#   $Id$
#   Copyright (c) 1999 by the Free Pascal Development Team
#
#   Makefile for <template> for Free Pascal
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


# Where are the include files located
INC=
PROCINC=
OSINC=

# Needed options, without it won't compile
NEEDOPT=

# Needed unit dir, which is searched as the first path
NEEDUNITDIR=

# Where need we to place the executables/ppu/objects
TARGETDIR=
UNITTARGETDIR=

# As default make only the units
#DEFAULTUNITS=1


#####################################################################
# Real targets
#####################################################################

all: alldirs;
clean: cleanalldirs
install: installalldirs

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
# Targets
#####################################################################
# Construct real targets

.PHONY: all clean install info alldirs\
        staticlib sharedlib libsclean \
        staticinstall sharedinstall libinstall


clean: testfpcmake fpc_clean

install: testfpcmake fpc_install

info: testfpcmake fpc_info

staticlib: testfpcmake fpc_staticlib

sharedlib: testfpcmake fpc_sharedlib

libsclean: testfpcmake fpc_libsclean

staticinstall: testfpcmake fpc_staticinstall

sharedinstall: testfpcmake fpc_sharedinstall

libinstall: testfpcmake fpc_libinstall


alldirs:
	$(MAKE) -C ibase
	$(MAKE) -C mysql
	$(MAKE) -C uncgi
ifdef inlinux
	$(MAKE) -C utmp
	$(MAKE) -C inet
	$(MAKE) -C postgres
	$(MAKE) -C ncurses
	$(MAKE) -C forms
	$(MAKE) -C svgalib
endif

cleanalldirs:

	$(MAKE) -C ibase clean
	$(MAKE) -C mysql clean
	$(MAKE) -C uncgi clean
ifdef inlinux
	$(MAKE) -C utmp clean
	$(MAKE) -C inet clean
	$(MAKE) -C postgres clean
	$(MAKE) -C ncurses clean
	$(MAKE) -C forms clean
	$(MAKE) -C svgalib clean
endif

installalldirs:
	$(MAKE) -C ibase DEFAULTUNITS=1 install
	$(MAKE) -C mysql DEFAULTUNITS=1 install
	$(MAKE) -C uncgi DEFAULTUNITS=1 install
ifdef inlinux
	$(MAKE) -C utmp DEFAULTUNITS=1 install
	$(MAKE) -C inet DEFAULTUNITS=1 install
	$(MAKE) -C postgres DEFAULTUNITS=1 install
	$(MAKE) -C ncurses DEFAULTUNITS=1 install
	$(MAKE) -C forms DEFAULTUNITS=1 install
	$(MAKE) -C svgalib DEFAULTUNITS=1 install
endif

#
# $Log$
# Revision 1.1  1999-05-12 00:11:27  michael
# initial import
#
# Revision 1.1  1999/03/16 00:50:29  peter
#   + init
#
#
