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

# Version of forms
FORMSVERSION=88
#FORMSVERSION=86

# Where are the include files located
INC=
PROCINC=
OSINC=

# Needed options, without it won't compile
NEEDOPT=-S2

# Needed unit dir, which is searched as the first path
NEEDUNITDIR=

# Define if forms library is not in /usr/lib 
# FORMSLIBDIR=

# Define if X library is not in /usr/lib 
# XLIBDIR=

# Where need we to place the executables/ppu/objects
TARGETDIR=
UNITTARGETDIR=

# As default make only the units
#DEFAULTUNITS=1

# Uncomment this is the makefile.fpc isn't located in ../..
DEFAULTFPCDIR=../..

#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=x xresource xlib xutil forms
EXEOBJECTS=fd2pascal

#####################################################################
# Common targets
#####################################################################

ifdef FORMSLLIBDIR 
override NEEDLIBDIR+=$(FORMSLIBDIR)
endif
ifdef XLIBDIR 
override NEEDLIBDIR+=$(XLIBDIR)
endif

#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
        staticlib sharedlib libsclean \
        staticinstall sharedinstall libinstall demo\
        
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

demo:
	$(MAKE) -C demo

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

forms$(PPUEXT): forms-$(FORMSVERSION)$(PASEXT) x$(PPUEXT) xresource$(PPUEXT) xlib$(PPUEXT)\
              xutil$(PPUEXT)
	ln -sf forms-$(FORMSVERSION)$(PASEXT) forms$(PASEXT)
	$(COMPILER) forms$(PASEXT)

#
# $Log$
# Revision 1.1  1999-05-12 00:11:25  michael
# initial import
#
# Revision 1.1  1999/03/16 00:50:29  peter
#   + init
#
#
