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

# Where is makefile.fpc by default
DEFAULTFPCDIR=../..

# Needed options, without it won't compile
NEEDOPT=-S2

# Linux only
# Define if libgds.a is not in /usr/lib 
# GDSLIBDIR=


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=ibase
EXEOBJECTS=testib

#####################################################################
# Common targets
#####################################################################

ifdef inlinux
ifdef GDSLIBDIR 
override NEEDLIBDIR+=$(GDSLIBDIR)
endif
endif

#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
        staticlib sharedlib libsclean \
        staticinstall sharedinstall libinstall \
        
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

testib$(EXEEXT): testib.pp ibase$(PPUEXT)

#
# $Log$
# Revision 1.2  1999-06-08 18:28:23  peter
#   * fixes for 0.99.12 release
#
# Revision 1.1  1999/05/12 00:11:27  michael
# initial import
#
# Revision 1.1  1999/03/16 00:50:29  peter
#   + init
#
#
