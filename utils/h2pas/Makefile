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
DEFAULTFPCDIR=../..

# We need -Sg
NEEDOPT=-Sg


#####################################################################
# Real targets
#####################################################################

# the files in this dir use .pas
PASEXT=.pas

EXEOBJECTS=h2pas
UNITOBJECTS=

EXTRACLEANUNITS=options lexlib scan yacclib converu


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

# do we have pyacc?
ifndef PYACC
PYACC=$(strip $(wildcard $(addsuffix /pyacc$(EXEEXT),$(SEARCHPATH))))
ifeq ($(PYACC),)
PYACC=
else
export PYACC:=$(firstword $(PYACC))
endif
endif

# do we have plex?
ifndef PLEX
PLEX=$(strip $(wildcard $(addsuffix /plex$(EXEEXT),$(SEARCHPATH))))
ifeq ($(PLEX),)
PLEX=
else
export PLEX:=$(firstword $(PLEX))
endif
endif

h2pas$(EXEEXT): h2pas$(PASEXT)

#
# Lex and Yacc (only if pyacc is found)
#
ifdef  PYACC
h2pas$(PASEXT): h2pas.y
	$(PYACC) h2pas.y

yacclex: yacc lex

yacc:
	$(PYACC) h2pas.y

lex :
	$(PLEX) scan.l
endif

#
# $Log$
# Revision 1.3  1999-07-16 13:45:38  peter
#   * 0.99.12b updates
#   * merges
#
# Revision 1.2  1999/06/18 11:03:14  peter
#   * merged
#
# Revision 1.1.2.2  1999/07/16 13:40:59  peter
#   + extrainstallunits,extracleanunits
#
# Revision 1.1.2.1  1999/06/18 10:55:37  peter
#   * version fixes
#   * EXTRAUNITS to set extra units that are build and needs to be cleaned
#
# Revision 1.1  1999/05/12 16:11:39  peter
#   * moved
#
# Revision 1.12  1999/04/01 22:52:01  peter
#   * update for makefile.fpc
#
# Revision 1.1  1999/03/16 00:50:29  peter
#   + init
#
#
