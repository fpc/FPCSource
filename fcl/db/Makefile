#
#   $Id$
#   Copyright (c) 1999 by the Free Pascal Development Team
#
#   Makefile for database part of the Free Component Library 
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

# set target and cpu which are required
#override OS_TARGET=linux
override CPU=i386

# Include files
INC=../inc
XML=../xml
PROCINC=../$(CPU)

# default library name
LIBNAME=fpfcl

# Where to place the files
TARGETDIR=.

# We need always -S2
# add here the path to the mysql lib if it isn't in /usr/lib
# Also add the path to the mysql units if not in the compiler path.
NEEDOPT=-S2 -Fu../$(OS_TARGET)

# we need lib gcc
NEEDGCCLIB=yes

# As default make only the units
#DEFAULTUNITS=


#####################################################################
# Real targets
#####################################################################

# INCUNITS is defined in makefile.inc
# They are default units for all platforms.
include $(INC)/Makefile.inc
include $(XML)/Makefile.inc

UNITOBJECTS=db ddg_ds ddg_rec mysqldb
EXEOBJECTS=testds createds mtest


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

vpath %$(PASEXT) $(INC) $(XML)

INCFILES=

db$(PPUEXT): db.pp fields.inc dataset.inc dbs.inc

ddg_ds$(PPUEXT): db$(PPUEXT) ddg_rec$(PPUEXT) ddg_ds$(PASEXT)

testds$(EXEEXT): ddg_ds$(PPUEXT) testds$(PASEXT)

createds$(EXEEXT): createds$(PASEXT) ddg_rec$(PPUEXT)

mysqldb$(PPUEXT): db$(PPUEXT) mysqldb$(PASEXT)

mtest$(EXEEXT): mysqldb$(PPUEXT) mtest$(PASEXT)

#
# $Log$
# Revision 1.1  1999-10-24 16:15:38  michael
# + Initial read-only implementation
#
