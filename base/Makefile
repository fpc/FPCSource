#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for Free Pascal Source Tree
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#####################################################################
# Config
#####################################################################

MODULES=compiler rtl api fv gdb ide

ifndef APIDIR
APIDIR=api
endif

ifndef FVDIR
FVDIR=fv
endif

ifndef GDBDIR
GDBDIR=gdbint
endif

ifndef IDEDIR
IDEDIR=ide/text
endif


#####################################################################
# Defaults
#####################################################################

RELEASE=1
NODEFAULTRULES=1


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

.PHONY: all clean install staticinstall sharedinstall \
	$(addsuffix _all,$(MODULES)) \
	$(addsuffix _clean,$(MODULES)) \
	$(addsuffix _install,$(MODULES)) \
	$(addsuffix _staticinstall,$(MODULES)) \
	$(addsuffix _sharedinstall,$(MODULES))

info:
	@echo
	@echo Please use one of the following targets:
	@echo
	@echo $(MODULES)
	@echo
	@echo All targets can follow after a _ with:
	@echo all,clean,install,staticinstall,sharedinstall
	@echo
	@echo example: make api_staticinstall
	@exit

#######################################
# Compiler
#######################################

compiler_all:
	$(MAKE) -C compiler cycle

compiler_clean:
	$(MAKE) -C compiler clean

compiler_install:
	$(MAKE) -C compiler install

compiler_staticinstall:

compiler_sharedinstall:


#######################################
# RTL
#######################################

rtl_all:
	$(MAKE) -C rtl/$(OS_TARGET) all

rtl_clean:
	$(MAKE) -C rtl/$(OS_TARGET) clean

rtl_install:
	$(MAKE) -C rtl/$(OS_TARGET) install

rtl_staticinstall:
	$(MAKE) -C rtl/$(OS_TARGET) staticlibinstall

rtl_sharedinstall:
	$(MAKE) -C rtl/$(OS_TARGET) sharedlibinstall


#######################################
# API
#######################################

api_all: rtl_all
	$(MAKE) -C $(APIDIR) all

api_clean:
	$(MAKE) -C $(APIDIR) clean

api_install:
	$(MAKE) -C $(APIDIR) install

api_staticinstall:
	$(MAKE) -C $(APIDIR) staticlibinstall

api_sharedinstall:
	$(MAKE) -C $(APIDIR) sharedlibinstall


#######################################
# FV
#######################################

fv_all: rtl_all api_all
	$(MAKE) -C $(FVDIR) all

fv_clean:
	$(MAKE) -C $(FVDIR) clean

fv_install:
	$(MAKE) -C $(FVDIR) install

fv_staticinstall:
	$(MAKE) -C $(FVDIR) staticlibinstall

fv_sharedinstall:
	$(MAKE) -C $(FVDIR) sharedlibinstall


#######################################
# GDB
#######################################

gdb_all: rtl_all
	$(MAKE) -C $(GDBDIR) all

gdb_clean:
	$(MAKE) -C $(GDBDIR) clean

gdb_install:
	$(MAKE) -C $(GDBDIR) install

gdb_staticinstall:
	$(MAKE) -C $(GDBDIR) staticlibinstall

gdb_sharedinstall:
	$(MAKE) -C $(GDBDIR) sharedlibinstall



#######################################
# IDE
#######################################

ide_all: rtl_all api_all fv_all gdb_all
	$(MAKE) -C $(IDEDIR) fullgdb

ide_clean:
	$(MAKE) -C $(IDEDIR) clean

ide_install:
	$(MAKE) -C $(IDEDIR) install

ide_staticinstall:
	$(MAKE) -C $(IDEDIR) staticlibinstall

ide_sharedinstall:
	$(MAKE) -C $(IDEDIR) sharedlibinstall


#######################################
# Common targets all,install,clean
#######################################

all: $(addsuffix _all,$(MODULES))

clean: $(addsuffix _clean,$(MODULES))

install: $(addsuffix _install,$(MODULES))

staticinstall: $(addsuffix _staticinstall,$(MODULES))

sharedinstall: $(addsuffix _sharedinstall,$(MODULES))


#######################################
# Defaults
#######################################

#
# $Log$
# Revision 1.2  1999-01-28 19:58:19  peter
#   * makefile updates
#
# Revision 1.1  1999/01/21 11:48:39  peter
#   * initial version
#
#
