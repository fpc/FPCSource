#
#   $Id$
#   Copyright (c) 1999 by the Free Pascal Development Team
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

ifndef FPINSTDIR
FPINSTDIR=fpinst
endif

ifndef IDEDIR
IDEDIR=ide/text
endif

#####################################################################
# Defaults
#####################################################################

RELEASE=1


#####################################################################
# Main targets
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

all: $(addsuffix _all,$(MODULES))

clean: $(addsuffix _clean,$(MODULES))

install: $(addsuffix _install,$(MODULES))

staticinstall: $(addsuffix _staticinstall,$(MODULES))

sharedinstall: $(addsuffix _sharedinstall,$(MODULES))


#####################################################################
# Include default makefile
#####################################################################

# test if FPCMAKE is still valid
ifndef FPCMAKE
ifdef FPCDIR
FPCMAKE=$(FPCDIR)/makefile.fpc
endif
endif
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
ifndef FPCMAKE
testfpcmake:
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@stopnow
ifndef NODEFAULTALL
all: testfpcmake
endif
install: testfpcmake
clean: testfpcmake
else
include $(FPCMAKE)
testfpcmake:
endif


#####################################################################
# Dependencies
#####################################################################

#######################################
# Compiler
#######################################

compiler_all:
	$(MAKE) -C compiler cycle

compiler_clean:
	$(MAKE) -C compiler clean

compiler_install:
	$(MAKE) -C compiler install

compiler_installlib:
	$(MAKE) -C compiler installlib

compiler_staticinstall:

compiler_sharedinstall:


#######################################
# RTL
#######################################

RTLDIR=rtl/$(OS_TARGET)

rtl_all:
	$(MAKE) -C $(RTLDIR) all

rtl_clean:
	$(MAKE) -C $(RTLDIR) clean

rtl_install:
	$(MAKE) -C $(RTLDIR) install

rtl_staticinstall:
	$(MAKE) -C $(RTLDIR) staticlibinstall

rtl_sharedinstall:
	$(MAKE) -C $(RTLDIR) sharedlibinstall


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
# FPC fpinst
#######################################

fpinst_all: rtl_all api_all fv_all
	$(MAKE) -C $(FPINSTDIR) all

fpinst_clean:
	$(MAKE) -C $(FPINSTDIR) clean

fpinst_install:
	$(MAKE) -C $(FPINSTDIR) install

fpinst_staticinstall:
	$(MAKE) -C $(FPINSTDIR) staticlibinstall

fpinst_sharedinstall:
	$(MAKE) -C $(FPINSTDIR) sharedlibinstall


#######################################
# IDE
#######################################

ide_all: rtl_all api_all fv_all
	$(MAKE) -C $(IDEDIR) all

ide_gdb: rtl_all api_all fv_all gdb_all
	$(MAKE) -C $(IDEDIR) gdb

ide_full: rtl_all api_all fv_all
	$(MAKE) -C $(IDEDIR) full

ide_fullgdb: rtl_all api_all fv_all gdb_all
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
# Packaging targets
#######################################

ifndef ZIPTARGET
ZIPTARGET=install
endif

export ZIPTARGET
export PACKAGEDIR=$(BASEDIR)

idezips: clean
	$(MAKE) ide_all
	$(MAKE) -C $(IDEDIR) zipinstall ZIPNAME=ide_fake
	$(MAKE) ide_clean
	$(MAKE) ide_gdb
	$(MAKE) -C $(IDEDIR) zipinstall ZIPNAME=ide_gdb
	$(MAKE) ide_clean
	$(MAKE) ide_full
	$(MAKE) -C $(IDEDIR) zipinstall ZIPNAME=ide_comp
	$(MAKE) ide_clean
	$(MAKE) ide_fullgdb
	$(MAKE) -C $(IDEDIR) zipinstall ZIPNAME=ide_full

fvzip: rtl_clean api_clean fv_clean
	$(MAKE) api_all
	$(MAKE) -C $(APIDIR) zipinstall ZIPNAME=fv-$(PACKAGESUFFIX)
	$(MAKE) fv_all
	$(MAKE) -C $(FVDIR) zipinstalladd ZIPNAME=fv-$(PACKAGESUFFIX)

compilerzip: #compiler_clean rtl_clean
	$(MAKE) compiler_all
	$(MAKE) -C compiler zipinstall ZIPTARGET=quickinstall ZIPNAME=compiler-$(PACKAGESUFFIX)
	$(MAKE) -C $(RTLDIR) zipinstalladd ZIPNAME=compiler-$(PACKAGESUFFIX)

#
# $Log$
# Revision 1.8  1999-03-16 00:46:54  peter
#   * makefile.fpc targets start with fpc_
#   * small updates for install scripts
#
# Revision 1.7  1999/03/09 01:35:46  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
