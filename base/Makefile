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

MODULES=rtl compiler utils api fv gdb ide

ifndef RTLDIR
RTLDIR=rtl
endif

ifndef COMPILERDIR
COMPILERDIR=utils
endif

ifndef UTILSDIR
UTILSDIR=utils
endif

ifndef FCLDIR
FCLDIR=fcl
endif

ifndef GTKDIR
GTKDIR=gtk
endif

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
IDEDIR=ide
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
	$(addsuffix _sharedinstall,$(MODULES))\
	ide_allzip ide_gdbzip ide_fullzip ide_fullgdbzip

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
# RTL
#######################################

override RTLDIR:=$(RTLDIR)/$(OS_TARGET)

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
# Compiler
#######################################

compiler_all: rtl_all
	$(MAKE) -C $(COMPILERDIR) all

compiler_clean:
	$(MAKE) -C $(COMPILERDIR) clean

compiler_install:
	$(MAKE) -C $(COMPILERDIR) install

compiler_installlib:
	$(MAKE) -C $(COMPILERDIR) installlib

compiler_staticinstall:

compiler_sharedinstall:


#######################################
# Utils
#######################################

utils_all: rtl_all
	$(MAKE) -C $(UTILSDIR) all

utils_clean:
	$(MAKE) -C $(UTILSDIR) clean

utils_install:
	$(MAKE) -C $(UTILSDIR) install

utils_installlib:
	$(MAKE) -C $(UTILSDIR) installlib

utils_staticinstall:

utils_sharedinstall:


#######################################
# FCL
#######################################

override FCLDIR:=$(FCLDIR)/$(OS_TARGET)

fcl_all: rtl_all
	$(MAKE) -C $(FCLDIR) all

fcl_clean:
	$(MAKE) -C $(FCLDIR) clean

fcl_install:
	$(MAKE) -C $(FCLDIR) install

fcl_staticinstall:
	$(MAKE) -C $(FCLDIR) staticlibinstall

fcl_sharedinstall:
	$(MAKE) -C $(FCLDIR) sharedlibinstall


#######################################
# GTK
#######################################

gtk_all: rtl_all
	$(MAKE) -C $(GTKDIR) all

gtk_clean:
	$(MAKE) -C $(GTKDIR) clean

gtk_install:
	$(MAKE) -C $(GTKDIR) install

gtk_staticinstall:
	$(MAKE) -C $(GTKDIR) staticlibinstall

gtk_sharedinstall:
	$(MAKE) -C $(GTKDIR) sharedlibinstall



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

idezips: clean ide_allzip ide_gdbzip ide_fullzip ide_fullgdbzip

ide_allzip:
	$(MAKE) ide_clean
	$(MAKE) ide_all
	$(MAKE) -C $(IDEDIR)/text fpc_zipinstall ZIPNAME=ide-fake-$(PACKAGESUFFIX)
ide_gdbzip:
	$(MAKE) ide_clean
	$(MAKE) ide_gdb
	$(MAKE) -C $(IDEDIR)/text fpc_zipinstall ZIPNAME=ide-gdb-$(PACKAGESUFFIX)
ide_fullzip:
	$(MAKE) ide_clean
	$(MAKE) ide_full
	$(MAKE) -C $(IDEDIR)/text fpc_zipinstall ZIPNAME=ide-comp-$(PACKAGESUFFIX)
ide_fullgdbzip:
	$(MAKE) ide_clean
	$(MAKE) ide_fullgdb
	$(MAKE) -C $(IDEDIR)/text fpc_zipinstall ZIPNAME=ide-full-$(PACKAGESUFFIX)

fvzip: rtl_clean api_clean fv_clean
	$(MAKE) api_all
	$(MAKE) -C $(APIDIR) fpc_zipinstall ZIPNAME=fv-$(PACKAGESUFFIX)
	$(MAKE) fv_all
	$(MAKE) -C $(FVDIR) fpc_zipinstalladd ZIPNAME=fv-$(PACKAGESUFFIX)

fclzip: rtl_clean fcl_clean
	$(MAKE) fcl_all
	$(MAKE) -C $(FCLDIR) fpc_zipinstall ZIPNAME=fcl-$(PACKAGESUFFIX)

gtkzip: rtl_clean gtk_clean
	$(MAKE) gtk_all
	$(MAKE) -C $(GTKDIR) fpc_zipinstall ZIPNAME=gtk-$(PACKAGESUFFIX)

compilerzip: compiler_clean rtl_clean
	$(MAKE) compiler_all
	$(MAKE) -C $(COMPILERDIR) fpc_zipinstall ZIPTARGET=quickinstall ZIPNAME=compiler-$(PACKAGESUFFIX)
	$(MAKE) -C $(RTLDIR) fpc_zipinstalladd ZIPNAME=compiler-$(PACKAGESUFFIX)

utilszip: utils_clean rtl_clean
	$(MAKE) utils_all
	$(MAKE) -C $(UTILSDIR) fpc_zipinstall ZIPNAME=utils-$(PACKAGESUFFIX)

#
# $Log$
# Revision 1.15  1999-05-12 16:17:06  peter
#   + utils_X targets
#   * missing export of prefixinstalldir
#
# Revision 1.14  1999/04/21 10:25:39  michael
# + Split idezips target
#
# Revision 1.13  1999/04/19 12:53:02  peter
#   * fixed idezips target
#
# Revision 1.12  1999/04/16 20:15:41  michael
# + Adapted ide Makefiles
#
# Revision 1.11  1999/04/08 10:16:16  peter
#   * zipinstall for linux with .tar.gz
#
# Revision 1.10  1999/04/01 22:16:02  peter
#   * renamed ide zips
#
# Revision 1.9  1999/03/18 11:19:31  peter
#   * fixed zipinstall
#
# Revision 1.8	1999/03/16 00:46:54  peter
#   * makefile.fpc targets start with fpc_
#   * small updates for install scripts
#
# Revision 1.7	1999/03/09 01:35:46  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
