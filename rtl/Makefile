#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Runtime Library
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#####################################################################
# Start of configurable section
#####################################################################

# Default place of makefile.fpc
DEFAULTFPCDIR=..

# Set redir to YES if you want a log file to be kept.
ifndef REDIR
REDIR=NO
endif

# Set NODEBUG to YES if you DON'T want debugging
ifndef NODEBUG
NODEBUG=YES
endif

# Optional : Specify the place of the log file.
# default is 'log' in the source directory
ifndef REDIRFILE
REDIRFILE=
endif

# Optional: Error definitions file you want the compiler to use.
# !! If you specify a path, specify an absolute path. !!
ifndef ERRORFILE
ERRORFILE=
endif


#######################################################################
# Fix options
#######################################################################

# Check debugging.
ifneq ($(NODEBUG),YES)
override OPT+=-g
endif

# Check error definitions file.
ifdef ERRORFILE
override OPT+=-Fr$(ERRORFILE)
endif

# Check logfile.
ifeq ($(REDIR),YES)
ifndef inlinux
override PP=redir -eo $(PP)
endif
# set the verbosity to max
override OPT+=-va
ifdef REDIRFILE
override REDIR:= >> $(REDIRFILE)
else
override REDIR:= >> log
endif
else
override REDIR:=
endif

# Variables to export
export PP OPT REDIR REDIRFILE ERRORFILE


############################################################################
# Defaults
############################################################################

.PHONY: rtl_go32v1 rtl_go32v2 rtl_linux rtl_os2 rtl_win32 \
	install_go32v1 install_go32v2 install_linux install_os2 install_win32 \
	go32v1libs go32v2libs linuxlibs os2libs win32libs \
	clean install native diffs diffclean \
	libs libinstall \

all: native

clean:
	$(MAKE) -C go32v1 clean
	$(MAKE) -C go32v2 clean
	$(MAKE) -C linux clean
	$(MAKE) -C win32 clean
	$(MAKE) -C os2 clean
	-$(DEL) *.dif


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


############################################################################
# RTLs
############################################################################

native: rtl$(OS_SOURCE)

rtls: rtlgo32v1 rtlgo32v2 rtllinux rtlos2 rtlwin32

rtlgo32v1:
	$(MAKE) -C go32v1

rtlgo32v2:
	$(MAKE) -C go32v2

rtllinux:
	$(MAKE) -C linux

rtlos2:
	$(MAKE) -C os2

rtlwin32:
	$(MAKE) -C win32

rtlamiga:
	$(MAKE) -C amiga


############################################################################
# Libs
############################################################################

nativelibs : $(OS_SOURCE)libs

libs: go32v1libs go32v2libs linuxlibs os2libs win32libs

go32v1libs:
	$(MAKE) -C go32v1 libs

go32v2libs:
	$(MAKE) -C go32v2 libs

linuxlibs:
	$(MAKE) -C linux libs

os2libs:
	$(MAKE) -C os2 libs

win32libs:
	$(MAKE) -C win32 libs


############################################################################
# Install
############################################################################

install: install_linux install_go32v1 install_go32v2 install_os2 install_win32

native_install: install_$(OS_SRC)

install_go32v1:
	$(MAKE) -C go32v1 install
install_go32v2:
	$(MAKE) -C go32v2 install
install_linux:
	$(MAKE) -C linux install
install_os2:
	$(MAKE) -C os2 install
install_win32:
	$(MAKE) -C win32 install


############################################################################
# LibInstall
############################################################################

libinstall: libinstall_linux libinstall_go32v1 libinstall_go32v2 \
	libinstall_os2 libinstall_win32

native_libinstall: libinstall_$(OS_SOURCE)

libinstall_go32v1:
	$(MAKE) -C go32v1 libinstall
libinstall_go32v2:
	$(MAKE) -C go32v2 libinstall
libinstall_linux:
	$(MAKE) -C linux libinstall
libinstall_os2:
	$(MAKE) -C os2 libinstall
libinstall_win32:
	$(MAKE) -C win32 libinstall


############################################################################
# Diffs
############################################################################

diffs: diffs_rtl diffs_inc diffs_i386 diffs_m68k diffs_dos diffs_cfg \
	diffs_os2 diffs_go32v1 diffs_go32v2 diffs_linux diffs_win32 \
	diffs_template

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/makefile > makefile.dif

diffs_rtl: makefile.dif

diffs_inc:
	$(MAKE) -C inc diffs
diffs_i386:
	$(MAKE) -C i386 diffs
diffs_m68k:
	$(MAKE) -C m68k diffs
diffs_cfg:
	$(MAKE) -C cfg diffs
diffs_template:
	$(MAKE) -C template diffs
diffs_go32v1:
	$(MAKE) -C go32v1 diffs
diffs_go32v2:
	$(MAKE) -C go32v2 diffs
diffs_linux:
	$(MAKE) -C linux diffs
diffs_os2:
	$(MAKE) -C os2 diffs
diffs_win32:
	$(MAKE) -C win32 diffs

diffclean:
	$(MAKE) -C template diffclean
	$(MAKE) -C dos diffclean
	$(MAKE) -C go32v1 diffclean
	$(MAKE) -C go32v2 diffclean
	$(MAKE) -C linux diffclean
	$(MAKE) -C os2 diffclean
	$(MAKE) -C win32 diffclean
	-$(DEL) *.dif

#
# $Log$
# Revision 1.7  1999-06-01 13:23:07  peter
#   * fixes to work with the new makefile
#   * os2 compiles now correct under linux
#
#
