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

# Everything else has been moved to rtl/cfg/makefile.cfg


#####################################################################
# Include default makefile
#####################################################################

CFG=cfg

include $(CFG)/makefile.cfg


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
override REDIR:= >> con
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
	utils utils_install

all: rtls

clean:
	make -C template clean
	make -C dos clean
	make -C go32v1 clean
	make -C go32v2 clean
	make -C linux clean
	make -C win32 clean
	make -C os2 clean
	make -C utils clean
	-$(DEL) *.dif


############################################################################
# RTLs
############################################################################

native: rtl$(OS_SOURCE)

rtls: rtlgo32v1 rtlgo32v2 rtllinux rtlos2 rtlwin32

rtlgo32v1:
	make -C go32v1

rtlgo32v2:
	make -C go32v2

rtllinux:
	make -C linux

rtlos2:
	make -C os2

rtlwin32:
	make -C win32

rtlamiga:
	make -C amiga

############################################################################
# Libs
############################################################################

nativelibs : $(OS_SOURCE)libs

libs: go32v1libs go32v2libs linuxlibs os2libs win32libs

go32v1libs:
	make -C go32v1 libs

go32v2libs:
	make -C go32v2 libs

linuxlibs:
	make -C linux libs

os2libs:
	make -C os2 libs

win32libs:
	make -C win32 libs


############################################################################
# Install
############################################################################

install: install_linux install_go32v1 install_go32v2 install_os2 install_win32

native_install: install_$(OS_SRC)

install_go32v1:
	make -C go32v1 install
install_go32v2:
	make -C go32v2 install
install_linux:
	make -C linux install
install_os2:
	make -C os2 install
install_win32:
	make -C win32 install


############################################################################
# LibInstall
############################################################################

libinstall: libinstall_linux libinstall_go32v1 libinstall_go32v2 \
	libinstall_os2 libinstall_win32

native_libinstall: libinstall_$(OS_SOURCE)

libinstall_go32v1:
	make -C go32v1 libinstall
libinstall_go32v2:
	make -C go32v2 libinstall
libinstall_linux:
	make -C linux libinstall
libinstall_os2:
	make -C os2 libinstall
libinstall_win32:
	make -C win32 libinstall


############################################################################
# Utils
############################################################################

utils:
	make -C utils all
utils_install:
	make -C utils install


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
	make -C inc diffs
diffs_i386:
	make -C i386 diffs
diffs_m68k:
	make -C m68k diffs
diffs_cfg:
	make -C cfg diffs
diffs_template:
	make -C template diffs
diffs_go32v1:
	make -C go32v1 diffs
diffs_go32v2:
	make -C go32v2 diffs
diffs_linux:
	make -C linux diffs
diffs_os2:
	make -C os2 diffs
diffs_win32:
	make -C win32 diffs

diffclean:
	make -C template diffclean
	make -C dos diffclean
	make -C go32v1 diffclean
	make -C go32v2 diffclean
	make -C linux diffclean
	make -C os2 diffclean
	make -C win32 diffclean
	-$(DEL) *.dif

#
# $Log$
# Revision 1.4  1998-12-21 14:26:52  pierre
#  * go32v? directory change reflected
#
# Revision 1.3  1998/10/22 16:45:23  pierre
#  + added rtlamiga target
#
# Revision 1.2  1998/09/16 16:47:20  peter
#   * merged fixes
#
# Revision 1.1.2.1  1998/09/16 16:17:46  peter
#   * updates to install with go32,win32
#
# Revision 1.1	1998/09/10 14:19:02  peter
#   - renamed
#
# Revision 1.10  1998/09/04 17:32:36  pierre
#   * REDIR handling corrected
#
