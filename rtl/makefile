#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Win32 Runtime Library
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#****************************************************************************

#####################################################################
# Start of configurable section
#####################################################################

# What compiler do you want to use :
ifndef PP
PP=ppc386
endif PP

# Where do you want to install the units ?
# For each of the systems, a subdirectory OSunits of
# this directry will be created, and the system dependent
# files will be set there.
# For linux:
UNITINSTALLDIR=/usr/lib/fpc/0.99.6
# For dos/os2 : 
# UNITINSTALLDIR=\pp\units

# Where do you want to install the (shared or static) run-time libraries ?
# For linux:
LIBINSTALLDIR=/usr/lib
# For dos/os2 : 
#LIBINSTALLDIR=\pp\units

# Where do you want the utilities installed ?
# for linux
BININSTALLDIR=/usr/bin
# for DOS, OS/2
# BININSTALLDIR=\pp\bin

#####################################################################
# Try to determine Operating System
#####################################################################

BASEDIR=$(shell pwd)

# in linux no : in pathes
ifeq ($(findstring :,$(BASEDIR)),)
inlinux=1
endif

# in case pwd is not present on the DOS-OS
ifeq ($(strip $(BASEDIR)),'')
inlinux=
BASEDIR:=.
endif

# set here the type of the ppc386 you use:
# Choose from: go32v1 go32v2 linux or os2
# can determine if on linux otherwise assume go32v2

ifdef inlinux
OS_SRC=linux
else
OS_SRC=go32v2
endif


# Set redir to YES if you want a log file to be kept.
REDIR=YES

# Set NODEBUG to YES if you DON'T want debugging
NODEBUG=YES

# set target processor type
CPU=i386
# CPU=m68k

# Set REFPATH if you want to generate diffs to a standard RTL
# this can not be a relative path.
REFPATH=/usr/local/fpk/work/0.9.7/rtl

# Where is the diff program ?  (No relative paths)
DIFF=diff

# What options do you want to pass to diff ?
DIFFOPTS=-b -c

# Where is the ppumove program ? (set only if you want to make libs)
# Don't specify a relative path.
PPUMOVE=ppumove

#
# Optional configuration settings.
#

# Set any options you wish to give to the compiler
OPT=

# Optional : Specify the place of the log file.
# default is 'log' in the source directory
# REDIRFILE=


# Optional: Error definitions file you want the compiler to use.
# !! If you specify a path, specify an absolute path. !!
# ERRORFILE=

# Optional: The program to create directories.
# Under DOS this is by default 'mkdir'. Under Linux this is 'install -m 755 -d'
# MKDIR=

# Optional: The program to install the files into the destination directory.
# Under DOS this is by default 'copy', under Linux this is 'install -m 644'
# copy will not work with /
# use cp from djgpp instead
# INSTALL=


#######################################################################
# End of configurable section.
# Do not edit after this line.
#######################################################################

# Check debugging.
ifneq (YES,$(NODEBUG))
OPT:=$(OPT) -g
endif

# Check logfile.
ifeq (YES,$(REDIR))
ifdef REDIRFILE
override REDIR:= >> $(REDIRFILE)
else
override REDIR:= >> log
endif
else
REDIR:=
endif

# Check error definitions file.
ifdef ERRORFILE
OPT:= $(OPT) -Fr$(ERRORFILE)
endif

# Check operating system.
ifeq ($(OS_SRC),linux)
DOS=NO
else
DOS=YES
# also redirect the standard error to the redir file
ifdef REDIR
PP:=redir -eo $(PP)
# set the verbosity to max
OPT:=$(OPT) -va
endif
endif

# Check copy delete commands.
# You need cp from GNU to handle / as directory separator
COPY=cp -p
DEL=rm

# To install programs
ifndef INSTALL
ifeq ($(DOS),YES)
INSTALL=copy
else
INSTALL=install
endif
endif

# To make a directory.
ifndef MKDIR
ifeq ($(DOS),YES)
MKDIR=mkdir
else
MKDIR=install -m 755 -d
endif
endif

# add target processor define to command line options
OPT:=$(OPT) -d$(CPU)

# Variables to export
export OS_SRC DOS SEP PP OPT REDIR COPY DEL LIBINSTALLDIR INSTALL MKDIR \
       REFPATH CPU PPUMOVE UNITINSTALLDIR

.PHONY: rtl_go32v1 rtl_go32v2 rtl_linux rtl_os2 clean install install_os2 install_linux \
        install_go3v2 install_go32v1 native diffs diffclean libs libinstall utils\
        utils_install

native: rtl$(OS_SRC)

nativelibs : $(OS_SRC)libs

all: rtlgo32v1 rtlgo32v2 rtllinux rtlos2

libs: go32v1libs go32v2libs linuxlibs os2libs

diffs: diffs_rtl diffs_inc diffs_i386 diffs_m68k diffs_dos diffs_cfg \
	diffs_os2 diffs_go32v1 diffs_go32v2 diffs_linux diffs_template


rtlgo32v1:
	$(MAKE) -C dos/go32v1 CFGFILE=../../cfg/ppgo32v1.cfg OS_TARGET=go32v1

rtlgo32v2:
	$(MAKE) -C dos/go32v2 CFGFILE=../../cfg/ppgo32v2.cfg  OS_TARGET=go32v2

rtllinux:
	$(MAKE) -C linux CFGFILE=../cfg/pplinux.cfg OS_TARGET=linux

rtlos2:
	$(MAKE) -C os2 CFGFILE=../cfg/ppos2.cfg OS_TARGET=os2

rtlwin32:
	$(MAKE) -C win32 CFGFILE=../cfg/ppw32.cfg OS_TARGET=win32

go32v1libs:
	$(MAKE) -C dos/go32v1 CFGFILE=../../cfg/ppdos.cfg OS_TARGET=go32v1 libs

go32v2libs:
	$(MAKE) -C dos/go32v2 CFGFILE=../../cfg/ppgo32v2.cfg  OS_TARGET=go32v2 libs

linuxlibs:
	$(MAKE) -C linux CFGFILE=../cfg/pplinux.cfg OS_TARGET=linux libs

os2libs:
	$(MAKE) -C os2 CFGFILE=../cfg/ppos2.cfg OS_TARGET=os2 libs

win32libs:
	$(MAKE) -C win32 CFGFILE=../cfg/ppw32.cfg OS_TARGET=win32 libs

clean:
	$(MAKE) -C template clean
	$(MAKE) -C dos clean
	$(MAKE) -C dos/go32v1 clean
	$(MAKE) -C dos/go32v2 clean
	$(MAKE) -C linux clean
	$(MAKE) -C win32 clean
	$(MAKE) -C os2 clean
	-$(DEL) *.dif

diffclean:
	$(MAKE) -C template diffclean
	$(MAKE) -C dos diffclean
	$(MAKE) -C dos/go32v1 diffclean
	$(MAKE) -C dos/go32v2 diffclean
	$(MAKE) -C linux diffclean
	$(MAKE) -C os2 diffclean
	$(MAKE) -C win32 diffclean
	-$(DEL) *.dif


install: install_linux install_go32v1 install_go32v2 install_os2

libinstall: libinstall_linux libinstall_go32v1 libinstall_go32v2 libinstall_os2

native_install: install_$(OS_SRC)

native_libinstall: libinstall_$(OS_SRC)

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/makefile > makefile.dif
	

diffs_rtl: makefile.dif 

diffs_dos:
	$(MAKE) -C dos diffs
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
	$(MAKE) -C dos/go32v1 diffs
diffs_go32v2:
	$(MAKE) -C dos/go32v2 diffs
diffs_linux:
	$(MAKE) -C linux diffs
diffs_os2:
	$(MAKE) -C os2 diffs

install_go32v1:
	$(MAKE) -C dos/go32v1 install
install_go32v2:
	$(MAKE) -C dos/go32v2 install
install_linux:
	$(MAKE) -C linux install
install_os2:
	$(MAKE) -C os2 install

libinstall_go32v1:
	$(MAKE) -C dos/go32v1 libinstall
libinstall_go32v2:
	$(MAKE) -C dos/go32v2 libinstall
libinstall_linux:
	$(MAKE) -C linux libinstall
libinstall_os2:
	$(MAKE) -C os2 libinstall

utils:
	$(MAKE) -C utils all
utils_install:
	$(MAKE) -C utils install

