#****************************************************************************
#                     Makefile for Free Component Library
#            Copyright (c) 1998 by the Free Pascal Development Team
#****************************************************************************

#####################################################################
# Start of configurable section
#####################################################################

# What compiler do you want to use :
# !! If you specify a path, specify an absolute path !!
#PP=/pas/fpk/curver/ppc386
PP=ppc386

# What is your OS ?
# Possible choices : linux win32 go32v2 go32v1 os2
OS_SRC=linux
#OS_SRC=win32
#OS_SRC=go32v1
#OS_SRC=go32v2
#OS_SRC=os2

# Where do you want to install the units ?
UNITINSTALLDIR=/usr/lib/ppc/0.99.6/fcl
# UNITINSTALLDIR=\pp\units\fcl

# set target processor type
CPU=i386
# CPU=m68k

# Where is the ppumove program ? (set only if you want to make libs)
# Don't specify a relative path.
PPUMOVE=ppumove

# Set any options you wish to give to the compiler
OPT=

#######################################################################
# End of configurable section.
# Do not edit after this line.
#######################################################################

# Where are we ?
BASEDIR=$(shell pwd)
ifeq ($(findstring :,$(BASEDIR)),)
inlinux=1
endif
ifeq ($(strip $(BASEDIR)),'')
inlinux=
BASEDIR:=.
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
INSTALL=cp
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

.PHONY: native all linux win32 os2 go32v2 go32v1 clean install

native: $(OS_SRC)

all: linux go32v2 win32 go32v1 os2


go32v1:
	$(MAKE) -C dos/go32v1

go32v2:
	$(MAKE) -C dos/go32v2

linux:
	$(MAKE) -C linux

os2:
	$(MAKE) -C os2

win32:
	$(MAKE) -C win32

clean:
	$(MAKE) -C inc clean
	$(MAKE) -C i386 clean
	$(MAKE) -C dos/go32v2 clean
	$(MAKE) -C linux clean
	$(MAKE) -C os2 clean
	$(MAKE) -C win32 clean

install:
	$(MAKE) -C $(OS_SRC) install
