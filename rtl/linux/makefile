#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Linux Runtime Library
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#####################################################################
# Start of configurable section.
# Please note that all these must be set in the main makefile, and
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

# set the directory where to install the units.
ifndef UNITINSTALLDIR
UNITINSTALLDIR=/usr/lib/ppc/0.99.0/linuxunits
endif

# set the directory where to install the libs (must exist)
ifndef LIBINSTALLDIR
LIBINSTALLDIR=/usr/lib
endif

# What is the Operating System ?
ifndef OS_SRC
OS_SRC=linux
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=linux
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
endif

# What compiler to use ?
ifndef PP
PP=ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

# Where is the PPUMOVE program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

# Set this to 'shared' or 'static'
LIBTYPE=shared

# AOUT should be defined in main makefile.
# But you can set it here too.
# AOUT = -DAOUT

# Do you want to link to the C library ?
# Standard it is NO. You can set it to YES to link in th C library.
ifndef LINK_TO_C
LINK_TO_C=NO
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

# Where are the include files ?
INC=../inc
PROCINC=../$(CPU)
OBJPASDIR=../objpas

# To copy pograms
ifndef COPY
ifeq ($(DOS),YES)
COPY=copy
else
COPY=cp -p
endif
endif

# Check delete program
ifndef DEL
ifeq ($(DOS),YES)
DEL=del
else
DEL=rm -f
endif
endif

# To install files
ifndef INSTALL
ifeq ($(DOS),YES)
INSTALL=copy
else
INSTALL=install -m 644
endif
endif

# To install programs
ifndef INSTALLEXE
ifeq ($(DOS),YES)
INSTALLEXE=copy
else
INSTALLEXE=install -m 755
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

# diff program
ifndef REFPATH
REFPATH=/usr/local/fpk/work/new/rtl
endif
ifndef DIFF
DIFF=diff
endif
ifndef DIFFOPTS
DIFFOPTS=-b -c
endif

#
# System independent Commandline Options
#

# Cross compiling ?
ifeq ($(OS_TARGET),$(OS_SRC))
CROSSCOMPILE=NO
else
CROSSCOMPILE=YES
endif

# add required options... (-dFPC is required for older versions)
override OPT:= $(OPT) -dFPC -d$(CPU)

# Was a config file specified ?
ifdef CFGFILE
override OPT:=$(OPT) @$(CFGFILE)
endif

# Check for crosscompile
ifeq ($(CROSSCOMPILE),YES)
override OPT:= $(OPT) -dCROSSCOMPILE -T$(OS_TARGET)
endif

#
# System dependent Commandline Options
#

# Check if we need C library.
ifeq ($(LINK_TO_C),YES)
override OPT:=$(OPT) -dCRTLIB
endif

#####################################################################
# System dependent
#####################################################################

# check whether we make shared or static libs
ifndef LIBTYPE
LIBTYPE=shared
endif

# determine libary extension.
ifeq ($(LIBTYPE),static)
LIBEXT=.a
else
LIBEXT=.so
endif

# Determine needed extensions
PPUEXT=.ppu
PPLEXT=.ppl
OEXT=.o
ASMEXT=.s

# Define Linux Units
SYSTEMPPU=syslinux$(PPUEXT)
OBJECTS=strings linux objpas \
	dos crt objects printer \
	getopts errors sockets \

# Extra Syslinux Depends
ifeq ($(LINK_TO_C),YES)
SYSLINUXDEPS=lprt$(OEXT)
else
SYSLINUXDEPS=sysconst.inc systypes.inc syscalls.inc
endif

# Determine on what dos unit should depend.
DOSDEPS = dos.pp $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT)
ifeq ($(LINK_TO_C),YES)
DOSDEPS:=$(DOSDEPS) lprt$(OEXT)
else
DOSDEPS:=$(DOSDEPS) linux$(PPUEXT)
endif

# Which prt to use ?
ifdef AOUT
PRT=prt0
else
PRT=prt1
endif

LOADERAS=$(CPU)/$(PRT).as
GLOADERAS=$(CPU)/g$(PRT).as

# Define Loaders
ifeq ($(LINK_TO_C),NO)
LOADERS=prt0 gprt0
else
LOADERS=lprt
endif

#####################################################################
# System independent Makefile
#####################################################################

# OS Independent Depends
SYSTEMDEPS=system.inc systemh.inc mathh.inc real2str.inc \
	heaph.inc innr.inc sstrings.inc file.inc \
	text.inc typefile.inc version.inc filerec.inc \
	textrec.inc

# Processor Dependent Depends
SYSPROCDEPS=math.inc set.inc heap.inc $(CPU).inc

# Add Prefix and Suffixes
OBJLOADERS=$(addsuffix $(OEXT), $(LOADERS))
PPUOBJECTS=$(addsuffix $(PPUEXT), $(OBJECTS))
DSYSTEMDEPS=$(addprefix $(INC)/, $(SYSTEMDEPS))
DSYSPROCDEPS=$(addprefix $(PROCINC)/, $(SYSPROCDEPS))


.PHONY : all install clean \
	 libs libsclean \
	 diffs diffclean \

all : $(OBJLOADERS) $(PPUOBJECTS)

install : all
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(UNITINSTALLDIR)

clean :
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) *.PPS log

#####################################################################
# Files
#####################################################################

#
# Loaders
#

prt0$(OEXT) : $(LOADERAS)
	-as $(LOADERAS) -o prt0$(OEXT)

gprt0$(OEXT) : $(GLOADERAS)
	-as $(GLOADERAS) -o gprt0$(OEXT)

lprt$(OEXT) : lprt.c
	gcc $(AOUT) -c lprt.c -o lprt$(OEXT)

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syslinux.pp $(SYSLINUXDEPS) $(DSYSTEMDEPS) $(DSYSPROCDEPS)
	$(COPY) $(DSYSTEMDEPS) $(DSYSPROCDEPS) .
	$(PP) $(OPT) -Us -Sg syslinux.pp $(REDIR)
	$(DEL) $(SYSTEMDEPS) $(SYSPROCDEPS)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

linux$(PPUEXT) : linux.pp strings$(PPUEXT) $(INC)/textrec.inc $(INC)/filerec.inc \
		 syscalls.inc systypes.inc sysconst.inc $(SYSTEMPPU)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) linux.pp $(REDIR)
	$(DEL) filerec.inc textrec.inc

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(SYSTEMPPU)
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(PP) $(OPT) objpas $(REDIR)
	$(DEL) objpas.pp

#
# System Dependent Units
#

sockets$(PPUEXT) : sockets.pp $(INC)/textrec.inc $(INC)/filerec.inc \
		   linux$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) sockets.pp $(REDIR)
	$(DEL) filerec.inc textrec.inc

errors$(PPUEXT) : errors.pp strings$(PPUEXT) $(SYSTEMPPU)
	$(PP) $(OPT) errors.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : $(DOSDEPS) $(SYSTEMPPU)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) filerec.inc textrec.inc

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc $(INC)/filerec.inc linux$(PPUEXT)\
	       $(SYSTEMPPU)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) crt $(REDIR)
	$(DEL) filerec.inc textrec.inc

objects$(PPUEXT) : objects.pp $(SYSTEMPPU)
	$(PP) $(OPT) objects $(REDIR)

printer$(PPUEXT) : printer.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(INC)/textrec.inc .
	$(PP) $(OPT) printer $(REDIR)
	$(DEL) textrec.inc

#
# Other RTL Units
#

getopts$(PPUEXT) : $(PROCINC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts $(REDIR)
	$(DEL) getopts.pp

#####################################################################
# Libs
#####################################################################

libs	: all libfpc$(LIBEXT)

libfpc.so:
	$(PPUMOVE) -o fpc *.ppu

libfpc.a:
	$(PPUMOVE) -s -o fpc *.ppu

libinstall : libs
	$(INSTALLEXE) libfpc$(LIBEXT) $(LIBINSTALLDIR)
	$(INSTALL) *$(PPLEXT) $(UNITINSTALLDIR)

libsclean : clean
	-$(DEL) *.a *.so *$(PPLEXT)

#####################################################################
# Diffs
#####################################################################

%.dif : %.pp
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/linux/$*.pp >$*.dif

%.dif :  %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/linux/$*.inc >$*.dif

%.dif : %.as
	-$(DIFF) $(DIFFOPTS) $*.s $(REFPATH)/linux/$*.s >$*.dif

diffclean :
	-$(DEL) *.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/linux/makefile > makefile.dif

diffs : dos.dif syslinux.dif objects.dif errno.dif prt0.dif prt1.dif \
	objects.dif linux.dif printer.dif os.dif syscalls.dif sysnr.dif \
	makefile.dif

#####################################################################
# Distribution
#####################################################################

distclean : clean libsclean diffclean

