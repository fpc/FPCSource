#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Go32v1 Runtime Library
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
UNITINSTALLDIR=c:\lib\ppc\win32
endif

# set the directory where to install libraries
ifndef LIBINSTALLDIR
LIBINSTALLDIR=c:\lib
endif

# What is the Operating System
ifndef OS_SRC
OS_SRC=GO32V2
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=WIN32
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
endif

# What compiler to use ?
# I think ppc386 is better (it's mostly in path) (FK)
ifndef PP
PP=ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

# Where is the ppumove program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

# Where are the include files
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


#####################################################################
# System dependent
#####################################################################

# Determine needed extensions
PPUEXT=.ppw
PPLEXT=.ppl
OEXT=.obj
ASMEXT=.s
LIBEXT=.a

# Define Linux Units
SYSTEMPPU=syswin32$(PPUEXT)
OBJECTS=strings objpas \
	base \
#	 dos crt objects printer \
	cpu mmx getopts \

# No loaders needed
LOADERS=

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

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syswin32.pp $(DSYSTEMDEPS) $(DSYSPROCDEPS)
	$(COPY) $(DSYSTEMDEPS) .
	$(COPY) $(DSYSPROCDEPS) .
	$(PP) $(OPT) -Us -Sg syswin32.pp $(REDIR)
	$(DEL) $(SYSTEMDEPS)
	$(DEL) $(SYSPROCDEPS)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings.pp $(REDIR)
	$(DEL) strings.pp

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

base$(PPUEXT) : base.pp $(SYSTEMPPU)
	$(PP) $(OPT) base.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc \
	       go32$(PPUEXT) strings$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) filerec.inc textrec.inc

#crt$(PPUEXT) : crt.pp $(INC)/textrec.inc go32$(PPUEXT) $(SYSTEMPPU)
#	 $(COPY) $(INC)/textrec.inc .
#	 $(PP) $(OPT) crt $(REDIR)
#	 $(DEL) textrec.inc

#objects$(PPUEXT) : objects.pp $(SYSTEMPPU)
#	 $(PP) $(OPT) objects.pp $(REDIR)

#printer$(PPUEXT) : printer.pp $(SYSTEMPPU)
#	 $(PP) $(OPT) printer.pp $(REDIR)

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/cpu.pp .
	$(PP) $(OPT) cpu.pp $(REDIR)
	$(DEL) cpu.pp

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(PROCINC)/mmx.pp .
	$(PP) $(OPT) mmx.pp $(REDIR)
	$(DEL) mmx.pp

getopts$(PPUEXT) : $(PROCINC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts.pp $(REDIR)
	$(DEL) getopts.pp


#####################################################################
# Libs
#####################################################################

libs: all

libsclean : clean
	-$(DEL) *.$(LIBEXT) *$(PPLEXT)

#####################################################################
# Diffs
#####################################################################

%.dif : %.pp
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/dos/go32v1/$*.pp > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/dos/go32v1/$*.inc > $*.dif

%.dif : %.as
	-$(DIFF) $(DIFFOPTS) $*.as $(REFPATH)/dos/go32v1/$*.as > $*.dif

diffclean:
	-$(DEL) *.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/dos/go32v1/makefile > makefile.dif

diffs: syswin32.dif os.dif makefile.dif dos.dif base.dif struct.dif \
       winheap.dif messages.dif

#####################################################################
# Distribution
#####################################################################

distclean : clean libsclean diffclean

