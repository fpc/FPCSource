#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1993-98 by the Free Pascal Development Team
#
#   Makefile for the Free Pascal Compiler
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

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

# What compiler to use
ifndef PP
PP=ppc386
endif

#####################################################################
# Setup Targets
#####################################################################

# what target do we use
# currently dos go32v2 os2 and linux are available
ifdef inlinux
TARGET=linux
else
TARGET=go32v2
endif

# What processor do you want to compile for : i386 m68k (case sensitive !!)
ifndef CPU
CPU= i386
# CPU= m68k
endif

#####################################################################
# Setup Files Directories
#####################################################################

# Set os-dependent files and extensions
ifdef inlinux
EXEEXT=
REPLACE=mv -f
CP=cp -f
else
EXEEXT=.exe
REPLACE=mv -f
CP=cp -f
endif

COMPILERDIR=$(BASEDIR)
RTLDIR:=$(BASEDIR)/../rtl

# specify where units are.
# This needs to be set correctly for the 'remake' target to work !
ifndef UNITDIR
UNITDIR=$(RTLDIR)/$(TARGET)
ifeq ($(TARGET),dos)
UNITDIR=$(RTLDIR)/dos/go32v1
endif
ifeq ($(TARGET),go32v2)
UNITDIR=$(RTLDIR)/dos/go32v2
endif
endif
# not def UNITDIR

# Where to install the executable program/link
ifndef PROGINSTALLDIR
ifdef inlinux
PROGINSTALLDIR = /usr/bin
else
PROGINSTALLDIR = c:\pp\bin
endif
endif

# !!! Linux only
# Where to install the _real_executable and support files
ifndef LIBINSTALLDIR
ifdef inlinux
LIBINSTALLDIR=/usr/lib/fpc/0.99.6
# for a.out
# LIBINSTALLDIR=/usr/lib/ppc/aout/0.99.6
else
LIBINSTALLDIR=$(PROGINSTALLDIR)
endif
endif

# Where the .msg files will be stored
ifndef MSGINSTALLDIR
MSGINSTALLDIR=$(LIBINSTALLDIR)/msg
endif

ifndef UNITINSTALLDIR
ifdef inlinux
UNITINSTALLDIR=$(LIBINSTALLDIR)/linuxunits
else
UNITINSTALLDIR=$(UNITDIR)
endif
endif

# !!! Linux only
# GCCLIBPATH is wat is it on my PC... it MUST be set in the main Makefile
ifndef GCCLIBPATH
GCCLIBPATH=/usr/lib/gcc-lib/i486-linux/2.6.3
endif

#####################################################################
# When making diffs of the sources
#####################################################################

# Diff program
DIFF = diff

# Diff3 program
DIFF3 = diff3

# Options to diff.
DIFFOPTS = -b -c

# Directory where files are to make diffs with..
ifdef inlinux
DIFDIR = /usr/local/fpk/work/new/compiler
else
DIFDIR = h:/cvs/compiler
endif

# Directory where reference files are for diff3
ifdef inlinux
REFDIR = /usr/local/fpk/dist/source/compiler
else
REFDIR = h:/myref/compiler
endif

#####################################################################
# End of configurable section. Do not edit after this line.
#####################################################################

# set correct defines (also needed by mkdep)
PPDEFS:=-d$(CPU) -dGDB -dFPC

# Set the needed compiler options
PPOPTS:=$(OPT) $(PPDEFS) -Sg

# Unitdir specified ?
ifneq ("$(UNITDIR)", "")
PPOPTS:=$(PPOPTS) -Up$(UNITDIR)
endif

# Do we need the GCC library ?
ifeq ($(LINK_TO_C),YES)
PPOPTS:=$(PPOPTS) -Fg$(GCCLIBPATH)
endif

# Create the whole compiler commandline
COMPILER=$(PP) $(PPOPTS)

#####################################################################
# Setup os-independent filenames
#####################################################################

PPEXENAME=pp$(EXEEXT)
EXENAME=ppc386$(EXEEXT)
TEMPNAME=ppc$(EXEEXT)
TEMPNAME1=ppc1$(EXEEXT)
TEMPNAME2=ppc2$(EXEEXT)
TEMPNAME3=ppc3$(EXEEXT)
MAKEDEP=mkdep$(EXEEXT)

PASFILES:=$(shell ls *.pas)
INCFILES:=$(shell ls *.inc)
MSGFILES:=$(shell ls *.msg)

#####################################################################
# Default makefile
#####################################################################

.SUFFIXES: .pas $(EXEEXT) .ppu .dif .d3p .d3i .d3m .new

.PHONY : all clean info \
	 cycle remake remake3 \
	 install \
	 diff diff3 patch rtl toflor replacediff3 restorediff3 \
	 test rtlzip \

.pas.ppu:
	$(COMPILER) $<

.pas$(EXEEXT):
	$(COMPILER) $<

all : $(EXENAME)

clean :
	-rm -f *.o *.ppu *.s $(EXENAME)

distclean: clean
	-rm -f $(TEMPNAME) $(TEMPNAME1) $(TEMPNAME2) $(TEMPNAME3)

#####################################################################
# Info
#####################################################################

info :
	@echo - Target is $(TARGET)
	@echo - Basedir is $(BASEDIR)
	@echo - Pascal files are $(PASFILES)
	@echo - Inc files are $(INCFILES)
	@echo - Msg files are $(MSGFILES)

#####################################################################
# Include depencies (linux only)
#####################################################################

ifdef inlinux
$(MAKEDEP) : $(RTLDIR)/utils/mkdep.pp
	$(PP) $(RTLDIR)/utils/mkdep.pp
	$(CP) $(RTLDIR)/utils/$(MAKEDEP) $(MAKEDEP)

dependencies : $(MAKEDEP)
	$(MAKEDEP) pp.pas $(PPDEFS) '-A$$(COMPILER)' > depend

include depend
endif

#####################################################################
# Make targets
#####################################################################

msgtxt.inc: errore.msg
	msg2inc errore.msg msgtxt.inc msgtxt

optmsg.inc: optione.msg
	msg2inc optione.msg optmsg.inc optiontxt

msg: msgtxt.inc optmsg.inc

# Make only the compiler
ifdef inlinux
$(EXENAME) : $(PPEXENAME)
	$(COMPILER) pp.pas
	$(REPLACE) $(PPEXENAME) $(EXENAME)
else
$(EXENAME) : $(PASFILES) $(INCFILES) $(MSGFILES)
	$(COMPILER) pp.pas
	$(REPLACE) $(PPEXENAME) $(EXENAME)
endif

# This target remakes the units with the currently made version
remake: $(EXENAME)
	$(REPLACE) $(EXENAME) $(TEMPNAME)
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR) 'PP=$(COMPILERDIR)/$(TEMPNAME)' all
	$(MAKE) 'PP=./$(TEMPNAME)' all

remake3: $(TEMPNAME3)
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR) 'PP=$(COMPILERDIR)/$(TEMPNAME3)' all
	$(MAKE) 'PP=./$(TEMPNAME3)' all
	diff $(TEMPNAME3) $(EXENAME)

$(TEMPNAME1) : $(EXENAME)
	$(REPLACE) $(EXENAME) $(TEMPNAME1)

$(TEMPNAME2) : $(TEMPNAME1)
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR) 'PP=$(COMPILERDIR)/$(TEMPNAME1)' all
	$(MAKE) 'PP=./$(TEMPNAME1)' all
	$(REPLACE) $(EXENAME) $(TEMPNAME2)

$(TEMPNAME3) : $(TEMPNAME2)
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR) 'PP=$(COMPILERDIR)/$(TEMPNAME2)' all
	$(MAKE) 'PP=./$(TEMPNAME2)' all
	$(REPLACE) $(EXENAME) $(TEMPNAME3)

cycle:
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR)
	$(MAKE) remake3

#####################################################################
# Installation
#####################################################################

install:
	umask 022
	strip ppc386
	install -m 755 -d $(LIBINSTALLDIR)
	install -m 755 ppc386 $(LIBINSTALLDIR)
	ln -sf $(LIBINSTALLDIR)/ppc386 $(PROGINSTALLDIR)/ppc386
	-makecfg $(LIBINSTALLDIR)/samplecfg $(UNITINSTALLDIR) $(MSGINSTALLDIR) $(GCCLIBPATH)
	install -m 755 -d $(MSGINSTALLDIR)
	install -m 666 errore.msg $(MSGINSTALLDIR)
	install -m 666 errorn.msg $(MSGINSTALLDIR)

#####################################################################
# Diffs
#####################################################################

SOURCEFILES = $(PASFILES) $(INCFILES) $(MSGFILES) Makefile

DIFFFILES = $(patsubst %.pas,%.dif,$(PASFILES)) \
	$(patsubst %.inc,%.dif,$(INCFILES)) \
	$(patsubst %.msg,%.dif,$(MSGFILES)) \
	Makefile.dif

DIFF3FILES = $(patsubst %.pas,%.d3p,$(PASFILES)) \
	$(patsubst %.inc,%.d3i,$(INCFILES)) \
	$(patsubst %.msg,%.d3m,$(MSGFILES)) \
	Makefile.di3

PATCHFILES = $(patsubst %.pas,%.new,$(PASFILES)) \
	$(patsubst %.inc,%.new,$(INCFILES)) \
	$(patsubst %.msg,%.new,$(MSGFILES)) \
	Makefile.new

%.dif : %.pas
	-$(DIFF) $(DIFFOPTS) $*.pas $(DIFDIR)/$*.pas   > $*.dif

%.dif : %.msg
	-$(DIFF) $(DIFFOPTS) $*.msg $(DIFDIR)/$*.msg   > $*.dif

%.dif : %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(DIFDIR)/$*.inc   > $*.dif

Makefile.dif : Makefile
	-$(DIFF) $(DIFFOPTS) Makefile $(DIFDIR)/Makefile   > Makefile.dif

%.new : %.pas %.dif
	-copy /y $*.pas $*.new
	-patch $*.new $*.dif

%.new : %.msg %.dif
	-copy /y $*.msg $*.new
	-patch $*.new $*.dif

%.new : %.inc %.dif
	-copy /y $*.inc $*.new
	-patch $*.new $*.dif

Makefile.new : Makefile Makefile.dif
	-copy /y Makefile Makefile.new
	-patch Makefile.new Makefile.dif

%.d3p : %.pas
	-$(DIFF3) -m -E $*.pas $(REFDIR)/$*.pas $(DIFDIR)/$*.pas > $*.d3p

%.d3m : %.msg
	-$(DIFF3) -m -E $*.msg $(REFDIR)/$*.msg $(DIFDIR)/$*.msg > $*.d3m

%.d3i : %.inc
	-diff3 -m -E $*.inc $(REFDIR)/$*.inc $(DIFDIR)/$*.inc > $*.d3i

Makefile.di3: Makefile
	-diff3 -m -E Makefile $(REFDIR)/Makefile $(DIFDIR)/Makefile > Makefile.di3

diff : $(DIFFFILES)

diff3 : $(DIFF3FILES)

replacediff3 : diff3
	copy /y *.pas *.bkp
	copy /y *.inc *.bki
	copy /y *.msg *.bkm
	copy /y Makefile Makefile.old
	copy /y *.d3p *.pas
	copy /y *.d3i *.inc
	copy /y *.d3m *.msg
	copy /y Makefile.di3 Makefile

restorediff3 :
	copy /y *.bkp *.pas
	copy /y *.bki *.inc
	copy /y *.bkm *.msg
	copy /y Makefile.old Makefile


patch : $(PATCHFILES)

diffclean :
	-del *.dif
	-del *.d3*
	-del *.new

rtl :
	make -C $(UNITDIR) all

rtlclean :
	make -C $(UNITDIR) clean
# Test of log at the end
# does CVS add # at start of each line ??
# $Log$
# Revision 1.15  1998-06-03 09:33:39  michael
# added distclean target to remove ppc1-ppc3 too
#
# Revision 1.14  1998/06/03 09:27:51  michael
# Clean cannot remove ppc1-ppc3 : make cycle uses clean
#
# Revision 1.13  1998/06/02 11:29:36  peter
#   + make clean removes also ppc1 - ppc3
#
# Revision 1.12  1998/05/31 14:09:45  peter
#   * use mv -f instead of move /y whch is not dos6.2 compatible
#
# Revision 1.11  1998/05/06 14:03:27  michael
# + Make install doesn't stop if makecfg not found
#
# Revision 1.10  1998/04/23 13:21:46  michael
# Updated version number
#
# Revision 1.9	1998/04/15 14:16:48  michael
# + Removed CR characters
#
# Revision 1.8	1998/04/06 12:28:23  pierre
#   Test of makefile log !
#
# End of log
