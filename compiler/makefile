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

# first try go32v2 specific gdate
DATE=$(strip $(wildcard $(addsuffix /gdate.exe,$(subst ;, ,$(PATH)))))

# try generic date.exe
ifeq ($(DATE),)
DATE=$(strip $(wildcard $(addsuffix /date.exe,$(subst :, ,$(PATH)))))
endif

# finally try for linux
ifeq ($(DATE),)
DATE=$(strip $(wildcard $(addsuffix /date,$(subst :, ,$(PATH)))))
endif

#####################################################################
# Setup Targets
#####################################################################

# what target do we use
# currently dos go32v2 os2 and linux are available
ifndef TARGET
ifdef inlinux
TARGET=linux
else
TARGET=go32v2
endif
endif

# What processor do you want to compile for : i386 m68k (case sensitive !!)
ifndef CPU
CPU= i386
# CPU= m68k
endif

ifndef LANGUAGE
LANGUAGE=e
endif

#####################################################################
# Setup Files Directories
#####################################################################

# Set os-dependent files and extensions
ifdef inlinux
EXEEXT=
else
EXEEXT=.exe
endif

REPLACE=mv -f
REMOVE=rm
CP=cp -f

MSGFILE=error$(LANGUAGE).msg

COMPILERDIR=$(BASEDIR)
RTLDIR:=$(BASEDIR)/../rtl

# specify where units are.
# This needs to be set correctly for the 'remake' target to work !
ifndef UNITDIR
UNITDIR=$(RTLDIR)/$(TARGET)
ifeq ($(TARGET),go32v1)
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
LIBINSTALLDIR=/usr/lib/fpc/0.99.8
# for a.out
# LIBINSTALLDIR=/usr/lib/ppc/aout/0.99.8
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

DIFFEXIST:=$(shell $(DIFF) --help)

# Used to avoid unnecessary steps in remake3
ifdef DIFFEXIST
ifdef OLDPP
DIFFRESULT:=$(shell $(DIFF) $(OLDPP) $(PP))
else
DIFFRESULT=Not equal
endif
else
DIFFRESULT=No diff program
endif

# set correct defines (also needed by mkdep)
PPDEFS:=-d$(CPU) -dGDB -dFPC

# Set the needed compiler options
PPOPTS:=$(OPT) $(LOCALOPT) $(PPDEFS) -Sg -T$(TARGET)

# for win32 create at least 8 mb heap
ifdef Win32
PPOPTS:=$(PPOPTS) -Ch8000000
endif

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
M68KEXENAME=ppc68k$(EXEEXT)
TEMPNAME=ppc$(EXEEXT)
TEMPNAME1=ppc1$(EXEEXT)
TEMPNAME2=ppc2$(EXEEXT)
TEMPNAME3=ppc3$(EXEEXT)
MAKEDEP=mkdep$(EXEEXT)

PASFILES:=$(wildcard *.pas)
INCFILES:=$(wildcard *.inc)
MSGFILES:=$(wildcard *.msg)

#####################################################################
# Default makefile
#####################################################################

.SUFFIXES: .pas $(EXEEXT) .ppu .dif .d3p .d3i .d3m .new

.PHONY : all clean info \
	 cycle remake remake3 \
	 install next \
	 diff diff3 patch rtl toflor replacediff3 restorediff3 \
	 test rtlzip rtlclean \

# also call ppas if with command option -s 
ifeq (,$(findstring -s ,$(COMPILER)))
EXECPPAS=
else
EXECPPAS=@ppas
endif

.pas.ppu:
	$(COMPILER) $<
	$(EXECPPAS)

.pas$(EXEEXT):
	$(COMPILER) $<
	$(EXECPPAS)


all : $(EXENAME)
#	@echo Start $(STARTTIME) now $(ENDTIME)
# does not work because $(ENDTIME) is expanded
# before execution !!
	$(MAKE) echotime

ifeq ($(MAKELEVEL),0)
ifndef STARTTIME
STARTTIME:=$(shell $(DATE) +%T)
endif
endif

export STARTTIME

ENDTIME=$(shell $(DATE) +%T)

echotime:
	@echo Start $(STARTTIME) now $(ENDTIME)

ifndef DIFFRESULT
next :
	@echo $(OLDPP) and $(PP) are equal
	$(CP) $(PP) $(EXENAME)
else
next :
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR) 'PP=$(COMPILERDIR)/$(PP)' all
	$(MAKE) all
endif

clean :
	-rm -f *.o *.ppu *.ppw *.s $(EXENAME)

distclean: clean
	-rm -f $(TEMPNAME) $(TEMPNAME1) $(TEMPNAME2) $(TEMPNAME3)

#####################################################################
# Info
#####################################################################

info :
	@echo Target is $(TARGET)
	@echo Basedir is $(BASEDIR)
	@echo Unitdir is $(UNITDIR)
	@echo Compiler is $(COMPILER)
	@echo Execppas is $(EXECPPAS)
	@echo Pascal files are $(PASFILES)
	@echo Inc files are $(INCFILES)
	@echo Msg files are $(MSGFILES)

#####################################################################
# Include depencies (linux only)
#####################################################################

ifdef inlinux
$(MAKEDEP) : $(RTLDIR)/utils/mkdep.pp
	$(PP) $(RTLDIR)/utils/mkdep.pp
	$(CP) $(RTLDIR)/utils/$(MAKEDEP) $(MAKEDEP)

dependencies : $(MAKEDEP)
	$(MAKEDEP) pp.pas $(PPDEFS) '-F$$(COMPILER)' > depend

include depend

endif

#####################################################################
# Make targets
#####################################################################

msgtxt.inc: $(MSGFILE)
	$(RTLDIR)/utils/msg2inc $(MSGFILE) msg msg

msg: msgtxt.inc

# Make only the compiler
ifdef inlinux
$(EXENAME) : $(PPEXENAME)
	$(EXECPPAS)
	$(REPLACE) $(PPEXENAME) $(EXENAME)
else
$(EXENAME) : $(PASFILES) $(INCFILES) $(MSGFILES)
	$(COMPILER) pp.pas
	$(EXECPPAS)
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
	$(MAKE) 'PP=./$(TEMPNAME3)' 'OLDPP=./$(TEMPNAME2)' next
	diff $(TEMPNAME3) $(EXENAME)

$(TEMPNAME1) : $(EXENAME)
	-$(REMOVE) $(TEMPNAME1)
	$(REPLACE) $(EXENAME) $(TEMPNAME1)

$(TEMPNAME2) : $(TEMPNAME1)
	$(MAKE) 'PP=./$(TEMPNAME1)' 'OLDPP=' next
	-$(REMOVE) $(TEMPNAME2)
	$(REPLACE) $(EXENAME) $(TEMPNAME2)

$(TEMPNAME3) : $(TEMPNAME2)
	$(MAKE) 'PP=./$(TEMPNAME2)' 'OLDPP=./$(TEMPNAME1)' next
	-$(REMOVE) $(TEMPNAME3)
	$(REPLACE) $(EXENAME) $(TEMPNAME3)

cycle:
	$(MAKE) clean
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR)
	$(MAKE) remake3
	$(MAKE) echotime

ansirtl:
	$(MAKE) -C $(UNITDIR) clean
	$(MAKE) -C $(UNITDIR) PP=$(COMPILERDIR)/ppc386 \
	 OPT='-g -dUSeAnsiStrings' all

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
	$(MAKE) -C $(UNITDIR) all

rtlclean :
	$(MAKE) -C $(UNITDIR) clean

# just a quick way to get ppc68k
$(M68KEXENAME):
	$(MAKE) clean
	$(PP) -uI386 -uSUPPORT_MMX -dm68k -o$(M68KEXENAME) pp
	$(MAKE) clean

test:
	@echo time is $(STARTTIME) date.exe is $(DATE)

# Test of log at the end
# does CVS add # at start of each line ??
# $Log$
# Revision 1.31  1998-09-09 18:26:25  michael
# Spaces to tabs...
#
# Revision 1.30  1998/09/09 18:17:15  florian
#   * version number changed to 0.99.8
#
# Revision 1.29  1998/09/02 08:51:30  pierre
#   + added -s support must be defined as LOCALOPT
#     because it does not work for rtl building
#   + added some compilation time info
#
# Revision 1.28  1998/08/29 13:52:30  peter
#   + new messagefile
#   * merged optione.msg into errore.msg
#
# Revision 1.27  1998/08/21 15:16:56  peter
#   * win32 compiles a bit better, no growheap crash
#
# Revision 1.26  1998/08/21 14:08:46  pierre
#   + TEST_FUNCRET now default (old code removed)
#     works also for m68k (at least compiles)
#
# Revision 1.25  1998/08/18 09:24:41  pierre
#   * small warning position bug fixed
#   * support_mmx switches splitting was missing
#   * rhide error and warning output corrected
#
# Revision 1.24  1998/08/14 09:29:53  michael
# Updated compiler number
#
# Revision 1.23  1998/08/06 10:42:56  pierre
#   + faster remake3 :
#     stops when two executables are equal
#
# Revision 1.22  1998/07/23 18:17:58  michael
# + Added ansirtl target to make an ansistrings capable rtl
#
# Revision 1.21  1998/07/22 21:33:32  michael
# + COPY and Replace back to mv and cp
#
# Revision 1.20  1998/06/24 14:02:58  peter
#   * new depend for the new ra units
#
# Revision 1.19  1998/06/23 14:00:15  peter
#   * renamed RA* units
#
# Revision 1.18  1998/06/10 10:42:44  peter
#   * ifndef TARGET to allow target on commandline
#
# Revision 1.17  1998/06/08 09:22:02  michael
# fixed dependcies and rules. Make would not run any more after peters changes.
#
# Revision 1.16  1998/06/05 14:37:28  pierre
#   * fixes for inline for operators
#   * inline procedure more correctly restricted
#
# Revision 1.15  1998/06/03 09:33:39  michael
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
