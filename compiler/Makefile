#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1993-99 by the Free Pascal Development Team
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
# Configuration section
#####################################################################

ifndef FPCLANG
FPCLANG=e
endif

# Local defines for the compiler only
ifndef LOCALDEF
LOCALDEF=
endif

# Local options for the compiler only
ifndef LOCALOPT
LOCALOPT=
endif

# Options for the RTL only when cycling
ifndef RTLOPTS
RTLOPTS=
endif


#####################################################################
# Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=..

# RTL
RTL=../rtl

# At least set -Sg
override LOCALOPT+=-Sg

# set correct defines (-d$(CPU) is automaticly added in makefile.fpc)
override LOCALDEF+=-dGDB -dBROWSERLOG

override LOCALOPT+=$(LOCALDEF)


#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
	staticlib sharedlib \
	staticinstall sharedinstall libinstall \

all: testfpcmake myall

info: testfpcmake fpc_info

staticlib: testfpcmake fpc_staticlib

sharedlib: testfpcmake fpc_sharedlib

staticinstall: testfpcmake fpc_staticinstall

sharedinstall: testfpcmake fpc_sharedinstall

libinstall: testfpcmake fpc_libinstall


#####################################################################
# Include default makefile
#####################################################################

# test if FPCMAKE is still valid
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
ifeq ($(FPCMAKE),)
testfpcmake:
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@exit
else
include $(FPCMAKE)
testfpcmake:
endif


#####################################################################
# Setup Defs/Opts which depend on the makefile.fpc
#####################################################################

# for i386 also insert MMX support
ifneq ($(findstring -di386,$(COMPILER)),)
override COMPILER+=-dSUPPORT_MMX
endif

override COMPILER+=$(LOCALOPT)

ifneq (,$(findstring -s ,$(LOCALOPT)))
override EXECPPAS=@$(PPAS)
endif

# include OPT also for the RTL (also needed to pass the RELEASE options
# to the RTL !!)
override RTLOPTS+=$(OPT)


#####################################################################
# Setup Targets
#####################################################################

MSGFILE=error$(FPCLANG).msg

ifeq ($(OS_TARGET),win32)
ifdef CMP
override DIFF:=$(CMP) -i138
endif
endif

# Used to avoid unnecessary steps in remake3
ifdef DIFF
ifdef OLDPP
DIFFRESULT:=$(shell $(DIFF) $(OLDPP) $(PP))
else
DIFFRESULT=Not equal
endif
else
DIFFRESULT=No diff program
endif


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
MSG2INC=msg2inc$(EXEEXT)


#####################################################################
# Default makefile
#####################################################################

myall: $(EXENAME)
	$(MAKE) echotime

ifeq ($(MAKELEVEL),0)
ifndef STARTTIME
ifdef DATE
STARTTIME:=$(shell $(DATE) +%T)
else
STARTTIME:=unknown
endif
endif
endif

export STARTTIME

ifdef DATE
ENDTIME=$(shell $(DATE) +%T)
else
ENDTIME:=unknown
endif

echotime:
	@echo Start $(STARTTIME) now $(ENDTIME)

ifndef DIFFRESULT
next :
	@echo $(OLDPP) and $(PP) are equal
	$(COPY) $(PP) $(EXENAME)
else
next :
	$(MAKE) execlean
	$(MAKE) -C $(RTLDIR) libsclean
	$(MAKE) -C $(RTLDIR) 'PP=$(BASEDIR)/$(PP)' 'OPT=$(RTLOPTS)' all
	$(MAKE) clean
	$(MAKE) all
endif

clean : testfpcmake execlean fpc_cleanall

execlean :
	-$(DEL) $(EXENAME)

distclean: clean
	-$(DEL) $(TEMPNAME) $(TEMPNAME1) $(TEMPNAME2) $(TEMPNAME3)


#####################################################################
# Include depencies
#####################################################################

$(MAKEDEP): $(RTL)/utils/mkdep.pp
	$(COMPILER) $(RTL)/utils/mkdep.pp
	$(COPY) $(RTL)/utils/$(MAKEDEP) $(MAKEDEP)

dependencies : $(MAKEDEP)
	$(MAKEDEP) pp.pas $(PPOPTDEF) $(LOCALDEF) '-F$$(COMPILER) $$(LOCALOPT)' > depend

ifdef USEDEPEND

include depend

endif


#####################################################################
# Make targets
#####################################################################

$(MSG2INC): $(RTL)/utils/msg2inc.pp
	$(COMPILER) -FE. $(RTL)/utils/msg2inc.pp

msgtxt.inc: $(MSGFILE) $(MSG2INC)
	$(MSG2INC) $(MSGFILE) msg msg

msg: msgtxt.inc

# Make only the compiler
ifndef COMPLETE
$(EXENAME) : $(wildcard *.pas) $(wildcard *.inc)
	$(COMPILER) pp.pas
	$(EXECPPAS)
	$(MOVE) $(PPEXENAME) $(EXENAME)
else
$(EXENAME) : $(wildcard *.pas) $(wildcard *.inc)
	$(COMPILER) $(LOCALOPT) pp.pas
	$(EXECPPAS)
	$(COMPILER) $(LOCALOPT) pp.pas
	$(EXECPPAS)
	$(COMPILER) $(LOCALOPT) pp.pas
	$(EXECPPAS)
	$(MOVE) $(PPEXENAME) $(EXENAME)
endif

# This target remakes the units with the currently made version
remake: $(EXENAME)
	$(MOVE) $(EXENAME) $(TEMPNAME)
	$(MAKE) execlean
	$(MAKE) -C $(RTLDIR) libsclean
	$(MAKE) clean
	$(MAKE) 'PP=./$(TEMPNAME)' all

remake3: $(TEMPNAME3)
	$(MAKE) 'PP=./$(TEMPNAME3)' 'OLDPP=./$(TEMPNAME2)' next
	$(DIFF) $(TEMPNAME3) $(EXENAME)

$(TEMPNAME1) : $(EXENAME)
	-$(DEL) $(TEMPNAME1)
	$(MOVE) $(EXENAME) $(TEMPNAME1)

$(TEMPNAME2) : $(TEMPNAME1)
	$(MAKE) 'PP=./$(TEMPNAME1)' 'OLDPP=' next
	-$(DEL) $(TEMPNAME2)
	$(MOVE) $(EXENAME) $(TEMPNAME2)

$(TEMPNAME3) : $(TEMPNAME2)
	$(MAKE) 'PP=./$(TEMPNAME2)' 'OLDPP=./$(TEMPNAME1)' next
	-$(DEL) $(TEMPNAME3)
	$(MOVE) $(EXENAME) $(TEMPNAME3)

cycle: testfpcmake
	$(MAKE) clean
	$(MAKE) -C $(RTLDIR) libsclean
	$(MAKE) -C $(RTLDIR) 'OPT=$(RTLOPTS)' all
	$(MAKE) remake3
	$(MAKE) echotime

cycledep: testfpcmake
	$(MAKE) cycle USEDEPEND=1

cvstest: testfpcmake
	$(MAKE) cycle 'LOCALOPT=-n -Se' 'RTLOPTS=-n -Se'


#####################################################################
# Installation
#####################################################################

# This will only install the ppc386.exe, not the message files etc.
quickinstall: testfpcmake
ifdef inlinux
	$(MKDIR) $(LIBINSTALLDIR)
	$(INSTALLEXE) $(EXENAME) $(LIBINSTALLDIR)
else
	$(MKDIR) $(BININSTALLDIR)
ifdef UPX
	-$(UPX) $(EXENAME)
endif
	$(INSTALLEXE) $(EXENAME) $(BININSTALLDIR)
endif

installlib: quickinstall
ifdef inlinux
	chmod 755 makecfg
	makecfg $(LIBINSTALLDIR)/samplecfg
endif
	$(MKDIR) $(MSGINSTALLDIR)
	$(INSTALL) errore.msg errorn.msg $(MSGINSTALLDIR)

# this also installs the link /usr/bin/ppc386. The .deb does that later
install: testfpcmake installlib
ifdef inlinux
	$(MKDIR) $(BININSTALLDIR)
	ln -sf $(LIBINSTALLDIR)/ppc386 $(BININSTALLDIR)/ppc386
endif


#####################################################################
# Misc
#####################################################################

rtl :
	$(MAKE) -C $(RTLDIR) all

rtlclean :
	$(MAKE) -C $(RTLDIR) libsclean


#####################################################################
# local user configurable file
# in makefile.loc you can add any desired target
#####################################################################

localmake:=$(strip $(wildcard makefile.loc))

ifdef localmake
include ./$(localmake)
endif


#####################################################################
# M68k test targets
#####################################################################

# just a quick way to get ppc68k
# needs to be after makefile.def for PASFILES INCFILES

$(M68KEXENAME): $(PASFILES) $(INCFILES)
	$(MAKE) clean
	$(PP) -uI386 -uSUPPORT_MMX -dm68k -o$(M68KEXENAME) pp
	$(MAKE) clean

#
# $Log$
# Revision 1.27  1999-04-26 13:31:24  peter
#   * release storenumber,double_checksum
#
# Revision 1.26  1999/04/09 10:14:19  peter
#   * renamed language -> fpclang
#
# Revision 1.25  1999/04/07 15:26:50  pierre
#  + added COMPLETE env for triple compilation test
#
# Revision 1.24  1999/04/01 22:05:16  peter
#   * only 'make' now does a make all instead of make info
#
# Revision 1.23  1999/03/24 23:12:54  peter
#   * use ./ before tempnames
#
# Revision 1.22  1999/03/16 00:46:56  peter
#   * makefile.fpc targets start with fpc_
#   * small updates for install scripts
#
# Revision 1.21  1999/03/12 17:24:43  michael
# + Fixed clean target
#
# Revision 1.20  1999/03/11 17:54:01  peter
#   * better check for makefile.fpc
#   * check if cmp exists
#
# Revision 1.19  1999/03/10 12:30:12  pierre
#  + override EXECPPAS if LOCALOPT contains win32
#
# Revision 1.18  1999/03/09 01:35:48  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
