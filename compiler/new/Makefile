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

# Message files
MSGFILES=$(wildcard error*.msg)


#####################################################################
# Defaults
#####################################################################

# Default message file
MSGFILE=error$(FPCLANG).msg

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# RTL
RTL=../../rtl
UTILS=../../utils

# Utils used by compiler development/installation
COMPILERUTILS=../utils

# At least set -Sg
override LOCALOPT+=-Sg

# set correct defines (-d$(CPU) is automaticly added in makefile.fpc)
override LOCALDEF+=-dGDB -dBROWSERLOG -dNEWCG -Fu.. -Fu. -Fi..

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

override COMPILER+=$(LOCALOPT) -Fu$(CPU)

ifneq (,$(findstring -s ,$(LOCALOPT)))
override EXECPPAS=@$(PPAS)
endif

# include OPT also for the RTL (also needed to pass the RELEASE options
# to the RTL !!)
override RTLOPTS+=$(OPT)


#####################################################################
# Setup Targets
#####################################################################

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
MAKEDEP=ppdep$(EXEEXT)
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
	$(MAKE) -C $(RTLDIR) 'PP=$(PP)' 'OPT=$(RTLOPTS)' all
	$(MAKE) clean
	$(MAKE) all
endif

clean : testfpcmake execlean fpc_cleanall        
	-$(DEL) ../*$(PPUEXT)

execlean :
	-$(DEL) $(EXENAME)

distclean: clean
	-$(DEL) $(TEMPNAME) $(TEMPNAME1) $(TEMPNAME2) $(TEMPNAME3) $(MSG2INC)


#####################################################################
# Include depencies
#####################################################################

$(MAKEDEP): $(UTILS)/ppdep.pp
	$(COMPILER) $(UTILS)/ppdep.pp
	$(COPY) $(UTILS)/$(MAKEDEP) $(MAKEDEP)

dependencies : $(MAKEDEP)
	$(MAKEDEP) pp.pas $(PPOPTDEF) $(LOCALDEF) '-F$$(COMPILER) $$(LOCALOPT)' > depend

ifdef USEDEPEND

include depend

endif


#####################################################################
# Make targets
#####################################################################

$(MSG2INC): $(COMPILERUTILS)/msg2inc.pp
	$(COMPILER) -FE. $(COMPILERUTILS)/msg2inc.pp

# The msgtxt.inc only depends on the error?.msg file, not on msg2inc,
# because that one will be new almost everytime
msgtxt.inc: $(MSGFILE)
	$(MAKE) $(MSG2INC)
	$(MSG2INC) $(MSGFILE) msg msg

msg: msgtxt.inc

# Make only the compiler
ifndef COMPLETE
$(EXENAME) : $(wildcard *.pas) $(wildcard *.inc) msg
	$(COMPILER) pp.pas
	$(EXECPPAS)
	$(MOVE) $(PPEXENAME) $(EXENAME)
else
$(EXENAME) : $(wildcard *.pas) $(wildcard *.inc) msg
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
	$(MAKE) 'PP=$(BASEDIR)/$(TEMPNAME)' all

remake3: $(TEMPNAME3)
	$(MAKE) 'PP=$(BASEDIR)/$(TEMPNAME3)' 'OLDPP=$(BASEDIR)/$(TEMPNAME2)' next
	$(DIFF) $(TEMPNAME3) $(EXENAME)

$(TEMPNAME1) : $(EXENAME)
	-$(DEL) $(TEMPNAME1)
	$(MOVE) $(EXENAME) $(TEMPNAME1)

$(TEMPNAME2) : $(TEMPNAME1)
	$(MAKE) 'PP=$(BASEDIR)/$(TEMPNAME1)' 'OLDPP=' next
	-$(DEL) $(TEMPNAME2)
	$(MOVE) $(EXENAME) $(TEMPNAME2)

$(TEMPNAME3) : $(TEMPNAME2)
	$(MAKE) 'PP=$(BASEDIR)/$(TEMPNAME2)' 'OLDPP=$(BASEDIR)/$(TEMPNAME1)' next
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
	$(INSTALLEXE) $(COMPILERUTILS)/samplecfg $(LIBINSTALLDIR)/samplecfg
endif
	$(MKDIR) $(MSGINSTALLDIR)
	$(INSTALL) $(MSGFILES) $(MSGINSTALLDIR)

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
# Revision 1.2  1999-08-02 17:16:41  michael
# + Changes for alpha
#
# Revision 1.1  1999/08/01 23:19:58  florian
#   + make a new makefile using the old compiler makefile
#
#
