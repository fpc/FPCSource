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
# Configuration section
#####################################################################

ifndef LANGUAGE
LANGUAGE=e
endif

ifndef CPU
CPU=i386
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

# Path to libgcc.a
ifndef GCCLIBPATH
GCCLIBPATH=/usr/lib/gcc-lib/i486-linux/2.7.2.3
endif


#####################################################################
# Defaults
#####################################################################

# Where is the makefile.cfg ?
CFG=../rtl/cfg

# At least set -Sg
override LOCALOPT+=-Sg

# set correct defines (-d$(CPU) is automaticly added in makefile.cfg)
override LOCALDEF+=-dGDB -dFPC

override LOCALOPT+=$(LOCALDEF)


#####################################################################
# Include default makefile
#####################################################################

include $(CFG)/makefile.cfg

# for i386 also insert MMX support
ifneq ($(findstring -di386,$(COMPILER)),)
override LOCALDEF+=-dSUPPORT_MMX
endif

# include OPT also for the RTL (also needed to pass the RELEASE options
# to the RTL !!)
override RTLOPTS+=$(OPT)


#####################################################################
# Setup Targets
#####################################################################


MSGFILE=error$(LANGUAGE).msg

DIFFEXIST:=$(shell $(DIFF) --help)

ifeq ($(OS_TARGET),win32) 
override DIFF=cmp -i138
endif

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

.SUFFIXES: .pas $(EXEEXT) .ppu .dif .d3p .d3i .d3m .new

.PHONY : all clean info \
	 cycle remake remake3 \
	 install next \
	 diff diff3 patch replacediff3 restorediff3 \
	 test rtl rtlclean \

# also call ppas if with command option -s
ifeq (,$(findstring -s ,$(COMPILER)))
EXECPPAS=
else
EXECPPAS=@$(PPAS)
endif

.pas$(PPUEXT):
	$(COMPILER) $(LOCALOPT) $<
	$(EXECPPAS)

.pas$(EXEEXT):
	$(COMPILER) $(LOCALOPT) $<
	$(EXECPPAS)


all : $(EXENAME)
#	@echo Start $(STARTTIME) now $(ENDTIME)
# does not work because $(ENDTIME) is expanded
# before execution !!
	make echotime

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
	$(COPY) $(PP) $(EXENAME)
else
next :
	make execlean
	make -C $(UNITDIR) libsclean
	make -C $(UNITDIR) 'PP=$(COMPILERDIR)/$(PP)' 'OPT=$(RTLOPTS)' all
	make clean
	make all
endif


clean :
	-$(DEL) *$(OEXT) *$(PPUEXT) *$(ASMEXT) *$(STATICLIBEXT) $(EXENAME)
	-$(DELTREE) $(SMARTEXT)

execlean :
	-$(DEL) $(EXENAME)

distclean: clean
	-$(DEL) $(TEMPNAME) $(TEMPNAME1) $(TEMPNAME2) $(TEMPNAME3)


#####################################################################
# Include depencies
#####################################################################

$(MAKEDEP): $(RTLDIR)/utils/mkdep.pp
	$(COMPILER) $(RTLDIR)/utils/mkdep.pp
	$(COPY) $(RTLDIR)/utils/$(MAKEDEP) $(MAKEDEP)

dependencies : $(MAKEDEP)
	$(MAKEDEP) pp.pas $(PPOPTDEF) $(LOCALDEF) '-F$$(COMPILER) $$(LOCALOPT)' > depend

ifdef USEDEPEND

include depend

endif


#####################################################################
# Make targets
#####################################################################

$(MSG2INC): $(RTLDIR)/utils/msg2inc.pp
	$(COMPILER) $(RTLDIR)/utils/msg2inc.pp
	$(COPY) $(RTLDIR)/utils/$(MSG2INC) $(MSG2INC)

msgtxt.inc: $(MSGFILE) $(MSG2INC)
	$(RTLDIR)/utils/$(MSG2INC) $(MSGFILE) msg msg

msg: msgtxt.inc

# Make only the compiler
ifdef inlinux
$(EXENAME) : $(PPEXENAME)
	$(MOVE) $(PPEXENAME) $(EXENAME)
else
$(EXENAME) : $(wildcard *.pas) $(wildcard *.inc)
	$(COMPILER) $(LOCALOPT) pp.pas
	$(EXECPPAS)
	$(MOVE) $(PPEXENAME) $(EXENAME)
endif

# This target remakes the units with the currently made version
remake: $(EXENAME)
	$(MOVE) $(EXENAME) $(TEMPNAME)
	make execlean
	make -C $(UNITDIR) libsclean
	make clean
	make 'PP=./$(TEMPNAME)' all

remake3: $(TEMPNAME3)
	make 'PP=./$(TEMPNAME3)' 'OLDPP=./$(TEMPNAME2)' next
	$(DIFF) $(TEMPNAME3) $(EXENAME)

$(TEMPNAME1) : $(EXENAME)
	-$(DEL) $(TEMPNAME1)
	$(MOVE) $(EXENAME) $(TEMPNAME1)

$(TEMPNAME2) : $(TEMPNAME1)
	make 'PP=./$(TEMPNAME1)' 'OLDPP=' next
	-$(DEL) $(TEMPNAME2)
	$(MOVE) $(EXENAME) $(TEMPNAME2)

$(TEMPNAME3) : $(TEMPNAME2)
	make 'PP=./$(TEMPNAME2)' 'OLDPP=./$(TEMPNAME1)' next
	-$(DEL) $(TEMPNAME3)
	$(MOVE) $(EXENAME) $(TEMPNAME3)

cycle:
	make clean
	make -C $(UNITDIR) libsclean
	make -C $(UNITDIR) 'OPT=$(RTLOPTS)' all
	make remake3
	make echotime

cycledep:
	make cycle USEDEPEND=1

cvstest:
	make cycle 'LOCALOPT=-n -Se' 'RTLOPTS=-n -Se'

#####################################################################
# Installation
#####################################################################

installlib:
ifdef inlinux
	$(MKDIR) $(LIBINSTALLDIR)
	$(INSTALLEXE) ppc386 $(LIBINSTALLDIR)
	chmod 755 makecfg
	makecfg $(LIBINSTALLDIR)/samplecfg
else
	$(MKDIR) $(BININSTALLDIR)
	$(INSTALLEXE) ppc386$(EXEEXT) $(BININSTALLDIR)
endif
	$(MKDIR) $(MSGINSTALLDIR)
	$(INSTALL) errore.msg errorn.msg $(MSGINSTALLDIR)

# this also installs the link /usr/bin/ppc386. The .deb does that later
install: installlib
ifdef inlinux
	$(MKDIR) $(BININSTALLDIR)
	ln -sf $(LIBINSTALLDIR)/ppc386 $(BININSTALLDIR)/ppc386
endif


#####################################################################
# Misc
#####################################################################

rtl :
	make -C $(UNITDIR) all

rtlclean :
	make -C $(UNITDIR) libsclean

ansirtl:
	make -C $(UNITDIR) libsclean
	make -C $(UNITDIR) PP=$(COMPILERDIR)/ppc386 OPT='-g -dUSeAnsiStrings' all

test:
	@echo time is $(STARTTIME) date.exe is $(DATE)

#####################################################################
# local user configurable file
# in makefile.loc you can add any desired target
#####################################################################

localmake:=$(strip $(wildcard makefile.loc))

ifdef localmake
include ./$(localmake)
endif


#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

# just a quick way to get ppc68k
# needs to be after makefile.def for PASFILES INCFILES

$(M68KEXENAME): $(PASFILES) $(INCFILES)
	make clean
	$(PP) -uI386 -uSUPPORT_MMX -dm68k -o$(M68KEXENAME) pp
	make clean

#
# $Log$
# Revision 1.17  1998-12-21 13:13:08  peter
#   * makecfg updated
#
# Revision 1.16  1998/11/16 15:30:19  pierre
#  + shorter cycle for win32 with cygnus b20
#
# Revision 1.15  1998/11/13 15:40:12  pierre
#   + added -Se in Makefile cvstest target
#   + lexlevel cleanup
#     normal_function_level main_program_level and unit_init_level defined
#   * tins_cache grown to A_EMMS (gave range check error in asm readers)
#     (test added in code !)
#   * -Un option was wrong
#   * _FAIL and _SELF only keyword inside
#     constructors and methods respectively
#
# Revision 1.14  1998/11/10 17:56:48  peter
#   * fixes to build correct again
#
# Revision 1.13  1998/10/22 23:55:24  peter
#   * dependencies,mkdep target is not linux only
#
# Revision 1.12  1998/10/22 19:15:46  peter
#   + VERBOSE=1 to set -vwni (necessary due the -n)
#   * fixed OPT, now also passed to RTLOPTS
#
# Revision 1.11  1998/10/12 12:20:37  pierre
#   + added tai_const_symbol_offset
#     for r : pointer = @var.field;
#   * better message for different arg names on implementation
#     of function
#
# Revision 1.10  1998/10/09 12:59:07  pierre
#   * MMX_SUPPORT used by default
#
# Revision 1.9	1998/10/09 11:07:44  peter
#   + cycledep
#
# Revision 1.8	1998/10/09 08:54:18  pierre
#   + added cvstest target
#     uses -n option to avoid use of local ppc386.cfg
#
# Revision 1.7	1998/09/29 18:34:45  peter
#   + chmod 755 makecfg
#
# Revision 1.6	1998/09/23 15:41:42  pierre
#  export for RTLOPTS (useful for make) !!
#
# Revision 1.5	1998/09/22 13:09:24  pierre
#   + added possibility to read makefile.loc if it exists
#     this allows simple addition of private rules
#   * restored EXENAME dependencies
#     PASFILES and INCFILES where not defined anymore !!
#
# Revision 1.4	1998/09/16 16:41:38  peter
#   * merged fixes
#
# Revision 1.1.2.2  1998/09/16 16:12:43  peter
#   * uses makefile.cfg
#
# Revision 1.3	1998/09/11 13:12:45  michael
# + makecfg has support for different kinds of units
#
# Revision 1.2	1998/09/11 12:27:04  pierre
#  added execlean target to be able to keep *.s files
#
# Revision 1.1.2.1  1998/09/11 12:07:51  pierre
#   + added execlean to be able to keep last *.s files
#
# Revision 1.1	1998/09/10 13:53:54  peter
#   * msg2inc compile works
#
# Revision 1.31  1998/09/09 18:26:25  michael
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
