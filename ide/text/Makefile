#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for Free Pascal Environment
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#####################################################################
# Defaults
#####################################################################

ifdef inlinux
LIBDIR=/usr/lib/gcc-lib/i486-linux/2.7.2.3
endif

# We always need the API and FV
NEEDUNITDIR=../../api ../../fv
# Adding these is not possible, because it will then find views.pas before
# it finds views.ppu !! (PFV)
# ../../fv/base ../../fv/app ../../fv/dialogs

# when making a full version include the compiler
ifdef FULL
override NEEDUNITDIR+=../../compiler
override NEEDOPT+=-dBrowserCol -Sg
else
override NEEDUNITDIR+=../fake/compiler
endif

# when including debugger include the gdbinterface
ifndef GDBINT
GDBINT=gdbint
endif

ifdef GDB
override NEEDUNITDIR+=../../$(GDBINT)
NEEDLIBDIR+=../../$(GDBINT)/libgdb
NEEDOBJDIR+=../../$(GDBINT)/libgdb
else
override NEEDUNITDIR+=../fake/gdb
endif



#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=fp


#####################################################################
# Include default makefile
#####################################################################

ifndef FPCMAKE
ifdef FPCDIR
FPCMAKE=$(FPCDIR)/makefile.fpc
else
FPCMAKE=makefile.fpc
endif
endif

override FPCMAKE:=$(strip $(wildcard $(FPCMAKE)))
ifeq ($(FPCMAKE),)
nofpcmake:
	@echo
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@echo
	@exit
else
include $(FPCMAKE)
endif


#####################################################################
# Dependencies
#####################################################################

fp$(EXEEXT): $(wildcard *.pas) $(wildcard *.inc)

full:
	make all FULL=1

fullgdb: clean_compiler
	make all FULL=1 GDB=1

clean_compiler:
	make -C ../../compiler clean

#
# $Log$
# Revision 1.8  1999-02-04 17:19:23  peter
#   * linux fixes
#
# Revision 1.7  1999/02/04 13:32:00  pierre
#   * Several things added (I cannot commit them independently !)
#   + added TBreakpoint and TBreakpointCollection
#   + added cmResetDebugger,cmGrep,CmToggleBreakpoint
#   + Breakpoint list in INIFile
#   * Select items now also depend of SwitchMode
#   * Reading of option '-g' was not possible !
#   + added search for -Fu args pathes in TryToOpen
#   + added code for automatic opening of FileDialog
#     if source not found
#
# Revision 1.6  1999/01/29 10:34:31  peter
#   + needobjdir,needlibdir
#
# Revision 1.5  1999/01/28 19:58:23  peter
#   * makefile updates
#
# Revision 1.4  1999/01/22 18:10:42  pierre
#  * added missing directories
#
# Revision 1.3  1999/01/21 11:54:09  peter
#   + tools menu
#   + speedsearch in symbolbrowser
#   * working run command
#
# Revision 1.2  1999/01/19 18:21:49  peter
#   * Use FPCDIR and FPCMAKE environment to find makefile.fpc
#   * better install dir setting
#
#
