#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for the <Template>
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

# Where are the include files ?
RTL=..
CFG=$(RTL)/cfg
#INC=$(RTL)/inc
#PROCINC=$(RTL)/$(CPU)


#####################################################################
# Include configuration makefile
#####################################################################

# Get some defaults for Programs and OSes.
# This will at least set the following variables :
# inlinux COPY REPLACE DEL INSTALL INSTALLEXE MKDIR
# INSTALLDIR UNITDIR PPOPT PP CPU COMPILER
include $(CFG)/makefile.cfg


#####################################################################
# Objects
#####################################################################

EXEOBJECTS=
UNITOBJECTS=


#####################################################################
# Main targets
#####################################################################

# Create Filenames
EXEFILES=$(addsuffix $(EXEEXT),$(EXEOBJECTS))
UNITFILES=$(addsuffix $(PPUEXT),$(UNITOBJECTS))
UNITOFILES=$(addsuffix $(OEXT),$(UNITOBJECTS))

.PHONY : all clean diffs install diffclean


all : $(EXEFILES) $(UNITFILES) targetdir

$(EXEFILES): %$(EXEEXT): %$(PASEXT)
	$(COMPILER) $*

$(UNITFILES): %$(PPUEXT): %$(PASEXT)
	$(COMPILER) $*


targetdir:
ifdef UNITTARGETDIR
ifdef EXEOBJECTS
	$(COPY) $(EXEFILES) $(BINTARGETDIR)
endif
endif
ifdef UNITTARGETDIR
ifdef UNITOBJECTS
	$(COPY) $(UNITFILES) $(UNITTARGETDIR)
ifeq ($(SMARTLINK),YES)
	$(COPY) $(LIBPREFIX)$(LIBNAME)$(LIBEXT) $(UNITTARGETDIR)
else
	$(COPY) $(UNITOFILES) $(UNITTARGETDIR)
endif
endif
endif


install : all
ifdef EXEOBJECTS
	$(MKDIR) $(BININSTALLDIR)
	$(INSTALLEXE) $(EXEFILES) $(BININSTALLDIR)
endif
ifdef UNITOBJECTS
	$(MKDIR) $(UNITINSTALLDIR)
ifeq ($(SMARTLINK),YES)
	$(INSTALL) $(LIBPREFIX)$(LIBNAME)$(LIBEXT) $(UNITINSTALLDIR)
else
	$(INSTALL) $(UNITFILES) $(UNITOFILES) $(UNITINSTALLDIR)
endif
endif


clean:
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) *$(STATICLIBEXT) *$(SHAREDLIBEXT) $(PPAS) link.res log
	-$(DELTREE) *$(SMARTEXT)
ifdef EXEOBJECTS
	-$(DEL) $(EXEFILES)
endif


#####################################################################
# Files
#####################################################################


#####################################################################
# Default makefile targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.2  1998-09-16 16:47:30  peter
#   * merged fixes
#
# Revision 1.1.2.1  1998/09/16 16:17:53  peter
#   * updates to install with go32,win32
#
#
