#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for the Free Pascal Objpas RTL
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

# Where are the include files
RTL=..
CFG=$(RTL)/cfg
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)
OBJPASDIR=.

# Libname is always objpas
override LIBNAME=objpas

# Don't forget the -S2 switch
override OPT+=-S2

#####################################################################
# Include configuration makefile
#####################################################################

# Get some defaults for Programs and OSes.
# This will set the following variables :
# inlinux COPY REPLACE DEL INSTALL INSTALLEXE MKDIR
# It will also set OPT for cross-compilation, and add required options.
# also checks for config file.
# it expects CFG INC PROCINC to be set !!
include $(CFG)/makefile.cfg

#####################################################################
# Objects
#####################################################################

EXEOBJECTS=
UNITOBJECTS=objpas sysutils math

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

objpas$(PPUEXT): objpas$(PASEXT) $(INC)/except.inc 

# Need to change language file !!
sysutils$(PPUEXT): objpas$(PPUEXT) sysutils$(PASEXT) sysstr.inc sysstrh.inc\
                   stre.inc syspch.inc syspchh.inc dati.inc datih.inc \
                   finah.inc fina.inc filutilh.inc

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
# Revision 1.7  1998-10-11 12:23:40  michael
# + More sysutils calls.
#
# Revision 1.6  1998/10/02 10:41:08  michael
# Included explicit dependencies for all targets
#
# Revision 1.5  1998/10/02 09:26:02  peter
#   * fixed rtl path
#
# Revision 1.4  1998/09/16 16:47:29  peter
#   * merged fixes
#
# Revision 1.3  1998/09/15 12:09:12  peter
#   * merged updates
#
# Revision 1.2.2.2  1998/09/16 16:17:51  peter
#   * updates to install with go32,win32
#
# Revision 1.2.2.1  1998/09/15 12:02:05  peter
#   * updates to get objpas using its own makefile
#
# Revision 1.2	1998/09/10 14:15:50  peter
#   - renamed makefile to Makefile
#
#
