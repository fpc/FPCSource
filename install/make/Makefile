#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for <template>
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

# Where are the include files located
INC=
PROCINC=
OSINC=

# Needed options, without it won't compile
NEEDOPT=

# Needed unit dir, which is searched as the first path
NEEDUNITDIR=

# Where need we to place the executables/ppu/objects
TARGETDIR=
UNITTARGETDIR=

# As default make only the units
#DEFAULTUNITS=1


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=


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


#
# $Log$
# Revision 1.4  1999-01-19 18:21:51  peter
#   * Use FPCDIR and FPCMAKE environment to find makefile.fpc
#   * better install dir setting
#
#
#
