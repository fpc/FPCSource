#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for the Free Pascal Examples
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#####################################################################
# Include configuration makefile
#####################################################################

# Where are the include files ?
CFG=../cfg
#INC=../inc
#PROCINC=../$(CPU)
#OBJPAS=../objpas

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

EXEOBJECTS=hello lines eratos magic qsort mandel blackbox
UNITOBJECTS=

#####################################################################
# Main targets
#####################################################################

# Create Filenames
EXEFILES=$(addsuffix $(EXEEXT),$(EXEOBJECTS))
UNITFILES=$(addsuffix $(PPUEXT),$(UNITOBJECTS))
UNITOFILES=$(addsuffix $(OEXT),$(UNITOBJECTS))

.PHONY : all clean diffs install diffclean

all : $(EXEFILES) $(UNITFILES)

$(EXEFILES): %$(EXEEXT): %$(PASEXT)
	$(PP) $(OPT) $* 

$(UNITFILES): %$(PPUEXT): %$(PASEXT)
	$(PP) $(OPT) $* 

install : all
ifdef EXEOBJECTS
	$(MKDIR) $(BININSTALLDIR)
	$(INSTALLEXE) $(EXEFILES) $(BININSTALLDIR)
endif
ifdef UNITOBJECTS
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) $(UNITFILES) $(UNITOFILES) $(UNITINSTALLDIR)
endif

clean:
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) $(PPAS) link.res log
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
# Revision 1.1  1998-09-11 10:55:20  peter
#   + header+log
#
# Revision 1.1  1998/09/10 13:55:07  peter
#   * updates
#
#
