#
#   $Id$
#   Copyright (c) 1998 by the Free Pascal Development Team
#
#   Makefile for demos
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

# Where are the files located
MAKEFILEFPC=makefile.fpc

# Where need we to place the executables/ppu/objects
TARGETDIR=.


#####################################################################
# Real targets
#####################################################################

UNITOBJECTS=
EXEOBJECTS=eratos qsort hello blackbox magic

ifeq ($(OS_TARGET),win32)
override EXEOBJECTS+=win32/winhello
else
override EXEOBJECTS+=mandel lines
endif

#####################################################################
# Include default makefile
#####################################################################

include $(MAKEFILEFPC)


#####################################################################
# Dependencies
#####################################################################


#
# $Log$
# Revision 1.2  1998-12-20 22:22:09  peter
#   * updates
#
# Revision 1.3  1998/12/12 19:14:42  peter
#   + DEFAULTUNITS to have a make all only compile the units
#
#
