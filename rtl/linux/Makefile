#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Linux Runtime Library
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

# Warning: this file contains TAB (#9) characters that are required for
# make. Make sure you use an editor that does not replace TABs with
# spaces, or the makefile won't work anymore after you save.

#####################################################################
# Makefile Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=linux
override CPU=i386

# Where are the include files
RTL=..
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)

# Where to place the result files
TARGETDIR=.

# These units belong to the RTL
UNITPREFIX=rtl


#####################################################################
# Own defaults
#####################################################################

# Paths
OBJPASDIR=$(RTL)/objpas

# Define Go32v2 Units
SYSTEMUNIT=syslinux


# Define Loaders
ifdef AOUT
PRT=prt0
else
PRT=prt1
endif

LOADERAS=$(CPU)/$(PRT).as
GLOADERAS=$(CPU)/g$(PRT).as

LOADEROBJECTS=prt0 gprt0

ifndef AOUT
LOADEROBJECTS+=cprt0 cprt21 gprt21
endif


# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) objpas strings \
            linux ports \
            dos crt objects printer graph \
            sysutils typinfo math \
            cpu mmx getopts heaptrc \
            errors sockets graph


#####################################################################
# Common targets
#####################################################################

.PHONY: all clean install info \
	staticlib sharedlib libsclean \
	staticinstall sharedinstall libinstall \

all: testfpcmake fpc_all

clean: testfpcmake fpc_clean

install: testfpcmake fpc_install

info: testfpcmake fpc_info

staticlib: testfpcmake fpc_staticlib

sharedlib: testfpcmake fpc_sharedlib

libsclean: testfpcmake fpc_libsclean

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
# Include system unit dependencies
#####################################################################

SYSTEMPPU=$(addsuffix $(PPUEXT),$(SYSTEMUNIT))

# Get the system independent include file names.
# This will set the following variables :
# SYSINCNAMES
include $(INC)/makefile.inc
SYSINCDEPS=$(addprefix $(INC)/,$(SYSINCNAMES))

# Get the processor dependent include file names.
# This will set the following variables :
# CPUINCNAMES
include $(PROCINC)/makefile.cpu
SYSCPUDEPS=$(addprefix $(PROCINC)/,$(CPUINCNAMES))

# Put system unit dependencies together.
SYSDEPS=$(SYSINCDEPS) $(SYSCPUDEPS)


#####################################################################
# Dependencies
#####################################################################

# Options needed
ifneq ("$(FPC_VERSION)","0.99.10")
ifndef BROWSER
NEEDOPT=-b-
endif
endif 

vpath %$(PASEXT) $(INC) $(PROCINC)

#
# Loaders
#

prt0$(OEXT) : $(LOADERAS)
	$(AS) -o prt0$(OEXT) $(LOADERAS)

gprt0$(OEXT) : $(GLOADERAS)
	$(AS) -o gprt0$(OEXT) $(GLOADERAS)

ifndef AOUT

cprt0$(OEXT) : $(CPU)/cprt1.as
	$(AS) -o cprt0$(OEXT) $(CPU)/cprt1.as

cprt21$(OEXT) : $(CPU)/cprt21.as
	$(AS) -o cprt21$(OEXT) $(CPU)/cprt21.as 

# still need to use gprt1, because gprt21 crashes
gprt21$(OEXT) : $(CPU)/gprt1.as
	$(AS) -o gprt21$(OEXT) $(CPU)/gprt1.as

endif

#
# System Units (System, Objpas, Strings)
#

$(SYSTEMPPU) : syslinux.pp sysconst.inc systypes.inc syscalls.inc $(SYSDEPS)
	$(COMPILER) -Us -Sg syslinux.pp $(REDIR)

objpas$(PPUEXT): $(OBJPASDIR)/objpas.pp $(INC)/except.inc $(SYSTEMPPU)
	$(COMPILER) -I$(OBJPASDIR) $(OBJPASDIR)/objpas.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc\
		   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc\
		   $(SYSTEMPPU)

#
# System Dependent Units
#

linux$(PPUEXT) : linux.pp strings$(PPUEXT) $(INC)/textrec.inc $(INC)/filerec.inc \
		 syscalls.inc systypes.inc sysconst.inc $(SYSTEMPPU)

ports$(PPUEXT) : ports.pp linux$(PPUEXT) objpas$(PPUEXT)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc strings$(PPUEXT) \
	       linux$(PPUEXT) $(SYSTEMPPU)

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

printer$(PPUEXT) : printer.pp $(INC)/textrec.inc linux$(PPUEXT) $(SYSTEMPPU)

graph$(PPUEXT) : graph.pp linux$(PPUEXT) objects$(PPUEXT)

#
# Delphi Compatible Units
#

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(wildcard $(OBJPASDIR)/*.inc) \
                    filutil.inc disk.inc objpas$(PPUEXT) linux$(PPUEXT)
	$(COMPILER) -I$(OBJPASDIR) $(OBJPASDIR)/sysutils.pp $(REDIR)

typinfo$(PPUEXT): $(OBJPASDIR)/typinfo.pp objpas$(PPUEXT)
	$(COMPILER) -Sg $(OBJPASDIR)/typinfo.pp $(REDIR)

math$(PPUEXT): $(OBJPASDIR)/math.pp objpas$(PPUEXT) sysutils$(PPUEXT)
	$(COMPILER) $(OBJPASDIR)/math.pp $(REDIR)

#
# Other system-independent RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COMPILER) -Sg $(INC)/heaptrc.pp $(REDIR)

#
# Other system-dependent RTL Units
#

sockets$(PPUEXT) : sockets.pp $(INC)/textrec.inc $(INC)/filerec.inc \
		   linux$(PPUEXT) $(SYSTEMPPU)

errors$(PPUEXT) : errors.pp strings$(PPUEXT) $(SYSTEMPPU)

ipc$(PPUEXT) : ipc.pp linux$(PPUEXT) $(SYSTEMPPU)


#
# $Log$
# Revision 1.26  1999-05-28 11:21:06  peter
#   * moved fpc_version check so it works correct
#
# Revision 1.25  1999/05/13 07:39:07  michael
# + Fix in heaptrc target: needs -Sg
#
# Revision 1.24  1999/05/05 22:24:08  peter
#   * 0.99.10 check for browser
#
# Revision 1.23  1999/05/04 11:59:45  peter
#   * browser off by default
#
# Revision 1.22  1999/05/03 23:30:28  peter
#   * small update
#   * uses gprt1 again for gprt21 becuase gprt21.as crashes
#
# Revision 1.21  1999/05/03 21:29:35  peter
#   + glibc 2.1 support
#
# Revision 1.20  1999/04/22 10:56:32  peter
#   * fixed sysutils dependencys
#   * objpas files are agian in the main Makefile, makefile.op is obsolete
#
# Revision 1.19  1999/04/14 09:07:17  peter
#   * fixed strings dependency
#
# Revision 1.18  1999/03/22 13:09:10  peter
#   - ipc unit because it crashes on 0.99.10
#
# Revision 1.17  1999/03/16 00:47:09  peter
#   * makefile.fpc targets start with fpc_
#   * small updates for install scripts
#
# Revision 1.16  1999/03/12 21:07:51  michael
# +  clean and libsclean added
#
# Revision 1.15  1999/03/09 01:35:55  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
