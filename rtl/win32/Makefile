#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Win32 Runtime Library
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
# Defaults
#####################################################################

# Default place of the makefile.fpc
DEFAULTFPCDIR=../..

# set target and cpu which are required
override OS_TARGET=win32
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

# Define Win32 Units
SYSTEMUNIT=syswin32

# Loaders
LOADEROBJECTS=wprt0 wdllprt0

# Unit Objects
UNITOBJECTS=$(SYSTEMUNIT) strings windows \
	    dos objects \
	    objpas sysutils typinfo math \
	    cpu mmx getopts heaptrc

# Files used by windows.pp
WINDOWS_FILES=base errors defines \
	struct ascfun ascdef \
	unifun unidef func

WINDOWS_SOURCE_FILES=$(addsuffix .pp,$(WINDOWS_FILES))


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
nofpcmake:
	@echo makefile.fpc not found!
	@echo Check the FPCMAKE and FPCDIR environment variables.
	@exit
else
include $(FPCMAKE)
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

vpath %$(PASEXT) $(INC) $(PROCINC)

#
# Loaders
#

override AS=asw

wprt0$(OEXT) : wprt0.as
	$(AS) -o wprt0$(OEXT) wprt0.as

wdllprt0$(OEXT) : wdllprt0.as
	$(AS) -o wdllprt0$(OEXT) wdllprt0.as


#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syswin32.pp win32.inc $(SYSDEPS)
	$(COMPILER) -Us -Sg syswin32.pp $(REDIR)

strings$(PPUEXT) : $(INC)/strings.pp $(INC)/stringsi.inc\
		   $(PROCINC)/strings.inc $(PROCINC)/stringss.inc\
		   $(SYSTEMPPU)

#
# Delphi Object Model
#

include $(OBJPASDIR)/makefile.op


#
# System Dependent Units
#

base$(PPUEXT) : base.pp $(SYSTEMPPU)

messages$(PPUEXT) : messages.pp $(SYSTEMPPU)

defines$(PPUEXT) : defines.pp $(SYSTEMPPU)

windows$(PPUEXT) : windows.pp $(WINDOWS_SOURCE_FILES) $(SYSTEMPPU)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : dos.pp $(INC)/filerec.inc $(INC)/textrec.inc $(SYSTEMPPU)

objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)


#####################################################################
# windows.pp
#####################################################################

# Getting DLL names
# not present in headers !!

# first get the list of all exported function names
# uses pedump
# for system dll 's
# gdi32.exp will contain all exported functions names of gdi32.dll

WINDOWS_DIR:=c\:/windows

%.exp : $(WINDOWS_DIR)/system/%.dll
	pedump $< > $*.tmp
	sed -n -e "s/Addr:\(.*\)Ord:\(.*\)Name: \(.*\)/@\3@/p" $*.tmp > $*.exp
#	-rm $*.tmp

%.exd : $(WINDOWS_DIR)/system/%.drv
	pedump $< > $*.tmp
	sed -n -e "s/Addr:\(.*\)Ord:\(.*\)Name: \(.*\)/@\3@/p" $*.tmp > $*.exd
#	-rm $*.tmp

# list of usefull dll's for windows.pp
dllexps : gdi32.exp kernel32.exp advapi32.exp user32.exp mapi32.exp  \
	comdlg32.exp shell32.exp mpr.exp comctl32.exp version.exp \
	opengl32.exp spoolss.exp winspool.exd

# get a complete listing of all system dll's
allexps : $(notdir $(patsubst %.dll,%.exp,$(wildcard $(WINDOWS_DIR)/system/*.dll)) \
	$(patsubst %.drv,%.exd,$(wildcard $(WINDOWS_DIR)/system/*.drv)))

# extract the dllnames for which the real dll file is not
# known yet
# func.lst will contain all functions for which we still do
# not know the origin DLL
%.lst : %.pp
	@$(ECHO) listing DLL function names of $*.pp
	sed -n -e "s/\(.*\)External_library name '\([^']*\)'\(.*\)/\2/p" $*.pp > $*.lst

# get the DLL name from the listing in .exp files
# of the current target
define grepname
$(filter %.dll %.drv ,$(subst .exd:,.drv ,$(subst .exp:,.dll ,$(shell grep @$*@ *.ex*))))
endef

# creating of a sed script that
# will substitute all External_library
# by the real name of the DLL if found in exports files

# two stages
# because you cannot set a variable inside the commands
# Level 1 : set filename variable
# Level 2 :
%.sub : %.lst
	@$(ECHO) getting DLL file name for $*
	-rm $*.sub
	@$(ECHO) # Substitutions for $* >$*.sub
# call make for all names in lst file
# define LongList if there is an error
# because the list is too long
ifdef LongList
	$(foreach name,$(shell cat $*.lst),$(MAKE) subfile=$*.sub $(name).find ; )
else
	$(MAKE) subfile=$*.sub $(addsuffix .find,$(shell cat $*.lst))
endif
# resubstitute unfound ones !!
	@$(ECHO) s/external \'\' name \'\([^\']*\)\'/external\
	External_library name \'\1\'/ >>$*.sub
	@$(ECHO) # End of substitutions for $* >>$*.sub

# Change file according to function found in export
# list remaining unfound functions in $*.mis
%.npp : %.sub
	sed -f $*.sub $*.pp > $*.npp
	sed -n -e "s/\(.*\)External_library name \'\([^\']*\)\'\(.*\)/\2/p" $*.npp > $*.mis

%.find :
	@$(ECHO) $* is in $(grepname)
ifdef subfile
	@$(ECHO) s/external External_library name \'$*\'/external\
	\'$(filter %.dll %.drv ,$(subst .exd:,.drv ,$(subst .exp:,.dll ,$(shell grep @$*@ *.ex*))))\'\
	name \'$*\'/ >>$(subfile)
else
	@$(ECHO) external \
	\'$(filter %.dll %.drv ,$(subst .exd:,.drv ,$(subst .exp:,.dll ,$(shell grep @$*@ *.ex*))))\'\
	name $*
endif

GNUWIN32LIBDIR=./

%.find2 :
	@$(ECHO) s/In archive \(.*\)/\1/p >find.sed
	@$(ECHO) s/\(.*\)___imp_$*@\(.*\)/found: $*/p >>find.sed
ifdef subfile
	sed -n -f find.sed alllibs.sym >> $(subfile)
else
	sed -n -f find.sed alllibs.sym > $*.res
endif

missing : $(GNUWIN32LIBDIR)alllibs.sym $(addsuffix .lst,$(WINDOWS_FILES))
	-rm missing
	$(MAKE) subfile=missing $(addsuffix .find2,$(shell cat *.lst))

substmissing : missing
	dtou missing
	@$(ECHO) N > test.sed
	@$(ECHO) s/lib\(.*\)\.a:\nfound: \(.*\)/\1.dll : \2/p >> test.sed
	@$(ECHO) D >> test.sed
	sed -n -f test.sed missing > missing.tmp
	sed -e "s#\(.*\) : \(.*\)#s/external External_library name \'\2\'/external \'\1\' name \'\2\'/#" missing.tmp > missing.sub

dllnames:
	$(MAKE) $(addsuffix .lst,$(WINDOWS_FILES))

test:
	@$(ECHO) namelist of $(filename) is "$(namelist)"

# automatic conversion from ascfun.pp to ascdef.pp
# and unifun.pp to unidef.pp
# only if sed is present
ifdef SED
ascdef.pp : ascfun.pp ascdef.sed
	sed -f ascdef.sed ascfun.pp > ascdef.pp

unidef.pp : unifun.pp unidef.sed
	sed -f unidef.sed unifun.pp > unidef.pp
endif

#######################################################################
# CLEAN targets
#######################################################################

clean: fpc_clean

libsclean: fpc_libsclean


#
# $Log$
# Revision 1.22  1999-03-12 21:02:19  michael
# +  clean and libsclean added
#
# Revision 1.21  1999/03/10 23:45:21  peter
#   * AS is now always asw
#
# Revision 1.20  1999/03/09 01:35:56  peter
#   * makefile.fpc updates and defaultfpcdir var
#
#
