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

#####################################################################
# Start of configurable section.
# Please note that all these must be set in the main makefile, and
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

# What is the Operating System ?
ifndef OS_SOURCE
OS_SOURCE=win32
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=win32
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
endif

# What compiler to use ?
ifndef PP
PP=ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

# Where is the PPUMOVE program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

# Use smartlinking ?
ifndef SMARTLINK
SMARTLINK=NO
endif

# Name of library ?
# If this is set, all units will be put in the same library.
# If it is empty (default), the units will be left in separate files.
ifndef LIBNAME
LIBNAME=
#LIBNAME=fpc
endif

# Should the library be shared or static (only if LIBNAME is set).
# Set this to 'shared' or 'static' to create a librrary
# Setting this to shared will disable smart linking.
ifndef	LIBTYPE
LIBTYPE=
#LIBTYPE=static
endif

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

#####################################################################
# System independent
#####################################################################

# Where are the include files ?
INC=../inc
PROCINC=../$(CPU)
CFG=../cfg
OBJPASDIR=../objpas

# Get some defaults for Programs and OSes.
# This will set the following variables :
# inlinux indos COPY REPLACE DEL INSTALL INSTALLEXE MKDIR
# It will also set OPT for cross-compilation, and add required options.
# also checks for config file.
# it expects INC PROCINC to be set !!
include $(CFG)/makefile.cfg

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
# System dependent
#####################################################################

# Override extensions
PPUEXT=.ppw
SHAREDLIBEXT=.dll

# Define Linux Units
SYSTEMPPU=syswin32$(PPUEXT)
OBJECTS=strings objpas \
	dos \
        windows
#	base messages defines

# Files used by windows.pp
WINDOWS_FILES=base errors defines \
	struct ascfun ascdef \
	unifun unidef func

# Add Prefix and Suffixes
PPUOBJECTS=$(addsuffix $(PPUEXT), $(OBJECTS))

WINDOWS_SOURCE_FILES=$(addsuffix .pp,$(WINDOWS_FILES))

.PHONY : all install clean \
	 libs libsclean \
	 diffs diffclean \
	dllnames test

all : $(OBJLOADERS) $(PPUOBJECTS)

install : all
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(UNITINSTALLDIR)

clean :
	-$(DEL) *$(OEXT) *$(ASMEXT) *$(PPUEXT) log
	-$(DELTREE) *$(SMARTEXT)

#####################################################################
# Files
#####################################################################

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syswin32.pp win32.inc $(SYSDEPS)
	$(PP) $(OPT) -Us -Sg syswin32.pp $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(SYSTEMPPU)
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(PP) $(OPT) objpas $(REDIR)
	$(DEL) objpas.pp

#
# System Dependent Units
#

base$(PPUEXT) : base.pp $(SYSTEMPPU)
	$(PP) $(OPT) base.pp $(REDIR)

messages$(PPUEXT) : messages.pp $(SYSTEMPPU)
	$(PP) $(OPT) messages.pp $(REDIR)

defines$(PPUEXT) : defines.pp $(SYSTEMPPU)
	$(PP) $(OPT) defines.pp $(REDIR)

windows$(PPUEXT) : windows.pp $(WINDOWS_SOURCE_FILES) $(SYSTEMPPU)
	$(PP) $(OPT) windows.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : $(DOSDEPS) $(INC)/filerec.inc $(INC)/textrec.inc $(SYSTEMPPU)
	$(PP) $(OPT) dos $(REDIR)

#objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
#	 $(COPY) $(INC)/objects.pp .
#	 $(PP) $(OPT) objects $(REDIR)
#	 $(DEL) objects.pp

#
# Other RTL Units
#

#####################################################################
# Libs
#####################################################################

staticlib:
	make clean
	make all SMARTLINK=YES LIBNAME=fpc LIBTYPE=static

sharedlib:
	make clean
	make all
	$(PPUMOVE) -o fpc $(SHAREDLIBFILES)

staticlibinstall: staticlib
	$(MKDIR) $(STATIC_LIBINSTALLDIR)
	$(MKDIR) $(STATIC_UNITINSTALLDIR)
	$(INSTALLEXE) libfpc$(STATICLIBEXT) $(STATIC_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(STATIC_UNITINSTALLDIR)

sharedlibinstall: sharedlib
	$(MKDIR) $(SHARED_LIBINSTALLDIR)
	$(MKDIR) $(SHARED_UNITINSTALLDIR)
	$(INSTALLEXE) libfpc$(SHAREDLIBEXT) $(SHARED_LIBINSTALLDIR)
	$(INSTALL) *$(PPUEXT) *$(OEXT) $(SHARED_UNITINSTALLDIR)
	ldconfig

libinstall: staticlibinstall

libsclean : clean
	-$(DEL) *$(SMARTLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)

# Getting DLL names
# not present in headers !!

# first get the list of all exported function names
# uses pedump 
# for system dll 's
# gdi32.exp will contain all exported functions names of gdi32.dll

WINDOWS_DIR:=c:/windows

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
	@echo listing DLL function names of $*.pp
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
	@echo getting DLL file name for $*
	-rm $*.sub
	@echo # Substitutions for $* >$*.sub
# call make for all names in lst file
# define LongList if there is an error 
# because the list is too long
ifdef LongList
	$(foreach name,$(shell cat $*.lst),$(MAKE) subfile=$*.sub $(name).find ; )
else
	$(MAKE) subfile=$*.sub $(addsuffix .find,$(shell cat $*.lst))
endif
# resubstitute unfound ones !!
	@echo s/external \'\' name \'\([^\']*\)\'/external\
	External_library name \'\1\'/ >>$*.sub
	@echo # End of substitutions for $* >>$*.sub

# Change file according to function found in export
# list remaining unfound functions in $*.mis
%.npp : %.sub
	sed -f $*.sub $*.pp > $*.npp
	sed -n -e "s/\(.*\)External_library name \'\([^\']*\)\'\(.*\)/\2/p" $*.npp > $*.mis

%.find : 
	@echo $* is in $(grepname)
ifdef subfile
	@echo s/external External_library name \'$*\'/external\
	\'$(filter %.dll %.drv ,$(subst .exd:,.drv ,$(subst .exp:,.dll ,$(shell grep @$*@ *.ex*))))\'\
	name \'$*\'/ >>$(subfile)
else
	@echo external \
	\'$(filter %.dll %.drv ,$(subst .exd:,.drv ,$(subst .exp:,.dll ,$(shell grep @$*@ *.ex*))))\'\
	name $* 
endif

GNUWIN32LIBDIR=./

%.find2 :
	@echo s/In archive \(.*\)/\1/p >find.sed
	@echo s/\(.*\)___imp_$*@\(.*\)/found: $*/p >>find.sed
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
	@echo N > test.sed
	@echo s/lib\(.*\)\.a:\nfound: \(.*\)/\1.dll : \2/p >> test.sed
	@echo D >> test.sed
	sed -n -f test.sed missing > missing.tmp
	sed -e "s#\(.*\) : \(.*\)#s/external External_library name \'\2\'/external \'\1\' name \'\2\'/#" missing.tmp > missing.sub

dllnames:
	$(MAKE) $(addsuffix .lst,$(WINDOWS_FILES))

test:
	@echo namelist of $(filename) is "$(namelist)"

# automatic conversion from ascfun.pp to ascdef.pp
# and unifun.pp to unidef.pp
# only if sed is present 
ifdef SED
ascdef.pp : ascfun.pp ascdef.sed
	$(SED) -f ascdef.sed ascfun.pp > ascdef.pp

unidef.pp : unifun.pp unidef.sed
	$(SED) -f unidef.sed unifun.pp > unidef.pp
endif


#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.11  1998-09-04 12:33:12  pierre
#   + added SED testing for ascdef.pp and unidef.pp
#   * func.pp ready
#     still some functions missing (commented out for now)
#
# Revision 1.10  1998/09/03 18:17:35  pierre
#   * small improvements in number of found functions
#     all remaining are in func.pp
#
# Revision 1.9  1998/09/03 17:14:54  pierre
#   * most functions found in main DLL's
#     still some missing
#     use 'make dllnames' to get missing names
#
# Revision 1.7  1998/08/21 15:17:01  peter
#   * win32 compiles a bit better, no growheap crash
#
#

