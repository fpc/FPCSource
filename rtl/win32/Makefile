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
# Configuration section
#####################################################################

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
# Defaults
#####################################################################

# set target and cpu which are required
override OS_TARGET=win32
override CPU=i386

# Where are the include files
RTL=..
CFG=$(RTL)/cfg
INC=$(RTL)/inc
PROCINC=$(RTL)/$(CPU)
OBJPASDIR=$(RTL)/objpas


#####################################################################
# Include default makefile
#####################################################################

include $(CFG)/makefile.cfg


#####################################################################
# System dependent
#####################################################################

# Define Win32 Units
SYSTEMPPU=syswin32$(PPUEXT)
OBJECTS=strings dos \
	windows \
	cpu mmx getopts heaptrc objpas sysutils

# Files used by windows.pp
WINDOWS_FILES=base errors defines \
	struct ascfun ascdef \
	unifun unidef func

WINDOWS_SOURCE_FILES=$(addsuffix .pp,$(WINDOWS_FILES))


#####################################################################
# Include system unit dependencies
#####################################################################

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
# System independent Makefile
#####################################################################

# Add Prefix and Suffixes
OBJLOADERS=$(addsuffix $(OEXT), $(LOADERS))
PPUOBJECTS=$(addsuffix $(PPUEXT), $(OBJECTS))

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
	-make -C $(OBJPASDIR) clean


#####################################################################
# Files
#####################################################################

#
# Base Units (System, strings, os-dependent-base-unit)
#

$(SYSTEMPPU) : syswin32.pp win32.inc $(SYSDEPS)
	$(COMPILER) -Us -Sg syswin32.pp $(REDIR)

strings$(PPUEXT) : $(PROCINC)/strings.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/strings.pp .
	$(COMPILER) strings $(REDIR)
	$(DEL) strings.pp

#
# Delphi Object Model
#

objpas$(PPUEXT) : $(OBJPASDIR)/objpas.pp $(INC)/except.inc
	$(COPY) $(OBJPASDIR)/objpas.pp .
	$(COMPILER) -S2 -I$(OBJPASDIR) objpas $(REDIR)
	$(DEL) objpas.pp

SYSUTILINC = $(wildcard $(OBJPASDIR)/*.inc)

sysutils$(PPUEXT) : $(OBJPASDIR)/sysutils.pp $(SYSUTILINC) filutil.inc disk.inc
	$(COPY) $(OBJPASDIR)/sysutils.pp .
	$(COMPILER) -S2 -I$(OBJPASDIR) sysutils $(REDIR)
	$(DEL) sysutils.pp


#
# System Dependent Units
#

base$(PPUEXT) : base.pp $(SYSTEMPPU)
	$(COMPILER) base.pp $(REDIR)

messages$(PPUEXT) : messages.pp $(SYSTEMPPU)
	$(COMPILER) messages.pp $(REDIR)

defines$(PPUEXT) : defines.pp $(SYSTEMPPU)
	$(COMPILER) defines.pp $(REDIR)

windows$(PPUEXT) : windows.pp $(WINDOWS_SOURCE_FILES) $(SYSTEMPPU)
	$(COMPILER) windows.pp $(REDIR)

#
# TP7 Compatible RTL Units
#

dos$(PPUEXT) : $(DOSDEPS) $(INC)/filerec.inc $(INC)/textrec.inc $(SYSTEMPPU)
	$(COMPILER) dos $(REDIR)

#objects$(PPUEXT) : $(INC)/objects.pp objinc.inc $(SYSTEMPPU)
#	 $(COPY) $(INC)/objects.pp .
#	 $(COMPILER) objects $(REDIR)
#	 $(DEL) objects.pp

#
# Other RTL Units
#

cpu$(PPUEXT) : $(PROCINC)/cpu.pp $(SYSTEMPPU)
	$(COPY) $(PROCINC)/cpu.pp .
	$(COMPILER) cpu.pp $(REDIR)
	$(DEL) cpu.pp

mmx$(PPUEXT) : $(PROCINC)/mmx.pp cpu$(PPUEXT) $(SYSTEMPPU)
	$(COPY) $(PROCINC)/mmx.pp .
	$(COMPILER) mmx.pp $(REDIR)
	$(DEL) mmx.pp

getopts$(PPUEXT) : $(INC)/getopts.pp $(SYSTEMPPU)
	$(COPY) $(INC)/getopts.pp .
	$(COMPILER) getopts.pp $(REDIR)
	$(DEL) getopts.pp

heaptrc$(PPUEXT) : $(INC)/heaptrc.pp $(SYSTEMPPU)
	$(COPY) $(INC)/heaptrc.pp .
	$(COMPILER) heaptrc $(REDIR)
	$(DEL) heaptrc.pp


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
	-$(DEL) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)


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
	sed -f ascdef.sed ascfun.pp > ascdef.pp

unidef.pp : unifun.pp unidef.sed
	sed -f unidef.sed unifun.pp > unidef.pp
endif


#####################################################################
# Default targets
#####################################################################

include $(CFG)/makefile.def

#
# $Log$
# Revision 1.9  1998-10-12 08:36:32  pierre
#  * wrong 'objpas' target in all removed
#
# Revision 1.8  1998/10/11 13:45:03  michael
# + Added disk.inc to sysutils dependencies
#
# Revision 1.7  1998/10/11 12:21:46  michael
# + Further sysutils implementations.
#
# Revision 1.6  1998/10/06 22:10:32  peter
#   + heaptrc
#
# Revision 1.5  1998/10/02 09:26:05  peter
#   * fixed rtl path
#
# Revision 1.4  1998/09/16 16:47:35  peter
#   * merged fixes
#
# Revision 1.1.2.2  1998/09/16 16:17:55  peter
#   * updates to install with go32,win32
#
# Revision 1.3  1998/09/15 12:09:13  peter
#   * merged updates
#
# Revision 1.2  1998/09/11 15:09:48  michael
# fixed colon problem
#
# Revision 1.1.2.1  1998/09/15 12:02:06  peter
#   * updates to get objpas using its own makefile
#
# Revision 1.1	1998/09/10 14:15:55  peter
#   - renamed makefile to Makefile
#
# Revision 1.13  1998/09/07 18:31:54  peter
#   * fixed $smartlibext -> $staticlibext to fix a 'rm *' :(
#
# Revision 1.12  1998/09/04 17:17:36  pierre
#   + all unknown function ifdef with
#     conditionnal unknown_functions
#     testwin works now, but windowcreate still fails !!
#
# Revision 1.11  1998/09/04 12:33:12  pierre
#   + added SED testing for ascdef.pp and unidef.pp
#   * func.pp ready
#     still some functions missing (commented out for now)
#
# Revision 1.10  1998/09/03 18:17:35  pierre
#   * small improvements in number of found functions
#     all remaining are in func.pp
#
# Revision 1.9	1998/09/03 17:14:54  pierre
#   * most functions found in main DLL's
#     still some missing
#     use 'make dllnames' to get missing names
#
# Revision 1.7	1998/08/21 15:17:01  peter
#   * win32 compiles a bit better, no growheap crash
#
#

