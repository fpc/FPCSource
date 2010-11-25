#
#   $Id: Makefile,v 1.2 2002/09/07 16:01:29 peter Exp $
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Michael van Canneyt
#
#   Makefile for the Free Pascal windows.pp include files
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

ECHO=echo

# Files used by windows.pp
include makefile.inc


#####################################################################
# windows.pp
#####################################################################

# default target
all: ascdef.inc unidef.inc

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

# list of useful dll's for windows.pp
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
%.lst : %.inc
	@$(ECHO) listing DLL function names of $*.inc
	sed -n -e "s/\(.*\)External_library name '\([^']*\)'\(.*\)/\2/p" $*.inc > $*.lst

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
	sed -f $*.sub $*.inc > $*.npp
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

# automatic conversion from ascfun.inc to ascdef.inc
# and unifun.inc to unidef.inc
ascdef.inc : ascfun.inc ascdef.sed
	sed -f ascdef.sed ascfun.inc > ascdef.inc

unidef.inc : unifun.inc unidef.sed
	sed -f unidef.sed unifun.inc > unidef.inc

