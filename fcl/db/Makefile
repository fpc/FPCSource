#
# Makefile generated from Makefile.fpc on 1999-11-09 16:02
#

defaultrule: Makefile all

Makefile: Makefile.fpc
	fpcmake
	

#####################################################################
# Autodetect OS (Linux or Dos or Windows NT)
# define inlinux when running under linux
# define inWinNT when running under WinNT
#####################################################################

# We need only / in the path
override PATH:=$(subst \,/,$(PATH))

# Search for PWD and determine also if we are under linux
PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(subst ;, ,$(PATH)))))
ifeq ($(PWD),)
PWD:=$(strip $(wildcard $(addsuffix /pwd,$(subst :, ,$(PATH)))))
ifeq ($(PWD),)
nopwd:
	@echo You need the GNU utils package to use this Makefile!
	@echo Get ftp://ftp.freepascal.org/pub/fpc/dist/go32v2/utilgo32.zip
	@exit
else
inlinux=1
endif
else
PWD:=$(firstword $(PWD))
endif

# Detect NT - NT sets OS to Windows_NT
ifndef inlinux
ifeq ($(OS),Windows_NT)
inWinNT=1
endif
endif

# Detect OS/2 - OS/2 has OS2_SHELL defined
ifndef inlinux
ifndef inWinNT
ifdef OS2_SHELL
inOS2=1
endif
endif
endif

# The extension of executables
ifdef inlinux
EXEEXT=
else
EXEEXT=.exe
endif

# The path which is search separated by spaces
ifdef inlinux
SEARCHPATH=$(subst :, ,$(PATH))
else
SEARCHPATH=$(subst ;, ,$(PATH))
endif

#####################################################################
# Default target
#####################################################################

override CPU_TARGET:=i386

#####################################################################
# FPC version/target Detection
#####################################################################

# What compiler to use ?
ifndef FPC
ifdef inOS2
export FPC=ppos2$(EXEEXT)
else
export FPC=ppc386$(EXEEXT)
endif
endif

# Target OS
ifndef OS_TARGET
export OS_TARGET:=$(shell $(FPC) -iTO)
endif

# Source OS
ifndef OS_SOURCE
export OS_SOURCE:=$(shell $(FPC) -iSO)
endif

# Target CPU
ifndef CPU_TARGET
export CPU_TARGET:=$(shell $(FPC) -iTP)
endif

# Source CPU
ifndef CPU_SOURCE
export CPU_SOURCE:=$(shell $(FPC) -iSP)
endif

# FPC version
ifndef FPC_VERSION
export FPC_VERSION:=$(shell $(FPC) -iV)
endif

#####################################################################
# Default Settings
#####################################################################

# Release ? Then force OPT and don't use extra opts via commandline
ifdef RELEASE
override OPT:=-Xs -OG2p3 -n
endif

# Verbose settings (warning,note,info)
ifdef VERBOSE
override OPT+=-vwni
endif

#####################################################################
# User Settings
#####################################################################


# Targets

UNITOBJECTS=db ddg_ds ddg_rec mysqldb
EXEOBJECTS=testds createds mtest

# Clean


# Install


# Defaults

DEFAULTUNITS=1
override NEEDOPT=-S2

# Directories

ifndef FPCDIR
FPCDIR=../..
endif
override NEEDUNITDIR=../$(OS_TARGET)
ifndef TARGETDIR
TARGETDIR=.
endif

# Libraries

override NEEDGCCLIB=1

# Info

FPCINFO=fpc_infocfg fpc_infoobjects fpc_infoinstall 

#####################################################################
# Compiler Command Line
#####################################################################

# Load commandline OPTDEF and add FPC_CPU define
override FPCOPTDEF:=-d$(CPU_TARGET)

# Load commandline OPT and add target and unit dir to be sure
ifneq ($(OS_TARGET),$(OS_SOURCE))
override FPCOPT+=-T$(OS_TARGET)
endif


ifdef NEEDOPT
override FPCOPT+=$(NEEDOPT)
endif

# RTL first and then Unit dir (a unit can override RTLunit). Don't add the
# dirs if fpcdir=. which can be used for the rtl makefiles
ifdef FPCDIR
ifneq ($(FPCDIR),.)
override FPCOPT+=-Fu$(FPCDIR)/rtl/$(OS_TARGET) -Fu$(FPCDIR)/units/$(OS_TARGET)
endif
endif

ifdef NEEDUNITDIR
override FPCOPT+=$(addprefix -Fu,$(NEEDUNITDIR))
endif

# Add GCC lib path if asked
ifdef GCCLIBDIR
override FPCOPT+=-Fl$(GCCLIBDIR)
endif

# Target dirs
ifdef TARGETDIR
override FPCOPT+=-FE$(TARGETDIR)
endif

# Smartlinking
ifeq ($(SMARTLINK),YES)
override FPCOPT+=-Cx
endif

# Add commandline options
ifdef OPT
override FPCOPT+=$(OPT)
endif
ifdef UNITDIR
override FPCOPT+=$(addprefix -Fu,$(UNITDIR))
endif
ifdef LIBDIR
override FPCOPT+=$(addprefix -Fl,$(LIBDIR))
endif
ifdef OBJDIR
override FPCOPT+=$(addprefix -Fo,$(OBJDIR))
endif
ifdef INCDIR
override FPCOPT+=$(addprefix -Fi,$(INCDIR))
endif

# Add defines from FPCOPTDEF to FPCOPT
ifdef FPCOPTDEF
override FPCOPT+=$(FPCOPTDEF)
endif

# Was a config file specified ?
ifdef CFGFILE
override FPCOPT+=@$(CFGFILE)
endif

override COMPILER=$(FPC) $(FPCOPT)

#####################################################################
# Shell tools
#####################################################################

# To copy pograms
ifndef COPY
export COPY:=cp -fp
endif

# To move pograms
ifndef MOVE
export MOVE:=mv -f
endif

# Check delete program
ifndef DEL
export DEL:=rm -f
endif

# Check deltree program
ifndef DELTREE
export DELTREE:=rm -rf
endif

# To install files
ifndef INSTALL
ifdef inlinux
export INSTALL:=install -m 644
else
export INSTALL:=$(COPY)
endif
endif

# To install programs
ifndef INSTALLEXE
ifdef inlinux
export INSTALLEXE:=install -m 755
else
export INSTALLEXE:=$(COPY)
endif
endif

# To make a directory.
ifndef MKDIR
ifdef inlinux
export MKDIR:=install -m 755 -d
else
export MKDIR:=ginstall -m 755 -d
endif
endif

#####################################################################
# Default Tools
#####################################################################

# assembler, redefine it if cross compiling
ifndef AS
AS=as
endif

# linker, but probably not used
ifndef LD
LD=ld
endif

# Where is the ppumove program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

# Where is the ppdep program ?
ifndef PPDEP
PPDEP=ppdep
endif

# ppas.bat / ppas.sh
ifdef inlinux
PPAS=ppas.sh
else
ifdef inOS2
PPAS=ppas.cmd
else
PPAS=ppas.bat
endif
endif

# also call ppas if with command option -s
ifeq (,$(findstring -s ,$(COMPILER)))
EXECPPAS=
else
EXECPPAS=@$(PPAS)
endif

# ldconfig to rebuild .so cache
ifdef inlinux
LDCONFIG=ldconfig
else
LDCONFIG=
endif

# echo
ifndef ECHO
ECHO:=$(strip $(wildcard $(addsuffix /echo$(EXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
export ECHO:=echo
else
export ECHO:=$(firstword $(ECHO))
endif
endif

# Look if UPX is found for go32v2 and win32. We can't use $UPX becuase
# upx uses that one itself (PFV)
ifndef UPXPROG
ifeq ($(OS_TARGET),go32v2)
UPXPROG:=1
endif
ifeq ($(OS_TARGET),win32)
UPXPROG:=1
endif
ifdef UPXPROG
UPXPROG:=$(strip $(wildcard $(addsuffix /upx$(EXEEXT),$(SEARCHPATH))))
ifeq ($(UPX),)
UPXPROG=
else
export UPXPROG:=$(firstword $(UPX))
endif
else
UPXPROG=
endif
endif

# gdate/date
ifndef DATE
DATE:=$(strip $(wildcard $(addsuffix /date$(EXEEXT),$(SEARCHPATH))))
ifeq ($(DATE),)
DATE:=$(strip $(wildcard $(addsuffix /gdate$(EXEEXT),$(SEACHPATH))))
ifeq ($(DATE),)
DATE=
else
export DATE:=$(firstword $(DATE))
endif
else
export DATE:=$(firstword $(DATE))
endif
endif

ifdef DATE
DATESTR:=$(shell $(DATE) +%Y%m%d)
else
DATESTR=
endif

# ZipProg, you can't use Zip as the var name (PFV)
ifndef ZIPPROG
ZIPPROG:=$(strip $(wildcard $(addsuffix /zip$(EXEEXT),$(SEARCHPATH))))
ifeq ($(ZIPPROG),)
ZIPPROG=
else
export ZIPPROG:=$(firstword $(ZIPPROG)) -D9 -r
endif
endif

ifndef ZIPEXT
ZIPEXT=.zip
endif

#####################################################################
# Default Directories
#####################################################################

# Base dir
ifdef PWD
BASEDIR:=$(shell $(PWD))
else
BASEDIR=.
endif

# set the prefix directory where to install everything
ifndef PREFIXINSTALLDIR
ifdef inlinux
export PREFIXINSTALLDIR=/usr
else
export PREFIXINSTALLDIR=/pp
endif
endif

# set the directory to the rtl base
ifndef RTLDIR
ifdef RTL
RTLDIR:=$(RTL)/$(OS_TARGET)
else
RTLDIR:=$(FPCDIR)/rtl/$(OS_TARGET)
endif
endif

# specify where units are.
ifndef UNITDIR
ifdef UNITS
UNITDIR=$(UNITS)/$(OS_TARGET)
else
UNITDIR=$(FPCDIR)/units/$(OS_TARGET)
endif
endif
ifeq ($(strip $(wildcard $(UNITDIR)/*)),)
UNITDIR=
endif

# On linux, try to find where libgcc.a is.
ifdef inlinux
ifndef GCCLIBDIR
export GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '{ print $$4 } '`)
endif
endif

# Where to find other libraries
ifdef inlinux
ifndef OTHERLIBDIR
export OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | awk '{ ORS=" "; print $1 }')
endif
endif

#####################################################################
# Install Directories based on BASEINSTALLDIR
#####################################################################

# set the base directory where to install everything
ifndef BASEINSTALLDIR
ifdef inlinux
BASEINSTALLDIR=$(PREFIXINSTALLDIR)/lib/fpc/$(FPC_VERSION)
else
BASEINSTALLDIR=$(PREFIXINSTALLDIR)
endif
endif


# Linux binary really goes to baseinstalldir
ifndef LIBINSTALLDIR
ifdef inlinux
LIBINSTALLDIR=$(BASEINSTALLDIR)
else
LIBINSTALLDIR=$(BASEINSTALLDIR)/lib
endif
endif

# set the directory where to install the binaries
ifndef BININSTALLDIR
ifdef inlinux
BININSTALLDIR=$(PREFIXINSTALLDIR)/bin
else
BININSTALLDIR=$(BASEINSTALLDIR)/bin/$(OS_TARGET)
endif
endif

# Where the .msg files will be stored
ifndef MSGINSTALLDIR
MSGINSTALLDIR=$(BASEINSTALLDIR)/msg
endif

# Where the .msg files will be stored
ifndef SOURCEINSTALLDIR
SOURCEINSTALLDIR=$(BASEINSTALLDIR)/source
endif

# Where the doc files will be stored
ifndef DOCINSTALLDIR
ifdef inlinux
DOCINSTALLDIR=$(PREFIXINSTALLDIR)/doc/fpc/$(FPC_VERSION)
else
DOCINSTALLDIR=$(BASEINSTALLDIR)/doc
endif
endif

########################
# Unit Directories
########################

# this can be set to 'rtl' when the RTL units are installed
ifndef UNITPREFIX
UNITPREFIX=units
endif

# set the directory where to install the units.
ifndef UNITINSTALLDIR
UNITINSTALLDIR=$(BASEINSTALLDIR)/$(UNITPREFIX)/$(OS_TARGET)
endif

# set the directory where to install the units.
ifndef STATIC_UNITINSTALLDIR
STATIC_UNITINSTALLDIR=$(UNITINSTALLDIR)/static
endif

# set the directory where to install the units.
ifndef SHARED_UNITINSTALLDIR
SHARED_UNITINSTALLDIR=$(UNITINSTALLDIR)/shared
endif

# set the directory where to install the libs (must exist)
ifndef STATIC_LIBINSTALLDIR
STATIC_LIBINSTALLDIR=$(STATIC_UNITINSTALLDIR)
endif

# set the directory where to install the libs (must exist)
ifndef SHARED_LIBINSTALLDIR
ifdef inlinux
SHARED_LIBINSTALLDIR=$(PREFIXINSTALLDIR)/lib
else
SHARED_LIBINSTALLDIR=$(SHARED_UNITINSTALLDIR)
endif
endif

#####################################################################
# Default extensions
#####################################################################

# Default needed extensions (Go32v2,Linux)
LOADEREXT=.as
PPLEXT=.ppl
PPUEXT=.ppu
OEXT=.o
ASMEXT=.s
SMARTEXT=.sl
STATICLIBEXT=.a
SHAREDLIBEXT=.so
PACKAGESUFFIX=

# Go32v1
ifeq ($(OS_TARGET),go32v1)
PPUEXT=.pp1
OEXT=.o1
ASMEXT=.s1
SMARTEXT=.sl1
STATICLIBEXT=.a1
SHAREDLIBEXT=.so1
PACKAGESUFFIX=v1
endif

# Go32v2
ifeq ($(OS_TARGET),go32v2)
PACKAGESUFFIX=go32
endif

# Linux
ifeq ($(OS_TARGET),linux)
PACKAGESUFFIX=linux
endif

# Win32
ifeq ($(OS_TARGET),win32)
PPUEXT=.ppw
OEXT=.ow
ASMEXT=.sw
SMARTEXT=.slw
STATICLIBEXT=.aw
SHAREDLIBEXT=.dll
PACKAGESUFFIX=win32
endif

# OS/2
ifeq ($(OS_TARGET),os2)
PPUEXT=.ppo
ASMEXT=.so2
OEXT=.oo2
SMARTEXT=.so
STATICLIBEXT=.ao2
SHAREDLIBEXT=.dll
PACKAGESUFFIX=os2
endif

# library prefix
LIBPREFIX=lib
ifeq ($(OS_TARGET),go32v2)
LIBPREFIX=
endif
ifeq ($(OS_TARGET),go32v1)
LIBPREFIX=
endif

# determine which .pas extension is used
ifndef PASEXT
ifdef EXEOBJECTS
override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstword $(EXEOBJECTS)))))
else
override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstword $(UNITOBJECTS)))))
endif
ifeq ($(TESTPAS),)
PASEXT=.pp
else
PASEXT=.pas
endif
endif

#####################################################################
# Default rules
#####################################################################

.PHONY: defaultrule all staticlib sharedlib showinstall install \
	staticinstall sharedinstall libinstall zipinstall zipinstalladd \
	clean cleanall depend info

all: fpc_all

staticlib: fpc_staticlib

sharedlib: fpc_sharedlib

showinstall: fpc_showinstall

install: fpc_install

staticinstall: fpc_staticinstall

sharedinstall: fpc_sharedinstall

libinstall: fpc_libinstall

zipinstall: fpc_zipinstall

zipinstalladd: fpc_zipinstalladd

clean: fpc_clean

clean_all: fpc_clean_all

depend: fpc_depend

info: fpc_info

#####################################################################
# General compile rules
#####################################################################

.PHONY: fpc_all fpc_units fpc_exes fpc_loaders

# Create Filenames
LOADEROFILES=$(addsuffix $(OEXT),$(LOADEROBJECTS))
EXEFILES=$(addsuffix $(EXEEXT),$(EXEOBJECTS))
EXEOFILES=$(addsuffix $(OEXT),$(EXEOBJECTS))
UNITPPUFILES=$(addsuffix $(PPUEXT),$(UNITOBJECTS))
UNITOFILES=$(addsuffix $(OEXT),$(UNITOBJECTS))
UNITAFILES=$(addsuffix $(STATICLIBEXT),$(UNITOBJECTS))

.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp

ifdef DEFAULTUNITS
fpc_all: fpc_loaders fpc_units
else
fpc_all: fpc_loaders fpc_units fpc_exes
endif

fpc_loaders: $(LOADEROFILES)

fpc_units: $(UNITPPUFILES)

fpc_exes: $(EXEFILES)

# General compile rules, available for both possible PASEXT
%$(PPUEXT): %.pp
	$(COMPILER) $< $(REDIR)
	$(EXECPASS)

%$(PPUEXT): %.pas
	$(COMPILER) $< $(REDIR)
	$(EXECPASS)

%$(EXEEXT): %.pp
	$(COMPILER) $< $(REDIR)
	$(EXECPASS)

%$(EXEEXT): %.pas
	$(COMPILER) $< $(REDIR)
	$(EXECPASS)

%$(OEXT): %$(LOADEREXT)
	$(AS) -o $*$(OEXT) $<

#####################################################################
# Library
#####################################################################

.PHONY: fpc_staticlib fpc_sharedlib

# Default sharedlib units are all unit objects
ifndef SHAREDLIBUNITOBJECTS
SHAREDLIBUNITOBJECTS=$(UNITOBJECTS)
endif

fpc_staticlib:
	$(MAKE) libsclean
	$(MAKE) all SMARTLINK=YES

fpc_sharedlib: all
ifdef inlinux
ifndef LIBNAME
	@$(ECHO) LIBNAME not set
else
	$(PPUMOVE) $(SHAREDLIBUNITOBJECTS) -o$(LIBNAME)
endif
else
	@$(ECHO) Shared Libraries not supported
endif

#####################################################################
# Install rules
#####################################################################

.PHONY: fpc_showinstallfiles fpc_install

ifdef UNITOBJECTS
override UNITINSTALLFILES=$(wildcard $(UNITPPUFILES) $(UNITOFILES) $(UNITAFILES))
endif
ifdef EXTRAINSTALLUNITS
override EXTRAINSTALLFILES+=$(wildcard $(addsuffix $(OEXT),$(EXTRAINSTALLUNITS)) $(addsuffix $(STATICLIBEXT),$(EXTRAINSTALLUNITS)) $(addsuffix $(PPUEXT),$(EXTRAINSTALLUNITS)))
endif

fpc_showinstallfiles : all
ifndef DEFAULTUNITS
ifdef EXEOBJECTS
	@$(ECHO) $(addprefix "\n"$(BININSTALLDIR)/,$(EXEFILES))
endif
endif
ifdef LOADEROBJECTS
	@$(ECHO) $(addprefix "\n"$(UNITINSTALLDIR)/,$(LOADEROFILES))
endif
ifdef UNITINSTALLFILES
	@$(ECHO) $(addprefix "\n"$(UNITINSTALLDIR)/,$(UNITINSTALLFILES))
endif
ifdef EXTRAINSTALLFILES
	@$(ECHO) $(addprefix "\n"$(UNITINSTALLDIR)/,$(EXTRAINSTALLFILES))
endif

fpc_install:
# Create UnitInstallFiles
ifndef DEFAULTUNITS
ifdef EXEOBJECTS
	$(MKDIR) $(BININSTALLDIR)
# Compress the exes if upx is defined
ifdef UPXPROG
	-$(UPXPROG) $(EXEFILES)
endif
	$(INSTALLEXE) $(EXEFILES) $(BININSTALLDIR)
endif
endif
ifdef LOADEROBJECTS
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) $(LOADEROFILES) $(UNITINSTALLDIR)
endif
ifdef UNITINSTALLFILES
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) $(UNITINSTALLFILES) $(UNITINSTALLDIR)
endif
ifdef EXTRAINSTALLFILES
	$(MKDIR) $(UNITINSTALLDIR)
	$(INSTALL) $(EXTRAINSTALLFILES) $(UNITINSTALLDIR)
endif

#####################################################################
# Zip
#####################################################################

.PHONY: fpc_zipinstall fpc_zipinstalladd

# Temporary path to pack a file
ifndef PACKDIR
ifndef inlinux
PACKDIR=pack_tmp
else
PACKDIR=/tmp/fpc-pack
endif
endif

# Test dir if none specified
ifndef PACKAGEDIR
PACKAGEDIR=$(BASEDIR)
endif

# Add .zip/.tar.gz extension
ifdef ZIPNAME
ifndef inlinux
override ZIPNAME:=$(ZIPNAME)$(ZIPEXT)
endif
endif

# Default target which is call before zipping
ifndef ZIPTARGET
ZIPTARGET=install
endif

# Note: This will not remove the zipfile first
fpc_zipinstalladd:
ifndef ZIPNAME
	@$(ECHO) Please specify ZIPNAME!
	@exit
else
	$(MAKE) $(ZIPTARGET) PREFIXINSTALLDIR=$(PACKDIR)
ifdef inlinux
	gzip -d $(PACKAGEDIR)/$(ZIPNAME).tar.gz
	cd $(PACKDIR) ; tar rv --file $(PACKAGEDIR)/$(ZIPNAME).tar * ; cd $(BASEDIR)
	gzip $(PACKAGEDIR)/$(ZIPNAME).tar
else
	cd $(PACKDIR) ; $(ZIPPROG) $(PACKAGEDIR)/$(ZIPNAME) * ; cd $(BASEDIR)
endif
	$(DELTREE) $(PACKDIR)
endif

# First remove the zip and then install
fpc_zipinstall:
ifndef ZIPNAME
	@$(ECHO) Please specify ZIPNAME!
	@exit
else
	$(DEL) $(PACKAGEDIR)/$(ZIPNAME)
	$(MAKE) $(ZIPTARGET) PREFIXINSTALLDIR=$(PACKDIR)
ifdef inlinux
	cd $(PACKDIR) ; tar cvz --file $(PACKAGEDIR)/$(ZIPNAME).tar.gz * ; cd $(BASEDIR)
else
	cd $(PACKDIR) ; $(ZIPPROG) $(PACKAGEDIR)/$(ZIPNAME) * ; cd $(BASEDIR)
endif
	$(DELTREE) $(PACKDIR)
endif

#####################################################################
# Clean rules
#####################################################################

.PHONY: fpc_clean fpc_libsclean fpc_cleanall

ifdef UNITOBJECTS
override UNITCLEANFILES=$(wildcard $(UNITPPUFILES) $(UNITOFILES) $(UNITAFILES))
endif
ifdef EXTRACLEANUNITS
override EXTRACLEANFILES+=$(wildcard $(addsuffix $(OEXT),$(EXTRACLEANUNITS)) $(addsuffix $(STATICLIBEXT),$(EXTRACLEANUNITS)) $(addsuffix $(PPUEXT),$(EXTRACLEANUNITS)))
endif

fpc_clean:
ifdef EXEOBJECTS
	-$(DEL) $(EXEFILES) $(EXEOFILES)
endif
ifdef LOADEROBJECTS
	-$(DEL) $(LOADEROFILES)
endif
ifdef UNITCLEANFILES
	-$(DEL) $(UNITCLEANFILES)
endif
ifdef EXTRACLEANFILES
	-$(DEL) $(EXTRACLEANFILES)
endif
	-$(DEL) $(PPAS) link.res log

fpc_libsclean: clean
	-$(DEL) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)

fpc_cleanall:
ifdef EXEOBJECTS
	-$(DEL) $(EXEFILES)
endif
	-$(DEL) *$(OEXT) *$(PPUEXT) *$(ASMEXT) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)
	-$(DELTREE) *$(SMARTEXT)

#####################################################################
# Depend rules
#####################################################################

.PHONY: fpc_depend

fpc_depend:
	$(PPDEP) $(UNITOBJECTS)

#####################################################################
# Info rules
#####################################################################

.PHONY: fpc_info fpc_cfginfo fpc_objectinfo fpc_toolsinfo fpc_installinfo \
	fpc_dirinfo

fpc_info: $(FPCINFO)

fpc_infocfg:
	@$(ECHO)
	@$(ECHO)  == Configuration info ==
	@$(ECHO)
	@$(ECHO)  FPC....... $(FPC)
	@$(ECHO)  Version... $(FPC_VERSION)
	@$(ECHO)  CPU....... $(CPU_TARGET)
	@$(ECHO)  Source.... $(OS_SOURCE)
	@$(ECHO)  Target.... $(OS_TARGET)
	@$(ECHO)


fpc_infoinstall:
	@$(ECHO)
	@$(ECHO)  == Install info ==
	@$(ECHO)
ifdef DATE
	@$(ECHO)  DateStr.............. $(DATESTR)
endif
	@$(ECHO)  PackageSuffix........ $(PACKAGESUFFIX)
	@$(ECHO)
	@$(ECHO)  BaseInstallDir....... $(BASEINSTALLDIR)
	@$(ECHO)  BinInstallDir........ $(BININSTALLDIR)
	@$(ECHO)  UnitInstallDir....... $(UNITINSTALLDIR)
	@$(ECHO)  StaticUnitInstallDir. $(STATIC_UNITINSTALLDIR)
	@$(ECHO)  SharedUnitInstallDir. $(SHARED_UNITINSTALLDIR)
	@$(ECHO)  LibInstallDir........ $(LIBINSTALLDIR)
	@$(ECHO)  StaticLibInstallDir.. $(STATIC_LIBINSTALLDIR)
	@$(ECHO)  SharedLibInstallDir.. $(SHARED_LIBINSTALLDIR)
	@$(ECHO)  MsgInstallDir........ $(MSGINSTALLDIR)
	@$(ECHO)  DocInstallDir........ $(DOCINSTALLDIR)
	@$(ECHO)

#####################################################################
# Users rules
#####################################################################


vpath %$(PASEXT) $(INC) $(XML)

INCFILES=

db$(PPUEXT): db.pp fields.inc dataset.inc dbs.inc

ddg_ds$(PPUEXT): db$(PPUEXT) ddg_rec$(PPUEXT) ddg_ds$(PASEXT)

testds$(EXEEXT): ddg_ds$(PPUEXT) testds$(PASEXT)

createds$(EXEEXT): createds$(PASEXT) ddg_rec$(PPUEXT)

mysqldb$(PPUEXT): db$(PPUEXT) mysqldb$(PASEXT)

mtest$(EXEEXT): mysqldb$(PPUEXT) mtest$(PASEXT)
