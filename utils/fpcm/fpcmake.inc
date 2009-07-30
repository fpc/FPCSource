{$ifdef Delphi}
const fpcmakeini : array[0..218] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..218,1..240] of char=(
{$endif Delphi}
  ';'#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#010+
  ';'#010+
  #010+
  '[defines]'#010+
  '#####################################################################'#010+
  '# Misc defines to be used by anyone'#010+
  '####################################################','################'+
  '#'#010+
  #010+
  '# OS categories'#010+
  'BSDs = freebsd netbsd openbsd darwin'#010+
  'UNIXs = linux $(BSDs) solaris qnx'#010+
  'LIMIT83fs = go32v2 os2 emx watcom'#010+
  'OSNeedsComspecToRunBatch = go32v2 watcom'#010+
  #010+
  '#Empty target for rules that always should run. Needed if'#010,
  '#the target is non-phoney, and there is non-phony prereqisites.'#010+
  '#Then add FORCE as an prerequisite'#010+
  '#See gnu make manual: 4.7 Rules without Commands or Prerequisites'#010+
  'FORCE:'#010+
  '.PHONY: FORCE'#010+
  #010+
  '[osdetect]'#010+
  '##########################################','##########################'+
  '#'#010+
  '# Autodetect source OS (Linux or Dos or Windows NT or OS/2 or other)'#010+
  '# define inUnix when running under Unix like environment'#010+
  '#               (Linux,FreeBSD,NetBSD,OpenBSD,Darwin,Cygwin)'#010+
  '# define inWinNT when run','ning under WinNT'#010+
  '# define inOS2 when running under OS/2'#010+
  '# define inCygwin when running under Cygwin32'#010+
  '#####################################################################'#010+
  #010+
  '# We need only / in the path also remove the current dir,'#010+
  '# also re','move trailing /'#039's'#010+
  'override PATH:=$(patsubst %/,%,$(subst \,/,$(PATH)))'#010+
  #010+
  '# Detect unix'#010+
  '# Darwin is handled specially'#010+
  'ifneq ($(findstring darwin,$(OSTYPE)),)'#010+
  'inUnix=1 #darwin'#010+
  'SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))'#010+
  'else'#010+
  '# Determine i','f we'#039've a unix searchpath by looking for a ;'#010+
  '# that normally doesn'#039't exists in the unix PATH var.'#010+
  'ifeq ($(findstring ;,$(PATH)),)'#010+
  'inUnix=1'#010+
  'SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))'#010+
  'else'#010+
  'SEARCHPATH:=$(subst ;, ,$(PATH))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '#',' Add path were make is located'#010+
  'SEARCHPATH+=$(patsubst %/,%,$(subst \,/,$(dir $(MAKE))))'#010+
  #010+
  '# Search for PWD'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(SEARCHPATH))))'#010+
  'ifeq ($(PWD),)'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(SEARCHPATH))))',#010+
  'ifeq ($(PWD),)'#010+
  '$(error You need the GNU utils package to use this Makefile)'#010+
  'else'#010+
  'PWD:=$(firstword $(PWD))'#010+
  'SRCEXEEXT='#010+
  'endif'#010+
  'else'#010+
  'PWD:=$(firstword $(PWD))'#010+
  'SRCEXEEXT=.exe'#010+
  'endif'#010+
  #010+
  '# Detect NT - NT sets OS to Windows_NT'#010+
  '# Detect OS/2 - OS/2 has ','OS2_SHELL defined'#010+
  'ifndef inUnix'#010+
  'ifeq ($(OS),Windows_NT)'#010+
  'inWinNT=1'#010+
  'else'#010+
  'ifdef OS2_SHELL'#010+
  'inOS2=1'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'ifneq ($(findstring cygdrive,$(PATH)),)'#010+
  'inCygWin=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# The extension of batch files / scripts'#010+
  'ifdef inUnix'#010+
  'SRCBATCHEX','T=.sh'#010+
  'else'#010+
  'ifdef inOS2'#010+
  'SRCBATCHEXT=.cmd'#010+
  'else'#010+
  'SRCBATCHEXT=.bat'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifdef COMSPEC'#010+
  'ifneq ($(findstring $(OS_SOURCE),$(OSNeedsComspecToRunBatch)),)'#010+
  'ifndef RUNBATCH'#010+
  'RUNBATCH=$(COMSPEC) /C'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Path Separator, the subst t','rick is necessary for the \ that can'#039+
  't exists'#010+
  '# at the end of a line'#010+
  'ifdef inUnix'#010+
  'PATHSEP=/'#010+
  'else'#010+
  'PATHSEP:=$(subst /,\,/)'#010+
  '# cygwin bash or sh can not handle backslashs'#010+
  'ifdef inCygWin'#010+
  'PATHSEP=/'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Base dir'#010+
  'ifdef PWD'#010+
  'BASEDIR:=$(sub','st \,/,$(shell $(PWD)))'#010+
  '# For Cygwin we need to replace /cygdrive/c/ with c:/'#010+
  'ifdef inCygWin'#010+
  'ifneq ($(findstring /cygdrive/,$(BASEDIR)),)'#010+
  'BASENODIR:=$(patsubst /cygdrive%,%,$(BASEDIR))'#010+
  'BASEDRIVE:=$(firstword $(subst /, ,$(BASENODIR)))'#010+
  'BASED','IR:=$(subst /cygdrive/$(BASEDRIVE)/,$(BASEDRIVE):/,$(BASEDIR))'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'BASEDIR=.'#010+
  'endif'#010+
  #010+
  '# Echo is an internal command under OS/2 (and others), so it'#039's alw'+
  'ays found'#010+
  'ifdef inOS2'#010+
  'ifndef ECHO'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho','$(SRCEXEEXT),$(SEARCHPATH'+
  '))))'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO=echo'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'endif'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'endif'#010+
  'endif'#010+
  'export ECHO'#010+
  'en','dif'#010+
  #010+
  '[fpcdetect]'#010+
  '#####################################################################'#010+
  '# FPC Binary and Version Detection'#010+
  '#####################################################################'#010+
  #010+
  '# Compatibility with old makefiles'#010+
  'ifndef FPC'#010+
  'i','fdef PP'#010+
  'FPC=$(PP)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Try to detect the ppcXXX file to use by using "fpc -PB" option'#010+
  '# to query for the default ppcXXX the fpc executable tries. When'#010+
  '# fpc is not found use ppc386 by default. Also when fpc -PB gives'#010+
  '# an error pp','c386 will be used.'#010+
  'ifndef FPC'#010+
  '# check if fpc exists'#010+
  'FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#010+
  'ifneq ($(FPCPROG),)'#010+
  'FPCPROG:=$(firstword $(FPCPROG))'#010+
  'ifneq ($(CPU_TARGET),)'#010+
  'FPC:=$(shell $(FPCPROG) -P$(CPU_TARGET',') -PB)'#010+
  'else'#010+
  'FPC:=$(shell $(FPCPROG) -PB)'#010+
  'endif'#010+
  '# Older fpc executables didn'#039't support it and return'#010+
  '# Error: Illegal processor... If found then fallback to ppc386'#010+
  'ifneq ($(findstring Error,$(FPC)),)'#010+
  'override FPC=$(firstword $(strip $(wildca','rd $(addsuffix /ppc386$(SRC'+
  'EXEEXT),$(SEARCHPATH)))))'#010+
  'else'#010+
  '# if the cross compiler is not found, fall back to fpc'#010+
  'ifeq ($(strip $(wildcard $(FPC))),)'#010+
  'FPC:=$(firstword $(FPCPROG))'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  '# fpc binary not found, fallback to ppc386'#010+
  'ov','erride FPC=$(firstword $(strip $(wildcard $(addsuffix /ppc386$(SRC'+
  'EXEEXT),$(SEARCHPATH)))))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Get a clean executable name'#010+
  'override FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#010+
  #010+
  '# Try to find ','the binary direct first, otherwise in the path,'#010+
  '# if not found give an error'#010+
  'FOUNDFPC:=$(strip $(wildcard $(FPC)))'#010+
  'ifeq ($(FOUNDFPC),)'#010+
  'FOUNDFPC=$(strip $(wildcard $(addsuffix /$(FPC),$(SEARCHPATH))))'#010+
  'ifeq ($(FOUNDFPC),)'#010+
  '$(error Compiler $(F','PC) not found)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# For 1.1 and up we can use a single compiler call to retrieve'#010+
  '# all needed information'#010+
  'ifndef FPC_COMPILERINFO'#010+
  'FPC_COMPILERINFO:=$(shell $(FPC) -iVSPTPSOTO)'#010+
  'endif'#010+
  #010+
  '# FPC version'#010+
  'ifndef FPC_VERSION'#010+
  'FPC_VERSION:=$','(word 1,$(FPC_COMPILERINFO))'#010+
  'endif'#010+
  #010+
  'export FPC FPC_VERSION FPC_COMPILERINFO'#010+
  '# CHECKDEPEND should not be exported'#010+
  '# This should limit multiple checks'#010+
  'unexport CHECKDEPEND ALLDEPENDENCIES'#010+
  #010+
  '#####################################################','###############'+
  '#'#010+
  '# FPC Target Detection'#010+
  '#####################################################################'#010+
  #010+
  '# Fall back to default values if needed'#010+
  'ifndef CPU_TARGET'#010+
  'ifdef CPU_TARGET_DEFAULT'#010+
  'CPU_TARGET=$(CPU_TARGET_DEFAULT)'#010+
  'endif'#010+
  'endif'#010+
  'i','fndef OS_TARGET'#010+
  'ifdef OS_TARGET_DEFAULT'#010+
  'OS_TARGET=$(OS_TARGET_DEFAULT)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# For 1.0.x we need to use extra calls to retrieve all info'#010+
  'ifneq ($(words $(FPC_COMPILERINFO)),5)'#010+
  'FPC_COMPILERINFO+=$(shell $(FPC) -iSP)'#010+
  'FPC_COMPILERINFO+=','$(shell $(FPC) -iTP)'#010+
  'FPC_COMPILERINFO+=$(shell $(FPC) -iSO)'#010+
  'FPC_COMPILERINFO+=$(shell $(FPC) -iTO)'#010+
  'endif'#010+
  #010+
  '# Retrieve Target/Source CPU and Target/Source OS'#010+
  'ifndef CPU_SOURCE'#010+
  'CPU_SOURCE:=$(word 2,$(FPC_COMPILERINFO))'#010+
  'endif'#010+
  'ifndef CPU_TARGET'#010,
  'CPU_TARGET:=$(word 3,$(FPC_COMPILERINFO))'#010+
  'endif'#010+
  'ifndef OS_SOURCE'#010+
  'OS_SOURCE:=$(word 4,$(FPC_COMPILERINFO))'#010+
  'endif'#010+
  'ifndef OS_TARGET'#010+
  'OS_TARGET:=$(word 5,$(FPC_COMPILERINFO))'#010+
  'endif'#010+
  'FULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)'#010+
  'FULL_SOURCE=$(CPU_SOURCE)','-$(OS_SOURCE)'#010+
  #010+
  'ifeq ($(CPU_TARGET),armeb)'#010+
  'ARCH=arm'#010+
  'override FPCOPT+=-Cb'#010+
  'else'#010+
  'ifeq ($(CPU_TARGET),armel)'#010+
  'ARCH=arm'#010+
  'override FPCOPT+=-CaEABI'#010+
  'else'#010+
  'ARCH=$(CPU_TARGET)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Full name of the target, including CPU and OS. For OSs limited'#010,
  '# to 8.3 we only use the target OS'#010+
  'ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)'#010+
  'TARGETSUFFIX=$(OS_TARGET)'#010+
  'SOURCESUFFIX=$(OS_SOURCE)'#010+
  'else'#010+
  'TARGETSUFFIX=$(FULL_TARGET)'#010+
  'SOURCESUFFIX=$(FULL_SOURCE)'#010+
  'endif'#010+
  #010+
  '# Cross compile flag'#010+
  'ifneq ($(FULL_T','ARGET),$(FULL_SOURCE))'#010+
  'CROSSCOMPILE=1'#010+
  'endif'#010+
  #010+
  '# Check if the Makefile supports this target, but not'#010+
  '# when the make target is to rebuild the makefile'#010+
  'ifeq ($(findstring makefile,$(MAKECMDGOALS)),)'#010+
  'ifeq ($(findstring $(FULL_TARGET),$(MAKEFILE','TARGETS)),)'#010+
  '$(error The Makefile doesn'#039't support target $(FULL_TARGET), please'+
  ' run fpcmake first)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  #010+
  '# Detect BSD, since BSD uses a slightly different directory hierarchy.'+
  #010+
  'ifneq ($(findstring $(OS_TARGET),$(BSDs)),)'#010+
  'BSDhier=1'#010+
  'end','if'#010+
  #010+
  '# Detect Linux, will also use its own directory hierarchy.'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'linuxHier=1'#010+
  'endif'#010+
  #010+
  'export OS_TARGET OS_SOURCE ARCH CPU_TARGET CPU_SOURCE FULL_TARGET FULL'+
  '_SOURCE TARGETSUFFIX SOURCESUFFIX CROSSCOMPILE'#010+
  #010+
  #010+
  '[fpcdirchecke','nv]'#010+
  '#####################################################################'#010+
  '# FPCDIR Setting'#010+
  '#####################################################################'#010+
  #010+
  '# Test FPCDIR to look if the RTL dir exists'#010+
  'ifdef FPCDIR'#010+
  'override FPCDIR:=$(su','bst \,/,$(FPCDIR))'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  'else'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  #010+
  '# Default FPCDIR from Makefile.fpc'#010+
  'ifdef DEFAULT_FPCDIR'#010+
  'ifeq ($(FPCDIR),wrong)'#010+
  'override FPCDIR:=$(subst \',',/,$(DEFAULT_FPCDIR))'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[fpcdirdetect]'#010+
  '# Detect FPCDIR'#010+
  'ifeq ($(FPCDIR),wrong)'#010+
  'ifdef inUnix'#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION)'#010+
  'ifeq (','$(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#010+
  'endif'#010+
  'else'#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC),$(SEARCHPATH))))))'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildcard $(ad','dprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR:=$(BASEDIR)'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR=c:/pp'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'e','ndif'#010+
  'endif'#010+
  #010+
  '# Cross binaries dir'#010+
  'ifndef CROSSBINDIR'#010+
  'CROSSBINDIR:=$(wildcard $(FPCDIR)/bin/$(TARGETSUFFIX))'#010+
  'endif'#010+
  #010+
  '# Default binutils prefix for cross compile when the'#010+
  '# crossbindir is not set (except for Darwin)'#010+
  'ifeq ($(OS_TARGET),darwin)'#010+
  'i','feq ($(OS_SOURCE),darwin)'#010+
  'DARWIN2DARWIN=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifndef BINUTILSPREFIX'#010+
  'ifndef CROSSBINDIR'#010+
  'ifdef CROSSCOMPILE'#010+
  'ifndef DARWIN2DARWIN'#010+
  'BINUTILSPREFIX=$(CPU_TARGET)-$(OS_TARGET)-'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Try first the full target name, ot','herwise try only'#010+
  '# the OS for backwards compatibility'#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(TARGETSUFFIX))'#010+
  'ifeq ($(UNITSDIR),)'#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_TARGET))'#010+
  'endif'#010+
  #010+
  '# Packages dir'#010+
  'PACKAGESDIR:=$(wildcard $(FPCDIR) $(FPC','DIR)/packages $(FPCDIR)/packa'+
  'ges/base $(FPCDIR)/packages/extra)'#010+
  #010+
  #010+
  '[shelltools]'#010+
  '#####################################################################'#010+
  '# Shell tools'#010+
  '#####################################################################'#010+
  #010+
  '# Echo',' that can redir (must be able run in the default OS shell)'#010+
  'ifndef ECHOREDIR'#010+
  'ifndef inUnix'#010+
  'ECHOREDIR=echo'#010+
  'else'#010+
  'ECHOREDIR=$(ECHO)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To copy pograms'#010+
  'ifndef COPY'#010+
  'COPY:=$(CPPROG) -fp'#010+
  'endif'#010+
  #010+
  '# Copy a whole tree'#010+
  'ifndef COPYTREE'#010+
  'COPYTR','EE:=$(CPPROG) -Rfp'#010+
  'endif'#010+
  #010+
  '# Copy a whole tree'#010+
  'ifndef MKDIRTREE'#010+
  'MKDIRTREE:=$(MKDIRPROG) -p'#010+
  'endif'#010+
  #010+
  '# To move pograms'#010+
  'ifndef MOVE'#010+
  'MOVE:=$(MVPROG) -f'#010+
  'endif'#010+
  #010+
  '# Check delete program'#010+
  'ifndef DEL'#010+
  'DEL:=$(RMPROG) -f'#010+
  'endif'#010+
  #010+
  '# Check deltree program'#010+
  'ifnd','ef DELTREE'#010+
  'DELTREE:=$(RMPROG) -rf'#010+
  'endif'#010+
  #010+
  '# To install files'#010+
  'ifndef INSTALL'#010+
  'ifdef inUnix'#010+
  'INSTALL:=$(GINSTALL) -c -m 644'#010+
  'else'#010+
  'INSTALL:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To install programs'#010+
  'ifndef INSTALLEXE'#010+
  'ifdef inUnix'#010+
  'INSTALLEXE:=$(GINSTALL) -c -m 755',#010+
  'else'#010+
  'INSTALLEXE:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To make a directory.'#010+
  'ifndef MKDIR'#010+
  'MKDIR:=$(GINSTALL) -m 755 -d'#010+
  'endif'#010+
  #010+
  'export ECHOREDIR COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKD'+
  'IR'#010+
  #010+
  #010+
  '[defaultdirs]'#010+
  '########################################','############################'+
  '#'#010+
  '# Default Directories'#010+
  '#####################################################################'#010+
  #010+
  '# Units dir'#010+
  'ifdef REQUIRE_UNITSDIR'#010+
  'override UNITSDIR+=$(REQUIRE_UNITSDIR)'#010+
  'endif'#010+
  #010+
  '# Units dir'#010+
  'ifdef REQUIRE_PACKAGESDI','R'#010+
  'override PACKAGESDIR+=$(REQUIRE_PACKAGESDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '# Unixes use unix dirs with /usr/bin, /usr/lib'#010+
  '# When zipping use the target os default, when normal install then'#010+
  '# use the source os as default'#010+
  'ifdef ZIPINSTALL'#010+
  '# Zipinstall'#010+
  'ifneq ($(f','indstring $(OS_TARGET),$(UNIXs)),)'#010+
  'UNIXHier=1'#010+
  'endif'#010+
  'else'#010+
  '# Normal install'#010+
  'ifneq ($(findstring $(OS_SOURCE),$(UNIXs)),)'#010+
  'UNIXHier=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# When install prefix is not set try to use prefix'#010+
  'ifndef INSTALL_PREFIX'#010+
  'ifdef PREFIX'#010+
  'INSTALL_PRE','FIX=$(PREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the prefix directory where to install everything'#010+
  'ifndef INSTALL_PREFIX'#010+
  'ifdef UNIXHier'#010+
  'INSTALL_PREFIX=/usr/local'#010+
  'else'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_BASEDIR:=/pp'#010+
  'else'#010+
  'INSTALL_BASEDIR:=/$(PACKAGE_NAME)'#010+
  'endif',#010+
  'endif'#010+
  'endif'#010+
  'export INSTALL_PREFIX'#010+
  #010+
  '# Export also INSTALL_SOURCESUBDIR set so it will be'#010+
  '# used recursively for all subdirs'#010+
  'ifdef INSTALL_FPCSUBDIR'#010+
  'export INSTALL_FPCSUBDIR'#010+
  'endif'#010+
  #010+
  '# Where to place the resulting zip files'#010+
  'ifndef DIST_DESTDIR',#010+
  'DIST_DESTDIR:=$(BASEDIR)'#010+
  'endif'#010+
  'export DIST_DESTDIR'#010+
  #010+
  '# EXE/PPU Target directories'#010+
  'ifndef COMPILER_UNITTARGETDIR'#010+
  'ifdef PACKAGEDIR_MAIN'#010+
  'COMPILER_UNITTARGETDIR=$(PACKAGEDIR_MAIN)/units/$(TARGETSUFFIX)'#010+
  'else'#010+
  'COMPILER_UNITTARGETDIR=units/$(TARGET','SUFFIX)'#010+
  'endif'#010+
  'endif'#010+
  'ifndef COMPILER_TARGETDIR'#010+
  'COMPILER_TARGETDIR=.'#010+
  'endif'#010+
  #010+
  #010+
  '#####################################################################'#010+
  '# Install Directories'#010+
  '#####################################################################'#010+
  #010+
  '# ','set the base directory where to install everything'#010+
  'ifndef INSTALL_BASEDIR'#010+
  'ifdef UNIXHier'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)'#010+
  'else'#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'el','se'#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the binaries'#010+
  'ifndef INSTALL_BINDIR'#010+
  'ifdef UNIXHier'#010+
  'INSTALL_BINDIR:=$(INSTALL_PREFIX)/bin'#010+
  'else'#010+
  'INSTALL_BINDIR:=$(INSTALL_BASEDIR)/bin'#010+
  '# for FPC packages i','nstall the binaries under their target subdir'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'ifdef CROSSCOMPILE'#010+
  'ifdef CROSSINSTALL'#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(SOURCESUFFIX)'#010+
  'else'#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(TARGETSUFFIX)'#010+
  'endif'#010+
  'else'#010+
  'INSTALL_BINDIR:','=$(INSTALL_BINDIR)/$(TARGETSUFFIX)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the units.'#010+
  'ifndef INSTALL_UNITDIR'#010+
  'INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/units/$(TARGETSUFFIX)'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'ifdef PACKAGE_NAME'#010+
  'INSTALL','_UNITDIR:=$(INSTALL_UNITDIR)/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install shared libraries'#010+
  'ifndef INSTALL_LIBDIR'#010+
  'ifdef UNIXHier'#010+
  'INSTALL_LIBDIR:=$(INSTALL_PREFIX)/lib'#010+
  'else'#010+
  'INSTALL_LIBDIR:=$(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the ','source files will be stored'#010+
  'ifndef INSTALL_SOURCEDIR'#010+
  'ifdef UNIXHier'#010+
  'ifdef BSDhier'#010+
  'SRCPREFIXDIR=share/src'#010+
  'else'#010+
  'ifdef linuxHier'#010+
  'SRCPREFIXDIR=share/src'#010+
  'else'#010+
  'SRCPREFIXDIR=src'#010+
  'endif'#010+
  'endif'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'ifdef INSTALL_FPCSUBDIR'#010+
  'INSTALL_','SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSI'+
  'ON)/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'else'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX',')/$(SRCPREFIXDIR)/$(PACKAGE_NAME)'+
  '-$(PACKAGE_VERSION)'#010+
  'endif'#010+
  'else'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'ifdef INSTALL_FPCSUBDIR'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(INSTALL_FPCSUBDIR)/$(PA'+
  'CKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/so','urce/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'else'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the doc files will be stored'#010+
  'ifndef INSTALL_DOCDIR'#010+
  'ifdef UNIXHier'#010+
  'ifdef BSDhier'#010+
  'DOCPREFIXDIR=share/doc'#010+
  'else'#010+
  'ifdef linuxHier'#010+
  'DOCPREFIXDIR','=share/doc'#010+
  'else'#010+
  'DOCPREFIXDIR=doc'#010+
  'endif'#010+
  'endif'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/fpc-$(FPC_VERSION)/$'+
  '(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/$(PACKAGE_NAME)-$(PA'+
  'CKAGE_VERS','ION)'#010+
  'endif'#010+
  'else'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc/$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install the examples, under linux we use the doc dir'#010+
  '# because the cop','ytree command will create a subdir itself'#010+
  'ifndef INSTALL_EXAMPLEDIR'#010+
  'ifdef UNIXHier'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  #010+
  'ifdef BSDhier'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#010+
  'else'#010+
  'ifdef linuxHier'#010+
  'INSTALL_','EXAMPLEDIR:=$(INSTALL_DOCDIR)/examples'#010+
  'else'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)/examples/'+
  '$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'else'#010+
  #010+
  'ifdef BSDhier'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/$(PACKAGE_NAME)-$'+
  '(PACKAG','E_VERSION)'#010+
  'else'#010+
  'ifdef linuxHier'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_DOCDIR)/examples/$(PACKAGE_NAME)-$(PACKA'+
  'GE_VERSION)'#010+
  'else'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_NAME)-$(PACKAGE_VE'+
  'RSION)'#010+
  'endif'#010+
  #010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'ifdef INSTALL_FPCPAC','KAGE'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples/$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the some extra (data)files will be stored'#010+
  'ifndef INSTALL_DATADIR'#010+
  'INSTALL_DATADIR=$(INSTALL_','BASEDIR)'#010+
  'endif'#010+
  #010+
  'ifndef INSTALL_SHAREDDIR'#010+
  'INSTALL_SHAREDDIR=$(INSTALL_PREFIX)/lib'#010+
  'endif'#010+
  #010+
  '#####################################################################'#010+
  '# Cross compile dirs'#010+
  '#############################################################','#######'+
  '#'#010+
  #010+
  'ifdef CROSSCOMPILE'#010+
  '# Directory where the cross compile tools are stored.'#010+
  '# First check if they are available in FPCDIR. If no targets/ subdir'#010+
  '# is found use the targets/ subdir in INSTALL_BASEDIR.'#010+
  'ifndef CROSSBINDIR'#010+
  'CROSSBINDIR:=$','(wildcard $(CROSSTARGETDIR)/bin/$(SOURCESUFFIX))'#010+
  'ifeq ($(CROSSBINDIR),)'#010+
  'CROSSBINDIR:=$(wildcard $(INSTALL_BASEDIR)/cross/$(TARGETSUFFIX)/bin/$'+
  '(FULL_SOURCE))'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'CROSSBINDIR='#010+
  'endif'#010+
  #010+
  #010+
  '[dirlibc]'#010+
  '# On linux, try to find where libgc','c.a is.'#010+
  'ifeq ($(OS_SOURCE),linux)'#010+
  #010+
  '# Amd64 to i386?'#010+
  'ifndef GCCLIBDIR'#010+
  'ifeq ($(CPU_TARGET),i386)'#010+
  'ifneq ($(findstring x86_64,$(shell uname -a)),)'#010+
  'ifeq ($(BINUTILSPREFIX),)'#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m32 -print-libgcc-file-name`)'#010+
  'endif'#010+
  'en','dif'#010+
  'endif'#010+
  'ifeq ($(CPU_TARGET),powerpc64)'#010+
  'ifeq ($(BINUTILSPREFIX),)'#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m64 -print-libgcc-file-name`)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Try cross gcc'#010+
  'ifndef GCCLIBDIR'#010+
  'CROSSGCC=$(strip $(wildcard $(addsuffix /$(BINUTILSPREFIX)','gcc$(SRCEX'+
  'EEXT),$(SEARCHPATH))))'#010+
  'ifneq ($(CROSSGCC),)'#010+
  'GCCLIBDIR:=$(shell dirname `$(CROSSGCC) -print-libgcc-file-name`)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Other libraries from ld.so.conf'#010+
  'ifndef OTHERLIBDIR'#010+
  'OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | a','wk '#039'{ ORS='+
  '" "; print $1 }'#039')'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifdef inUnix'#010+
  'ifeq ($(OS_SOURCE),netbsd)'#010+
  'OTHERLIBDIR+=/usr/pkg/lib'#010+
  'endif'#010+
  'export GCCLIBDIR OTHERLIB'#010+
  'endif'#010+
  #010+
  #010+
  '[extensions]'#010+
  '#####################################################################'#010+
  '# Default ','extensions'#010+
  '#####################################################################'#010+
  #010+
  '# Default needed extensions (Go32v2,Linux)'#010+
  'BATCHEXT=.bat'#010+
  'LOADEREXT=.as'#010+
  'EXEEXT=.exe'#010+
  'PPLEXT=.ppl'#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'ASMEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAR','EDLIBEXT=.so'#010+
  'SHAREDLIBPREFIX=libfp'#010+
  'STATICLIBPREFIX=libp'#010+
  'IMPORTLIBPREFIX=libimp'#010+
  'RSTEXT=.rst'#010+
  '#DEBUGSYMEXT #for debugger symbol files, define only for targets which'+
  ' has this'#010+
  #010+
  '# 1.0.x has target specific extensions for ppu files and objects'#010+
  'ife','q ($(findstring 1.0.,$(FPC_VERSION)),)'#010+
  '# short version for 1.1 and above - no target specific extensions'#010+
  #010+
  '# Go32v1'#010+
  'ifeq ($(OS_TARGET),go32v1)'#010+
  'STATICLIBPREFIX='#010+
  'SHORTSUFFIX=v1'#010+
  'endif'#010+
  #010+
  '# Go32v2'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'STATICLIBPREFIX='#010+
  'SHORTS','UFFIX=dos'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# watcom'#010+
  'ifeq ($(OS_TARGET),watcom)'#010+
  'STATICLIBPREFIX='#010+
  'OEXT=.obj'#010+
  'ASMEXT=.asm'#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHORTSUFFIX=wat'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHO','RTSUFFIX=lnx'#010+
  'endif'#010+
  #010+
  '# FreeBSD'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=fbs'#010+
  'endif'#010+
  #010+
  '# NetBSD'#010+
  'ifeq ($(OS_TARGET),netbsd)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=nbs'#010+
  'endif'#010+
  #010+
  '# OpenBSD'#010+
  'ifeq ($(OS_TARGET)',',openbsd)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=obs'#010+
  'endif'#010+
  #010+
  '# Win32'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHORTSUFFIX=w32'#010+
  'endif'#010+
  #010+
  '# OS/2'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'BATCHEXT=.cmd'#010+
  'AOUTEXT=.out'#010+
  'STATICLIBPREFIX='#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHO','RTSUFFIX=os2'#010+
  'ECHO=echo'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# EMX'#010+
  'ifeq ($(OS_TARGET),emx)'#010+
  'BATCHEXT=.cmd'#010+
  'AOUTEXT=.out'#010+
  'STATICLIBPREFIX='#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHORTSUFFIX=emx'#010+
  'ECHO=echo'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# Amiga'#010+
  'ifeq ($(OS_TARGET),amiga)'#010+
  'EXEEXT='#010+
  'SHAREDLIB','EXT=.library'#010+
  'SHORTSUFFIX=amg'#010+
  'endif'#010+
  #010+
  '# MorphOS'#010+
  'ifeq ($(OS_TARGET),morphos)'#010+
  'EXEEXT='#010+
  'SHAREDLIBEXT=.library'#010+
  'SHORTSUFFIX=mos'#010+
  'endif'#010+
  #010+
  '# Atari'#010+
  'ifeq ($(OS_TARGET),atari)'#010+
  'EXEEXT=.ttp'#010+
  'SHORTSUFFIX=ata'#010+
  'endif'#010+
  #010+
  '# BeOS'#010+
  'ifeq ($(OS_TARGET),beos)'#010+
  'BATCHEXT=.sh',#010+
  'EXEEXT='#010+
  'SHORTSUFFIX=be'#010+
  'endif'#010+
  #010+
  '# Haiku'#010+
  'ifeq ($(OS_TARGET),haiku)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'SHORTSUFFIX=hai'#010+
  'endif'#010+
  #010+
  '# Solaris'#010+
  'ifeq ($(OS_TARGET),solaris)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'SHORTSUFFIX=sun'#010+
  'endif'#010+
  #010+
  '# QNX'#010+
  'ifeq ($(OS_TARGET),qnx)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT','='#010+
  'SHORTSUFFIX=qnx'#010+
  'endif'#010+
  #010+
  '# Netware clib'#010+
  'ifeq ($(OS_TARGET),netware)'#010+
  'EXEEXT=.nlm'#010+
  'STATICLIBPREFIX='#010+
  'SHORTSUFFIX=nw'#010+
  'IMPORTLIBPREFIX=imp'#010+
  'endif'#010+
  #010+
  '# Netware libc'#010+
  'ifeq ($(OS_TARGET),netwlibc)'#010+
  'EXEEXT=.nlm'#010+
  'STATICLIBPREFIX='#010+
  'SHORTSUFFIX=nwl'#010+
  'IMPORTLIBPRE','FIX=imp'#010+
  'endif'#010+
  #010+
  '# MacOS'#010+
  'ifeq ($(OS_TARGET),macos)'#010+
  'BATCHEXT='#010+
  'EXEEXT='#010+
  'DEBUGSYMEXT=.xcoff'#010+
  'SHORTSUFFIX=mac'#010+
  'IMPORTLIBPREFIX=imp'#010+
  'endif'#010+
  #010+
  '# Darwin'#010+
  'ifeq ($(OS_TARGET),darwin)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=dwn'#010+
  'endif'#010+
  #010+
  '# gba'#010+
  'ifeq ($(OS','_TARGET),gba)'#010+
  'EXEEXT=.gba'#010+
  'SHAREDLIBEXT=.so'#010+
  'SHORTSUFFIX=gba'#010+
  'endif'#010+
  #010+
  '# Symbian OS'#010+
  'ifeq ($(OS_TARGET),symbian)'#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHORTSUFFIX=symbian'#010+
  'endif'#010+
  #010+
  'else'#010+
  '# long version for 1.0.x - target specific extensions'#010+
  #010+
  '# Go32v1'#010+
  'ifeq ($(OS_TARGET),','go32v1)'#010+
  'PPUEXT=.pp1'#010+
  'OEXT=.o1'#010+
  'ASMEXT=.s1'#010+
  'SMARTEXT=.sl1'#010+
  'STATICLIBEXT=.a1'#010+
  'SHAREDLIBEXT=.so1'#010+
  'STATICLIBPREFIX='#010+
  'SHORTSUFFIX=v1'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# Go32v2'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'STATICLIBPREFIX='#010+
  'SHORTSUFFIX=dos'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# ','watcom'#010+
  'ifeq ($(OS_TARGET),watcom)'#010+
  'STATICLIBPREFIX='#010+
  'SHORTSUFFIX=wat'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=lnx'#010+
  'endif'#010+
  #010+
  '# FreeBSD'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEX','T='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=fbs'#010+
  'endif'#010+
  #010+
  '# NetBSD'#010+
  'ifeq ($(OS_TARGET),netbsd)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=nbs'#010+
  'endif'#010+
  #010+
  '# OpenBSD'#010+
  'ifeq ($(OS_TARGET),openbsd)'#010+
  'BATCHEXT=.sh'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'SHORTSUFFIX=obs'#010+
  'endif'#010+
  #010+
  '# Win32',#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'PPUEXT=.ppw'#010+
  'OEXT=.ow'#010+
  'ASMEXT=.sw'#010+
  'SMARTEXT=.slw'#010+
  'STATICLIBEXT=.aw'#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHORTSUFFIX=w32'#010+
  'endif'#010+
  #010+
  '# OS/2'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'BATCHEXT=.cmd'#010+
  'PPUEXT=.ppo'#010+
  'ASMEXT=.so2'#010+
  'OEXT=.oo2'#010+
  'AOUTEXT=.out'#010+
  'SMARTEXT=.sl2'#010+
  'STA','TICLIBPREFIX='#010+
  'STATICLIBEXT=.ao2'#010+
  'SHAREDLIBEXT=.dll'#010+
  'SHORTSUFFIX=os2'#010+
  'ECHO=echo'#010+
  'IMPORTLIBPREFIX='#010+
  'endif'#010+
  #010+
  '# Amiga'#010+
  'ifeq ($(OS_TARGET),amiga)'#010+
  'EXEEXT='#010+
  'PPUEXT=.ppu'#010+
  'ASMEXT=.s'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.library'#010+
  'SHORTSUFFIX=amg'#010+
  'e','ndif'#010+
  #010+
  '# Atari'#010+
  'ifeq ($(OS_TARGET),atari)'#010+
  'PPUEXT=.ppu'#010+
  'ASMEXT=.s'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'EXEEXT=.ttp'#010+
  'SHORTSUFFIX=ata'#010+
  'endif'#010+
  #010+
  '# BeOS'#010+
  'ifeq ($(OS_TARGET),beos)'#010+
  'BATCHEXT=.sh'#010+
  'PPUEXT=.ppu'#010+
  'ASMEXT=.s'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'EX','EEXT='#010+
  'SHORTSUFFIX=be'#010+
  'endif'#010+
  #010+
  '# Solaris'#010+
  'ifeq ($(OS_TARGET),solaris)'#010+
  'BATCHEXT=.sh'#010+
  'PPUEXT=.ppu'#010+
  'ASMEXT=.s'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'EXEEXT='#010+
  'SHORTSUFFIX=sun'#010+
  'endif'#010+
  #010+
  '# QNX'#010+
  'ifeq ($(OS_TARGET),qnx)'#010+
  'BATCHEXT=.sh'#010+
  'PPUEXT=.ppu'#010+
  'ASMEXT=.s'#010+
  'OEXT=.','o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'EXEEXT='#010+
  'SHORTSUFFIX=qnx'#010+
  'endif'#010+
  #010+
  '# Netware'#010+
  'ifeq ($(OS_TARGET),netware)'#010+
  'STATICLIBPREFIX='#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'ASMEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.nlm'#010+
  'EXEEXT=.nlm'#010+
  'SHORTSUFFIX=nw'#010+
  'IMPORTLIBPREFIX=imp',#010+
  'endif'#010+
  #010+
  '# Netware libc'#010+
  'ifeq ($(OS_TARGET),netwlibc)'#010+
  'STATICLIBPREFIX='#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'ASMEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.nlm'#010+
  'EXEEXT=.nlm'#010+
  'SHORTSUFFIX=nwl'#010+
  'IMPORTLIBPREFIX=imp'#010+
  'endif'#010+
  #010+
  '# MacOS'#010+
  'ifeq ($(OS_TARGET),macos)'#010+
  'BATCH','EXT='#010+
  'PPUEXT=.ppu'#010+
  'ASMEXT=.s'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'EXEEXT='#010+
  'DEBUGSYMEXT=.xcoff'#010+
  'SHORTSUFFIX=mac'#010+
  'IMPORTLIBPREFIX=imp'#010+
  'endif'#010+
  #010+
  '#end of target specific settings'#010+
  'endif'#010+
  #010+
  '# For 8.3 limited OS'#039's the short suffixes'#010+
  '# Otherwise use the full',' source/target names'#010+
  'ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)'#010+
  'FPCMADE=fpcmade.$(SHORTSUFFIX)'#010+
  'ZIPSUFFIX=$(SHORTSUFFIX)'#010+
  'ZIPCROSSPREFIX='#010+
  'ZIPSOURCESUFFIX=src'#010+
  'ZIPEXAMPLESUFFIX=exm'#010+
  'else'#010+
  'FPCMADE=fpcmade.$(TARGETSUFFIX)'#010+
  'ZIPSOURCESUFFIX=.sou','rce'#010+
  'ZIPEXAMPLESUFFIX=.examples'#010+
  'ifdef CROSSCOMPILE'#010+
  'ZIPSUFFIX=.$(SOURCESUFFIX)'#010+
  'ZIPCROSSPREFIX=$(TARGETSUFFIX)-'#010+
  'else'#010+
  'ZIPSUFFIX=.$(TARGETSUFFIX)'#010+
  'ZIPCROSSPREFIX='#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[defaulttools]'#010+
  '#######################################################','#############'+
  '#'#010+
  '# Default Tools'#010+
  '#####################################################################'#010+
  #010+
  '# Names of the binutils tools'#010+
  'ASNAME=$(BINUTILSPREFIX)as'#010+
  'LDNAME=$(BINUTILSPREFIX)ld'#010+
  'ARNAME=$(BINUTILSPREFIX)ar'#010+
  'RCNAME=$(BINUTILSPREFIX)rc'#010,
  'ifneq ($(findstring 1.0.,$(FPC_VERSION)),)'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'ifeq ($(CROSSBINDIR),)'#010+
  'ASNAME=asw'#010+
  'LDNAME=ldw'#010+
  'ARNAME=arw'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# assembler, redefine it if cross compiling'#010+
  'ifndef ASPROG'#010+
  'ifdef CROSSBINDIR'#010+
  'ASPROG=$(CROSSBINDIR',')/$(ASNAME)$(SRCEXEEXT)'#010+
  'else'#010+
  'ASPROG=$(ASNAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# linker, but probably not used'#010+
  'ifndef LDPROG'#010+
  'ifdef CROSSBINDIR'#010+
  'LDPROG=$(CROSSBINDIR)/$(LDNAME)$(SRCEXEEXT)'#010+
  'else'#010+
  'LDPROG=$(LDNAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Resource compiler'#010+
  'ifndef RCPROG'#010+
  'ifde','f CROSSBINDIR'#010+
  'RCPROG=$(CROSSBINDIR)/$(RCNAME)$(SRCEXEEXT)'#010+
  'else'#010+
  'RCPROG=$(RCNAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Archiver'#010+
  'ifndef ARPROG'#010+
  'ifdef CROSSBINDIR'#010+
  'ARPROG=$(CROSSBINDIR)/$(ARNAME)$(SRCEXEEXT)'#010+
  'else'#010+
  'ARPROG=$(ARNAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Override defaults'#010+
  'AS=$(','ASPROG)'#010+
  'LD=$(LDPROG)'#010+
  'RC=$(RCPROG)'#010+
  'AR=$(ARPROG)'#010+
  #010+
  '# ppas.bat / ppas.sh'#010+
  'PPAS=ppas$(SRCBATCHEXT)'#010+
  #010+
  '# ldconfig to rebuild .so cache'#010+
  'ifdef inUnix'#010+
  'LDCONFIG=ldconfig'#010+
  'else'#010+
  'LDCONFIG='#010+
  'endif'#010+
  #010+
  'ifdef DATE'#010+
  'DATESTR:=$(shell $(DATE) +%Y%m%d)'#010+
  'else'#010+
  'DATESTR='#010+
  'en','dif'#010+
  #010+
  '# Look if UPX is found for go32v2 and win32. We can'#039't use $UPX bec'+
  'uase'#010+
  '# upx uses that one itself (PFV)'#010+
  'ifndef UPXPROG'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'UPXPROG:=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'UPXPROG:=1'#010+
  'endif'#010+
  'ifdef UPXPROG'#010+
  'UPXPROG:=$(stri','p $(wildcard $(addsuffix /upx$(SRCEXEEXT),$(SEARCHPAT'+
  'H))))'#010+
  'ifeq ($(UPXPROG),)'#010+
  'UPXPROG='#010+
  'else'#010+
  'UPXPROG:=$(firstword $(UPXPROG))'#010+
  'endif'#010+
  'else'#010+
  'UPXPROG='#010+
  'endif'#010+
  'endif'#010+
  'export UPXPROG'#010+
  #010+
  '# Zip options'#010+
  'ZIPOPT=-9'#010+
  'ZIPEXT=.zip'#010+
  #010+
  '# Tar options'#010+
  'ifeq ($(USETAR),','bz2)'#010+
  'TAROPT=vj'#010+
  'TAREXT=.tar.bz2'#010+
  'else'#010+
  'TAROPT=vz'#010+
  'TAREXT=.tar.gz'#010+
  'endif'#010+
  #010+
  #010+
  '[command_begin]'#010+
  '#####################################################################'#010+
  '# Compiler Command Line'#010+
  '#############################################################','#######'+
  '#'#010+
  #010+
  '# Load commandline OPTDEF and add FPC_CPU define, for compiling the'#010+
  '# compiler this needs to be turned off'#010+
  'ifndef NOCPUDEF'#010+
  'override FPCOPTDEF=$(ARCH)'#010+
  'endif'#010+
  #010+
  #010+
  '# Load commandline OPT and add target and unit dir to be sure'#010+
  'ifneq ($(O','S_TARGET),$(OS_SOURCE))'#010+
  'override FPCOPT+=-T$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  'ifneq ($(CPU_TARGET),$(CPU_SOURCE))'#010+
  'override FPCOPT+=-P$(ARCH)'#010+
  'endif'#010+
  #010+
  'ifeq ($(OS_SOURCE),openbsd)'#010+
  'override FPCOPT+=-FD$(NEW_BINUTILS_PATH)'#010+
  'endif'#010+
  #010+
  'ifndef CROSSBOOTSTRAP'#010+
  'ifneq ($(','BINUTILSPREFIX),)'#010+
  'override FPCOPT+=-XP$(BINUTILSPREFIX)'#010+
  'endif'#010+
  'ifneq ($(BINUTILSPREFIX),)'#010+
  'override FPCOPT+=-Xr$(RLINKPATH)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# User dirs should be first, so they are looked at first'#010+
  'ifdef UNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$','(UNITDIR))'#010+
  'endif'#010+
  'ifdef LIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#010+
  'endif'#010+
  'ifdef OBJDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDIR))'#010+
  'endif'#010+
  'ifdef INCDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(INCDIR))'#010+
  'endif'#010+
  #010+
  '# Smartlinking'#010+
  'ifdef LINKSMART'#010+
  'ov','erride FPCOPT+=-XX'#010+
  'endif'#010+
  #010+
  '# Smartlinking creation'#010+
  'ifdef CREATESMART'#010+
  'override FPCOPT+=-CX'#010+
  'endif'#010+
  #010+
  '# Debug'#010+
  'ifdef DEBUG'#010+
  'override FPCOPT+=-gl'#010+
  'override FPCOPTDEF+=DEBUG'#010+
  'endif'#010+
  #010+
  '# Release mode'#010+
  '# (strip, optimize and don'#039't load fpc.cfg)'#010+
  '# fpc 2.1 ha','s -O2 for all targets'#010+
  'ifdef RELEASE'#010+
  'ifneq ($(findstring 2.0.,$(FPC_VERSION)),)'#010+
  'ifeq ($(CPU_TARGET),i386)'#010+
  'FPCCPUOPT:=-OG2p3'#010+
  'endif'#010+
  'ifeq ($(CPU_TARGET),powerpc)'#010+
  'FPCCPUOPT:=-O1r'#010+
  'endif'#010+
  'else'#010+
  'FPCCPUOPT:=-O2'#010+
  'endif'#010+
  'override FPCOPT+=-Ur -Xs $(FPCCPUO','PT) -n'#010+
  'override FPCOPTDEF+=RELEASE'#010+
  'endif'#010+
  #010+
  '# Strip'#010+
  'ifdef STRIP'#010+
  'override FPCOPT+=-Xs'#010+
  'endif'#010+
  #010+
  '# Optimizer'#010+
  'ifdef OPTIMIZE'#010+
  'override FPCOPT+=-O2'#010+
  'endif'#010+
  #010+
  '# Verbose settings (warning,note,info)'#010+
  'ifdef VERBOSE'#010+
  'override FPCOPT+=-vwni'#010+
  'endif'#010+
  #010+
  '# Needed com','piler options'#010+
  'ifdef COMPILER_OPTIONS'#010+
  'override FPCOPT+=$(COMPILER_OPTIONS)'#010+
  'endif'#010+
  'ifdef COMPILER_UNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(COMPILER_UNITDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_LIBRARYDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARY','DIR))'#010+
  'endif'#010+
  'ifdef COMPILER_OBJECTDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_INCLUDEDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(COMPILER_INCLUDEDIR))'#010+
  'endif'#010+
  #010+
  '# Cross compiler utils'#010+
  'ifdef CROSSBINDIR'#010+
  'override F','PCOPT+=-FD$(CROSSBINDIR)'#010+
  'endif'#010+
  #010+
  '# Target dirs and the prefix to use for clean/install'#010+
  'ifdef COMPILER_TARGETDIR'#010+
  'override FPCOPT+=-FE$(COMPILER_TARGETDIR)'#010+
  'ifeq ($(COMPILER_TARGETDIR),.)'#010+
  'override TARGETDIRPREFIX='#010+
  'else'#010+
  'override TARGETDIRPREFIX=','$(COMPILER_TARGETDIR)/'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Unit target dir (default is units/<cpu>-<os>/'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  'override FPCOPT+=-FU$(COMPILER_UNITTARGETDIR)'#010+
  'ifeq ($(COMPILER_UNITTARGETDIR),.)'#010+
  'override UNITTARGETDIRPREFIX='#010+
  'else'#010+
  'override UN','ITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/'#010+
  'endif'#010+
  'else'#010+
  'ifdef COMPILER_TARGETDIR'#010+
  'override COMPILER_UNITTARGETDIR=$(COMPILER_TARGETDIR)'#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifdef CREATESHARED'#010+
  'override FPCOPT+=-Cg'#010+
  #010+
  'ife','q ($(CPU_TARGET),i386)'#010+
  'override FPCOPT+=-Aas'#010+
  'endif'#010+
  #010+
  'endif'#010+
  #010+
  '# create always pic'#039'ed code on x86_64'#010+
  'ifeq ($(findstring 2.0.,$(FPC_VERSION)),)'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'ifeq ($(CPU_TARGET),x86_64)'#010+
  'override FPCOPT+=-Cg'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifdef L','INKSHARED'#010+
  '#!!! deactived for testing override FPCOPT+=-XD'#010+
  'endif'#010+
  #010+
  #010+
  '[command_libc]'#010+
  '# Add GCC lib path if asked'#010+
  'ifdef GCCLIBDIR'#010+
  'override FPCOPT+=-Fl$(GCCLIBDIR)'#010+
  'endif'#010+
  'ifdef OTHERLIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[','command_end]'#010+
  '# Add commandline options last so they can override'#010+
  'ifdef OPT'#010+
  'override FPCOPT+=$(OPT)'#010+
  'endif'#010+
  #010+
  '# Add defines from FPCOPTDEF to FPCOPT'#010+
  'ifdef FPCOPTDEF'#010+
  'override FPCOPT+=$(addprefix -d,$(FPCOPTDEF))'#010+
  'endif'#010+
  #010+
  '# Was a config file specif','ied ?'#010+
  'ifdef CFGFILE'#010+
  'override FPCOPT+=@$(CFGFILE)'#010+
  'endif'#010+
  #010+
  '# Use the environment to pass commandline options?'#010+
  'ifdef USEENV'#010+
  'override FPCEXTCMD:=$(FPCOPT)'#010+
  'override FPCOPT:=!FPCEXTCMD'#010+
  'export FPCEXTCMD'#010+
  'endif'#010+
  #010+
  'override AFULL_TARGET=$(CPU_TARGET)-$(','OS_TARGET)'#010+
  'override AFULL_SOURCE=$(CPU_SOURCE)-$(OS_SOURCE)'#010+
  #010+
  'ifneq ($(AFULL_TARGET),$(AFULL_SOURCE))'#010+
  'override ACROSSCOMPILE=1'#010+
  'endif'#010+
  #010+
  'ifdef ACROSSCOMPILE'#010+
  'override FPCOPT+=$(CROSSOPT)'#010+
  'endif'#010+
  #010+
  '# Compiler commandline'#010+
  'override COMPILER:=$(FPC) $(','FPCOPT)'#010+
  #010+
  '# also call ppas if with command option -s'#010+
  '# but only if the FULL_SOURCE and FULL_TARGET are equal'#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#010+
  'EXECPPAS='#010+
  'else'#010+
  'ifeq ($(FULL_SOURCE),$(FULL_TARGET))'#010+
  'ifdef RUNBATCH'#010+
  'EXECPPAS:=@$(RUNBATCH) $(PP','AS)'#010+
  'else'#010+
  'EXECPPAS:=@$(PPAS)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  #010+
  '[loaderrules]'#010+
  '#####################################################################'#010+
  '# Loaders'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_loaders'#010+
  #010+
  'ifneq ','($(TARGET_LOADERS),)'#010+
  'override ALLTARGET+=fpc_loaders'#010+
  'override CLEANTARGET+=fpc_loaders_clean'#010+
  'override INSTALLTARGET+=fpc_loaders_install'#010+
  #010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(TARGET_LOADERS))'#010+
  'endif'#010+
  #010+
  '%$(OEXT): %$(LOADEREXT)'#010+
  'ifdef COM','PILER_UNITTARGETDIR'#010+
  '        $(AS) -o $(COMPILER_UNITTARGETDIR)/$*$(OEXT) $<'#010+
  'else'#010+
  '        $(AS) -o $*$(OEXT) $<'#010+
  'endif'#010+
  #010+
  'fpc_loaders: $(COMPILER_UNITTARGETDIR) $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_clean:'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  '        -$(DEL) $(','addprefix $(COMPILER_UNITTARGETDIR)/,$(LOADEROFILE'+
  'S))'#010+
  'else'#010+
  '        -$(DEL) $(LOADEROFILES)'#010+
  'endif'#010+
  #010+
  'fpc_loaders_install:'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  '        $(INSTALL) $(addprefix $(COMPILER_UNITTARGETDIR)','/,$(LOADEROF'+
  'ILES)) $(INSTALL_UNITDIR)'#010+
  'else'#010+
  '        $(INSTALL) $(LOADEROFILES) $(INSTALL_UNITDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[unitrules]'#010+
  '#####################################################################'#010+
  '# Units'#010+
  '#############################################','#######################'+
  '#'#010+
  #010+
  '.PHONY: fpc_units'#010+
  #010+
  'ifneq ($(TARGET_UNITS)$(TARGET_IMPLICITUNITS),)'#010+
  'override ALLTARGET+=fpc_units'#010+
  #010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))'#010+
  'override IMPLICITUNITPPUFILES=$(addsuffix $(PPUEXT),$(T','ARGET_IMPLICI'+
  'TUNITS))'#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#010+
  'endif'#010+
  #010+
  'fpc_units: $(COMPILER_UNITTARGETDIR) $(UNITPPUFILES)'#010+
  #010+
  #010+
  '[exerules]'#010+
  '###############','#####################################################'+
  '#'#010+
  '# Exes'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_exes'#010+
  #010+
  '# Programs are not needed for a cross installation'#010+
  'ifndef CROSSINSTALL'#010+
  'ifneq ($(TARGET_PR','OGRAMS),)'#010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(TARGET_PROGRAMS))'#010+
  'override EXEOFILES:=$(addsuffix $(OEXT),$(TARGET_PROGRAMS)) $(addprefi'+
  'x $(STATICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_PROGRAMS))) '+
  '$(addprefix $(IMPORTLIBPREFIX)',',$(addsuffix $(STATICLIBEXT),$(TARGET_'+
  'PROGRAMS)))'#010+
  #010+
  'override ALLTARGET+=fpc_exes'#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#010+
  'override CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(','TARGET_PROGRAMS))'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),emx)'#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'fpc_exes: $(COMPILER_TARGETDIR) $(COMPILER_UNITTARGETDIR) $(EXEFILES)'#010+
  #010+
  #010+
  '[rstrules]'#010+
  '#######################','#############################################'+
  '#'#010+
  '# Resource strings'#010+
  '#####################################################################'#010+
  #010+
  'ifdef TARGET_RSTS'#010+
  'override RSTFILES=$(addsuffix $(RSTEXT),$(TARGET_RSTS))'#010+
  #010+
  'override CLEANRSTFILES+=$(RS','TFILES)'#010+
  'endif'#010+
  #010+
  #010+
  '[examplerules]'#010+
  '#####################################################################'#010+
  '# Examples'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_examples'#010+
  #010+
  'ifneq ($(TARGET_EXAMPLES),)'#010+
  'HASEXAM','PLES=1'#010+
  'override EXAMPLESOURCEFILES:=$(wildcard $(addsuffix .pp,$(TARGET_EXAMP'+
  'LES)) $(addsuffix .pas,$(TARGET_EXAMPLES)) $(addsuffix .lpr,$(TARGET_E'+
  'XAMPLES)) $(addsuffix .dpr,$(TARGET_EXAMPLES)))'#010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),','$(TARGET_EXAMPLES))'#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(TARGET_EXAMPLES)) $(addp'+
  'refix $(STATICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_EXAMPLES'+
  '))) $(addprefix $(IMPORTLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGE'+
  'T_EXAMPLES',')))'#010+
  #010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),emx)'#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_E','XAMPLES))'#010+
  'endif'#010+
  'endif'#010+
  'ifneq ($(TARGET_EXAMPLEDIRS),)'#010+
  'HASEXAMPLES=1'#010+
  'endif'#010+
  #010+
  'fpc_examples: all $(EXAMPLEFILES) $(addsuffix _all,$(TARGET_EXAMPLEDIR'+
  'S))'#010+
  #010+
  #010+
  '[compilerules]'#010+
  '#####################################################################'#010+
  '# Gen','eral compile rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_all fpc_smart fpc_debug fpc_release fpc_shared'#010+
  #010+
  '$(FPCMADE): $(ALLDEPENDENCIES) $(ALLTARGET)'#010+
  '        @$(ECHOREDIR) Compiled > $(FPCMADE)'#010+
  #010+
  'f','pc_all: $(FPCMADE)'#010+
  #010+
  'fpc_smart:'#010+
  '        $(MAKE) all LINKSMART=1 CREATESMART=1'#010+
  #010+
  'fpc_debug:'#010+
  '        $(MAKE) all DEBUG=1'#010+
  #010+
  'fpc_release:'#010+
  '        $(MAKE) all RELEASE=1'#010+
  #010+
  '# General compile rules, available for both possible .pp and .pas exte'+
  'nsions'#010+
  #010,
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .lpr .dpr .pp .rc .res'#010+
  #010+
  '$(COMPILER_UNITTARGETDIR):'#010+
  '        $(MKDIRTREE) $(COMPILER_UNITTARGETDIR)'#010+
  #010+
  '$(COMPILER_TARGETDIR):'#010+
  '        $(MKDIRTREE) $(COMPILER_TARGETDIR)'#010+
  #010+
  '%$(PPUEXT): %.pp'#010+
  '        $(COM','PILER) $<'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(PPUEXT): %.pas'#010+
  '        $(COMPILER) $<'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(EXEEXT): %.pp'#010+
  '        $(COMPILER) $<'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(EXEEXT): %.pas'#010+
  '        $(COMPILER) $<'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(EXEEXT): %.lpr'#010+
  '      ','  $(COMPILER) $<'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(EXEEXT): %.dpr'#010+
  '        $(COMPILER) $<'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%.res: %.rc'#010+
  '        windres -i $< -o $@'#010+
  #010+
  '# Search paths for .ppu, .pp, .pas, .lpr, .dpr'#010+
  'vpath %.pp $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDED','IR)'#010+
  'vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#010+
  'vpath %.lpr $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#010+
  'vpath %.dpr $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#010+
  'vpath %.inc $(COMPILER_INCLUDEDIR)'#010+
  'vpath %$(OEXT) $(COMPILER_UNIT','TARGETDIR)'#010+
  'vpath %$(PPUEXT) $(COMPILER_UNITTARGETDIR)'#010+
  #010+
  '[sharedrules]'#010+
  '#####################################################################'#010+
  '# Library'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_shared'#010+
  #010,
  'override INSTALLTARGET+=fpc_shared_install'#010+
  #010+
  'ifndef SHARED_LIBVERSION'#010+
  'SHARED_LIBVERSION=$(FPC_VERSION)'#010+
  'endif'#010+
  #010+
  'ifndef SHARED_LIBNAME'#010+
  'SHARED_LIBNAME=$(PACKAGE_NAME)'#010+
  'endif'#010+
  #010+
  'ifndef SHARED_FULLNAME'#010+
  'SHARED_FULLNAME=$(SHAREDLIBPREFIX)$(SHARED_LIBNA','ME)-$(SHARED_LIBVERS'+
  'ION)$(SHAREDLIBEXT)'#010+
  'endif'#010+
  #010+
  '# Default sharedlib units are all unit objects'#010+
  'ifndef SHARED_LIBUNITS'#010+
  'SHARED_LIBUNITS:=$(TARGET_UNITS) $(TARGET_IMPLICITUNITS)'#010+
  'override SHARED_LIBUNITS:=$(filter-out $(INSTALL_BUILDUNIT),$(SHAR','ED'+
  '_LIBUNITS))'#010+
  'endif'#010+
  #010+
  'fpc_shared:'#010+
  'ifdef HASSHAREDLIB'#010+
  '        $(MAKE) all CREATESHARED=1 LINKSHARED=1 CREATESMART=1'#010+
  'ifneq ($(SHARED_BUILD),n)'#010+
  '        $(PPUMOVE) -q $(SHARED_LIBUNITS) -i$(COMPILER_UNITTARGETDIR) -'+
  'o$(SHARED_FULLNAME) -d$(COMPIL','ER_UNITTARGETDIR)'#010+
  'endif'#010+
  'else'#010+
  '        @$(ECHO) Shared Libraries not supported'#010+
  'endif'#010+
  #010+
  'fpc_shared_install:'#010+
  'ifneq ($(SHARED_BUILD),n)'#010+
  'ifneq ($(SHARED_LIBUNITS),)'#010+
  'ifneq ($(wildcard $(COMPILER_UNITTARGETDIR)/$(SHARED_FULLNAME)),)'#010+
  '        $(INSTAL','L) $(COMPILER_UNITTARGETDIR)/$(SHARED_FULLNAME) $(IN'+
  'STALL_SHAREDDIR)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[installrules]'#010+
  '#####################################################################'#010+
  '# Install rules'#010+
  '###################################################','#################'+
  '#'#010+
  #010+
  '.PHONY: fpc_install fpc_sourceinstall fpc_exampleinstall'#010+
  #010+
  'ifdef INSTALL_UNITS'#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(INSTALL_UNITS))'#010+
  'endif'#010+
  #010+
  'ifdef INSTALL_BUILDUNIT'#010+
  'override INSTALLPPUFILES:=$(filter-out $(INST','ALL_BUILDUNIT)$(PPUEXT)'+
  ',$(INSTALLPPUFILES))'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLPPUFILES'#010+
  'override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFI'+
  'LES)) $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT)'+
  ',$(INSTALLPPUFILES))) $(add','prefix $(IMPORTLIBPREFIX),$(subst $(PPUEX'+
  'T),$(STATICLIBEXT),$(INSTALLPPUFILES)))'#010+
  'ifneq ($(UNITTARGETDIRPREFIX),)'#010+
  'override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(notdir '+
  '$(INSTALLPPUFILES)))'#010+
  'override INSTALLPPULINKFILES:=$(wild','card $(addprefix $(UNITTARGETDIR'+
  'PREFIX),$(notdir $(INSTALLPPULINKFILES))))'#010+
  'endif'#010+
  '# Implicitly install Package.fpc'#010+
  'override INSTALL_CREATEPACKAGEFPC=1'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLEXEFILES'#010+
  'ifneq ($(TARGETDIRPREFIX),)'#010+
  'override INSTALLEXEFILES:=$(addpr','efix $(TARGETDIRPREFIX),$(notdir $('+
  'INSTALLEXEFILES)))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'fpc_install: all $(INSTALLTARGET)'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        $(MKDIR) $(INSTALL_BINDIR)'#010+
  '# Compress the exes if upx is defined'#010+
  'ifdef UPXPROG'#010+
  '        -$(UPXPROG) $(INSTALLE','XEFILES)'#010+
  'endif'#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)'#010+
  'endif'#010+
  'ifdef INSTALL_CREATEPACKAGEFPC'#010+
  'ifdef FPCMAKE'#010+
  '# If the fpcpackage variable is set then create and install Package.fp'+
  'c,'#010+
  '# a safety check is done if Makefile.fpc i','s available'#010+
  'ifdef PACKAGE_VERSION'#010+
  'ifneq ($(wildcard Makefile.fpc),)'#010+
  '        $(FPCMAKE) -p -T$(CPU_TARGET)-$(OS_TARGET) Makefile.fpc'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#010+
  '        $(INSTALL) Package.fpc $(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endi','f'#010+
  'ifdef INSTALLPPUFILES'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(INSTALL_UNITDIR)'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '        $(INSTALL) $(INSTALLPPULINKFILES) $(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'ifneq ($(wildcard $(LIB_','FULLNAME)),)'#010+
  '        $(MKDIR) $(INSTALL_LIBDIR)'#010+
  '        $(INSTALL) $(LIB_FULLNAME) $(INSTALL_LIBDIR)'#010+
  'ifdef inUnix'#010+
  '        ln -sf $(LIB_FULLNAME) $(INSTALL_LIBDIR)/$(LIB_NAME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'ifdef INSTALL_FILES'#010+
  '        $(MKDIR) $(INSTALL_','DATADIR)'#010+
  '        $(INSTALL) $(INSTALL_FILES) $(INSTALL_DATADIR)'#010+
  'endif'#010+
  #010+
  'fpc_sourceinstall: distclean'#010+
  '        $(MKDIR) $(INSTALL_SOURCEDIR)'#010+
  '        $(COPYTREE) $(BASEDIR)/* $(INSTALL_SOURCEDIR)'#010+
  #010+
  'fpc_exampleinstall: $(addsuffix _distclean,$(TA','RGET_EXAMPLEDIRS))'#010+
  'ifdef HASEXAMPLES'#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDIR)'#010+
  'endif'#010+
  'ifdef EXAMPLESOURCEFILES'#010+
  '        $(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)'#010+
  'endif'#010+
  'ifdef TARGET_EXAMPLEDIRS'#010+
  '        $(COPYTREE) $(addsuffix /*,$(TARGE','T_EXAMPLEDIRS)) $(INSTALL_'+
  'EXAMPLEDIR)'#010+
  'endif'#010+
  #010+
  '[distinstallrules]'#010+
  '#####################################################################'#010+
  '# Dist Install'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_distinst','all'#010+
  #010+
  'fpc_distinstall: install exampleinstall'#010+
  #010+
  #010+
  '[zipinstallrules]'#010+
  '#####################################################################'#010+
  '# Zip'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_zipinstall fpc_z','ipsourceinstall fpc_zipexampleinstall'#010+
  #010+
  '# Temporary path to pack a file, can only use a single deep'#010+
  '# subdir, because the deltree can'#039't see the whole tree to remove'#010+
  'ifndef PACKDIR'#010+
  'ifndef inUnix'#010+
  'PACKDIR=$(BASEDIR)/../fpc-pack'#010+
  'else'#010+
  'PACKDIR=/tm','p/fpc-pack'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Maybe create default zipname from packagename'#010+
  'ifndef ZIPNAME'#010+
  'ifdef DIST_ZIPNAME'#010+
  'ZIPNAME=$(DIST_ZIPNAME)'#010+
  'else'#010+
  'ZIPNAME=$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifndef FULLZIPNAME'#010+
  'FULLZIPNAME=$(ZIPCROSSPREFIX)$(ZIPPREFIX)$(ZIPNAM','E)$(ZIPSUFFIX)'#010+
  'endif'#010+
  #010+
  '# ZipTarget'#010+
  'ifndef ZIPTARGET'#010+
  'ifdef DIST_ZIPTARGET'#010+
  'ZIPTARGET=DIST_ZIPTARGET'#010+
  'else'#010+
  'ZIPTARGET=install'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Use tar by default under linux'#010+
  'ifndef USEZIP'#010+
  'ifdef inUnix'#010+
  'USETAR=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Use a wrapper script by',' default for Os/2'#010+
  'ifndef inUnix'#010+
  'USEZIPWRAPPER=1'#010+
  'endif'#010+
  #010+
  '# We need to be able to run in the current OS so fix'#010+
  '# the path separator'#010+
  'ifdef USEZIPWRAPPER'#010+
  'ZIPPATHSEP=$(PATHSEP)'#010+
  'ZIPWRAPPER=$(subst /,$(PATHSEP),$(DIST_DESTDIR)/fpczip$(SRCBATCHEXT))',
  #010+
  'else'#010+
  'ZIPPATHSEP=/'#010+
  'endif'#010+
  #010+
  '# Create commands to create the zip/tar file'#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(ZIPPATHSEP),$(PACKDIR))'#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(ZIPPATHSEP),$(BASEDIR))'#010+
  'ifdef USETAR'#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(FULLZIPNAME)$(TAR','EXT)'#010+
  'ZIPCMD_ZIP:=$(TARPROG) cf$(TAROPT) $(ZIPDESTFILE) *'#010+
  'else'#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(FULLZIPNAME)$(ZIPEXT)'#010+
  'ZIPCMD_ZIP:=$(subst /,$(ZIPPATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDES'+
  'TFILE) *'#010+
  'endif'#010+
  #010+
  'fpc_zipinstall:'#010+
  '        $(MAKE) $(ZIPTA','RGET) INSTALL_PREFIX=$(PACKDIR) ZIPINSTALL=1'#010+
  '        $(MKDIR) $(DIST_DESTDIR)'#010+
  '        $(DEL) $(ZIPDESTFILE)'#010+
  'ifdef USEZIPWRAPPER'#010+
  '# Handle gecho separate as we need to espace \ with \\'#010+
  'ifneq ($(ECHOREDIR),echo)'#010+
  '        $(ECHOREDIR) -e "$(subs','t \,\\,$(ZIPCMD_CDPACK))" > $(ZIPWRAP'+
  'PER)'#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_ZIP))" >> $(ZIPWRAPPER)'+
  #010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDBASE))" >> $(ZIPWRAPP'+
  'ER)'#010+
  'else'#010+
  '        echo $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#010+
  '  ','      echo $(ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#010+
  '        echo $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)'#010+
  'endif'#010+
  'ifdef inUnix'#010+
  '        /bin/sh $(ZIPWRAPPER)'#010+
  'else'#010+
  'ifdef RUNBATCH'#010+
  '        $(RUNBATCH) $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ZIPWRAPPER)'#010+
  'endif'#010+
  'endif'#010+
  '        $','(DEL) $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(ZIPCMD_CDBASE)'#010+
  'endif'#010+
  '        $(DELTREE) $(PACKDIR)'#010+
  #010+
  'fpc_zipsourceinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=$(ZIP'+
  'SOURCESUFFIX)'#010+
  #010+
  'fpc_zipexam','pleinstall:'#010+
  'ifdef HASEXAMPLES'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall ZIPSUFFIX=$(ZI'+
  'PEXAMPLESUFFIX)'#010+
  'endif'#010+
  #010+
  'fpc_zipdistinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=distinstall'#010+
  #010+
  #010+
  '[cleanrules]'#010+
  '#############################','#######################################'+
  '#'#010+
  '# Clean rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#010+
  #010+
  'ifdef EXEFILES'#010+
  'override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX','),$(CLEANEXEFIL'+
  'ES))'#010+
  'endif'#010+
  #010+
  'ifdef CLEAN_UNITS'#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS))'#010+
  'endif'#010+
  #010+
  'ifdef CLEANPPUFILES'#010+
  'override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)'+
  ') $(addprefix $(STATICLIBPREFIX),$(s','ubst $(PPUEXT),$(STATICLIBEXT),$'+
  '(CLEANPPUFILES))) $(addprefix $(IMPORTLIBPREFIX),$(subst $(PPUEXT),$(S'+
  'TATICLIBEXT),$(CLEANPPUFILES)))'#010+
  'ifdef DEBUGSYMEXT'#010+
  'override CLEANPPULINKFILES+=$(subst $(PPUEXT),$(DEBUGSYMEXT),$(CLEANPP'+
  'UFILES))'#010+
  'endif'#010+
  'ove','rride CLEANPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPP'+
  'UFILES))'#010+
  'override CLEANPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPREF'+
  'IX),$(CLEANPPULINKFILES)))'#010+
  'endif'#010+
  #010+
  'fpc_clean: $(CLEANTARGET)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) ','$(CLEANEXEFILES)'#010+
  'endif'#010+
  'ifdef CLEANPPUFILES'#010+
  '        -$(DEL) $(CLEANPPUFILES)'#010+
  'endif'#010+
  'ifneq ($(CLEANPPULINKFILES),)'#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#010+
  'endif'#010+
  'ifdef CLEANRSTFILES'#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILE','S))'+
  #010+
  'endif'#010+
  'ifdef CLEAN_FILES'#010+
  '        -$(DEL) $(CLEAN_FILES)'#010+
  'endif'#010+
  'ifdef LIB_NAME'#010+
  '        -$(DEL) $(LIB_NAME) $(LIB_FULLNAME)'#010+
  'endif'#010+
  '        -$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(F'+
  'PCEXTFILE) $(REDIRFILE)'#010+
  '        -$(DEL) ','*$(ASMEXT) *_ppas$(BATCHEXT)'#010+
  #010+
  'fpc_cleanall: $(CLEANTARGET)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  'ifdef CLEANPPUFILES'#010+
  '        -$(DEL) $(CLEANPPUFILES)'#010+
  'endif'#010+
  'ifneq ($(CLEANPPULINKFILES),)'#010+
  '    ','    -$(DEL) $(CLEANPPULINKFILES)'#010+
  'endif'#010+
  'ifdef CLEANRSTFILES'#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#010+
  'endif'#010+
  'endif'#010+
  '        -$(DELTREE) units'#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT)',' *$(SHAREDLIBEXT) *$(PPLEXT)'#010+
  'ifneq ($(PPUEXT),.ppu)'#010+
  '        -$(DEL) *.o *.ppu *.a'#010+
  'endif'#010+
  '        -$(DELTREE) *$(SMARTEXT)'#010+
  '        -$(DEL) fpcmade.* Package.fpc $(PPAS) script.res link.res $(FP'+
  'CEXTFILE) $(REDIRFILE)'#010+
  '        -$(DEL) *_ppas$(BA','TCHEXT)'#010+
  'ifdef AOUTEXT'#010+
  '        -$(DEL) *$(AOUTEXT)'#010+
  'endif'#010+
  'ifdef DEBUGSYMEXT'#010+
  '        -$(DEL) *$(DEBUGSYMEXT)'#010+
  'endif'#010+
  #010+
  'fpc_distclean: cleanall'#010+
  #010+
  #010+
  '[baseinforules]'#010+
  '#####################################################################'#010+
  '# Base info rul','es'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_baseinfo'#010+
  #010+
  'override INFORULES+=fpc_baseinfo'#010+
  #010+
  'fpc_baseinfo:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Package info =='#010+
  '        @$(ECHO)  Package Name..... $(PACK','AGE_NAME)'#010+
  '        @$(ECHO)  Package Version.. $(PACKAGE_VERSION)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Configuration info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  FPC.......... $(FPC)'#010+
  '        @$(ECHO)  FPC Version.. $(FPC_VERSION)'#010+
  '        @$(E','CHO)  Source CPU... $(CPU_SOURCE)'#010+
  '        @$(ECHO)  Target CPU... $(CPU_TARGET)'#010+
  '        @$(ECHO)  Source OS.... $(OS_SOURCE)'#010+
  '        @$(ECHO)  Target OS.... $(OS_TARGET)'#010+
  '        @$(ECHO)  Full Source.. $(FULL_SOURCE)'#010+
  '        @$(ECHO)  Full ','Target.. $(FULL_TARGET)'#010+
  '        @$(ECHO)  SourceSuffix. $(SOURCESUFFIX)'#010+
  '        @$(ECHO)  TargetSuffix. $(TARGETSUFFIX)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Directory info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Required pkgs... $(REQUIRE_PA','CKAGES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#010+
  '        @$(ECHO)  FPCDir.......... $(FPCDIR)'#010+
  '        @$(ECHO)  CrossBinDir..... $(CROSSBINDIR)'#010+
  '        @$(ECHO)  UnitsDir........ $(UNITSDIR)'#010+
  '        @$(ECHO)  PackagesD','ir..... $(PACKAGESDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Tools info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  As........ $(AS',')'#010+
  '        @$(ECHO)  Ld........ $(LD)'#010+
  '        @$(ECHO)  Ar........ $(AR)'#010+
  '        @$(ECHO)  Rc........ $(RC)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Mv........ $(MVPROG)'#010+
  '        @$(ECHO)  Cp........ $(CPPROG)'#010+
  '        @$(ECHO)  Rm........ $(RMPROG)',#010+
  '        @$(ECHO)  GInstall.. $(GINSTALL)'#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#010+
  '        @$(ECHO)  Shell..... $(SHELL)'#010+
  '        @$(ECHO)  Date...... $(DATE)'#010+
  '        @$(ECHO)  FPCMake... $(FPCMAKE)'#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#010+
  '      ','  @$(ECHO)  Upx....... $(UPXPROG)'#010+
  '        @$(ECHO)  Zip....... $(ZIPPROG)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Object info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Target Loaders........ $(TARGET_LOADERS)'#010+
  '        @$(ECHO)  Target Units.......','... $(TARGET_UNITS)'#010+
  '        @$(ECHO)  Target Implicit Units. $(TARGET_IMPLICITUNITS)'#010+
  '        @$(ECHO)  Target Programs....... $(TARGET_PROGRAMS)'#010+
  '        @$(ECHO)  Target Dirs........... $(TARGET_DIRS)'#010+
  '        @$(ECHO)  Target Examples......','. $(TARGET_EXAMPLES)'#010+
  '        @$(ECHO)  Target ExampleDirs.... $(TARGET_EXAMPLEDIRS)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Clean Units......... $(CLEAN_UNITS)'#010+
  '        @$(ECHO)  Clean Files......... $(CLEAN_FILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECH','O)  Install Units....... $(INSTALL_UNITS)'#010+
  '        @$(ECHO)  Install Files....... $(INSTALL_FILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Install info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#010+
  '        @$(ECHO)  Zi','pName.............. $(ZIPNAME)'#010+
  '        @$(ECHO)  ZipPrefix............ $(ZIPPREFIX)'#010+
  '        @$(ECHO)  ZipCrossPrefix....... $(ZIPCROSSPREFIX)'#010+
  '        @$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)'#010+
  '        @$(ECHO)  FullZipName.......... $(FUL','LZIPNAME)'#010+
  '        @$(ECHO)  Install FPC Package.. $(INSTALL_FPCPACKAGE)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)'#010+
  '        @$(ECHO)  Install binary dir... $(INSTALL_BINDIR)'#010+
  '        @$(ECHO)  Install library ','dir.. $(INSTALL_LIBDIR)'#010+
  '        @$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)'#010+
  '        @$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)'#010+
  '        @$(ECHO)  Install doc dir...... $(INSTALL_DOCDIR)'#010+
  '        @$(ECHO)  Install example dir.','. $(INSTALL_EXAMPLEDIR)'#010+
  '        @$(ECHO)  Install data dir..... $(INSTALL_DATADIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Dist destination dir. $(DIST_DESTDIR)'#010+
  '        @$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[inforules',']'#010+
  '#####################################################################'#010+
  '# Info rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_info'#010+
  #010+
  'fpc_info: $(INFORULES)'#010+
  #010+
  '[makefilerules]'#010+
  '##########################','##########################################'+
  '#'#010+
  '# Rebuild Makefile'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_makefile fpc_makefiles fpc_makefile_sub1 fpc_makefile_sub2'+
  ' \'#010+
  '        fpc_makefile_dirs'#010+
  #010+
  'fpc_ma','kefile:'#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) Makefile.fpc'#010+
  #010+
  'fpc_makefile_sub1:'#010+
  'ifdef TARGET_DIRS'#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGE'+
  'T_DIRS))'#010+
  'endif'#010+
  'ifdef TARGET_EXAMPLEDIRS'#010+
  '        $(FPCMAKE) -w -T$(OS_TARGE','T) $(addsuffix /Makefile.fpc,$(TAR'+
  'GET_EXAMPLEDIRS))'#010+
  'endif'#010+
  #010+
  'fpc_makefile_sub2: $(addsuffix _makefile_dirs,$(TARGET_DIRS) $(TARGET_'+
  'EXAMPLEDIRS))'#010+
  #010+
  'fpc_makefile_dirs: fpc_makefile_sub1 fpc_makefile_sub2'#010+
  #010+
  'fpc_makefiles: fpc_makefile fpc_makefile','_dirs'#010+
  #010+
  '[localmakefile]'#010+
  '#####################################################################'#010+
  '# Local Makefile'#010+
  '#####################################################################'#010+
  #010+
  'ifneq ($(wildcard fpcmake.loc),)'#010+
  'include fpcmake.loc'#010+
  'endif'#010,
  #010+
  #010+
  '[userrules]'#010+
  '#####################################################################'#010+
  '# Users rules'#010+
  '#####################################################################'#010+
  #010+
  '[lclrules]'#010+
  '############################################################','########'+
  '#'#010+
  '# LCL Rules'#010+
  '#####################################################################'#010+
  #010+
  '# LCL Platform'#010+
  'ifndef LCL_PLATFORM'#010+
  'ifneq ($(findstring $(OS_TARGET),win32 win64),)'#010+
  'LCL_PLATFORM=win32'#010+
  'else'#010+
  'LCL_PLATFORM=gtk'#010+
  'endif'#010+
  'endif'#010+
  'export LCL_','PLATFORM'#010+
  #010+
  '# Check if the specified LCLDIR is correct'#010+
  'ifdef LCLDIR'#010+
  'override LCLDIR:=$(subst \,/,$(LCLDIR))'#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#010+
  'override LCLDIR=wrong'#010+
  'endif'#010+
  'else'#010+
  'override LCLDIR=wrong'#010+
  'endif'#010+
  #010+
  '# Check if the defau','lt LCLDIR is correct'#010+
  'ifdef DEFAULT_LCLDIR'#010+
  'override LCLDIR:=$(subst \,/,$(DEFAULT_LCLDIR))'#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#010+
  'override LCLDIR=wrong'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Check for development version'#010+
  'ifeq ($(LCLDIR),wrong)'#010+
  'override',' LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildca'+
  'rd $(addsuffix /units/$(LCL_PLATFORM),$(BASEDIR)/lcl $(BASEDIR)))))'#010+
  'ifeq ($(LCLDIR),)'#010+
  'override LCLDIR=wrong'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Check for release version'#010+
  'ifeq ($(LCLDIR),wrong)'#010+
  'override',' LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildca'+
  'rd $(addsuffix /lib/lazarus/units/$(LCL_PLATFORM),/usr/local /usr))))'#010+
  'ifeq ($(LCLDIR),)'#010+
  'override LCLDIR=wrong'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Generate dirs'#010+
  'override LCLUNITDIR:=$(wildcard $(LCLDIR)','/units/$(LCL_PLATFORM) $(LC'+
  'LDIR)/units)'#010+
  'override LCLCOMPONENTDIR:=$(wildcard $(LCLDIR)/.. $(LCLDIR)/../compone'+
  'nts $(LCLDIR)/components)'#010+
  'export LCLDIR LCLUNITDIR LCLCOMPONENTDIR'#010+
  #010+
  '# Add LCL dirs to paths'#010+
  'override REQUIRE_PACKAGESDIR+=$(LCLCOM','PONENTDIR)'#010+
  'override COMPILER_UNITDIR+=$(LCLUNITDIR)'#010+
  #010+
  '[lclinforules]'#010+
  '#####################################################################'#010+
  '# LCL Info rules'#010+
  '#####################################################################'#010+
  'override INFORU','LES+=lclinfo'#010+
  #010+
  '.PHONY: lclinfo'#010+
  #010+
  'lclinfo:'#010+
  '        @$(ECHO)  == LCL info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Platform............. $(LCL_PLATFORM)'#010+
  '        @$(ECHO)  LCLDIR............... $(LCLDIR)'#010+
  '        @$(ECHO)  LCL Unit dir......... $(LC','LUNITDIR)'#010+
  '        @$(ECHO)  LCL Component dir.... $(LCLCOMPONENTDIR)'#010+
  '        @$(ECHO)'#010
);
