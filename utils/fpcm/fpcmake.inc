{$ifdef Delphi}
const fpcmakeini : array[0..231] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..231,1..240] of char=(
{$endif Delphi}
  ';'#013#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#013+
  #010+
  ';'#013#010+
  #013#010+
  '[defines]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Misc defines to be used by anyone'#013#010+
  '#############################################','#######################'+
  '#'#013#010+
  #013#010+
  '# OS categories'#013#010+
  'BSDs = freebsd netbsd openbsd darwin'#013#010+
  'UNIXs = linux $(BSDs) solaris qnx haiku aix'#013#010+
  'LIMIT83fs = go32v2 os2 emx watcom msdos'#013#010+
  'OSNeedsComspecToRunBatch = go32v2 watcom'#013#010+
  #013#010+
  '#Empty target for rules tha','t always should run. Needed if'#013#010+
  '#the target is non-phoney, and there is non-phony prereqisites.'#013#010+
  '#Then add FORCE as an prerequisite'#013#010+
  '#See gnu make manual: 4.7 Rules without Commands or Prerequisites'#013#010+
  'FORCE:'#013#010+
  '.PHONY: FORCE'#013#010+
  #013#010+
  '[osdetect]'#013#010+
  '###','#################################################################'+
  '#'#013#010+
  '# Autodetect source OS (Linux or Dos or Windows NT or OS/2 or other)'#013+
  #010+
  '# define inUnix when running under Unix like environment'#013#010+
  '#               (Linux,FreeBSD,NetBSD,OpenBS','D,Darwin,Cygwin)'#013#010+
  '# define inWinNT when running under WinNT'#013#010+
  '# define inOS2 when running under OS/2'#013#010+
  '# define inCygwin when running under Cygwin32'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# We need only / in',' the path also remove the current dir,'#013#010+
  '# also remove trailing /'#039's'#013#010+
  'override PATH:=$(patsubst %/,%,$(subst \,/,$(PATH)))'#013#010+
  #013#010+
  '# Detect unix'#013#010+
  '# Darwin is handled specially'#013#010+
  'ifneq ($(findstring darwin,$(OSTYPE)),)'#013#010+
  'inUnix=1 #darwin'#013#010+
  'SEARCHPATH:','=$(filter-out .,$(subst :, ,$(PATH)))'#013#010+
  'else'#013#010+
  '# Determine if we'#039've a unix searchpath by looking for a ;'#013#010+
  '# that normally doesn'#039't exists in the unix PATH var.'#013#010+
  'ifeq ($(findstring ;,$(PATH)),)'#013#010+
  'inUnix=1'#013#010+
  'SEARCHPATH:=$(filter-out .,$(subst :, ,','$(PATH)))'#013#010+
  'else'#013#010+
  'SEARCHPATH:=$(subst ;, ,$(PATH))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Add path were make is located'#013#010+
  'SEARCHPATH+=$(patsubst %/,%,$(subst \,/,$(dir $(MAKE))))'#013#010+
  #013#010+
  '# Search for PWD'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(SEARCHPATH))))'#013,#010+
  'ifeq ($(PWD),)'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(SEARCHPATH))))'#013#010+
  'ifeq ($(PWD),)'#013#010+
  '$(error You need the GNU utils package to use this Makefile)'#013#010+
  'else'#013#010+
  'PWD:=$(firstword $(PWD))'#013#010+
  'SRCEXEEXT='#013#010+
  'endif'#013#010+
  'else'#013#010+
  'PWD:=$(firstword $(PWD))'#013#010+
  'SRC','EXEEXT=.exe'#013#010+
  'endif'#013#010+
  #013#010+
  '# Detect NT - NT sets OS to Windows_NT'#013#010+
  '# Detect OS/2 - OS/2 has OS2_SHELL defined'#013#010+
  'ifndef inUnix'#013#010+
  'ifeq ($(OS),Windows_NT)'#013#010+
  'inWinNT=1'#013#010+
  'else'#013#010+
  'ifdef OS2_SHELL'#013#010+
  'inOS2=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifneq ($(findstring cygdrive,$(','PATH)),)'#013#010+
  'inCygWin=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# The extension of batch files / scripts'#013#010+
  'ifdef inUnix'#013#010+
  'SRCBATCHEXT=.sh'#013#010+
  'else'#013#010+
  'ifdef inOS2'#013#010+
  'SRCBATCHEXT=.cmd'#013#010+
  'else'#013#010+
  'SRCBATCHEXT=.bat'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef COMSPEC'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(OS','NeedsComspecToRunBatch)),)'#013#010+
  'ifndef RUNBATCH'#013#010+
  'RUNBATCH=$(COMSPEC) /C'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Path Separator, the subst trick is necessary for the \ that can'#039't'+
  ' exists'#013#010+
  '# at the end of a line'#013#010+
  'ifdef inUnix'#013#010+
  'PATHSEP=/'#013#010+
  'else'#013#010+
  'PATHSEP:=$(subst ','/,\,/)'#013#010+
  '# cygwin bash or sh can not handle backslashs'#013#010+
  'ifdef inCygWin'#013#010+
  'PATHSEP=/'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Base dir'#013#010+
  'ifdef PWD'#013#010+
  'BASEDIR:=$(subst \,/,$(shell $(PWD)))'#013#010+
  '# For Cygwin we need to replace /cygdrive/c/ with c:/'#013#010+
  'ifdef inCygWin'#013#010+
  'ifneq ($(','findstring /cygdrive/,$(BASEDIR)),)'#013#010+
  'BASENODIR:=$(patsubst /cygdrive%,%,$(BASEDIR))'#013#010+
  'BASEDRIVE:=$(firstword $(subst /, ,$(BASENODIR)))'#013#010+
  'BASEDIR:=$(subst /cygdrive/$(BASEDRIVE)/,$(BASEDRIVE):/,$(BASEDIR))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'BASEDIR=.'#013#010+
  'endi','f'#013#010+
  #013#010+
  '# Echo is an internal command under OS/2 (and others), so it'#039's alw'+
  'ays found'#013#010+
  'ifdef inOS2'#013#010+
  'ifndef ECHO'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))'+
  '))'#013#010+
  'ifeq ($(ECHO),)'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /ech','o$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifeq ($(ECHO),)'#013#010+
  'ECHO=echo'#013#010+
  'else'#013#010+
  'ECHO:=$(firstword $(ECHO))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ECHO:=$(firstword $(ECHO))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export ECHO'#013#010+
  'endif'#013#010+
  #013#010+
  '[fpcdetect]'#013#010+
  '########################################################','############'+
  '#'#013#010+
  '# FPC Binary and Version Detection'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Compatibility with old makefiles'#013#010+
  'ifndef FPC'#013#010+
  'ifdef PP'#013#010+
  'FPC=$(PP)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Try to detect the ppcXXX file',' to use by using "fpc -PB" option'#013+
  #010+
  '# to query for the default ppcXXX the fpc executable tries. When'#013#010+
  '# fpc is not found use ppc386 by default. Also when fpc -PB gives'#013#010+
  '# an error ppc386 will be used.'#013#010+
  'ifndef FPC'#013#010+
  '# check if fpc exists'#013#010+
  'FPCP','ROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPAT'+
  'H))))'#013#010+
  'ifneq ($(FPCPROG),)'#013#010+
  'FPCPROG:=$(firstword $(FPCPROG))'#013#010+
  'ifneq ($(CPU_TARGET),)'#013#010+
  'FPC:=$(shell $(FPCPROG) -P$(CPU_TARGET) -PB)'#013#010+
  'else'#013#010+
  'FPC:=$(shell $(FPCPROG) -PB)'#013#010+
  'endif'#013#010+
  '#',' Older fpc executables didn'#039't support it and return'#013#010+
  '# Error: Illegal processor... If found then fallback to ppc386'#013#010+
  'ifneq ($(findstring Error,$(FPC)),)'#013#010+
  'override FPC=$(firstword $(strip $(wildcard $(addsuffix /ppc386$(SRCEX'+
  'EEXT),$(SEARCHPA','TH)))))'#013#010+
  'else'#013#010+
  '# if the cross compiler is not found, fall back to fpc'#013#010+
  'ifeq ($(strip $(wildcard $(FPC))),)'#013#010+
  'FPC:=$(firstword $(FPCPROG))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# fpc binary not found, fallback to ppc386'#013#010+
  'override FPC=$(firstword $(strip $(wil','dcard $(addsuffix /ppc386$(SRC'+
  'EXEEXT),$(SEARCHPATH)))))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Get a clean executable name'#013#010+
  'override FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#013#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#013#010+
  #013#010+
  '# Try to find the binary direct first, oth','erwise in the path,'#013#010+
  '# if not found give an error'#013#010+
  'FOUNDFPC:=$(strip $(wildcard $(FPC)))'#013#010+
  'ifeq ($(FOUNDFPC),)'#013#010+
  'FOUNDFPC=$(strip $(wildcard $(addsuffix /$(FPC),$(SEARCHPATH))))'#013#010+
  'ifeq ($(FOUNDFPC),)'#013#010+
  '$(error Compiler $(FPC) not found)'#013#010+
  'endif'#013,#010+
  'endif'#013#010+
  #013#010+
  '# For 1.1 and up we can use a single compiler call to retrieve'#013#010+
  '# all needed information'#013#010+
  'ifndef FPC_COMPILERINFO'#013#010+
  'FPC_COMPILERINFO:=$(shell $(FPC) -iVSPTPSOTO)'#013#010+
  'endif'#013#010+
  #013#010+
  '# FPC version'#013#010+
  'ifndef FPC_VERSION'#013#010+
  'FPC_VERSION:=$(word 1,$(','FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  #013#010+
  'export FPC FPC_VERSION FPC_COMPILERINFO'#013#010+
  '# CHECKDEPEND should not be exported'#013#010+
  '# This should limit multiple checks'#013#010+
  'unexport CHECKDEPEND ALLDEPENDENCIES'#013#010+
  #013#010+
  '#######################################################','#############'+
  '#'#013#010+
  '# FPC Target Detection'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Fall back to default values if needed'#013#010+
  'ifndef CPU_TARGET'#013#010+
  'ifdef CPU_TARGET_DEFAULT'#013#010+
  'CPU_TARGET=$(CPU_TARGET_DEFAULT)'#013#010+
  'endif'#013#010,
  'endif'#013#010+
  'ifndef OS_TARGET'#013#010+
  'ifdef OS_TARGET_DEFAULT'#013#010+
  'OS_TARGET=$(OS_TARGET_DEFAULT)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Retrieve Target/Source CPU and Target/Source OS'#013#010+
  'ifndef CPU_SOURCE'#013#010+
  'CPU_SOURCE:=$(word 2,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef CPU_TARGET'#013#010+
  'CPU','_TARGET:=$(word 3,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef OS_SOURCE'#013#010+
  'OS_SOURCE:=$(word 4,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef OS_TARGET'#013#010+
  'OS_TARGET:=$(word 5,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'FULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)'#013#010+
  'FULL_SOURCE=$(CPU_S','OURCE)-$(OS_SOURCE)'#013#010+
  #013#010+
  'ifeq ($(CPU_TARGET),armeb)'#013#010+
  'ARCH=arm'#013#010+
  'override FPCOPT+=-Cb'#013#010+
  'else'#013#010+
  'ifeq ($(CPU_TARGET),armel)'#013#010+
  'ARCH=arm'#013#010+
  'override FPCOPT+=-CaEABI'#013#010+
  'else'#013#010+
  'ARCH=$(CPU_TARGET)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(FULL_TARGET),arm-embedded)'#013#010+
  'ifeq ($(S','UBARCH),)'#013#010+
  '# we don'#039't add a default here, people should explicitly add the SU'+
  'BARCH'#013#010+
  '$(error When compiling for arm-embedded, a sub-architecture (e.g. SUBA'+
  'RCH=armv4t or SUBARCH=armv7m) must be defined)'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Cp$(SUBARCH)'#013,#010+
  'endif'#013#010+
  #013#010+
  '# Full name of the target, including CPU and OS. For OSs limited'#013#010+
  '# to 8.3 we only use the target OS'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)'#013#010+
  'TARGETSUFFIX=$(OS_TARGET)'#013#010+
  'SOURCESUFFIX=$(OS_SOURCE)'#013#010+
  'else'#013#010+
  'ifneq ($(findstrin','g $(OS_TARGET),$(LIMIT83fs)),)'#013#010+
  'TARGETSUFFIX=$(OS_TARGET)'#013#010+
  'else'#013#010+
  'TARGETSUFFIX=$(FULL_TARGET)'#013#010+
  'endif'#013#010+
  'SOURCESUFFIX=$(FULL_SOURCE)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross compile flag'#013#010+
  'ifneq ($(FULL_TARGET),$(FULL_SOURCE))'#013#010+
  'CROSSCOMPILE=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if the ','Makefile supports this target, but not'#013#010+
  '# when the make target is to rebuild the makefile'#013#010+
  'ifeq ($(findstring makefile,$(MAKECMDGOALS)),)'#013#010+
  'ifeq ($(findstring $(FULL_TARGET),$(MAKEFILETARGETS)),)'#013#010+
  '$(error The Makefile doesn'#039't support target ','$(FULL_TARGET), plea'+
  'se run fpcmake first)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# Detect BSD, since BSD uses a slightly different directory hierarchy.'+
  #013#010+
  'ifneq ($(findstring $(OS_TARGET),$(BSDs)),)'#013#010+
  'BSDhier=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Detect Linux, will also use its own dire','ctory hierarchy.'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'linuxHier=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef CROSSCOMPILE'#013#010+
  'BUILDFULLNATIVE=1'#013#010+
  'export BUILDFULLNATIVE'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef BUILDFULLNATIVE'#013#010+
  'BUILDNATIVE=1'#013#010+
  'export BUILDNATIVE'#013#010+
  'endif'#013#010+
  #013#010+
  'export OS_TARGET OS_SOURCE ARCH C','PU_TARGET CPU_SOURCE FULL_TARGET FU'+
  'LL_SOURCE TARGETSUFFIX SOURCESUFFIX CROSSCOMPILE'#013#010+
  #013#010+
  '[fpmakefpcdetect]'#013#010+
  '#####################################################################'#013+
  #010+
  '# FPC Binary and Version Detection'#013#010+
  '###########################','#########################################'+
  '#'#013#010+
  '# If FPCFPMAKE is not set and we'#039're cross-compiling, try to find a'+
  #013#010+
  '# native compiler.'#013#010+
  'ifndef FPCFPMAKE'#013#010+
  #013#010+
  'ifdef CROSSCOMPILE'#013#010+
  '# Search for the (native) ppc compiler which is used to do the lates','t'+
  ' build'#013#010+
  '# of the native rtl'#013#010+
  #013#010+
  'ifeq ($(strip $(wildcard $(addsuffix /compiler/ppc$(SRCEXEEXT),$(FPCDI'+
  'R)))),)'#013#010+
  '# If that compiler is not found, use the common, installed native comp'+
  'iler'#013#010+
  '# Note that in this case FPCFPMAKE is not exported, b','ecause it could'+
  ' be that'#013#010+
  '# a more suitable compiler is compiled later on'#013#010+
  '# check if fpc exists'#013#010+
  'FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifneq ($(FPCPROG),)'#013#010+
  'FPCPROG:=$(firstword $(FPCPROG))'#013#010+
  'FPCFPMAKE:=$(','shell $(FPCPROG) -PB)'#013#010+
  '# if a native compiler is not found, fall back to fpc'#013#010+
  'ifeq ($(strip $(wildcard $(FPCFPMAKE))),)'#013#010+
  'FPCFPMAKE:=$(firstword $(FPCPROG))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# fpc binary not found, fallback to ppc386'#013#010+
  'override FPCFPMAKE=$(first','word $(strip $(wildcard $(addsuffix /ppc38'+
  '6$(SRCEXEEXT),$(SEARCHPATH)))))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# Use the ppc compiler. Because the path to the compiler is relative, '+
  'do not'#013#010+
  '# evaluate the variable here, but do that on the fly.'#013#010+
  'FPCFPMAKE=$(strip $','(wildcard $(addsuffix /compiler/ppc$(SRCEXEEXT),$'+
  '(FPCDIR))))'#013#010+
  'FPMAKE_SKIP_CONFIG=-n'#013#010+
  'export FPCFPMAKE'#013#010+
  'export FPMAKE_SKIP_CONFIG'#013#010+
  'endif'#013#010+
  #013#010+
  'else'#013#010+
  'FPMAKE_SKIP_CONFIG=-n'#013#010+
  'FPCFPMAKE=$(FPC)'#013#010+
  'endif'#013#010+
  #013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[fpcdircheckenv]'#013#010+
  '################','####################################################'+
  '#'#013#010+
  '# FPCDIR Setting'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Test FPCDIR to look if the RTL dir exists'#013#010+
  'ifdef FPCDIR'#013#010+
  'override FPCDIR:=$(subst \,/,$(FPCD','IR))'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default FPCDIR from Makefile.fpc'#013#010+
  'ifdef DEFAULT_FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'override FPCDIR:=$(subst \,/,','$(DEFAULT_FPCDIR))'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[fpcdirdetect]'#013#010+
  '# Detect FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'ifdef inUnix'#013#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION',')'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC),$(SEARCHPATH))))))'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013#010+
  'ifeq (','$(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR:=$(BASEDIR)'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR=c:/pp',#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross binaries dir'#013#010+
  'ifndef CROSSBINDIR'#013#010+
  'CROSSBINDIR:=$(wildcard $(FPCDIR)/bin/$(TARGETSUFFIX))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default binutils prefix for cross compile when the'#013#010+
  '# crossbindir is not set (except for Dar','win)'#013#010+
  'ifneq ($(findstring $(OS_TARGET),darwin iphonesim),)'#013#010+
  'ifeq ($(OS_SOURCE),darwin)'#013#010+
  'DARWIN2DARWIN=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef BINUTILSPREFIX'#013#010+
  'ifndef CROSSBINDIR'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'ifneq ($(OS_TARGET),msdos)'#013#010+
  'ifndef DARWIN2DARWIN'#013#010+
  'ifneq ','($(CPU_TARGET),jvm)'#013#010+
  'BINUTILSPREFIX=$(CPU_TARGET)-$(OS_TARGET)-'#013#010+
  #013#010+
  '# For Android use standard NDK prefixes'#013#010+
  'ifeq ($(OS_TARGET),android)'#013#010+
  'ifeq ($(CPU_TARGET),arm)'#013#010+
  'BINUTILSPREFIX=arm-linux-androideabi-'#013#010+
  'else'#013#010+
  'ifeq ($(CPU_TARGET),i386)'#013#010+
  'BINUT','ILSPREFIX=i686-linux-android-'#013#010+
  'else'#013#010+
  'ifeq ($(CPU_TARGET),mipsel)'#013#010+
  'BINUTILSPREFIX=mipsel-linux-android-'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'BINUTILSPREFIX=$(OS_TARGET)-'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Try first the full targe','t name, otherwise try only'#013#010+
  '# the OS for backwards compatibility'#013#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(TARGETSUFFIX))'#013#010+
  'ifeq ($(UNITSDIR),)'#013#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_TARGET))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Packages dir'#013#010+
  'PACKAGESDIR:=$(wildca','rd $(FPCDIR) $(FPCDIR)/packages $(FPCDIR)/packa'+
  'ges/base $(FPCDIR)/packages/extra)'#013#010+
  #013#010+
  #013#010+
  '[shelltools]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Shell tools'#013#010+
  '#####################################################','###############'+
  '#'#013#010+
  #013#010+
  '# Echo that can redir (must be able run in the default OS shell)'#013#010+
  'ifndef ECHOREDIR'#013#010+
  'ifndef inUnix'#013#010+
  'ECHOREDIR=echo'#013#010+
  'else'#013#010+
  'ECHOREDIR=$(ECHO)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# To copy pograms'#013#010+
  'ifndef COPY'#013#010+
  'COPY:=$(CPPROG) -fp'#013#010+
  'endif'#013#010+
  #013#010+
  '# ','Copy a whole tree'#013#010+
  'ifndef COPYTREE'#013#010+
  'COPYTREE:=$(CPPROG) -Rfp'#013#010+
  'endif'#013#010+
  #013#010+
  '# Copy a whole tree'#013#010+
  'ifndef MKDIRTREE'#013#010+
  'MKDIRTREE:=$(MKDIRPROG) -p'#013#010+
  'endif'#013#010+
  #013#010+
  '# To move pograms'#013#010+
  'ifndef MOVE'#013#010+
  'MOVE:=$(MVPROG) -f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check delete program'#013#010+
  'ifndef D','EL'#013#010+
  'DEL:=$(RMPROG) -f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check deltree program'#013#010+
  'ifndef DELTREE'#013#010+
  'DELTREE:=$(RMPROG) -rf'#013#010+
  'endif'#013#010+
  #013#010+
  '# To install files'#013#010+
  'ifndef INSTALL'#013#010+
  'ifdef inUnix'#013#010+
  'INSTALL:=$(GINSTALL) -c -m 644'#013#010+
  'else'#013#010+
  'INSTALL:=$(COPY)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# To install',' programs'#013#010+
  'ifndef INSTALLEXE'#013#010+
  'ifdef inUnix'#013#010+
  'INSTALLEXE:=$(GINSTALL) -c -m 755'#013#010+
  'else'#013#010+
  'INSTALLEXE:=$(COPY)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# To make a directory.'#013#010+
  'ifndef MKDIR'#013#010+
  'MKDIR:=$(GINSTALL) -m 755 -d'#013#010+
  'endif'#013#010+
  #013#010+
  'export ECHOREDIR COPY COPYTREE MOVE DEL D','ELTREE INSTALL INSTALLEXE M'+
  'KDIR'#013#010+
  #013#010+
  #013#010+
  '[defaultdirs]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Units dir'#013#010+
  'ifdef RE','QUIRE_UNITSDIR'#013#010+
  'override UNITSDIR+=$(REQUIRE_UNITSDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Units dir'#013#010+
  'ifdef REQUIRE_PACKAGESDIR'#013#010+
  'override PACKAGESDIR+=$(REQUIRE_PACKAGESDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# Unixes use unix dirs with /usr/bin, /usr/lib'#013#010+
  '# When zipping use the target ','os default, when normal install then'#013+
  #010+
  '# use the source os as default'#013#010+
  'ifdef ZIPINSTALL'#013#010+
  '# Zipinstall'#013#010+
  'ifneq ($(findstring $(OS_TARGET),$(UNIXs)),)'#013#010+
  'UNIXHier=1'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# Normal install'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(UNIXs)),)'#013#010+
  'UNI','XHier=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# When install prefix is not set try to use prefix'#013#010+
  'ifndef INSTALL_PREFIX'#013#010+
  'ifdef PREFIX'#013#010+
  'INSTALL_PREFIX=$(PREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the prefix directory where to install everything'#013#010+
  'ifndef INSTALL_PREFIX'#013#010+
  'ifdef',' UNIXHier'#013#010+
  'INSTALL_PREFIX=/usr/local'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_BASEDIR:=/pp'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export INSTALL_PREFIX'#013#010+
  #013#010+
  '# Export also INSTALL_SOURCESUBDIR set so it will be'#013#010+
  '# used ','recursively for all subdirs'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'export INSTALL_FPCSUBDIR'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to place the resulting zip files'#013#010+
  'ifndef DIST_DESTDIR'#013#010+
  'DIST_DESTDIR:=$(BASEDIR)'#013#010+
  'endif'#013#010+
  'export DIST_DESTDIR'#013#010+
  #013#010+
  '# EXE/PPU Target directories'#013#010+
  'if','ndef COMPILER_UNITTARGETDIR'#013#010+
  'ifdef PACKAGEDIR_MAIN'#013#010+
  'COMPILER_UNITTARGETDIR=$(PACKAGEDIR_MAIN)/units/$(TARGETSUFFIX)'#013#010+
  'else'#013#010+
  'COMPILER_UNITTARGETDIR=units/$(TARGETSUFFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifndef COMPILER_TARGETDIR'#013#010+
  'COMPILER_TARGETDIR=.'#013#010+
  'endif'#013#010+
  #013,#010+
  #013#010+
  '#####################################################################'#013+
  #010+
  '# Install Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# set the base directory where to install everything'#013#010+
  'ifndef INSTALL_B','ASEDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# se','t the directory where to install the binaries'#013#010+
  'ifndef INSTALL_BINDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_PREFIX)/bin'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BASEDIR)/bin'#013#010+
  '# for FPC packages install the binaries under their target subdir'#013#010+
  'if','def INSTALL_FPCPACKAGE'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'ifdef CROSSINSTALL'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(SOURCESUFFIX)'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(TARGETSUFFIX)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(TARGETSUFFIX)'#013#010+
  'endi','f'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the directory where to install the units.'#013#010+
  'ifndef INSTALL_UNITDIR'#013#010+
  'INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/units/$(TARGETSUFFIX)'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef PACKAGE_NAME'#013#010+
  'INSTALL_UNITDIR:=$(INSTALL_UNITDIR)/','$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to install shared libraries'#013#010+
  'ifndef INSTALL_LIBDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'INSTALL_LIBDIR:=$(INSTALL_PREFIX)/lib'#013#010+
  'else'#013#010+
  'INSTALL_LIBDIR:=$(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the source files wi','ll be stored'#013#010+
  'ifndef INSTALL_SOURCEDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'ifdef BSDhier'#013#010+
  'SRCPREFIXDIR=share/src'#013#010+
  'else'#013#010+
  'ifdef linuxHier'#013#010+
  'SRCPREFIXDIR=share/src'#013#010+
  'else'#013#010+
  'SRCPREFIXDIR=src'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'INSTALL_S','OURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSI'+
  'ON)/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PR','EFIX)/$(SRCPREFIXDIR)/$(PACKAGE_NAME)'+
  '-$(PACKAGE_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(INSTALL_FPCSUBDIR)/$(PA'+
  'CKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_','BASEDIR)/source/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the doc files will be stored'#013#010+
  'ifndef INSTALL_DOCDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'ifdef BSDhier'#013#010+
  'DOCPREFIXDIR=share/doc'#013#010+
  'else'#013#010+
  'ifd','ef linuxHier'#013#010+
  'DOCPREFIXDIR=share/doc'#013#010+
  'else'#013#010+
  'DOCPREFIXDIR=doc'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/fpc-$(FPC_VERSION)/$'+
  '(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXD','IR)/$(PACKAGE_NAME)-$('+
  'PACKAGE_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to install the examples, und','er linux we use the doc dir'#013#010+
  '# because the copytree command will create a subdir itself'#013#010+
  'ifndef INSTALL_EXAMPLEDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  #013#010+
  'ifdef BSDhier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/fpc-$(FPC_VE','RSI'+
  'ON)/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'ifdef linuxHier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_DOCDIR)/examples'#013#010+
  'else'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)/examples/'+
  '$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'else'#013#010+
  #013#010+
  'ifdef BSDhier'#013#010+
  'INSTALL_EXAMP','LEDIR:=$(INSTALL_PREFIX)/share/examples/$(PACKAGE_NAME)'+
  '-$(PACKAGE_VERSION)'#013#010+
  'else'#013#010+
  'ifdef linuxHier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_DOCDIR)/examples/$(PACKAGE_NAME)-$(PACKA'+
  'GE_VERSION)'#013#010+
  'else'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_','NAME)-$(PACKAGE_'+
  'VERSION)'#013#010+
  'endif'#013#010+
  #013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the some',' extra (data)files will be stored'#013#010+
  'ifndef INSTALL_DATADIR'#013#010+
  'INSTALL_DATADIR=$(INSTALL_BASEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef INSTALL_SHAREDDIR'#013#010+
  'INSTALL_SHAREDDIR=$(INSTALL_PREFIX)/lib'#013#010+
  'endif'#013#010+
  #013#010+
  '############################################################','########'+
  '#'#013#010+
  '# Cross compile dirs'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  'ifdef CROSSCOMPILE'#013#010+
  '# Directory where the cross compile tools are stored.'#013#010+
  '# First check if they are available in FPCDIR. If no target','s/ subdir'+
  #013#010+
  '# is found use the targets/ subdir in INSTALL_BASEDIR.'#013#010+
  'ifndef CROSSBINDIR'#013#010+
  'CROSSBINDIR:=$(wildcard $(CROSSTARGETDIR)/bin/$(SOURCESUFFIX))'#013#010+
  'ifeq ($(CROSSBINDIR),)'#013#010+
  'CROSSBINDIR:=$(wildcard $(INSTALL_BASEDIR)/cross/$(TARGETSUFFIX)/','bin'+
  '/$(FULL_SOURCE))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'CROSSBINDIR='#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[dirlibc]'#013#010+
  '# On linux, try to find where libgcc.a is.'#013#010+
  'ifeq ($(OS_SOURCE),linux)'#013#010+
  #013#010+
  '# Amd64 to i386?'#013#010+
  'ifndef GCCLIBDIR'#013#010+
  'ifeq ($(CPU_TARGET),i386)'#013#010+
  'ifneq ($(findstring x86_64,','$(shell uname -a)),)'#013#010+
  'ifeq ($(BINUTILSPREFIX),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m32 -print-libgcc-file-name`)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifeq ($(CPU_TARGET),powerpc64)'#013#010+
  'ifeq ($(BINUTILSPREFIX),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m64 -print-lib','gcc-file-name`)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Try cross gcc'#013#010+
  'ifndef GCCLIBDIR'#013#010+
  'CROSSGCC=$(strip $(wildcard $(addsuffix /$(BINUTILSPREFIX)gcc$(SRCEXEE'+
  'XT),$(SEARCHPATH))))'#013#010+
  'ifneq ($(CROSSGCC),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `$(CROSSGCC) -print-libg','cc-file-name`)'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Other libraries from ld.so.conf'#013#010+
  'ifndef OTHERLIBDIR'#013#010+
  'OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | awk '#039'{ ORS=" '+
  '"; print $1 }'#039')'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef inUnix'#013#010+
  'ifeq ($(OS_SOURCE),netbsd)'#013#010+
  'OTHERLIBDIR+','=/usr/pkg/lib'#013#010+
  'endif'#013#010+
  'export GCCLIBDIR OTHERLIB'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[extensions]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default extensions'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010,
  '# Default needed extensions (Go32v2,Linux)'#013#010+
  'BATCHEXT=.bat'#013#010+
  'LOADEREXT=.as'#013#010+
  'EXEEXT=.exe'#013#010+
  'PPLEXT=.ppl'#013#010+
  'PPUEXT=.ppu'#013#010+
  'OEXT=.o'#013#010+
  'ASMEXT=.s'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=.so'#013#010+
  'SHAREDLIBPREFIX=libfp'#013#010+
  'STATICLIBPREFIX=libp'#013#010+
  'IMPORTLIBPREF','IX=libimp'#013#010+
  'RSTEXT=.rst'#013#010+
  '# external debug info for executable'#013#010+
  'EXEDBGEXT=.dbg'#013#010+
  '#DEBUGSYMEXT #for debugger symbol files, define only for targets which'+
  ' has this'#013#010+
  #013#010+
  '# Go32v1'#013#010+
  'ifeq ($(OS_TARGET),go32v1)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHORTSUFFIX=v1'#013#010+
  'endif'#013#010,
  #013#010+
  '# Go32v2'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHORTSUFFIX=dos'#013#010+
  'IMPORTLIBPREFIX='#013#010+
  'endif'#013#010+
  #013#010+
  '# watcom'#013#010+
  'ifeq ($(OS_TARGET),watcom)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'OEXT=.obj'#013#010+
  'ASMEXT=.asm'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=wat'#013#010+
  'IMPORTLIBPREFIX='#013#010+
  'endi','f'#013#010+
  #013#010+
  '# Android native'#013#010+
  'ifneq ($(CPU_TARGET),jvm)'#013#010+
  'ifeq ($(OS_TARGET),android)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=lnx'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Linux'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=ln','x'#013#010+
  'endif'#013#010+
  #013#010+
  '# FreeBSD'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=fbs'#013#010+
  'endif'#013#010+
  #013#010+
  '# NetBSD'#013#010+
  'ifeq ($(OS_TARGET),netbsd)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=nbs'#013#010+
  'endif'#013#010+
  #013#010+
  '# OpenBSD'#013#010+
  'ifeq ($(O','S_TARGET),openbsd)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=obs'#013#010+
  'endif'#013#010+
  #013#010+
  '# Win32'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=w32'#013#010+
  'endif'#013#010+
  #013#010+
  '# OS/2'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'BATCHEXT=.cmd'#013#010+
  'AOUTEXT=.out'#013#010+
  'STATICLIBPRE','FIX='#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=os2'#013#010+
  'ECHO=echo'#013#010+
  'IMPORTLIBPREFIX='#013#010+
  'endif'#013#010+
  #013#010+
  '# EMX'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'BATCHEXT=.cmd'#013#010+
  'AOUTEXT=.out'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=emx'#013#010+
  'ECHO=echo'#013#010+
  'IMPORTLIBPREFIX='#013#010+
  'endif'#013#010+
  #013#010+
  '# Amiga',#013#010+
  'ifeq ($(OS_TARGET),amiga)'#013#010+
  'EXEEXT='#013#010+
  'SHAREDLIBEXT=.library'#013#010+
  'SHORTSUFFIX=amg'#013#010+
  'endif'#013#010+
  #013#010+
  '# MorphOS'#013#010+
  'ifeq ($(OS_TARGET),morphos)'#013#010+
  'EXEEXT='#013#010+
  'SHAREDLIBEXT=.library'#013#010+
  'SHORTSUFFIX=mos'#013#010+
  'endif'#013#010+
  #013#010+
  '# Atari'#013#010+
  'ifeq ($(OS_TARGET),atari)'#013#010+
  'EXEEXT=.ttp'#013#010+
  'SHORTS','UFFIX=ata'#013#010+
  'endif'#013#010+
  #013#010+
  '# BeOS'#013#010+
  'ifeq ($(OS_TARGET),beos)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'SHORTSUFFIX=be'#013#010+
  'endif'#013#010+
  #013#010+
  '# Haiku'#013#010+
  'ifeq ($(OS_TARGET),haiku)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'SHORTSUFFIX=hai'#013#010+
  'endif'#013#010+
  #013#010+
  '# Solaris'#013#010+
  'ifeq ($(OS_TARGET),solaris)'#013#010+
  'BATCHEXT=.sh'#013,#010+
  'EXEEXT='#013#010+
  'SHORTSUFFIX=sun'#013#010+
  'endif'#013#010+
  #013#010+
  '# QNX'#013#010+
  'ifeq ($(OS_TARGET),qnx)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'SHORTSUFFIX=qnx'#013#010+
  'endif'#013#010+
  #013#010+
  '# Netware clib'#013#010+
  'ifeq ($(OS_TARGET),netware)'#013#010+
  'EXEEXT=.nlm'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHORTSUFFIX=nw'#013#010+
  'IMPORTLIBPREFIX=imp'#013#010+
  'endif'#013#010+
  #013#010+
  '#',' Netware libc'#013#010+
  'ifeq ($(OS_TARGET),netwlibc)'#013#010+
  'EXEEXT=.nlm'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHORTSUFFIX=nwl'#013#010+
  'IMPORTLIBPREFIX=imp'#013#010+
  'endif'#013#010+
  #013#010+
  '# MacOS'#013#010+
  'ifeq ($(OS_TARGET),macos)'#013#010+
  'BATCHEXT='#013#010+
  'EXEEXT='#013#010+
  'DEBUGSYMEXT=.xcoff'#013#010+
  'SHORTSUFFIX=mac'#013#010+
  'IMPORTLIBPREFIX=imp'#013#010+
  'end','if'#013#010+
  #013#010+
  '# Darwin'#013#010+
  'ifneq ($(findstring $(OS_TARGET),darwin iphonesim),)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=dwn'#013#010+
  'EXEDBGEXT=.dSYM'#013#010+
  'endif'#013#010+
  #013#010+
  '# gba'#013#010+
  'ifeq ($(OS_TARGET),gba)'#013#010+
  'EXEEXT=.gba'#013#010+
  'SHAREDLIBEXT=.so'#013#010+
  'SHORTSUFFIX=gba'#013#010+
  'endif'#013#010+
  #013,#010+
  '# Symbian OS'#013#010+
  'ifeq ($(OS_TARGET),symbian)'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=symbian'#013#010+
  'endif'#013#010+
  #013#010+
  '# NativeNT'#013#010+
  'ifeq ($(OS_TARGET),NativeNT)'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=nativent'#013#010+
  'endif'#013#010+
  #013#010+
  '# wii'#013#010+
  'ifeq ($(OS_TARGET),wii)'#013#010+
  'EXEEXT=.dol'#013#010+
  'SHAREDLIBE','XT=.so'#013#010+
  'SHORTSUFFIX=wii'#013#010+
  'endif'#013#010+
  #013#010+
  '# AIX'#013#010+
  'ifeq ($(OS_TARGET),aix)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'SHORTSUFFIX=aix'#013#010+
  'endif'#013#010+
  #013#010+
  '# Java'#013#010+
  'ifeq ($(OS_TARGET),java)'#013#010+
  'OEXT=.class'#013#010+
  'ASMEXT=.j'#013#010+
  'SHAREDLIBEXT=.jar'#013#010+
  'SHORTSUFFIX=java'#013#010+
  'endif'#013#010+
  #013#010+
  '# Android java'#013#010+
  'ifeq ','($(CPU_TARGET),jvm)'#013#010+
  'ifeq ($(OS_TARGET),android)'#013#010+
  'OEXT=.class'#013#010+
  'ASMEXT=.j'#013#010+
  'SHAREDLIBEXT=.jar'#013#010+
  'SHORTSUFFIX=android'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# MS-DOS'#013#010+
  'ifeq ($(OS_TARGET),msdos)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHORTSUFFIX=d16'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  #013#010+
  '# For 8.','3 limited OS'#039's the short suffixes'#013#010+
  '# Otherwise use the full source/target names'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)'#013#010+
  'FPCMADE=fpcmade.$(SHORTSUFFIX)'#013#010+
  'ZIPSUFFIX=$(SHORTSUFFIX)'#013#010+
  'ZIPCROSSPREFIX='#013#010+
  'ZIPSOURCESUFFIX=src'#013#010+
  'ZIPEXAMPLESUF','FIX=exm'#013#010+
  'else'#013#010+
  'FPCMADE=fpcmade.$(TARGETSUFFIX)'#013#010+
  'ZIPSOURCESUFFIX=.source'#013#010+
  'ZIPEXAMPLESUFFIX=.examples'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'ZIPSUFFIX=.$(SOURCESUFFIX)'#013#010+
  'ZIPCROSSPREFIX=$(TARGETSUFFIX)-'#013#010+
  'else'#013#010+
  'ZIPSUFFIX=.$(TARGETSUFFIX)'#013#010+
  'ZIPCROSSPREFIX='#013#010+
  'endif'#013#010,
  'endif'#013#010+
  #013#010+
  '[defaulttools]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default Tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Names of the binutils tools'#013#010+
  'ASNAME=$(BINUTILSPREFIX',')as'#013#010+
  'LDNAME=$(BINUTILSPREFIX)ld'#013#010+
  'ARNAME=$(BINUTILSPREFIX)ar'#013#010+
  'RCNAME=$(BINUTILSPREFIX)rc'#013#010+
  'NASMNAME=$(BINUTILSPREFIX)nasm'#013#010+
  #013#010+
  '# assembler, redefine it if cross compiling'#013#010+
  'ifndef ASPROG'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'ASPROG=$(CROSSBINDIR)/$(ASNAME)$(SRCEX','EEXT)'#013#010+
  'else'#013#010+
  'ASPROG=$(ASNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# linker, but probably not used'#013#010+
  'ifndef LDPROG'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'LDPROG=$(CROSSBINDIR)/$(LDNAME)$(SRCEXEEXT)'#013#010+
  'else'#013#010+
  'LDPROG=$(LDNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Resource compiler'#013#010+
  'ifndef RCPROG'#013#010+
  'ifdef',' CROSSBINDIR'#013#010+
  'RCPROG=$(CROSSBINDIR)/$(RCNAME)$(SRCEXEEXT)'#013#010+
  'else'#013#010+
  'RCPROG=$(RCNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Archiver'#013#010+
  'ifndef ARPROG'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'ARPROG=$(CROSSBINDIR)/$(ARNAME)$(SRCEXEEXT)'#013#010+
  'else'#013#010+
  'ARPROG=$(ARNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# NASM ass','embler, redefine it if cross compiling'#013#010+
  'ifndef NASMPROG'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'NASMPROG=$(CROSSBINDIR)/$(NASMNAME)$(SRCEXEEXT)'#013#010+
  'else'#013#010+
  'NASMPROG=$(NASMNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Override defaults'#013#010+
  'AS=$(ASPROG)'#013#010+
  'LD=$(LDPROG)'#013#010+
  'RC=$(RCPROG)'#013#010+
  'AR=$(ARP','ROG)'#013#010+
  'NASM=$(NASMPROG)'#013#010+
  #013#010+
  '# ppas.bat / ppas.sh'#013#010+
  'ifdef inUnix'#013#010+
  'PPAS=./ppas$(SRCBATCHEXT)'#013#010+
  'else'#013#010+
  'PPAS=ppas$(SRCBATCHEXT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# ldconfig to rebuild .so cache'#013#010+
  'ifdef inUnix'#013#010+
  'LDCONFIG=ldconfig'#013#010+
  'else'#013#010+
  'LDCONFIG='#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef DATE'#013#010+
  'DATESTR',':=$(shell $(DATE) +%Y%m%d)'#013#010+
  'else'#013#010+
  'DATESTR='#013#010+
  'endif'#013#010+
  #013#010+
  '# Zip options'#013#010+
  'ZIPOPT=-9'#013#010+
  'ZIPEXT=.zip'#013#010+
  #013#010+
  '# Tar options'#013#010+
  'ifeq ($(USETAR),bz2)'#013#010+
  'TAROPT=vj'#013#010+
  'TAREXT=.tar.bz2'#013#010+
  'else'#013#010+
  'TAROPT=vz'#013#010+
  'TAREXT=.tar.gz'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_begin]'#013#010+
  '####################','################################################'+
  '#'#013#010+
  '# Compiler Command Line'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Load commandline OPTDEF and add FPC_CPU define, for compiling the'#013+
  #010+
  '# compiler this needs ','to be turned off'#013#010+
  'ifndef NOCPUDEF'#013#010+
  'override FPCOPTDEF=$(ARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# Load commandline OPT and add target and unit dir to be sure'#013#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#013#010+
  'override FPCOPT+=-T$(OS_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifneq ($(CPU_TARGET),$(CPU','_SOURCE))'#013#010+
  'override FPCOPT+=-P$(ARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(OS_SOURCE),openbsd)'#013#010+
  'override FPCOPT+=-FD$(NEW_BINUTILS_PATH)'#013#010+
  'override FPCMAKEOPT+=-FD$(NEW_BINUTILS_PATH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef CROSSBOOTSTRAP'#013#010+
  'ifneq ($(BINUTILSPREFIX),)'#013#010+
  'override FPCOPT','+=-XP$(BINUTILSPREFIX)'#013#010+
  'endif'#013#010+
  'ifneq ($(BINUTILSPREFIX),)'#013#010+
  'override FPCOPT+=-Xr$(RLINKPATH)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# When BINUTILSPREFIX is given and we are not cross-compiling then use'+
  #013#010+
  '# it while compiling the fpmake file. (For example to build',' i386-fre'+
  'ebsd'#013#010+
  '# with BINUTILSPREFIX=i386-)'#013#010+
  'ifndef CROSSCOMPILE'#013#010+
  'ifneq ($(BINUTILSPREFIX),)'#013#010+
  'override FPCMAKEOPT+=-XP$(BINUTILSPREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# User dirs should be first, so they are looked at first'#013#010+
  'ifdef UNITDIR'#013#010+
  'override FPCOP','T+=$(addprefix -Fu,$(UNITDIR))'#013#010+
  'endif'#013#010+
  'ifdef LIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#013#010+
  'endif'#013#010+
  'ifdef OBJDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDIR))'#013#010+
  'endif'#013#010+
  'ifdef INCDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fi,$(INCDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  '#',' Smartlinking'#013#010+
  'ifdef LINKSMART'#013#010+
  'override FPCOPT+=-XX'#013#010+
  'endif'#013#010+
  #013#010+
  '# Smartlinking creation'#013#010+
  'ifdef CREATESMART'#013#010+
  'override FPCOPT+=-CX'#013#010+
  'endif'#013#010+
  #013#010+
  '# Debug'#013#010+
  'ifdef DEBUG'#013#010+
  'override FPCOPT+=-gl'#013#010+
  'override FPCOPTDEF+=DEBUG'#013#010+
  'endif'#013#010+
  #013#010+
  '# Release mode'#013#010+
  '# (str','ip, optimize and don'#039't load fpc.cfg)'#013#010+
  '# fpc 2.1 has -O2 for all targets'#013#010+
  'ifdef RELEASE'#013#010+
  'ifneq ($(findstring 2.0.,$(FPC_VERSION)),)'#013#010+
  'ifeq ($(CPU_TARGET),i386)'#013#010+
  'FPCCPUOPT:=-OG2p3'#013#010+
  'endif'#013#010+
  'ifeq ($(CPU_TARGET),powerpc)'#013#010+
  'FPCCPUOPT:=-O1r'#013#010+
  'endif'#013#010+
  'e','lse'#013#010+
  'FPCCPUOPT:=-O2'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Ur -Xs $(FPCCPUOPT) -n'#013#010+
  'override FPCOPTDEF+=RELEASE'#013#010+
  'endif'#013#010+
  #013#010+
  '# Strip'#013#010+
  'ifdef STRIP'#013#010+
  'override FPCOPT+=-Xs'#013#010+
  'endif'#013#010+
  #013#010+
  '# Optimizer'#013#010+
  'ifdef OPTIMIZE'#013#010+
  'override FPCOPT+=-O2'#013#010+
  'endif'#013#010+
  #013#010+
  '# Verbose settings ','(warning,note,info)'#013#010+
  'ifdef VERBOSE'#013#010+
  'override FPCOPT+=-vwni'#013#010+
  'endif'#013#010+
  #013#010+
  '# Needed compiler options'#013#010+
  'ifdef COMPILER_OPTIONS'#013#010+
  'override FPCOPT+=$(COMPILER_OPTIONS)'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_UNITDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fu,$(COMPILER_UNITDIR','))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_LIBRARYDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARYDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_OBJECTDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_INCLUDEDIR'#013#010+
  'override FPCOPT+=$(a','ddprefix -Fi,$(COMPILER_INCLUDEDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross compiler utils'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'override FPCOPT+=-FD$(CROSSBINDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Target dirs and the prefix to use for clean/install'#013#010+
  'ifdef COMPILER_TARGETDIR'#013#010+
  'override FPCOPT+=-FE$(CO','MPILER_TARGETDIR)'#013#010+
  'ifeq ($(COMPILER_TARGETDIR),.)'#013#010+
  'override TARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override TARGETDIRPREFIX=$(COMPILER_TARGETDIR)/'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Unit target dir (default is units/<cpu>-<os>/'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  'override FPC','OPT+=-FU$(COMPILER_UNITTARGETDIR)'#013#010+
  'ifeq ($(COMPILER_UNITTARGETDIR),.)'#013#010+
  'override UNITTARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override UNITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef COMPILER_TARGETDIR'#013#010+
  'override COMPILER_UNITTARGETDIR=$(CO','MPILER_TARGETDIR)'#013#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CREATESHARED'#013#010+
  'override FPCOPT+=-Cg'#013#010+
  'endif'#013#010+
  #013#010+
  '# create always pic'#039'ed code on x86_64'#013#010+
  'ifneq ($(findstring $(OS_TARGET),freebsd openbsd netbsd linux sola','ri'+
  's),)'#013#010+
  'ifeq ($(CPU_TARGET),x86_64)'#013#010+
  'override FPCOPT+=-Cg'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef LINKSHARED'#013#010+
  '#!!! deactived for testing override FPCOPT+=-XD'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_libc]'#013#010+
  '# Add GCC lib path if asked'#013#010+
  'ifdef GCCLIBDIR'#013#010+
  'override FPCOPT+=-Fl$(GCC','LIBDIR)'#013#010+
  'endif'#013#010+
  'ifdef OTHERLIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_end]'#013#010+
  '# Add commandline options last so they can override'#013#010+
  'ifdef OPT'#013#010+
  'override FPCOPT+=$(OPT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Add defines from FPCOPTDEF to',' FPCOPT'#013#010+
  'ifdef FPCOPTDEF'#013#010+
  'override FPCOPT+=$(addprefix -d,$(FPCOPTDEF))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Was a config file specified ?'#013#010+
  'ifdef CFGFILE'#013#010+
  'override FPCOPT+=@$(CFGFILE)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Use the environment to pass commandline options?'#013#010+
  'ifdef USEENV'#013#010+
  'overr','ide FPCEXTCMD:=$(FPCOPT)'#013#010+
  'override FPCOPT:=!FPCEXTCMD'#013#010+
  'export FPCEXTCMD'#013#010+
  'endif'#013#010+
  #013#010+
  'override AFULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)'#013#010+
  'override AFULL_SOURCE=$(CPU_SOURCE)-$(OS_SOURCE)'#013#010+
  #013#010+
  'ifneq ($(AFULL_TARGET),$(AFULL_SOURCE))'#013#010+
  'override ACROSS','COMPILE=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef ACROSSCOMPILE'#013#010+
  'override FPCOPT+=$(CROSSOPT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Compiler commandline'#013#010+
  'override COMPILER:=$(strip $(FPC) $(FPCOPT))'#013#010+
  #013#010+
  '# also call ppas if with command option -s'#013#010+
  '# but only if the FULL_SOURCE and FULL_TARGE','T are equal'#013#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#013#010+
  'EXECPPAS='#013#010+
  'else'#013#010+
  'ifeq ($(FULL_SOURCE),$(FULL_TARGET))'#013#010+
  'ifdef RUNBATCH'#013#010+
  'EXECPPAS:=@$(RUNBATCH) $(PPAS)'#013#010+
  'else'#013#010+
  'EXECPPAS:=@$(PPAS)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[loaderrules]'#013#010+
  '###################','#################################################'+
  '#'#013#010+
  '# Loaders'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_loaders'#013#010+
  #013#010+
  'ifneq ($(TARGET_LOADERS),)'#013#010+
  'override ALLTARGET+=fpc_loaders'#013#010+
  'override CLEANTARGET','+=fpc_loaders_clean'#013#010+
  'override INSTALLTARGET+=fpc_loaders_install'#013#010+
  #013#010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(TARGET_LOADERS))'#013#010+
  'endif'#013#010+
  #013#010+
  '%$(OEXT): %$(LOADEREXT)'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  '        $(AS) -o $(COMPILER_UNITTARGETDIR)/$*','$(OEXT) $<'#013#010+
  'else'#013#010+
  '        $(AS) -o $*$(OEXT) $<'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_loaders: $(COMPILER_UNITTARGETDIR) $(LOADEROFILES)'#013#010+
  #013#010+
  'fpc_loaders_clean:'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  '        -$(DEL) $(addprefix $(COMPILER_UNITTARGETDIR)/,$(LOADEROFILES)'+
  ')'#013#010+
  'e','lse'#013#010+
  '        -$(DEL) $(LOADEROFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_loaders_install:'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  '        $(INSTALL) $(addprefix $(COMPILER_UNITTARGETDIR)/,$(LOADEROFIL'+
  'ES)) $(INSTALL_UNITDIR)'#013#010+
  'else'#013#010+
  '   ','     $(INSTALL) $(LOADEROFILES) $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[unitrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Units'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc','_units'#013#010+
  #013#010+
  'ifneq ($(TARGET_UNITS)$(TARGET_IMPLICITUNITS),)'#013#010+
  'override ALLTARGET+=fpc_units'#013#010+
  #013#010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))'#013#010+
  'override IMPLICITUNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_IMPLICITU'+
  'NITS))'#013#010+
  'override',' INSTALLPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#013+
  #010+
  'override CLEANPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_units: $(COMPILER_UNITTARGETDIR) $(UNITPPUFILES)'#013#010+
  #013#010+
  #013#010+
  '[exerules]'#013#010+
  '#####################################','###############################'+
  '#'#013#010+
  '# Exes'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_exes'#013#010+
  #013#010+
  '# Programs are not needed for a cross installation'#013#010+
  'ifndef CROSSINSTALL'#013#010+
  'ifneq ($(TARGET_PROGRAMS),)'#013#010+
  'ove','rride EXEFILES=$(addsuffix $(EXEEXT),$(TARGET_PROGRAMS))'#013#010+
  'override EXEOFILES:=$(addsuffix $(OEXT),$(TARGET_PROGRAMS)) $(addprefi'+
  'x $(STATICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_PROGRAMS))) '+
  '$(addprefix $(IMPORTLIBPREFIX),$(addsuffix',' $(STATICLIBEXT),$(TARGET_'+
  'PROGRAMS)))'#013#010+
  'override EXEDBGFILES:=$(addsuffix $(EXEDBGEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  #013#010+
  'override ALLTARGET+=fpc_exes'#013#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#013#010+
  'override CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#013#010+
  'override CLE','ANEXEDBGFILES+=$(EXEDBGFILES)'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'en','dif'#013#010+
  #013#010+
  'fpc_exes: $(COMPILER_TARGETDIR) $(COMPILER_UNITTARGETDIR) $(EXEFILES)'#013+
  #010+
  #013#010+
  #013#010+
  '[rstrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Resource strings'#013#010+
  '#######################################################','#############'+
  '#'#013#010+
  #013#010+
  'ifdef TARGET_RSTS'#013#010+
  'override RSTFILES=$(addsuffix $(RSTEXT),$(TARGET_RSTS))'#013#010+
  #013#010+
  'override CLEANRSTFILES+=$(RSTFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[examplerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Example','s'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_examples'#013#010+
  #013#010+
  'ifneq ($(TARGET_EXAMPLES),)'#013#010+
  'HASEXAMPLES=1'#013#010+
  'override EXAMPLESOURCEFILES:=$(wildcard $(addsuffix .pp,$(TARGET_EXAMP'+
  'LES)) $(addsuffix .pas,$(T','ARGET_EXAMPLES)) $(addsuffix .lpr,$(TARGET'+
  '_EXAMPLES)) $(addsuffix .dpr,$(TARGET_EXAMPLES)))'#013#010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),$(TARGET_EXAMPLES))'#013#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(TARGET_EXAMPLES)) $(addp'+
  'refix $(STA','TICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_EXAMPL'+
  'ES))) $(addprefix $(IMPORTLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TAR'+
  'GET_EXAMPLES)))'#013#010+
  'override EXAMPLEDBGFILES:=$(addsuffix $(EXEDBGEXT),$(TARGET_EXAMPLES))'+
  #013#010+
  #013#010+
  'override CLEANEXEFILES','+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#013#010+
  'override CLEANEXEDBGFILES+=$(EXAMPLEDBGFILES)'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#013+
  #010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'override CLEANEXEFILES+=$(addsuf','fix $(AOUTEXT),$(TARGET_EXAMPLES))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'ifneq ($(TARGET_EXAMPLEDIRS),)'#013#010+
  'HASEXAMPLES=1'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_examples: all $(EXAMPLEFILES) $(addsuffix _all,$(TARGET_EXAMPLEDIR'+
  'S))'#013#010+
  #013#010+
  #013#010+
  '[compilerules]'#013#010+
  '#######################################','#############################'+
  '#'#013#010+
  '# General compile rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_all fpc_smart fpc_debug fpc_release fpc_shared'#013#010+
  #013#010+
  '$(FPCMADE): $(ALLDEPENDENCIES) $(ALLTARGET)'#013#010+
  '   ','     @$(ECHOREDIR) Compiled > $(FPCMADE)'#013#010+
  #013#010+
  'fpc_all: $(FPCMADE)'#013#010+
  #013#010+
  'fpc_smart:'#013#010+
  '        $(MAKE) all LINKSMART=1 CREATESMART=1'#013#010+
  #013#010+
  'fpc_debug:'#013#010+
  '        $(MAKE) all DEBUG=1'#013#010+
  #013#010+
  'fpc_release:'#013#010+
  '        $(MAKE) all RELEASE=1'#013#010+
  #013#010+
  '# General compile rule','s, available for both possible .pp and .pas ex'+
  'tensions'#013#010+
  #013#010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .lpr .dpr .pp .rc .res'#013+
  #010+
  #013#010+
  '$(COMPILER_UNITTARGETDIR):'#013#010+
  '        $(MKDIRTREE) $(COMPILER_UNITTARGETDIR)'#013#010+
  #013#010+
  '$(COMPILER_TARGETDIR):'#013#010+
  '        $(','MKDIRTREE) $(COMPILER_TARGETDIR)'#013#010+
  #013#010+
  '%$(PPUEXT): %.pp'#013#010+
  '        $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(PPUEXT): %.pas'#013#010+
  '        $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEXT): %.pp'#013#010+
  '        $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEX','T): %.pas'#013#010+
  '        $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEXT): %.lpr'#013#010+
  '        $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEXT): %.dpr'#013#010+
  '        $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%.res: %.rc'#013#010+
  '        windres -i $< -o $@'#013#010+
  #013#010+
  '# Sear','ch paths for .ppu, .pp, .pas, .lpr, .dpr'#013#010+
  'vpath %.pp $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.lpr $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.dpr $(COMPILER_SO','URCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.inc $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %$(OEXT) $(COMPILER_UNITTARGETDIR)'#013#010+
  'vpath %$(PPUEXT) $(COMPILER_UNITTARGETDIR)'#013#010+
  #013#010+
  '[sharedrules]'#013#010+
  '####################################################################','#'+
  #013#010+
  '# Library'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_shared'#013#010+
  #013#010+
  'override INSTALLTARGET+=fpc_shared_install'#013#010+
  #013#010+
  'ifndef SHARED_LIBVERSION'#013#010+
  'SHARED_LIBVERSION=$(FPC_VERSION)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef SHARED_LI','BNAME'#013#010+
  'SHARED_LIBNAME=$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef SHARED_FULLNAME'#013#010+
  'SHARED_FULLNAME=$(SHAREDLIBPREFIX)$(SHARED_LIBNAME)-$(SHARED_LIBVERSIO'+
  'N)$(SHAREDLIBEXT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default sharedlib units are all unit objects'#013#010+
  'ifndef SHARED_LIBUNITS',#013#010+
  'SHARED_LIBUNITS:=$(TARGET_UNITS) $(TARGET_IMPLICITUNITS)'#013#010+
  'override SHARED_LIBUNITS:=$(filter-out $(INSTALL_BUILDUNIT),$(SHARED_L'+
  'IBUNITS))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_shared:'#013#010+
  'ifdef HASSHAREDLIB'#013#010+
  '        $(MAKE) all CREATESHARED=1 LINKSHARED=1 CREATESMA','RT=1'#013#010+
  'ifneq ($(SHARED_BUILD),n)'#013#010+
  '        $(PPUMOVE) -q $(SHARED_LIBUNITS) -i$(COMPILER_UNITTARGETDIR) -'+
  'o$(SHARED_FULLNAME) -d$(COMPILER_UNITTARGETDIR)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '        @$(ECHO) Shared Libraries not supported'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_shared_instal','l:'#013#010+
  'ifneq ($(SHARED_BUILD),n)'#013#010+
  'ifneq ($(SHARED_LIBUNITS),)'#013#010+
  'ifneq ($(wildcard $(COMPILER_UNITTARGETDIR)/$(SHARED_FULLNAME)),)'#013#010+
  '        $(INSTALL) $(COMPILER_UNITTARGETDIR)/$(SHARED_FULLNAME) $(INST'+
  'ALL_SHAREDDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[ins','tallrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Install rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_install fpc_sourceinstall fpc_exampleinstall'#013#010+
  #013#010+
  'ifdef I','NSTALL_UNITS'#013#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(INSTALL_UNITS))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALL_BUILDUNIT'#013#010+
  'override INSTALLPPUFILES:=$(filter-out $(INSTALL_BUILDUNIT)$(PPUEXT),$'+
  '(INSTALLPPUFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  'ove','rride INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPU'+
  'FILES)) $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEX'+
  'T),$(INSTALLPPUFILES))) $(addprefix $(IMPORTLIBPREFIX),$(subst $(PPUEX'+
  'T),$(STATICLIBEXT),$(INSTALLPPUFILE','S)))'#013#010+
  'ifneq ($(UNITTARGETDIRPREFIX),)'#013#010+
  'override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(notdir '+
  '$(INSTALLPPUFILES)))'#013#010+
  'override INSTALLPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPR'+
  'EFIX),$(notdir $(INSTALLPPULINKFILES)))',')'#013#010+
  'endif'#013#010+
  '# Implicitly install Package.fpc'#013#010+
  'override INSTALL_CREATEPACKAGEFPC=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  'ifneq ($(TARGETDIRPREFIX),)'#013#010+
  'override INSTALLEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(notdir $(IN'+
  'STALLEXEFILES)))'#013#010+
  'endif'#013#010+
  'end','if'#013#010+
  #013#010+
  'fpc_install: all $(INSTALLTARGET)'#013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  '        $(MKDIR) $(INSTALL_BINDIR)'#013#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_CREATEPACKAGEFPC'#013#010+
  'ifdef FPCMAKE'#013#010+
  '# If the fpcpackage variab','le is set then create and install Package.'+
  'fpc,'#013#010+
  '# a safety check is done if Makefile.fpc is available'#013#010+
  'ifdef PACKAGE_VERSION'#013#010+
  'ifneq ($(wildcard Makefile.fpc),)'#013#010+
  '        $(FPCMAKE) -p -T$(CPU_TARGET)-$(OS_TARGET) Makefile.fpc'#013#010+
  '        $(MKDI','R) $(INSTALL_UNITDIR)'#013#010+
  '        $(INSTALL) Package.fpc $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(INSTALL_UNITDIR)'#013#010+
  'ifneq ($(INSTALLPPUL','INKFILES),)'#013#010+
  '        $(INSTALL) $(INSTALLPPULINKFILES) $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'ifneq ($(wildcard $(LIB_FULLNAME)),)'#013#010+
  '        $(MKDIR) $(INSTALL_LIBDIR)'#013#010+
  '        $(INSTALL) $(LIB_FULLNAME) $(INSTALL_LIBDIR)'#013#010+
  'ifdef inUnix'#013#010+
  '        ln -sf $','(LIB_FULLNAME) $(INSTALL_LIBDIR)/$(LIB_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FILES'#013#010+
  '        $(MKDIR) $(INSTALL_DATADIR)'#013#010+
  '        $(INSTALL) $(INSTALL_FILES) $(INSTALL_DATADIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_sourceinstall: distclean'#013#010+
  '        $(MKDIR) $(I','NSTALL_SOURCEDIR)'#013#010+
  '        $(COPYTREE) $(BASEDIR)/* $(INSTALL_SOURCEDIR)'#013#010+
  #013#010+
  'fpc_exampleinstall: $(addsuffix _distclean,$(TARGET_EXAMPLEDIRS))'#013#010+
  'ifdef HASEXAMPLES'#013#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDIR)'#013#010+
  'endif'#013#010+
  'ifdef EXAMPLESOURCEFILES'#013#010+
  '     ','   $(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)'#013#010+
  'endif'#013#010+
  'ifdef TARGET_EXAMPLEDIRS'#013#010+
  '        $(COPYTREE) $(addsuffix /*,$(TARGET_EXAMPLEDIRS)) $(INSTALL_EX'+
  'AMPLEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '[distinstallrules]'#013#010+
  '#########################################','###########################'+
  '#'#013#010+
  '# Dist Install'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_distinstall'#013#010+
  #013#010+
  'fpc_distinstall: install exampleinstall'#013#010+
  #013#010+
  #013#010+
  '[zipinstallrules]'#013#010+
  '##############################','######################################'+
  '#'#013#010+
  '# Zip'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_zipinstall fpc_zipsourceinstall fpc_zipexampleinstall'#013#010+
  #013#010+
  '# Temporary path to pack a file, can only use a si','ngle deep'#013#010+
  '# subdir, because the deltree can'#039't see the whole tree to remove'#013+
  #010+
  'ifndef PACKDIR'#013#010+
  'ifndef inUnix'#013#010+
  'PACKDIR=$(BASEDIR)/../fpc-pack'#013#010+
  'else'#013#010+
  'PACKDIR=/tmp/fpc-pack'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Maybe create default zipname from packagename'#013#010+
  'ifndef',' ZIPNAME'#013#010+
  'ifdef DIST_ZIPNAME'#013#010+
  'ZIPNAME=$(DIST_ZIPNAME)'#013#010+
  'else'#013#010+
  'ZIPNAME=$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef FULLZIPNAME'#013#010+
  'FULLZIPNAME=$(ZIPCROSSPREFIX)$(ZIPPREFIX)$(ZIPNAME)$(ZIPSUFFIX)'#013#010+
  'endif'#013#010+
  #013#010+
  '# ZipTarget'#013#010+
  'ifndef ZIPTARGET'#013#010+
  'ifdef DIST_ZI','PTARGET'#013#010+
  'ZIPTARGET=DIST_ZIPTARGET'#013#010+
  'else'#013#010+
  'ZIPTARGET=install'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Use tar by default under linux'#013#010+
  'ifndef USEZIP'#013#010+
  'ifdef inUnix'#013#010+
  'USETAR=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Use a wrapper script by default for Os/2'#013#010+
  'ifndef inUnix'#013#010+
  'USEZIPWRAPPER=1'#013,#010+
  'endif'#013#010+
  #013#010+
  '# We need to be able to run in the current OS so fix'#013#010+
  '# the path separator'#013#010+
  'ifdef USEZIPWRAPPER'#013#010+
  'ZIPPATHSEP=$(PATHSEP)'#013#010+
  'ZIPWRAPPER=$(subst /,$(PATHSEP),$(DIST_DESTDIR)/fpczip$(SRCBATCHEXT))'#013+
  #010+
  'else'#013#010+
  'ZIPPATHSEP=/'#013#010+
  'endif'#013#010+
  #013#010+
  '# Create c','ommands to create the zip/tar file'#013#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(ZIPPATHSEP),$(PACKDIR))'#013#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(ZIPPATHSEP),$(BASEDIR))'#013#010+
  'ifdef USETAR'#013#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(FULLZIPNAME)$(TAREXT)'#013#010+
  'ZIPCMD_ZIP:=$(TARPROG) c$(','TAROPT)f $(ZIPDESTFILE) *'#013#010+
  'else'#013#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(FULLZIPNAME)$(ZIPEXT)'#013#010+
  'ZIPCMD_ZIP:=$(subst /,$(ZIPPATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDES'+
  'TFILE) *'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_zipinstall:'#013#010+
  '        $(MAKE) $(ZIPTARGET) INSTALL_PREFIX=$(P','ACKDIR) ZIPINSTALL=1'#013+
  #010+
  '        $(MKDIR) $(DIST_DESTDIR)'#013#010+
  '        $(DEL) $(ZIPDESTFILE)'#013#010+
  'ifdef USEZIPWRAPPER'#013#010+
  '# Handle gecho separate as we need to espace \ with \\'#013#010+
  'ifneq ($(ECHOREDIR),echo)'#013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CD','PACK))" > $(ZIPWRAP'+
  'PER)'#013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_ZIP))" >> $(ZIPWRAPPER)'+
  #013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDBASE))" >> $(ZIPWRAPP'+
  'ER)'#013#010+
  'else'#013#010+
  '        echo $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#013#010+
  '        echo $(','ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#013#010+
  '        echo $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)'#013#010+
  'endif'#013#010+
  'ifdef inUnix'#013#010+
  '        /bin/sh $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  'ifdef RUNBATCH'#013#010+
  '        $(RUNBATCH) $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  '        $(ZIPWRAPPER)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  '        $(','DEL) $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  '        $(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(ZIPCMD_CDBASE)'#013#010+
  'endif'#013#010+
  '        $(DELTREE) $(PACKDIR)'#013#010+
  #013#010+
  'fpc_zipsourceinstall:'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=$(ZIP'+
  'SOURCESUFFIX)'#013#010+
  #013#010+
  'fpc','_zipexampleinstall:'#013#010+
  'ifdef HASEXAMPLES'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall ZIPSUFFIX=$(ZI'+
  'PEXAMPLESUFFIX)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_zipdistinstall:'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=distinstall'#013#010+
  #013#010+
  #013#010+
  '[cleanrules]'#013#010+
  '###########','#########################################################'+
  '#'#013#010+
  '# Clean rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#013#010+
  #013#010+
  'ifdef EXEFILES'#013#010+
  'override CLEANEXEFILES:=$(ad','dprefix $(TARGETDIRPREFIX),$(CLEANEXEFIL'+
  'ES))'#013#010+
  'override CLEANEXEDBGFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEDB'+
  'GFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEAN_PROGRAMS'#013#010+
  'override CLEANEXEFILES+=$(addprefix $(TARGETDIRPREFIX),$(addsuffix $(E'+
  'XEEXT), $(CLE','AN_PROGRAMS)))'#013#010+
  'override CLEANEXEDBGFILES+=$(addprefix $(TARGETDIRPREFIX),$(addsuffix '+
  '$(EXEDBGEXT), $(CLEAN_PROGRAMS)))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEAN_UNITS'#013#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEANPPUFILES',#013#010+
  'override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)'+
  ') $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(C'+
  'LEANPPUFILES))) $(addprefix $(IMPORTLIBPREFIX),$(subst $(PPUEXT),$(STA'+
  'TICLIBEXT),$(CLEANPPUFILES))',')'#013#010+
  'ifdef DEBUGSYMEXT'#013#010+
  'override CLEANPPULINKFILES+=$(subst $(PPUEXT),$(DEBUGSYMEXT),$(CLEANPP'+
  'UFILES))'#013#010+
  'endif'#013#010+
  'override CLEANPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPPUF'+
  'ILES))'#013#010+
  'override CLEANPPULINKFILES:=$(wildcard $(addprefix $(','UNITTARGETDIRPR'+
  'EFIX),$(CLEANPPULINKFILES)))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_clean: $(CLEANTARGET)'#013#010+
  'ifdef CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  '# DELTREE instead of DEL because on Mac OS X these are directories'#013+
  #010+
  'ifdef CLEANEXEDBGFILES'#013#010+
  '     ','   -$(DELTREE) $(CLEANEXEDBGFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANPPUFILES'#013#010+
  '        -$(DEL) $(CLEANPPUFILES)'#013#010+
  'endif'#013#010+
  'ifneq ($(CLEANPPULINKFILES),)'#013#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANRSTFILES'#013#010+
  '        -$(DEL) $(addprefix $(UNITTARG','ETDIRPREFIX),$(CLEANRSTFILES))'+
  #013#010+
  'endif'#013#010+
  'ifdef CLEAN_FILES'#013#010+
  '        -$(DEL) $(CLEAN_FILES)'#013#010+
  'endif'#013#010+
  'ifdef LIB_NAME'#013#010+
  '        -$(DEL) $(LIB_NAME) $(LIB_FULLNAME)'#013#010+
  'endif'#013#010+
  '        -$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(F'+
  'PCEXT','FILE) $(REDIRFILE)'#013#010+
  '        -$(DEL) *$(ASMEXT) *_ppas$(BATCHEXT)'#013#010+
  #013#010+
  'fpc_cleanall: $(CLEANTARGET)'#013#010+
  'ifdef CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  'ifdef CLEANPPUFILES'#013#010+
  '        -$(DEL) $(CLEANPPUFIL','ES)'#013#010+
  'endif'#013#010+
  'ifneq ($(CLEANPPULINKFILES),)'#013#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANRSTFILES'#013#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef CLEAN_FILES'#013#010+
  '        -$(DEL) $(CLEAN_FILE','S)'#013#010+
  'endif'#013#010+
  '        -$(DELTREE) units'#013#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#013#010+
  'ifneq ($(PPUEXT),.ppu)'#013#010+
  '        -$(DEL) *.o *.ppu *.a'#013#010+
  'endif'#013#010+
  '        -$(DELTREE) *$(SMARTEXT)'#013#010+
  '  ','      -$(DEL) fpcmade.* Package.fpc $(PPAS) script.res link.res $('+
  'FPCEXTFILE) $(REDIRFILE)'#013#010+
  '        -$(DEL) *_ppas$(BATCHEXT)'#013#010+
  'ifdef AOUTEXT'#013#010+
  '        -$(DEL) *$(AOUTEXT)'#013#010+
  'endif'#013#010+
  'ifdef DEBUGSYMEXT'#013#010+
  '        -$(DEL) *$(DEBUGSYMEXT)'#013#010+
  'endif'#013#010+
  #013#010+
  'f','pc_distclean: cleanall'#013#010+
  #013#010+
  #013#010+
  '[baseinforules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Base info rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_baseinfo'#013#010+
  #013#010+
  'override',' INFORULES+=fpc_baseinfo'#013#010+
  #013#010+
  'fpc_baseinfo:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Package info =='#013#010+
  '        @$(ECHO)  Package Name..... $(PACKAGE_NAME)'#013#010+
  '        @$(ECHO)  Package Version.. $(PACKAGE_VERSION)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECH','O)  == Configuration info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  FPC.......... $(FPC)'#013#010+
  '        @$(ECHO)  FPC Version.. $(FPC_VERSION)'#013#010+
  '        @$(ECHO)  Source CPU... $(CPU_SOURCE)'#013#010+
  '        @$(ECHO)  Target CPU... $(CPU_TARGET)'#013#010+
  '        @$','(ECHO)  Source OS.... $(OS_SOURCE)'#013#010+
  '        @$(ECHO)  Target OS.... $(OS_TARGET)'#013#010+
  '        @$(ECHO)  Full Source.. $(FULL_SOURCE)'#013#010+
  '        @$(ECHO)  Full Target.. $(FULL_TARGET)'#013#010+
  '        @$(ECHO)  SourceSuffix. $(SOURCESUFFIX)'#013#010+
  '        @$(EC','HO)  TargetSuffix. $(TARGETSUFFIX)'#013#010+
  '        @$(ECHO)  FPC fpmake... $(FPCFPMAKE)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Directory info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Required pkgs... $(REQUIRE_PACKAGES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        ','@$(ECHO)  Basedir......... $(BASEDIR)'#013#010+
  '        @$(ECHO)  FPCDir.......... $(FPCDIR)'#013#010+
  '        @$(ECHO)  CrossBinDir..... $(CROSSBINDIR)'#013#010+
  '        @$(ECHO)  UnitsDir........ $(UNITSDIR)'#013#010+
  '        @$(ECHO)  PackagesDir..... $(PACKAGESDIR)'#013#010+
  '     ','   @$(ECHO)'#013#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#013#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Tools info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  As........ $(AS)'#013#010+
  '        @$(ECHO)  L','d........ $(LD)'#013#010+
  '        @$(ECHO)  Ar........ $(AR)'#013#010+
  '        @$(ECHO)  Rc........ $(RC)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Mv........ $(MVPROG)'#013#010+
  '        @$(ECHO)  Cp........ $(CPPROG)'#013#010+
  '        @$(ECHO)  Rm........ $(RMPROG)'#013#010+
  '        @$(EC','HO)  GInstall.. $(GINSTALL)'#013#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#013#010+
  '        @$(ECHO)  Shell..... $(SHELL)'#013#010+
  '        @$(ECHO)  Date...... $(DATE)'#013#010+
  '        @$(ECHO)  FPCMake... $(FPCMAKE)'#013#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#013#010+
  '        @$(ECH','O)  Zip....... $(ZIPPROG)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Object info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Target Loaders........ $(TARGET_LOADERS)'#013#010+
  '        @$(ECHO)  Target Units.......... $(TARGET_UNITS)'#013#010+
  '        @$(ECHO)  Targ','et Implicit Units. $(TARGET_IMPLICITUNITS)'#013+
  #010+
  '        @$(ECHO)  Target Programs....... $(TARGET_PROGRAMS)'#013#010+
  '        @$(ECHO)  Target Dirs........... $(TARGET_DIRS)'#013#010+
  '        @$(ECHO)  Target Examples....... $(TARGET_EXAMPLES)'#013#010+
  '        @$(ECHO) ',' Target ExampleDirs.... $(TARGET_EXAMPLEDIRS)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Clean Units......... $(CLEAN_UNITS)'#013#010+
  '        @$(ECHO)  Clean Files......... $(CLEAN_FILES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Install Units....... $(INSTAL','L_UNITS)'#013#010+
  '        @$(ECHO)  Install Files....... $(INSTALL_FILES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Install info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#013#010+
  '        @$(ECHO)  ZipName.............. $(ZIPNA','ME)'#013#010+
  '        @$(ECHO)  ZipPrefix............ $(ZIPPREFIX)'#013#010+
  '        @$(ECHO)  ZipCrossPrefix....... $(ZIPCROSSPREFIX)'#013#010+
  '        @$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)'#013#010+
  '        @$(ECHO)  FullZipName.......... $(FULLZIPNAME)'#013#010+
  '        @$(E','CHO)  Install FPC Package.. $(INSTALL_FPCPACKAGE)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)'#013#010+
  '        @$(ECHO)  Install binary dir... $(INSTALL_BINDIR)'#013#010+
  '        @$(ECHO)  Install library dir.. $(INSTALL_LI','BDIR)'#013#010+
  '        @$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)'#013#010+
  '        @$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)'#013#010+
  '        @$(ECHO)  Install doc dir...... $(INSTALL_DOCDIR)'#013#010+
  '        @$(ECHO)  Install example dir.. $(INSTALL_EX','AMPLEDIR)'#013#010+
  '        @$(ECHO)  Install data dir..... $(INSTALL_DATADIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Dist destination dir. $(DIST_DESTDIR)'#013#010+
  '        @$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[inforules]'#013#010+
  '####','################################################################'+
  '#'#013#010+
  '# Info rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_info'#013#010+
  #013#010+
  'fpc_info: $(INFORULES)'#013#010+
  #013#010+
  '[makefilerules]'#013#010+
  '#######################','#############################################'+
  '#'#013#010+
  '# Rebuild Makefile'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_makefile fpc_makefiles fpc_makefile_sub1 fpc_makefile_sub2'+
  ' \'#013#010+
  '        fpc_makefile_dirs',#013#010+
  #013#010+
  'fpc_makefile:'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) Makefile.fpc'#013#010+
  #013#010+
  'fpc_makefile_sub1:'#013#010+
  'ifdef TARGET_DIRS'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGE'+
  'T_DIRS))'#013#010+
  'endif'#013#010+
  'ifdef TARGET_EXAMPLEDIRS'#013#010+
  '        $(FPCMAK','E) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TAR'+
  'GET_EXAMPLEDIRS))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_makefile_sub2: $(addsuffix _makefile_dirs,$(TARGET_DIRS) $(TARGET_'+
  'EXAMPLEDIRS))'#013#010+
  #013#010+
  'fpc_makefile_dirs: fpc_makefile_sub1 fpc_makefile_sub2'#013#010+
  #013#010+
  'fpc_makefiles: ','fpc_makefile fpc_makefile_dirs'#013#010+
  #013#010+
  '[localmakefile]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Local Makefile'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  'ifneq ($(wildcard fpcmake.l','oc),)'#013#010+
  'include fpcmake.loc'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[userrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Users rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '[lclrules]'#013#010+
  '#################','###################################################'+
  '#'#013#010+
  '# LCL Rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# LCL Platform'#013#010+
  'ifndef LCL_PLATFORM'#013#010+
  'ifneq ($(findstring $(OS_TARGET),win32 win64),)'#013#010+
  'LCL_PLATFORM=w','in32'#013#010+
  'else'#013#010+
  'LCL_PLATFORM=gtk'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export LCL_PLATFORM'#013#010+
  #013#010+
  '# Check if the specified LCLDIR is correct'#013#010+
  'ifdef LCLDIR'#013#010+
  'override LCLDIR:=$(subst \,/,$(LCLDIR))'#013#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#013#010+
  'override LCLDIR=wrong',#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if the default LCLDIR is correct'#013#010+
  'ifdef DEFAULT_LCLDIR'#013#010+
  'override LCLDIR:=$(subst \,/,$(DEFAULT_LCLDIR))'#013#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endi','f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check for development version'#013#010+
  'ifeq ($(LCLDIR),wrong)'#013#010+
  'override LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildcard'+
  ' $(addsuffix /units/$(LCL_PLATFORM),$(BASEDIR)/lcl $(BASEDIR)))))'#013#010+
  'ifeq ($(LCLDIR),)'#013#010+
  'override LCLDIR','=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check for release version'#013#010+
  'ifeq ($(LCLDIR),wrong)'#013#010+
  'override LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildcard'+
  ' $(addsuffix /lib/lazarus/units/$(LCL_PLATFORM),/usr/local /usr))))'#013+
  #010+
  'ifeq ($(LCLDIR),)'#013#010+
  'overr','ide LCLDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Generate dirs'#013#010+
  'override LCLUNITDIR:=$(wildcard $(LCLDIR)/units/$(LCL_PLATFORM) $(LCLD'+
  'IR)/units)'#013#010+
  'override LCLCOMPONENTDIR:=$(wildcard $(LCLDIR)/.. $(LCLDIR)/../compone'+
  'nts $(LCLDIR)/components)'#013#010+
  'export LCL','DIR LCLUNITDIR LCLCOMPONENTDIR'#013#010+
  #013#010+
  '# Add LCL dirs to paths'#013#010+
  'override REQUIRE_PACKAGESDIR+=$(LCLCOMPONENTDIR)'#013#010+
  'override COMPILER_UNITDIR+=$(LCLUNITDIR)'#013#010+
  #013#010+
  '[lclinforules]'#013#010+
  '#####################################################################'#013+
  #010,
  '# LCL Info rules'#013#010+
  '#####################################################################'#013+
  #010+
  'override INFORULES+=lclinfo'#013#010+
  #013#010+
  '.PHONY: lclinfo'#013#010+
  #013#010+
  'lclinfo:'#013#010+
  '        @$(ECHO)  == LCL info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Platform.............',' $(LCL_PLATFORM)'#013#010+
  '        @$(ECHO)  LCLDIR............... $(LCLDIR)'#013#010+
  '        @$(ECHO)  LCL Unit dir......... $(LCLUNITDIR)'#013#010+
  '        @$(ECHO)  LCL Component dir.... $(LCLCOMPONENTDIR)'#013#010+
  '        @$(ECHO)'#013#010
);
