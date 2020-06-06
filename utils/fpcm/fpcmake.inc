{$ifdef Delphi}
const fpcmakeini : array[0..262] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..262,1..240] of char=(
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
  'BSDs = freebsd netbsd openbsd darwin dragonfly'#013#010+
  'UNIXs = linux $(BSDs) solaris qnx haiku aix'#013#010+
  'LIMIT83fs = go32v2 os2 emx watcom msdos win16 atari'#013#010+
  'OSNeedsComspecToRunBatch = go32v2 watcom'#013#010+
  #013#010+
  '#Empt','y target for rules that always should run. Needed if'#013#010+
  '#the target is non-phoney, and there is non-phony prereqisites.'#013#010+
  '#Then add FORCE as an prerequisite'#013#010+
  '#See gnu make manual: 4.7 Rules without Commands or Prerequisites'#013#010+
  'FORCE:'#013#010+
  '.PHONY: FO','RCE'#013#010+
  #013#010+
  '[osdetect]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Autodetect source OS (Linux or Dos or Windows NT or OS/2 or other)'#013+
  #010+
  '# define inUnix when running under Unix like environment'#013#010+
  '#               (Linux',',FreeBSD,NetBSD,OpenBSD,Darwin,Cygwin)'#013#010+
  '# define inWinNT when running under WinNT'#013#010+
  '# define inOS2 when running under OS/2'#013#010+
  '# define inCygwin when running under Cygwin32'#013#010+
  '#####################################################################'#013,
  #010+
  #013#010+
  '# We need only / in the path also remove the current dir,'#013#010+
  '# also remove trailing /'#039's'#013#010+
  'override PATH:=$(patsubst %/,%,$(subst \,/,$(PATH)))'#013#010+
  #013#010+
  '# Detect unix'#013#010+
  '# Darwin is handled specially'#013#010+
  'ifneq ($(findstring darwin,$(OSTYPE)),)'#013#010+
  'inUnix=','1 #darwin'#013#010+
  'SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))'#013#010+
  'else'#013#010+
  '# Determine if we'#039've a unix searchpath by looking for a ;'#013#010+
  '# that normally doesn'#039't exists in the unix PATH var.'#013#010+
  'ifeq ($(findstring ;,$(PATH)),)'#013#010+
  'inUnix=1'#013#010+
  'SEARCHPATH:=$(fil','ter-out .,$(subst :, ,$(PATH)))'#013#010+
  'else'#013#010+
  'SEARCHPATH:=$(subst ;, ,$(PATH))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Add path were make is located'#013#010+
  'SEARCHPATH+=$(patsubst %/,%,$(subst \,/,$(dir $(MAKE))))'#013#010+
  #013#010+
  '# Search for PWD'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd','.exe,$(SEARCHPATH))))'#013#010+
  'ifeq ($(PWD),)'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(SEARCHPATH))))'#013#010+
  'ifeq ($(PWD),)'#013#010+
  '$(error You need the GNU utils package to use this Makefile)'#013#010+
  'else'#013#010+
  'PWD:=$(firstword $(PWD))'#013#010+
  'SRCEXEEXT='#013#010+
  'endif'#013#010+
  'else'#013#010+
  'PWD:=$(','firstword $(PWD))'#013#010+
  'SRCEXEEXT=.exe'#013#010+
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
  'ifneq ($(','findstring cygdrive,$(PATH)),)'#013#010+
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
  'ifneq ($(findst','ring $(OS_SOURCE),$(OSNeedsComspecToRunBatch)),)'#013#010+
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
  'e','lse'#013#010+
  'PATHSEP:=$(subst /,\,/)'#013#010+
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
  'ifd','ef inCygWin'#013#010+
  'ifneq ($(findstring /cygdrive/,$(BASEDIR)),)'#013#010+
  'BASENODIR:=$(patsubst /cygdrive%,%,$(BASEDIR))'#013#010+
  'BASEDRIVE:=$(firstword $(subst /, ,$(BASENODIR)))'#013#010+
  'BASEDIR:=$(subst /cygdrive/$(BASEDRIVE)/,$(BASEDRIVE):/,$(BASEDIR))'#013+
  #010+
  'endif'#013#010+
  'endif'#013,#010+
  'else'#013#010+
  'BASEDIR=.'#013#010+
  'endif'#013#010+
  #013#010+
  '# Echo is an internal command under OS/2 (and others), so it'#039's alw'+
  'ays found'#013#010+
  'ifdef inOS2'#013#010+
  'ifndef ECHO'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))'+
  '))'#013#010+
  'ifeq ($(ECHO),)'#013#010+
  'ECHO:=$(strip $(wil','dcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH)'+
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
  '##################################','##################################'+
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
  '# Try to ','detect the ppcXXX file to use by using "fpc -PB" option'#013+
  #010+
  '# to query for the default ppcXXX the fpc executable tries. When'#013#010+
  '# fpc is not found use ppc386 by default. Also when fpc -PB gives'#013#010+
  '# an error ppc386 will be used.'#013#010+
  'ifndef FPC'#013#010+
  '# che','ck if fpc exists'#013#010+
  'FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifneq ($(FPCPROG),)'#013#010+
  'FPCPROG:=$(firstword $(FPCPROG))'#013#010+
  'ifneq ($(CPU_TARGET),)'#013#010+
  'FPC:=$(shell $(FPCPROG) -P$(CPU_TARGET) -PB)'#013#010+
  'else'#013#010+
  'FPC:=$(shell $(F','PCPROG) -PB)'#013#010+
  'endif'#013#010+
  '# Older fpc executables didn'#039't support it and return'#013#010+
  '# Error: Illegal processor... If found then fallback to ppc386'#013#010+
  'ifneq ($(findstring Error,$(FPC)),)'#013#010+
  'override FPC=$(firstword $(strip $(wildcard $(addsuffix /ppc386$','(SRC'+
  'EXEEXT),$(SEARCHPATH)))))'#013#010+
  'else'#013#010+
  '# if the cross compiler is not found, fall back to fpc'#013#010+
  'ifeq ($(strip $(wildcard $(FPC))),)'#013#010+
  'FPC:=$(firstword $(FPCPROG))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# fpc binary not found, fallback to ppc386'#013#010+
  'override FPC=$(f','irstword $(strip $(wildcard $(addsuffix /ppc386$(SRC'+
  'EXEEXT),$(SEARCHPATH)))))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Get a clean executable name'#013#010+
  'override FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#013#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#013#010+
  #013#010+
  '# Try to find the bi','nary direct first, otherwise in the path,'#013#010+
  '# if not found give an error'#013#010+
  'FOUNDFPC:=$(strip $(wildcard $(FPC)))'#013#010+
  'ifeq ($(FOUNDFPC),)'#013#010+
  'FOUNDFPC=$(strip $(wildcard $(addsuffix /$(FPC),$(SEARCHPATH))))'#013#010+
  'ifeq ($(FOUNDFPC),)'#013#010+
  '$(error Compiler $(F','PC) not found)'#013#010+
  'endif'#013#010+
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
  'FP','C_VERSION:=$(word 1,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  #013#010+
  'export FPC FPC_VERSION FPC_COMPILERINFO'#013#010+
  '# CHECKDEPEND should not be exported'#013#010+
  '# This should limit multiple checks'#013#010+
  'unexport CHECKDEPEND ALLDEPENDENCIES'#013#010+
  #013#010+
  '#################################','###################################'+
  '#'#013#010+
  '# FPC Target Detection'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Fall back to default values if needed'#013#010+
  'ifndef CPU_TARGET'#013#010+
  'ifdef CPU_TARGET_DEFAULT'#013#010+
  'CPU_TARGET=$(CPU_TA','RGET_DEFAULT)'#013#010+
  'endif'#013#010+
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
  'endif'#013#010,
  'ifndef CPU_TARGET'#013#010+
  'CPU_TARGET:=$(word 3,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef OS_SOURCE'#013#010+
  'OS_SOURCE:=$(word 4,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef OS_TARGET'#013#010+
  'OS_TARGET:=$(word 5,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'FULL_TARGET=$(CPU_TARGET)-$(OS_TARGET',')'#013#010+
  'FULL_SOURCE=$(CPU_SOURCE)-$(OS_SOURCE)'#013#010+
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
  'ifeq ($(FULL_TARGET),ar','m-embedded)'#013#010+
  'ifeq ($(SUBARCH),)'#013#010+
  '# we don'#039't add a default here, people should explicitly add the SU'+
  'BARCH'#013#010+
  '$(error When compiling for arm-embedded, a sub-architecture (e.g. SUBA'+
  'RCH=armv4t or SUBARCH=armv7m) must be defined)'#013#010+
  'endif'#013#010+
  'override ','FPCOPT+=-Cp$(SUBARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(FULL_TARGET),avr-embedded)'#013#010+
  'ifeq ($(SUBARCH),)'#013#010+
  '# we don'#039't add a default here, people should explicitly add the SU'+
  'BARCH'#013#010+
  '$(error When compiling for avr-embedded, a sub-architecture (e.g. SUBA'+
  'RCH=avr25',' or SUBARCH=avr35) must be defined)'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Cp$(SUBARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(FULL_TARGET),mipsel-embedded)'#013#010+
  'ifeq ($(SUBARCH),)'#013#010+
  '# we don'#039't add a default here, people should explicitly add the SU'+
  'BARCH'#013#010+
  '$(error When compiling ','for mipsel-embedded, a sub-architecture (e.g.'+
  ' SUBARCH=pic32mx) must be defined)'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Cp$(SUBARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(FULL_TARGET),xtensa-embedded)'#013#010+
  'ifeq ($(SUBARCH),)'#013#010+
  '$(error When compiling for xtensa-embedded, a sub-ar','chitecture (e.g.'+
  ' SUBARCH=lx106 or SUBARCH=lx6) must be defined)'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Cp$(SUBARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(FULL_TARGET),xtensa-freertos)'#013#010+
  'ifeq ($(SUBARCH),)'#013#010+
  '$(error When compiling for xtensa-freertos, a sub-architecture (e.g.',' '+
  'SUBARCH=lx106 or SUBARCH=lx6) must be defined)'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Cp$(SUBARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(FULL_TARGET),arm-freertos)'#013#010+
  'ifeq ($(SUBARCH),)'#013#010+
  '$(error When compiling for arm-freertos, a sub-architecture (e.g. SUBA'+
  'RCH=armv6m or SUB','ARCH=armv7em) must be defined)'#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Cp$(SUBARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Full name of the target, including CPU and OS. For OSs limited'#013#010+
  '# to 8.3 we only use the target OS'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)'#013#010+
  'TARGETSU','FFIX=$(OS_TARGET)'#013#010+
  'SOURCESUFFIX=$(OS_SOURCE)'#013#010+
  'else'#013#010+
  'ifneq ($(findstring $(OS_TARGET),$(LIMIT83fs)),)'#013#010+
  'TARGETSUFFIX=$(OS_TARGET)'#013#010+
  'else'#013#010+
  'TARGETSUFFIX=$(FULL_TARGET)'#013#010+
  'endif'#013#010+
  'SOURCESUFFIX=$(FULL_SOURCE)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross compile flag'#013#010+
  'ifneq ($(','FULL_TARGET),$(FULL_SOURCE))'#013#010+
  'CROSSCOMPILE=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if the Makefile supports this target, but not'#013#010+
  '# when the make target is to rebuild the makefile'#013#010+
  'ifeq ($(findstring makefile,$(MAKECMDGOALS)),)'#013#010+
  'ifeq ($(findstring $(FULL_TARGE','T),$(MAKEFILETARGETS)),)'#013#010+
  '$(error The Makefile doesn'#039't support target $(FULL_TARGET), please'+
  ' run fpcmake first)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# Detect BSD, since BSD uses a slightly different directory hierarchy.'+
  #013#010+
  'ifneq ($(findstring $(OS_TARGET),$(BS','Ds)),)'#013#010+
  'BSDhier=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Detect Linux, will also use its own directory hierarchy.'#013#010+
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
  'BUILDNA','TIVE=1'#013#010+
  'export BUILDNATIVE'#013#010+
  'endif'#013#010+
  #013#010+
  'export OS_TARGET OS_SOURCE ARCH CPU_TARGET CPU_SOURCE FULL_TARGET FULL'+
  '_SOURCE TARGETSUFFIX SOURCESUFFIX CROSSCOMPILE'#013#010+
  #013#010+
  '[fpmakefpcdetect]'#013#010+
  '################################################################','####'+
  '#'#013#010+
  '# FPC Binary and Version Detection'#013#010+
  '#####################################################################'#013+
  #010+
  '# If FPCFPMAKE is not set and we'#039're cross-compiling, try to find a'+
  #013#010+
  '# native compiler.'#013#010+
  'ifndef FPCFPMAKE'#013#010+
  #013#010+
  'ifdef CROSSCOMPILE',#013#010+
  '# Search for the (native) ppc compiler which is used to do the latest '+
  'build'#013#010+
  '# of the native rtl'#013#010+
  #013#010+
  'ifeq ($(strip $(wildcard $(addsuffix /compiler/ppc$(SRCEXEEXT),$(FPCDI'+
  'R)))),)'#013#010+
  '# If that compiler is not found, use the common, installed ','native co'+
  'mpiler'#013#010+
  '# Note that in this case FPCFPMAKE is not exported, because it could b'+
  'e that'#013#010+
  '# a more suitable compiler is compiled later on'#013#010+
  '# check if fpc exists'#013#010+
  'FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))',#013#010+
  'ifneq ($(FPCPROG),)'#013#010+
  'FPCPROG:=$(firstword $(FPCPROG))'#013#010+
  'FPCFPMAKE:=$(shell $(FPCPROG) -PB)'#013#010+
  '# if a native compiler is not found, fall back to fpc'#013#010+
  'ifeq ($(strip $(wildcard $(FPCFPMAKE))),)'#013#010+
  'FPCFPMAKE:=$(firstword $(FPCPROG))'#013#010+
  'endif'#013#010+
  'else'#013#010,
  '# fpc binary not found, fallback to ppc386'#013#010+
  'override FPCFPMAKE=$(firstword $(strip $(wildcard $(addsuffix /ppc386$'+
  '(SRCEXEEXT),$(SEARCHPATH)))))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# Use the ppc compiler. Because the path to the compiler is relative, '+
  'do not'#013#010+
  '# ev','aluate the variable here, but do that on the fly.'#013#010+
  'FPCFPMAKE=$(strip $(wildcard $(addsuffix /compiler/ppc$(SRCEXEEXT),$(F'+
  'PCDIR))))'#013#010+
  'FPMAKE_SKIP_CONFIG=-n'#013#010+
  'export FPCFPMAKE'#013#010+
  'export FPMAKE_SKIP_CONFIG'#013#010+
  'endif'#013#010+
  #013#010+
  'else'#013#010+
  'FPMAKE_SKIP_CONFIG=-n'#013#010+
  'FP','CFPMAKE=$(FPC)'#013#010+
  'endif'#013#010+
  #013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[fpcdircheckenv]'#013#010+
  '#####################################################################'#013+
  #010+
  '# FPCDIR Setting'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Test FPCDIR to look if',' the RTL dir exists'#013#010+
  'ifdef FPCDIR'#013#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default FPCDIR from Makefile.fpc'#013#010+
  'ifdef DEFAU','LT_FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'override FPCDIR:=$(subst \,/,$(DEFAULT_FPCDIR))'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[fpcdirdetect]'#013#010+
  '# Detect FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'ifde','f inUnix'#013#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION)'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC)',',$(SEARCHPATH))))))'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)'#013#010+
  'override FPCDIR:=$(BASEDIR)'#013#010+
  'ifeq ($(wildcard $(addprefix $','(FPCDIR)/,rtl)),)'#013#010+
  'override FPCDIR=c:/pp'#013#010+
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
  '# Default binutils prefix for cross compile when the'#013#010,
  '# crossbindir is not set (except for Darwin)'#013#010+
  'ifneq ($(findstring $(OS_TARGET),darwin iphonesim),)'#013#010+
  'ifeq ($(OS_SOURCE),darwin)'#013#010+
  'DARWIN2DARWIN=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef BINUTILSPREFIX'#013#010+
  'ifndef CROSSBINDIR'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'ifneq ($(OS_TARG','ET),msdos)'#013#010+
  'ifndef DARWIN2DARWIN'#013#010+
  'ifneq ($(CPU_TARGET),jvm)'#013#010+
  'BINUTILSPREFIX=$(CPU_TARGET)-$(OS_TARGET)-'#013#010+
  #013#010+
  '# For Android use standard NDK prefixes'#013#010+
  'ifeq ($(OS_TARGET),android)'#013#010+
  'ifeq ($(CPU_TARGET),arm)'#013#010+
  'BINUTILSPREFIX=arm-linux-androideabi-',#013#010+
  'else'#013#010+
  'ifeq ($(CPU_TARGET),i386)'#013#010+
  'BINUTILSPREFIX=i686-linux-android-'#013#010+
  'else'#013#010+
  'BINUTILSPREFIX=$(CPU_TARGET)-linux-android-'#013#010+
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
  '# Try first the',' full target name, otherwise try only'#013#010+
  '# the OS for backwards compatibility'#013#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(TARGETSUFFIX))'#013#010+
  'ifeq ($(UNITSDIR),)'#013#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_TARGET))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Packages dir'#013#010+
  'PACKAGESDI','R:=$(wildcard $(FPCDIR) $(FPCDIR)/packages)'#013#010+
  #013#010+
  #013#010+
  '[shelltools]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Shell tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Echo that can re','dir (must be able run in the default OS shell)'#013+
  #010+
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
  '# Copy a whole tree'#013#010+
  'ifndef COPYTREE'#013#010+
  'CO','PYTREE:=$(CPPROG) -Rfp'#013#010+
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
  'ifndef DEL'#013#010+
  'DEL:=$(RMPROG) -f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Chec','k deltree program'#013#010+
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
  '# To install programs'#013#010+
  'ifndef INSTALLEXE'#013#010+
  'ifdef in','Unix'#013#010+
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
  'export ECHOREDIR COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKD'+
  'IR'#013#010+
  #013#010+
  #013#010+
  '[','defaultdirs]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Units dir'#013#010+
  'ifdef REQUIRE_UNITSDIR'#013#010+
  'override UNITSDIR+=$(R','EQUIRE_UNITSDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Units dir'#013#010+
  'ifdef REQUIRE_PACKAGESDIR'#013#010+
  'override PACKAGESDIR+=$(REQUIRE_PACKAGESDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# Unixes use unix dirs with /usr/bin, /usr/lib'#013#010+
  '# When zipping use the target os default, when normal install then'#013+
  #010,
  '# use the source os as default'#013#010+
  'ifdef ZIPINSTALL'#013#010+
  '# Zipinstall'#013#010+
  'ifneq ($(findstring $(OS_TARGET),$(UNIXs)),)'#013#010+
  'UNIXHier=1'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# Normal install'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(UNIXs)),)'#013#010+
  'UNIXHier=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# When instal','l prefix is not set try to use prefix'#013#010+
  'ifndef INSTALL_PREFIX'#013#010+
  'ifdef PREFIX'#013#010+
  'INSTALL_PREFIX=$(PREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the prefix directory where to install everything'#013#010+
  'ifndef INSTALL_PREFIX'#013#010+
  'ifdef UNIXHier'#013#010+
  'INSTALL_PREFIX=/usr/local'#013#010,
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
  '# used recursively for all subdirs'#013#010+
  'ifdef INS','TALL_FPCSUBDIR'#013#010+
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
  'ifndef COMPILER_UNITTARGETDIR'#013#010+
  'ifdef PAC','KAGEDIR_MAIN'#013#010+
  'COMPILER_UNITTARGETDIR=$(PACKAGEDIR_MAIN)/units/$(TARGETSUFFIX)'#013#010+
  'else'#013#010+
  'COMPILER_UNITTARGETDIR=units/$(TARGETSUFFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifndef COMPILER_TARGETDIR'#013#010+
  'COMPILER_TARGETDIR=.'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '###################################','#################################'+
  '#'#013#010+
  '# Install Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# set the base directory where to install everything'#013#010+
  'ifndef INSTALL_BASEDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'ifdef INSTALL_','FPCPACKAGE'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the directory where to install the b','inaries'#013#010+
  'ifndef INSTALL_BINDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_PREFIX)/bin'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BASEDIR)/bin'#013#010+
  '# for FPC packages install the binaries under their target subdir'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef CROSSCOM','PILE'#013#010+
  'ifdef CROSSINSTALL'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(SOURCESUFFIX)'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(TARGETSUFFIX)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(TARGETSUFFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the di','rectory where to install the units.'#013#010+
  'ifndef INSTALL_UNITDIR'#013#010+
  'INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/units/$(TARGETSUFFIX)'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef PACKAGE_NAME'#013#010+
  'INSTALL_UNITDIR:=$(INSTALL_UNITDIR)/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010,
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
  '# Where the source files will be stored'#013#010+
  'ifndef INSTALL_SOURCEDIR',#013#010+
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
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFI','XDIR)/fpc-$(FPC_VERSI'+
  'ON)/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/$(PACKAGE_NAME)-',
  '$(PACKAGE_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(INSTALL_FPCSUBDIR)/$(PA'+
  'CKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(PACKAGE_NAME)'#013#010+
  'endif',#013#010+
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
  'ifdef linuxHier'#013#010+
  'DOCPREFIXDIR=share/doc'#013#010,
  'else'#013#010+
  'DOCPREFIXDIR=doc'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/fpc-$(FPC_VERSION)/$'+
  '(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/$(PACKAGE_NAME)-$(PA'+
  'CKAGE_VERSION)',#013#010+
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
  '# Where to install the examples, under linux we use the doc dir'#013#010+
  '# because',' the copytree command will create a subdir itself'#013#010+
  'ifndef INSTALL_EXAMPLEDIR'#013#010+
  'ifdef UNIXHier'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  #013#010+
  'ifdef BSDhier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'ifdef li','nuxHier'#013#010+
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
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/example','s/$(PACKAGE_NAME)'+
  '-$(PACKAGE_VERSION)'#013#010+
  'else'#013#010+
  'ifdef linuxHier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_DOCDIR)/examples/$(PACKAGE_NAME)-$(PACKA'+
  'GE_VERSION)'#013#010+
  'else'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_NAME)-$(PACKAGE_VE'+
  'RSION)'#013#010+
  'endif'#013#010+
  #013#010+
  'end','if'#013#010+
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
  '# Where the some extra (data)files will be stored'#013#010+
  'ifn','def INSTALL_DATADIR'#013#010+
  'INSTALL_DATADIR=$(INSTALL_BASEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef INSTALL_SHAREDDIR'#013#010+
  'INSTALL_SHAREDDIR=$(INSTALL_PREFIX)/lib'#013#010+
  'endif'#013#010+
  #013#010+
  '#####################################################################'#013+
  #010+
  '# Cross compile dirs'#013#010+
  '#####','###############################################################'+
  '#'#013#010+
  #013#010+
  'ifdef CROSSCOMPILE'#013#010+
  '# Directory where the cross compile tools are stored.'#013#010+
  '# First check if they are available in FPCDIR. If no targets/ subdir'#013+
  #010+
  '# is found use the targets/',' subdir in INSTALL_BASEDIR.'#013#010+
  'ifndef CROSSBINDIR'#013#010+
  'CROSSBINDIR:=$(wildcard $(CROSSTARGETDIR)/bin/$(SOURCESUFFIX))'#013#010+
  'ifeq ($(CROSSBINDIR),)'#013#010+
  'CROSSBINDIR:=$(wildcard $(INSTALL_BASEDIR)/cross/$(TARGETSUFFIX)/bin/$'+
  '(FULL_SOURCE))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'els','e'#013#010+
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
  'ifneq ($(findstring x86_64,$(shell uname -a)),)'#013#010+
  'ifeq ($(BINUTILS','PREFIX),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m32 -print-libgcc-file-name`)'#013#010+
  'else'#013#010+
  'CROSSGCCOPT=-m32'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifeq ($(CPU_TARGET),powerpc64)'#013#010+
  'ifeq ($(BINUTILSPREFIX),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m64 -print-libgcc-file-name`',')'#013#010+
  'else'#013#010+
  'CROSSGCCOPT=-m64'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifeq ($(CPU_TARGET),sparc)'#013#010+
  'ifneq ($(findstring sparc64,$(shell uname -a)),)'#013#010+
  'ifeq ($(BINUTILSPREFIX),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `gcc -m32 -print-libgcc-file-name`)'#013#010+
  'else'#013#010+
  'CROSSGCCOPT=-m32'#013#010+
  'endif'#013,#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if FPCFPMAKE compiler is same target as FPC'#013#010+
  'ifdef FPCFPMAKE'#013#010+
  'FPCFPMAKE_CPU_TARGET=$(shell $(FPCFPMAKE) -iTP)'#013#010+
  'ifeq ($(CPU_TARGET),$(FPCFPMAKE_CPU_TARGET))'#013#010+
  '# In that case use GCCLIBDIR value for FPCMAKEGCCLI','BDIR'#013#010+
  'FPCMAKEGCCLIBDIR:=$(GCCLIBDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef FPCMAKEGCCLIBDIR'#013#010+
  'FPCMAKEGCCLIBDIR:=$(shell dirname `gcc -print-libgcc-file-name`)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Try cross gcc'#013#010+
  'ifndef GCCLIBDIR'#013#010+
  'CROSSGCC=$(strip $(wildcard $(addsuffix /$(BINUTILS','PREFIX)gcc$(SRCEX'+
  'EEXT),$(SEARCHPATH))))'#013#010+
  'ifneq ($(CROSSGCC),)'#013#010+
  'GCCLIBDIR:=$(shell dirname `$(CROSSGCC) $(CROSSGCCOPT) -print-libgcc-f'+
  'ile-name`)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  '#ifeq($(OS_SOURCE),linux)'#013#010+
  #013#010+
  'ifdef inUnix'#013#010+
  'ifeq ($(OS_SOURCE),netbsd)'#013#010+
  'OTH','ERLIBDIR:=/usr/pkg/lib'#013#010+
  'endif'#013#010+
  'export GCCLIBDIR FPCMAKEGCCLIBDIR OTHERLIBDIR'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[extensions]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default extensions'#013#010+
  '############################################','########################'+
  '#'#013#010+
  #013#010+
  '# Default needed extensions (Go32v2,Linux)'#013#010+
  'BATCHEXT=.bat'#013#010+
  'LOADEREXT=.as'#013#010+
  'EXEEXT=.exe'#013#010+
  'PPLEXT=.ppl'#013#010+
  'PPUEXT=.ppu'#013#010+
  'OEXT=.o'#013#010+
  'LTOEXT=.bc'#013#010+
  'ASMEXT=.s'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=.so'#013#010+
  'SHAREDLIBPREFIX=l','ibfp'#013#010+
  'STATICLIBPREFIX=libp'#013#010+
  'IMPORTLIBPREFIX=libimp'#013#010+
  'RSTEXT=.rst'#013#010+
  '# external debug info for executable'#013#010+
  'EXEDBGEXT=.dbg'#013#010+
  '#DEBUGSYMEXT #for debugger symbol files, define only for targets which'+
  ' has this'#013#010+
  #013#010+
  '# Go32v1'#013#010+
  'ifeq ($(OS_TARGET),go32v1)'#013#010,
  'STATICLIBPREFIX='#013#010+
  'SHORTSUFFIX=v1'#013#010+
  'endif'#013#010+
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
  'SHAREDLIBEXT=.dll',#013#010+
  'SHORTSUFFIX=wat'#013#010+
  'IMPORTLIBPREFIX='#013#010+
  'endif'#013#010+
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
  'BATCHEXT=.sh',#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=lnx'#013#010+
  'endif'#013#010+
  #013#010+
  '# DragonFly BSD'#013#010+
  'ifeq ($(OS_TARGET),dragonfly)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=df'#013#010+
  'endif'#013#010+
  #013#010+
  '# FreeBSD'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB','=1'#013#010+
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
  'ifeq ($(OS_TARGET),openbsd)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'SHORTSUFFIX=obs'#013#010+
  'endif'#013#010+
  #013#010+
  '# ','Win32'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=w32'#013#010+
  'endif'#013#010+
  #013#010+
  '# OS/2'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'BATCHEXT=.cmd'#013#010+
  'AOUTEXT=.out'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=os2'#013#010+
  'ECHO=echo'#013#010+
  'IMPORTLIBPREFIX='#013#010+
  'endif'#013#010+
  #013#010+
  '# EMX'#013#010,
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
  '# Amiga'#013#010+
  'ifeq ($(OS_TARGET),amiga)'#013#010+
  'EXEEXT='#013#010+
  'SHAREDLIBEXT=.library'#013#010+
  'SHORTSUFFIX=amg'#013#010+
  'endif'#013#010+
  #013#010,
  '# AROS'#013#010+
  'ifeq ($(OS_TARGET),aros)'#013#010+
  'EXEEXT='#013#010+
  'SHAREDLIBEXT=.library'#013#010+
  'SHORTSUFFIX=aros'#013#010+
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
  'EXEEXT=.ttp'#013#010,
  'SHORTSUFFIX=ata'#013#010+
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
  'BATCHEX','T=.sh'#013#010+
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
  'endi','f'#013#010+
  #013#010+
  '# Netware libc'#013#010+
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
  'IMPORTLIBPREFIX=im','p'#013#010+
  'endif'#013#010+
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
  'en','dif'#013#010+
  #013#010+
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
  'SHAR','EDLIBEXT=.so'#013#010+
  'SHORTSUFFIX=wii'#013#010+
  'endif'#013#010+
  #013#010+
  '# AIX'#013#010+
  'ifeq ($(OS_TARGET),aix)'#013#010+
  'BATCHEXT=.sh'#013#010+
  'EXEEXT='#013#010+
  'SHAREDLIBEXT=.a'#013#010+
  'SHORTSUFFIX=aix'#013#010+
  'endif'#013#010+
  #013#010+
  '# Java'#013#010+
  'ifeq ($(OS_TARGET),java)'#013#010+
  'OEXT=.class'#013#010+
  'ASMEXT=.j'#013#010+
  'SHAREDLIBEXT=.jar'#013#010+
  'SHORTSUFFIX=java'#013#010+
  'endif'#013#010,
  #013#010+
  '# Android java'#013#010+
  'ifeq ($(CPU_TARGET),jvm)'#013#010+
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
  'SHORTSUFFIX=d16',#013#010+
  'endif'#013#010+
  #013#010+
  '# MSX-DOS'#013#010+
  'ifeq ($(OS_TARGET),msxdos)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHORTSUFFIX=msd'#013#010+
  'endif'#013#010+
  #013#010+
  '# Embedded'#013#010+
  'ifeq ($(OS_TARGET),embedded)'#013#010+
  'ifeq ($(CPU_TARGET),i8086)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'STATICLIBEXT=.a'#013#010+
  'else'#013#010+
  'EXEEXT=.bin'#013#010+
  'endif',#013#010+
  'ifeq ($(CPU_TARGET),z80)'#013#010+
  'OEXT=.rel'#013#010+
  'endif'#013#010+
  'SHORTSUFFIX=emb'#013#010+
  'endif'#013#010+
  #013#010+
  '# Win16'#013#010+
  'ifeq ($(OS_TARGET),win16)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'SHORTSUFFIX=w16'#013#010+
  'endif'#013#010+
  #013#010+
  '# ZX Spectrum'#013#010+
  'ifeq ($(OS_TARGET),zxspectrum)'#013#010+
  'OEXT=','.rel'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# For 8.3 limited OS'#039's the short suffixes'#013#010+
  '# Otherwise use the full source/target names'#013#010+
  'ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)'#013#010+
  'FPCMADE=fpcmade.$(SHORTSUFFIX)'#013#010+
  'ZIPSUFFIX=$(SHORTSUFFIX)'#013#010+
  'ZIPCROSSPREFIX='#013#010+
  'ZIPSOURCE','SUFFIX=src'#013#010+
  'ZIPEXAMPLESUFFIX=exm'#013#010+
  'else'#013#010+
  'FPCMADE=fpcmade.$(TARGETSUFFIX)'#013#010+
  'ZIPSOURCESUFFIX=.source'#013#010+
  'ZIPEXAMPLESUFFIX=.examples'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'ZIPSUFFIX=.$(SOURCESUFFIX)'#013#010+
  'ZIPCROSSPREFIX=$(TARGETSUFFIX)-'#013#010+
  'else'#013#010+
  'ZIPSUFFIX=.$(TARGETSUFFIX)'#013,#010+
  'ZIPCROSSPREFIX='#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[defaulttools]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default Tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Names of the binutils tools',#013#010+
  'ASNAME=$(BINUTILSPREFIX)as'#013#010+
  'LDNAME=$(BINUTILSPREFIX)ld'#013#010+
  'ARNAME=$(BINUTILSPREFIX)ar'#013#010+
  'RCNAME=$(BINUTILSPREFIX)rc'#013#010+
  'NASMNAME=$(BINUTILSPREFIX)nasm'#013#010+
  #013#010+
  '# assembler, redefine it if cross compiling'#013#010+
  'ifndef ASPROG'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'ASPROG=$(CROS','SBINDIR)/$(ASNAME)$(SRCEXEEXT)'#013#010+
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
  '# Resource compi','ler'#013#010+
  'ifndef RCPROG'#013#010+
  'ifdef CROSSBINDIR'#013#010+
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
  'e','ndif'#013#010+
  'endif'#013#010+
  #013#010+
  '# NASM assembler, redefine it if cross compiling'#013#010+
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
  'LD=$(LDPROG',')'#013#010+
  'RC=$(RCPROG)'#013#010+
  'AR=$(ARPROG)'#013#010+
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
  'end','if'#013#010+
  #013#010+
  'ifdef DATE'#013#010+
  'DATESTR:=$(shell $(DATE) +%Y%m%d)'#013#010+
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
  '[command_beg','in]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Compiler Command Line'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Load commandline OPTDEF and add FPC_CPU define, for compiling th','e'#013+
  #010+
  '# compiler this needs to be turned off'#013#010+
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
  'i','fneq ($(CPU_TARGET),$(CPU_SOURCE))'#013#010+
  'override FPCOPT+=-P$(ARCH)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(OS_SOURCE),openbsd)'#013#010+
  'override FPCOPT+=-FD$(NEW_BINUTILS_PATH)'#013#010+
  'override FPCMAKEOPT+=-FD$(NEW_BINUTILS_PATH)'#013#010+
  'override FPMAKE_BUILD_OPT+=-FD$(NEW_BINUTILS_PATH)',#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef CROSSBOOTSTRAP'#013#010+
  'ifneq ($(BINUTILSPREFIX),)'#013#010+
  'override FPCOPT+=-XP$(BINUTILSPREFIX)'#013#010+
  'endif'#013#010+
  'ifneq ($(BINUTILSPREFIX),)'#013#010+
  'override FPCOPT+=-Xr$(RLINKPATH)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# When BINUTILSPREFIX is given and we are not cross-co','mpiling then u'+
  'se'#013#010+
  '# it while compiling the fpmake file. (For example to build i386-freeb'+
  'sd'#013#010+
  '# with BINUTILSPREFIX=i386-)'#013#010+
  'ifndef CROSSCOMPILE'#013#010+
  'ifneq ($(BINUTILSPREFIX),)'#013#010+
  'override FPCMAKEOPT+=-XP$(BINUTILSPREFIX)'#013#010+
  'override FPMAKE_BUILD_OPT+','=-XP$(BINUTILSPREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# User dirs should be first, so they are looked at first'#013#010+
  'ifdef UNITDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fu,$(UNITDIR))'#013#010+
  'endif'#013#010+
  'ifdef LIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#013#010+
  'endif'#013#010+
  'ifdef OBJ','DIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDIR))'#013#010+
  'endif'#013#010+
  'ifdef INCDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fi,$(INCDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Smartlinking'#013#010+
  'ifdef LINKSMART'#013#010+
  'override FPCOPT+=-XX'#013#010+
  'endif'#013#010+
  #013#010+
  '# Smartlinking creation'#013#010+
  'ifdef CREATESMART'#013#010+
  'overrid','e FPCOPT+=-CX'#013#010+
  'endif'#013#010+
  #013#010+
  '# Debug'#013#010+
  'ifdef DEBUG'#013#010+
  'override FPCOPT+=-gl'#013#010+
  'override FPCOPTDEF+=DEBUG'#013#010+
  'endif'#013#010+
  #013#010+
  '# Release mode'#013#010+
  '# (strip, optimize and don'#039't load fpc.cfg)'#013#010+
  'ifdef RELEASE'#013#010+
  'FPCCPUOPT:=-O2'#013#010+
  'override FPCOPT+=-Ur -Xs $(FPCCPUOPT) -n'#013#010+
  'ove','rride FPCOPTDEF+=RELEASE'#013#010+
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
  '# Verbose settings (warning,note,info)'#013#010+
  'ifdef VERBOSE'#013#010+
  'override FPCOPT+=-vwni'#013#010+
  'endif'#013#010+
  #013#010+
  '# Ne','eded compiler options'#013#010+
  'ifdef COMPILER_OPTIONS'#013#010+
  'override FPCOPT+=$(COMPILER_OPTIONS)'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_UNITDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fu,$(COMPILER_UNITDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_LIBRARYDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(','COMPILER_LIBRARYDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_OBJECTDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_INCLUDEDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fi,$(COMPILER_INCLUDEDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross compiler utils'#013#010+
  'if','def CROSSBINDIR'#013#010+
  'override FPCOPT+=-FD$(CROSSBINDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Target dirs and the prefix to use for clean/install'#013#010+
  'ifdef COMPILER_TARGETDIR'#013#010+
  'override FPCOPT+=-FE$(COMPILER_TARGETDIR)'#013#010+
  'ifeq ($(COMPILER_TARGETDIR),.)'#013#010+
  'override TARGETDIRPREF','IX='#013#010+
  'else'#013#010+
  'override TARGETDIRPREFIX=$(COMPILER_TARGETDIR)/'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Unit target dir (default is units/<cpu>-<os>/'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  'override FPCOPT+=-FU$(COMPILER_UNITTARGETDIR)'#013#010+
  'ifeq ($(COMPILER_UNITTARGETDIR),.)'#013#010+
  'ov','erride UNITTARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override UNITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef COMPILER_TARGETDIR'#013#010+
  'override COMPILER_UNITTARGETDIR=$(COMPILER_TARGETDIR)'#013#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)'#013#010+
  'endif',#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CREATESHARED'#013#010+
  'override FPCOPT+=-Cg'#013#010+
  'endif'#013#010+
  #013#010+
  '# create always pic'#039'ed code on x86_64, mips and mipsel'#013#010+
  '# on unix-like systems'#013#010+
  'ifneq ($(findstring $(OS_TARGET),dragonfly freebsd openbsd netbsd linu'+
  'x solaris),)'#013#010+
  'ifneq ($(finds','tring $(CPU_TARGET),x86_64 mips mipsel),)'#013#010+
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
  'override FPCOPT+=-F','l$(GCCLIBDIR)'#013#010+
  'ifdef FPCMAKEGCCLIBDIR'#013#010+
  'override FPCMAKEOPT+=-Fl$(FPCMAKEGCCLIBDIR)'#013#010+
  'else'#013#010+
  'override FPCMAKEOPT+=-Fl$(GCCLIBDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef OTHERLIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_end]'#013#010+
  '# A','dd commandline options last so they can override'#013#010+
  'ifdef OPT'#013#010+
  'override FPCOPT+=$(OPT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Override options to compile the fpmake-binary with command-line opti'+
  'ons provided in FPMAKEBUILDOPT'#013#010+
  'ifdef FPMAKEBUILDOPT'#013#010+
  'override FPMAKE_BUILD','_OPT+=$(FPMAKEBUILDOPT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Add defines from FPCOPTDEF to FPCOPT'#013#010+
  'ifdef FPCOPTDEF'#013#010+
  'override FPCOPT+=$(addprefix -d,$(FPCOPTDEF))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Was a config file specified ?'#013#010+
  'ifdef CFGFILE'#013#010+
  'override FPCOPT+=@$(CFGFILE)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Use ','the environment to pass commandline options?'#013#010+
  'ifdef USEENV'#013#010+
  'override FPCEXTCMD:=$(FPCOPT)'#013#010+
  'override FPCOPT:=!FPCEXTCMD'#013#010+
  'export FPCEXTCMD'#013#010+
  'endif'#013#010+
  #013#010+
  'override AFULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)'#013#010+
  'override AFULL_SOURCE=$(CPU_SOURCE)-$(OS_SO','URCE)'#013#010+
  #013#010+
  'ifneq ($(AFULL_TARGET),$(AFULL_SOURCE))'#013#010+
  'override ACROSSCOMPILE=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef ACROSSCOMPILE'#013#010+
  'override FPCOPT+=$(CROSSOPT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Compiler commandline'#013#010+
  'override COMPILER:=$(strip $(FPC) $(FPCOPT))'#013#010+
  #013#010+
  '# also call ppas if wit','h command option -s'#013#010+
  '# but only if the FULL_SOURCE and FULL_TARGET are equal'#013#010+
  '# or if -sh is used'#013#010+
  'ifneq (,$(findstring -sh ,$(COMPILER)))'#013#010+
  'UseEXECPPAS=1'#013#010+
  'endif'#013#010+
  'ifneq (,$(findstring -s ,$(COMPILER)))'#013#010+
  'ifeq ($(FULL_SOURCE),$(FULL_TARGET))'#013#010,
  'UseEXECPPAS=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifneq ($(UseEXECPPAS),1)'#013#010+
  'EXECPPAS='#013#010+
  'else'#013#010+
  'ifdef RUNBATCH'#013#010+
  'EXECPPAS:=@$(RUNBATCH) $(PPAS)'#013#010+
  'else'#013#010+
  'EXECPPAS:=@$(PPAS)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[loaderrules]'#013#010+
  '#############################################################','#######'+
  '#'#013#010+
  '# Loaders'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_loaders'#013#010+
  #013#010+
  'ifneq ($(TARGET_LOADERS),)'#013#010+
  'override ALLTARGET+=fpc_loaders'#013#010+
  'override CLEANTARGET+=fpc_loaders_clean'#013#010+
  'override INSTALLTARGE','T+=fpc_loaders_install'#013#010+
  #013#010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(TARGET_LOADERS))'#013#010+
  'endif'#013#010+
  #013#010+
  '%$(OEXT): %$(LOADEREXT)'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  '        $(AS) -o $(COMPILER_UNITTARGETDIR)/$*$(OEXT) $<'#013#010+
  'else'#013#010+
  '        $(AS) -o $*$(OEX','T) $<'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_loaders: $(COMPILER_UNITTARGETDIR) $(LOADEROFILES)'#013#010+
  #013#010+
  'fpc_loaders_clean:'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  '        -$(DEL) $(addprefix $(COMPILER_UNITTARGETDIR)/,$(LOADEROFILES)'+
  ')'#013#010+
  'else'#013#010+
  '        -$(DEL) $(LOADEROFILES)'#013#010+
  'endi','f'#013#010+
  #013#010+
  'fpc_loaders_install:'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  '        $(INSTALL) $(addprefix $(COMPILER_UNITTARGETDIR)/,$(LOADEROFIL'+
  'ES)) $(INSTALL_UNITDIR)'#013#010+
  'else'#013#010+
  '        $(INSTALL) $(LOADEROFILES) $(INSTALL_','UNITDIR)'#013#010+
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
  '.PHONY: fpc_units'#013#010+
  #013#010+
  'ifneq ($(TARGET_UNITS)$(TARGET_I','MPLICITUNITS),)'#013#010+
  'override ALLTARGET+=fpc_units'#013#010+
  #013#010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))'#013#010+
  'override IMPLICITUNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_IMPLICITU'+
  'NITS))'#013#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES) $(IMPLIC','ITUNITPPUFILES)'#013+
  #010+
  'override CLEANPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_units: $(COMPILER_UNITTARGETDIR) $(UNITPPUFILES)'#013#010+
  #013#010+
  #013#010+
  '[exerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Exes'#013#010,
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_exes'#013#010+
  #013#010+
  '# Programs are not needed for a cross installation'#013#010+
  'ifndef CROSSINSTALL'#013#010+
  'ifneq ($(TARGET_PROGRAMS),)'#013#010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(TAR','GET_PROGRAMS))'#013#010+
  'override EXEOFILES:=$(addsuffix $(OEXT),$(TARGET_PROGRAMS)) $(addsuffi'+
  'x $(LTOEXT),$(TARGET_PROGRAMS))$(addprefix $(STATICLIBPREFIX),$(addsuf'+
  'fix $(STATICLIBEXT),$(TARGET_PROGRAMS))) $(addprefix $(IMPORTLIBPREFIX'+
  '),$(addsuffix ','$(STATICLIBEXT),$(TARGET_PROGRAMS)))'#013#010+
  'override EXEDBGFILES:=$(addsuffix $(EXEDBGEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  #013#010+
  'override ALLTARGET+=fpc_exes'#013#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#013#010+
  'override CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#013#010+
  'override CLEA','NEXEDBGFILES+=$(EXEDBGFILES)'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'end','if'#013#010+
  #013#010+
  'fpc_exes: $(COMPILER_TARGETDIR) $(COMPILER_UNITTARGETDIR) $(EXEFILES)'#013+
  #010+
  #013#010+
  #013#010+
  '[rstrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Resource strings'#013#010+
  '########################################################','############'+
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
  '# Examples',#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_examples'#013#010+
  #013#010+
  'ifneq ($(TARGET_EXAMPLES),)'#013#010+
  'HASEXAMPLES=1'#013#010+
  'override EXAMPLESOURCEFILES=$(wildcard $(addsuffix .pp,$(TARGET_EXAMPL'+
  'ES)) $(addsuffix .pas,$(TAR','GET_EXAMPLES)) $(addsuffix .lpr,$(TARGET_'+
  'EXAMPLES)) $(addsuffix .dpr,$(TARGET_EXAMPLES)))'#013#010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),$(TARGET_EXAMPLES))'#013#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(TARGET_EXAMPLES)) $(adds'+
  'uffix $(LTOEX','T),$(TARGET_EXAMPLES)) $(addprefix $(STATICLIBPREFIX),$'+
  '(addsuffix $(STATICLIBEXT),$(TARGET_EXAMPLES))) $(addprefix $(IMPORTLI'+
  'BPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_EXAMPLES)))'#013#010+
  'override EXAMPLEDBGFILES:=$(addsuffix $(EXEDBGEXT),$(TAR','GET_EXAMPLES'+
  '))'#013#010+
  #013#010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#013#010+
  'override CLEANEXEDBGFILES+=$(EXAMPLEDBGFILES)'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#013+
  #010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET','),emx)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#013+
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
  '[compilerules]'#013,#010+
  '#####################################################################'#013+
  #010+
  '# General compile rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_all fpc_smart fpc_debug fpc_release fpc_shared'#013#010+
  #013#010+
  '$(FPCMAD','E): $(ALLDEPENDENCIES) $(ALLTARGET)'#013#010+
  '        @$(ECHOREDIR) Compiled > $(FPCMADE)'#013#010+
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
  '        $(MAKE)',' all RELEASE=1'#013#010+
  #013#010+
  '# General compile rules, available for both possible .pp and .pas exte'+
  'nsions'#013#010+
  #013#010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) $(LTOEXT) .pas .lpr .dpr .pp .r'+
  'c .res'#013#010+
  #013#010+
  '$(COMPILER_UNITTARGETDIR):'#013#010+
  '        $(MKDIRTREE) $(COMPILER_UN','ITTARGETDIR)'#013#010+
  #013#010+
  '$(COMPILER_TARGETDIR):'#013#010+
  '        $(MKDIRTREE) $(COMPILER_TARGETDIR)'#013#010+
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
  '     ','   $(COMPILER) $<'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEXT): %.pas'#013#010+
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
  #013#010,
  '%.res: %.rc'#013#010+
  '        windres -i $< -o $@'#013#010+
  #013#010+
  '# Search paths for .ppu, .pp, .pas, .lpr, .dpr'#013#010+
  'vpath %.pp $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.lpr $(COMPILER_SOURCEDIR)',' $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.dpr $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.inc $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %$(OEXT) $(COMPILER_UNITTARGETDIR)'#013#010+
  'vpath %$(LTOEXT) $(COMPILER_UNITTARGETDIR)'#013#010+
  'vpath %$(PPUEXT) $(COMPILER_UNITTAR','GETDIR)'#013#010+
  #013#010+
  '[sharedrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Library'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_shared'#013#010+
  #013#010+
  'override INSTALLTARGET+=fpc_shared_in','stall'#013#010+
  #013#010+
  'ifndef SHARED_LIBVERSION'#013#010+
  'SHARED_LIBVERSION=$(FPC_VERSION)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef SHARED_LIBNAME'#013#010+
  'SHARED_LIBNAME=$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifndef SHARED_FULLNAME'#013#010+
  'SHARED_FULLNAME=$(SHAREDLIBPREFIX)$(SHARED_LIBNAME)-$(SHARED_LIBVERSIO'+
  'N)$(','SHAREDLIBEXT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default sharedlib units are all unit objects'#013#010+
  'ifndef SHARED_LIBUNITS'#013#010+
  'SHARED_LIBUNITS:=$(TARGET_UNITS) $(TARGET_IMPLICITUNITS)'#013#010+
  'override SHARED_LIBUNITS:=$(filter-out $(INSTALL_BUILDUNIT),$(SHARED_L'+
  'IBUNITS))'#013#010+
  'endif',#013#010+
  #013#010+
  'fpc_shared:'#013#010+
  'ifdef HASSHAREDLIB'#013#010+
  '        $(MAKE) all CREATESHARED=1 LINKSHARED=1 CREATESMART=1'#013#010+
  'ifneq ($(SHARED_BUILD),n)'#013#010+
  '        $(PPUMOVE) -q $(SHARED_LIBUNITS) -i$(COMPILER_UNITTARGETDIR) -'+
  'o$(SHARED_FULLNAME) -d$(COMPILER_UNITTARGET','DIR) -P$(BINUTILSPREFIX)'#013+
  #010+
  'endif'#013#010+
  'else'#013#010+
  '        @$(ECHO) Shared Libraries not supported'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_shared_install:'#013#010+
  'ifneq ($(SHARED_BUILD),n)'#013#010+
  'ifneq ($(SHARED_LIBUNITS),)'#013#010+
  'ifneq ($(wildcard $(COMPILER_UNITTARGETDIR)/$(SHARED_FULLNAME)),)'#013,
  #010+
  '        $(INSTALL) $(COMPILER_UNITTARGETDIR)/$(SHARED_FULLNAME) $(INST'+
  'ALL_SHAREDDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[installrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Install rules'#013#010+
  '##########################','##########################################'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_install fpc_sourceinstall fpc_exampleinstall'#013#010+
  #013#010+
  'ifdef INSTALL_UNITS'#013#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(INSTALL_UNITS))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALL_BUILDUNIT'#013#010+
  'override IN','STALLPPUFILES:=$(filter-out $(INSTALL_BUILDUNIT)$(PPUEXT)'+
  ',$(INSTALLPPUFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  '# Avoid getting the same name twice as some install EXE fail'#013#010+
  '# in this case, happends for instance for OS/2 target'#013#010+
  'ifneq ($(IMP','ORTLIBPREFIX)-$(STATICLIBEXT),$(STATICLIBPREFIX)-$(STATI'+
  'CLIBEXT))'#013#010+
  'override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFI'+
  'LES)) $(subst $(PPUEXT),$(LTOEXT),$(INSTALLPPUFILES)) $(addprefix $(ST'+
  'ATICLIBPREFIX),$(subst $(PPUEXT),','$(STATICLIBEXT),$(INSTALLPPUFILES))'+
  ') $(addprefix $(IMPORTLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(I'+
  'NSTALLPPUFILES)))'#013#010+
  'else'#013#010+
  'override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFI'+
  'LES)) $(subst $(PPUEXT),$(LTOEXT),$(INST','ALLPPUFILES)) $(addprefix $('+
  'STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(INSTALLPPUFILES))'+
  ')'#013#010+
  'endif'#013#010+
  #013#010+
  'ifneq ($(UNITTARGETDIRPREFIX),)'#013#010+
  'override INSTALLPPUFILENAMES:=$(notdir $(INSTALLPPUFILES))'#013#010+
  'override INSTALLPPULINKFILENAMES:=$(n','otdir $(INSTALLPPULINKFILES))'#013+
  #010+
  'override INSTALLPPUFILES=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTALLP'+
  'PUFILENAMES))'#013#010+
  'override INSTALLPPULINKFILES=$(wildcard $(addprefix $(UNITTARGETDIRPRE'+
  'FIX),$(INSTALLPPULINKFILENAMES)))'#013#010+
  'endif'#013#010+
  '# Implicitly',' install Package.fpc'#013#010+
  'override INSTALL_CREATEPACKAGEFPC=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  'ifneq ($(TARGETDIRPREFIX),)'#013#010+
  'override INSTALLEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(notdir $(IN'+
  'STALLEXEFILES)))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_install: all',' $(INSTALLTARGET)'#013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  '        $(MKDIR) $(INSTALL_BINDIR)'#013#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_CREATEPACKAGEFPC'#013#010+
  'ifdef FPCMAKE'#013#010+
  '# If the fpcpackage variable is set then create ','and install Package.'+
  'fpc,'#013#010+
  '# a safety check is done if Makefile.fpc is available'#013#010+
  'ifdef PACKAGE_VERSION'#013#010+
  'ifneq ($(wildcard Makefile.fpc),)'#013#010+
  '        $(FPCMAKE) -p -T$(CPU_TARGET)-$(OS_TARGET) Makefile.fpc'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013,#010+
  '        $(INSTALL) Package.fpc $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(INSTALL_UNITDIR)'#013#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#013#010+
  '        $','(INSTALL) $(INSTALLPPULINKFILES) $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'ifneq ($(wildcard $(LIB_FULLNAME)),)'#013#010+
  '        $(MKDIR) $(INSTALL_LIBDIR)'#013#010+
  '        $(INSTALL) $(LIB_FULLNAME) $(INSTALL_LIBDIR)'#013#010+
  'ifdef inUnix'#013#010+
  '        ln -sf $(LIB_FULLNAME) $(INSTA','LL_LIBDIR)/$(LIB_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FILES'#013#010+
  '        $(MKDIR) $(INSTALL_DATADIR)'#013#010+
  '        $(INSTALL) $(INSTALL_FILES) $(INSTALL_DATADIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_sourceinstall: distclean'#013#010+
  '        $(MKDIR) $(INSTALL_SOURCEDIR)'#013#010+
  '   ','     $(COPYTREE) $(BASEDIR)/* $(INSTALL_SOURCEDIR)'#013#010+
  #013#010+
  'fpc_exampleinstall: $(EXAMPLEINSTALLTARGET) $(addsuffix _distclean,$(T'+
  'ARGET_EXAMPLEDIRS))'#013#010+
  'ifdef HASEXAMPLES'#013#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDIR)'#013#010+
  'endif'#013#010+
  'ifdef EXAMPLESOURCEFILES'#013#010+
  '   ','     $(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)'#013#010+
  'endif'#013#010+
  'ifdef TARGET_EXAMPLEDIRS'#013#010+
  '        $(COPYTREE) $(addsuffix /*,$(TARGET_EXAMPLEDIRS)) $(INSTALL_EX'+
  'AMPLEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '[distinstallrules]'#013#010+
  '#######################################','#############################'+
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
  '############################','########################################'+
  '#'#013#010+
  '# Zip'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_zipinstall fpc_zipsourceinstall fpc_zipexampleinstall'#013#010+
  #013#010+
  '# Temporary path to pack a file, can only use a ','single deep'#013#010+
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
  'ifnd','ef ZIPNAME'#013#010+
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
  'ifdef DIST_','ZIPTARGET'#013#010+
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
  'USEZIPWRAPPER=','1'#013#010+
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
  '# Create',' commands to create the zip/tar file'#013#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(ZIPPATHSEP),$(PACKDIR))'#013#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(ZIPPATHSEP),$(BASEDIR))'#013#010+
  'ifdef USETAR'#013#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(FULLZIPNAME)$(TAREXT)'#013#010+
  'ZIPCMD_ZIP:=$(TARPROG) c','$(TAROPT)f $(ZIPDESTFILE) *'#013#010+
  'else'#013#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(FULLZIPNAME)$(ZIPEXT)'#013#010+
  'ZIPCMD_ZIP:=$(subst /,$(ZIPPATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDES'+
  'TFILE) *'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_zipinstall:'#013#010+
  '        $(MAKE) $(ZIPTARGET) INSTALL_PREFIX=$','(PACKDIR) ZIPINSTALL=1'#013+
  #010+
  '        $(MKDIR) $(DIST_DESTDIR)'#013#010+
  '        $(DEL) $(ZIPDESTFILE)'#013#010+
  'ifdef USEZIPWRAPPER'#013#010+
  '# Handle gecho separate as we need to espace \ with \\'#013#010+
  'ifneq ($(ECHOREDIR),echo)'#013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_','CDPACK))" > $(ZIPWRAP'+
  'PER)'#013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_ZIP))" >> $(ZIPWRAPPER)'+
  #013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDBASE))" >> $(ZIPWRAPP'+
  'ER)'#013#010+
  'else'#013#010+
  '        echo $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#013#010+
  '        echo ','$(ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#013#010+
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
  '        ','$(DEL) $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  '        $(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(ZIPCMD_CDBASE)'#013#010+
  'endif'#013#010+
  '        $(DELTREE) $(PACKDIR)'#013#010+
  #013#010+
  'fpc_zipsourceinstall:'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=$(ZIP'+
  'SOURCESUFFIX)'#013#010+
  #013#010+
  'f','pc_zipexampleinstall:'#013#010+
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
  '#########','###########################################################'+
  '#'#013#010+
  '# Clean rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#013#010+
  #013#010+
  'ifdef EXEFILES'#013#010+
  'override CLEANEXEFILES:=$(','addprefix $(TARGETDIRPREFIX),$(CLEANEXEFIL'+
  'ES))'#013#010+
  'override CLEANEXEDBGFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEDB'+
  'GFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEAN_PROGRAMS'#013#010+
  'override CLEANEXEFILES+=$(addprefix $(TARGETDIRPREFIX),$(addsuffix $(E'+
  'XEEXT), $(C','LEAN_PROGRAMS)))'#013#010+
  'override CLEANEXEDBGFILES+=$(addprefix $(TARGETDIRPREFIX),$(addsuffix '+
  '$(EXEDBGEXT), $(CLEAN_PROGRAMS)))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEAN_UNITS'#013#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEANPPUFIL','ES'#013#010+
  'override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)'+
  ') $(subst $(PPUEXT),$(LTOEXT),$(CLEANPPUFILES)) $(addprefix $(STATICLI'+
  'BPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLEANPPUFILES))) $(addpre'+
  'fix $(IMPORTLIBPREFIX),$(s','ubst $(PPUEXT),$(STATICLIBEXT),$(CLEANPPUF'+
  'ILES)))'#013#010+
  'ifdef DEBUGSYMEXT'#013#010+
  'override CLEANPPULINKFILES+=$(subst $(PPUEXT),$(DEBUGSYMEXT),$(CLEANPP'+
  'UFILES))'#013#010+
  'endif'#013#010+
  'override CLEANPPUFILENAMES:=$(CLEANPPUFILES)'#013#010+
  'override CLEANPPUFILES=$(addprefix $(','UNITTARGETDIRPREFIX),$(CLEANPPU'+
  'FILENAMES))'#013#010+
  'override CLEANPPULINKFILENAMES:=$(CLEANPPULINKFILES)'#013#010+
  'override CLEANPPULINKFILES=$(wildcard $(addprefix $(UNITTARGETDIRPREFI'+
  'X),$(CLEANPPULINKFILENAMES)))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_clean: $(CLEANTARGET)'#013#010+
  'ifdef',' CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  '# DELTREE instead of DEL because on Mac OS X these are directories'#013+
  #010+
  'ifdef CLEANEXEDBGFILES'#013#010+
  '        -$(DELTREE) $(CLEANEXEDBGFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANPPUFILES'#013#010+
  '        -$(DEL) $(CLEA','NPPUFILES)'#013#010+
  'endif'#013#010+
  'ifneq ($(CLEANPPULINKFILES),)'#013#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANRSTFILES'#013#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#013+
  #010+
  'endif'#013#010+
  'ifdef CLEAN_FILES'#013#010+
  '        -$(DEL) $(CLEAN_FILE','S)'#013#010+
  'endif'#013#010+
  'ifdef LIB_NAME'#013#010+
  '        -$(DEL) $(LIB_NAME) $(LIB_FULLNAME)'#013#010+
  'endif'#013#010+
  '        -$(DEL) $(FPCMADE) *$(FULL_TARGET).fpm Package.fpc *$(ASMEXT)'#013+
  #010+
  '        -$(DEL) $(FPCEXTFILE) $(REDIRFILE) script*.res link*.res *_scr'+
  'ipt.res *_link.res'#013#010,
  '        -$(DEL) $(PPAS) *_ppas$(BATCHEXT) ppas$(BATCHEXT) ppaslink$(BA'+
  'TCHEXT)'#013#010+
  #013#010+
  'fpc_cleanall: $(CLEANTARGET)'#013#010+
  'ifdef CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  'ifdef CLEANPPUFILES'#013#010+
  '        -$(DEL) ','$(CLEANPPUFILES)'#013#010+
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
  '        -$(DEL)',' $(CLEAN_FILES)'#013#010+
  'endif'#013#010+
  '        -$(DELTREE) units'#013#010+
  '        -$(DELTREE) bin'#013#010+
  '        -$(DEL) *$(OEXT) *$(LTOEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *'+
  '$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#013#010+
  'ifneq ($(PPUEXT),.ppu)'#013#010+
  '        -$(DEL) *.o *.ppu',' *.a'#013#010+
  'endif'#013#010+
  '        -$(DELTREE) *$(SMARTEXT)'#013#010+
  '        -$(DEL) fpcmade.* Package.fpc *.fpm'#013#010+
  '        -$(DEL) $(FPCEXTFILE) $(REDIRFILE) script*.res link*.res *_scr'+
  'ipt.res *_link.res'#013#010+
  '        -$(DEL) $(PPAS) *_ppas$(BATCHEXT) ppas$(BATCHEXT) ','ppaslink$('+
  'BATCHEXT)'#013#010+
  'ifdef AOUTEXT'#013#010+
  '        -$(DEL) *$(AOUTEXT)'#013#010+
  'endif'#013#010+
  'ifdef DEBUGSYMEXT'#013#010+
  '        -$(DEL) *$(DEBUGSYMEXT)'#013#010+
  'endif'#013#010+
  'ifdef LOCALFPMAKEBIN'#013#010+
  '        -$(DEL) $(LOCALFPMAKEBIN)'#013#010+
  '        -$(DEL) $(FPMAKEBINOBJ)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_distcl','ean: cleanall'#013#010+
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
  'override INFORULE','S+=fpc_baseinfo'#013#010+
  #013#010+
  'fpc_baseinfo:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Package info =='#013#010+
  '        @$(ECHO)  Package Name..... $(PACKAGE_NAME)'#013#010+
  '        @$(ECHO)  Package Version.. $(PACKAGE_VERSION)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Co','nfiguration info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  FPC.......... $(FPC)'#013#010+
  '        @$(ECHO)  FPC Version.. $(FPC_VERSION)'#013#010+
  '        @$(ECHO)  Source CPU... $(CPU_SOURCE)'#013#010+
  '        @$(ECHO)  Target CPU... $(CPU_TARGET)'#013#010+
  '        @$(ECHO)  S','ource OS.... $(OS_SOURCE)'#013#010+
  '        @$(ECHO)  Target OS.... $(OS_TARGET)'#013#010+
  '        @$(ECHO)  Full Source.. $(FULL_SOURCE)'#013#010+
  '        @$(ECHO)  Full Target.. $(FULL_TARGET)'#013#010+
  '        @$(ECHO)  SourceSuffix. $(SOURCESUFFIX)'#013#010+
  '        @$(ECHO)  Targ','etSuffix. $(TARGETSUFFIX)'#013#010+
  '        @$(ECHO)  FPC fpmake... $(FPCFPMAKE)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Directory info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Required pkgs... $(REQUIRE_PACKAGES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO) ',' Basedir......... $(BASEDIR)'#013#010+
  '        @$(ECHO)  FPCDir.......... $(FPCDIR)'#013#010+
  '        @$(ECHO)  CrossBinDir..... $(CROSSBINDIR)'#013#010+
  '        @$(ECHO)  UnitsDir........ $(UNITSDIR)'#013#010+
  '        @$(ECHO)  PackagesDir..... $(PACKAGESDIR)'#013#010+
  '        @$(ECH','O)'#013#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#013#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Tools info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  As........ $(AS)'#013#010+
  '        @$(ECHO)  Ld........',' $(LD)'#013#010+
  '        @$(ECHO)  Ar........ $(AR)'#013#010+
  '        @$(ECHO)  Rc........ $(RC)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Mv........ $(MVPROG)'#013#010+
  '        @$(ECHO)  Cp........ $(CPPROG)'#013#010+
  '        @$(ECHO)  Rm........ $(RMPROG)'#013#010+
  '        @$(ECHO)  GIns','tall.. $(GINSTALL)'#013#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#013#010+
  '        @$(ECHO)  Shell..... $(SHELL)'#013#010+
  '        @$(ECHO)  Date...... $(DATE)'#013#010+
  '        @$(ECHO)  FPCMake... $(FPCMAKE)'#013#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#013#010+
  '        @$(ECHO)  Zip..','..... $(ZIPPROG)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Object info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Target Loaders........ $(TARGET_LOADERS)'#013#010+
  '        @$(ECHO)  Target Units.......... $(TARGET_UNITS)'#013#010+
  '        @$(ECHO)  Target Implic','it Units. $(TARGET_IMPLICITUNITS)'#013+
  #010+
  '        @$(ECHO)  Target Programs....... $(TARGET_PROGRAMS)'#013#010+
  '        @$(ECHO)  Target Dirs........... $(TARGET_DIRS)'#013#010+
  '        @$(ECHO)  Target Examples....... $(TARGET_EXAMPLES)'#013#010+
  '        @$(ECHO)  Target E','xampleDirs.... $(TARGET_EXAMPLEDIRS)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Clean Units......... $(CLEAN_UNITS)'#013#010+
  '        @$(ECHO)  Clean Files......... $(CLEAN_FILES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Install Units....... $(INSTALL_UNITS)'#013,#010+
  '        @$(ECHO)  Install Files....... $(INSTALL_FILES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Install info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#013#010+
  '        @$(ECHO)  ZipName.............. $(ZIPNAME)'#013#010+
  '    ','    @$(ECHO)  ZipPrefix............ $(ZIPPREFIX)'#013#010+
  '        @$(ECHO)  ZipCrossPrefix....... $(ZIPCROSSPREFIX)'#013#010+
  '        @$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)'#013#010+
  '        @$(ECHO)  FullZipName.......... $(FULLZIPNAME)'#013#010+
  '        @$(ECHO)  Ins','tall FPC Package.. $(INSTALL_FPCPACKAGE)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)'#013#010+
  '        @$(ECHO)  Install binary dir... $(INSTALL_BINDIR)'#013#010+
  '        @$(ECHO)  Install library dir.. $(INSTALL_LIBDIR)'#013#010+
  '  ','      @$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)'#013#010+
  '        @$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)'#013#010+
  '        @$(ECHO)  Install doc dir...... $(INSTALL_DOCDIR)'#013#010+
  '        @$(ECHO)  Install example dir.. $(INSTALL_EXAMPLEDIR)',#013#010+
  '        @$(ECHO)  Install data dir..... $(INSTALL_DATADIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Dist destination dir. $(DIST_DESTDIR)'#013#010+
  '        @$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[inforules]'#013#010+
  '#############','#######################################################'+
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
  '################################','####################################'+
  '#'#013#010+
  '# Rebuild Makefile'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_makefile fpc_makefiles fpc_makefile_sub1 fpc_makefile_sub2'+
  ' \'#013#010+
  '        fpc_makefile_dirs'#013#010+
  #013#010+
  'fpc_m','akefile:'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) Makefile.fpc'#013#010+
  #013#010+
  'fpc_makefile_sub1:'#013#010+
  'ifdef TARGET_DIRS'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGE'+
  'T_DIRS))'#013#010+
  'endif'#013#010+
  'ifdef TARGET_EXAMPLEDIRS'#013#010+
  '        $(FPCMAKE) -w -T$','(OS_TARGET) $(addsuffix /Makefile.fpc,$(TAR'+
  'GET_EXAMPLEDIRS))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_makefile_sub2: $(addsuffix _makefile_dirs,$(TARGET_DIRS) $(TARGET_'+
  'EXAMPLEDIRS))'#013#010+
  #013#010+
  'fpc_makefile_dirs: fpc_makefile_sub1 fpc_makefile_sub2'#013#010+
  #013#010+
  'fpc_makefiles: fpc_makef','ile fpc_makefile_dirs'#013#010+
  #013#010+
  '[localmakefile]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Local Makefile'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  'ifneq ($(wildcard fpcmake.loc),)'#013#010+
  'in','clude fpcmake.loc'#013#010+
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
  '##########################','##########################################'+
  '#'#013#010+
  '# LCL Rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# LCL Platform'#013#010+
  'ifndef LCL_PLATFORM'#013#010+
  'ifneq ($(findstring $(OS_TARGET),win32 win64),)'#013#010+
  'LCL_PLATFORM=win32'#013#010+
  'els','e'#013#010+
  'LCL_PLATFORM=gtk'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export LCL_PLATFORM'#013#010+
  #013#010+
  '# Check if the specified LCLDIR is correct'#013#010+
  'ifdef LCLDIR'#013#010+
  'override LCLDIR:=$(subst \,/,$(LCLDIR))'#013#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endif'#013#010,
  'else'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if the default LCLDIR is correct'#013#010+
  'ifdef DEFAULT_LCLDIR'#013#010+
  'override LCLDIR:=$(subst \,/,$(DEFAULT_LCLDIR))'#013#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013,#010+
  #013#010+
  '# Check for development version'#013#010+
  'ifeq ($(LCLDIR),wrong)'#013#010+
  'override LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildcard'+
  ' $(addsuffix /units/$(LCL_PLATFORM),$(BASEDIR)/lcl $(BASEDIR)))))'#013#010+
  'ifeq ($(LCLDIR),)'#013#010+
  'override LCLDIR=wrong'#013#010+
  'e','ndif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check for release version'#013#010+
  'ifeq ($(LCLDIR),wrong)'#013#010+
  'override LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildcard'+
  ' $(addsuffix /lib/lazarus/units/$(LCL_PLATFORM),/usr/local /usr))))'#013+
  #010+
  'ifeq ($(LCLDIR),)'#013#010+
  'override LCLDI','R=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Generate dirs'#013#010+
  'override LCLUNITDIR=$(wildcard $(LCLDIR)/units/$(LCL_PLATFORM) $(LCLDI'+
  'R)/units)'#013#010+
  'override LCLCOMPONENTDIR=$(wildcard $(LCLDIR)/.. $(LCLDIR)/../componen'+
  'ts $(LCLDIR)/components)'#013#010+
  'export LCLDIR LCLUNIT','DIR LCLCOMPONENTDIR'#013#010+
  #013#010+
  '# Add LCL dirs to paths'#013#010+
  'override REQUIRE_PACKAGESDIR+=$(LCLCOMPONENTDIR)'#013#010+
  'override COMPILER_UNITDIR+=$(LCLUNITDIR)'#013#010+
  #013#010+
  '[lclinforules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# LCL Info ','rules'#013#010+
  '#####################################################################'#013+
  #010+
  'override INFORULES+=lclinfo'#013#010+
  #013#010+
  '.PHONY: lclinfo'#013#010+
  #013#010+
  'lclinfo:'#013#010+
  '        @$(ECHO)  == LCL info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Platform............. $(LCL_PLAT','FORM)'#013#010+
  '        @$(ECHO)  LCLDIR............... $(LCLDIR)'#013#010+
  '        @$(ECHO)  LCL Unit dir......... $(LCLUNITDIR)'#013#010+
  '        @$(ECHO)  LCL Component dir.... $(LCLCOMPONENTDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[fpmakeprerules]'#013#010+
  '#############################','#######################################'+
  '#'#013#010+
  '# fpmake prerules'#013#010+
  '#####################################################################'#013+
  #010+
  'FPMAKEBIN=fpmake$(SRCEXEEXT)'#013#010+
  'FPMAKEBINOBJ=fpmake$(OEXT) fpmake$(LTOEXT)'#013#010+
  'LOCALFPMAKEBIN=.$(PATHSEP)$(FPMAKE','BIN)'#013#010+
  #013#010+
  '# Convert the OS_TARGET and CPU_TARGET options to fpmake'#039's --os an'+
  'd --cpu parameters'#013#010+
  'ifdef OS_TARGET'#013#010+
  'FPC_TARGETOPT+=--os=$(OS_TARGET)'#013#010+
  'endif'#013#010+
  'ifdef CPU_TARGET'#013#010+
  'FPC_TARGETOPT+=--cpu=$(CPU_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Get the location of the',' bootstrap-fpmkunit units'#013#010+
  'PACKAGEDIR_FPMKUNIT:=$(firstword $(subst /Makefile.fpc,,$(strip $(wild'+
  'card $(addsuffix /fpmkunit/Makefile.fpc,$(PACKAGESDIR))))))'#013#010+
  'ifneq ($(PACKAGEDIR_FPMKUNIT),)'#013#010+
  'UNITDIR_FPMAKE_FPMKUNIT=$(PACKAGEDIR_FPMKUNIT)/un','its_bs/$(SOURCESUFF'+
  'IX)'#013#010+
  'override COMPILER_FPMAKE_UNITDIR=$(UNITDIR_FPMAKE_FPMKUNIT)'#013#010+
  'FPMKUNIT_SRC=$(PACKAGEDIR_FPMKUNIT)/src/fpmkunit.pp'#013#010+
  'FPMKUNIT_PPU=$(UNITDIR_FPMAKE_FPMKUNIT)/fpmkunit.ppu'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef FPMAKE_SKIP_CONFIG'#013#010+
  'override FPMA','KE_BUILD_OPT+=$(FPMAKE_SKIP_CONFIG)'#013#010+
  'endif'#013#010+
  #013#010+
  '[fpmakerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# fpmake rules'#013#010+
  '#####################################################################'#013+
  #010+
  '.PHONY: fpc_fpmake fp','c_fpmake_clean fpc_fpmake_install fpc_fpmake_ex'+
  'ampleinstall'#013#010+
  #013#010+
  '# Do not pass the Makefile'#039's unit and binary target locations. fpm'+
  'ake uses it'#039's own.'#013#010+
  'override FPCOPT:=$(filter-out -FU%,$(FPCOPT))'#013#010+
  'override FPCOPT:=$(filter-out -FE%,$(FPCOPT)',')'#013#010+
  '# Compose general fpmake-parameters'#013#010+
  'ifdef FPMAKEOPT'#013#010+
  'FPMAKE_OPT+=$(FPMAKEOPT)'#013#010+
  'endif'#013#010+
  'FPMAKE_OPT+=--localunitdir=$(FPCDIR)'#013#010+
  'FPMAKE_OPT+=--globalunitdir=$(FPCDIR)/packages'#013#010+
  'FPMAKE_OPT+=$(FPC_TARGETOPT)'#013#010+
  'FPMAKE_OPT+=$(addprefix -o ,$(FPCO','PT))'#013#010+
  'FPMAKE_OPT+=--compiler=$(FPC)'#013#010+
  'FPMAKE_OPT+=-bu'#013#010+
  #013#010+
  'FPMAKE_INSTALL_OPT+=--unitinstalldir=$(INSTALL_UNITDIR)'#013#010+
  'ifdef UNIXHier'#013#010+
  'FPMAKE_INSTALL_OPT+=--prefix=$(INSTALL_PREFIX)'#013#010+
  'FPMAKE_INSTALL_OPT+=--baseinstalldir=$(INSTALL_LIBDIR)/fpc/$(FP','C_VER'+
  'SION)'#013#010+
  'else'#013#010+
  'FPMAKE_INSTALL_OPT+=--prefix=$(INSTALL_BASEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'override ALLTARGET+=fpc_fpmake'#013#010+
  'override INSTALLTARGET+=fpc_fpmake_install'#013#010+
  'override EXAMPLEINSTALLTARGET+=fpc_fpmake_exampleinstall'#013#010+
  '# If no fpmake exists and (dis','t)clean is called, do not try to build'+
  ' fpmake, it will'#013#010+
  '# most often fail because the dependencies are cleared.'#013#010+
  '# In case of a clean, simply do nothing'#013#010+
  'ifneq ($(wildcard $(LOCALFPMAKEBIN)),)'#013#010+
  'override CLEANTARGET+=fpc_fpmake_clean'#013#010+
  'endif'#013,#010+
  #013#010+
  '$(FPMKUNIT_PPU): $(FPMKUNIT_SRC)'#013#010+
  '            $(MAKE) -C $(PACKAGEDIR_FPMKUNIT) bootstrap $(addprefix OP'+
  'T=,$(FPMAKE_BUILD_OPT))'#013#010+
  #013#010+
  '$(FPMAKEBIN): fpmake.pp $(FPMKUNIT_PPU)'#013#010+
  '        $(FPCFPMAKE) fpmake.pp $(addprefix -Fu,$(COMPILER_FPMAKE_','UNI'+
  'TDIR)) $(FPMAKE_BUILD_OPT)'#013#010+
  #013#010+
  'fpc_fpmake: $(FPMAKEBIN)'#013#010+
  '            $(LOCALFPMAKEBIN) compile $(FPMAKE_OPT)'#013#010+
  #013#010+
  'fpc_fpmake_clean: $(FPMAKEBIN)'#013#010+
  '            $(LOCALFPMAKEBIN) clean $(FPMAKE_OPT)'#013#010+
  #013#010+
  'fpc_fpmake_install: $(FPMAKEBIN)'#013#010+
  '       ','     $(LOCALFPMAKEBIN) install $(FPMAKE_OPT) $(FPMAKE_INSTALL'+
  '_OPT)'#013#010+
  #013#010+
  '# This is not completely valid. Exampleinstall should only install the'+
  ' examples, while'#013#010+
  '# fpmake -ie installs everything, including the examples. This also me'+
  'ans that on'#013,#010+
  '# a distinstall fpmake install wil be called twice.'#013#010+
  'fpc_fpmake_exampleinstall: $(FPMAKEBIN)'#013#010+
  '            $(LOCALFPMAKEBIN) install -ie $(FPMAKE_OPT) $(FPMAKE_INSTA'+
  'LL_OPT)'#013#010
);
