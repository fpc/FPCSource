{$ifdef Delphi}
const fpcmakeini : array[0..189] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..189,1..240] of char=(
{$endif Delphi}
  ';'#013#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#013+
  #010+
  ';'#013#010+
  #013#010+
  '[osdetect]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Autodetect OS (Linux or Dos or Windows NT or OS/2)'#013#010+
  '# define inUnix when runnin','g under Unix like environment'#013#010+
  '#               (Linux,FreeBSD,NetBSD,Cygwin)'#013#010+
  '# define inWinNT when running under WinNT'#013#010+
  '# define inOS2 when running under OS/2'#013#010+
  '# define inCygwin when running under Cygwin32'#013#010+
  '################################','####################################'+
  '#'#013#010+
  #013#010+
  '# We need only / in the path also remove the current dir'#013#010+
  'override PATH:=$(subst \,/,$(PATH))'#013#010+
  #013#010+
  '# Determine if we'#039've a unix searchpath using : by looking for a ;'#013+
  #010+
  '# that normally doesn'#039't exists in p','ath names.'#013#010+
  'ifeq ($(findstring ;,$(PATH)),)'#013#010+
  'inUnix=1'#013#010+
  'SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))'#013#010+
  'else'#013#010+
  'SEARCHPATH:=$(subst ;, ,$(PATH))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Search for PWD and determine also if we are under linux'#013#010+
  'PWD:=$(strip $(wildcard $(a','ddsuffix /pwd.exe,$(SEARCHPATH))))'#013#010+
  'ifeq ($(PWD),)'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(SEARCHPATH))))'#013#010+
  'ifeq ($(PWD),)'#013#010+
  '$(error You need the GNU utils package to use this Makefile)'#013#010+
  'else'#013#010+
  'PWD:=$(firstword $(PWD))'#013#010+
  'SRCEXEEXT='#013#010+
  'endif'#013#010,
  'else'#013#010+
  'PWD:=$(firstword $(PWD))'#013#010+
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
  'el','se'#013#010+
  'ifneq ($(findstring cygwin,$(MACHTYPE)),)'#013#010+
  'inCygWin=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Detect BSD, since BSD uses a slightly different directory hierarchy.'+
  #013#010+
  '# Note that maybe Linux Slackware users would also like this, since'#013+
  #010+
  '# Slackwares hierachy is',' closer to BSD than other unices IIRC.'#013#010+
  #013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'BSDhier=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),netbsd)'#013#010+
  'BSDhier=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),openbsd)'#013#010+
  'BSDhier=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# The extension of batch files / scripts'#013#010+
  'ifdef inUnix'#013#010,
  'BATCHEXT=.sh'#013#010+
  'else'#013#010+
  'ifdef inOS2'#013#010+
  'BATCHEXT=.cmd'#013#010+
  'else'#013#010+
  'BATCHEXT=.bat'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Path Separator, the subst trick is necessary for the \ that can'#039't'+
  ' exists'#013#010+
  '# at the end of a line'#013#010+
  'ifdef inUnix'#013#010+
  'PATHSEP=/'#013#010+
  'else'#013#010+
  'PATHSEP:=$(subst /,\,/)'#013,#010+
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
  'ifneq ($(findstr','ing /cygdrive/,$(BASEDIR)),)'#013#010+
  'BASENODIR:=$(patsubst /cygdrive%,%,$(BASEDIR))'#013#010+
  'BASEDRIVE:=$(firstword $(subst /, ,$(BASENODIR)))'#013#010+
  'BASEDIR:=$(subst /cygdrive/$(BASEDRIVE)/,$(BASEDRIVE):/,$(BASEDIR))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'BASEDIR=.'#013#010+
  'endif'#013#010+
  #013#010+
  '# ','Echo is an internal command under OS/2 (and others), so it'#039's a'+
  'lways found'#013#010+
  'ifdef inOS2'#013#010+
  'ifndef ECHO'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))'+
  '))'#013#010+
  'ifeq ($(ECHO),)'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCE','XEEXT),$(SEARCHPATH)'+
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
  '###############################################################','#####'+
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
  '# Try to detect the ppcXXX file to use',' by using "fpc -PB" option'#013+
  #010+
  '# to query for the default ppcXXX the fpc executable tries. When'#013#010+
  '# fpc is not found use ppc386 by default. Also when fpc -PB gives'#013#010+
  '# an error ppc386 will be used.'#013#010+
  'ifndef FPC'#013#010+
  '# check if fpc exists'#013#010+
  'FPCPROG:=$(','strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPAT'+
  'H))))'#013#010+
  'ifneq ($(FPCPROG),)'#013#010+
  'FPCPROG:=$(firstword $(FPCPROG))'#013#010+
  'FPC:=$(shell $(FPCPROG) -PB)'#013#010+
  '# Older fpc executables didn'#039't support it and return'#013#010+
  '# Error: Illegal processor... When fo','und then fallback to ppc386'#013+
  #010+
  'ifneq ($(findstring Error,$(FPC)),)'#013#010+
  'override FPC=ppc386'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# fpc binary not found, fallback to ppc386'#013#010+
  'override FPC=ppc386'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Get a clean executable name'#013#010+
  'override FPC:=$(subst $(SRCE','XEEXT),,$(FPC))'#013#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#013#010+
  #013#010+
  '# FPC version'#013#010+
  'ifndef FPC_VERSION'#013#010+
  '# For 1.1 and up we can use a single compiler call to retrieve'#013#010+
  '# all needed information'#013#010+
  'FPC_COMPILERINFO:=$(shell $(FPC) -iVSPTPSOTO)'#013#010+
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
  '# For 1.0.x we need to use extra calls to retrieve all info'#013#010+
  'ifneq ($(words $(FPC_COMPILERINFO)),5)'#013#010+
  'FPC_COMPILERINFO+=$','(shell $(FPC) -iSP)'#013#010+
  'FPC_COMPILERINFO+=$(shell $(FPC) -iTP)'#013#010+
  'FPC_COMPILERINFO+=$(shell $(FPC) -iSO)'#013#010+
  'FPC_COMPILERINFO+=$(shell $(FPC) -iTO)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Retrieve Target/Source CPU and Target/Source OS'#013#010+
  'ifndef CPU_SOURCE'#013#010+
  'CPU_SOURCE:=$(word 2',',$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef CPU_TARGET'#013#010+
  'CPU_TARGET:=$(word 3,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef OS_SOURCE'#013#010+
  'OS_SOURCE:=$(word 4,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  'ifndef OS_TARGET'#013#010+
  'OS_TARGET:=$(word 5,$(FPC_COMPILERINFO))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Ful','l name of the target, including CPU and OS'#013#010+
  'FULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)'#013#010+
  'FULL_SOURCE=$(CPU_SOURCE)-$(OS_SOURCE)'#013#010+
  #013#010+
  '# Cross compile flag'#013#010+
  'ifneq ($(FULL_TARGET),$(FULL_SOURCE))'#013#010+
  'CROSSCOMPILE=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if the Makefile supp','orts this target, but not'#013#010+
  '# when the make target is to rebuild the makefile'#013#010+
  'ifeq ($(findstring makefile,$(MAKECMDGOALS)),)'#013#010+
  'ifeq ($(findstring $(OS_TARGET),$(MAKEFILETARGETS)),)'#013#010+
  '$(error The Makefile doesn'#039't support target $(OS_TARGET), p','lease'+
  ' run fpcmake first)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'export OS_TARGET OS_SOURCE CPU_TARGET CPU_SOURCE FULL_TARGET FULL_SOUR'+
  'CE CROSSCOMPILE'#013#010+
  #013#010+
  #013#010+
  '[fpcdircheckenv]'#013#010+
  '#####################################################################'#013+
  #010+
  '# FPCDIR Setting'#013#010,
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Test FPCDIR to look if the RTL dir exists'#013#010+
  'ifdef FPCDIR'#013#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPC','DIR=wrong'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default FPCDIR from Makefile.fpc'#013#010+
  'ifdef DEFAULT_FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'override FPCDIR:=$(subst \,/,$(DEFAULT_FPCDIR))'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)',#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[fpcdirdetect]'#013#010+
  '# Detect FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'ifdef inUnix'#013#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION)'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR=/usr/lib/fpc/$(FP','C_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC),$(SEARCHPATH))))))'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR:=$(FPCDI','R)/..'#013#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#013#010+
  'override FPCDIR=c:/pp'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross compiler dir'#013#010+
  'ifndef CROSSDIR'#013#010+
  'CROSSDIR:=$(FPCDIR)/cross/$(FULL_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross target dir'#013#010+
  'ifndef CROSSTAR','GETDIR'#013#010+
  'CROSSTARGETDIR=$(CROSSDIR)/$(FULL_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Units dir, when cross compiling try first the'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'UNITSDIR:=$(wildcard $(CROSSTARGETDIR)/units)'#013#010+
  'ifeq ($(UNITSDIR),)'#013#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_T','ARGET))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_TARGET))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Packages dir'#013#010+
  'PACKAGESDIR:=$(wildcard $(FPCDIR) $(FPCDIR)/packages/base $(FPCDIR)/pa'+
  'ckages/extra)'#013#010+
  #013#010+
  #013#010+
  '[shelltools]'#013#010+
  '######################################','##############################'+
  '#'#013#010+
  '# Shell tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Echo that can redir (must be able run in the default OS shell)'#013#010+
  'ifndef ECHOREDIR'#013#010+
  'ECHOREDIR:=$(subst /,$(PATHSEP),$(E','CHO))'#013#010+
  'endif'#013#010+
  #013#010+
  '# To copy pograms'#013#010+
  'ifndef COPY'#013#010+
  'COPY:=$(CPPROG) -fp'#013#010+
  'endif'#013#010+
  #013#010+
  '# Copy a whole tree'#013#010+
  'ifndef COPYTREE'#013#010+
  'COPYTREE:=$(CPPROG) -rfp'#013#010+
  'endif'#013#010+
  #013#010+
  '# To move pograms'#013#010+
  'ifndef MOVE'#013#010+
  'MOVE:=$(MVPROG) -f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check delete program'#013#010+
  'ifnd','ef DEL'#013#010+
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
  '# To ins','tall programs'#013#010+
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
  'export ECHOREDIR COPY COPYTREE MOVE D','EL DELTREE INSTALL INSTALLEXE M'+
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
  'ifde','f REQUIRE_UNITSDIR'#013#010+
  'override UNITSDIR+=$(REQUIRE_UNITSDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Units dir'#013#010+
  'ifdef REQUIRE_PACKAGESDIR'#013#010+
  'override PACKAGESDIR+=$(REQUIRE_PACKAGESDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '# Linux, netbsd and freebsd use unix dirs with /usr/bin, /usr/lib'#013#010+
  '# When ','zipping use the target as default, when normal install then'#013+
  #010+
  '# use the source os as default'#013#010+
  'ifdef ZIPINSTALL'#013#010+
  '# Zipinstall'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(O','S_TARGET),netbsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),openbsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(OS_TARGET),sunos)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),qnx)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# Normal install'#013#010+
  'ifeq ($(OS_SOU','RCE),linux)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_SOURCE),freebsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_SOURCE),netbsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_SOURCE),openbsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),sunos)'#013#010+
  'UNIXINSTALLDIR','=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),qnx)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
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
  '# set the prefix directory where to ins','tall everything'#013#010+
  'ifndef INSTALL_PREFIX'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
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
  '# Export a','lso INSTALL_SOURCESUBDIR set so it will be'#013#010+
  '# used recursively for all subdirs'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'export INSTALL_FPCSUBDIR'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to place the resulting zip files'#013#010+
  'ifndef DIST_DESTDIR'#013#010+
  'DIST_DESTDIR:=$(BASEDIR)'#013#010+
  'endif'#013#010+
  'expo','rt DIST_DESTDIR'#013#010+
  #013#010+
  #013#010+
  '#####################################################################'#013+
  #010+
  '# Install Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# set the base directory where to install everything',#013#010+
  'ifndef INSTALL_BASEDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFI','X)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the directory where to install the binaries'#013#010+
  'ifndef INSTALL_BINDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_PREFIX)/bin'#013#010+
  'else'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BASEDIR)/bin'#013#010+
  '# for FPC packages install the binaries',' under their os target subdir'+
  #013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(OS_TARGET)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the directory where to install the units.'#013#010+
  'ifndef INSTALL_UNITDIR'#013#010+
  '# If cross compiling install in the cro','ss compile directory'#013#010+
  'ifdef CROSSCOMPILE'#013#010+
  'INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/cross/$(FULL_TARGET)/units'#013#010+
  'else'#013#010+
  'INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/units/$(OS_TARGET)'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef PACKAGE_NAME'#013#010+
  'INSTALL_UNITDIR:=$(','INSTALL_UNITDIR)/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to install shared libraries'#013#010+
  'ifndef INSTALL_LIBDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'INSTALL_LIBDIR:=$(INSTALL_PREFIX)/lib'#013#010+
  'else'#013#010+
  'INSTALL_LIBDIR:=$(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Wh','ere the source files will be stored'#013#010+
  'ifndef INSTALL_SOURCEDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'ifdef BSDhier'#013#010+
  'SRCPREFIXDIR=share/src'#013#010+
  'else'#013#010+
  'SRCPREFIXDIR=src'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFI','X)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSI'+
  'ON)/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/$(P','ACKAGE_NAME)'+
  '-$(PACKAGE_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef INSTALL_FPCSUBDIR'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(INSTALL_FPCSUBDIR)/$(PA'+
  'CKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(PACKAGE','_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the doc files will be stored'#013#010+
  'ifndef INSTALL_DOCDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'ifdef BSDhier'#013#010+
  'DOCPREFIXDIR=share/doc'#013#010+
  'else'#013#010+
  'DOCPREFIXDIR=doc'#013#010+
  'endi','f'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/fpc-$(FPC_VERSION)/$'+
  '(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/$(PACKAGE_NAME)-$(PA'+
  'CKAGE_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef INSTALL_FPCPAC','KAGE'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to install the examples, under linux we use the doc dir'#013#010+
  '# because the copytree command will create a',' subdir itself'#013#010+
  'ifndef INSTALL_EXAMPLEDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'ifdef INSTALL_FPCPACKAGE'#013#010+
  'ifdef BSDhier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/fpc-$(FPC_VERSION'+
  ')/$(PACKAGE_NAME)'#013#010+
  'else'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/d','oc/fpc-$(FPC_VERSION)/example'+
  's/$(PACKAGE_NAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef BSDhier'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/$(PACKAGE_NAME)-$'+
  '(PACKAGE_VERSION)'#013#010+
  'else'#013#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_NAME)-$(PACKAGE_V',
  'ERSION)'#013#010+
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
  '# Where the some extra (data)files will be',' stored'#013#010+
  'ifndef INSTALL_DATADIR'#013#010+
  'INSTALL_DATADIR=$(INSTALL_BASEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '#####################################################################'#013+
  #010+
  '# Cross compile dirs'#013#010+
  '#####################################################################',
  #013#010+
  #013#010+
  'ifdef CROSSCOMPILE'#013#010+
  '# Directory where the cross compile tools are stored.'#013#010+
  '# First check if they are available in FPCDIR. If no targets/ subdir'#013+
  #010+
  '# is found use the targets/ subdir in INSTALL_BASEDIR.'#013#010+
  'ifndef CROSSBINDIR'#013#010+
  'CROSSBINDIR:=$(','wildcard $(CROSSTARGETDIR)/bin/$(FULL_SOURCE))'#013#010+
  'ifeq ($(CROSSBINDIR),)'#013#010+
  'CROSSBINDIR:=$(wildcard $(INSTALL_BASEDIR)/cross/$(FULL_TARGET)/bin/$('+
  'FULL_SOURCE))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'CROSSBINDIR='#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[dirlibc]'#013#010+
  '# On linux, try to find whe','re libgcc.a is.'#013#010+
  'ifdef inUnix'#013#010+
  'ifndef GCCLIBDIR'#013#010+
  'GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '#039'{ print'+
  ' $$4 } '#039'`)'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'ifndef OTHERLIBDIR'#013#010+
  'OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | awk '#039'{',' ORS='+
  '" "; print $1 }'#039')'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),netbsd)'#013#010+
  'OTHERLIBDIR+=/usr/pkg/lib'#013#010+
  'endif'#013#010+
  'export GCCLIBDIR OTHERLIB'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[extensions]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default extensi','ons'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Default needed extensions (Go32v2,Linux)'#013#010+
  'LOADEREXT=.as'#013#010+
  'EXEEXT=.exe'#013#010+
  'PPLEXT=.ppl'#013#010+
  'PPUEXT=.ppu'#013#010+
  'OEXT=.o'#013#010+
  'ASMEXT=.s'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=','.so'#013#010+
  'STATICLIBPREFIX=libp'#013#010+
  'RSTEXT=.rst'#013#010+
  'FPCMADE=fpcmade'#013#010+
  #013#010+
  #013#010+
  'ifeq ($(findstring 1.0.,$(FPC_VERSION)),)'#013#010+
  '# Go32v1'#013#010+
  'ifeq ($(OS_TARGET),go32v1)'#013#010+
  'PPUEXT=.pp1'#013#010+
  'OEXT=.o1'#013#010+
  'ASMEXT=.s1'#013#010+
  'SMARTEXT=.sl1'#013#010+
  'STATICLIBEXT=.a1'#013#010+
  'SHAREDLIBEXT=.so1'#013#010+
  'STATICLIBPR','EFIX='#013#010+
  'FPCMADE=fpcmade.v1'#013#010+
  'PACKAGESUFFIX=v1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Go32v2'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'FPCMADE=fpcmade.dos'#013#010+
  'ZIPSUFFIX=go32'#013#010+
  'endif'#013#010+
  #013#010+
  '# Linux'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.lnx'#013#010+
  'ZI','PSUFFIX=linux'#013#010+
  'endif'#013#010+
  #013#010+
  '# FreeBSD'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.freebsd'#013#010+
  'ZIPSUFFIX=freebsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# NetBSD'#013#010+
  'ifeq ($(OS_TARGET),netbsd)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.netbsd'#013#010+
  'ZIPSUFFIX=n','etbsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# OpenBSD'#013#010+
  'ifeq ($(OS_TARGET),openbsd)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.openbsd'#013#010+
  'ZIPSUFFIX=openbsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# Win32'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'PPUEXT=.ppw'#013#010+
  'OEXT=.ow'#013#010+
  'ASMEXT=.sw'#013#010+
  'SMARTEXT=.slw'#013#010+
  'STATICLIBEXT=.aw'#013#010+
  'SH','AREDLIBEXT=.dll'#013#010+
  'FPCMADE=fpcmade.w32'#013#010+
  'ZIPSUFFIX=w32'#013#010+
  'endif'#013#010+
  #013#010+
  '# OS/2'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'PPUEXT=.ppo'#013#010+
  'ASMEXT=.so2'#013#010+
  'OEXT=.oo2'#013#010+
  'AOUTEXT=.out'#013#010+
  'SMARTEXT=.sl2'#013#010+
  'STATICLIBPREFIX='#013#010+
  'STATICLIBEXT=.ao2'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'FPCMADE=fpcmade.os2'#013#010+
  'ZI','PSUFFIX=emx'#013#010+
  'ECHO=echo'#013#010+
  'endif'#013#010+
  #013#010+
  '# Amiga'#013#010+
  'ifeq ($(OS_TARGET),amiga)'#013#010+
  'EXEEXT='#013#010+
  'PPUEXT=.ppu'#013#010+
  'ASMEXT=.asm'#013#010+
  'OEXT=.o'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=.library'#013#010+
  'FPCMADE=fpcmade.amg'#013#010+
  'endif'#013#010+
  #013#010+
  '# Atari'#013#010+
  'ifeq ($(OS_TARGET),atari)'#013#010+
  'PPUEXT=','.ppu'#013#010+
  'ASMEXT=.s'#013#010+
  'OEXT=.o'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'EXEEXT=.ttp'#013#010+
  'FPCMADE=fpcmade.ata'#013#010+
  'endif'#013#010+
  #013#010+
  '# BeOS'#013#010+
  'ifeq ($(OS_TARGET),beos)'#013#010+
  'PPUEXT=.ppu'#013#010+
  'ASMEXT=.s'#013#010+
  'OEXT=.o'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.be'#013#010+
  'ZIPSUFFIX=be'#013,#010+
  'endif'#013#010+
  #013#010+
  '# Solaris'#013#010+
  'ifeq ($(OS_TARGET),sunos)'#013#010+
  'PPUEXT=.ppu'#013#010+
  'ASMEXT=.s'#013#010+
  'OEXT=.o'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.sun'#013#010+
  'ZIPSUFFIX=sun'#013#010+
  'endif'#013#010+
  #013#010+
  '# QNX'#013#010+
  'ifeq ($(OS_TARGET),qnx)'#013#010+
  'PPUEXT=.ppu'#013#010+
  'ASMEXT=.s'#013#010+
  'OEXT=.o'#013#010+
  'SMARTEXT=','.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.qnx'#013#010+
  'ZIPSUFFIX=qnx'#013#010+
  'endif'#013#010+
  #013#010+
  '# Netware'#013#010+
  'ifeq ($(OS_TARGET),netware)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'PPUEXT=.ppu'#013#010+
  'OEXT=.o'#013#010+
  'ASMEXT=.s'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=.nlm'#013#010+
  'FPCMADE=fpcmade.nw'#013#010+
  'ZIP','SUFFIX=nw'#013#010+
  'EXEEXT=.nlm'#013#010+
  'endif'#013#010+
  #013#010+
  '# MacOS'#013#010+
  'ifeq ($(OS_TARGET),macos)'#013#010+
  'PPUEXT=.ppu'#013#010+
  'ASMEXT=.s'#013#010+
  'OEXT=.o'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.mcc'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '#shorter version for 1.1 and above - no target specific extensio','ns'#013+
  #010+
  '# Go32v1'#013#010+
  'ifeq ($(OS_TARGET),go32v1)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'FPCMADE=fpcmade.v1'#013#010+
  'PACKAGESUFFIX=v1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Go32v2'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'STATICLIBPREFIX='#013#010+
  'FPCMADE=fpcmade.dos'#013#010+
  'ZIPSUFFIX=go32'#013#010+
  'endif'#013#010+
  #013#010+
  '# Linux'#013#010+
  'ifeq ($(OS_TARGET),lin','ux)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.lnx'#013#010+
  'ZIPSUFFIX=linux'#013#010+
  'endif'#013#010+
  #013#010+
  '# FreeBSD'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.freebsd'#013#010+
  'ZIPSUFFIX=freebsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# NetBSD'#013#010+
  'ifeq ($(OS_TARGET),netbsd)'#013#010+
  'EXEEXT=',#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.netbsd'#013#010+
  'ZIPSUFFIX=netbsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# OpenBSD'#013#010+
  'ifeq ($(OS_TARGET),openbsd)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.openbsd'#013#010+
  'ZIPSUFFIX=openbsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# Win32'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'SHAREDLIBEXT=.dll',#013#010+
  'FPCMADE=fpcmade.w32'#013#010+
  'ZIPSUFFIX=w32'#013#010+
  'endif'#013#010+
  #013#010+
  '# OS/2'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'AOUTEXT=.out'#013#010+
  'STATICLIBPREFIX='#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'FPCMADE=fpcmade.os2'#013#010+
  'ZIPSUFFIX=os2'#013#010+
  'ECHO=echo'#013#010+
  'endif'#013#010+
  #013#010+
  '# EMX'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'AOUTEXT=.out'#013#010+
  'STATICL','IBPREFIX='#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'FPCMADE=fpcmade.emx'#013#010+
  'ZIPSUFFIX=emx'#013#010+
  'ECHO=echo'#013#010+
  'endif'#013#010+
  #013#010+
  '# Amiga'#013#010+
  'ifeq ($(OS_TARGET),amiga)'#013#010+
  'EXEEXT='#013#010+
  'SHAREDLIBEXT=.library'#013#010+
  'FPCMADE=fpcmade.amg'#013#010+
  'endif'#013#010+
  #013#010+
  '# Atari'#013#010+
  'ifeq ($(OS_TARGET),atari)'#013#010+
  'EXEEXT=.ttp'#013#010+
  'FPCMADE','=fpcmade.ata'#013#010+
  'endif'#013#010+
  #013#010+
  '# BeOS'#013#010+
  'ifeq ($(OS_TARGET),beos)'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.be'#013#010+
  'ZIPSUFFIX=be'#013#010+
  'endif'#013#010+
  #013#010+
  '# Solaris'#013#010+
  'ifeq ($(OS_TARGET),sunos)'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.sun'#013#010+
  'ZIPSUFFIX=sun'#013#010+
  'endif'#013#010+
  #013#010+
  '# QNX'#013#010+
  'ifeq ($(OS_TARGET),qnx)'#013#010+
  'EXEEXT=',#013#010+
  'FPCMADE=fpcmade.qnx'#013#010+
  'ZIPSUFFIX=qnx'#013#010+
  'endif'#013#010+
  #013#010+
  '# Netware'#013#010+
  'ifeq ($(OS_TARGET),netware)'#013#010+
  'EXEEXT=.nlm'#013#010+
  'STATICLIBPREFIX='#013#010+
  'FPCMADE=fpcmade.nw'#013#010+
  'ZIPSUFFIX=nw'#013#010+
  'endif'#013#010+
  #013#010+
  '# MacOS'#013#010+
  'ifeq ($(OS_TARGET),macos)'#013#010+
  'EXEEXT='#013#010+
  'FPCMADE=fpcmade.mcc'#013#010+
  'endif'#013#010+
  #013#010+
  '#end',' of target specific settings'#013#010+
  'endif'#013#010+
  #013#010+
  '[defaulttools]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default Tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Names of the binutils ','tools'#013#010+
  'ASNAME=as'#013#010+
  'LDNAME=ld'#013#010+
  'ARNAME=ar'#013#010+
  'RCNAME=rc'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'ASNAME=asw'#013#010+
  'LDNAME=ldw'#013#010+
  'ARNAME=arw'#013#010+
  'endif'#013#010+
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
  '# Override',' defaults'#013#010+
  'AS=$(ASPROG)'#013#010+
  'LD=$(LDPROG)'#013#010+
  'RC=$(RCPROG)'#013#010+
  'AR=$(ARPROG)'#013#010+
  #013#010+
  '# ppas.bat / ppas.sh'#013#010+
  'PPAS=ppas$(BATCHEXT)'#013#010+
  #013#010+
  '# ldconfig to rebuild .so cache'#013#010+
  'ifdef inUnix'#013#010+
  'LDCONFIG=ldconfig'#013#010+
  'else'#013#010+
  'LDCONFIG='#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef DATE'#013#010+
  'DATESTR:=$(shell $(DA','TE) +%Y%m%d)'#013#010+
  'else'#013#010+
  'DATESTR='#013#010+
  'endif'#013#010+
  #013#010+
  '# Look if UPX is found for go32v2 and win32. We can'#039't use $UPX bec'+
  'uase'#013#010+
  '# upx uses that one itself (PFV)'#013#010+
  'ifndef UPXPROG'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'UPXPROG:=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'UPXPR','OG:=1'#013#010+
  'endif'#013#010+
  'ifdef UPXPROG'#013#010+
  'UPXPROG:=$(strip $(wildcard $(addsuffix /upx$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifeq ($(UPXPROG),)'#013#010+
  'UPXPROG='#013#010+
  'else'#013#010+
  'UPXPROG:=$(firstword $(UPXPROG))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'UPXPROG='#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export UPXPROG'#013#010+
  #013#010+
  '# Zip optio','ns'#013#010+
  'ZIPOPT=-9'#013#010+
  'ZIPEXT=.zip'#013#010+
  #013#010+
  '# Tar options'#013#010+
  'ifeq ($(USETAR),bz2)'#013#010+
  'TAROPT=vI'#013#010+
  'TAREXT=.tar.bz2'#013#010+
  'else'#013#010+
  'TAROPT=vz'#013#010+
  'TAREXT=.tar.gz'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_begin]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Compiler Co','mmand Line'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Load commandline OPTDEF and add FPC_CPU define, for compiling the'#013+
  #010+
  '# compiler this needs to be turned off'#013#010+
  'ifndef NOCPUDEF'#013#010+
  'override FPCOPTDEF=$(CPU_TARG','ET)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Load commandline OPT and add target and unit dir to be sure'#013#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#013#010+
  'override FPCOPT+=-T$(OS_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  'ifeq ($(OS_SOURCE),openbsd)'#013#010+
  'override FPCOPT+=-FD$(NEW_BINUTILS_PATH)'#013#010+
  'endif'#013#010+
  #013#010+
  '# User',' dirs should be first, so they are looked at first'#013#010+
  'ifdef UNITDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fu,$(UNITDIR))'#013#010+
  'endif'#013#010+
  'ifdef LIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#013#010+
  'endif'#013#010+
  'ifdef OBJDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDI','R))'#013#010+
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
  'override FPCOPT+=-CX'#013#010+
  'endif'#013#010+
  #013#010+
  '# Debug'#013#010+
  'ifdef DEBUG'#013,#010+
  'override FPCOPT+=-gl'#013#010+
  'override FPCOPTDEF+=DEBUG'#013#010+
  'endif'#013#010+
  #013#010+
  '# Release mode (strip, optimize and don'#039't load fpc.cfg)'#013#010+
  'ifdef RELEASE'#013#010+
  'ifeq ($(CPU_TARGET),i386)'#013#010+
  'FPCCPUOPT:=-OG2p3'#013#010+
  'else'#013#010+
  'FPCCPUOPT:='#013#010+
  'endif'#013#010+
  'override FPCOPT+=-Xs $(FPCCPUOPT) -n'#013,#010+
  'override FPCOPTDEF+=RELEASE'#013#010+
  'endif'#013#010+
  #013#010+
  '# Strip'#013#010+
  'ifdef STRIP'#013#010+
  'override FPCOPT+=-Xs'#013#010+
  'endif'#013#010+
  #013#010+
  '# Optimizer (i386 only for now)'#013#010+
  'ifdef OPTIMIZE'#013#010+
  'ifeq ($(CPU_TARGET),i386)'#013#010+
  'override FPCOPT+=-OG2p3'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Verbose settings (warning,not','e,info)'#013#010+
  'ifdef VERBOSE'#013#010+
  'override FPCOPT+=-vwni'#013#010+
  'endif'#013#010+
  #013#010+
  '# Needed compiler options'#013#010+
  'ifdef COMPILER_OPTIONS'#013#010+
  'override FPCOPT+=$(COMPILER_OPTIONS)'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_UNITDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fu,$(COMPILER_UNITDIR))'#013#010+
  'endif'#013#010+
  'i','fdef COMPILER_LIBRARYDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARYDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_OBJECTDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))'#013#010+
  'endif'#013#010+
  'ifdef COMPILER_INCLUDEDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fi',',$(COMPILER_INCLUDEDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Cross compiler utils'#013#010+
  'ifdef CROSSBINDIR'#013#010+
  'override FPCOPT+=-FD$(CROSSBINDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Target dirs and the prefix to use for clean/install'#013#010+
  'ifdef COMPILER_TARGETDIR'#013#010+
  'override FPCOPT+=-FE$(COMPILER_TARGE','TDIR)'#013#010+
  'ifeq ($(COMPILER_TARGETDIR),.)'#013#010+
  'override TARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override TARGETDIRPREFIX=$(COMPILER_TARGETDIR)/'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Unit target dir'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  'override FPCOPT+=-FU$(COMPILER_UNITTARGETDIR)'#013#010+
  'ifeq ($','(COMPILER_UNITTARGETDIR),.)'#013#010+
  'override UNITTARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override UNITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef COMPILER_TARGETDIR'#013#010+
  'override COMPILER_UNITTARGETDIR=$(COMPILER_TARGETDIR)'#013#010+
  'override UNITTARGETDIRP','REFIX=$(TARGETDIRPREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Temporary hack to add HASUNIX define for linux 1.0.6 compiler'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'ifeq ($(FPC_VERSION),1.0.6)'#013#010+
  'override FPCOPTDEF+=HASUNIX'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[command_libc]'#013#010+
  '# Add GCC lib pa','th if asked'#013#010+
  'ifdef GCCLIBDIR'#013#010+
  'override FPCOPT+=-Fl$(GCCLIBDIR)'#013#010+
  'endif'#013#010+
  'ifdef OTHERLIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_end]'#013#010+
  '# Add commandline options last so they can override'#013#010+
  'ifdef OPT'#013#010+
  'override F','PCOPT+=$(OPT)'#013#010+
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
  '# Use the enviro','nment to pass commandline options?'#013#010+
  'ifdef USEENV'#013#010+
  'override FPCEXTCMD:=$(FPCOPT)'#013#010+
  'override FPCOPT:=!FPCEXTCMD'#013#010+
  'export FPCEXTCMD'#013#010+
  'endif'#013#010+
  #013#010+
  '# Compiler commandline'#013#010+
  'override COMPILER:=$(FPC) $(FPCOPT)'#013#010+
  #013#010+
  '# also call ppas if with command option ','-s'#013#010+
  '# but only if the OS_SOURCE and OS_TARGET are equal'#013#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#013#010+
  'EXECPPAS='#013#010+
  'else'#013#010+
  'ifeq ($(OS_SOURCE),$(OS_TARGET))'#013#010+
  'EXECPPAS:=@$(PPAS)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[loaderrules]'#013#010+
  '########################################','############################'+
  '#'#013#010+
  '# Loaders'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_loaders'#013#010+
  #013#010+
  'ifdef TARGET_LOADERS'#013#010+
  'override ALLTARGET+=fpc_loaders'#013#010+
  'override CLEANTARGET+=fpc_loaders_clean'#013#010+
  'overri','de INSTALLTARGET+=fpc_loaders_install'#013#010+
  #013#010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(TARGET_LOADERS))'#013#010+
  'endif'#013#010+
  #013#010+
  '%$(OEXT): %$(LOADEREXT)'#013#010+
  '        $(AS) -o $*$(OEXT) $<'#013#010+
  #013#010+
  'fpc_loaders: $(LOADEROFILES)'#013#010+
  #013#010+
  'fpc_loaders_clean:'#013#010+
  '        -$(DEL) $','(LOADEROFILES)'#013#010+
  #013#010+
  'fpc_loaders_install:'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  '        $(INSTALL) $(LOADEROFILES) $(INSTALL_UNITDIR)'#013#010+
  #013#010+
  #013#010+
  '[unitrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Units'#013#010+
  '###########','#########################################################'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_units'#013#010+
  #013#010+
  'ifdef TARGET_UNITS'#013#010+
  'override ALLTARGET+=fpc_units'#013#010+
  #013#010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))'#013#010+
  'override IMPLICITUNITPPUFILES=$(addsuffix ','$(PPUEXT),$(TARGET_IMPLICI'+
  'TUNITS))'#013#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#013#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_units: $(UNITPPUFILES)'#013#010+
  #013#010+
  #013#010+
  '[exerules]'#013#010+
  '###################','#################################################'+
  '#'#013#010+
  '# Exes'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_exes'#013#010+
  #013#010+
  'ifdef TARGET_PROGRAMS'#013#010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(TARGET_PROGRAMS))'#013#010+
  'ove','rride EXEOFILES:=$(addsuffix $(OEXT),$(TARGET_PROGRAMS)) $(addpre'+
  'fix $(STATICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_PROGRAMS))'+
  ')'#013#010+
  #013#010+
  'override ALLTARGET+=fpc_exes'#013#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#013#010+
  'override CLEANEXEFILES+=$(EXEFI','LES) $(EXEOFILES)'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_exes: $(','EXEFILES)'#013#010+
  #013#010+
  #013#010+
  '[rstrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Resource strings'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  'ifdef TARGET_RSTS'#013#010+
  'override RSTFILES=$(addsuffix ','$(RSTEXT),$(TARGET_RSTS))'#013#010+
  #013#010+
  'override CLEANRSTFILES+=$(RSTFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[examplerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Examples'#013#010+
  '################################################################','####'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_examples'#013#010+
  #013#010+
  'ifdef TARGET_EXAMPLES'#013#010+
  'HASEXAMPLES=1'#013#010+
  'override EXAMPLESOURCEFILES:=$(wildcard $(addsuffix .pp,$(TARGET_EXAMP'+
  'LES)) $(addsuffix .pas,$(TARGET_EXAMPLES)))'#013#010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),$(TARGET_','EXAMPLES))'#013+
  #010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(TARGET_EXAMPLES)) $(addp'+
  'refix $(STATICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_EXAMPLES'+
  ')))'#013#010+
  #013#010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#013#010+
  'ifeq ($(OS_TARGET),os2)',#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#013+
  #010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),emx)'#013#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#013+
  #010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef TARGET_EXAMPLEDIRS'#013#010+
  'HASEXAMPLES=1'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_ex','amples: all $(EXAMPLEFILES) $(addsuffix _all,$(TARGET_EXAMPLED'+
  'IRS))'#013#010+
  #013#010+
  #013#010+
  '[compilerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# General compile rules'#013#010+
  '#######################################################','#############'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_all fpc_smart fpc_debug fpc_release'#013#010+
  #013#010+
  '$(FPCMADE): $(ALLDEPENDENCIES) $(ALLTARGET)'#013#010+
  '        @$(ECHOREDIR) Compiled > $(FPCMADE)'#013#010+
  #013#010+
  'fpc_all: $(FPCMADE)'#013#010+
  #013#010+
  'fpc_smart:'#013#010+
  '        $(MAKE) all LINKSMART=1 CREATESMART=','1'#013#010+
  #013#010+
  'fpc_debug:'#013#010+
  '        $(MAKE) all DEBUG=1'#013#010+
  #013#010+
  'fpc_release:'#013#010+
  '        $(MAKE) all RELEASE=1'#013#010+
  #013#010+
  '# General compile rules, available for both possible .pp and .pas exte'+
  'nsions'#013#010+
  #013#010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp .rc .res'#013#010+
  #013#010+
  '%$(PP','UEXT): %.pp'#013#010+
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
  '%$(EXEEXT): %.pas'#013#010+
  '        $(COMPILER) $<'#013#010+
  '      ','  $(EXECPPAS)'#013#010+
  #013#010+
  '%.res: %.rc'#013#010+
  '        windres -i $< -o $@'#013#010+
  #013#010+
  '# Search paths for .ppu and .pp and .pas'#013#010+
  'vpath %.pp $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#013#010+
  'vpath %$(PPUEXT) $(COM','PILER_UNITTARGETDIR)'#013#010+
  #013#010+
  #013#010+
  '[libraryrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Library'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_shared'#013#010+
  #013#010+
  'ifndef LIB_FULLNAME'#013#010,
  'ifdef LIB_VERSION'#013#010+
  'LIB_FULLNAME=$(LIB_NAME).$(LIB_VERSION)'#013#010+
  'else'#013#010+
  'LIB_FULLNAME=$(LIB_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default sharedlib units are all unit objects'#013#010+
  'ifndef LIB_SHAREDUNITS'#013#010+
  'LIB_SHAREDUNITS:=$(TARGET_UNITS)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_shared: all'#013#010,
  'ifdef HASSHAREDLIB'#013#010+
  '        $(PPUMOVE) $(LIB_SHAREDUNITS) -o$(LIB_FULLNAME)'#013#010+
  'else'#013#010+
  '        @$(ECHO) Shared Libraries not supported'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[installrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Install ','rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_install fpc_sourceinstall fpc_exampleinstall'#013#010+
  #013#010+
  'ifdef INSTALL_UNITS'#013#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(INSTALL_UNITS))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifd','ef INSTALL_BUILDUNIT'#013#010+
  'override INSTALLPPUFILES:=$(filter-out $(INSTALL_BUILDUNIT)$(PPUEXT),$'+
  '(INSTALLPPUFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  'override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFI'+
  'LES)) $(addprefix $(STATIC','LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEX'+
  'T),$(INSTALLPPUFILES)))'#013#010+
  'override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTALL'+
  'PPUFILES))'#013#010+
  'override INSTALLPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPR'+
  'EFIX),$(INSTALLPPULIN','KFILES)))'#013#010+
  '# Implicitly install Package.fpc'#013#010+
  'override INSTALL_CREATEPACKAGEFPC=1'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  'override INSTALLEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(INSTALLEXEF'+
  'ILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_install: all $(INSTALLTARGET)'#013#010+
  'ifde','f INSTALLEXEFILES'#013#010+
  '        $(MKDIR) $(INSTALL_BINDIR)'#013#010+
  '# Compress the exes if upx is defined'#013#010+
  'ifdef UPXPROG'#013#010+
  '        -$(UPXPROG) $(INSTALLEXEFILES)'#013#010+
  'endif'#013#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_CRE','ATEPACKAGEFPC'#013#010+
  'ifdef FPCMAKE'#013#010+
  '# If the fpcpackage variable is set then create and install Package.fp'+
  'c,'#013#010+
  '# a safety check is done if Makefile.fpc is available'#013#010+
  'ifdef PACKAGE_VERSION'#013#010+
  'ifneq ($(wildcard Makefile.fpc),)'#013#010+
  '        $(FPCMAKE) -p -','T$(OS_TARGET) Makefile.fpc'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  '        $(INSTALL) Package.fpc $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#013#010+
  '        $(INSTALL) $(INSTALLPPUFILES',') $(INSTALL_UNITDIR)'#013#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#013#010+
  '        $(INSTALL) $(INSTALLPPULINKFILES) $(INSTALL_UNITDIR)'#013#010+
  'endif'#013#010+
  'ifneq ($(wildcard $(LIB_FULLNAME)),)'#013#010+
  '        $(MKDIR) $(INSTALL_LIBDIR)'#013#010+
  '        $(INSTALL) $(LIB_FULLNAME) $(INSTA','LL_LIBDIR)'#013#010+
  'ifdef inUnix'#013#010+
  '        ln -sf $(LIB_FULLNAME) $(INSTALL_LIBDIR)/$(LIB_NAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef INSTALL_FILES'#013#010+
  '        $(MKDIR) $(INSTALL_DATADIR)'#013#010+
  '        $(INSTALL) $(INSTALL_FILES) $(INSTALL_DATADIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_sour','ceinstall: distclean'#013#010+
  '        $(MKDIR) $(INSTALL_SOURCEDIR)'#013#010+
  '        $(COPYTREE) $(BASEDIR)/* $(INSTALL_SOURCEDIR)'#013#010+
  #013#010+
  'fpc_exampleinstall: $(addsuffix _distclean,$(TARGET_EXAMPLEDIRS))'#013#010+
  'ifdef HASEXAMPLES'#013#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDI','R)'#013#010+
  'endif'#013#010+
  'ifdef EXAMPLESOURCEFILES'#013#010+
  '        $(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)'#013#010+
  'endif'#013#010+
  'ifdef TARGET_EXAMPLEDIRS'#013#010+
  '        $(COPYTREE) $(addsuffix /*,$(TARGET_EXAMPLEDIRS)) $(INSTALL_EX'+
  'AMPLEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '[distinstallrules]'#013,#010+
  '#####################################################################'#013+
  #010+
  '# Dist Install'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_distinstall'#013#010+
  #013#010+
  'fpc_distinstall: install exampleinstall'#013#010+
  #013#010+
  #013#010+
  '[zipins','tallrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Zip'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_zipinstall fpc_zipsourceinstall fpc_zipexampleinstall'#013#010+
  #013#010+
  '# Tempor','ary path to pack a file, can only use a single deep'#013#010+
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
  '# Maybe creat','e default zipname from packagename'#013#010+
  'ifndef ZIPNAME'#013#010+
  'ifdef DIST_ZIPNAME'#013#010+
  'ZIPNAME=$(DIST_ZIPNAME)'#013#010+
  'else'#013#010+
  'ZIPNAME=$(ZIPPREFIX)$(PACKAGE_NAME)$(ZIPSUFFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# ZipTarget'#013#010+
  'ifndef ZIPTARGET'#013#010+
  'ifdef DIST_ZIPTARGET'#013#010+
  'ZIPTARGET=DIST_ZIPT','ARGET'#013#010+
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
  'USEZIPWRAPPER=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# We need to be ab','le to run in the current OS so fix'#013#010+
  '# the path separator'#013#010+
  'ifdef USEZIPWRAPPER'#013#010+
  'ZIPPATHSEP=$(PATHSEP)'#013#010+
  'ZIPWRAPPER=$(subst /,$(PATHSEP),$(DIST_DESTDIR)/fpczip$(BATCHEXT))'#013+
  #010+
  'else'#013#010+
  'ZIPPATHSEP=/'#013#010+
  'endif'#013#010+
  #013#010+
  '# Create commands to create the zip/tar f','ile'#013#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(ZIPPATHSEP),$(PACKDIR))'#013#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(ZIPPATHSEP),$(BASEDIR))'#013#010+
  'ifdef USETAR'#013#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(TAREXT)'#013#010+
  'ZIPCMD_ZIP:=$(TARPROG) cf$(TAROPT) $(ZIPDESTFILE) *'#013#010+
  'else'#013#010+
  'ZI','PDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(ZIPEXT)'#013#010+
  'ZIPCMD_ZIP:=$(subst /,$(ZIPPATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDES'+
  'TFILE) *'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_zipinstall:'#013#010+
  '        $(MAKE) $(ZIPTARGET) INSTALL_PREFIX=$(PACKDIR) ZIPINSTALL=1'#013+
  #010+
  '        $(MKDIR) ','$(DIST_DESTDIR)'#013#010+
  '        $(DEL) $(ZIPDESTFILE)'#013#010+
  'ifdef USEZIPWRAPPER'#013#010+
  '# Handle gecho separate as we need to espace \ with \\'#013#010+
  'ifneq ($(ECHOREDIR),echo)'#013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDPACK))" > $(ZIPWRAPPE'+
  'R)'#013#010+
  '        $(ECHO','REDIR) -e "$(subst \,\\,$(ZIPCMD_ZIP))" >> $(ZIPWRAPPE'+
  'R)'#013#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDBASE))" >> $(ZIPWRAPP'+
  'ER)'#013#010+
  'else'#013#010+
  '        echo $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#013#010+
  '        echo $(ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#013#010+
  '        e','cho $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)'#013#010+
  'endif'#013#010+
  'ifdef inUnix'#013#010+
  '        /bin/sh $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  '        $(ZIPWRAPPER)'#013#010+
  'endif'#013#010+
  '        $(DEL) $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  '        $(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(ZIPCMD_CDBASE)'#013#010+
  'endif'#013#010+
  '        $','(DELTREE) $(PACKDIR)'#013#010+
  #013#010+
  'fpc_zipsourceinstall:'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=src'#013+
  #010+
  #013#010+
  'fpc_zipexampleinstall:'#013#010+
  'ifdef HASEXAMPLES'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall ZIPSUFFIX=exm'#013+
  #010+
  'endif'#013#010,
  #013#010+
  'fpc_zipdistinstall:'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=distinstall'#013#010+
  #013#010+
  #013#010+
  '[cleanrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Clean rules'#013#010+
  '###########################################################','#########'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#013#010+
  #013#010+
  'ifdef EXEFILES'#013#010+
  'override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES'+
  '))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEAN_UNITS'#013#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS)',')'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEANPPUFILES'#013#010+
  'override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)'+
  ') $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(C'+
  'LEANPPUFILES)))'#013#010+
  'override CLEANPPUFILES:=$(addprefix $(UNITTARGETDI','RPREFIX),$(CLEANPP'+
  'UFILES))'#013#010+
  'override CLEANPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPREF'+
  'IX),$(CLEANPPULINKFILES)))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_clean: $(CLEANTARGET)'#013#010+
  'ifdef CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANPPUFI','LES'#013#010+
  '        -$(DEL) $(CLEANPPUFILES)'#013#010+
  'endif'#013#010+
  'ifneq ($(CLEANPPULINKFILES),)'#013#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANRSTFILES'#013#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#013+
  #010+
  'endif'#013#010+
  'ifdef CLEAN_FILES'#013#010+
  ' ','       -$(DEL) $(CLEAN_FILES)'#013#010+
  'endif'#013#010+
  'ifdef LIB_NAME'#013#010+
  '        -$(DEL) $(LIB_NAME) $(LIB_FULLNAME)'#013#010+
  'endif'#013#010+
  '        -$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(F'+
  'PCEXTFILE) $(REDIRFILE)'#013#010+
  #013#010+
  'fpc_distclean: clean'#013#010+
  #013#010+
  '# Also run cl','ean first if targetdir is set. Unittargetdir is always'#013+
  #010+
  '# set if targetdir or unittargetdir is specified'#013#010+
  'ifdef COMPILER_UNITTARGETDIR'#013#010+
  'TARGETDIRCLEAN=fpc_clean'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_cleanall: $(CLEANTARGET) $(TARGETDIRCLEAN)'#013#010+
  'ifdef CLEANEXEFILES'#013#010,
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#013#010+
  '        -$(DELTREE) *$(SMARTEXT)'#013#010+
  '        -$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res lin','k.res $'+
  '(FPCEXTFILE) $(REDIRFILE)'#013#010+
  'ifdef AOUTEXT'#013#010+
  '        -$(DEL) *$(AOUTEXT)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[baseinforules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Base info rules'#013#010+
  '############################################','########################'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_baseinfo'#013#010+
  #013#010+
  'override INFORULES+=fpc_baseinfo'#013#010+
  #013#010+
  'fpc_baseinfo:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Package info =='#013#010+
  '        @$(ECHO)  Package Name..... $(PACKAGE_NAME)'#013#010+
  '        @$(ECHO)  Package V','ersion.. $(PACKAGE_VERSION)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Configuration info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  FPC.......... $(FPC)'#013#010+
  '        @$(ECHO)  FPC Version.. $(FPC_VERSION)'#013#010+
  '        @$(ECHO)  Source CPU... $(CPU_SOURC','E)'#013#010+
  '        @$(ECHO)  Target CPU... $(CPU_TARGET)'#013#010+
  '        @$(ECHO)  Source OS.... $(OS_SOURCE)'#013#010+
  '        @$(ECHO)  Target OS.... $(OS_TARGET)'#013#010+
  '        @$(ECHO)  Full Source.. $(FULL_SOURCE)'#013#010+
  '        @$(ECHO)  Full Target.. $(FULL_TARGET)'#013#010+
  ' ','       @$(ECHO)'#013#010+
  '        @$(ECHO)  == Directory info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Required pkgs... $(REQUIRE_PACKAGES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#013#010+
  '        @$(ECHO)  FPCDir.......... $(FPCDIR)',#013#010+
  '        @$(ECHO)  CrossBinDir..... $(CROSSBINDIR)'#013#010+
  '        @$(ECHO)  UnitsDir........ $(UNITSDIR)'#013#010+
  '        @$(ECHO)  PackagesDir..... $(PACKAGESDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#013#010+
  '        @$(ECHO)  Oth','er library... $(OTHERLIBDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Tools info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  As........ $(AS)'#013#010+
  '        @$(ECHO)  Ld........ $(LD)'#013#010+
  '        @$(ECHO)  Ar........ $(AR)'#013#010+
  '        @$(ECHO)  Rc........ $','(RC)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Mv........ $(MVPROG)'#013#010+
  '        @$(ECHO)  Cp........ $(CPPROG)'#013#010+
  '        @$(ECHO)  Rm........ $(RMPROG)'#013#010+
  '        @$(ECHO)  GInstall.. $(GINSTALL)'#013#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#013#010+
  '        @$(ECHO)','  Shell..... $(SHELL)'#013#010+
  '        @$(ECHO)  Date...... $(DATE)'#013#010+
  '        @$(ECHO)  FPCMake... $(FPCMAKE)'#013#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#013#010+
  '        @$(ECHO)  Upx....... $(UPXPROG)'#013#010+
  '        @$(ECHO)  Zip....... $(ZIPPROG)'#013#010+
  '        @$(ECHO',')'#013#010+
  '        @$(ECHO)  == Object info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Target Loaders........ $(TARGET_LOADERS)'#013#010+
  '        @$(ECHO)  Target Units.......... $(TARGET_UNITS)'#013#010+
  '        @$(ECHO)  Target Implicit Units. $(TARGET_IMPLICITUNITS)',#013#010+
  '        @$(ECHO)  Target Programs....... $(TARGET_PROGRAMS)'#013#010+
  '        @$(ECHO)  Target Dirs........... $(TARGET_DIRS)'#013#010+
  '        @$(ECHO)  Target Examples....... $(TARGET_EXAMPLES)'#013#010+
  '        @$(ECHO)  Target ExampleDirs.... $(TARGET_EXAMPLEDI','RS)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Clean Units......... $(CLEAN_UNITS)'#013#010+
  '        @$(ECHO)  Clean Files......... $(CLEAN_FILES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Install Units....... $(INSTALL_UNITS)'#013#010+
  '        @$(ECHO)  Install Files.','...... $(INSTALL_FILES)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Install info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#013#010+
  '        @$(ECHO)  ZipPrefix............ $(ZIPPREFIX)'#013#010+
  '        @$(ECHO)  ZipSuffix........','.... $(ZIPSUFFIX)'#013#010+
  '        @$(ECHO)  Install FPC Package.. $(INSTALL_FPCPACKAGE)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)'#013#010+
  '        @$(ECHO)  Install binary dir... $(INSTALL_BINDIR)'#013#010+
  '        @$(ECHO)  Ins','tall library dir.. $(INSTALL_LIBDIR)'#013#010+
  '        @$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)'#013#010+
  '        @$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)'#013#010+
  '        @$(ECHO)  Install doc dir...... $(INSTALL_DOCDIR)'#013#010+
  '        @$(ECHO)  Ins','tall example dir.. $(INSTALL_EXAMPLEDIR)'#013#010+
  '        @$(ECHO)  Install data dir..... $(INSTALL_DATADIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Dist destination dir. $(DIST_DESTDIR)'#013#010+
  '        @$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)'#013#010+
  '      ','  @$(ECHO)'#013#010+
  #013#010+
  '[inforules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Info rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_info'#013#010+
  #013#010+
  'fpc_info: $(INFORULES)'#013#010+
  #013#010+
  '[makefile','rules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Rebuild Makefile'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_makefile fpc_makefiles fpc_makefile_sub1 fpc_makefile_su','b'+
  '2 \'#013#010+
  '        fpc_makefile_dirs'#013#010+
  #013#010+
  'fpc_makefile:'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) Makefile.fpc'#013#010+
  #013#010+
  'fpc_makefile_sub1:'#013#010+
  'ifdef TARGET_DIRS'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGE'+
  'T_DIRS))'#013#010+
  'endif'#013#010+
  'ifdef TARGE','T_EXAMPLEDIRS'#013#010+
  '        $(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGE'+
  'T_EXAMPLEDIRS))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_makefile_sub2: $(addsuffix _makefile_dirs,$(TARGET_DIRS) $(TARGET_'+
  'EXAMPLEDIRS))'#013#010+
  #013#010+
  'fpc_makefile_dirs: fpc_makefile_sub1 fpc_m','akefile_sub2'#013#010+
  #013#010+
  'fpc_makefiles: fpc_makefile fpc_makefile_dirs'#013#010+
  #013#010+
  '[localmakefile]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Local Makefile'#013#010+
  '#####################################################################',
  #013#010+
  #013#010+
  'ifneq ($(wildcard fpcmake.loc),)'#013#010+
  'include fpcmake.loc'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[userrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Users rules'#013#010+
  '#####################################################################'#013+
  #010,
  #013#010+
  '[lclrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# LCL Rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# LCL Platform'#013#010+
  'ifndef LCL_PLATFORM'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'LCL_P','LATFORM=win32'#013#010+
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
  'override LCL','DIR=wrong'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check if the default LCLDIR is correct'#013#010+
  'ifdef DEFAULT_LCLDIR'#013#010+
  'override LCLDIR:=$(subst \,/,$(DEFAULT_LCLDIR))'#013#010+
  'ifeq ($(wildcard $(LCLDIR)/units/$(LCL_PLATFORM)),)'#013#010+
  'override LCLDIR=wr','ong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check for development version'#013#010+
  'ifeq ($(LCLDIR),wrong)'#013#010+
  'override LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildcard'+
  ' $(addsuffix /units/$(LCL_PLATFORM),$(BASEDIR)/lcl $(BASEDIR)))))'#013#010+
  'ifeq ($(LCLDIR),)'#013#010+
  'overri','de LCLDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check for release version'#013#010+
  'ifeq ($(LCLDIR),wrong)'#013#010+
  'override LCLDIR=$(subst /units/$(LCL_PLATFORM),,$(firstword $(wildcard'+
  ' $(addsuffix /lib/lazarus/units/$(LCL_PLATFORM),/usr/local /usr))))'#013+
  #010+
  'ifeq ($(LCLDIR)',',)'#013#010+
  'override LCLDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Generate dirs'#013#010+
  'override LCLUNITDIR:=$(wildcard $(LCLDIR)/units/$(LCL_PLATFORM) $(LCLD'+
  'IR)/units)'#013#010+
  'override LCLCOMPONENTDIR:=$(wildcard $(LCLDIR)/.. $(LCLDIR)/../compone'+
  'nts $(LCLDIR)/components)'#013#010+
  'e','xport LCLDIR LCLUNITDIR LCLCOMPONENTDIR'#013#010+
  #013#010+
  '# Add LCL dirs to paths'#013#010+
  'override REQUIRE_PACKAGESDIR+=$(LCLCOMPONENTDIR)'#013#010+
  'override COMPILER_UNITDIR+=$(LCLUNITDIR)'#013#010+
  #013#010+
  '[lclinforules]'#013#010+
  '##############################################################','######'+
  '#'#013#010+
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
  '        @$(ECHO)  Platform....','......... $(LCL_PLATFORM)'#013#010+
  '        @$(ECHO)  LCLDIR............... $(LCLDIR)'#013#010+
  '        @$(ECHO)  LCL Unit dir......... $(LCLUNITDIR)'#013#010+
  '        @$(ECHO)  LCL Component dir.... $(LCLCOMPONENTDIR)'#013#010+
  '        @$(ECHO)'#013#010
);
