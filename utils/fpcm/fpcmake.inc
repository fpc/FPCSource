{$ifdef Delphi}
const fpcmakeini : array[0..127] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..127,1..240] of char=(
{$endif Delphi}
  ';'#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#010+
  ';'#010+
  #010+
  '[osdetect]'#010+
  '#####################################################################'#010+
  '# Autodetect OS (Linux or Dos or Windows NT or OS/2)'#010+
  '# define inUnix when running under',' Unix like environment'#010+
  '#               (Linux,FreeBSD,NetBSD,Cygwin)'#010+
  '# define inWinNT when running under WinNT'#010+
  '# define inOS2 when running under OS/2'#010+
  '# define inCygwin when running under Cygwin32'#010+
  '############################################','########################'+
  '#'#010+
  #010+
  '# We need only / in the path also remove the current dir'#010+
  'override PATH:=$(subst \,/,$(PATH))'#010+
  #010+
  '# Determine if we'#039've a unix searchpath using : by looking for a ;'#010+
  '# that normally doesn'#039't exists in path names.'#010+
  'ifeq ($','(findstring ;,$(PATH)),)'#010+
  'inUnix=1'#010+
  'SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))'#010+
  'else'#010+
  'SEARCHPATH:=$(subst ;, ,$(PATH))'#010+
  'endif'#010+
  #010+
  '# Search for PWD and determine also if we are under linux'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(SEARCHP','ATH))))'#010+
  'ifeq ($(PWD),)'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(SEARCHPATH))))'#010+
  'ifeq ($(PWD),)'#010+
  'nopwd:'#010+
  '        @echo You need the GNU utils package to use this Makefile!'#010+
  '        @echo Get ftp://ftp.freepascal.org/pub/fpc/dist/go32v2/utilgo3',
  '2.zip'#010+
  '        @exit'#010+
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
  '# Detect OS/2 - OS/2 has OS2_SHELL defined'#010+
  'ifndef inUnix'#010+
  'ifeq ($(OS),Windows_NT)'#010+
  'i','nWinNT=1'#010+
  'else'#010+
  'ifdef OS2_SHELL'#010+
  'inOS2=1'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'ifneq ($(findstring cygwin,$(MACHTYPE)),)'#010+
  'inCygWin=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# The extension of batch files / scripts'#010+
  'ifdef inUnix'#010+
  'BATCHEXT=.sh'#010+
  'else'#010+
  'ifdef inOS2'#010+
  'BATCHEXT=.cmd'#010+
  'else'#010+
  'BATCHEXT=.bat'#010+
  'en','dif'#010+
  'endif'#010+
  #010+
  '# Path Separator, the subst trick is necessary for the \ that can'#039't'+
  ' exists'#010+
  '# at the end of a line'#010+
  'ifdef inUnix'#010+
  'PATHSEP=/'#010+
  'else'#010+
  'PATHSEP:=$(subst /,\,/)'#010+
  'endif'#010+
  #010+
  '# Base dir'#010+
  'ifdef PWD'#010+
  'BASEDIR:=$(subst \,/,$(shell $(PWD)))'#010+
  '# For Cygwin ','we need to replace /cygdrive/c/ with c:/'#010+
  'ifdef inCygWin'#010+
  'ifneq ($(findstring /cygdrive/,$(BASEDIR)),)'#010+
  'BASENODIR:=$(patsubst /cygdrive%,%,$(BASEDIR))'#010+
  'BASEDRIVE:=$(firstword $(subst /, ,$(BASENODIR)))'#010+
  'BASEDIR:=$(subst /cygdrive/$(BASEDRIVE)/,$','(BASEDRIVE):/,$(BASEDIR))'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'BASEDIR=.'#010+
  'endif'#010+
  #010+
  '[fpcdetect]'#010+
  '#####################################################################'#010+
  '# FPC version/target Detection'#010+
  '###################################################################','#'+
  '#'#010+
  #010+
  '# What compiler to use ?'#010+
  'ifndef FPC'#010+
  '# Compatibility with old makefiles'#010+
  'ifdef PP'#010+
  'FPC=$(PP)'#010+
  'else'#010+
  'ifdef inUnix'#010+
  'CPU_SOURCE=$(shell uname -m)'#010+
  'ifeq (m68k,$(CPU_SOURCE))'#010+
  'FPC=ppc68k'#010+
  'else'#010+
  'FPC=ppc386'#010+
  'endif'#010+
  'else'#010+
  'FPC=ppc386'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'overri','de FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#010+
  #010+
  '# Target OS'#010+
  'ifndef OS_TARGET'#010+
  'OS_TARGET:=$(shell $(FPC) -iTO)'#010+
  'endif'#010+
  #010+
  '# Source OS'#010+
  'ifndef OS_SOURCE'#010+
  'OS_SOURCE:=$(shell $(FPC) -iSO)'#010+
  'endif'#010+
  #010+
  '# Target CPU'#010+
  'ifndef',' CPU_TARGET'#010+
  'CPU_TARGET:=$(shell $(FPC) -iTP)'#010+
  'endif'#010+
  #010+
  '# Source CPU'#010+
  'ifndef CPU_SOURCE'#010+
  'CPU_SOURCE:=$(shell $(FPC) -iSP)'#010+
  'endif'#010+
  #010+
  '# FPC version'#010+
  'ifndef FPC_VERSION'#010+
  'FPC_VERSION:=$(shell $(FPC) -iV)'#010+
  'endif'#010+
  #010+
  'export FPC OS_TARGET OS_SOURCE CPU_TARGET CP','U_SOURCE FPC_VERSION'#010+
  #010+
  #010+
  '[fpcdircheckenv]'#010+
  '#####################################################################'#010+
  '# FPCDIR Setting'#010+
  '#####################################################################'#010+
  #010+
  '# Test FPCDIR to look if the RTL dir exist','s'#010+
  'ifdef FPCDIR'#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  'else'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  #010+
  #010+
  '[fpcdirdetect]'#010+
  '# Detect FPCDIR'#010+
  'ifeq ($(FPCDIR),wrong)'#010+
  'ifdef inUnix'#010+
  'o','verride FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#010+
  'endif'#010+
  'else'#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC),$(SEARCHPATH)))',')))'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)'#010+
  'override FPCDIR=c:/pp'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Units dir'#010+
  'UNITSDIR:','=$(wildcard $(FPCDIR)/units/$(OS_TARGET))'#010+
  #010+
  '# Packages dir'#010+
  'PACKAGESDIR:=$(wildcard $(FPCDIR) $(FPCDIR)/packages)'#010+
  #010+
  #010+
  '[shelltools]'#010+
  '#####################################################################'#010+
  '# Shell tools'#010+
  '#############################','#######################################'+
  '#'#010+
  #010+
  '# Echo that can redir (must be able run in the default OS shell)'#010+
  'ifndef ECHOREDIR'#010+
  'ECHOREDIR:=$(subst /,$(PATHSEP),$(ECHO))'#010+
  'endif'#010+
  #010+
  '# To copy pograms'#010+
  'ifndef COPY'#010+
  'COPY:=$(CPPROG) -fp'#010+
  'endif'#010+
  #010+
  '# Copy a wh','ole tree'#010+
  'ifndef COPYTREE'#010+
  'COPYTREE:=$(CPPROG) -rfp'#010+
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
  'ifndef DELTREE'#010+
  'DELTREE:=$(RMPROG) -rf'#010+
  'endif'#010,
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
  'INSTALLEXE:=$(GINSTALL) -c -m 755'#010+
  'else'#010+
  'INSTALLEXE:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '#',' To make a directory.'#010+
  'ifndef MKDIR'#010+
  'MKDIR:=$(GINSTALL) -m 755 -d'#010+
  'endif'#010+
  #010+
  'export ECHOREDIR COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKD'+
  'IR'#010+
  #010+
  #010+
  '[defaulttools]'#010+
  '#####################################################################'#010+
  '# Default',' Tools'#010+
  '#####################################################################'#010+
  #010+
  '# assembler, redefine it if cross compiling'#010+
  'ifndef AS'#010+
  'AS=as'#010+
  'endif'#010+
  #010+
  '# linker, but probably not used'#010+
  'ifndef LD'#010+
  'LD=ld'#010+
  'endif'#010+
  #010+
  '# Resource compiler'#010+
  'ifndef RC'#010+
  'RC=rc'#010+
  'endi','f'#010+
  #010+
  '# ppas.bat / ppas.sh'#010+
  'PPAS=ppas$(BATCHEXT)'#010+
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
  'endif'#010+
  #010+
  '# Look if UPX is found for go32v2 and win32','. We can'#039't use $UPX b'+
  'ecuase'#010+
  '# upx uses that one itself (PFV)'#010+
  'ifndef UPXPROG'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'UPXPROG:=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'UPXPROG:=1'#010+
  'endif'#010+
  'ifdef UPXPROG'#010+
  'UPXPROG:=$(strip $(wildcard $(addsuffix /upx$(SRCEXEEXT),$(SEAR','CHPAT'+
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
  'ifeq ($(USETAR),bz2)'#010+
  'TAROPT=vI'#010+
  'TAREXT=.tar.bz2'#010+
  'else'#010+
  'TAROPT=vz'#010+
  'TA','REXT=.tar.gz'#010+
  'endif'#010+
  #010+
  #010+
  '[extensions]'#010+
  '#####################################################################'#010+
  '# Default extensions'#010+
  '#####################################################################'#010+
  #010+
  '# Default needed extensions (Go32v2,Linux)'#010+
  'L','OADEREXT=.as'#010+
  'EXEEXT=.exe'#010+
  'PPLEXT=.ppl'#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'ASMEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.so'#010+
  'STATICLIBPREFIX=libp'#010+
  'RSTEXT=.rst'#010+
  'FPCMADE=fpcmade'#010+
  #010+
  #010+
  '# Go32v1'#010+
  'ifeq ($(OS_TARGET),go32v1)'#010+
  'PPUEXT=.pp1'#010+
  'OEXT=.o1'#010+
  'ASMEXT=.s1'#010+
  'SMARTEXT','=.sl1'#010+
  'STATICLIBEXT=.a1'#010+
  'SHAREDLIBEXT=.so1'#010+
  'STATICLIBPREFIX='#010+
  'FPCMADE=fpcmade.v1'#010+
  'PACKAGESUFFIX=v1'#010+
  'endif'#010+
  #010+
  '# Go32v2'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'STATICLIBPREFIX='#010+
  'FPCMADE=fpcmade.dos'#010+
  'ZIPSUFFIX=go32'#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'EXEEXT='#010+
  'HA','SSHAREDLIB=1'#010+
  'FPCMADE=fpcmade.lnx'#010+
  'ZIPSUFFIX=linux'#010+
  'endif'#010+
  #010+
  '# FreeBSD'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'FPCMADE=fpcmade.freebsd'#010+
  'ZIPSUFFIX=freebsd'#010+
  'endif'#010+
  #010+
  '# NetBSD'#010+
  'ifeq ($(OS_TARGET),netbsd)'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'FPCMADE=fpcmade','.netbsd'#010+
  'ZIPSUFFIX=netbsd'#010+
  'endif'#010+
  #010+
  '# Win32'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'PPUEXT=.ppw'#010+
  'OEXT=.ow'#010+
  'ASMEXT=.sw'#010+
  'SMARTEXT=.slw'#010+
  'STATICLIBEXT=.aw'#010+
  'SHAREDLIBEXT=.dll'#010+
  'FPCMADE=fpcmade.w32'#010+
  'ZIPSUFFIX=w32'#010+
  'endif'#010+
  #010+
  '# OS/2'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'PPUEXT=.ppo'#010+
  'ASMEXT=.s','o2'#010+
  'OEXT=.oo2'#010+
  'AOUTEXT=.out'#010+
  'SMARTEXT=.so'#010+
  'STATICLIBEXT=.ao2'#010+
  'SHAREDLIBEXT=.dll'#010+
  'FPCMADE=fpcmade.os2'#010+
  'ZIPSUFFIX=emx'#010+
  'endif'#010+
  #010+
  '# Amiga'#010+
  'ifeq ($(OS_TARGET),amiga)'#010+
  'EXEEXT='#010+
  'PPUEXT=.ppa'#010+
  'ASMEXT=.asm'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.library',#010+
  'FPCMADE=fpcmade.amg'#010+
  'endif'#010+
  #010+
  '# Atari'#010+
  'ifeq ($(OS_TARGET),atari)'#010+
  'PPUEXT=.ppt'#010+
  'ASMEXT=.s'#010+
  'OEXT=.o'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'EXEEXT=.ttp'#010+
  'FPCMADE=fpcmade.ata'#010+
  'endif'#010+
  #010+
  '[defaultdirs]'#010+
  '##################################################################','##'+
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
  'ifdef REQUIRE_PACKAGESDIR'#010+
  'override PACKAGESDIR+=$(','REQUIRE_PACKAGESDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '# Linux, netbsd and freebsd use unix dirs with /usr/bin, /usr/lib'#010+
  '# When zipping use the target as default, when normal install then'#010+
  '# use the source os as default'#010+
  'ifdef ZIPINSTALL'#010+
  '# Zipinstall'#010+
  'ifeq ($(OS_TARGET','),linux)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),netbsd)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'else'#010+
  '# Normal install'#010+
  'ifeq ($(OS_SOURCE),linux)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_SOURCE),freebsd)'#010+
  'UNIXINST','ALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_SOURCE),netbsd)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the prefix directory where to install everything'#010+
  'ifndef INSTALL_PREFIX'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_PREFIX=/usr/local'#010+
  'else'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_BASEDI','R:=/pp'#010+
  'else'#010+
  'INSTALL_BASEDIR:=/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'export INSTALL_PREFIX'#010+
  #010+
  '# Where to place the resulting zip files'#010+
  'ifndef DIST_DESTDIR'#010+
  'DIST_DESTDIR:=$(BASEDIR)'#010+
  'endif'#010+
  'export DIST_DESTDIR'#010+
  #010+
  #010+
  '#######################################','#############################'+
  '#'#010+
  '# Install Directories'#010+
  '#####################################################################'#010+
  #010+
  '# set the base directory where to install everything'#010+
  'ifndef INSTALL_BASEDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'ifdef INSTALL_FPCPA','CKAGE'#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)'#010+
  'else'#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'else'#010+
  'INSTALL_BASEDIR:=$(INSTALL_PREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the binaries'#010+
  'ifndef ','INSTALL_BINDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_BINDIR:=$(INSTALL_PREFIX)/bin'#010+
  'else'#010+
  'INSTALL_BINDIR:=$(INSTALL_BASEDIR)/bin'#010+
  '# for FPC packages install the binaries under their os target subdir'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_BINDIR:=$(INSTALL_BI','NDIR)/$(OS_TARGET)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the units.'#010+
  'ifndef INSTALL_UNITDIR'#010+
  'INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/units/$(OS_TARGET)'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'ifdef PACKAGE_NAME'#010+
  'INSTALL_UNITDIR:=$(INSTALL_UNITD','IR)/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install shared libraries'#010+
  'ifndef INSTALL_LIBDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_LIBDIR:=$(INSTALL_PREFIX)/lib'#010+
  'else'#010+
  'INSTALL_LIBDIR:=$(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the source files will b','e stored'#010+
  'ifndef INSTALL_SOURCEDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/src/fpc-$(FPC_VERSION)/$(PACKAGE_'+
  'NAME)'#010+
  'else'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/src/$(PACKAGE_NAME)-$(PACKAGE_VER'+
  'SION)'#010+
  'end','if'#010+
  'else'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_SOURCEDIRL:=$(INSTALL_BASEDIR)/source'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the doc files will be stored'#010+
  'ifndef INSTALL_DOCDIR'#010+
  'ifdef UNIXINSTALL','DIR'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)/$(PACKAGE_NAM'+
  'E)'#010+
  'else'#010+
  'INSTALL_DOCDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_NAME)-$(PACKAGE_VERSIO'+
  'N)'#010+
  'endif'#010+
  'else'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_DOCDIR:=$(INSTAL','L_BASEDIR)/doc/$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install the examples, under linux we use the doc dir'#010+
  '# because the copytree command will create a subdir itself'#010+
  'ifndef INSTALL_EXAMPLED','IR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'ifdef INSTALL_FPCPACKAGE'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)/examples/'+
  '$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_NAME)-$(PACKAGE_VE'+
  'RSION)'#010+
  'endif'#010+
  'else'#010+
  'ifdef INSTAL','L_FPCPACKAGE'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples/$(PACKAGE_NAME)'#010+
  'else'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the some extra (data)files will be stored'#010+
  'ifndef INSTALL_DATADIR'#010+
  'INSTALL_DATADIR=$(','INSTALL_BASEDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[dirlibc]'#010+
  '# On linux, try to find where libgcc.a is.'#010+
  'ifdef inUnix'#010+
  'ifndef GCCLIBDIR'#010+
  'GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '#039'{ print'+
  ' $$4 } '#039'`)'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'ifndef OTHERLIBDIR'#010+
  'OTHE','RLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | awk '#039'{ ORS='+
  '" "; print $1 }'#039')'#010+
  'endif'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),netbsd)'#010+
  'OTHERLIBDIR+=/usr/pkg/lib'#010+
  'endif'#010+
  'export GCCLIBDIR OTHERLIB'#010+
  'endif'#010+
  #010+
  #010+
  '[command_begin]'#010+
  '#########################################','###########################'+
  '#'#010+
  '# Compiler Command Line'#010+
  '#####################################################################'#010+
  #010+
  '# Load commandline OPTDEF and add FPC_CPU define'#010+
  'override FPCOPTDEF=$(CPU_TARGET)'#010+
  #010+
  '# Load commandline OPT and add ta','rget and unit dir to be sure'#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#010+
  'override FPCOPT+=-T$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  '# User dirs should be first, so they are looked at first'#010+
  'ifdef UNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(UNITDIR))'#010+
  'endif'#010+
  'ifdef LIBDIR'#010+
  'ove','rride FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#010+
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
  'override FPCOPT+=-XX'#010+
  'endif'#010+
  #010+
  '# Smart','linking creation'#010+
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
  '# Release mode (strip, optimize and don'#039't load ppc386.cfg)'#010+
  'ifdef RELEASE'#010+
  'ifeq ($(CPU_TARGET),i386)'#010+
  'FPCC','PUOPT:=-OG2p3'#010+
  'else'#010+
  'FPCCPUOPT:='#010+
  'endif'#010+
  'override FPCOPT+=-Xs $(FPCCPUOPT) -n'#010+
  'override FPCOPTDEF+=RELEASE'#010+
  'endif'#010+
  #010+
  '# Strip'#010+
  'ifdef STRIP'#010+
  'override FPCOPT+=-Xs'#010+
  'endif'#010+
  #010+
  '# Optimizer (i386 only for now)'#010+
  'ifdef OPTIMIZE'#010+
  'ifeq ($(CPU_TARGET),i386)'#010+
  'override F','PCOPT+=-OG2p3'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Verbose settings (warning,note,info)'#010+
  'ifdef VERBOSE'#010+
  'override FPCOPT+=-vwni'#010+
  'endif'#010+
  #010+
  '# Needed compiler options'#010+
  'ifdef COMPILER_OPTIONS'#010+
  'override FPCOPT+=$(COMPILER_OPTIONS)'#010+
  'endif'#010+
  'ifdef COMPILER_UNITDIR'#010+
  'override FPCOPT','+=$(addprefix -Fu,$(COMPILER_UNITDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_LIBRARYDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARYDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_OBJECTDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_INCL','UDEDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(COMPILER_INCLUDEDIR))'#010+
  'endif'#010+
  #010+
  '# Target dirs and the prefix to use for clean/install'#010+
  'ifdef COMPILER_TARGETDIR'#010+
  'override FPCOPT+=-FE$(COMPILER_TARGETDIR)'#010+
  'ifeq ($(COMPILER_TARGETDIR),.)'#010+
  'override TARGETDI','RPREFIX='#010+
  'else'#010+
  'override TARGETDIRPREFIX=$(COMPILER_TARGETDIR)/'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Unit target dir'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  'override FPCOPT+=-FU$(COMPILER_UNITTARGETDIR)'#010+
  'ifeq ($(COMPILER_UNITTARGETDIR),.)'#010+
  'override UNITTARGETDIRPREFIX='#010+
  'else'#010+
  'ov','erride UNITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/'#010+
  'endif'#010+
  'else'#010+
  'ifdef COMPILER_TARGETDIR'#010+
  'override COMPILER_UNITTARGETDIR=$(COMPILER_TARGETDIR)'#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[command_libc]'#010+
  '# Add GCC lib path i','f asked'#010+
  'ifdef GCCLIBDIR'#010+
  'override FPCOPT+=-Fl$(GCCLIBDIR)'#010+
  'endif'#010+
  'ifdef OTHERLIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[command_end]'#010+
  '# Add commandline options last so they can override'#010+
  'ifdef OPT'#010+
  'override FPCOPT+=$(OPT)'#010+
  'en','dif'#010+
  #010+
  '# Add defines from FPCOPTDEF to FPCOPT'#010+
  'ifdef FPCOPTDEF'#010+
  'override FPCOPT+=$(addprefix -d,$(FPCOPTDEF))'#010+
  'endif'#010+
  #010+
  '# Was a config file specified ?'#010+
  'ifdef CFGFILE'#010+
  'override FPCOPT+=@$(CFGFILE)'#010+
  'endif'#010+
  #010+
  '# Use the environment to pass commandline opt','ions?'#010+
  'ifdef USEENV'#010+
  'override FPCEXTCMD:=$(FPCOPT)'#010+
  'override FPCOPT:=!FPCEXTCMD'#010+
  'export FPCEXTCMD'#010+
  'endif'#010+
  #010+
  '# Compiler commandline'#010+
  'override COMPILER:=$(FPC) $(FPCOPT)'#010+
  #010+
  '# also call ppas if with command option -s'#010+
  '# but only if the OS_SOURCE and OS_T','ARGET are equal'#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#010+
  'EXECPPAS='#010+
  'else'#010+
  'ifeq ($(OS_SOURCE),$(OS_TARGET))'#010+
  'EXECPPAS:=@$(PPAS)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  #010+
  '[loaderrules]'#010+
  '#####################################################################'#010+
  '# Loaders'#010+
  '###########','#########################################################'+
  '#'#010+
  #010+
  '.PHONY: fpc_loaders'#010+
  #010+
  'ifdef TARGET_LOADERS'#010+
  'override ALLTARGET+=fpc_loaders'#010+
  'override CLEANTARGET+=fpc_loaders_clean'#010+
  'override INSTALLTARGET+=fpc_loaders_install'#010+
  #010+
  'override LOADEROFILES',':=$(addsuffix $(OEXT),$(TARGET_LOADERS))'#010+
  'endif'#010+
  #010+
  '%$(OEXT): %$(LOADEREXT)'#010+
  '        $(AS) -o $*$(OEXT) $<'#010+
  #010+
  'fpc_loaders: $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_clean:'#010+
  '        -$(DEL) $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_install:'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR',')'#010+
  '        $(INSTALL) $(LOADEROFILES) $(INSTALL_UNITDIR)'#010+
  #010+
  #010+
  '[unitrules]'#010+
  '#####################################################################'#010+
  '# Units'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_units'#010+
  #010+
  'if','def TARGET_UNITS'#010+
  'override ALLTARGET+=fpc_units'#010+
  #010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))'#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES)'#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES)'#010+
  'endif'#010+
  #010+
  'fpc_units: $(UNITPPUFILES)'#010+
  #010+
  #010+
  '[exerules]'#010+
  '##','##################################################################'+
  '#'#010+
  '# Exes'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_exes'#010+
  #010+
  'ifdef TARGET_PROGRAMS'#010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(TARGET_PROGR','AMS))'#010+
  'override EXEOFILES:=$(addsuffix $(OEXT),$(TARGET_PROGRAMS)) $(addprefi'+
  'x $(STATICLIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_PROGRAMS)))'#010+
  #010+
  'override ALLTARGET+=fpc_exes'#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#010+
  'override CLEANEXEFILES+=$(','EXEFILES) $(EXEOFILES)'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_PROGRAMS))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'fpc_exes: $(EXEFILES)'#010+
  #010+
  #010+
  '[rstrules]'#010+
  '#####################################################################'#010+
  '# Resour','ce strings'#010+
  '#####################################################################'#010+
  #010+
  'ifdef TARGET_RSTS'#010+
  'override RSTFILES=$(addsuffix $(RSTEXT),$(TARGET_RSTS))'#010+
  #010+
  'override CLEANRSTFILES+=$(RSTFILES)'#010+
  'endif'#010+
  #010+
  #010+
  '[examplerules]'#010+
  '########################','############################################'+
  '#'#010+
  '# Examples'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_examples'#010+
  #010+
  'ifdef TARGET_EXAMPLES'#010+
  'HASEXAMPLES=1'#010+
  'override EXAMPLESOURCEFILES:=$(wildcard $(addsuffix .p','p,$(TARGET_EXA'+
  'MPLES)) $(addsuffix .pas,$(TARGET_EXAMPLES)))'#010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),$(TARGET_EXAMPLES))'#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(TARGET_EXAMPLES)) $(addp'+
  'refix $(STATICLIBPREFIX),$(addsuffix $(STATIC','LIBEXT),$(TARGET_EXAMPL'+
  'ES)))'#010+
  #010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'override CLEANEXEFILES+=$(addsuffix $(AOUTEXT),$(TARGET_EXAMPLES))'#010+
  'endif'#010+
  'endif'#010+
  'ifdef TARGET_EXAMPLEDIRS'#010+
  'HASEXAMPLES=1'#010+
  'endif'#010+
  #010+
  'fpc_','examples: all $(EXAMPLEFILES) $(addsuffix _all,$(TARGET_EXAMPLED'+
  'IRS))'#010+
  #010+
  #010+
  '[compilerules]'#010+
  '#####################################################################'#010+
  '# General compile rules'#010+
  '###########################################################','#########'+
  '#'#010+
  #010+
  '.PHONY: fpc_packages fpc_all fpc_smart fpc_debug'#010+
  #010+
  '$(FPCMADE): $(ALLTARGET)'#010+
  '        @$(ECHOREDIR) Compiled > $(FPCMADE)'#010+
  #010+
  'fpc_packages: $(COMPILEPACKAGES)'#010+
  #010+
  'fpc_all: fpc_packages $(FPCMADE)'#010+
  #010+
  'fpc_smart:'#010+
  '        $(MAKE) all LINKSMART','=1 CREATESMART=1'#010+
  #010+
  'fpc_debug:'#010+
  '        $(MAKE) all DEBUG=1'#010+
  #010+
  '# General compile rules, available for both possible .pp and .pas exte'+
  'nsions'#010+
  #010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp'#010+
  #010+
  '%$(PPUEXT): %.pp'#010+
  '        $(COMPILER) $<'#010+
  '        $(EXECP','PAS)'#010+
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
  '# Search paths for .ppu and .pp and .pas'#010+
  'vpath %.','pp $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#010+
  'vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)'#010+
  'vpath %$(PPUEXT) $(COMPILER_UNITTARGETDIR)'#010+
  #010+
  #010+
  '[libraryrules]'#010+
  '#####################################################################'#010+
  '# Lib','rary'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_shared'#010+
  #010+
  'ifndef LIB_FULLNAME'#010+
  'ifdef LIB_VERSION'#010+
  'LIB_FULLNAME=$(LIB_NAME).$(LIB_VERSION)'#010+
  'else'#010+
  'LIB_FULLNAME=$(LIB_NAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Default sharedlib uni','ts are all unit objects'#010+
  'ifndef LIB_SHAREDUNITS'#010+
  'LIB_SHAREDUNITS:=$(TARGET_UNITS)'#010+
  'endif'#010+
  #010+
  'fpc_shared: all'#010+
  'ifdef HASSHAREDLIB'#010+
  '        $(PPUMOVE) $(LIB_SHAREDUNITS) -o$(LIB_FULLNAME)'#010+
  'else'#010+
  '        @$(ECHO) Shared Libraries not supported'#010+
  'endif'#010+
  #010+
  #010+
  '[','installrules]'#010+
  '#####################################################################'#010+
  '# Install rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_install fpc_sourceinstall fpc_exampleinstall'#010+
  #010+
  'ifdef INSTA','LL_UNITS'#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(INSTALL_UNITS))'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLPPUFILES'#010+
  'override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFI'+
  'LES)) $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIB','EX'+
  'T),$(INSTALLPPUFILES)))'#010+
  'override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTALL'+
  'PPUFILES))'#010+
  'override INSTALLPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPR'+
  'EFIX),$(INSTALLPPULINKFILES)))'#010+
  '# Implicitly install Package.fpc',#010+
  'override INSTALL_CREATEPACKAGEFPC=1'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLEXEFILES'#010+
  'override INSTALLEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(INSTALLEXEF'+
  'ILES))'#010+
  'endif'#010+
  #010+
  'fpc_install: all $(INSTALLTARGET)'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        $(MKDIR) $(INSTALL_BINDIR)',#010+
  '# Compress the exes if upx is defined'#010+
  'ifdef UPXPROG'#010+
  '        -$(UPXPROG) $(INSTALLEXEFILES)'#010+
  'endif'#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)'#010+
  'endif'#010+
  'ifdef INSTALL_CREATEPACKAGEFPC'#010+
  'ifdef FPCMAKE'#010+
  '# If the fpcpackage variable is ','set then create and install Package.'+
  'fpc,'#010+
  '# a safety check is done if Makefile.fpc is available'#010+
  'ifdef PACKAGE_VERSION'#010+
  'ifneq ($(wildcard Makefile.fpc),)'#010+
  '        $(FPCMAKE) -p -T$(OS_TARGET) Makefile.fpc'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#010+
  '   ','     $(INSTALL) Package.fpc $(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'ifdef INSTALLPPUFILES'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(INSTALL_UNITDIR)'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '        $(INSTALL) $(I','NSTALLPPULINKFILES) $(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'ifneq ($(wildcard $(LIB_FULLNAME)),)'#010+
  '        $(MKDIR) $(INSTALL_LIBDIR)'#010+
  '        $(INSTALL) $(LIB_FULLNAME) $(INSTALL_LIBDIR)'#010+
  'ifdef inUnix'#010+
  '        ln -sf $(LIB_FULLNAME) $(INSTALL_LIBDIR)/$(LIB_NA','ME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'ifdef INSTALL_FILES'#010+
  '        $(MKDIR) $(INSTALL_DATADIR)'#010+
  '        $(INSTALL) $(INSTALL_FILES) $(INSTALL_DATADIR)'#010+
  'endif'#010+
  #010+
  'fpc_sourceinstall: distclean'#010+
  '        $(MKDIR) $(INSTALL_SOURCEDIR)'#010+
  '        $(COPYTREE) $(BASEDIR) $(','INSTALL_SOURCEDIR)'#010+
  #010+
  'fpc_exampleinstall: $(addsuffix _distclean,$(TARGET_EXAMPLEDIRS))'#010+
  'ifdef HASEXAMPLES'#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDIR)'#010+
  'endif'#010+
  'ifdef EXAMPLESOURCEFILES'#010+
  '        $(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)'#010+
  'endif'#010,
  'ifdef TARGET_EXAMPLEDIRS'#010+
  '        $(COPYTREE) $(addsuffix /*,$(TARGET_EXAMPLEDIRS)) $(INSTALL_EX'+
  'AMPLEDIR)'#010+
  'endif'#010+
  #010+
  '[distinstallrules]'#010+
  '#####################################################################'#010+
  '# Dist Install'#010+
  '########################','############################################'+
  '#'#010+
  #010+
  '.PHONY: fpc_distinstall'#010+
  #010+
  'fpc_distinstall: install exampleinstall'#010+
  #010+
  #010+
  '[zipinstallrules]'#010+
  '#####################################################################'#010+
  '# Zip'#010+
  '################################','####################################'+
  '#'#010+
  #010+
  '.PHONY: fpc_zipinstall fpc_zipsourceinstall fpc_zipexampleinstall'#010+
  #010+
  '# Temporary path to pack a file, can only use a single deep'#010+
  '# subdir, because the deltree can'#039't see the whole tree to remove'#010+
  'ifndef PA','CKDIR'#010+
  'ifndef inUnix'#010+
  'PACKDIR=$(BASEDIR)/../fpc-pack'#010+
  'else'#010+
  'PACKDIR=/tmp/fpc-pack'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Maybe create default zipname from packagename'#010+
  'ifndef ZIPNAME'#010+
  'ifdef DIST_ZIPNAME'#010+
  'ZIPNAME=$(DIST_ZIPNAME)'#010+
  'else'#010+
  'ZIPNAME=$(ZIPPREFIX)$(PACKAGE_NAME)$(Z','IPSUFFIX)'#010+
  'endif'#010+
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
  '# Use a wrapper script b','y default for Os/2'#010+
  'ifndef inUnix'#010+
  'USEZIPWRAPPER=1'#010+
  'endif'#010+
  #010+
  '# We need to be able to run in the current OS so fix'#010+
  '# the path separator'#010+
  'ifdef USEZIPWRAPPER'#010+
  'ZIPPATHSEP=$(PATHSEP)'#010+
  'ZIPWRAPPER=$(subst /,$(PATHSEP),$(DIST_DESTDIR)/fpczip$(BATCHEXT))'#010+
  'e','lse'#010+
  'ZIPPATHSEP=/'#010+
  'endif'#010+
  #010+
  '# Create commands to create the zip/tar file'#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(ZIPPATHSEP),$(PACKDIR))'#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(ZIPPATHSEP),$(BASEDIR))'#010+
  'ifdef USETAR'#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(TAREXT)'#010+
  'Z','IPCMD_ZIP:=$(TARPROG) cf$(TAROPT) $(ZIPDESTFILE) *'#010+
  'else'#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(ZIPEXT)'#010+
  'ZIPCMD_ZIP:=$(subst /,$(ZIPPATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDES'+
  'TFILE) *'#010+
  'endif'#010+
  #010+
  'fpc_zipinstall:'#010+
  '        $(MAKE) $(ZIPTARGET) INST','ALL_PREFIX=$(PACKDIR) ZIPINSTALL=1'#010+
  '        $(MKDIR) $(DIST_DESTDIR)'#010+
  '        $(DEL) $(ZIPDESTFILE)'#010+
  'ifdef USEZIPWRAPPER'#010+
  '# Handle gecho separate as we need to espace \ with \\'#010+
  'ifneq ($(ECHOREDIR),echo)'#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(Z','IPCMD_CDPACK))" > $(ZIPWRAP'+
  'PER)'#010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_ZIP))" >> $(ZIPWRAPPER)'+
  #010+
  '        $(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDBASE))" >> $(ZIPWRAPP'+
  'ER)'#010+
  'else'#010+
  '        echo $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#010+
  '        echo',' $(ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#010+
  '        echo $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)'#010+
  'endif'#010+
  'ifdef inUnix'#010+
  '        /bin/sh $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ZIPWRAPPER)'#010+
  'endif'#010+
  '        $(DEL) $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(','ZIPCMD_CDBASE)'#010+
  'endif'#010+
  '        $(DELTREE) $(PACKDIR)'#010+
  #010+
  'fpc_zipsourceinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=src'#010+
  #010+
  'fpc_zipexampleinstall:'#010+
  'ifdef HASEXAMPLES'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall',' ZIPSUFFIX=ex'+
  'm'#010+
  'endif'#010+
  #010+
  'fpc_zipdistinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=distinstall'#010+
  #010+
  #010+
  '[cleanrules]'#010+
  '#####################################################################'#010+
  '# Clean rules'#010+
  '##############################################','######################'+
  '#'#010+
  #010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#010+
  #010+
  'ifdef EXEFILES'#010+
  'override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES'+
  '))'#010+
  'endif'#010+
  #010+
  'ifdef CLEAN_UNITS'#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UN','ITS))'#010+
  'endif'#010+
  #010+
  'ifdef CLEANPPUFILES'#010+
  'override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)'+
  ') $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(C'+
  'LEANPPUFILES)))'#010+
  'override CLEANPPUFILES:=$(addprefix $(UNITTARGETDIR','PREFIX),$(CLEANPP'+
  'UFILES))'#010+
  'override CLEANPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPREF'+
  'IX),$(CLEANPPULINKFILES)))'#010+
  'endif'#010+
  #010+
  'fpc_clean: $(CLEANTARGET)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  'ifdef CLEANPPUFILES'#010+
  '     ','   -$(DEL) $(CLEANPPUFILES)'#010+
  'endif'#010+
  'ifneq ($(CLEANPPULINKFILES),)'#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#010+
  'endif'#010+
  'ifdef CLEANRSTFILES'#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#010+
  'endif'#010+
  'ifdef CLEAN_FILES'#010+
  '        -$(DEL) $(CL','EAN_FILES)'#010+
  'endif'#010+
  'ifdef LIB_NAME'#010+
  '        -$(DEL) $(LIB_NAME) $(LIB_FULLNAME)'#010+
  'endif'#010+
  '        -$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(F'+
  'PCEXTFILE) $(REDIRFILE)'#010+
  #010+
  'fpc_distclean: clean'#010+
  #010+
  '# Also run clean first if targetdir is se','t. Unittargetdir is always'#010+
  '# set if targetdir or unittargetdir is specified'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  'TARGETDIRCLEAN=fpc_clean'#010+
  'endif'#010+
  #010+
  'fpc_cleanall: $(CLEANTARGET) $(TARGETDIRCLEAN)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'end','if'#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#010+
  '        -$(DELTREE) *$(SMARTEXT)'#010+
  '        -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTFILE) $(REDIRFILE)'+
  #010+
  'ifdef AOUTEXT'#010+
  '        -$(DEL) ','*$(AOUTEXT)'#010+
  'endif'#010+
  #010+
  #010+
  '[inforules]'#010+
  '#####################################################################'#010+
  '# Info rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_info'#010+
  #010+
  'fpc_info:'#010+
  '        @$(ECHO)'#010+
  '        @','$(ECHO)  == Package info =='#010+
  '        @$(ECHO)  Package Name..... $(PACKAGE_NAME)'#010+
  '        @$(ECHO)  Package Version.. $(PACKAGE_VERSION)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Configuration info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  FPC.......','... $(FPC)'#010+
  '        @$(ECHO)  FPC Version.. $(FPC_VERSION)'#010+
  '        @$(ECHO)  Source CPU... $(CPU_SOURCE)'#010+
  '        @$(ECHO)  Target CPU... $(CPU_TARGET)'#010+
  '        @$(ECHO)  Source OS.... $(OS_SOURCE)'#010+
  '        @$(ECHO)  Target OS.... $(OS_TARGET)'#010,
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Directory info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#010+
  '        @$(ECHO)  FPCDir.......... $(FPCDIR)'#010+
  '        @$(ECHO)  UnitsDir........ $(UNITSDIR)'#010+
  '        @$(ECHO)  PackagesDir','..... $(PACKAGESDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Tools info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Pwd....... $(PWD)',#010+
  '        @$(ECHO)  Mv........ $(MVPROG)'#010+
  '        @$(ECHO)  Cp........ $(CPPROG)'#010+
  '        @$(ECHO)  Rm........ $(RMPROG)'#010+
  '        @$(ECHO)  GInstall.. $(GINSTALL)'#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#010+
  '        @$(ECHO)  Date...... $(DATE)'#010+
  '       ',' @$(ECHO)  FPCMake... $(FPCMAKE)'#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#010+
  '        @$(ECHO)  Upx....... $(UPXPROG)'#010+
  '        @$(ECHO)  Zip....... $(ZIPPROG)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Object info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO) ',' Target Loaders...... $(TARGET_LOADERS)'#010+
  '        @$(ECHO)  Target Units........ $(TARGET_UNITS)'#010+
  '        @$(ECHO)  Target Programs..... $(TARGET_PROGRAMS)'#010+
  '        @$(ECHO)  Target Dirs......... $(TARGET_DIRS)'#010+
  '        @$(ECHO)  Target Examples','..... $(TARGET_EXAMPLES)'#010+
  '        @$(ECHO)  Target ExampleDirs.. $(TARGET_EXAMPLEDIRS)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Clean Units......... $(CLEAN_UNITS)'#010+
  '        @$(ECHO)  Clean Files......... $(CLEAN_FILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(E','CHO)  Install Units....... $(INSTALL_UNITS)'#010+
  '        @$(ECHO)  Install Files....... $(INSTALL_FILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Install info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#010+
  '        @$(ECHO)  ','ZipPrefix............ $(ZIPPREFIX)'#010+
  '        @$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)'#010+
  '        @$(ECHO)  Install FPC Package.. $(INSTALL_FPCPACKAGE)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)'#010+
  '        @$(ECH','O)  Install binary dir... $(INSTALL_BINDIR)'#010+
  '        @$(ECHO)  Install library dir.. $(INSTALL_LIBDIR)'#010+
  '        @$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)'#010+
  '        @$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)'#010+
  '        @$(ECHO)  ','Install doc dir...... $(INSTALL_DOCDIR)'#010+
  '        @$(ECHO)  Install example dir.. $(INSTALL_EXAMPLEDIR)'#010+
  '        @$(ECHO)  Install data dir..... $(INSTALL_DATADIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Dist destination dir. $(DIST_DESTDIR)'#010+
  '      ','  @$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[localmakefile]'#010+
  '#####################################################################'#010+
  '# Local Makefile'#010+
  '#####################################################################',
  #010+
  #010+
  'ifneq ($(wildcard fpcmake.loc),)'#010+
  'include fpcmake.loc'#010+
  'endif'#010+
  #010+
  '[userrules]'#010+
  '#####################################################################'#010+
  '# Users rules'#010+
  '#####################################################################'#010
);
