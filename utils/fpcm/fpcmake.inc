{$ifdef Delphi}
const fpcmakeini : array[0..118] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..118,1..240] of char=(
{$endif Delphi}
  ';'#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#010+
  ';'#010+
  #010+
  '[osdetect]'#010+
  '#####################################################################'#010+
  '# Autodetect OS (Linux or Dos or Windows NT or OS/2)'#010+
  '# define inUnix when running under',' Unix (Linux,FreeBSD)'#010+
  '# define inWinNT when running under WinNT'#010+
  '# define inOS2 when running under OS/2'#010+
  '#####################################################################'#010+
  #010+
  '# We need only / in the path'#010+
  'override PATH:=$(subst \,/,$(PATH))'#010+
  #010,
  '# Search for PWD and determine also if we are under linux'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(subst ;, ,$(PATH)))))'#010+
  'ifeq ($(PWD),)'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(subst :, ,$(PATH)))))'#010+
  'ifeq ($(PWD),)'#010+
  'nopwd:'#010+
  '        @ec','ho You need the GNU utils package to use this Makefile!'#010+
  '        @echo Get ftp://ftp.freepascal.org/pub/fpc/dist/go32v2/utilgo3'+
  '2.zip'#010+
  '        @exit'#010+
  'else'#010+
  'inUnix=1'#010+
  'PWD:=$(firstword $(PWD))'#010+
  'endif'#010+
  'else'#010+
  'PWD:=$(firstword $(PWD))'#010+
  'endif'#010+
  #010+
  '# Detect NT ','- NT sets OS to Windows_NT'#010+
  '# Detect OS/2 - OS/2 has OS2_SHELL defined'#010+
  'ifndef inUnix'#010+
  'ifeq ($(OS),Windows_NT)'#010+
  'inWinNT=1'#010+
  'else'#010+
  'ifdef OS2_SHELL'#010+
  'inOS2=1'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# The extension of executables'#010+
  'ifdef inUnix'#010+
  'SRCEXEEXT='#010+
  'else'#010+
  'SRCEXEEXT=.exe',#010+
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
  'endif'#010+
  'endif'#010+
  #010+
  '# Path Separator, the subst trick is necessary for the \ that can'#039't'+
  ' exists'#010+
  '# at the end of a line'#010+
  'ifde','f inUnix'#010+
  'PATHSEP=/'#010+
  'else'#010+
  'PATHSEP=$(subst /,\,/)'#010+
  'endif'#010+
  #010+
  '# The path which is searched separated by spaces'#010+
  'ifdef inUnix'#010+
  'SEARCHPATH=$(subst :, ,$(PATH))'#010+
  'else'#010+
  'SEARCHPATH=$(subst ;, ,$(PATH))'#010+
  'endif'#010+
  #010+
  '# Base dir'#010+
  'ifdef PWD'#010+
  'BASEDIR:=$(shell $(PWD))'#010+
  'el','se'#010+
  'BASEDIR=.'#010+
  'endif'#010+
  #010+
  '[fpcdetect]'#010+
  '#####################################################################'#010+
  '# FPC version/target Detection'#010+
  '#####################################################################'#010+
  #010+
  '# What compiler to use ?'#010+
  'ifndef FPC'#010,
  '# Compatibility with old makefiles'#010+
  'ifdef PP'#010+
  'FPC=$(PP)'#010+
  'else'#010+
  'FPC=ppc386'#010+
  'endif'#010+
  'endif'#010+
  'override FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#010+
  #010+
  '# Target OS'#010+
  'ifndef OS_TARGET'#010+
  'OS_TARGET:=$(shell $(FPC) -iTO)'#010+
  'endif'#010,
  #010+
  '# Source OS'#010+
  'ifndef OS_SOURCE'#010+
  'OS_SOURCE:=$(shell $(FPC) -iSO)'#010+
  'endif'#010+
  #010+
  '# Target CPU'#010+
  'ifndef CPU_TARGET'#010+
  'CPU_TARGET:=$(shell $(FPC) -iTP)'#010+
  'endif'#010+
  #010+
  '# Source CPU'#010+
  'ifndef CPU_SOURCE'#010+
  'CPU_SOURCE:=$(shell $(FPC) -iSP)'#010+
  'endif'#010+
  #010+
  '# FPC version'#010+
  'ifndef FPC_VERS','ION'#010+
  'FPC_VERSION:=$(shell $(FPC) -iV)'#010+
  'endif'#010+
  #010+
  'export FPC OS_TARGET OS_SOURCE CPU_TARGET CPU_SOURCE FPC_VERSION'#010+
  #010+
  #010+
  '[fpcdirdetect]'#010+
  '#####################################################################'#010+
  '# FPCDIR Setting'#010+
  '###########################','#########################################'+
  '#'#010+
  #010+
  '# Test FPCDIR to look if the RTL dir exists'#010+
  'ifdef FPCDIR'#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#010+
  'ifeq ($(wildcard $(FPCDIR)/rtl),)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  'e','ndif'#010+
  'else'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  #010+
  '# Detect FPCDIR'#010+
  'ifeq ($(FPCDIR),wrong)'#010+
  'ifdef inUnix'#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#010+
  'endif'#010+
  'else'#010+
  'override',' FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(a'+
  'ddsuffix /$(FPC),$(SEARCHPATH))))))'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildcard $(FPCDIR)/rtl),)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildca','rd $(FPCDIR)/rtl),)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=c:/pp'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'override FPCDIR:=$(wildcard $(FPCDIR))'#010+
  #010+
  '# Units dir'#010+
  'UNITSDIR=$(wildcard $(FPCDIR)/units/$(OS_TARGET))'#010+
  #010+
  '# Packages dir'#010+
  'PACKAGES','DIR=$(wildcard $(FPCDIR) $(FPCDIR)/packages)'#010+
  #010+
  #010+
  '[shelltools]'#010+
  '#####################################################################'#010+
  '# Shell tools'#010+
  '#####################################################################'#010+
  #010+
  '# echo'#010+
  'ifndef ECHO'#010+
  'ECHO:=','$(strip $(wildcard $(addsuffix /gecho$(EXEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=echo'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'endif'#010+
  'else'#010+
  'ECHO:=$(firstword ','$(ECHO))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To copy pograms'#010+
  'ifndef COPY'#010+
  'COPY:=cp -fp'#010+
  'endif'#010+
  #010+
  '# Copy a whole tree'#010+
  'ifndef COPYTREE'#010+
  'COPYTREE:=cp -rfp'#010+
  'endif'#010+
  #010+
  '# To move pograms'#010+
  'ifndef MOVE'#010+
  'MOVE:=mv -f'#010+
  'endif'#010+
  #010+
  '# Check delete program'#010+
  'ifndef DEL'#010+
  'DEL:=rm -f'#010+
  'endif'#010+
  #010+
  '# Chec','k deltree program'#010+
  'ifndef DELTREE'#010+
  'DELTREE:=rm -rf'#010+
  'endif'#010+
  #010+
  '# To install files'#010+
  'ifndef INSTALL'#010+
  'ifdef inUnix'#010+
  'INSTALL:=install -c -m 644'#010+
  'else'#010+
  'INSTALL:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To install programs'#010+
  'ifndef INSTALLEXE'#010+
  'ifdef inUnix'#010+
  'INSTALLEXE:=install -c',' -m 755'#010+
  'else'#010+
  'INSTALLEXE:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To make a directory.'#010+
  'ifndef MKDIR'#010+
  'ifdef inUnix'#010+
  'MKDIR:=install -m 755 -d'#010+
  'else'#010+
  'MKDIR:=ginstall -m 755 -d'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'export ECHO COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKDIR'#010+
  #010+
  #010+
  '[defau','lttools]'#010+
  '#####################################################################'#010+
  '# Default Tools'#010+
  '#####################################################################'#010+
  #010+
  '# assembler, redefine it if cross compiling'#010+
  'ifndef AS'#010+
  'AS=as'#010+
  'endif'#010+
  #010+
  '# linke','r, but probably not used'#010+
  'ifndef LD'#010+
  'LD=ld'#010+
  'endif'#010+
  #010+
  '# ppas.bat / ppas.sh'#010+
  'PPAS=$(BATCHEXT)'#010+
  #010+
  '# ldconfig to rebuild .so cache'#010+
  'ifdef inUnix'#010+
  'LDCONFIG=ldconfig'#010+
  'else'#010+
  'LDCONFIG='#010+
  'endif'#010+
  #010+
  '# ppumove'#010+
  'ifndef PPUMOVE'#010+
  'PPUMOVE:=$(strip $(wildcard $(addsuffix /pp','umove$(SRCEXEEXT),$(SEARC'+
  'HPATH))))'#010+
  'ifeq ($(PPUMOVE),)'#010+
  'PPUMOVE='#010+
  'else'#010+
  'PPUMOVE:=$(firstword $(PPUMOVE))'#010+
  'endif'#010+
  'endif'#010+
  'export PPUMOVE'#010+
  #010+
  '# ppufiles'#010+
  'ifndef PPUFILES'#010+
  'PPUFILES:=$(strip $(wildcard $(addsuffix /ppufiles$(SRCEXEEXT),$(SEARC'+
  'HPATH))))'#010+
  'ifeq',' ($(PPUFILES),)'#010+
  'PPUFILES='#010+
  'else'#010+
  'PPUFILES:=$(firstword $(PPUFILES))'#010+
  'endif'#010+
  'endif'#010+
  'export PPUFILES'#010+
  #010+
  '# gdate/date'#010+
  'ifndef DATE'#010+
  'DATE:=$(strip $(wildcard $(addsuffix /date$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(DATE),)'#010+
  'DATE:=$(strip $(wildcard $(addsu','ffix /gdate$(SRCEXEEXT),$(SEACHPATH)'+
  ')))'#010+
  'ifeq ($(DATE),)'#010+
  'DATE='#010+
  'else'#010+
  'DATE:=$(firstword $(DATE))'#010+
  'endif'#010+
  'else'#010+
  'DATE:=$(firstword $(DATE))'#010+
  'endif'#010+
  'endif'#010+
  'export DATE'#010+
  #010+
  'ifdef DATE'#010+
  'DATESTR:=$(shell $(DATE) +%Y%m%d)'#010+
  'else'#010+
  'DATESTR='#010+
  'endif'#010+
  #010+
  '# Look if UPX is ','found for go32v2 and win32. We can'#039't use $UPX b'+
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
  'UPXPROG:=$(strip $(wildcard $(addsuff','ix /upx$(SRCEXEEXT),$(SEARCHPAT'+
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
  '# ZipProg, you can'#039't use Zip as the var name (PFV)'#010+
  'ifndef ZIPPROG'#010+
  'ZIPPROG:=$(strip $(wild','card $(addsuffix /zip$(SRCEXEEXT),$(SEARCHPAT'+
  'H))))'#010+
  'ifeq ($(ZIPPROG),)'#010+
  'ZIPPROG='#010+
  'else'#010+
  'ZIPPROG:=$(firstword $(ZIPPROG))'#010+
  'endif'#010+
  'endif'#010+
  'export ZIPPROG'#010+
  #010+
  'ZIPOPT=-9'#010+
  'ZIPEXT=.zip'#010+
  #010+
  '# Tar'#010+
  'ifndef TARPROG'#010+
  'TARPROG:=$(strip $(wildcard $(addsuffix /tar$(SRCEX','EEXT),$(SEARCHPAT'+
  'H))))'#010+
  'ifeq ($(TARPROG),)'#010+
  'TARPROG='#010+
  'else'#010+
  'TARPROG:=$(firstword $(TARPROG))'#010+
  'endif'#010+
  'endif'#010+
  'export TARPROG'#010+
  #010+
  'ifeq ($(USETAR),bz2)'#010+
  'TAROPT=vI'#010+
  'TAREXT=.tar.bz2'#010+
  'else'#010+
  'TAROPT=vz'#010+
  'TAREXT=.tar.gz'#010+
  'endif'#010+
  #010+
  #010+
  '[extensions]'#010+
  '#########################','###########################################'+
  '#'#010+
  '# Default extensions'#010+
  '#####################################################################'#010+
  #010+
  '# Default needed extensions (Go32v2,Linux)'#010+
  'LOADEREXT=.as'#010+
  'EXEEXT=.exe'#010+
  'PPLEXT=.ppl'#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'AS','MEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.so'#010+
  'LIBPREFIX=lib'#010+
  'RSTEXT=.rst'#010+
  'FPCMADE=fpcmade'#010+
  #010+
  #010+
  '# Go32v1'#010+
  'ifeq ($(OS_TARGET),go32v1)'#010+
  'PPUEXT=.pp1'#010+
  'OEXT=.o1'#010+
  'ASMEXT=.s1'#010+
  'SMARTEXT=.sl1'#010+
  'STATICLIBEXT=.a1'#010+
  'SHAREDLIBEXT=.so1'#010+
  'LIBPREFIX='#010+
  'FPCMADE=fpcmad','e.v1'#010+
  'PACKAGESUFFIX=v1'#010+
  'endif'#010+
  #010+
  '# Go32v2'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'LIBPREFIX='#010+
  'FPCMADE=fpcmade.dos'#010+
  'ZIPSUFFIX=go32'#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'FPCMADE=fpcmade.lnx'#010+
  'ZIPSUFFIX=linux'#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TAR','GET),freebsd)'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'FPCMADE=fpcmade.freebsd'#010+
  'ZIPSUFFIX=freebsd'#010+
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
  'endif',#010+
  #010+
  '# OS/2'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'PPUEXT=.ppo'#010+
  'ASMEXT=.so2'#010+
  'OEXT=.oo2'#010+
  'SMARTEXT=.so'#010+
  'STATICLIBEXT=.ao2'#010+
  'SHAREDLIBEXT=.dll'#010+
  'FPCMADE=fpcmade.os2'#010+
  'ZIPSUFFIX=emx'#010+
  'endif'#010+
  #010+
  '[defaultdirs]'#010+
  '#####################################################################',
  #010+
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
  'override PACKAGESDIR+=$(REQ','UIRE_PACKAGESDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '# Linux and freebsd use unix dirs with /usr/bin, /usr/lib'#010+
  '# When zipping use the target as default, when normal install then'#010+
  '# use the source os as default'#010+
  'ifdef ZIPINSTALL'#010+
  '# Zipinstall'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'UN','IXINSTALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'else'#010+
  '# Normal install'#010+
  'ifeq ($(OS_SOURCE),linux)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_SOURCE),freebsd)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the prefix directory where to ins','tall everything'#010+
  'ifndef INSTALL_PREFIX'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_PREFIX=/usr/local'#010+
  'else'#010+
  'INSTALL_PREFIX=/pp'#010+
  'endif'#010+
  'endif'#010+
  'export INSTALL_PREFIX'#010+
  #010+
  '# Where to place the resulting zip files'#010+
  'ifndef DIST_DESTDIR'#010+
  'DIST_DESTDIR:=$(BASEDIR)'#010+
  'endif'#010+
  'expo','rt DIST_DESTDIR'#010+
  #010+
  #010+
  '#####################################################################'#010+
  '# Install Directories'#010+
  '#####################################################################'#010+
  #010+
  '# set the base directory where to install everything'#010+
  'ifndef',' INSTALL_BASEDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_BASEDIR=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)'#010+
  'else'#010+
  'INSTALL_BASEDIR=$(INSTALL_PREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the binaries'#010+
  'ifndef INSTALL_BINDIR'#010+
  'ifdef UNIXINSTALLDIR'#010,
  'INSTALL_BINDIR=$(INSTALL_PREFIX)/bin'#010+
  'else'#010+
  'INSTALL_BINDIR=$(INSTALL_BASEDIR)/bin/$(OS_TARGET)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the units.'#010+
  'ifndef INSTALL_UNITDIR'#010+
  'INSTALL_UNITDIR=$(INSTALL_BASEDIR)/units/$(OS_TARGET)'#010+
  'ifdef PAC','KAGE_NAME'#010+
  'INSTALL_UNITDIR:=$(INSTALL_UNITDIR)/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install shared libraries'#010+
  'ifndef INSTALL_LIBDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_LIBDIR=$(INSTALL_PREFIX)/lib'#010+
  'else'#010+
  'INSTALL_LIBDIR=$(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'endi','f'#010+
  #010+
  '# Where the source files will be stored'#010+
  'ifndef INSTALL_SOURCEDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_SOURCEDIR=$(INSTALL_PREFIX)/src/fpc-$(FPC_VERSION)'#010+
  'else'#010+
  'INSTALL_SOURCEDIR=$(INSTALL_BASEDIR)/source'#010+
  'endif'#010+
  'ifdef PACKAGE_NAME'#010+
  'INSTALL_SOURCEDIR:','=$(INSTALL_SOURCEDIR)/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the doc files will be stored'#010+
  'ifndef INSTALL_DOCDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_DOCDIR=$(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)'#010+
  'else'#010+
  'INSTALL_DOCDIR=$(INSTALL_BASEDIR)/doc'#010+
  'endif'#010+
  'endi','f'#010+
  #010+
  '# Where to install the examples, under linux we use the doc dir'#010+
  '# because the copytree command will create a subdir itself'#010+
  'ifndef INSTALL_EXAMPLEDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'INSTALL_EXAMPLEDIR=$(INSTALL_DOCDIR)/examples'#010+
  'else'#010+
  'INSTALL_EXAMPLED','IR=$(INSTALL_BASEDIR)/examples'#010+
  'endif'#010+
  'ifdef EXAMPLESUBDIR'#010+
  'INSTALL_EXAMPLEDIR:=$(INSTALL_EXAMPLEDIR)/$(EXAMPLESUBDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the some extra (data)files will be stored'#010+
  'ifndef INSTALL_DATADIR'#010+
  'INSTALL_DATADIR=$(INSTALL_BASEDIR)'#010+
  'end','if'#010+
  #010+
  #010+
  '[dirlibc]'#010+
  '# On linux, try to find where libgcc.a is.'#010+
  'ifdef inUnix'#010+
  'ifndef GCCLIBDIR'#010+
  'GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '#039'{ print'+
  ' $$4 } '#039'`)'#010+
  'endif'#010+
  'ifndef OTHERLIBDIR'#010+
  'OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf',' | awk '#039'{ ORS='+
  '" "; print $1 }'#039')'#010+
  'endif'#010+
  'export GCCLIBDIR OTHERLIB'#010+
  'endif'#010+
  #010+
  #010+
  '[command_begin]'#010+
  '#####################################################################'#010+
  '# Compiler Command Line'#010+
  '##########################################################','##########'+
  '#'#010+
  #010+
  '# Load commandline OPTDEF and add FPC_CPU define'#010+
  'override FPCOPTDEF=$(CPU_TARGET)'#010+
  #010+
  '# Load commandline OPT and add target and unit dir to be sure'#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#010+
  'override FPCOPT+=-T$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  '# User di','rs should be first, so they are looked at first'#010+
  'ifdef UNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(UNITDIR))'#010+
  'endif'#010+
  'ifdef LIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#010+
  'endif'#010+
  'ifdef OBJDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDIR))'#010+
  'endif'#010+
  'i','fdef INCDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(INCDIR))'#010+
  'endif'#010+
  #010+
  '# Smartlinking'#010+
  'ifdef LINKSMART'#010+
  'override FPCOPT+=-XX'#010+
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
  'overrid','e FPCOPTDEF+=DEBUG'#010+
  'endif'#010+
  #010+
  '# Release mode (strip, optimize and don'#039't load ppc386.cfg)'#010+
  'ifdef RELEASE'#010+
  'override FPCOPT+=-Xs -OG2p3 -n'#010+
  'override FPCOPTDEF+=RELEASE'#010+
  'endif'#010+
  #010+
  '# Strip'#010+
  'ifdef STRIP'#010+
  'override FPCOPT+=-Xs'#010+
  'endif'#010+
  #010+
  '# Optimizer'#010+
  'ifdef OPTIMIZE'#010,
  'override FPCOPT+=-OG2p3'#010+
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
  'override FP','COPT+=$(addprefix -Fu,$(COMPILER_UNITDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_LIBRARYDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARYDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_OBJECTDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))'#010+
  'endif'#010+
  'ifdef COMPILER_','INCLUDEDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(COMPILER_INCLUDEDIR))'#010+
  'endif'#010+
  #010+
  '# Target dirs and the prefix to use for clean/install'#010+
  'ifdef COMPILER_TARGETDIR'#010+
  'override FPCOPT+=-FE$(COMPILER_TARGETDIR)'#010+
  'ifeq ($(COMPILER_TARGETDIR),.)'#010+
  'override TARG','ETDIRPREFIX='#010+
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
  'els','e'#010+
  'override UNITTARGETDIRPREFIX=$(COMPILER_TARGETDIR)/'#010+
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
  '# For win32 the options are passed using the ','environment FPCEXTCMD'#010+
  'ifeq ($(OS_SOURCE),win32)'#010+
  'override FPCEXTCMD:=$(FPCOPT)'#010+
  'override FPCOPT:=!FPCEXTCMD'#010+
  'export FPCEXTCMD'#010+
  'endif'#010+
  #010+
  '# Compiler commandline'#010+
  'override COMPILER:=$(FPC) $(FPCOPT)'#010+
  #010+
  '# also call ppas if with command option -s'#010+
  '# but o','nly if the OS_SOURCE and OS_TARGET are equal'#010+
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
  '##############################################################','######'+
  '#'#010+
  '# Loaders'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_loaders'#010+
  #010+
  'ifdef TARGET_LOADERS'#010+
  'override ALLTARGET+=fpc_loaders'#010+
  'override CLEANTARGET+=fpc_loaders_clean'#010+
  'override INSTALLTARGET+=fpc_loaders_i','nstall'#010+
  #010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(TARGET_LOADERS))'#010+
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
  '     ','   $(MKDIR) $(INSTALL_UNITDIR)'#010+
  '        $(INSTALL) $(LOADEROFILES) $(INSTALL_UNITDIR)'#010+
  #010+
  #010+
  '[unitrules]'#010+
  '#####################################################################'#010+
  '# Units'#010+
  '###############################################################','#####'+
  '#'#010+
  #010+
  '.PHONY: fpc_units'#010+
  #010+
  'ifdef TARGET_UNITS'#010+
  'override ALLTARGET+=fpc_units'#010+
  #010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))'#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES)'#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES)'#010+
  'endif'#010+
  #010+
  'fpc_units: $(','UNITPPUFILES)'#010+
  #010+
  #010+
  '[exerules]'#010+
  '#####################################################################'#010+
  '# Exes'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_exes'#010+
  #010+
  'ifdef TARGET_PROGRAMS'#010+
  'override EXEFILES=$(addsu','ffix $(EXEEXT),$(TARGET_PROGRAMS))'#010+
  'override EXEOFILES:=$(addsuffix $(OEXT),$(TARGET_PROGRAMS)) $(addprefi'+
  'x $(LIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_PROGRAMS)))'#010+
  #010+
  'override ALLTARGET+=fpc_exes'#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#010+
  'ove','rride CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#010+
  'endif'#010+
  #010+
  'fpc_exes: $(EXEFILES)'#010+
  #010+
  #010+
  '[rstrules]'#010+
  '#####################################################################'#010+
  '# Resource strings'#010+
  '###############################################################','#####'+
  '#'#010+
  #010+
  'ifdef TARGET_RSTS'#010+
  'override RSTFILES=$(addsuffix $(RSTEXT),$(TARGET_RSTS))'#010+
  #010+
  'override CLEANRSTFILES+=$(RSTFILES)'#010+
  'endif'#010+
  #010+
  #010+
  '[examplerules]'#010+
  '#####################################################################'#010+
  '# Examples'#010+
  '#################','###################################################'+
  '#'#010+
  #010+
  '.PHONY: fpc_examples fpc_test'#010+
  #010+
  'ifdef TARGET_EXAMPLES'#010+
  'override EXAMPLESOURCEFILES:=$(wildcard $(addsuffix .pp,$(TARGET_EXAMP'+
  'LES)) $(addsuffix .pas,$(TARGET_EXAMPLES)))'#010+
  'override EXAMPLEFIL','ES:=$(addsuffix $(EXEEXT),$(TARGET_EXAMPLES))'#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(TARGET_EXAMPLES)) $(addp'+
  'refix $(LIBPREFIX),$(addsuffix $(STATICLIBEXT),$(TARGET_EXAMPLES)))'#010+
  #010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES',')'#010+
  'endif'#010+
  #010+
  'fpc_examples: all $(EXAMPLEFILES) $(addsuffix _all,$(TARGET_EXAMPLEDIR'+
  'S))'#010+
  #010+
  #010+
  '[compilerules]'#010+
  '#####################################################################'#010+
  '# General compile rules'#010+
  '##############################################','######################'+
  '#'#010+
  #010+
  '.PHONY: fpc_packages fpc_all fpc_smart fpc_debug'#010+
  #010+
  '$(FPCMADE): $(ALLTARGET)'#010+
  '        @$(ECHO) Compiled > $(FPCMADE)'#010+
  #010+
  'fpc_packages: $(COMPILEPACKAGES)'#010+
  #010+
  'fpc_all: fpc_packages $(FPCMADE)'#010+
  #010+
  'fpc_smart:'#010+
  '        $(MAKE) all L','INKSMART=1 CREATESMART=1'#010+
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
  '       ',' $(EXECPPAS)'#010+
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
  '# Search paths for .ppu and .pp and .pas'#010,
  'vpath %.pp $(COMPILER_SOURCEDIR) $(COMPILER_INCDIR)'#010+
  'vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCDIR)'#010+
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
  'ifdef PPUFILES'#010+
  'override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTALL'+
  'PPUFILES))'#010+
  'override INSTALLPPULINKFILES:=$(addprefi','x $(UNITTARGETDIRPREFIX),$(s'+
  'hell $(PPUFILES) -S -O $(INSTALLPPUFILES)))'#010+
  'else'#010+
  'override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFI'+
  'LES)) $(addprefix $(LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(INS'+
  'TALLPPUFILES)))'#010+
  'overrid','e INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTA'+
  'LLPPUFILES))'#010+
  'override INSTALLPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPR'+
  'EFIX),$(INSTALLPPULINKFILES)))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLEXEFILES'#010+
  'override INSTALLEXEFILES:=$(ad','dprefix $(TARGETDIRPREFIX),$(INSTALLEX'+
  'EFILES))'#010+
  'endif'#010+
  #010+
  #010+
  'fpc_install: $(INSTALLTARGET)'#010+
  '# Create UnitInstallFiles'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        $(MKDIR) $(INSTALL_BINDIR)'#010+
  '# Compress the exes if upx is defined'#010+
  'ifdef UPXPROG'#010+
  '        -$(UPXPROG) ','$(INSTALLEXEFILES)'#010+
  'endif'#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)'#010+
  'endif'#010+
  'ifdef INSTALLPPUFILES'#010+
  '        $(MKDIR) $(INSTALL_UNITDIR)'#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(INSTALL_UNITDIR)'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '   ','     $(INSTALL) $(INSTALLPPULINKFILES) $(INSTALL_UNITDIR)'#010+
  'endif'#010+
  'ifneq ($(wildcard $(LIB_FULLNAME)),)'#010+
  '        $(MKDIR) $(INSTALL_LIBDIR)'#010+
  '        $(INSTALL) $(LIB_FULLNAME) $(INSTALL_LIBDIR)'#010+
  'ifdef inUnix'#010+
  '        ln -sf $(LIB_FULLNAME) $(INSTA','LL_LIBDIR)/$(LIB_NAME)'#010+
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
  '        $(COPY','TREE) $(BASEDIR) $(INSTALL_SOURCEDIR)'#010+
  #010+
  'fpc_exampleinstall: $(addsuffix _distclean,$(TARGET_EXAMPLEDIRS))'#010+
  'ifdef EXAMPLESOURCEFILES'#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDIR)'#010+
  '        $(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)'#010+
  'endif'#010+
  'ifdef',' TARGET_EXAMPLEDIRS'#010+
  'ifndef EXAMPLESOURCEFILES'#010+
  '        $(MKDIR) $(INSTALL_EXAMPLEDIR)'#010+
  'endif'#010+
  '        $(COPYTREE) $(addsuffix /*,$(TARGET_EXAMPLEDIRS)) $(INSTALL_EX'+
  'AMPLEDIR)'#010+
  'endif'#010+
  #010+
  'fpc_distinstall: fpc_install fpc_exampleinstall'#010+
  #010+
  #010+
  '[zipinstallr','ules]'#010+
  '#####################################################################'#010+
  '# Zip'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_zipinstall fpc_zipsourceinstall fpc_zipexampleinstall'#010+
  #010+
  '# Temporary path to ','pack a file'#010+
  'ifndef PACKDIR'#010+
  'ifndef inUnix'#010+
  'PACKDIR=$(BASEDIR)/../pack_tmp/$(PACKAGE_NAME)'#010+
  'else'#010+
  'PACKDIR=/tmp/fpc-pack/$(PACKAGE_NAME)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Maybe create default zipname from packagename'#010+
  'ifndef ZIPNAME'#010+
  'ifdef DIST_ZIPNAME'#010+
  'ZIPNAME=$(DIST','_ZIPNAME)'#010+
  'else'#010+
  'ZIPNAME=$(ZIPPREFIX)$(PACKAGE_NAME)$(ZIPSUFFIX)'#010+
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
  'ifdef ','inUnix'#010+
  'USETAR=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Use a wrapper script by default for Os/2'#010+
  'ifdef inOS2'#010+
  'USEZIPWRAPPER=1'#010+
  'endif'#010+
  #010+
  '# Create commands to create the zip/tar file'#010+
  'ZIPWRAPPER=$(DIST_DESTDIR)/fpczip$(BATCHEXT)'#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(PATHSEP),$(PA','CKDIR))'#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(PATHSEP),$(BASEDIR))'#010+
  'ifdef USETAR'#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(TAREXT)'#010+
  'ZIPCMD_ZIP:=$(TARPROG) cf$(TAROPT) $(ZIPDESTFILE) *'#010+
  'else'#010+
  'ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(ZIPEXT)'#010+
  'ZIPCMD_ZIP:=$','(subst /,$(PATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDEST'+
  'FILE) *'#010+
  'endif'#010+
  #010+
  'fpc_zipinstall:'#010+
  '        $(MAKE) $(ZIPTARGET) INSTALL_PREFIX=$(PACKDIR) ZIPINSTALL=1'#010+
  '        $(DEL) $(ZIPDESTFILE)'#010+
  'ifdef USEZIPWRAPPER'#010+
  'ifneq ($(ECHO),echo)'#010+
  '        $(ECHO)',' "$(ZIPCMD_CDPACK)" > $(ZIPWRAPPER)'#010+
  '        $(ECHO) "$(ZIPCMD_ZIP)" >> $(ZIPWRAPPER)'#010+
  '        $(ECHO) "$(ZIPCMD_CDBASE)" >> $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ECHO) $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#010+
  '        $(ECHO) $(ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#010+
  '  ','      $(ECHO) $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)'#010+
  'endif'#010+
  'ifdef inUnix'#010+
  '        /bin/sh $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ZIPWRAPPER)'#010+
  'endif'#010+
  '        $(DEL) $(ZIPWRAPPER)'#010+
  'else'#010+
  '        $(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(ZIPCMD_CDBASE)'#010+
  'endif'#010+
  '        $(','DELTREE) $(PACKDIR)'#010+
  #010+
  'fpc_zipsourceinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=src'#010+
  #010+
  'fpc_zipexampleinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall ZIPSUFFIX=exm'#010+
  #010+
  'fpc_zipdistinstall:'#010+
  '        $(MAK','E) fpc_zipinstall ZIPTARGET=distinstall'#010+
  #010+
  #010+
  '[cleanrules]'#010+
  '#####################################################################'#010+
  '# Clean rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_clean fpc_cleanall',' fpc_distclean'#010+
  #010+
  'ifdef EXEFILES'#010+
  'override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES'+
  '))'#010+
  'endif'#010+
  #010+
  'ifdef CLEAN_UNITS'#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS))'#010+
  'endif'#010+
  #010+
  'ifdef CLEANPPUFILES'#010+
  'override CLEANPPUFILES',':=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPP'+
  'UFILES))'#010+
  '# Get the .o and .a files created for the units'#010+
  'ifdef PPUFILES'#010+
  'CLEANPPULINKFILES:=$(shell $(PPUFILES) $(CLEANPPUFILES))'#010+
  'else'#010+
  'CLEANPPULINKFILES:=$(wildcard $(subst $(PPUEXT),$(OEXT),$(CL','EANPPUFI'+
  'LES)) $(addprefix $(LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLE'+
  'ANPPUFILES))))'#010+
  'endif'#010+
  'override CLEANPPULINKFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEAN'+
  'PPULINKFILES))'#010+
  'endif'#010+
  #010+
  'fpc_clean: $(CLEANTARGET)'#010+
  'ifdef CLEANEXEFILES'#010+
  '   ','     -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  'ifdef CLEANPPUFILES'#010+
  '        -$(DEL) $(CLEANPPUFILES)'#010+
  'endif'#010+
  'ifneq ($(CLEANPPULINKFILES),)'#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#010+
  'endif'#010+
  'ifdef CLEANRSTFILES'#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$','(CLEANRSTFILES))'+
  #010+
  'endif'#010+
  'ifdef CLEAN_FILES'#010+
  '        -$(DEL) $(CLEAN_FILES)'#010+
  'endif'#010+
  'ifdef LIB_NAME'#010+
  '        -$(DEL) $(LIB_NAME) $(LIB_FULLNAME)'#010+
  'endif'#010+
  '        -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTFILE) $(REDIRFILE)'+
  #010+
  #010+
  'fpc_distclean: fpc_clean'#010,
  #010+
  '# Also run clean first if targetdir is set. Unittargetdir is always'#010+
  '# set if targetdir or unittargetdir is specified'#010+
  'ifdef COMPILER_UNITTARGETDIR'#010+
  'TARGETDIRCLEAN=fpc_clean'#010+
  'endif'#010+
  #010+
  'fpc_cleanall: $(CLEANTARGET) $(TARGETDIRCLEAN)'#010+
  'ifdef CLEANEXE','FILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#010+
  '        -$(DELTREE) *$(SMARTEXT)'#010+
  '        -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTFILE) $','(REDIRFIL'+
  'E)'#010+
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
  '        @$(ECHO',')  == Package info =='#010+
  '        @$(ECHO)  Package Name..... $(PACKAGE_NAME)'#010+
  '        @$(ECHO)  Package Version.. $(PACKAGE_VERSION)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Configuration info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  FPC.......... $(','FPC)'#010+
  '        @$(ECHO)  FPC Version.. $(FPC_VERSION)'#010+
  '        @$(ECHO)  Source CPU... $(CPU_SOURCE)'#010+
  '        @$(ECHO)  Target CPU... $(CPU_TARGET)'#010+
  '        @$(ECHO)  Source OS.... $(OS_SOURCE)'#010+
  '        @$(ECHO)  Target OS.... $(OS_TARGET)'#010+
  '      ','  @$(ECHO)'#010+
  '        @$(ECHO)  == Directory info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#010+
  '        @$(ECHO)  FPCDir.......... $(FPCDIR)'#010+
  '        @$(ECHO)  UnitsDir........ $(UNITSDIR)'#010+
  '        @$(ECHO)  PackagesDir..... ','$(PACKAGESDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Tools info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Pwd....... $(PWD)'#010+
  '     ','   @$(ECHO)  Echo...... $(ECHO)'#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#010+
  '        @$(ECHO)  PPUFiles.. $(PPUFILES)'#010+
  '        @$(ECHO)  Date...... $(DATE)'#010+
  '        @$(ECHO)  Upx....... $(UPXPROG)'#010+
  '        @$(ECHO)  Zip....... $(ZIPPROG)'#010+
  '        @$','(ECHO)'#010+
  '        @$(ECHO)  == Object info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Target Loaders...... $(TARGET_LOADERS)'#010+
  '        @$(ECHO)  Target Units........ $(TARGET_UNITS)'#010+
  '        @$(ECHO)  Target Programs..... $(TARGET_PROGRAMS)'#010+
  '        @$','(ECHO)  Target Dirs......... $(TARGET_DIRS)'#010+
  '        @$(ECHO)  Target Examples..... $(TARGET_EXAMPLES)'#010+
  '        @$(ECHO)  Target ExampleDirs.. $(TARGET_EXAMPLEDIRS)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Clean Units......... $(CLEAN_UNITS)'#010+
  '      ','  @$(ECHO)  Clean Files......... $(CLEAN_FILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Install Units....... $(INSTALL_UNITS)'#010+
  '        @$(ECHO)  Install Files....... $(INSTALL_FILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Install info =='#010+
  '        @','$(ECHO)'#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#010+
  '        @$(ECHO)  ZipPrefix............ $(ZIPPREFIX)'#010+
  '        @$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  INSTALL_BASEDIR...... $(INSTALL_BASEDIR)',#010+
  '        @$(ECHO)  INSTALL_BINDIR....... $(INSTALL_BINDIR)'#010+
  '        @$(ECHO)  INSTALL_LIBDIR....... $(INSTALL_LIBDIR)'#010+
  '        @$(ECHO)  INSTALL_UNITDIR...... $(INSTALL_UNITDIR)'#010+
  '        @$(ECHO)  INSTALL_SOURCEDIR.... $(INSTALL_SOURCEDIR)'#010+
  '   ','     @$(ECHO)  INSTALL_DOCDIR....... $(INSTALL_DOCDIR)'#010+
  '        @$(ECHO)  INSTALL_DATADIR...... $(INSTALL_DATADIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  DIST_DESTDIR......... $(DIST_DESTDIR)'#010+
  '        @$(ECHO)  DIST_ZIPNAME......... $(DIST_ZIPNAM','E)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[localmakefile]'#010+
  '#####################################################################'#010+
  '# Local Makefile'#010+
  '#####################################################################'#010+
  #010+
  'ifneq ($(wildcard fpcmake.loc),)'#010+
  'include fpcm','ake.loc'#010+
  'endif'#010+
  #010+
  '[userrules]'#010+
  '#####################################################################'#010+
  '# Users rules'#010+
  '#####################################################################'#010
);
