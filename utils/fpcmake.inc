{$ifdef Delphi}
const fpcmakeini : array[0..134] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..134,1..240] of char=(
{$endif Delphi}
  ';'#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#010+
  ';'#010+
  #010+
  '[osdetect]'#010+
  '#####################################################################'#010+
  '# Autodetect OS (Linux or Dos or Windows NT)'#010+
  '# define inUnix when running under Unix (L','inux,FreeBSD)'#010+
  '# define inWinNT when running under WinNT'#010+
  '#####################################################################'#010+
  #010+
  '# We need only / in the path'#010+
  'override PATH:=$(subst \,/,$(PATH))'#010+
  #010+
  '# Search for PWD and determine also if we are u','nder linux'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(subst ;, ,$(PATH)))))'#010+
  'ifeq ($(PWD),)'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(subst :, ,$(PATH)))))'#010+
  'ifeq ($(PWD),)'#010+
  'nopwd:'#010+
  '        @echo You need the GNU utils package to use this M','akefile!'#010+
  '        @echo Get ftp://ftp.freepascal.org/pub/fpc/dist/go32v2/utilgo3'+
  '2.zip'#010+
  '        @exit'#010+
  'else'#010+
  'inUnix=1'#010+
  'endif'#010+
  'else'#010+
  'PWD:=$(firstword $(PWD))'#010+
  'endif'#010+
  #010+
  '# Detect NT - NT sets OS to Windows_NT'#010+
  '# Detect OS/2 - OS/2 has OS2_SHELL defined'#010+
  'if','ndef inUnix'#010+
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
  'SRCEXEEXT=.exe'#010+
  'endif'#010+
  #010+
  '# The path which is searched separated by spaces'#010+
  'ifdef inUnix'#010+
  'SE','ARCHPATH=$(subst :, ,$(PATH))'#010+
  'else'#010+
  'SEARCHPATH=$(subst ;, ,$(PATH))'#010+
  'endif'#010+
  #010+
  '# Base dir'#010+
  'ifdef PWD'#010+
  'BASEDIR:=$(shell $(PWD))'#010+
  'else'#010+
  'BASEDIR=.'#010+
  'endif'#010+
  #010+
  #010+
  '[defaulttarget]'#010+
  '#####################################################################'#010+
  '# Default t','arget'#010+
  '#####################################################################'#010+
  #010+
  '[fpcdetect]'#010+
  '#####################################################################'#010+
  '# FPC version/target Detection'#010+
  '##################################################','##################'+
  '#'#010+
  #010+
  '# What compiler to use ?'#010+
  'ifndef FPC'#010+
  '# Compatibility with old makefiles'#010+
  'ifdef PP'#010+
  'FPC=$(PP)'#010+
  'else'#010+
  'ifdef inOS2'#010+
  'FPC=ppos2'#010+
  'else'#010+
  'FPC=ppc386'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'override FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#010+
  'override FPC:=$(subst \,','/,$(FPC))$(SRCEXEEXT)'#010+
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
  'ifndef CPU_TARGET'#010+
  'CPU_TARGET:=$(shell $(FPC) -iTP)'#010+
  'endif'#010+
  #010+
  '# Source C','PU'#010+
  'ifndef CPU_SOURCE'#010+
  'CPU_SOURCE:=$(shell $(FPC) -iSP)'#010+
  'endif'#010+
  #010+
  '# FPC version'#010+
  'ifndef FPC_VERSION'#010+
  'FPC_VERSION:=$(shell $(FPC) -iV)'#010+
  'endif'#010+
  #010+
  'export FPC OS_TARGET OS_SOURCE CPU_TARGET CPU_SOURCE FPC_VERSION'#010+
  #010+
  #010+
  '[fpcdircheckenv]'#010+
  '######################','##############################################'+
  '#'#010+
  '# FPCDIR Setting'#010+
  '#####################################################################'#010+
  #010+
  '# Test FPCDIR to look if the RTL dir exists'#010+
  'ifdef FPCDIR'#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#010+
  'ifeq ($','(wildcard $(FPCDIR)/rtl),)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  'endif'#010+
  'else'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  #010+
  '[fpcdirdetect]'#010+
  '# Detect FPCDIR'#010+
  'ifeq ($(FPCDIR),wrong)'#010+
  'ifdef inUnix'#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_V','ERSION)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#010+
  'endif'#010+
  'else'#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC),$(SEARCHPATH))))))'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($','(wildcard $(FPCDIR)/rtl),)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR:=$(FPCDIR)/..'#010+
  'ifeq ($(wildcard $(FPCDIR)/rtl),)'#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#010+
  'override FPCDIR=c:/pp'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[fpcdirsubs]'#010+
  '# Cre','ate units dir'#010+
  'ifneq ($(FPCDIR),.)'#010+
  'UNITSDIR=$(FPCDIR)/units/$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  '[checkfpcdirsubs]'#010+
  '# Check if the dirs really exists, else turn it off'#010+
  'ifeq ($(wildcard $(UNITSDIR)),)'#010+
  'UNITSDIR='#010+
  'endif'#010+
  'ifeq ($(wildcard $(TOOLKITSDIR)),)'#010+
  'TOOLKITS','DIR='#010+
  'endif'#010+
  'ifeq ($(wildcard $(PACKAGESDIR)),)'#010+
  'PACKAGESDIR='#010+
  'endif'#010+
  'ifeq ($(wildcard $(COMPONENTSDIR)),)'#010+
  'COMPONENTSDIR='#010+
  'endif'#010+
  #010+
  #010+
  '[presettings]'#010+
  '#####################################################################'#010+
  '# Pre Settings'#010+
  '################','####################################################'+
  '#'#010+
  #010+
  #010+
  '[usersettings]'#010+
  '#####################################################################'#010+
  '# User Settings'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[postsettin','gs]'#010+
  '#####################################################################'#010+
  '# Post Settings'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[shelltools]'#010+
  '#################################################################','###'+
  '#'#010+
  '# Shell tools'#010+
  '#####################################################################'#010+
  #010+
  '# echo'#010+
  'ifndef ECHO'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho$(EXEEXT),$(SEARCHPATH))))'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /echo$(S','RCEXEEXT),$(SEARCHPATH)'+
  ')))'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=echo'#010+
  'ECHOE:=echo'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'ECHOE=$(ECHO) -E'#010+
  'endif'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'ECHOE=$(ECHO) -E'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To copy pograms'#010+
  'ifndef COPY'#010+
  'COPY:=cp -fp'#010+
  'endif'#010+
  #010+
  '# Copy ','a whole tree'#010+
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
  '# Check deltree program'#010+
  'ifndef DELTREE'#010+
  'DELTREE:=rm -rf'#010+
  'endif'#010+
  #010+
  '# To install files'#010+
  'ifnd','ef INSTALL'#010+
  'ifdef inUnix'#010+
  'INSTALL:=install -m 644'#010+
  'else'#010+
  'INSTALL:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To install programs'#010+
  'ifndef INSTALLEXE'#010+
  'ifdef inUnix'#010+
  'INSTALLEXE:=install -m 755'#010+
  'else'#010+
  'INSTALLEXE:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To make a directory.'#010+
  'ifndef MKDIR'#010+
  'ifd','ef inUnix'#010+
  'MKDIR:=install -m 755 -d'#010+
  'else'#010+
  'MKDIR:=ginstall -m 755 -d'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'export ECHO ECHOE COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MK'+
  'DIR'#010+
  #010+
  #010+
  '[tool_default]'#010+
  '#####################################################################'#010,
  '# Default Tools'#010+
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
  '# ppas.bat / ppas.sh'#010+
  'ifdef inUn','ix'#010+
  'PPAS=ppas.sh'#010+
  'else'#010+
  'ifdef inOS2'#010+
  'PPAS=ppas.cmd'#010+
  'else'#010+
  'PPAS=ppas.bat'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# ldconfig to rebuild .so cache'#010+
  'ifdef inUnix'#010+
  'LDCONFIG=ldconfig'#010+
  'else'#010+
  'LDCONFIG='#010+
  'endif'#010+
  #010+
  '[tool_ppdep]'#010+
  '# ppdep'#010+
  'ifndef PPDEP'#010+
  'PPDEP:=$(strip $(wildcard $(addsuffix /ppd','ep$(SRCEXEEXT),$(SEARCHPAT'+
  'H))))'#010+
  'ifeq ($(PPDEP),)'#010+
  'PPDEP='#010+
  'else'#010+
  'PPDEP:=$(firstword $(PPDEP))'#010+
  'endif'#010+
  'endif'#010+
  'export PPDEP'#010+
  #010+
  '[tool_ppumove]'#010+
  '# ppumove'#010+
  'ifndef PPUMOVE'#010+
  'PPUMOVE:=$(strip $(wildcard $(addsuffix /ppumove$(SRCEXEEXT),$(SEARCHP'+
  'ATH))))'#010+
  'ifeq (','$(PPUMOVE),)'#010+
  'PPUMOVE='#010+
  'else'#010+
  'PPUMOVE:=$(firstword $(PPUMOVE))'#010+
  'endif'#010+
  'endif'#010+
  'export PPUMOVE'#010+
  #010+
  '[tool_ppufiles]'#010+
  '# ppufiles'#010+
  'ifndef PPUFILES'#010+
  'PPUFILES:=$(strip $(wildcard $(addsuffix /ppufiles$(SRCEXEEXT),$(SEARC'+
  'HPATH))))'#010+
  'ifeq ($(PPUFILES),)'#010+
  'PPUFILES=',#010+
  'else'#010+
  'PPUFILES:=$(firstword $(PPUFILES))'#010+
  'endif'#010+
  'endif'#010+
  'export PPUFILES'#010+
  #010+
  '[tool_data2inc]'#010+
  '# data2inc'#010+
  'ifndef DATA2INC'#010+
  'DATA2INC:=$(strip $(wildcard $(addsuffix /data2inc$(SRCEXEEXT),$(SEARC'+
  'HPATH))))'#010+
  'ifeq ($(DATA2INC),)'#010+
  'DATA2INC='#010+
  'else'#010+
  'DATA2INC:=$(','firstword $(DATA2INC))'#010+
  'endif'#010+
  'endif'#010+
  'export DATA2INC'#010+
  #010+
  '[tool_diff]'#010+
  '# diff'#010+
  'ifndef DIFF'#010+
  'DIFF:=$(strip $(wildcard $(addsuffix /diff$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(DIFF),)'#010+
  'DIFF='#010+
  'else'#010+
  'DIFF:=$(firstword $(DIFF))'#010+
  'endif'#010+
  'endif'#010+
  'export DIFF'#010+
  #010+
  '[tool_','cmp]'#010+
  '# cmp'#010+
  'ifndef CMP'#010+
  'CMP:=$(strip $(wildcard $(addsuffix /cmp$(SRCEXEEXT),$(SEARCHPATH))))'#010+
  'ifeq ($(CMP),)'#010+
  'CMP='#010+
  'else'#010+
  'CMP:=$(firstword $(CMP))'#010+
  'endif'#010+
  'endif'#010+
  'export CMP'#010+
  #010+
  '[tool_sed]'#010+
  '# Sed'#010+
  'ifndef SED'#010+
  'SED:=$(strip $(wildcard $(addsuffix /sed$(SRCE','XEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(SED),)'#010+
  'SED='#010+
  'else'#010+
  'SED:=$(firstword $(SED))'#010+
  'endif'#010+
  'endif'#010+
  'export SED'#010+
  #010+
  '[tool_upx]'#010+
  '# Look if UPX is found for go32v2 and win32. We can'#039't use $UPX bec'+
  'uase'#010+
  '# upx uses that one itself (PFV)'#010+
  'ifndef UPXPROG'#010+
  'ifeq ($(OS_T','ARGET),go32v2)'#010+
  'UPXPROG:=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'UPXPROG:=1'#010+
  'endif'#010+
  'ifdef UPXPROG'#010+
  'UPXPROG:=$(strip $(wildcard $(addsuffix /upx$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#010+
  'ifeq ($(UPXPROG),)'#010+
  'UPXPROG='#010+
  'else'#010+
  'UPXPROG:=$(firstword $(UPXPROG))'#010+
  'endif'#010+
  'else'#010,
  'UPXPROG='#010+
  'endif'#010+
  'endif'#010+
  'export UPXPROG'#010+
  #010+
  '[tool_date]'#010+
  '# gdate/date'#010+
  'ifndef DATE'#010+
  'DATE:=$(strip $(wildcard $(addsuffix /date$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#010+
  'ifeq ($(DATE),)'#010+
  'DATE:=$(strip $(wildcard $(addsuffix /gdate$(SRCEXEEXT),$(SEACHPATH)))'+
  ')'#010+
  'ifeq (','$(DATE),)'#010+
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
  '[tool_zip]'#010+
  '# ZipProg, you can'#039't use Zip as the var name (PFV)'#010+
  'i','fndef ZIPPROG'#010+
  'ZIPPROG:=$(strip $(wildcard $(addsuffix /zip$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#010+
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
  '[tool_tar]'#010+
  '# Tar'#010+
  'ifndef TARPROG'#010+
  'TAR','PROG:=$(strip $(wildcard $(addsuffix /tar$(SRCEXEEXT),$(SEARCHPAT'+
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
  'TAREXT=.tar.g','z'#010+
  'endif'#010+
  #010+
  #010+
  '[extensions]'#010+
  '#####################################################################'#010+
  '# Default extensions'#010+
  '#####################################################################'#010+
  #010+
  '# Default needed extensions (Go32v2,Linux)'#010+
  'LOADEREXT=.a','s'#010+
  'EXEEXT=.exe'#010+
  'PPLEXT=.ppl'#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'ASMEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.so'#010+
  'RSTEXT=.rst'#010+
  'FPCMADE=fpcmade'#010+
  #010+
  '# Go32v1'#010+
  'ifeq ($(OS_TARGET),go32v1)'#010+
  'PPUEXT=.pp1'#010+
  'OEXT=.o1'#010+
  'ASMEXT=.s1'#010+
  'SMARTEXT=.sl1'#010+
  'STATICLIBEXT=.a1'#010+
  'SHAREDLIBE','XT=.so1'#010+
  'FPCMADE=fpcmade.v1'#010+
  'endif'#010+
  #010+
  '# Go32v2'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'FPCMADE=fpcmade.dos'#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010+
  'FPCMADE=fpcmade.lnx'#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'EXEEXT='#010+
  'HASSHAREDLIB=1'#010,
  'FPCMADE=fpcmade.freebsd'#010+
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
  'endif'#010+
  #010+
  '# OS/2'#010+
  'ifeq ($(OS_TARGET),os2)'#010+
  'PPUEXT=.ppo'#010+
  'ASMEXT=.so2'#010+
  'OEXT=.oo2'#010+
  'SM','ARTEXT=.so'#010+
  'STATICLIBEXT=.ao2'#010+
  'SHAREDLIBEXT=.dll'#010+
  'FPCMADE=fpcmade.os2'#010+
  'endif'#010+
  #010+
  '# library prefix'#010+
  'LIBPREFIX=lib'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'LIBPREFIX='#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),go32v1)'#010+
  'LIBPREFIX='#010+
  'endif'#010+
  #010+
  '# determine which .pas extension is used'#010+
  'ifnde','f PASEXT'#010+
  'ifdef EXEOBJECTS'#010+
  'override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstword $(EX'+
  'EOBJECTS)))))'#010+
  'else'#010+
  'override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstword $(UN'+
  'ITOBJECTS)))))'#010+
  'endif'#010+
  'ifeq ($(TESTPAS),)'#010+
  'PASEXT=.pp'#010+
  'else',#010+
  'PASEXT=.pas'#010+
  'endif'#010+
  'endif'#010+
  #010+
  #010+
  '[dir_default]'#010+
  '#####################################################################'#010+
  '# Default Directories'#010+
  '#####################################################################'#010+
  #010+
  '# Linux and freebsd use unix dirs wi','th /usr/bin, /usr/lib'#010+
  'ifeq ($(OS_TARGET),linux)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),freebsd)'#010+
  'UNIXINSTALLDIR=1'#010+
  'endif'#010+
  #010+
  '# set the prefix directory where to install everything'#010+
  'ifndef PREFIXINSTALLDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'PREFIXINSTALLDIR=','/usr'#010+
  'else'#010+
  'PREFIXINSTALLDIR=/pp'#010+
  'endif'#010+
  'endif'#010+
  'export PREFIXINSTALLDIR'#010+
  #010+
  '# Where to place the resulting zip files'#010+
  'ifndef DESTZIPDIR'#010+
  'DESTZIPDIR:=$(BASEDIR)'#010+
  'endif'#010+
  'export DESTZIPDIR'#010+
  #010+
  #010+
  '[dir_install]'#010+
  '##################################################','##################'+
  '#'#010+
  '# Install Directories'#010+
  '#####################################################################'#010+
  #010+
  '# set the base directory where to install everything'#010+
  'ifndef BASEINSTALLDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'BASEINSTALLDIR=$(PREFIXINSTALLD','IR)/lib/fpc/$(FPC_VERSION)'#010+
  'else'#010+
  'BASEINSTALLDIR=$(PREFIXINSTALLDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the binaries'#010+
  'ifndef BININSTALLDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'BININSTALLDIR=$(PREFIXINSTALLDIR)/bin'#010+
  'else'#010+
  'BININSTALLDIR=$(BASEINSTA','LLDIR)/bin/$(OS_TARGET)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the units.'#010+
  'ifndef UNITINSTALLDIR'#010+
  'UNITINSTALLDIR=$(BASEINSTALLDIR)/units/$(OS_TARGET)'#010+
  'ifdef UNITSUBDIR'#010+
  'UNITINSTALLDIR:=$(UNITINSTALLDIR)/$(UNITSUBDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# W','here to install shared libraries'#010+
  'ifndef LIBINSTALLDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'LIBINSTALLDIR=$(PREFIXINSTALLDIR)/lib'#010+
  'else'#010+
  'LIBINSTALLDIR=$(UNITINSTALLDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the source files will be stored'#010+
  'ifndef SOURCEINSTALLDIR'#010+
  'ifdef UNIXINS','TALLDIR'#010+
  'SOURCEINSTALLDIR=$(PREFIXINSTALLDIR)/src/fpc-$(FPC_VERSION)'#010+
  'else'#010+
  'SOURCEINSTALLDIR=$(BASEINSTALLDIR)/source'#010+
  'endif'#010+
  'ifdef SOURCESUBDIR'#010+
  'SOURCEINSTALLDIR:=$(SOURCEINSTALLDIR)/$(SOURCESUBDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the doc files will be sto','red'#010+
  'ifndef DOCINSTALLDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'DOCINSTALLDIR=$(PREFIXINSTALLDIR)/doc/fpc-$(FPC_VERSION)'#010+
  'else'#010+
  'DOCINSTALLDIR=$(BASEINSTALLDIR)/doc'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install the examples, under linux we use the doc dir'#010+
  '# because the copyt','ree command will create a subdir itself'#010+
  'ifndef EXAMPLEINSTALLDIR'#010+
  'ifdef UNIXINSTALLDIR'#010+
  'EXAMPLEINSTALLDIR=$(DOCINSTALLDIR)/examples'#010+
  'else'#010+
  'EXAMPLEINSTALLDIR=$(BASEINSTALLDIR)/examples'#010+
  'endif'#010+
  'ifdef EXAMPLESUBDIR'#010+
  'EXAMPLEINSTALLDIR:=$(EXAMPLEINSTAL','LDIR)/$(EXAMPLESUBDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the some extra (data)files will be stored'#010+
  'ifndef DATAINSTALLDIR'#010+
  'DATAINSTALLDIR=$(BASEINSTALLDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[dir_gcclib]'#010+
  '# On linux, try to find where libgcc.a is.'#010+
  'ifdef inUnix'#010+
  'ifndef GCCLIBDIR'#010+
  'GCCLI','BDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '#039'{ pri'+
  'nt $$4 } '#039'`)'#010+
  'endif'#010+
  'endif'#010+
  'export GCCLIBDIR'#010+
  #010+
  #010+
  '[dir_otherlib]'#010+
  '# Where to find other libraries'#010+
  'ifdef inUnix'#010+
  'ifndef OTHERLIBDIR'#010+
  'OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | awk '#039,'{ ORS="'+
  ' "; print $1 }'#039')'#010+
  'endif'#010+
  'endif'#010+
  'export OTHERLIBDIR'#010+
  #010+
  #010+
  '[redir]'#010+
  '#####################################################################'#010+
  '# Redirection'#010+
  '#####################################################################'#010+
  #010+
  'ifndef REDIRFILE'#010+
  'RED','IRFILE=log'#010+
  'endif'#010+
  #010+
  'ifdef REDIR'#010+
  'ifndef inUnix'#010+
  'override FPC=redir -eo $(FPC)'#010+
  'endif'#010+
  '# set the verbosity to max'#010+
  'override FPCOPT+=-va'#010+
  'override REDIR:= >> $(REDIRFILE)'#010+
  'endif'#010+
  #010+
  '[command_begin]'#010+
  '########################################################','############'+
  '#'#010+
  '# Compiler Command Line'#010+
  '#####################################################################'#010+
  #010+
  '# Load commandline OPTDEF and add FPC_CPU define'#010+
  'override FPCOPTDEF:=-d$(CPU_TARGET)'#010+
  #010+
  '# Load commandline OPT and add target and uni','t dir to be sure'#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#010+
  'override FPCOPT+=-T$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  '# User dirs should be first, so they are looked at first'#010+
  'ifdef UNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(UNITDIR))'#010+
  'endif'#010+
  'ifdef LIBDIR'#010+
  'override FPCOPT','+=$(addprefix -Fl,$(LIBDIR))'#010+
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
  '# Smartlinking crea','tion'#010+
  'ifdef CREATESMART'#010+
  'override FPCOPT+=-CX'#010+
  'endif'#010+
  #010+
  '# Debug'#010+
  'ifdef DEBUG'#010+
  'override FPCOPT+=-g -dDEBUG'#010+
  'endif'#010+
  #010+
  '# Release mode (strip, optimize and don'#039't load ppc386.cfg)'#010+
  '# 0.99.12b has a bug in the optimizer so don'#039't use it by default'#010+
  'ifdef RELE','ASE'#010+
  'ifeq ($(FPC_VERSION),0.99.12)'#010+
  'override FPCOPT+=-Xs -OGp3 -n'#010+
  'else'#010+
  'override FPCOPT+=-Xs -OG2p3 -n'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Strip'#010+
  'ifdef STRIP'#010+
  'override FPCOPT+=-Xs'#010+
  'endif'#010+
  #010+
  '# Optimizer'#010+
  'ifdef OPTIMIZE'#010+
  'override FPCOPT+=-OG2p3'#010+
  'endif'#010+
  #010+
  '# Verbose settings (w','arning,note,info)'#010+
  'ifdef VERBOSE'#010+
  'override FPCOPT+=-vwni'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needopt]'#010+
  'ifdef NEEDOPT'#010+
  'override FPCOPT+=$(NEEDOPT)'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needunit]'#010+
  'ifdef NEEDUNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(NEEDUNITDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[command_unitsd','ir]'#010+
  'ifdef UNITSDIR'#010+
  'override FPCOPT+=-Fu$(UNITSDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needlib]'#010+
  'ifdef NEEDLIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(NEEDLIBDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needobj]'#010+
  'ifdef NEEDOBJDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(NEEDOBJDIR))'#010+
  'endif'#010+
  #010,
  #010+
  '[command_needinc]'#010+
  'ifdef NEEDINCDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(NEEDINCDIR))'#010+
  'endif'#010+
  #010+
  '[command_gcclib]'#010+
  '# Add GCC lib path if asked'#010+
  'ifdef GCCLIBDIR'#010+
  'override FPCOPT+=-Fl$(GCCLIBDIR)'#010+
  'endif'#010+
  #010+
  '[command_otherlib]'#010+
  '# Add Other dirs path if aske','d'#010+
  'ifdef OTHERLIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#010+
  'endif'#010+
  #010+
  '[command_target]'#010+
  '# Target dirs and the prefix to use for clean/install'#010+
  'ifdef TARGETDIR'#010+
  'override FPCOPT+=-FE$(TARGETDIR)'#010+
  'ifeq ($(TARGETDIR),.)'#010+
  'override TARGETDIRPREF','IX='#010+
  'else'#010+
  'override TARGETDIRPREFIX=$(TARGETDIR)/'#010+
  'endif'#010+
  'endif'#010+
  'ifdef UNITTARGETDIR'#010+
  'override FPCOPT+=-FU$(UNITTARGETDIR)'#010+
  'ifeq ($(UNITTARGETDIR),.)'#010+
  'override UNITTARGETDIRPREFIX='#010+
  'else'#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIR)/'#010+
  'endif'#010+
  'else'#010+
  'ifdef TA','RGETDIR'#010+
  'override UNITTARGETDIR=$(TARGETDIR)'#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[command_end]'#010+
  '# Add commandline options last so they can override'#010+
  'ifdef OPT'#010+
  'override FPCOPT+=$(OPT)'#010+
  'endif'#010+
  #010+
  '# Add defines from FPCOPTDEF',' to FPCOPT'#010+
  'ifdef FPCOPTDEF'#010+
  'override FPCOPT+=$(FPCOPTDEF)'#010+
  'endif'#010+
  #010+
  '# Error file ?'#010+
  'ifdef ERRORFILE'#010+
  'override FPCOPT+=-Fr$(ERRORFILE)'#010+
  'endif'#010+
  #010+
  '# Was a config file specified ?'#010+
  'ifdef CFGFILE'#010+
  'override FPCOPT+=@$(CFGFILE)'#010+
  'endif'#010+
  #010+
  #010+
  '[command_win32file]'#010+
  '# ','For win32 the options are passed using the file fpcext.cmd'#010+
  'ifndef FPCEXTFILE'#010+
  'FPCEXTFILE=fpcext.cmd'#010+
  'endif'#010+
  'ifeq ($(OS_SOURCE),win32)'#010+
  'override TMPVAR:=$(shell "$(DEL) $(FPCEXTFILE)")'#010+
  'override TMPVAR:=$(foreach A,$(FPCOPT),$(shell "$(ECHOE) "$(','A)" >> $'+
  '(FPCEXTFILE)"))'#010+
  'override FPCOPT:=@$(FPCEXTFILE)'#010+
  'endif'#010+
  #010+
  '[command_win32env]'#010+
  '# For win32 the options are passed using the environment FPCEXTCMD'#010+
  'ifeq ($(OS_SOURCE),win32)'#010+
  'override FPCEXTCMD:=$(FPCOPT)'#010+
  'override FPCOPT:=!FPCEXTCMD'#010+
  'export ','FPCEXTCMD'#010+
  'endif'#010+
  #010+
  '[command_compiler]'#010+
  '# Compiler commandline'#010+
  'override COMPILER:=$(FPC) $(FPCOPT)'#010+
  #010+
  '# also call ppas if with command option -s'#010+
  '# but only if the OS_SOURCE and OS_TARGE are equal'#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#010+
  'EXECPPAS='#010+
  'el','se'#010+
  'ifeq ($(OS_SOURCE),$(OS_TARGET))'#010+
  'EXECPPAS:=@$(PPAS)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  #010+
  '[standardrules]'#010+
  '#####################################################################'#010+
  '# Standard rules'#010+
  '####################################################################','#'+
  #010+
  #010+
  '[packagedependrules]'#010+
  '#####################################################################'#010+
  '# Package depends'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[loaderrules]'#010+
  '##########################################','##########################'+
  '#'#010+
  '# Loaders'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_loaders'#010+
  #010+
  '%$(OEXT): %$(LOADEREXT)'#010+
  '        $(AS) -o $*$(OEXT) $<'#010+
  #010+
  'override ALLTARGET+=fpc_loaders'#010+
  'override CLEANTARGET+=f','pc_loaders_clean'#010+
  'override SHOWINSTALLTARGET+=fpc_loaders_showinstall'#010+
  'override INSTALLTARGET+=fpc_loaders_install'#010+
  #010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(LOADEROBJECTS))'#010+
  #010+
  'fpc_loaders: $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_clean:'#010+
  '        -$(DEL)',' $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_showinstall:'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNITINSTALLDIR)/,$(LOADEROFILES)'+
  ')'#010+
  #010+
  'fpc_loaders_install:'#010+
  '        $(MKDIR) $(UNITINSTALLDIR)'#010+
  '        $(INSTALL) $(LOADEROFILES) $(UNITINSTALLDIR)'#010+
  #010+
  #010+
  '[unitrules]'#010+
  '#','###################################################################'+
  '#'#010+
  '# Units'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_units'#010+
  #010+
  'override ALLTARGET+=fpc_units'#010+
  #010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT','),$(UNITOBJECTS))'#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES)'#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES)'#010+
  #010+
  'fpc_units: $(UNITPPUFILES)'#010+
  #010+
  #010+
  #010+
  '[exerules]'#010+
  '#####################################################################'#010+
  '# Exes'#010+
  '#####################','###############################################'+
  '#'#010+
  #010+
  '.PHONY: fpc_exes'#010+
  #010+
  'ifdef EXEOBJECTS'#010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(EXEOBJECTS))'#010+
  'override EXEOFILES=$(addsuffix $(OEXT),$(EXEOBJECTS))'#010+
  #010+
  'override ALLTARGET+=fpc_exes'#010+
  'override INSTALL','EXEFILES+=$(EXEFILES)'#010+
  'override CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#010+
  'endif'#010+
  #010+
  'fpc_exes: $(EXEFILES)'#010+
  #010+
  #010+
  '[rstrules]'#010+
  '#####################################################################'#010+
  '# Resource strings'#010+
  '######################################','##############################'+
  '#'#010+
  #010+
  'ifdef RSTOBJECTS'#010+
  'override RSTFILES=$(addsuffix $(RSTEXT),$(RSTOBJECTS))'#010+
  #010+
  'override CLEANRSTFILES+=$(RSTFILES)'#010+
  'endif'#010+
  #010+
  #010+
  '[examplerules]'#010+
  '#####################################################################'#010+
  '# Exa','mples'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_examples fpc_test'#010+
  #010+
  'ifdef EXAMPLEOBJECTS'#010+
  'override EXAMPLESOURCEFILES:=$(addsuffix $(PASEXT),$(EXAMPLEOBJECTS))'#010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEE','XT),$(EXAMPLEOBJECTS))'#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(EXAMPLEOBJECTS))'#010+
  #010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#010+
  'endif'#010+
  #010+
  'fpc_examples: all $(EXAMPLEFILES) $(addsuffix _all,$(EXAMPLEDIROBJECTS'+
  '))'#010+
  #010+
  'fpc_test: examp','les'#010+
  #010+
  #010+
  '[compilerules]'#010+
  '#####################################################################'#010+
  '# General compile rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_packages fpc_all fpc_debug'#010+
  #010+
  '$(FPCMADE): $(','ALLTARGET)'#010+
  '        @$(ECHO) Compiled > $(FPCMADE)'#010+
  #010+
  'fpc_packages: $(COMPILEPACKAGES)'#010+
  #010+
  'fpc_all: fpc_packages $(FPCMADE)'#010+
  #010+
  'fpc_debug:'#010+
  '        $(MAKE) all DEBUG=1'#010+
  #010+
  '# Search paths for .ppu if targetdir is set'#010+
  'ifdef UNITTARGETDIR'#010+
  'vpath %$(PPUEXT) ','$(UNITTARGETDIR)'#010+
  'endif'#010+
  #010+
  '# General compile rules, available for both possible PASEXT'#010+
  #010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp'#010+
  #010+
  '%$(PPUEXT): %.pp'#010+
  '        $(COMPILER) $< $(REDIR)'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(PPUEXT): %.pas'#010+
  '        $(COMPILER',') $< $(REDIR)'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(EXEEXT): %.pp'#010+
  '        $(COMPILER) $< $(REDIR)'#010+
  '        $(EXECPPAS)'#010+
  #010+
  '%$(EXEEXT): %.pas'#010+
  '        $(COMPILER) $< $(REDIR)'#010+
  '        $(EXECPPAS)'#010+
  #010+
  #010+
  '[libraryrules]'#010+
  '################################################','####################'+
  '#'#010+
  '# Library'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_smart fpc_shared'#010+
  #010+
  'ifdef LIBVERSION'#010+
  'LIBFULLNAME=$(LIBNAME).$(LIBVERSION)'#010+
  'else'#010+
  'LIBFULLNAME=$(LIBNAME)'#010+
  'endif'#010+
  #010+
  '# Default sharedli','b units are all unit objects'#010+
  'ifndef SHAREDLIBUNITOBJECTS'#010+
  'SHAREDLIBUNITOBJECTS:=$(UNITOBJECTS)'#010+
  'endif'#010+
  #010+
  'fpc_smart:'#010+
  '        $(MAKE) all LINKSMART=1 CREATESMART=1'#010+
  #010+
  'fpc_shared: all'#010+
  'ifdef HASSHAREDLIB'#010+
  'ifndef LIBNAME'#010+
  '        @$(ECHO) "LIBNAME not s','et"'#010+
  'else'#010+
  '        $(PPUMOVE) $(SHAREDLIBUNITOBJECTS) -o$(LIBFULLNAME)'#010+
  'endif'#010+
  'else'#010+
  '        @$(ECHO) "Shared Libraries not supported"'#010+
  'endif'#010+
  #010+
  #010+
  '[installrules]'#010+
  '#####################################################################'#010+
  '# Install rules'#010+
  '#','###################################################################'+
  '#'#010+
  #010+
  '.PHONY: fpc_showinstall fpc_install'#010+
  #010+
  'ifdef EXTRAINSTALLUNITS'#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(EXTRAINSTALLUNITS))'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLPPUFILES'#010+
  'override I','NSTALLPPUFILES:=$(addprefix $(TARGETDIRPREFIX),$(INSTALLPP'+
  'UFILES))'#010+
  'ifdef PPUFILES'#010+
  'INSTALLPPULINKFILES:=$(shell $(PPUFILES) -S -O $(INSTALLPPUFILES))'#010+
  'else'#010+
  'INSTALLPPULINKFILES:=$(wildcard $(subst $(PPUEXT),$(OEXT),$(INSTALLPPU'+
  'FILES)) $(addpre','fix $(LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$'+
  '(INSTALLPPUFILES))))'#010+
  'endif'#010+
  'override INSTALLPPULINKFILES:=$(addprefix $(TARGETDIRPREFIX),$(INSTALL'+
  'PPULINKFILES))'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLEXEFILES'#010+
  'override INSTALLEXEFILES:=$(addprefix $(TARGETD','IRPREFIX),$(INSTALLEX'+
  'EFILES))'#010+
  'endif'#010+
  #010+
  'fpc_showinstall: $(SHOWINSTALLTARGET)'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(BININSTALLDIR)/,$(INSTALLEXEFILE'+
  'S))'#010+
  'endif'#010+
  'ifdef INSTALLPPUFILES'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNI','TINSTALLDIR)/,$(INSTALLPPUF'+
  'ILES))'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNITINSTALLDIR)/,$(INSTALLPPULIN'+
  'KFILES))'#010+
  'endif'#010+
  'ifneq ($(wildcard $(LIBFULLNAME)),)'#010+
  '        @$(ECHO) $(LIBINSTALLDIR)/$(LIBFULLNAME)'#010+
  'ifde','f HASSHAREDLIB'#010+
  '        @$(ECHO) $(LIBINSTALLDIR)/$(LIBNAME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'ifdef EXTRAINSTALLFILES'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(DATAINSTALLDIR)/,$(EXTRAINSTALLF'+
  'ILES))'#010+
  'endif'#010+
  #010+
  'fpc_install: $(INSTALLTARGET)'#010+
  '# Create UnitInstallFile','s'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        $(MKDIR) $(BININSTALLDIR)'#010+
  '# Compress the exes if upx is defined'#010+
  'ifdef UPXPROG'#010+
  '        -$(UPXPROG) $(INSTALLEXEFILES)'#010+
  'endif'#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(BININSTALLDIR)'#010+
  'endif'#010+
  'ifdef INSTALLPPUFILES',#010+
  '        $(MKDIR) $(UNITINSTALLDIR)'#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(UNITINSTALLDIR)'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '        $(INSTALL) $(INSTALLPPULINKFILES) $(UNITINSTALLDIR)'#010+
  'endif'#010+
  'ifneq ($(wildcard $(LIBFULLNAME)),)'#010+
  '        $(MKDI','R) $(LIBINSTALLDIR)'#010+
  '        $(INSTALL) $(LIBFULLNAME) $(LIBINSTALLDIR)'#010+
  'ifdef inUnix'#010+
  '        ln -sf $(LIBFULLNAME) $(LIBINSTALLDIR)/$(LIBNAME)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  'ifdef EXTRAINSTALLFILES'#010+
  '        $(MKDIR) $(DATAINSTALLDIR)'#010+
  '        $(INSTALL) $(','EXTRAINSTALLFILES) $(DATAINSTALLDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[sourceinstallrules]'#010+
  '#####################################################################'#010+
  '# SourceInstall rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc','_sourceinstall'#010+
  #010+
  'ifndef SOURCETOPDIR'#010+
  'SOURCETOPDIR=$(BASEDIR)'#010+
  'endif'#010+
  #010+
  'fpc_sourceinstall: clean'#010+
  '        $(MKDIR) $(SOURCEINSTALLDIR)'#010+
  '        $(COPYTREE) $(SOURCETOPDIR) $(SOURCEINSTALLDIR)'#010+
  #010+
  '[exampleinstallrules]'#010+
  '################################','####################################'+
  '#'#010+
  '# exampleinstall rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_exampleinstall'#010+
  #010+
  'fpc_exampleinstall: $(addsuffix _clean,$(EXAMPLEDIROBJECTS))'#010+
  'ifdef EXAMPLESOURCE','FILES'#010+
  '        $(MKDIR) $(EXAMPLEINSTALLDIR)'#010+
  '        $(COPY) $(EXAMPLESOURCEFILES) $(EXAMPLEINSTALLDIR)'#010+
  'endif'#010+
  'ifdef EXAMPLEDIROBJECTS'#010+
  'ifndef EXAMPLESOURCEFILES'#010+
  '        $(MKDIR) $(EXAMPLEINSTALLDIR)'#010+
  'endif'#010+
  '        $(COPYTREE) $(addsuffix /*,$(','EXAMPLEDIROBJECTS)) $(EXAMPLEIN'+
  'STALLDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[zipinstallrules]'#010+
  '#####################################################################'#010+
  '# Zip'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_zipinstall'#010+
  #010+
  '# ','Create suffix to add'#010+
  'ifndef PACKAGESUFFIX'#010+
  'PACKAGESUFFIX=$(OS_TARGET)'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'PACKAGESUFFIX=go32'#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),win32)'#010+
  'PACKAGESUFFIX=w32'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Temporary path to pack a file'#010+
  'ifndef PACKDIR'#010+
  'ifndef inUnix'#010+
  'P','ACKDIR=$(BASEDIR)/pack_tmp'#010+
  'else'#010+
  'PACKDIR=/tmp/fpc-pack'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Maybe create default zipname from packagename'#010+
  'ifndef ZIPNAME'#010+
  'ifdef PACKAGENAME'#010+
  'ZIPNAME=$(PACKAGEPREFIX)$(PACKAGENAME)$(PACKAGESUFFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Use tar by default und','er linux'#010+
  'ifndef USEZIP'#010+
  'ifdef inUnix'#010+
  'USETAR=1'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'fpc_zipinstall:'#010+
  'ifndef ZIPNAME'#010+
  '        @$(ECHO) "Please specify ZIPNAME!"'#010+
  '        @exit 1'#010+
  'else'#010+
  '        $(MAKE) $(ZIPTARGET) PREFIXINSTALLDIR=$(PACKDIR)'#010+
  'ifdef USETAR'#010+
  '        $(DEL) $(','DESTZIPDIR)/$(ZIPNAME)$(TAREXT)'#010+
  '        cd $(PACKDIR) ; $(TARPROG) c$(TAROPT) --file $(DESTZIPDIR)/$(Z'+
  'IPNAME)$(TAREXT) * ; cd $(BASEDIR)'#010+
  'else'#010+
  '        $(DEL) $(DESTZIPDIR)/$(ZIPNAME)$(ZIPEXT)'#010+
  '        cd $(PACKDIR) ; $(ZIPPROG) -Dr $(ZIPOPT) ','$(DESTZIPDIR)/$(ZIP'+
  'NAME)$(ZIPEXT) * ; cd $(BASEDIR)'#010+
  'endif'#010+
  '        $(DELTREE) $(PACKDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[zipsourceinstallrules]'#010+
  '.PHONY:  fpc_zipsourceinstall'#010+
  #010+
  'fpc_zipsourceinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall PACKAGESUFFI','X'+
  '=src'#010+
  #010+
  #010+
  '[zipexampleinstallrules]'#010+
  '.PHONY:  fpc_zipexampleinstall'#010+
  #010+
  'fpc_zipexampleinstall:'#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall PACKAGESUFFIX='+
  'exm'#010+
  #010+
  #010+
  '[cleanrules]'#010+
  '###############################################################','#####'+
  '#'#010+
  '# Clean rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#010+
  #010+
  'ifdef EXTRACLEANUNITS'#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(EXTRACLEANUNITS))'#010+
  'endif'#010+
  #010+
  'ifdef C','LEANPPUFILES'#010+
  'override CLEANPPUFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANPPUFILES'+
  '))'#010+
  '# Get the .o and .a files created for the units'#010+
  'ifdef PPUFILES'#010+
  'CLEANPPULINKFILES:=$(shell $(PPUFILES) $(CLEANPPUFILES))'#010+
  'else'#010+
  'CLEANPPULINKFILES:=$(wildcard',' $(subst $(PPUEXT),$(OEXT),$(CLEANPPUFI'+
  'LES)) $(addprefix $(LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLE'+
  'ANPPUFILES))))'#010+
  'endif'#010+
  'override CLEANPPULINKFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANPPUL'+
  'INKFILES))'#010+
  'endif'#010+
  #010+
  'fpc_clean: $(CLEANTARG','ET)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) $(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES))'#010+
  'endif'#010+
  'ifdef CLEANPPUFILES'#010+
  '        -$(DEL) $(CLEANPPUFILES)'#010+
  'endif'#010+
  'ifneq ($(CLEANPPULINKFILES),)'#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#010+
  'endif'#010+
  'ifdef CLEANRS','TFILES'#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#010+
  'endif'#010+
  'ifdef EXTRACLEANFILES'#010+
  '        -$(DEL) $(EXTRACLEANFILES)'#010+
  'endif'#010+
  'ifdef LIBNAME'#010+
  '        -$(DEL) $(LIBNAME) $(LIBFULLNAME)'#010+
  'endif'#010+
  '        -$(DEL) $(FPCMADE) $(PPAS',') link.res $(FPCEXTFILE) $(REDIRFIL'+
  'E)'#010+
  #010+
  'fpc_distclean: fpc_clean'#010+
  #010+
  '# Also run clean first if targetdir is set. Unittargetdir is always'#010+
  '# set if targetdir or unittargetdir is specified'#010+
  'ifdef UNITTARGETDIR'#010+
  'TARGETDIRCLEAN=fpc_clean'#010+
  'endif'#010+
  #010+
  'fpc_cl','eanall: $(CLEANTARGET) $(TARGETDIRCLEAN)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#010+
  '        -$(DELTREE) *$(SMARTEXT)'#010+
  '    ','    -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTFILE) $(REDIRFIL'+
  'E)'#010+
  #010+
  #010+
  '[dependrules]'#010+
  '#####################################################################'#010+
  '# Depend rules'#010+
  '#####################################################################'#010+
  #010+
  '.','PHONY: fpc_depend'#010+
  #010+
  'fpc_depend:'#010+
  '        $(PPDEP) $(UNITOBJECTS)'#010+
  #010+
  #010+
  '[inforules]'#010+
  '#####################################################################'#010+
  '# Info rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: f','pc_info fpc_cfginfo fpc_objectinfo fpc_toolsinfo fpc_instal'+
  'linfo \'#010+
  '        fpc_dirinfo'#010+
  #010+
  'fpc_info: $(INFOTARGET)'#010+
  #010+
  '[info_cfg]'#010+
  'fpc_infocfg:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Configuration info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  FPC.....','.. $(FPC)'#010+
  '        @$(ECHO)  Version... $(FPC_VERSION)'#010+
  '        @$(ECHO)  CPU....... $(CPU_TARGET)'#010+
  '        @$(ECHO)  Source.... $(OS_SOURCE)'#010+
  '        @$(ECHO)  Target.... $(OS_TARGET)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_dirs]'#010+
  'fpc_infodirs:'#010+
  '        @$(ECHO',')'#010+
  '        @$(ECHO)  == Directory info =='#010+
  '        @$(ECHO)'#010+
  'ifdef NEEDGCCLIB'#010+
  '        @$(ECHO)  GCC library is needed.'#010+
  'endif'#010+
  'ifdef NEEDOTHERLIB'#010+
  '        @$(ECHO)  Other library is needed.'#010+
  'endif'#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#010+
  '    ','    @$(ECHO)'#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_tools]'#010+
  'fpc_infotools:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Tools info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECH','O)  Pwd....... $(PWD)'#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#010+
  'ifdef PPDEP'#010+
  '        @$(ECHO)  PPDep..... $(PPDEP)'#010+
  'endif'#010+
  'ifdef PPUMOVE'#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#010+
  'endif'#010+
  'ifdef PPUFILES'#010+
  '        @$(ECHO)  PPUFiles.. $(PPUFILES)'#010+
  'endif'#010+
  'ifd','ef DATA2INC'#010+
  '        @$(ECHO)  Data2Inc.. $(DATA2INC)'#010+
  'endif'#010+
  'ifdef SED'#010+
  '        @$(ECHO)  Sed....... $(SED)'#010+
  'endif'#010+
  'ifdef DATE'#010+
  '        @$(ECHO)  Date...... $(DATE)'#010+
  'endif'#010+
  'ifdef DIFF'#010+
  '        @$(ECHO)  Diff...... $(DIFF)'#010+
  'endif'#010+
  'ifdef CMP'#010+
  '        @$(','ECHO)  Cmp....... $(CMP)'#010+
  'endif'#010+
  'ifdef UPXPROG'#010+
  '        @$(ECHO)  Upx....... $(UPXPROG)'#010+
  'endif'#010+
  'ifdef ZIPPROG'#010+
  '        @$(ECHO)  Zip....... $(ZIPPROG)'#010+
  'endif'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_objects]'#010+
  'fpc_infoobjects:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == O','bject info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  LoaderObjects..... $(LOADEROBJECTS)'#010+
  '        @$(ECHO)  UnitObjects....... $(UNITOBJECTS)'#010+
  '        @$(ECHO)  ExeObjects........ $(EXEOBJECTS)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  ExtraCleanUnits..','. $(EXTRACLEANUNITS)'#010+
  '        @$(ECHO)  ExtraCleanFiles... $(EXTRACLEANFILES)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  ExtraInstallUnits. $(EXTRAINSTALLUNITS)'#010+
  '        @$(ECHO)  ExtraInstallFiles. $(EXTRAINSTALLFILES)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_instal','l]'#010+
  'fpc_infoinstall:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Install info =='#010+
  '        @$(ECHO)'#010+
  'ifdef DATE'#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#010+
  'endif'#010+
  'ifdef PACKAGEPREFIX'#010+
  '        @$(ECHO)  PackagePrefix........ $(PACKAGEPREFIX)'#010+
  'endi','f'#010+
  'ifdef PACKAGENAME'#010+
  '        @$(ECHO)  PackageName.......... $(PACKAGENAME)'#010+
  'endif'#010+
  '        @$(ECHO)  PackageSuffix........ $(PACKAGESUFFIX)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  BaseInstallDir....... $(BASEINSTALLDIR)'#010+
  '        @$(ECHO)  BinInstal','lDir........ $(BININSTALLDIR)'#010+
  '        @$(ECHO)  LibInstallDir........ $(LIBINSTALLDIR)'#010+
  '        @$(ECHO)  UnitInstallDir....... $(UNITINSTALLDIR)'#010+
  '        @$(ECHO)  SourceInstallDir..... $(SOURCEINSTALLDIR)'#010+
  '        @$(ECHO)  DocInstallDir....','.... $(DOCINSTALLDIR)'#010+
  '        @$(ECHO)  DataInstallDir....... $(DATAINSTALLDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  DestZipDir........... $(DESTZIPDIR)'#010+
  '        @$(ECHO)  ZipName.............. $(ZIPNAME)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[directorytargets]'#010+
  '#','###################################################################'+
  '#'#010+
  '# Directories'#010+
  '#####################################################################'#010+
  #010+
  '[localmakefile]'#010+
  '#####################################################################'#010,
  '# Local Makefile'#010+
  '#####################################################################'#010+
  #010+
  'ifneq ($(wildcard fpcmake.loc),)'#010+
  'include fpcmake.loc'#010+
  'endif'#010+
  #010+
  '[userrules]'#010+
  '#####################################################################'#010+
  '# Users ru','les'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[end]'#010
);
