{$ifdef Delphi}
const fpcmakeini : array[0..147] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..147,1..240] of char=(
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
  '# define inUnix when runnin','g under Unix (Linux,FreeBSD)'#013#010+
  '# define inWinNT when running under WinNT'#013#010+
  '# define inOS2 when running under OS/2'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# We need only / in the path'#013#010+
  'override PATH:=$(subst \',',/,$(PATH))'#013#010+
  #013#010+
  '# Search for PWD and determine also if we are under linux'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(subst ;, ,$(PATH)))))'#013+
  #010+
  'ifeq ($(PWD),)'#013#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(subst :, ,$(PATH)))))'#013#010+
  'ifeq ($(PWD),)',#013#010+
  'nopwd:'#013#010+
  '        @echo You need the GNU utils package to use this Makefile!'#013+
  #010+
  '        @echo Get ftp://ftp.freepascal.org/pub/fpc/dist/go32v2/utilgo3'+
  '2.zip'#013#010+
  '        @exit'#013#010+
  'else'#013#010+
  'inUnix=1'#013#010+
  'PWD:=$(firstword $(PWD))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'PWD:=$(firstwor','d $(PWD))'#013#010+
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
  'endif'#013#010+
  #013#010+
  '# The extension of executables',#013#010+
  'ifdef inUnix'#013#010+
  'SRCEXEEXT='#013#010+
  'else'#013#010+
  'SRCEXEEXT=.exe'#013#010+
  'endif'#013#010+
  #013#010+
  '# The extension of batch files / scripts'#013#010+
  'ifdef inUnix'#013#010+
  'BATCHEXT=.sh'#013#010+
  'else'#013#010+
  'ifdef inOS2'#013#010+
  'BATCHEXT=.cmd'#013#010+
  'else'#013#010+
  'BATCHEXT=.bat'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Path Separator, the subst trick is nec','essary for the \ that can'#039+
  't exists'#013#010+
  '# at the end of a line'#013#010+
  'ifdef inUnix'#013#010+
  'PATHSEP=/'#013#010+
  'else'#013#010+
  'PATHSEP=$(subst /,\,/)'#013#010+
  'endif'#013#010+
  #013#010+
  '# The path which is searched separated by spaces'#013#010+
  'ifdef inUnix'#013#010+
  'SEARCHPATH=$(subst :, ,$(PATH))'#013#010+
  'else'#013#010+
  'SEARCHPATH=$(','subst ;, ,$(PATH))'#013#010+
  'endif'#013#010+
  #013#010+
  '# Base dir'#013#010+
  'ifdef PWD'#013#010+
  'BASEDIR:=$(shell $(PWD))'#013#010+
  'else'#013#010+
  'BASEDIR=.'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[defaulttarget]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default target'#013#010+
  '############################','########################################'+
  '#'#013#010+
  #013#010+
  '[fpcdetect]'#013#010+
  '#####################################################################'#013+
  #010+
  '# FPC version/target Detection'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# What',' compiler to use ?'#013#010+
  'ifndef FPC'#013#010+
  '# Compatibility with old makefiles'#013#010+
  'ifdef PP'#013#010+
  'FPC=$(PP)'#013#010+
  'else'#013#010+
  'FPC=ppc386'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'override FPC:=$(subst $(SRCEXEEXT),,$(FPC))'#013#010+
  'override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)'#013#010+
  #013#010+
  '# Target OS'#013#010+
  'ifndef OS_TA','RGET'#013#010+
  'OS_TARGET:=$(shell $(FPC) -iTO)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Source OS'#013#010+
  'ifndef OS_SOURCE'#013#010+
  'OS_SOURCE:=$(shell $(FPC) -iSO)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Target CPU'#013#010+
  'ifndef CPU_TARGET'#013#010+
  'CPU_TARGET:=$(shell $(FPC) -iTP)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Source CPU'#013#010+
  'ifndef CPU_SOURCE'#013#010+
  'CPU_SOURCE',':=$(shell $(FPC) -iSP)'#013#010+
  'endif'#013#010+
  #013#010+
  '# FPC version'#013#010+
  'ifndef FPC_VERSION'#013#010+
  'FPC_VERSION:=$(shell $(FPC) -iV)'#013#010+
  'endif'#013#010+
  #013#010+
  'export FPC OS_TARGET OS_SOURCE CPU_TARGET CPU_SOURCE FPC_VERSION'#013#010+
  #013#010+
  #013#010+
  '[fpcdircheckenv]'#013#010+
  '#########################################','###########################'+
  '#'#013#010+
  '# FPCDIR Setting'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Test FPCDIR to look if the RTL dir exists'#013#010+
  'ifdef FPCDIR'#013#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#013#010+
  'ifeq ($(wildcard $(','FPCDIR)/rtl),)'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR=wrong'#013#010+
  'endif'#013#010+
  #013#010+
  '[fpcdirdetect]'#013#010+
  '# Detect FPCDIR'#013#010+
  'ifeq ($(FPCDIR),wrong)'#013#010+
  'ifdef inUnix'#013#010+
  'override FPCDIR=/usr/local/lib/fpc/$(FPC_','VERSION)'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(add'+
  'suffix /$(FPC),$(SEARCHPATH))))))'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013,#010+
  'ifeq ($(wildcard $(FPCDIR)/rtl),)'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR:=$(FPCDIR)/..'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/rtl),)'#013#010+
  'ifeq ($(wildcard $(FPCDIR)/units),)'#013#010+
  'override FPCDIR=c:/pp'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013,#010+
  #013#010+
  '[fpcdirsubs]'#013#010+
  '# Create units dir'#013#010+
  'ifneq ($(FPCDIR),.)'#013#010+
  'UNITSDIR=$(FPCDIR)/units/$(OS_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  '[checkfpcdirsubs]'#013#010+
  '# Check if the dirs really exists, else turn it off'#013#010+
  'ifeq ($(wildcard $(UNITSDIR)),)'#013#010+
  'UNITSDIR='#013#010+
  'endif'#013#010+
  'ifeq ($(wil','dcard $(TOOLKITSDIR)),)'#013#010+
  'TOOLKITSDIR='#013#010+
  'endif'#013#010+
  'ifeq ($(wildcard $(PACKAGESDIR)),)'#013#010+
  'PACKAGESDIR='#013#010+
  'endif'#013#010+
  'ifeq ($(wildcard $(COMPONENTSDIR)),)'#013#010+
  'COMPONENTSDIR='#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[presettings]'#013#010+
  '#########################################################','###########'+
  '#'#013#010+
  '# Pre Settings'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  #013#010+
  '[usersettings]'#013#010+
  '#####################################################################'#013+
  #010+
  '# User Settings'#013#010+
  '###############################','#####################################'+
  '#'#013#010+
  #013#010+
  #013#010+
  '[postsettings]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Post Settings'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  #013#010+
  '[shelltools]'#013#010+
  '###','#################################################################'+
  '#'#013#010+
  '# Shell tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# echo'#013#010+
  'ifndef ECHO'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho$(EXEEXT),$(SEARCHPA','TH)))'+
  ')'#013#010+
  'ifeq ($(ECHO),)'#013#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#013#010+
  'ifeq ($(ECHO),)'#013#010+
  'ECHO:=echo'#013#010+
  'ECHOE:=echo'#013#010+
  'else'#013#010+
  'ECHO:=$(firstword $(ECHO))'#013#010+
  'ECHOE=$(ECHO) -E'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ECHO:=$(firstword $(ECHO))'#013#010+
  'ECHOE=$','(ECHO) -E'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# To copy pograms'#013#010+
  'ifndef COPY'#013#010+
  'COPY:=cp -fp'#013#010+
  'endif'#013#010+
  #013#010+
  '# Copy a whole tree'#013#010+
  'ifndef COPYTREE'#013#010+
  'COPYTREE:=cp -rfp'#013#010+
  'endif'#013#010+
  #013#010+
  '# To move pograms'#013#010+
  'ifndef MOVE'#013#010+
  'MOVE:=mv -f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check delete program'#013#010+
  'ifndef DEL'#013#010+
  'DE','L:=rm -f'#013#010+
  'endif'#013#010+
  #013#010+
  '# Check deltree program'#013#010+
  'ifndef DELTREE'#013#010+
  'DELTREE:=rm -rf'#013#010+
  'endif'#013#010+
  #013#010+
  '# To install files'#013#010+
  'ifndef INSTALL'#013#010+
  'ifdef inUnix'#013#010+
  'INSTALL:=install -c -m 644'#013#010+
  'else'#013#010+
  'INSTALL:=$(COPY)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# To install programs'#013#010+
  'ifndef INSTAL','LEXE'#013#010+
  'ifdef inUnix'#013#010+
  'INSTALLEXE:=install -c -m 755'#013#010+
  'else'#013#010+
  'INSTALLEXE:=$(COPY)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# To make a directory.'#013#010+
  'ifndef MKDIR'#013#010+
  'ifdef inUnix'#013#010+
  'MKDIR:=install -m 755 -d'#013#010+
  'else'#013#010+
  'MKDIR:=ginstall -m 755 -d'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  'export ECHO ECHOE C','OPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE '+
  'MKDIR'#013#010+
  #013#010+
  #013#010+
  '[tool_default]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default Tools'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# a','ssembler, redefine it if cross compiling'#013#010+
  'ifndef AS'#013#010+
  'AS=as'#013#010+
  'endif'#013#010+
  #013#010+
  '# linker, but probably not used'#013#010+
  'ifndef LD'#013#010+
  'LD=ld'#013#010+
  'endif'#013#010+
  #013#010+
  '# ppas.bat / ppas.sh'#013#010+
  'PPAS=$(BATCHEXT)'#013#010+
  #013#010+
  '# ldconfig to rebuild .so cache'#013#010+
  'ifdef inUnix'#013#010+
  'LDCONFIG=ldconfig'#013#010+
  'els','e'#013#010+
  'LDCONFIG='#013#010+
  'endif'#013#010+
  #013#010+
  '[tool_ppdep]'#013#010+
  '# ppdep'#013#010+
  'ifndef PPDEP'#013#010+
  'PPDEP:=$(strip $(wildcard $(addsuffix /ppdep$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifeq ($(PPDEP),)'#013#010+
  'PPDEP='#013#010+
  'else'#013#010+
  'PPDEP:=$(firstword $(PPDEP))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export PPDEP'#013#010+
  #013#010+
  '[tool_ppumove',']'#013#010+
  '# ppumove'#013#010+
  'ifndef PPUMOVE'#013#010+
  'PPUMOVE:=$(strip $(wildcard $(addsuffix /ppumove$(SRCEXEEXT),$(SEARCHP'+
  'ATH))))'#013#010+
  'ifeq ($(PPUMOVE),)'#013#010+
  'PPUMOVE='#013#010+
  'else'#013#010+
  'PPUMOVE:=$(firstword $(PPUMOVE))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export PPUMOVE'#013#010+
  #013#010+
  '[tool_ppufiles]'#013#010+
  '# ppufiles'#013#010,
  'ifndef PPUFILES'#013#010+
  'PPUFILES:=$(strip $(wildcard $(addsuffix /ppufiles$(SRCEXEEXT),$(SEARC'+
  'HPATH))))'#013#010+
  'ifeq ($(PPUFILES),)'#013#010+
  'PPUFILES='#013#010+
  'else'#013#010+
  'PPUFILES:=$(firstword $(PPUFILES))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export PPUFILES'#013#010+
  #013#010+
  '[tool_data2inc]'#013#010+
  '# data2inc'#013#010+
  'ifndef',' DATA2INC'#013#010+
  'DATA2INC:=$(strip $(wildcard $(addsuffix /data2inc$(SRCEXEEXT),$(SEARC'+
  'HPATH))))'#013#010+
  'ifeq ($(DATA2INC),)'#013#010+
  'DATA2INC='#013#010+
  'else'#013#010+
  'DATA2INC:=$(firstword $(DATA2INC))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export DATA2INC'#013#010+
  #013#010+
  '[tool_diff]'#013#010+
  '# diff'#013#010+
  'ifndef DIFF'#013#010+
  'DIFF:=$','(strip $(wildcard $(addsuffix /diff$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifeq ($(DIFF),)'#013#010+
  'DIFF='#013#010+
  'else'#013#010+
  'DIFF:=$(firstword $(DIFF))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export DIFF'#013#010+
  #013#010+
  '[tool_cmp]'#013#010+
  '# cmp'#013#010+
  'ifndef CMP'#013#010+
  'CMP:=$(strip $(wildcard $(addsuffix /cmp$(SRCEXEEXT),$(S','EARCHPATH)))'+
  ')'#013#010+
  'ifeq ($(CMP),)'#013#010+
  'CMP='#013#010+
  'else'#013#010+
  'CMP:=$(firstword $(CMP))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export CMP'#013#010+
  #013#010+
  '[tool_sed]'#013#010+
  '# Sed'#013#010+
  'ifndef SED'#013#010+
  'SED:=$(strip $(wildcard $(addsuffix /sed$(SRCEXEEXT),$(SEARCHPATH))))'#013+
  #010+
  'ifeq ($(SED),)'#013#010+
  'SED='#013#010+
  'else'#013#010+
  'SED:=$(firstw','ord $(SED))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export SED'#013#010+
  #013#010+
  '[tool_upx]'#013#010+
  '# Look if UPX is found for go32v2 and win32. We can'#039't use $UPX bec'+
  'uase'#013#010+
  '# upx uses that one itself (PFV)'#013#010+
  'ifndef UPXPROG'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'UPXPROG:=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET)',',win32)'#013#010+
  'UPXPROG:=1'#013#010+
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
  'export UPXPROG'#013,#010+
  #013#010+
  '[tool_date]'#013#010+
  '# gdate/date'#013#010+
  'ifndef DATE'#013#010+
  'DATE:=$(strip $(wildcard $(addsuffix /date$(SRCEXEEXT),$(SEARCHPATH)))'+
  ')'#013#010+
  'ifeq ($(DATE),)'#013#010+
  'DATE:=$(strip $(wildcard $(addsuffix /gdate$(SRCEXEEXT),$(SEACHPATH)))'+
  ')'#013#010+
  'ifeq ($(DATE),)'#013#010+
  'DATE='#013#010+
  'else'#013#010+
  'DATE',':=$(firstword $(DATE))'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'DATE:=$(firstword $(DATE))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export DATE'#013#010+
  #013#010+
  'ifdef DATE'#013#010+
  'DATESTR:=$(shell $(DATE) +%Y%m%d)'#013#010+
  'else'#013#010+
  'DATESTR='#013#010+
  'endif'#013#010+
  #013#010+
  '[tool_zip]'#013#010+
  '# ZipProg, you can'#039't use Zip as the var name (PFV)'#013#010+
  'ifndef ZIP','PROG'#013#010+
  'ZIPPROG:=$(strip $(wildcard $(addsuffix /zip$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifeq ($(ZIPPROG),)'#013#010+
  'ZIPPROG='#013#010+
  'else'#013#010+
  'ZIPPROG:=$(firstword $(ZIPPROG))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export ZIPPROG'#013#010+
  #013#010+
  'ZIPOPT=-9'#013#010+
  'ZIPEXT=.zip'#013#010+
  #013#010+
  '[tool_tar]'#013#010+
  '# Tar'#013#010+
  'ifndef TARPR','OG'#013#010+
  'TARPROG:=$(strip $(wildcard $(addsuffix /tar$(SRCEXEEXT),$(SEARCHPATH)'+
  ')))'#013#010+
  'ifeq ($(TARPROG),)'#013#010+
  'TARPROG='#013#010+
  'else'#013#010+
  'TARPROG:=$(firstword $(TARPROG))'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export TARPROG'#013#010+
  #013#010+
  'ifeq ($(USETAR),bz2)'#013#010+
  'TAROPT=vI'#013#010+
  'TAREXT=.tar.bz2'#013#010+
  'else'#013#010+
  'TAR','OPT=vz'#013#010+
  'TAREXT=.tar.gz'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[extensions]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Default extensions'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Default needed extensio','ns (Go32v2,Linux)'#013#010+
  'LOADEREXT=.as'#013#010+
  'EXEEXT=.exe'#013#010+
  'PPLEXT=.ppl'#013#010+
  'PPUEXT=.ppu'#013#010+
  'OEXT=.o'#013#010+
  'ASMEXT=.s'#013#010+
  'SMARTEXT=.sl'#013#010+
  'STATICLIBEXT=.a'#013#010+
  'SHAREDLIBEXT=.so'#013#010+
  'RSTEXT=.rst'#013#010+
  'FPCMADE=fpcmade'#013#010+
  #013#010+
  '# Go32v1'#013#010+
  'ifeq ($(OS_TARGET),go32v1)'#013#010+
  'PPUEXT=.pp1'#013#010+
  'OEXT=.o1'#013#010+
  'ASMEX','T=.s1'#013#010+
  'SMARTEXT=.sl1'#013#010+
  'STATICLIBEXT=.a1'#013#010+
  'SHAREDLIBEXT=.so1'#013#010+
  'FPCMADE=fpcmade.v1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Go32v2'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'FPCMADE=fpcmade.dos'#013#010+
  'endif'#013#010+
  #013#010+
  '# Linux'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.lnx'#013#010+
  'en','dif'#013#010+
  #013#010+
  '# Linux'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'EXEEXT='#013#010+
  'HASSHAREDLIB=1'#013#010+
  'FPCMADE=fpcmade.freebsd'#013#010+
  'endif'#013#010+
  #013#010+
  '# Win32'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'PPUEXT=.ppw'#013#010+
  'OEXT=.ow'#013#010+
  'ASMEXT=.sw'#013#010+
  'SMARTEXT=.slw'#013#010+
  'STATICLIBEXT=.aw'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'FPCMADE=fpcma','de.w32'#013#010+
  'endif'#013#010+
  #013#010+
  '# OS/2'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'PPUEXT=.ppo'#013#010+
  'ASMEXT=.so2'#013#010+
  'OEXT=.oo2'#013#010+
  'SMARTEXT=.so'#013#010+
  'STATICLIBEXT=.ao2'#013#010+
  'SHAREDLIBEXT=.dll'#013#010+
  'FPCMADE=fpcmade.os2'#013#010+
  'endif'#013#010+
  #013#010+
  '# library prefix'#013#010+
  'LIBPREFIX=lib'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'LIBPREFIX=',#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),go32v1)'#013#010+
  'LIBPREFIX='#013#010+
  'endif'#013#010+
  #013#010+
  '# determine which .pas extension is used'#013#010+
  'ifndef PASEXT'#013#010+
  'ifdef EXEOBJECTS'#013#010+
  'override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstword $(EX'+
  'EOBJECTS)))))'#013#010+
  'else'#013#010+
  'override TESTPAS',':=$(strip $(wildcard $(addsuffix .pas,$(firstword $('+
  'UNITOBJECTS)))))'#013#010+
  'endif'#013#010+
  'ifeq ($(TESTPAS),)'#013#010+
  'PASEXT=.pp'#013#010+
  'else'#013#010+
  'PASEXT=.pas'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[dir_default]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Defaul','t Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Linux and freebsd use unix dirs with /usr/bin, /usr/lib'#013#010+
  '# When zipping use the target as default, when normal install then'#013+
  #010+
  '# use the source os as de','fault'#013#010+
  'ifdef ZIPNAME'#013#010+
  '# Zipinstall'#013#010+
  'ifeq ($(OS_TARGET),linux)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),freebsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '# Normal install'#013#010+
  'ifeq ($(OS_SOURCE),linux)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_SOURCE),fre','ebsd)'#013#010+
  'UNIXINSTALLDIR=1'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the prefix directory where to install everything'#013#010+
  'ifndef PREFIXINSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'PREFIXINSTALLDIR=/usr'#013#010+
  'else'#013#010+
  'PREFIXINSTALLDIR=/pp'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export PREFIXINSTALLDIR'#013#010+
  #013#010+
  '# Wh','ere to place the resulting zip files'#013#010+
  'ifndef DESTZIPDIR'#013#010+
  'DESTZIPDIR:=$(BASEDIR)'#013#010+
  'endif'#013#010+
  'export DESTZIPDIR'#013#010+
  #013#010+
  #013#010+
  '[dir_install]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Install Directories'#013#010+
  '####################','################################################'+
  '#'#013#010+
  #013#010+
  '# set the base directory where to install everything'#013#010+
  'ifndef BASEINSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'BASEINSTALLDIR=$(PREFIXINSTALLDIR)/lib/fpc/$(FPC_VERSION)'#013#010+
  'else'#013#010+
  'BASEINSTALLDIR=$(PREFIX','INSTALLDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set the directory where to install the binaries'#013#010+
  'ifndef BININSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'BININSTALLDIR=$(PREFIXINSTALLDIR)/bin'#013#010+
  'else'#013#010+
  'BININSTALLDIR=$(BASEINSTALLDIR)/bin/$(OS_TARGET)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# set',' the directory where to install the units.'#013#010+
  'ifndef UNITINSTALLDIR'#013#010+
  'UNITINSTALLDIR=$(BASEINSTALLDIR)/units/$(OS_TARGET)'#013#010+
  'ifdef UNITSUBDIR'#013#010+
  'UNITINSTALLDIR:=$(UNITINSTALLDIR)/$(UNITSUBDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to install shared libraries'#013#010,
  'ifndef LIBINSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'LIBINSTALLDIR=$(PREFIXINSTALLDIR)/lib'#013#010+
  'else'#013#010+
  'LIBINSTALLDIR=$(UNITINSTALLDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the source files will be stored'#013#010+
  'ifndef SOURCEINSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'SOURCEINSTALLD','IR=$(PREFIXINSTALLDIR)/src/fpc-$(FPC_VERSION)'#013#010+
  'else'#013#010+
  'SOURCEINSTALLDIR=$(BASEINSTALLDIR)/source'#013#010+
  'endif'#013#010+
  'ifdef SOURCESUBDIR'#013#010+
  'SOURCEINSTALLDIR:=$(SOURCEINSTALLDIR)/$(SOURCESUBDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the doc files will be stored'#013#010+
  'ifndef D','OCINSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'DOCINSTALLDIR=$(PREFIXINSTALLDIR)/doc/fpc-$(FPC_VERSION)'#013#010+
  'else'#013#010+
  'DOCINSTALLDIR=$(BASEINSTALLDIR)/doc'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where to install the examples, under linux we use the doc dir'#013#010+
  '# because the copytree',' command will create a subdir itself'#013#010+
  'ifndef EXAMPLEINSTALLDIR'#013#010+
  'ifdef UNIXINSTALLDIR'#013#010+
  'EXAMPLEINSTALLDIR=$(DOCINSTALLDIR)/examples'#013#010+
  'else'#013#010+
  'EXAMPLEINSTALLDIR=$(BASEINSTALLDIR)/examples'#013#010+
  'endif'#013#010+
  'ifdef EXAMPLESUBDIR'#013#010+
  'EXAMPLEINSTALLDIR:=$(EXAMPLEI','NSTALLDIR)/$(EXAMPLESUBDIR)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Where the some extra (data)files will be stored'#013#010+
  'ifndef DATAINSTALLDIR'#013#010+
  'DATAINSTALLDIR=$(BASEINSTALLDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[dir_gcclib]'#013#010+
  '# On linux, try to find where libgcc.a is.'#013#010+
  'ifdef inUnix'#013#010+
  'ifnd','ef GCCLIBDIR'#013#010+
  'GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '#039'{ print'+
  ' $$4 } '#039'`)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export GCCLIBDIR'#013#010+
  #013#010+
  #013#010+
  '[dir_otherlib]'#013#010+
  '# Where to find other libraries'#013#010+
  'ifdef inUnix'#013#010+
  'ifndef OTHERLIBDIR'#013#010+
  'OTHERLIBDIR:=$(shell grep -v ','"^\#" /etc/ld.so.conf | awk '#039'{ ORS='+
  '" "; print $1 }'#039')'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'export OTHERLIBDIR'#013#010+
  #013#010+
  #013#010+
  '[redir]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Redirection'#013#010+
  '#####################################################','###############'+
  '#'#013#010+
  #013#010+
  'ifndef REDIRFILE'#013#010+
  'REDIRFILE=log'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef REDIR'#013#010+
  'ifndef inUnix'#013#010+
  'override FPC=redir -eo $(FPC)'#013#010+
  'endif'#013#010+
  '# set the verbosity to max'#013#010+
  'override FPCOPT+=-va'#013#010+
  'override REDIR:= >> $(REDIRFILE)'#013#010+
  'endif'#013#010+
  #013#010+
  '[command_begin]'#013#010+
  '##','##################################################################'+
  '#'#013#010+
  '# Compiler Command Line'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '# Load commandline OPTDEF and add FPC_CPU define'#013#010+
  'override FPCOPTDEF:=-d$','(CPU_TARGET)'#013#010+
  #013#010+
  '# Load commandline OPT and add target and unit dir to be sure'#013#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#013#010+
  'override FPCOPT+=-T$(OS_TARGET)'#013#010+
  'endif'#013#010+
  #013#010+
  '# User dirs should be first, so they are looked at first'#013#010+
  'ifdef UNITDIR'#013#010+
  'override FP','COPT+=$(addprefix -Fu,$(UNITDIR))'#013#010+
  'endif'#013#010+
  'ifdef LIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#013#010+
  'endif'#013#010+
  'ifdef OBJDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDIR))'#013#010+
  'endif'#013#010+
  'ifdef INCDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fi,$(INCDIR))'#013#010+
  'endif'#013#010,
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
  'ifdef DEBUG'#013#010+
  'override FPCOPT+=-gl -dDEBUG'#013#010+
  'endif'#013#010+
  #013#010+
  '# Release mode (strip, optimize and do','n'#039't load ppc386.cfg)'#013#010+
  '# 0.99.12b has a bug in the optimizer so don'#039't use it by default'#013+
  #010+
  'ifdef RELEASE'#013#010+
  'ifeq ($(FPC_VERSION),0.99.12)'#013#010+
  'override FPCOPT+=-Xs -OGp3 -n'#013#010+
  'else'#013#010+
  'override FPCOPT+=-Xs -OG2p3 -n'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Strip'#013#010+
  'ifdef STRIP'#013,#010+
  'override FPCOPT+=-Xs'#013#010+
  'endif'#013#010+
  #013#010+
  '# Optimizer'#013#010+
  'ifdef OPTIMIZE'#013#010+
  'override FPCOPT+=-OG2p3'#013#010+
  'endif'#013#010+
  #013#010+
  '# Verbose settings (warning,note,info)'#013#010+
  'ifdef VERBOSE'#013#010+
  'override FPCOPT+=-vwni'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_needopt]'#013#010+
  'ifdef NEEDOPT'#013#010+
  'override FPCOPT+=$(NE','EDOPT)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_needunit]'#013#010+
  'ifdef NEEDUNITDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fu,$(NEEDUNITDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_unitsdir]'#013#010+
  'ifdef UNITSDIR'#013#010+
  'override FPCOPT+=-Fu$(UNITSDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_needlib]'#013#010+
  'ifdef NEEDLIBDIR'#013#010+
  'over','ride FPCOPT+=$(addprefix -Fl,$(NEEDLIBDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_needobj]'#013#010+
  'ifdef NEEDOBJDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fo,$(NEEDOBJDIR))'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_needinc]'#013#010+
  'ifdef NEEDINCDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fi,$(NEEDINCDIR))'#013#010+
  'e','ndif'#013#010+
  #013#010+
  '[command_gcclib]'#013#010+
  '# Add GCC lib path if asked'#013#010+
  'ifdef GCCLIBDIR'#013#010+
  'override FPCOPT+=-Fl$(GCCLIBDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '[command_otherlib]'#013#010+
  '# Add Other dirs path if asked'#013#010+
  'ifdef OTHERLIBDIR'#013#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#013#010+
  'endi','f'#013#010+
  #013#010+
  '[command_target]'#013#010+
  '# Target dirs and the prefix to use for clean/install'#013#010+
  'ifdef TARGETDIR'#013#010+
  'override FPCOPT+=-FE$(TARGETDIR)'#013#010+
  'ifeq ($(TARGETDIR),.)'#013#010+
  'override TARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override TARGETDIRPREFIX=$(TARGETDIR)/'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'i','fdef UNITTARGETDIR'#013#010+
  'override FPCOPT+=-FU$(UNITTARGETDIR)'#013#010+
  'ifeq ($(UNITTARGETDIR),.)'#013#010+
  'override UNITTARGETDIRPREFIX='#013#010+
  'else'#013#010+
  'override UNITTARGETDIRPREFIX=$(TARGETDIR)/'#013#010+
  'endif'#013#010+
  'else'#013#010+
  'ifdef TARGETDIR'#013#010+
  'override UNITTARGETDIR=$(TARGETDIR)'#013#010+
  'overrid','e UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '[command_end]'#013#010+
  '# Add commandline options last so they can override'#013#010+
  'ifdef OPT'#013#010+
  'override FPCOPT+=$(OPT)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Add defines from FPCOPTDEF to FPCOPT'#013#010+
  'ifdef FPCOPTDEF'#013#010+
  'override FPC','OPT+=$(FPCOPTDEF)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Error file ?'#013#010+
  'ifdef ERRORFILE'#013#010+
  'override FPCOPT+=-Fr$(ERRORFILE)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Was a config file specified ?'#013#010+
  'ifdef CFGFILE'#013#010+
  'override FPCOPT+=@$(CFGFILE)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[command_win32file]'#013#010+
  '# For win32 the options ar','e passed using the file fpcext.cmd'#013#010+
  'ifndef FPCEXTFILE'#013#010+
  'FPCEXTFILE=fpcext.cmd'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_SOURCE),win32)'#013#010+
  'override TMPVAR:=$(shell "$(DEL) $(FPCEXTFILE)")'#013#010+
  'override TMPVAR:=$(foreach A,$(FPCOPT),$(shell "$(ECHOE) "$(A)" >> $(F'+
  'PCEXTFIL','E)"))'#013#010+
  'override FPCOPT:=@$(FPCEXTFILE)'#013#010+
  'endif'#013#010+
  #013#010+
  '[command_win32env]'#013#010+
  '# For win32 the options are passed using the environment FPCEXTCMD'#013+
  #010+
  'ifeq ($(OS_SOURCE),win32)'#013#010+
  'override FPCEXTCMD:=$(FPCOPT)'#013#010+
  'override FPCOPT:=!FPCEXTCMD'#013#010+
  'export FPCEXTCMD',#013#010+
  'endif'#013#010+
  #013#010+
  '[command_compiler]'#013#010+
  '# Compiler commandline'#013#010+
  'override COMPILER:=$(FPC) $(FPCOPT)'#013#010+
  #013#010+
  '# also call ppas if with command option -s'#013#010+
  '# but only if the OS_SOURCE and OS_TARGE are equal'#013#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#013#010+
  'EXECPPAS='#013#010,
  'else'#013#010+
  'ifeq ($(OS_SOURCE),$(OS_TARGET))'#013#010+
  'EXECPPAS:=@$(PPAS)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[standardrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Standard rules'#013#010+
  '########################################################','############'+
  '#'#013#010+
  #013#010+
  '[packagedependrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Package depends'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  #013#010+
  '[loaderrules]'#013#010+
  '#####################','###############################################'+
  '#'#013#010+
  '# Loaders'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_loaders'#013#010+
  #013#010+
  '%$(OEXT): %$(LOADEREXT)'#013#010+
  '        $(AS) -o $*$(OEXT) $<'#013#010+
  #013#010+
  'override ALLTARGET+=fpc_l','oaders'#013#010+
  'override CLEANTARGET+=fpc_loaders_clean'#013#010+
  'override SHOWINSTALLTARGET+=fpc_loaders_showinstall'#013#010+
  'override INSTALLTARGET+=fpc_loaders_install'#013#010+
  #013#010+
  'override LOADEROFILES:=$(addsuffix $(OEXT),$(LOADEROBJECTS))'#013#010+
  #013#010+
  'fpc_loaders: $(LOADEROFILES',')'#013#010+
  #013#010+
  'fpc_loaders_clean:'#013#010+
  '        -$(DEL) $(LOADEROFILES)'#013#010+
  #013#010+
  'fpc_loaders_showinstall:'#013#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNITINSTALLDIR)/,$(LOADEROFILES)'+
  ')'#013#010+
  #013#010+
  'fpc_loaders_install:'#013#010+
  '        $(MKDIR) $(UNITINSTALLDIR)'#013#010+
  '        $(INSTALL) $(','LOADEROFILES) $(UNITINSTALLDIR)'#013#010+
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
  'override ALLTARG','ET+=fpc_units'#013#010+
  #013#010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(UNITOBJECTS))'#013#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES)'#013#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES)'#013#010+
  #013#010+
  'fpc_units: $(UNITPPUFILES)'#013#010+
  #013#010+
  #013#010+
  #013#010+
  '[exerules]'#013#010+
  '##############################','######################################'+
  '#'#013#010+
  '# Exes'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_exes'#013#010+
  #013#010+
  'ifdef EXEOBJECTS'#013#010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(EXEOBJECTS))'#013#010+
  'override EXEOFILES:=$(ad','dsuffix $(OEXT),$(EXEOBJECTS)) $(addprefix $'+
  '(LIBPREFIX),$(addsuffix $(STATICLIBEXT),$(EXEOBJECTS)))'#013#010+
  #013#010+
  'override ALLTARGET+=fpc_exes'#013#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#013#010+
  'override CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#013#010+
  #013#010+
  'endif'#013#010+
  #013#010+
  'fpc_exe','s: $(EXEFILES)'#013#010+
  #013#010+
  #013#010+
  '[rstrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Resource strings'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  'ifdef RSTOBJECTS'#013#010+
  'override RSTFILES=$(addsuf','fix $(RSTEXT),$(RSTOBJECTS))'#013#010+
  #013#010+
  'override CLEANRSTFILES+=$(RSTFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[examplerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Examples'#013#010+
  '#############################################################','#######'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_examples fpc_test'#013#010+
  #013#010+
  'ifdef EXAMPLEOBJECTS'#013#010+
  'override EXAMPLESOURCEFILES:=$(addsuffix $(PASEXT),$(EXAMPLEOBJECTS))'#013+
  #010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),$(EXAMPLEOBJECTS))'#013#010+
  'override EXAMPLEOFILES:=$(addsuffix $','(OEXT),$(EXAMPLEOBJECTS)) $(add'+
  'prefix $(LIBPREFIX),$(addsuffix $(STATICLIBEXT),$(EXAMPLEOBJECTS)))'#013+
  #010+
  #013#010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_examples: all $(EXAMPLEFILES) $(addsuffix _all,$(EXAMPLEDIROBJECTS'+
  ')',')'#013#010+
  #013#010+
  'fpc_test: examples'#013#010+
  #013#010+
  #013#010+
  '[compilerules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# General compile rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_packages fpc_a','ll fpc_debug'#013#010+
  #013#010+
  '$(FPCMADE): $(ALLTARGET)'#013#010+
  '        @$(ECHO) Compiled > $(FPCMADE)'#013#010+
  #013#010+
  'fpc_packages: $(COMPILEPACKAGES)'#013#010+
  #013#010+
  'fpc_all: fpc_packages $(FPCMADE)'#013#010+
  #013#010+
  'fpc_debug:'#013#010+
  '        $(MAKE) all DEBUG=1'#013#010+
  #013#010+
  '# Search paths for .ppu if targetdir is s','et'#013#010+
  'ifdef UNITTARGETDIR'#013#010+
  'vpath %$(PPUEXT) $(UNITTARGETDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  '# General compile rules, available for both possible PASEXT'#013#010+
  #013#010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp'#013#010+
  #013#010+
  '%$(PPUEXT): %.pp'#013#010+
  '        $(COMPILER) $< $(REDIR)'#013#010+
  '      ','  $(EXECPPAS)'#013#010+
  #013#010+
  '%$(PPUEXT): %.pas'#013#010+
  '        $(COMPILER) $< $(REDIR)'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEXT): %.pp'#013#010+
  '        $(COMPILER) $< $(REDIR)'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013#010+
  '%$(EXEEXT): %.pas'#013#010+
  '        $(COMPILER) $< $(REDIR)'#013#010+
  '        $(EXECPPAS)'#013#010+
  #013,#010+
  #013#010+
  '[libraryrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Library'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_smart fpc_shared'#013#010+
  #013#010+
  'ifdef LIBVERSION'#013#010+
  'LIBFULLNAME=$(LI','BNAME).$(LIBVERSION)'#013#010+
  'else'#013#010+
  'LIBFULLNAME=$(LIBNAME)'#013#010+
  'endif'#013#010+
  #013#010+
  '# Default sharedlib units are all unit objects'#013#010+
  'ifndef SHAREDLIBUNITOBJECTS'#013#010+
  'SHAREDLIBUNITOBJECTS:=$(UNITOBJECTS)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_smart:'#013#010+
  '        $(MAKE) all LINKSMART=1 CREATESMART','=1'#013#010+
  #013#010+
  'fpc_shared: all'#013#010+
  'ifdef HASSHAREDLIB'#013#010+
  'ifndef LIBNAME'#013#010+
  '        @$(ECHO) "LIBNAME not set"'#013#010+
  'else'#013#010+
  '        $(PPUMOVE) $(SHAREDLIBUNITOBJECTS) -o$(LIBFULLNAME)'#013#010+
  'endif'#013#010+
  'else'#013#010+
  '        @$(ECHO) "Shared Libraries not supported"'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[in','stallrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Install rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_showinstall fpc_install'#013#010+
  #013#010+
  'ifdef EXTRAINSTALLUNITS'#013#010+
  'ov','erride INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(EXTRAINSTALLUNITS)'+
  ')'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  'override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTALL'+
  'PPUFILES))'#013#010+
  'ifdef PPUFILES'#013#010+
  'INSTALLPPULINKFILES:=$(shell $(PPUFILES) ','-S -O $(INSTALLPPUFILES))'#013+
  #010+
  'else'#013#010+
  'INSTALLPPULINKFILES:=$(wildcard $(subst $(PPUEXT),$(OEXT),$(INSTALLPPU'+
  'FILES)) $(addprefix $(LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(I'+
  'NSTALLPPUFILES))))'#013#010+
  'endif'#013#010+
  'override INSTALLPPULINKFILES:=$(addprefi','x $(UNITTARGETDIRPREFIX),$(I'+
  'NSTALLPPULINKFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  'override INSTALLEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(INSTALLEXEF'+
  'ILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_showinstall: $(SHOWINSTALLTARGET)'#013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  '        @','$(ECHO) -e $(addprefix "\n"$(BININSTALLDIR)/,$(INSTALLEXEFI'+
  'LES))'#013#010+
  'endif'#013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNITINSTALLDIR)/,$(INSTALLPPUFIL'+
  'ES))'#013#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#013#010+
  '        @$(ECHO) -e $(addprefix "\n','"$(UNITINSTALLDIR)/,$(INSTALLPPUL'+
  'INKFILES))'#013#010+
  'endif'#013#010+
  'ifneq ($(wildcard $(LIBFULLNAME)),)'#013#010+
  '        @$(ECHO) $(LIBINSTALLDIR)/$(LIBFULLNAME)'#013#010+
  'ifdef HASSHAREDLIB'#013#010+
  '        @$(ECHO) $(LIBINSTALLDIR)/$(LIBNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef EXTRAINS','TALLFILES'#013#010+
  '        @$(ECHO) -e $(addprefix "\n"$(DATAINSTALLDIR)/,$(EXTRAINSTALLF'+
  'ILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_install: $(INSTALLTARGET)'#013#010+
  '# Create UnitInstallFiles'#013#010+
  'ifdef INSTALLEXEFILES'#013#010+
  '        $(MKDIR) $(BININSTALLDIR)'#013#010+
  '# Compress the exes if upx',' is defined'#013#010+
  'ifdef UPXPROG'#013#010+
  '        -$(UPXPROG) $(INSTALLEXEFILES)'#013#010+
  'endif'#013#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(BININSTALLDIR)'#013#010+
  'endif'#013#010+
  'ifdef INSTALLPPUFILES'#013#010+
  '        $(MKDIR) $(UNITINSTALLDIR)'#013#010+
  '        $(INSTALL) $(INSTALLPPUFILES) $(','UNITINSTALLDIR)'#013#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#013#010+
  '        $(INSTALL) $(INSTALLPPULINKFILES) $(UNITINSTALLDIR)'#013#010+
  'endif'#013#010+
  'ifneq ($(wildcard $(LIBFULLNAME)),)'#013#010+
  '        $(MKDIR) $(LIBINSTALLDIR)'#013#010+
  '        $(INSTALL) $(LIBFULLNAME) $(LIBINSTALLDIR)',#013#010+
  'ifdef inUnix'#013#010+
  '        ln -sf $(LIBFULLNAME) $(LIBINSTALLDIR)/$(LIBNAME)'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  'ifdef EXTRAINSTALLFILES'#013#010+
  '        $(MKDIR) $(DATAINSTALLDIR)'#013#010+
  '        $(INSTALL) $(EXTRAINSTALLFILES) $(DATAINSTALLDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[sourceinstal','lrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# SourceInstall rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_sourceinstall'#013#010+
  #013#010+
  'ifndef SOURCETOPDIR'#013#010+
  'SOURCETOPDIR=$','(BASEDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_sourceinstall: clean'#013#010+
  '        $(MKDIR) $(SOURCEINSTALLDIR)'#013#010+
  '        $(COPYTREE) $(SOURCETOPDIR) $(SOURCEINSTALLDIR)'#013#010+
  #013#010+
  '[exampleinstallrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# e','xampleinstall rules'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_exampleinstall'#013#010+
  #013#010+
  'fpc_exampleinstall: $(addsuffix _clean,$(EXAMPLEDIROBJECTS))'#013#010+
  'ifdef EXAMPLESOURCEFILES'#013#010+
  '        $(MKDIR) $(EXAMPLEIN','STALLDIR)'#013#010+
  '        $(COPY) $(EXAMPLESOURCEFILES) $(EXAMPLEINSTALLDIR)'#013#010+
  'endif'#013#010+
  'ifdef EXAMPLEDIROBJECTS'#013#010+
  'ifndef EXAMPLESOURCEFILES'#013#010+
  '        $(MKDIR) $(EXAMPLEINSTALLDIR)'#013#010+
  'endif'#013#010+
  '        $(COPYTREE) $(addsuffix /*,$(EXAMPLEDIROBJECTS)) $(EXAMP','LEIN'+
  'STALLDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[zipinstallrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Zip'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '.PHONY: fpc_zipinstall'#013#010+
  #013#010+
  '# Create suffix to',' add'#013#010+
  'ifndef PACKAGESUFFIX'#013#010+
  'PACKAGESUFFIX=$(OS_TARGET)'#013#010+
  'ifeq ($(OS_TARGET),go32v2)'#013#010+
  'PACKAGESUFFIX=go32'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),win32)'#013#010+
  'PACKAGESUFFIX=w32'#013#010+
  'endif'#013#010+
  'ifeq ($(OS_TARGET),os2)'#013#010+
  'PACKAGESUFFIX=emx'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Temporary path',' to pack a file'#013#010+
  'ifndef PACKDIR'#013#010+
  'ifndef inUnix'#013#010+
  'PACKDIR=$(BASEDIR)/pack_tmp'#013#010+
  'else'#013#010+
  'PACKDIR=/tmp/fpc-pack'#013#010+
  'endif'#013#010+
  'endif'#013#010+
  #013#010+
  '# Maybe create default zipname from packagename'#013#010+
  'ifndef ZIPNAME'#013#010+
  'ifdef PACKAGENAME'#013#010+
  'ZIPNAME=$(PACKAGEPREFIX)$(PACKAGEN','AME)$(PACKAGESUFFIX)'#013#010+
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
  'ifdef inOS2'#013#010+
  'USEZIPWRAPPER=1'#013#010+
  'endif'#013#010+
  #013#010+
  '# Create commands to create th','e zip/tar file'#013#010+
  'ZIPWRAPPER=$(DESTZIPDIR)/fpczip$(BATCHEXT)'#013#010+
  'ZIPCMD_CDPACK:=cd $(subst /,$(PATHSEP),$(PACKDIR))'#013#010+
  'ZIPCMD_CDBASE:=cd $(subst /,$(PATHSEP),$(BASEDIR))'#013#010+
  'ifdef USETAR'#013#010+
  'ZIPDESTFILE:=$(DESTZIPDIR)/$(ZIPNAME)$(TAREXT)'#013#010+
  'ZIPCMD_ZIP:=$(','TARPROG) cf$(TAROPT) $(ZIPDESTFILE) *'#013#010+
  'else'#013#010+
  'ZIPDESTFILE:=$(DESTZIPDIR)/$(ZIPNAME)$(ZIPEXT)'#013#010+
  'ZIPCMD_ZIP:=$(subst /,$(PATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDESTFI'+
  'LE) *'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_zipinstall:'#013#010+
  'ifndef ZIPNAME'#013#010+
  '        @$(ECHO) "Please spe','cify ZIPNAME!"'#013#010+
  '        @exit 1'#013#010+
  'else'#013#010+
  '        $(MAKE) $(ZIPTARGET) PREFIXINSTALLDIR=$(PACKDIR)'#013#010+
  '        $(DEL) $(ZIPDESTFILE)'#013#010+
  'ifdef USEZIPWRAPPER'#013#010+
  'ifneq ($(ECHO),echo)'#013#010+
  '        $(ECHO) "$(ZIPCMD_CDPACK)" > $(ZIPWRAPPER)'#013#010+
  '        $(ECHO) "','$(ZIPCMD_ZIP)" >> $(ZIPWRAPPER)'#013#010+
  '        $(ECHO) "$(ZIPCMD_CDBASE)" >> $(ZIPWRAPPER)'#013#010+
  'else'#013#010+
  '        $(ECHO) $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)'#013#010+
  '        $(ECHO) $(ZIPCMD_ZIP) >> $(ZIPWRAPPER)'#013#010+
  '        $(ECHO) $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)'#013,#010+
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
  '        $(DELTREE) $(PACKDIR)'#013#010+
  'endif'#013#010+
  #013#010+
  #013#010+
  '[zips','ourceinstallrules]'#013#010+
  '.PHONY:  fpc_zipsourceinstall'#013#010+
  #013#010+
  'fpc_zipsourceinstall:'#013#010+
  '        $(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall PACKAGESUFFIX=s'+
  'rc'#013#010+
  #013#010+
  #013#010+
  '[zipexampleinstallrules]'#013#010+
  '.PHONY:  fpc_zipexampleinstall'#013#010+
  #013#010+
  'fpc_zipexampleinstall:'#013#010+
  '  ','      $(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall PACKAGESUFFI'+
  'X=exm'#013#010+
  #013#010+
  #013#010+
  '[cleanrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Clean rules'#013#010+
  '###############################################################','#####'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_clean fpc_cleanall fpc_distclean'#013#010+
  #013#010+
  'ifdef EXEFILES'#013#010+
  'override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES'+
  '))'#013#010+
  'endif'#013#010+
  #013#010+
  'ifdef EXTRACLEANUNITS'#013#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(EXTRACLEANUN','ITS))'#013+
  #010+
  'endif'#013#010+
  #013#010+
  'ifdef CLEANPPUFILES'#013#010+
  'override CLEANPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPPUF'+
  'ILES))'#013#010+
  '# Get the .o and .a files created for the units'#013#010+
  'ifdef PPUFILES'#013#010+
  'CLEANPPULINKFILES:=$(shell $(PPUFILES) $(CLEANPPUFILES))'#013#010+
  'el','se'#013#010+
  'CLEANPPULINKFILES:=$(wildcard $(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILE'+
  'S)) $(addprefix $(LIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLEAN'+
  'PPUFILES))))'#013#010+
  'endif'#013#010+
  'override CLEANPPULINKFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEAN'+
  'PPULI','NKFILES))'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_clean: $(CLEANTARGET)'#013#010+
  'ifdef CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  'ifdef CLEANPPUFILES'#013#010+
  '        -$(DEL) $(CLEANPPUFILES)'#013#010+
  'endif'#013#010+
  'ifneq ($(CLEANPPULINKFILES),)'#013#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#013#010,
  'endif'#013#010+
  'ifdef CLEANRSTFILES'#013#010+
  '        -$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))'#013+
  #010+
  'endif'#013#010+
  'ifdef EXTRACLEANFILES'#013#010+
  '        -$(DEL) $(EXTRACLEANFILES)'#013#010+
  'endif'#013#010+
  'ifdef LIBNAME'#013#010+
  '        -$(DEL) $(LIBNAME) $(LIBFULLNAME)'#013#010+
  'endif'#013#010+
  '    ','    -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTFILE) $(REDIRFIL'+
  'E)'#013#010+
  #013#010+
  'fpc_distclean: fpc_clean'#013#010+
  #013#010+
  '# Also run clean first if targetdir is set. Unittargetdir is always'#013+
  #010+
  '# set if targetdir or unittargetdir is specified'#013#010+
  'ifdef UNITTARGETDIR'#013#010+
  'TA','RGETDIRCLEAN=fpc_clean'#013#010+
  'endif'#013#010+
  #013#010+
  'fpc_cleanall: $(CLEANTARGET) $(TARGETDIRCLEAN)'#013#010+
  'ifdef CLEANEXEFILES'#013#010+
  '        -$(DEL) $(CLEANEXEFILES)'#013#010+
  'endif'#013#010+
  '        -$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIB'+
  'EXT) *$(SHAREDLIBEXT) *$(PP','LEXT)'#013#010+
  '        -$(DELTREE) *$(SMARTEXT)'#013#010+
  '        -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTFILE) $(REDIRFILE)'+
  #013#010+
  #013#010+
  #013#010+
  '[dependrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Depend rules'#013#010+
  '#####################','###############################################'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_depend'#013#010+
  #013#010+
  'fpc_depend:'#013#010+
  '        $(PPDEP) $(UNITOBJECTS)'#013#010+
  #013#010+
  #013#010+
  '[inforules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Info rules'#013#010+
  '##################','##################################################'+
  '#'#013#010+
  #013#010+
  '.PHONY: fpc_info fpc_cfginfo fpc_objectinfo fpc_toolsinfo fpc_installi'+
  'nfo \'#013#010+
  '        fpc_dirinfo'#013#010+
  #013#010+
  'fpc_info: $(INFOTARGET)'#013#010+
  #013#010+
  '[info_cfg]'#013#010+
  'fpc_infocfg:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECH','O)  == Configuration info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  FPC....... $(FPC)'#013#010+
  '        @$(ECHO)  Version... $(FPC_VERSION)'#013#010+
  '        @$(ECHO)  CPU....... $(CPU_TARGET)'#013#010+
  '        @$(ECHO)  Source.... $(OS_SOURCE)'#013#010+
  '        @$(ECHO)  Targe','t.... $(OS_TARGET)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[info_dirs]'#013#010+
  'fpc_infodirs:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Directory info =='#013#010+
  '        @$(ECHO)'#013#010+
  'ifdef NEEDGCCLIB'#013#010+
  '        @$(ECHO)  GCC library is needed.'#013#010+
  'endif'#013#010+
  'ifdef NEEDOTHERLIB'#013#010+
  '        @','$(ECHO)  Other library is needed.'#013#010+
  'endif'#013#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#013#010+
  '        @$(ECHO)  Other library... $(OTHERLIBDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[info_tools]'#013,#010+
  'fpc_infotools:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Tools info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  Pwd....... $(PWD)'#013#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#013#010+
  'ifdef PPDEP'#013#010+
  '        @$(ECHO)  PPDep..... $(PPDEP)'#013#010+
  'endif'#013#010+
  'ifdef PPUMOVE'#013#010+
  '  ','      @$(ECHO)  PPUMove... $(PPUMOVE)'#013#010+
  'endif'#013#010+
  'ifdef PPUFILES'#013#010+
  '        @$(ECHO)  PPUFiles.. $(PPUFILES)'#013#010+
  'endif'#013#010+
  'ifdef DATA2INC'#013#010+
  '        @$(ECHO)  Data2Inc.. $(DATA2INC)'#013#010+
  'endif'#013#010+
  'ifdef SED'#013#010+
  '        @$(ECHO)  Sed....... $(SED)'#013#010+
  'endif'#013#010+
  'ifdef DAT','E'#013#010+
  '        @$(ECHO)  Date...... $(DATE)'#013#010+
  'endif'#013#010+
  'ifdef DIFF'#013#010+
  '        @$(ECHO)  Diff...... $(DIFF)'#013#010+
  'endif'#013#010+
  'ifdef CMP'#013#010+
  '        @$(ECHO)  Cmp....... $(CMP)'#013#010+
  'endif'#013#010+
  'ifdef UPXPROG'#013#010+
  '        @$(ECHO)  Upx....... $(UPXPROG)'#013#010+
  'endif'#013#010+
  'ifdef ZIPPROG'#013#010+
  '  ','      @$(ECHO)  Zip....... $(ZIPPROG)'#013#010+
  'endif'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[info_objects]'#013#010+
  'fpc_infoobjects:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Object info =='#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  LoaderObjects..... $(LOADEROBJECTS)'#013#010+
  '        @$(E','CHO)  UnitObjects....... $(UNITOBJECTS)'#013#010+
  '        @$(ECHO)  ExeObjects........ $(EXEOBJECTS)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  ExtraCleanUnits... $(EXTRACLEANUNITS)'#013#010+
  '        @$(ECHO)  ExtraCleanFiles... $(EXTRACLEANFILES)'#013#010+
  '        @$(ECHO',')'#013#010+
  '        @$(ECHO)  ExtraInstallUnits. $(EXTRAINSTALLUNITS)'#013#010+
  '        @$(ECHO)  ExtraInstallFiles. $(EXTRAINSTALLFILES)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[info_install]'#013#010+
  'fpc_infoinstall:'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  == Install info =='#013#010+
  '        @','$(ECHO)'#013#010+
  'ifdef DATE'#013#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#013#010+
  'endif'#013#010+
  'ifdef PACKAGEPREFIX'#013#010+
  '        @$(ECHO)  PackagePrefix........ $(PACKAGEPREFIX)'#013#010+
  'endif'#013#010+
  'ifdef PACKAGENAME'#013#010+
  '        @$(ECHO)  PackageName.......... $(PACKAGENAME)'#013,#010+
  'endif'#013#010+
  '        @$(ECHO)  PackageSuffix........ $(PACKAGESUFFIX)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  BaseInstallDir....... $(BASEINSTALLDIR)'#013#010+
  '        @$(ECHO)  BinInstallDir........ $(BININSTALLDIR)'#013#010+
  '        @$(ECHO)  LibInstallDir........',' $(LIBINSTALLDIR)'#013#010+
  '        @$(ECHO)  UnitInstallDir....... $(UNITINSTALLDIR)'#013#010+
  '        @$(ECHO)  SourceInstallDir..... $(SOURCEINSTALLDIR)'#013#010+
  '        @$(ECHO)  DocInstallDir........ $(DOCINSTALLDIR)'#013#010+
  '        @$(ECHO)  DataInstallDir....... $(D','ATAINSTALLDIR)'#013#010+
  '        @$(ECHO)'#013#010+
  '        @$(ECHO)  DestZipDir........... $(DESTZIPDIR)'#013#010+
  '        @$(ECHO)  ZipName.............. $(ZIPNAME)'#013#010+
  '        @$(ECHO)'#013#010+
  #013#010+
  '[directorytargets]'#013#010+
  '###########################################################','#########'+
  '#'#013#010+
  '# Directories'#013#010+
  '#####################################################################'#013+
  #010+
  #013#010+
  '[localmakefile]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Local Makefile'#013#010+
  '##################################','##################################'+
  '#'#013#010+
  #013#010+
  'ifneq ($(wildcard fpcmake.loc),)'#013#010+
  'include fpcmake.loc'#013#010+
  'endif'#013#010+
  #013#010+
  '[userrules]'#013#010+
  '#####################################################################'#013+
  #010+
  '# Users rules'#013#010+
  '######################################','##############################'+
  '#'#013#010+
  #013#010+
  #013#010+
  '[end]'#013#010
);
