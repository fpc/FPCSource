{$ifdef Delphi}
const fpcmakeini : array[0..114] of string[240]=(
{$else Delphi}
const fpcmakeini : array[0..114,1..240] of char=(
{$endif Delphi}
  ';'#010+
  '; Templates used by fpcmake to create a Makefile from Makefile.fpc'#010+
  ';'#010+
  #010+
  '[osdetect]'#010+
  '#####################################################################'#010+
  '# Autodetect OS (Linux or Dos or Windows NT)'#010+
  '# define inlinux when running under linux'#010,
  '# define inWinNT when running under WinNT'#010+
  '#####################################################################'#010+
  #010+
  '# We need only / in the path'#010+
  'override PATH:=$(subst \,/,$(PATH))'#010+
  #010+
  '# Search for PWD and determine also if we are under linux'#010+
  'PWD',':=$(strip $(wildcard $(addsuffix /pwd.exe,$(subst ;, ,$(PATH)))))'+
  #010+
  'ifeq ($(PWD),)'#010+
  'PWD:=$(strip $(wildcard $(addsuffix /pwd,$(subst :, ,$(PATH)))))'#010+
  'ifeq ($(PWD),)'#010+
  'nopwd:'#010+
  '        @echo You need the GNU utils package to use this Makefile!'#010+
  '     ','   @echo Get ftp://ftp.freepascal.org/pub/fpc/dist/go32v2/utilg'+
  'o32.zip'#010+
  '        @exit'#010+
  'else'#010+
  'inlinux=1'#010+
  'endif'#010+
  'else'#010+
  'PWD:=$(firstword $(PWD))'#010+
  'endif'#010+
  #010+
  '# Detect NT - NT sets OS to Windows_NT'#010+
  'ifndef inlinux'#010+
  'ifeq ($(OS),Windows_NT)'#010+
  'inWinNT=1'#010+
  'endif'#010+
  'end','if'#010+
  #010+
  '# Detect OS/2 - OS/2 has OS2_SHELL defined'#010+
  'ifndef inlinux'#010+
  'ifndef inWinNT'#010+
  'ifdef OS2_SHELL'#010+
  'inOS2=1'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# The extension of executables'#010+
  'ifdef inlinux'#010+
  'EXEEXT='#010+
  'else'#010+
  'EXEEXT=.exe'#010+
  'endif'#010+
  #010+
  '# The path which is searched separated by s','paces'#010+
  'ifdef inlinux'#010+
  'SEARCHPATH=$(subst :, ,$(PATH))'#010+
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
  '###########################################################','#########'+
  '#'#010+
  '# Default target'#010+
  '#####################################################################'#010+
  #010+
  '[fpcdetect]'#010+
  '#####################################################################'#010+
  '# FPC version/target Detection'#010+
  '############################','########################################'+
  '#'#010+
  #010+
  '# What compiler to use ?'#010+
  'ifndef FPC'#010+
  '# Compatibility with old makefiles'#010+
  'ifdef PP'#010+
  'FPC=$(PP)'#010+
  'else'#010+
  'ifdef inOS2'#010+
  'FPC=ppos2$(EXEEXT)'#010+
  'else'#010+
  'FPC=ppc386$(EXEEXT)'#010+
  'endif'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Target OS'#010+
  'ifndef OS_TARGE','T'#010+
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
  '# Source CPU'#010+
  'ifndef CPU_SOURCE'#010+
  'CPU_SOURCE:=$(shell $(FPC) -i','SP)'#010+
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
  '#####################################################################'#010+
  '# ','FPCDIR Setting'#010+
  '#####################################################################'#010+
  #010+
  '# Test FPCDIR to look if the RTL dir exists'#010+
  'ifdef FPCDIR'#010+
  'override FPCDIR:=$(subst \,/,$(FPCDIR))'#010+
  'ifeq ($(wildcard $(FPCDIR)/rtl),)'#010+
  'override FPCDIR=wrong'#010+
  'e','ndif'#010+
  'else'#010+
  'override FPCDIR=wrong'#010+
  'endif'#010+
  #010+
  '[fpcdirdetect]'#010+
  '# Detect FPCDIR'#010+
  'ifeq ($(FPCDIR),wrong)'#010+
  'ifdef inlinux'#010+
  'override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)'#010+
  'else'#010+
  'override FPCDIR:=$(subst /$(FPC)$(EXEEXT),,$(firstword $(strip $(wildc'+
  'ard $(addsuffi','x /$(FPC)$(EXEEXT),$(SEARCHPATH))))))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '[fpcdirsubs]'#010+
  '# Create units dir'#010+
  'ifneq ($(FPCDIR),.)'#010+
  'UNITSDIR=$(FPCDIR)/units/$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  '[checkfpcdirsubs]'#010+
  '# Check if the dirs really exists, else turn it off'#010+
  'ifeq ($(wildcard $(UNI','TSDIR)),)'#010+
  'UNITSDIR='#010+
  'endif'#010+
  'ifeq ($(wildcard $(TOOLKITSDIR)),)'#010+
  'TOOLKITSDIR='#010+
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
  '################################','####################################'+
  '#'#010+
  '# Pre Settings'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[usersettings]'#010+
  '#####################################################################'#010+
  '# User Settings'#010+
  '##############','######################################################'+
  '#'#010+
  #010+
  #010+
  '[postsettings]'#010+
  '#####################################################################'#010+
  '# Post Settings'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[shelltoo','ls]'#010+
  '#####################################################################'#010+
  '# Shell tools'#010+
  '#####################################################################'#010+
  #010+
  '# echo'#010+
  'ifndef ECHO'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /gecho$(EXEEXT),$(SEARCHP','ATH)))'+
  ')'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=$(strip $(wildcard $(addsuffix /echo$(EXEEXT),$(SEARCHPATH))))'#010+
  'ifeq ($(ECHO),)'#010+
  'ECHO:=echo'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'endif'#010+
  'else'#010+
  'ECHO:=$(firstword $(ECHO))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To copy pograms'#010+
  'ifndef COPY'#010+
  'COPY:=c','p -fp'#010+
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
  '# Check deltree program'#010+
  'ifndef DELTREE'#010+
  'DELTREE:=rm -rf'#010+
  'endif'#010+
  #010+
  '# T','o install files'#010+
  'ifndef INSTALL'#010+
  'ifdef inlinux'#010+
  'INSTALL:=install -m 644'#010+
  'else'#010+
  'INSTALL:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To install programs'#010+
  'ifndef INSTALLEXE'#010+
  'ifdef inlinux'#010+
  'INSTALLEXE:=install -m 755'#010+
  'else'#010+
  'INSTALLEXE:=$(COPY)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# To make a direc','tory.'#010+
  'ifndef MKDIR'#010+
  'ifdef inlinux'#010+
  'MKDIR:=install -m 755 -d'#010+
  'else'#010+
  'MKDIR:=ginstall -m 755 -d'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'export ECHO COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKDIR'#010+
  #010+
  #010+
  '[tool_default]'#010+
  '#####################################################','###############'+
  '#'#010+
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
  '# ppas.bat / p','pas.sh'#010+
  'ifdef inlinux'#010+
  'PPAS=ppas.sh'#010+
  'else'#010+
  'ifdef inOS2'#010+
  'PPAS=ppas.cmd'#010+
  'else'#010+
  'PPAS=ppas.bat'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# also call ppas if with command option -s'#010+
  'ifeq (,$(findstring -s ,$(COMPILER)))'#010+
  'EXECPPAS='#010+
  'else'#010+
  'EXECPPAS:=@$(PPAS)'#010+
  'endif'#010+
  #010+
  '# ldconfig to rebuild',' .so cache'#010+
  'ifdef inlinux'#010+
  'LDCONFIG=ldconfig'#010+
  'else'#010+
  'LDCONFIG='#010+
  'endif'#010+
  #010+
  '[tool_ppdep]'#010+
  '# ppdep'#010+
  'ifndef PPDEP'#010+
  'PPDEP:=$(strip $(wildcard $(addsuffix /ppdep$(EXEEXT),$(SEARCHPATH))))'+
  #010+
  'ifeq ($(PPDEP),)'#010+
  'PPDEP='#010+
  'else'#010+
  'PPDEP:=$(firstword $(PPDEP))'#010+
  'endif'#010+
  'endif'#010,
  'export PPDEP'#010+
  #010+
  '[tool_ppumove]'#010+
  '# ppumove'#010+
  'ifndef PPUMOVE'#010+
  'PPUMOVE:=$(strip $(wildcard $(addsuffix /ppumove$(EXEEXT),$(SEARCHPATH'+
  '))))'#010+
  'ifeq ($(PPUMOVE),)'#010+
  'PPUMOVE='#010+
  'else'#010+
  'PPUMOVE:=$(firstword $(PPUMOVE))'#010+
  'endif'#010+
  'endif'#010+
  'export PPUMOVE'#010+
  #010+
  '[tool_ppufiles]'#010+
  '#',' ppufiles'#010+
  'ifndef PPUFILES'#010+
  'PPUFILES:=$(strip $(wildcard $(addsuffix /ppufiles$(EXEEXT),$(SEARCHPA'+
  'TH))))'#010+
  'ifeq ($(PPUFILES),)'#010+
  'PPUFILES='#010+
  'else'#010+
  'PPUFILES:=$(firstword $(PPUFILES))'#010+
  'endif'#010+
  'endif'#010+
  'export PPUFILES'#010+
  #010+
  '[tool_data2inc]'#010+
  '# data2inc'#010+
  'ifndef DATA','2INC'#010+
  'DATA2INC:=$(strip $(wildcard $(addsuffix /data2inc$(EXEEXT),$(SEARCHPA'+
  'TH))))'#010+
  'ifeq ($(DATA2INC),)'#010+
  'DATA2INC='#010+
  'else'#010+
  'DATA2INC:=$(firstword $(DATA2INC))'#010+
  'endif'#010+
  'endif'#010+
  'export DATA2INC'#010+
  #010+
  '[tool_diff]'#010+
  '# diff'#010+
  'ifndef DIFF'#010+
  'DIFF:=$(strip $(wildcard $(a','ddsuffix /diff$(EXEEXT),$(SEARCHPATH))))'+
  #010+
  'ifeq ($(DIFF),)'#010+
  'DIFF='#010+
  'else'#010+
  'DIFF:=$(firstword $(DIFF))'#010+
  'endif'#010+
  'endif'#010+
  'export DIFF'#010+
  #010+
  '[tool_cmp]'#010+
  '# cmp'#010+
  'ifndef CMP'#010+
  'CMP:=$(strip $(wildcard $(addsuffix /cmp$(EXEEXT),$(SEARCHPATH))))'#010+
  'ifeq ($(CMP),)'#010+
  'CMP='#010+
  'else'#010,
  'CMP:=$(firstword $(CMP))'#010+
  'endif'#010+
  'endif'#010+
  'export CMP'#010+
  #010+
  '[tool_sed]'#010+
  '# Sed'#010+
  'ifndef SED'#010+
  'SED:=$(strip $(wildcard $(addsuffix /sed$(EXEEXT),$(SEARCHPATH))))'#010+
  'ifeq ($(SED),)'#010+
  'SED='#010+
  'else'#010+
  'SED:=$(firstword $(SED))'#010+
  'endif'#010+
  'endif'#010+
  'export SED'#010+
  #010+
  '[tool_upx]'#010+
  '# Look if U','PX is found for go32v2 and win32. We can'#039't use $UPX b'+
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
  'UPXPROG:=$(strip $(wildcard $(a','ddsuffix /upx$(EXEEXT),$(SEARCHPATH))'+
  '))'#010+
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
  '[tool_date]'#010+
  '# gdate/date'#010+
  'ifndef DATE'#010+
  'DATE:=$(strip $(wildcard $(addsuffix /date$(EXEEX','T),$(SEARCHPATH))))'+
  #010+
  'ifeq ($(DATE),)'#010+
  'DATE:=$(strip $(wildcard $(addsuffix /gdate$(EXEEXT),$(SEACHPATH))))'#010+
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
  'DATESTR',':=$(shell $(DATE) +%Y%m%d)'#010+
  'else'#010+
  'DATESTR='#010+
  'endif'#010+
  #010+
  '[tool_zip]'#010+
  '# ZipProg, you can'#039't use Zip as the var name (PFV)'#010+
  'ifndef ZIPPROG'#010+
  'ZIPPROG:=$(strip $(wildcard $(addsuffix /zip$(EXEEXT),$(SEARCHPATH))))'+
  #010+
  'ifeq ($(ZIPPROG),)'#010+
  'ZIPPROG='#010+
  'else'#010+
  'ZIPPROG:=$(','firstword $(ZIPPROG)) -D9 -r'#010+
  'endif'#010+
  'endif'#010+
  'export ZIPPROG'#010+
  #010+
  'ifndef ZIPEXT'#010+
  'ZIPEXT=.zip'#010+
  'endif'#010+
  #010+
  #010+
  '[extensions]'#010+
  '#####################################################################'#010+
  '# Default extensions'#010+
  '#############################################','#######################'+
  '#'#010+
  #010+
  '# Default needed extensions (Go32v2,Linux)'#010+
  'LOADEREXT=.as'#010+
  'PPLEXT=.ppl'#010+
  'PPUEXT=.ppu'#010+
  'OEXT=.o'#010+
  'ASMEXT=.s'#010+
  'SMARTEXT=.sl'#010+
  'STATICLIBEXT=.a'#010+
  'SHAREDLIBEXT=.so'#010+
  'RSTEXT=.rst'#010+
  'PACKAGESUFFIX='#010+
  'FPCMADE=fpcmade'#010+
  #010+
  '# Go32v1'#010+
  'ifeq ($(OS_TARGE','T),go32v1)'#010+
  'PPUEXT=.pp1'#010+
  'OEXT=.o1'#010+
  'ASMEXT=.s1'#010+
  'SMARTEXT=.sl1'#010+
  'STATICLIBEXT=.a1'#010+
  'SHAREDLIBEXT=.so1'#010+
  'PACKAGESUFFIX=v1'#010+
  'FPCMADE=fpcmade.v1'#010+
  'endif'#010+
  #010+
  '# Go32v2'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'PACKAGESUFFIX=go32'#010+
  'FPCMADE=fpcmade.dos'#010+
  'endif'#010+
  #010+
  '# Linux'#010+
  'ifeq ($(OS_TARG','ET),linux)'#010+
  'PACKAGESUFFIX=linux'#010+
  'FPCMADE=fpcmade.lnx'#010+
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
  'PACKAGESUFFIX=w32'#010+
  'FPCMADE=fpcmade.w32'#010+
  'endif'#010+
  #010+
  '# OS/2'#010+
  'ifeq ($(OS_TARG','ET),os2)'#010+
  'PPUEXT=.ppo'#010+
  'ASMEXT=.so2'#010+
  'OEXT=.oo2'#010+
  'SMARTEXT=.so'#010+
  'STATICLIBEXT=.ao2'#010+
  'SHAREDLIBEXT=.dll'#010+
  'PACKAGESUFFIX=os2'#010+
  'FPCMADE=fpcmade.os2'#010+
  'endif'#010+
  #010+
  '# library prefix'#010+
  'LIBPREFIX=lib'#010+
  'ifeq ($(OS_TARGET),go32v2)'#010+
  'LIBPREFIX='#010+
  'endif'#010+
  'ifeq ($(OS_TARGET),go32v1)'#010+
  'L','IBPREFIX='#010+
  'endif'#010+
  #010+
  '# determine which .pas extension is used'#010+
  'ifndef PASEXT'#010+
  'ifdef EXEOBJECTS'#010+
  'override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstword $(EX'+
  'EOBJECTS)))))'#010+
  'else'#010+
  'override TESTPAS:=$(strip $(wildcard $(addsuffix .pas,$(firstw','ord $('+
  'UNITOBJECTS)))))'#010+
  'endif'#010+
  'ifeq ($(TESTPAS),)'#010+
  'PASEXT=.pp'#010+
  'else'#010+
  'PASEXT=.pas'#010+
  'endif'#010+
  'endif'#010+
  #010+
  #010+
  '[dir_default]'#010+
  '#####################################################################'#010+
  '# Default Directories'#010+
  '############################################','########################'+
  '#'#010+
  #010+
  '# set the prefix directory where to install everything'#010+
  'ifndef PREFIXINSTALLDIR'#010+
  'ifdef inlinux'#010+
  'PREFIXINSTALLDIR=/usr'#010+
  'else'#010+
  'PREFIXINSTALLDIR=/pp'#010+
  'endif'#010+
  'endif'#010+
  'export PREFIXINSTALLDIR'#010+
  #010+
  #010+
  '[dir_install]'#010+
  '####################','################################################'+
  '#'#010+
  '# Install Directories'#010+
  '#####################################################################'#010+
  #010+
  '# set the base directory where to install everything'#010+
  'ifndef BASEINSTALLDIR'#010+
  'ifdef inlinux'#010+
  'BASEINST','ALLDIR=$(PREFIXINSTALLDIR)/lib/fpc/$(FPC_VERSION)'#010+
  'else'#010+
  'BASEINSTALLDIR=$(PREFIXINSTALLDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the binaries'#010+
  'ifndef BININSTALLDIR'#010+
  'ifdef inlinux'#010+
  'BININSTALLDIR=$(PREFIXINSTALLDIR)/bin'#010+
  'else'#010+
  'BININSTAL','LDIR=$(BASEINSTALLDIR)/bin/$(OS_TARGET)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# set the directory where to install the units.'#010+
  'ifndef UNITINSTALLDIR'#010+
  'UNITINSTALLDIR=$(BASEINSTALLDIR)/units/$(OS_TARGET)'#010+
  'ifdef UNITSUBDIR'#010+
  'UNITINSTALLDIR:=$(UNITINSTALLDIR)/$(UNITSUBDIR)'#010,
  'endif'#010+
  'endif'#010+
  #010+
  '# Where to install shared libraries'#010+
  'ifndef LIBINSTALLDIR'#010+
  'ifdef inlinux'#010+
  'LIBINSTALLDIR=$(PREFIXINSTALLDIR)/lib'#010+
  'else'#010+
  'LIBINSTALLDIR=$(UNITINSTALLDIR)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the source files will be stored'#010+
  'ifndef SOURCEINSTALLDIR'#010+
  'ifde','f inlinux'#010+
  'SOURCEINSTALLDIR=$(PREFIXINSTALLDIR)/src/fpc-$(FPC_VERSION)'#010+
  'else'#010+
  'SOURCEINSTALLDIR=$(BASEINSTALLDIR)/source'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Where the doc files will be stored'#010+
  'ifndef DOCINSTALLDIR'#010+
  'ifdef inlinux'#010+
  'DOCINSTALLDIR=$(PREFIXINSTALLDIR)/doc/','fpc-$(FPC_VERSION)'#010+
  'else'#010+
  'DOCINSTALLDIR=$(BASEINSTALLDIR)/doc'#010+
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
  '# On linux, try to find where libgcc.a is','.'#010+
  'ifdef inlinux'#010+
  'ifndef GCCLIBDIR'#010+
  'GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '#039'{ print'+
  ' $$4 } '#039'`)'#010+
  'endif'#010+
  'endif'#010+
  'export GCCLIBDIR'#010+
  #010+
  #010+
  '[dir_otherlib]'#010+
  '# Where to find other libraries'#010+
  'ifdef inlinux'#010+
  'ifndef OTHERLIBDIR'#010+
  'OTHERLIBDIR:=$(shel','l grep -v "^\#" /etc/ld.so.conf | awk '#039'{ ORS='+
  '" "; print $1 }'#039')'#010+
  'endif'#010+
  'endif'#010+
  'export OTHERLIBDIR'#010+
  #010+
  #010+
  '[redir]'#010+
  '#####################################################################'#010+
  '# Redirection'#010+
  '####################################################','################'+
  '#'#010+
  #010+
  '# Release ? Then force OPT and don'#039't use extra opts via commandlin'+
  'e'#010+
  'ifndef REDIRFILE'#010+
  'REDIRFILE=log'#010+
  'endif'#010+
  #010+
  'ifdef REDIR'#010+
  'ifndef inlinux'#010+
  'override FPC=redir -eo $(FPC)'#010+
  'endif'#010+
  '# set the verbosity to max'#010+
  'override FPCOPT+=-va'#010+
  'over','ride REDIR:= >> $(REDIRFILE)'#010+
  'endif'#010+
  #010+
  '[command_begin]'#010+
  '#####################################################################'#010+
  '# Compiler Command Line'#010+
  '#####################################################################'#010+
  #010+
  '# Load commandline OPTD','EF and add FPC_CPU define'#010+
  'override FPCOPTDEF:=-d$(CPU_TARGET)'#010+
  #010+
  '# Load commandline OPT and add target and unit dir to be sure'#010+
  'ifneq ($(OS_TARGET),$(OS_SOURCE))'#010+
  'override FPCOPT+=-T$(OS_TARGET)'#010+
  'endif'#010+
  #010+
  '[command_needopt]'#010+
  'ifdef NEEDOPT'#010+
  'override F','PCOPT+=$(NEEDOPT)'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needunit]'#010+
  'ifdef NEEDUNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(NEEDUNITDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[command_unitsdir]'#010+
  'ifdef UNITSDIR'#010+
  'override FPCOPT+=-Fu$(UNITSDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needlib]'#010+
  'ifdef NEEDLIBDIR'#010+
  'override FP','COPT+=$(addprefix -Fl,$(NEEDLIBDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needobj]'#010+
  'ifdef NEEDOBJDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(NEEDOBJDIR))'#010+
  'endif'#010+
  #010+
  #010+
  '[command_needinc]'#010+
  'ifdef NEEDINCDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(NEEDINCDIR))'#010+
  'endif'#010+
  #010+
  '[command_gccli','b]'#010+
  '# Add GCC lib path if asked'#010+
  'ifdef GCCLIBDIR'#010+
  'override FPCOPT+=-Fl$(GCCLIBDIR)'#010+
  'endif'#010+
  #010+
  '[command_otherlib]'#010+
  '# Add Other dirs path if asked'#010+
  'ifdef OTHERLIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))'#010+
  'endif'#010+
  #010+
  '[command_target]'#010+
  '# Target dir','s'#010+
  'ifdef TARGETDIR'#010+
  'override FPCOPT+=-FE$(TARGETDIR)'#010+
  'endif'#010+
  #010+
  '[command_unittarget]'#010+
  'ifdef UNITTARGETDIR'#010+
  'override FPCOPT+=-FU$(UNITTARGETDIR)'#010+
  'endif'#010+
  #010+
  '[command_end]'#010+
  '# Smartlinking'#010+
  'ifdef SMARTLINK'#010+
  'override FPCOPT+=-CX'#010+
  'endif'#010+
  #010+
  '# Debug'#010+
  'ifdef DEBUG'#010+
  'over','ride FPCOPT+=-g -dDEBUG'#010+
  'endif'#010+
  #010+
  '# Release mode (strip, optimize and don'#039't load ppc386.cfg)'#010+
  'ifdef RELEASE'#010+
  'override FPCOPT+=-Xs -OG2p3 -n'#010+
  'endif'#010+
  #010+
  '# Verbose settings (warning,note,info)'#010+
  'ifdef VERBOSE'#010+
  'override FPCOPT+=-vwni'#010+
  'endif'#010+
  #010+
  '# Add commandli','ne options'#010+
  'ifdef OPT'#010+
  'override FPCOPT+=$(OPT)'#010+
  'endif'#010+
  'ifdef UNITDIR'#010+
  'override FPCOPT+=$(addprefix -Fu,$(UNITDIR))'#010+
  'endif'#010+
  'ifdef LIBDIR'#010+
  'override FPCOPT+=$(addprefix -Fl,$(LIBDIR))'#010+
  'endif'#010+
  'ifdef OBJDIR'#010+
  'override FPCOPT+=$(addprefix -Fo,$(OBJDIR))'#010+
  'endi','f'#010+
  'ifdef INCDIR'#010+
  'override FPCOPT+=$(addprefix -Fi,$(INCDIR))'#010+
  'endif'#010+
  #010+
  '# Add defines from FPCOPTDEF to FPCOPT'#010+
  'ifdef FPCOPTDEF'#010+
  'override FPCOPT+=$(FPCOPTDEF)'#010+
  'endif'#010+
  #010+
  '# Error file ?'#010+
  'ifdef ERRORFILE'#010+
  'override FPCOPT+=-Fr$(ERRORFILE)'#010+
  'endif'#010+
  #010+
  '# Was a con','fig file specified ?'#010+
  'ifdef CFGFILE'#010+
  'override FPCOPT+=@$(CFGFILE)'#010+
  'endif'#010+
  #010+
  '# For win32 the options are passed using the file fpcext.cmd'#010+
  'ifndef FPCEXTCMD'#010+
  'FPCEXTCMD=fpcext.cmd'#010+
  'endif'#010+
  'ifeq ($(OS_SOURCE),win32)'#010+
  'override TMPVAR:=$(shell "$(DEL) $(FPC','EXTCMD)")'#010+
  'override TMPVAR:=$(foreach A,$(FPCOPT),$(shell "$(ECHO) $(A) >> $(FPCE'+
  'XTCMD)"))'#010+
  'override FPCOPT:=@$(FPCEXTCMD)'#010+
  'endif'#010+
  #010+
  '# Compiler commandline'#010+
  'override COMPILER:=$(FPC) $(FPCOPT)'#010+
  #010+
  #010+
  '[standardrules]'#010+
  '###################################','#################################'+
  '#'#010+
  '# Standard rules'#010+
  '#####################################################################'#010+
  #010+
  '[packagedependrules]'#010+
  '#####################################################################'#010+
  '# Package depends'#010+
  '########','############################################################'+
  '#'#010+
  #010+
  '[loaderrules]'#010+
  '#####################################################################'#010+
  '# Loaders'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_','loaders'#010+
  #010+
  '%$(OEXT): %$(LOADEREXT)'#010+
  '        $(AS) -o $*$(OEXT) $<'#010+
  #010+
  'override ALLTARGET+=fpc_loaders'#010+
  'override CLEANTARGET+=fpc_loaders_clean'#010+
  'override SHOWINSTALLTARGET+=fpc_loaders_showinstall'#010+
  'override INSTALLTARGET+=fpc_loaders_install'#010+
  #010+
  'overrid','e LOADEROFILES:=$(addsuffix $(OEXT),$(LOADEROBJECTS))'#010+
  #010+
  'fpc_loaders: $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_clean:'#010+
  '        -$(DEL) $(LOADEROFILES)'#010+
  #010+
  'fpc_loaders_showinstall:'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNITINSTALLDIR)/,$(LOADEROFILES)'+
  ')'#010+
  #010+
  'fpc_l','oaders_install:'#010+
  '        $(MKDIR) $(UNITINSTALLDIR)'#010+
  '        $(INSTALL) $(LOADEROFILES) $(UNITINSTALLDIR)'#010+
  #010+
  #010+
  '[unitrules]'#010+
  '#####################################################################'#010+
  '# Units'#010+
  '############################################','########################'+
  '#'#010+
  #010+
  '.PHONY: fpc_units'#010+
  #010+
  'override ALLTARGET+=fpc_units'#010+
  #010+
  'override UNITPPUFILES=$(addsuffix $(PPUEXT),$(UNITOBJECTS))'#010+
  'override INSTALLPPUFILES+=$(UNITPPUFILES)'#010+
  'override CLEANPPUFILES+=$(UNITPPUFILES)'#010+
  #010+
  'fpc_units: $(UNITPPU','FILES)'#010+
  #010+
  #010+
  #010+
  '[exerules]'#010+
  '#####################################################################'#010+
  '# Exes'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_exes'#010+
  #010+
  'override EXEFILES=$(addsuffix $(EXEEXT),$(EXEOBJECTS)',')'#010+
  'override EXEOFILES=$(addsuffix $(OEXT),$(EXEOBJECTS))'#010+
  #010+
  'override ALLTARGET+=fpc_exes'#010+
  'override INSTALLEXEFILES+=$(EXEFILES)'#010+
  'override CLEANEXEFILES+=$(EXEFILES) $(EXEOFILES)'#010+
  #010+
  'fpc_exes: $(EXEFILES)'#010+
  #010+
  #010+
  '[rstrules]'#010+
  '###############################','#####################################'+
  '#'#010+
  '# Resource strings'#010+
  '#####################################################################'#010+
  #010+
  'override RSTFILES=$(addsuffix $(RSTEXT),$(RSTOBJECTS))'#010+
  #010+
  'override CLEANRSTFILES+=$(RSTFILES)'#010+
  #010+
  #010+
  '[examplerules]'#010+
  '##','##################################################################'+
  '#'#010+
  '# Examples'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_examples fpc_test'#010+
  #010+
  'override EXAMPLEFILES:=$(addsuffix $(EXEEXT),$(EXAMPLEOBJEC','TS))'#010+
  'override EXAMPLEOFILES:=$(addsuffix $(OEXT),$(EXAMPLEOBJECTS))'#010+
  #010+
  'override CLEANEXEFILES+=$(EXAMPLEFILES) $(EXAMPLEOFILES)'#010+
  #010+
  'fpc_examples: all $(EXAMPLEFILES)'#010+
  #010+
  'fpc_test: examples'#010+
  #010+
  #010+
  #010+
  '[compilerules]'#010+
  '#########################################','###########################'+
  '#'#010+
  '# General compile rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_packages fpc_all fpc_debug'#010+
  #010+
  '$(FPCMADE): $(ALLTARGET)'#010+
  '        @$(ECHO) Compiled > $(FPCMADE)'#010+
  #010+
  'fpc_package','s: $(COMPILEPACKAGES)'#010+
  #010+
  'fpc_all: fpc_packages $(FPCMADE)'#010+
  #010+
  'fpc_debug:'#010+
  '        $(MAKE) all DEBUG=1'#010+
  #010+
  '# General compile rules, available for both possible PASEXT'#010+
  #010+
  '.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp'#010+
  #010+
  '%$(PPUEXT): %.pp'#010+
  '        $(COMPIL','ER) $< $(REDIR)'#010+
  '        $(EXECPASS)'#010+
  #010+
  '%$(PPUEXT): %.pas'#010+
  '        $(COMPILER) $< $(REDIR)'#010+
  '        $(EXECPASS)'#010+
  #010+
  '%$(EXEEXT): %.pp'#010+
  '        $(COMPILER) $< $(REDIR)'#010+
  '        $(EXECPASS)'#010+
  #010+
  '%$(EXEEXT): %.pas'#010+
  '        $(COMPILER) $< $(REDIR)'#010+
  '        $(EX','ECPASS)'#010+
  #010+
  #010+
  '[libraryrules]'#010+
  '#####################################################################'#010+
  '# Library'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_smart fpc_shared'#010+
  #010+
  '# Default sharedlib units are all ','unit objects'#010+
  'ifndef SHAREDLIBUNITOBJECTS'#010+
  'SHAREDLIBUNITOBJECTS:=$(UNITOBJECTS)'#010+
  'endif'#010+
  #010+
  'fpc_smart:'#010+
  '        $(MAKE) all SMARTLINK=1'#010+
  #010+
  'fpc_shared: all'#010+
  'ifdef inlinux'#010+
  'ifndef LIBNAME'#010+
  '        @$(ECHO) "LIBNAME not set"'#010+
  'else'#010+
  '        $(PPUMOVE) $(SHARE','DLIBUNITOBJECTS) -o$(LIBNAME)'#010+
  'endif'#010+
  'else'#010+
  '        @$(ECHO) "Shared Libraries not supported"'#010+
  'endif'#010+
  #010+
  #010+
  '[installrules]'#010+
  '#####################################################################'#010+
  '# Install rules'#010+
  '########################################','############################'+
  '#'#010+
  #010+
  '.PHONY: fpc_showinstall fpc_install'#010+
  #010+
  'ifdef EXTRAINSTALLUNITS'#010+
  'override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(EXTRAINSTALLUNITS))'#010+
  'endif'#010+
  #010+
  'ifdef INSTALLPPUFILES'#010+
  'ifdef PPUFILES'#010+
  'ifdef inlinux'#010+
  'INSTALLPPULINKFILES:','=$(shell $(PPUFILES) -S -O $(INSTALLPPUFILES))'#010+
  'INSTALLPPULIBFILES:=$(shell $(PPUFILES) -L $(INSTALLPPUFILES))'#010+
  'else'#010+
  'INSTALLPPULINKFILES:=$(shell $(PPUFILES) $(INSTALLPPUFILES))'#010+
  'endif'#010+
  'else'#010+
  'INSTALLPPULINKFILES:=$(wildcard $(subst $(PPUEXT),$(O','EXT),$(INSTALLP'+
  'PUFILES)))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'fpc_showinstall: $(SHOWINSTALLTARGET)'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(BININSTALLDIR)/,$(INSTALLEXEFILE'+
  'S))'#010+
  'endif'#010+
  'ifdef INSTALLPPUFILES'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(U','NITINSTALLDIR)/,$(INSTALLPPUF'+
  'ILES))'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(UNITINSTALLDIR)/,$(INSTALLPPULIN'+
  'KFILES))'#010+
  'endif'#010+
  'ifneq ($(INSTALLPPULIBFILES),)'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(LIBINSTALLDIR)/,$(','INSTALLPPULI'+
  'BFILES))'#010+
  'endif'#010+
  'endif'#010+
  'ifdef EXTRAINSTALLFILES'#010+
  '        @$(ECHO) -e $(addprefix "\n"$(DATAINSTALLDIR)/,$(EXTRAINSTALLF'+
  'ILES))'#010+
  'endif'#010+
  #010+
  'fpc_install: $(INSTALLTARGET)'#010+
  '# Create UnitInstallFiles'#010+
  'ifdef INSTALLEXEFILES'#010+
  '        $(MKDIR) $(BI','NINSTALLDIR)'#010+
  '# Compress the exes if upx is defined'#010+
  'ifdef UPXPROG'#010+
  '        -$(UPXPROG) $(INSTALLEXEFILES)'#010+
  'endif'#010+
  '        $(INSTALLEXE) $(INSTALLEXEFILES) $(BININSTALLDIR)'#010+
  'endif'#010+
  'ifdef INSTALLPPUFILES'#010+
  '        $(MKDIR) $(UNITINSTALLDIR)'#010+
  '        $','(INSTALL) $(INSTALLPPUFILES) $(UNITINSTALLDIR)'#010+
  'ifneq ($(INSTALLPPULINKFILES),)'#010+
  '        $(INSTALL) $(INSTALLPPULINKFILES) $(UNITINSTALLDIR)'#010+
  'endif'#010+
  'ifneq ($(INSTALLPPULIBFILES),)'#010+
  '        $(MKDIR) $(LIBINSTALLDIR)'#010+
  '        $(INSTALL) $(INSTALLPP','ULIBFILES) $(LIBINSTALLDIR)'#010+
  'endif'#010+
  'endif'#010+
  'ifdef EXTRAINSTALLFILES'#010+
  '        $(MKDIR) $(DATAINSTALLDIR)'#010+
  '        $(INSTALL) $(EXTRAINSTALLFILES) $(DATAINSTALLDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[sourceinstallrules]'#010+
  '######################################################','##############'+
  '#'#010+
  '# Source install rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_sourceinstall'#010+
  #010+
  'fpc_sourceinstall: clean'#010+
  '        $(MKDIR) $(SOURCEINSTALLDIR)'#010+
  '        $(COPYTREE) $(BASEDIR) $(SOURCEIN','STALLDIR)'#010+
  #010+
  '[zipinstallrules]'#010+
  '#####################################################################'#010+
  '# Zip'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_zipinstall fpc_zipinstalladd'#010+
  #010+
  '# Temporary path to pa','ck a file'#010+
  'ifndef PACKDIR'#010+
  'ifndef inlinux'#010+
  'PACKDIR=$(BASEDIR)/pack_tmp'#010+
  'else'#010+
  'PACKDIR=/tmp/fpc-pack'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Test dir if none specified'#010+
  'ifndef DESTZIPDIR'#010+
  'DESTZIPDIR:=$(BASEDIR)'#010+
  'endif'#010+
  #010+
  '# Maybe create default zipname from packagename'#010+
  'ifndef ','ZIPNAME'#010+
  'ifdef PACKAGENAME'#010+
  'ZIPNAME=$(PACKAGENAME)$(PACKAGESUFFIX)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Add .zip/.tar.gz extension'#010+
  'ifdef ZIPNAME'#010+
  'ifndef inlinux'#010+
  'override ZIPNAME:=$(ZIPNAME)$(ZIPEXT)'#010+
  'endif'#010+
  'endif'#010+
  #010+
  '# Note: This will not remove the zipfile first'#010+
  'fpc_zi','pinstalladd:'#010+
  'ifndef ZIPNAME'#010+
  '        @$(ECHO) "Please specify ZIPNAME!"'#010+
  '        @exit 1'#010+
  'else'#010+
  '        $(MAKE) $(ZIPTARGET) PREFIXINSTALLDIR=$(PACKDIR)'#010+
  'ifdef inlinux'#010+
  '        gzip -d $(DESTZIPDIR)/$(ZIPNAME).tar.gz'#010+
  '        cd $(PACKDIR) ; tar r','v --file $(DESTZIPDIR)/$(ZIPNAME).tar *'+
  ' ; cd $(BASEDIR)'#010+
  '        gzip $(DESTZIPDIR)/$(ZIPNAME).tar'#010+
  'else'#010+
  '        cd $(PACKDIR) ; $(ZIPPROG) $(DESTZIPDIR)/$(ZIPNAME) * ; cd $(B'+
  'ASEDIR)'#010+
  'endif'#010+
  '        $(DELTREE) $(PACKDIR)'#010+
  'endif'#010+
  #010+
  '# First remove t','he zip and then install'#010+
  'fpc_zipinstall:'#010+
  'ifndef ZIPNAME'#010+
  '        @$(ECHO) "Please specify ZIPNAME!"'#010+
  '        @exit 1'#010+
  'else'#010+
  '        $(DEL) $(DESTZIPDIR)/$(ZIPNAME)'#010+
  '        $(MAKE) $(ZIPTARGET) PREFIXINSTALLDIR=$(PACKDIR)'#010+
  'ifdef inlinux'#010+
  '        cd',' $(PACKDIR) ; tar cvz --file $(DESTZIPDIR)/$(ZIPNAME).tar.'+
  'gz * ; cd $(BASEDIR)'#010+
  'else'#010+
  '        cd $(PACKDIR) ; $(ZIPPROG) $(DESTZIPDIR)/$(ZIPNAME) * ; cd $(B'+
  'ASEDIR)'#010+
  'endif'#010+
  '        $(DELTREE) $(PACKDIR)'#010+
  'endif'#010+
  #010+
  #010+
  '[cleanrules]'#010+
  '#####################','###############################################'+
  '#'#010+
  '# Clean rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_clean fpc_cleanall'#010+
  #010+
  'ifdef EXTRACLEANUNITS'#010+
  'override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(EXTR','ACLEANUNITS))'#010+
  'endif'#010+
  #010+
  'ifdef CLEANPPUFILES'#010+
  'ifdef PPUFILES'#010+
  'CLEANPPULINKFILES:=$(shell $(PPUFILES) $(CLEANPPUFILES))'#010+
  'else'#010+
  'CLEANPPULINKFILES:=$(wildcard $(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILE'+
  'S)))'#010+
  'endif'#010+
  'endif'#010+
  #010+
  'fpc_clean: $(CLEANTARGET)'#010+
  'ifdef CL','EANEXEFILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  'ifdef CLEANPPUFILES'#010+
  '        -$(DEL) $(CLEANPPUFILES)'#010+
  'endif'#010+
  'ifneq ($(CLEANPPULINKFILES),)'#010+
  '        -$(DEL) $(CLEANPPULINKFILES)'#010+
  'endif'#010+
  'ifdef CLEANRSTFILES'#010+
  '        -$(DEL) $(CLEANRSTFILES)'#010+
  'endi','f'#010+
  'ifdef EXTRACLEANFILES'#010+
  '        -$(DEL) $(EXTRACLEANFILES)'#010+
  'endif'#010+
  '        -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTCMD) $(REDIRFILE)'#010+
  #010+
  'fpc_cleanall: $(CLEANTARGET)'#010+
  'ifdef CLEANEXEFILES'#010+
  '        -$(DEL) $(CLEANEXEFILES)'#010+
  'endif'#010+
  '        -$(DEL) ','*$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICL'+
  'IBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)'#010+
  '        -$(DELTREE) *$(SMARTEXT)'#010+
  '        -$(DEL) $(FPCMADE) $(PPAS) link.res $(FPCEXTCMD) $(REDIRFILE)'#010+
  #010+
  #010+
  '[dependrules]'#010+
  '##################################','##################################'+
  '#'#010+
  '# Depend rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_depend'#010+
  #010+
  'fpc_depend:'#010+
  '        $(PPDEP) $(UNITOBJECTS)'#010+
  #010+
  #010+
  '[inforules]'#010+
  '########################################','############################'+
  '#'#010+
  '# Info rules'#010+
  '#####################################################################'#010+
  #010+
  '.PHONY: fpc_info fpc_cfginfo fpc_objectinfo fpc_toolsinfo fpc_installi'+
  'nfo \'#010+
  '        fpc_dirinfo'#010+
  #010+
  'fpc_info: $(INFOTARGET)'#010+
  #010+
  '[inf','o_cfg]'#010+
  'fpc_infocfg:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Configuration info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  FPC....... $(FPC)'#010+
  '        @$(ECHO)  Version... $(FPC_VERSION)'#010+
  '        @$(ECHO)  CPU....... $(CPU_TARGET)'#010+
  '        @$(ECHO)  So','urce.... $(OS_SOURCE)'#010+
  '        @$(ECHO)  Target.... $(OS_TARGET)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_dirs]'#010+
  'fpc_infodirs:'#010+
  'ifdef inlinux'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Directory info =='#010+
  '        @$(ECHO)'#010+
  'ifdef NEEDGCCLIB'#010+
  '        @$(ECHO)  GCC librar','y is needed.'#010+
  'endif'#010+
  'ifdef NEEDOTHERLIB'#010+
  '        @$(ECHO)  Other library is needed.'#010+
  'endif'#010+
  '        @$(ECHO)  Basedir......... $(BASEDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  GCC library..... $(GCCLIBDIR)'#010+
  '        @$(ECHO)  Other library... $(OTHER','LIBDIR)'#010+
  '        @$(ECHO)'#010+
  'endif'#010+
  #010+
  '[info_tools]'#010+
  'fpc_infotools:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Tools info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  Pwd....... $(PWD)'#010+
  '        @$(ECHO)  Echo...... $(ECHO)'#010+
  'ifdef PPDEP'#010+
  '        @$(ECHO)  PPDep...','.. $(PPDEP)'#010+
  'endif'#010+
  'ifdef PPUMOVE'#010+
  '        @$(ECHO)  PPUMove... $(PPUMOVE)'#010+
  'endif'#010+
  'ifdef PPUFILES'#010+
  '        @$(ECHO)  PPUFiles.. $(PPUFILES)'#010+
  'endif'#010+
  'ifdef DATA2INC'#010+
  '        @$(ECHO)  Data2Inc.. $(DATA2INC)'#010+
  'endif'#010+
  'ifdef SED'#010+
  '        @$(ECHO)  Sed.......',' $(SED)'#010+
  'endif'#010+
  'ifdef DATE'#010+
  '        @$(ECHO)  Date...... $(DATE)'#010+
  'endif'#010+
  'ifdef DIFF'#010+
  '        @$(ECHO)  Diff...... $(DIFF)'#010+
  'endif'#010+
  'ifdef CMP'#010+
  '        @$(ECHO)  Cmp....... $(CMP)'#010+
  'endif'#010+
  'ifdef UPXPROG'#010+
  '        @$(ECHO)  Upx....... $(UPXPROG)'#010+
  'endif'#010+
  'ifdef ','ZIPPROG'#010+
  '        @$(ECHO)  Zip....... $(ZIPPROG)'#010+
  'endif'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_objects]'#010+
  'fpc_infoobjects:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Object info =='#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  LoaderObjects..... $(LOADEROBJECTS)'#010+
  '        @$(E','CHO)  UnitObjects....... $(UNITOBJECTS)'#010+
  '        @$(ECHO)  ExeObjects........ $(EXEOBJECTS)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  ExtraCleanUnits... $(EXTRACLEANUNITS)'#010+
  '        @$(ECHO)  ExtraCleanFiles... $(EXTRACLEANFILES)'#010+
  '        @$(ECHO)'#010+
  '   ','     @$(ECHO)  ExtraInstallUnits. $(EXTRAINSTALLUNITS)'#010+
  '        @$(ECHO)  ExtraInstallFiles. $(EXTRAINSTALLFILES)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[info_install]'#010+
  'fpc_infoinstall:'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  == Install info =='#010+
  '        @$(ECHO)'#010+
  'ifdef ','DATE'#010+
  '        @$(ECHO)  DateStr.............. $(DATESTR)'#010+
  'endif'#010+
  'ifdef PACKAGENAME'#010+
  '        @$(ECHO)  PackageName.......... $(PACKAGENAME)'#010+
  'endif'#010+
  '        @$(ECHO)  PackageSuffix........ $(PACKAGESUFFIX)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  BaseIns','tallDir....... $(BASEINSTALLDIR)'#010+
  '        @$(ECHO)  BinInstallDir........ $(BININSTALLDIR)'#010+
  '        @$(ECHO)  LibInstallDir........ $(LIBINSTALLDIR)'#010+
  '        @$(ECHO)  UnitInstallDir....... $(UNITINSTALLDIR)'#010+
  '        @$(ECHO)  SourceInstallDir.','.... $(SOURCEINSTALLDIR)'#010+
  '        @$(ECHO)  DocInstallDir........ $(DOCINSTALLDIR)'#010+
  '        @$(ECHO)  DataInstallDir....... $(DATAINSTALLDIR)'#010+
  '        @$(ECHO)'#010+
  '        @$(ECHO)  DestZipDir........... $(DESTZIPDIR)'#010+
  '        @$(ECHO)  ZipName....','.......... $(ZIPNAME)'#010+
  '        @$(ECHO)'#010+
  #010+
  '[localmakefile]'#010+
  '#####################################################################'#010+
  '# Local Makefile'#010+
  '#####################################################################'#010+
  #010+
  'ifneq ($(wildcard fpcmake.','loc),)'#010+
  'include fpcmake.loc'#010+
  'endif'#010+
  #010+
  '[userrules]'#010+
  '#####################################################################'#010+
  '# Users rules'#010+
  '#####################################################################'#010+
  #010+
  #010+
  '[end]'#010
);
