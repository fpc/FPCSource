# makes the SYSTEM-Unit for Linux
#
# Copyright (c) 1996 by Michael Van Canneyt

#####################################################################
# Start of configurable section.
# Please note that all these must be set in the main makefile, and
# should be set there.
# Don't remove the indef statements. They serve to avoid conflicts
# with the main makefile.
#####################################################################

# AOUT should be defined in main makefile.
# But you can set it here too.
# AOUT = -DAOUT

# set the directory where to install the units.
ifndef UNITINSTALLDIR
UNITINSTALLDIR=/usr/lib/ppc/0.99.0
endif

# set the directory where to install the libs (must exist)
ifndef LIBINSTALLDIR
LIBINSTALLDIR=/usr/lib
endif

# Do you want to link to the C library ?
# Standard it is NO. You can set it to YES to link in th C library.
ifndef LINK_TO_C
LINK_TO_C=NO
endif

# What is the Operating System ?
ifndef OS_SRC
OS_SRC=linux
endif

# What is the target operating system ?
ifndef OS_TARGET
OS_TARGET=linux
endif

# What is the target processor :
ifndef CPU
CPU=i386
#CPU=m68k
endif

# What compiler to use ?
ifndef PP
PP=ppc386
endif

# What options to pass to the compiler ?
# You may want to specify a config file or error definitions file here.
ifndef OPT
OPT=
endif

# Where is the PPUMOVE program ?
ifndef PPUMOVE
PPUMOVE=ppumove
endif

# Set this to 'shared' or 'static'
LIBTYPE=shared

#####################################################################
# End of configurable section.
# Do not edit after this line.
#####################################################################

# Where are the include files ?
INC=../inc
PROCINC=../$(CPU)

# add required options...
override OPT:= $(OPT) -d$(CPU)

ifeq ($(OS_TARGET),$(OS_SRC))
CROSSCOMPILE=NO
else
CROSSCOMPILE=YES
endif

# To copy pograms
ifndef COPY
ifeq ($(DOS),YES)
COPY=copy
else
COPY=cp -p
endif
endif

# Check delete program
ifndef DEL
ifeq ($(DOS),YES)
DEL=del
else
DEL=rm -f
endif
endif

# To install programs
ifndef INSTALL
ifeq ($(DOS),YES)
INSTALL=copy
else
INSTALL=install
endif
endif

# To make a directory.
ifndef MKDIR
ifeq ($(DOS),YES)
MKDIR=mkdir
else
MKDIR=install -m 755 -d
endif
endif

# diff program
ifndef REFPATH
REFPATH=/usr/local/fpk/work/new/rtl
endif
ifndef DIFF
DIFF=diff
endif
ifndef DIFFOPTS
DIFFOPTS=-b -c
endif

# Was a config file specified ?
ifdef CFGFILE
override OPT:=$(OPT) @$(CONFIG)
endif

# Check for crosscompile
ifeq ($(CROSSCOMPILE),YES)
override OPT:= $(OPT) -dCROSSCOMPILE -TLINUX
endif

# Check if we need C library.
ifeq ($(LINK_TO_C),YES)
override OPT:=$(OPT) -dCRTLIB
endif

# check whether we make shared or static libs
ifndef LIBTYPE
LIBTYPE=shared
endif

# determine libary extension.
ifeq ($(LIBTYPE),static)
LIBEXT=.a
else
LIBEXT=.so
endif

# determine needed extensions.
PPUEXT=.ppu
PPLEXT=.ppl
OEXT=.o

OBJECTS=syslinux strings dos objects linux crt printer getopts errors sockets 
        
PPUOBJECTS=$(addsuffix $(PPUEXT), $(OBJECTS))

ifeq ($(LINK_TO_C),NO)
PPUOBJECTS:= prt0$(OEXT) gprt0$(OEXT) $(PPUOBJECTS)
endif

# os independent depends
SYSTEMDEPS=system.inc systemh.inc mathh.inc real2str.inc \
	heaph.inc innr.inc sstrings.inc file.inc \
	text.inc typefile.inc version.inc filerec.inc \
	textrec.inc objpas.inc objpash.inc \

DSYSTEMDEPS=$(addprefix $(INC)/, $(SYSTEMDEPS))

ifeq ($(LINK_TO_C),YES)
SYSLINUXDEPS=lprt$(OEXT)
else
SYSLINUXDEPS=sysconst.inc systypes.inc syscalls.inc
endif

SYSPROCDEPS=math.inc set.inc heap.inc $(CPU).inc

DSYSPROCDEPS=$(addprefix $(PROCINC)/, $(SYSPROCDEPS))


# Determine on what dos unit should depend.
DOSDEPS = dos.pp $(INC)/filerec.inc syslinux$(PPUEXT) strings$(PPUEXT)
ifeq ($(LINK_TO_C),YES)
DOSDEPS:=$(DOSDEPS) lprt$(OEXT)
else
DOSDEPS:=$(DOSDEPS) linux$(PPUEXT)
endif

# Which prt to use ?
ifdef AOUT
PRT=prt0.as
else
PRT=prt1.as
endif


.PHONY : all clean diffs install diffclean libs libsclean realclean

all : $(PPUOBJECTS)

syslinux$(PPUEXT) : syslinux.pp $(SYSLINUXDEPS) $(DSYSTEMDEPS) $(DSYSPROCDEPS)
	$(COPY) $(DSYSTEMDEPS) $(DSYSPROCDEPS) .
	$(PP) $(OPT) -Us -Sg syslinux $(REDIR)
	$(DEL) $(SYSTEMDEPS) $(SYSPROCDEPS)

dos$(PPUEXT) : $(DOSDEPS)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) dos $(REDIR)
	$(DEL) filerec.inc textrec.inc

crt$(PPUEXT) : crt.pp $(INC)/textrec.inc $(INC)/filerec.inc linux$(PPUEXT)\
	       syslinux$(PPUEXT)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) crt $(REDIR)
	$(DEL) filerec.inc textrec.inc

lprt$(OEXT) : lprt.c
	gcc $(AOUT) -c lprt.c -o lprt$(OEXT)

getopts$(PPUEXT) : $(PROCINC)/getopts.pp syslinux$(PPUEXT)
	$(COPY) $(PROCINC)/getopts.pp .
	$(PP) $(OPT) getopts $(REDIR)
	$(DEL) getopts.pp

strings$(PPUEXT) : $(PROCINC)/strings.pp syslinux$(PPUEXT)
	$(COPY) $(PROCINC)/strings.pp .
	$(PP) $(OPT) strings $(REDIR)
	$(DEL) strings.pp

objects$(PPUEXT) : objects.pp syslinux$(PPUEXT)
	$(PP) $(OPT) objects $(REDIR)

printer$(PPUEXT) : printer.pp $(INC)/textrec.inc linux$(PPUEXT) syslinux$(PPUEXT)
	$(COPY) $(INC)/textrec.inc .
	$(PP) $(OPT) printer $(REDIR)
	$(DEL) textrec.inc

linux$(PPUEXT) : linux.pp strings$(PPUEXT) $(INC)/textrec.inc $(INC)/filerec.inc\
		 syscalls.inc systypes.inc sysconst.inc syslinux$(PPUEXT)
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) linux.pp $(REDIR)
	$(DEL) filerec.inc textrec.inc

sockets$(PPUEXT) : sockets.pp linux.ppu $(INC)/textrec.inc $(INC)/filerec.inc
	$(COPY) $(INC)/filerec.inc $(INC)/textrec.inc .
	$(PP) $(OPT) sockets.pp $(REDIR)
	$(DEL) filerec.inc textrec.inc 

errors$(PPUEXT) : errors.pp strings.ppu
	$(PP) $(OPT) errors.pp $(REDIR)

prt0$(OEXT) : $(PRT)
	-as $(PRT) -o prt0$(OEXT)

gprt0$(OEXT) : gprt1.as
	-as gprt1.as -o gprt0$(OEXT)

libs	: all libfpc$(LIBEXT)

libfpc.so:
	$(PPUMOVE) -o fpc *.ppu

libfpc.a:
	$(PPUMOVE) -s -o fpc *.ppu
		
install : all
	$(MKDIR) $(UNITINSTALLDIR)/linuxunits
	$(INSTALL) -m 644 *$(PPUEXT) *$(OEXT) $(UNITINSTALLDIR)/linuxunits

libinstall : libs
	$(INSTALL) -m 755 libfpc$(LIBEXT) $(LIBINSTALLDIR)
	$(INSTALL) -m 644 *$(PPLEXT) $(UNITINSTALLDIR)/linuxunits

clean :
	-$(DEL) *$(OEXT)
	-$(DEL) *.s
	-$(DEL) *$(PPUEXT)
	-$(DEL) *.dif
	-$(DEL) *.PPS
	-$(DEL) log

libsclean : clean
	-$(DEL) *.a *.so *$(PPLEXT)

diffclean :
	-$(DEL) *.dif

distclean : diffclean libsclean clean 

%.dif : %.pp
	-$(DIFF) $(DIFFOPTS) $*.pp $(REFPATH)/linux/$*.pp >$*.dif

%.dif :  %.inc
	-$(DIFF) $(DIFFOPTS) $*.inc $(REFPATH)/linux/$*.inc >$*.dif

%.dif : %.as
	-$(DIFF) $(DIFFOPTS) $*.s $(REFPATH)/linux/$*.s >$*.dif

makefile.dif : makefile
	-$(DIFF) $(DIFFOPTS) makefile $(REFPATH)/linux/makefile > makefile.dif


diffs : dos.dif syslinux.dif objects.dif errno.dif prt0.dif prt1.dif \
	objects.dif linux.dif printer.dif os.dif syscalls.dif sysnr.dif \
   makefile.dif
