# Makefile for freepascal rtl for netware
# Needs working nlmconv + i386-netware-ld

UNITDIR = $(INSTALL_PREFIX)/usr/lib/fpc/1.1/units/netware/rtl
#PPC386OPT = -n -dMT -dDEBUG_MT -di386 -dSYSTEMDEBUG -O3 -Sg -Tnetware -Aelf -a -al -gg -FE.
#PPC386OPT = -n -di386 -dSYSTEMDEBUG -O3 -Sg -Tnetware -Aelf -a -al -gg -FE.
PPC386OPT = -n -di386 -O3 -Og -Sg -Tnetware -Aelf -a -al -FE.
PPC386OPTDBG = -n -di386 -O3 -Sg -Tnetware -Aelf -a -al -gg -FE.
INCLUDES = -I../inc -I../i386 -I../objpas

SYSUNIT=system
NWPRE=nwpre
PRELUDE=prelude
OBJEXT=on
PPUEXT=ppn
ASMEXT=s
OBJS = $(SYSUNIT).$(OBJEXT) $(NWPRE).$(OBJEXT) $(PRELUDE).$(OBJEXT) ../inc/os_types.$(OBJEXT) ../inc/strings.$(OBJEXT) dos.$(OBJEXT) ../objpas/objpas.$(OBJEXT) sysutils.$(OBJEXT) crt.$(OBJEXT) sockets.$(OBJEXT) mouse.$(OBJEXT) netware.$(OBJEXT) video.$(OBJEXT) keyboard.$(OBJEXT) ../objpas/math.$(OBJEXT) ../objpas/typinfo.$(OBJEXT) ../inc/objects.$(OBJEXT) ../inc/getopts.$(OBJEXT) ../inc/heaptrc.$(OBJEXT) varutils.$(OBJEXT) ../i386/cpu.$(OBJEXT) ../i386/mmx.$(OBJEXT) winsock2.$(OBJEXT)


all:	$(OBJS)

$(SYSUNIT).$(OBJEXT):	$(SYSUNIT).pp nwsys.inc
	ppc386 -Us $(PPC386OPT) $(INCLUDES) $(SYSUNIT).pp

$(NWPRE).$(OBJEXT):	$(NWPRE).as
	as $(NWPRE).as -o $(NWPRE).$(OBJEXT)

$(PRELUDE).$(OBJEXT):	$(NWPRE).as
	as $(PRELUDE).as -o $(PRELUDE).$(OBJEXT)

%.$(OBJEXT):	%.pp nwsys.inc
	ppc386 $(PPC386OPT) $(INCLUDES) $*.pp

install: $(OBJS)
	rm -f $(UNITDIR)/*
	cp -f $(SYSUNIT).$(OBJEXT) $(UNITDIR)
	cp -f $(SYSUNIT).$(PPUEXT) $(UNITDIR)
	cp -f dos.$(OBJEXT) $(UNITDIR)
	cp -f dos.$(PPUEXT) $(UNITDIR)
	cp -f strings.$(OBJEXT) $(UNITDIR)
	cp -f strings.$(PPUEXT) $(UNITDIR)
	cp -f $(NWPRE).$(OBJEXT) $(UNITDIR)
	cp -f $(PRELUDE).$(OBJEXT) $(UNITDIR)
	cp -f sysutils.$(OBJEXT) $(UNITDIR)
	cp -f sysutils.$(PPUEXT) $(UNITDIR)
	cp -f objpas.$(OBJEXT) $(UNITDIR)
	cp -f objpas.$(PPUEXT) $(UNITDIR)
	cp -f crt.$(OBJEXT) $(UNITDIR)
	cp -f crt.$(PPUEXT) $(UNITDIR)
	cp -f sockets.$(OBJEXT) $(UNITDIR)
	cp -f sockets.$(PPUEXT) $(UNITDIR)
	cp -f mouse.$(OBJEXT) $(UNITDIR)
	cp -f mouse.$(PPUEXT) $(UNITDIR)
	cp -f netware.$(OBJEXT) $(UNITDIR)
	cp -f netware.$(PPUEXT) $(UNITDIR)
	cp -f video.$(OBJEXT) $(UNITDIR)
	cp -f video.$(PPUEXT) $(UNITDIR)
	cp -f keyboard.$(OBJEXT) $(UNITDIR)
	cp -f keyboard.$(PPUEXT) $(UNITDIR)
	cp -f math.$(OBJEXT) $(UNITDIR)
	cp -f math.$(PPUEXT) $(UNITDIR)
	cp -f typinfo.$(OBJEXT) $(UNITDIR)
	cp -f typinfo.$(PPUEXT) $(UNITDIR)
	cp -f objects.$(OBJEXT) $(UNITDIR)
	cp -f objects.$(PPUEXT) $(UNITDIR)
	cp -f getopts.$(OBJEXT) $(UNITDIR)
	cp -f getopts.$(PPUEXT) $(UNITDIR)
	cp -f heaptrc.$(OBJEXT) $(UNITDIR)
	cp -f heaptrc.$(PPUEXT) $(UNITDIR)
	cp -f varutils.$(OBJEXT) $(UNITDIR)
	cp -f varutils.$(PPUEXT) $(UNITDIR)
	cp -f cpu.$(OBJEXT) $(UNITDIR)
	cp -f cpu.$(PPUEXT) $(UNITDIR)
	cp -f mmx.$(OBJEXT) $(UNITDIR)
	cp -f mmx.$(PPUEXT) $(UNITDIR)
	cp -f os_types.$(PPUEXT) $(UNITDIR)
	cp -f winsock2.$(OBJEXT) $(UNITDIR)
	cp -f winsock2.$(PPUEXT) $(UNITDIR)
	cp -f nwimp/*.imp $(UNITDIR)

clean:
	rm -f *.$(OBJEXT) *.$(PPUEXT) *.$(ASMEXT) *.bak *.rst *.a

dist:
	clean
