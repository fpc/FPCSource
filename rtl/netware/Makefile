# Makefile for freepascal rtl for netware
# Needs working nlmconv + i386-netware-ld

UNITDIR = /usr/lib/fpc/1.1/units/netware/rtl
PPC386OPT = -n -di386 -dSYSTEMDEBUG -O3 -Sg -Tnetware -Aelf -a -al -FE.
INCLUDES = -I../inc -I../i386 -I../objpas

SYSUNIT=system
OBJEXT=on
PPUEXT=ppn
ASMEXT=s
OBJS = $(SYSUNIT).$(OBJEXT) ../inc/strings.$(OBJEXT) dos.$(OBJEXT) nwpre.$(OBJEXT) ../objpas/objpas.$(OBJEXT) sysutils.$(OBJEXT) crt.$(OBJEXT) sockets.$(OBJEXT) mouse.$(OBJEXT) netware.$(OBJEXT) video.$(OBJEXT) keyboard.$(OBJEXT) ../objpas/math.$(OBJEXT) ../objpas/typinfo.$(OBJEXT) ../inc/objects.$(OBJEXT) ../inc/getopts.$(OBJEXT) ../inc/heaptrc.$(OBJEXT) varutils.$(OBJEXT) ../i386/cpu.$(OBJEXT) ../i386/mmx.$(OBJEXT)


all:	$(OBJS)

$(SYSUNIT).$(OBJEXT):	$(SYSUNIT).pp nwsys.inc
	ppc386 -Us $(PPC386OPT) $(INCLUDES) $(SYSUNIT).pp

%.$(OBJEXT):	%.pp nwsys.inc
	ppc386 $(PPC386OPT) $(INCLUDES) $*.pp

install: $(OBJS)

	cp -f $(SYSUNIT).$(OBJEXT) $(UNITDIR)
	cp -f $(SYSUNIT).$(PPUEXT) $(UNITDIR)
	cp -f dos.$(OBJEXT) $(UNITDIR)
	cp -f dos.$(PPUEXT) $(UNITDIR)
	cp -f strings.$(OBJEXT) $(UNITDIR)
	cp -f strings.$(PPUEXT) $(UNITDIR)
	cp -f nwpre.$(OBJEXT) $(UNITDIR)
	cp -f nwpre.$(PPUEXT) $(UNITDIR)
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
	cp -f nwimp/*.imp $(UNITDIR)

clean:
	rm -f *.$(OBJEXT) *.$(PPUEXT) *.$(ASMEXT) *.bak

dist:
	clean
