# Makefile for freepascal nlm-test
# Needs working nlmconv + i386-netware-ld
# AD 8/2000

UNITDIR = /usr/lib/fpc/1.1/units/netware/rtl
PPC386OPT = -a -al -Or -O3 -XX -Tnetware -Fi$(UNITDIR)
INCLUDES = -Fo$(UNITDIR) -Fu$(UNITDIR)

OBJS = test.on

%.on:	%.pas
	ppc386 $(PPC386OPT) $(INCLUDES) $*.pas

all:	$(OBJS)

#test.nlm: $(OBJS)
#	nlmconv -Ttest.def

# mount netware and copy test.nlm to sys:test on 4.11 and 5.1 server
install:	all
	[ -d nw ] || mkdir nw
	ncpmount -S FS-DEVELOP -U linux.home.ad -V sys -n nw
	cp -f test.nlm nw/test/test.nlm
	umount nw
	ncpmount -S FS-AD -U linux.home.ad -V sys -n nw
	cp -f test.nlm nw/test/test.nlm
	umount nw

clean:
	rm -f *.on *.nlm *.ppn *.s *.bak *.o
	[ -d nw ] && rmdir nw

dist:		clean
