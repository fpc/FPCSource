# Makefile for freepascal nlm-test
# Needs working nlmconv + i386-netware-ld
# AD 8/2000

UNITDIR = /usr/lib/fpc/1.1/cross/i386-netware/units/rtl
PPC386OPT = -a -al -Or -O3 -XX -Tnetware -Fi$(UNITDIR)
INCLUDES = -Fo$(UNITDIR) -Fu$(UNITDIR)

OBJS = test.on thrd.on

%.on:	%.pas
	ppc386 $(PPC386OPT) $(INCLUDES) $*.pas

all:	$(OBJS)

#test.nlm: $(OBJS)
#	nlmconv -Ttest.def

# mount netware and copy test.nlm to sys:test on 4.11 and 5.1 server
install:	all
	ncftpput -u linux -p linux fs-develop /sys/test *.nlm
	ncftpput -u linux -p linux fs-ad /sys/test *.nlm


clean:
	rm -f *.on *.nlm *.ppn *.s *.bak *.o

dist:		clean
