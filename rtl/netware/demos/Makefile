# Makefile for freepascal nlm-test
# Needs working nlmconv + i386-netware-ld
# AD 8/2000

PPC386OPT = -XX -O3 -Tnetware -Xs
INCLUDES = 

OBJS = check.on

%.on:	%.pp
	ppc386 $(PPC386OPT) $(INCLUDES) $*.pp

all:	$(OBJS)

# copy test.nlm to sys:test on 4.11 (fs-develop) and 5.1 (fs-ad) server
install:	all
	ncftpput -u linux -p linux fs-develop /sys/test *.nlm
	ncftpput -u linux -p linux fs-ad /sys/test *.nlm

clean:
	rm -f *.on *.nlm *.ppn *.s *.bak *.o *.a

dist:		clean

distclean:	clean
