##################################
# Man page installation for linux
#

ifndef FPC
ifdef PP
FPC=$(PP)
endif
endif
ifndef FPC
FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH))))
ifneq ($(FPCPROG),)
FPCPROG:=$(firstword $(FPCPROG))
FPC:=$(shell $(FPCPROG) -PB)
ifneq ($(findstring Error,$(FPC)),)
override FPC=ppc386
endif
else
override FPC=ppc386
endif
endif
override FPC:=$(subst $(SRCEXEEXT),,$(FPC))
override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)

ifndef INSTALL_PREFIX
INSTALL_PREFIX=/usr/local
endif

ifndef INSTALL_DOCDIR
INSTALL_DOCDIR:=$(INSTALL_PREFIX)/doc/fpc-$(shell $(FPC) -iV)
endif 

installdoc:
	install -d -m 755 $(INSTALL_DOCDIR)
	cp readme.txt $(INSTALL_DOCDIR)/README
	cp whatsnew.txt $(INSTALL_DOCDIR)/NEWS
	cp faq.htm $(INSTALL_DOCDIR)/faq.html
	cp faq.txt $(INSTALL_DOCDIR)

faq.txt: faq.html
	lynx -dump faq.html > faq.txt

