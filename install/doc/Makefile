##################################
# Man page installation for linux
#

ifndef PREFIXINSTALLDIR
PREFIXINSTALLDIR=/usr
endif

ifndef DOCINSTALLDIR
DOCINSTALLDIR:=$(PREFIXINSTALLDIR)/doc/fpc-$(shell ppc386 -iV)
endif 

installdoc:
	install -d -m 755 $(DOCINSTALLDIR)
	cp readme.txt $(DOCINSTALLDIR)/README
	cp whatsnew.txt $(DOCINSTALLDIR)/NEWS
	cp faq.txt faq.html $(DOCINSTALLDIR)
