##################################
# Man page installation for linux
#

ifndef INSTALL_PREFIX
INSTALL_PREFIX=/usr/local
endif

ifndef INSTALL_DOCDIR
INSTALL_DOCDIR:=$(INSTALL_PREFIX)/doc/fpc-$(shell ppc386 -iV)
endif 

installdoc:
	install -d -m 755 $(INSTALL_DOCDIR)
	cp readme.txt $(INSTALL_DOCDIR)/README
	cp whatsnew.txt $(INSTALL_DOCDIR)/NEWS
	cp faq.htm $(INSTALL_DOCDIR)/faq.html
	cp faq.txt $(INSTALL_DOCDIR)

faq.txt: faq.html
	lynx -dump faq.html > faq.txt

