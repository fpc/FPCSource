##################################
# Man page installation for linux
#

ifndef INSTALL_PREFIX
INSTALL_PREFIX=/usr/local
endif

INSTALL_MANDIR=$(INSTALL_PREFIX)/man

installman:
	install -d -m 755 $(INSTALL_MANDIR)/man1
	install -d -m 755 $(INSTALL_MANDIR)/man5
	install -c -m 644 man1/*.1 $(INSTALL_MANDIR)/man1
	install -c -m 644 man5/*.5 $(INSTALL_MANDIR)/man5
