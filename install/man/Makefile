##################################
# Man page installation for linux
#

ifndef PREFIXINSTALLDIR
PREFIXINSTALLDIR=/usr
endif

MANINSTALLDIR=$(PREFIXINSTALLDIR)/man

installman:
	install -d -m 755 $(MANINSTALLDIR)/man1
	install -d -m 755 $(MANINSTALLDIR)/man5
	install -m 644 -c man1/*.1 $(MANINSTALLDIR)/man1
	install -m 644 -c man5/*.5 $(MANINSTALLDIR)/man5
