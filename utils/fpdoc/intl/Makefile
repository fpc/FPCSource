all:	de

clean:
	@rm -f *.mo *.pot

de:
	msgfmt fpdoc.de.po dwriter.de.po -o fpdoc.de.mo
	msgfmt dglobals.de.po -o dglobals.de.mo
	msgfmt makeskel.de.po -o makeskel.de.mo
	msgfmt fpdocmk.de.po -o fpdocmk.de.mo

pot:
	rstconv -i ../dglobals.rst -o dglobals.pot
	rstconv -i ../dwriter.rst -o dwriter.pot
	rstconv -i ../fpdoc.rst -o fpdoc.pot
	rstconv -i ../makeskel.rst -o makeskel.pot
	rstconv -i ../fpdocmk.rst -o fpdocmk.pot

install:
	install -D -m 0644 fpdoc.de.mo /usr/local/share/locale/de/LC_MESSAGES/fpdoc.mo
	install -D -m 0644 dglobals.de.mo /usr/local/share/locale/de/LC_MESSAGES/dglobals.mo
	install -D -m 0644 makeskel.de.mo /usr/local/share/locale/de/LC_MESSAGES/makeskel.mo
	install -D -m 0644 fpdocmk.de.mo /usr/local/share/locale/de/LC_MESSAGES/fpdocmk.mo
