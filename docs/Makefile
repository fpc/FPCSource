#
#   $Id$
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1996-98 by Michael van Canneyt
#
#   Makefile for the Free Pascal Documentation
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#####################################################################
# Configurable section
#####################################################################

# Debugging the install, that will only copy the .tex to index.html
# so a make html and make install goes quickly
#INSTALLDEBUG=1

# Which docs are made when 'html' is specified
HTML = user units ref prog

# Where do you want the html files to be put ?
ifndef PREFIXINSTALLDIR
PREFIXINSTALLDIR=/usr
endif

ifndef DOCINSTALLDIR
DOCINSTALLDIR:=$(PREFIXINSTALLDIR)/doc/fpc-$(shell ppc386 -iV)
endif 

#
# Program definitions
#

# Latex
ifndef LATEX
LATEX = latex
endif

# Latex2html conversion.
ifndef LATEX2HTML
LATEX2HTML = /usr/bin/latex2html
endif

# Options for converter
ifndef LATEX2HTMLOPTS
LATEX2HTMLOPTS = -no_reuse -up_url "../fpctoc.html"\
                 -up_title "Free Pascal manuals" -contents_in_navigation\
                 -html_version 4.0 -split 3 -image_type gif
endif

# Ascii mode ? (i.e. no pics)
ifndef ASCIIMODE
ASCIIMODE=NO
#ASCIIMODE=YES
endif

# DVI to PS converter
ifndef DVIPS
DVIPS=dvips
endif

# DVI to TXT converter
ifndef DVITXT
DVITXT=dvi2tty -w132
endif

# PDF converter
ifndef PDFLATEX
PDFLATEX=pdflatex
endif

# Makeindex programma
ifndef MAKEINDEX
MAKEINDEX=makeindex
endif

# tmpdir
export TMP=/tmp

#

#######################################################################
# End of configurable section. Do not edit after this line.
#######################################################################

PS = $(addsuffix .ps, $(HTML))
DVI = $(addsuffix .dvi, $(HTML))
TXT = $(addsuffix .txt, $(HTML))
PDF = $(addsuffix .pdf, $(HTML))
TOC = $(addsuffix .toc, $(HTML))
AUX = $(addsuffix .aux, $(HTML))
LOG = $(addsuffix .log, $(HTML))
LOT = $(addsuffix .lot, $(HTML))
OUT = $(addsuffix .out, $(HTML))
CHK = $(addsuffix .chk, $(HTML) unitex refex)

CHAPTERS = $(addsuffix .tex, crt dos getopts go32 graph linux printer\
             strings objects heaptrc sysutils ipc mouse gpm dxeload \
             emu387 ports math)

# Check if ascii-mode requested
ifeq ($(ASCIIMODE),YES)
LATEX2HTMLOPTS:=$(LATEX2HTMLOPTS) -ascii_mode
endif

#####################################################################
# Conversion from types
#####################################################################

.PHONY: clean dvi help html ps psdist htmldist pdfdist txtdist \
        htm txt pdf refex alldist messages onechap

.SUFFIXES: .dvi .tex .ps .txt .pdf

# default show help

.dvi.ps:
	$(DVIPS) $<

.tex.dvi:
	$(LATEX) $*
	-$(MAKEINDEX) $*
	$(LATEX) $*
	-$(MAKEINDEX) $*
	$(LATEX) $*

.dvi.txt:
	$(DVITXT) -o $*.txt  $*.dvi

.tex.pdf:
	$(PDFLATEX) $*
	-$(MAKEINDEX) $*
	$(PDFLATEX) $*
	-$(MAKEINDEX) $*
	$(PDFLATEX) $*


#####################################################################
# Targets
#####################################################################

help:
	@echo 'Possible targets :'
	@echo ' dvi           : Make documentation using latex.'
	@echo ' ps            : Make documentation using latex and dvips.'
	@echo ' html          : Make documentation using latex2html.'
	@echo ' pdf           : Make documentation using pdflatex'
	@echo ' txt           : dvi, convert to text using dvi2tty'
	@echo ' htm           : Convert .html to .htm files, zip result'
	@echo ' clean         : Clean up the mess.'
	@echo ' linuxexamples : Compile all examples for linux'
	@echo ' dosexamples   : Compile all examples for dos'
	@echo ' htmldist      : html, and rchive result.'
	@echo ' psdist        : ps, and archive result.'
	@echo ' pdfdist       : pdf, and archive result.'

clean:
	-rm -rf $(HTML)
	-rm -f $(TOC) $(LOG) $(DVI) $(PDF) $(AUX) $(OUT) $(PS) *.i* $(LOT) $(TXT) $(CHK)
	-rm -f onechap.dvi onechap.aux onechap.log

# Units
	-$(MAKE) -C crtex clean
	-$(MAKE) -C dosex clean
	-$(MAKE) -C go32ex clean
	-$(MAKE) -C heapex clean
	-$(MAKE) -C ipcex clean
	-$(MAKE) -C linuxex clean
	-$(MAKE) -C mouseex clean
	-$(MAKE) -C objectex clean
	-$(MAKE) -C optex clean
	-$(MAKE) -C printex clean
	-$(MAKE) -C sockex clean
	-$(MAKE) -C stringex clean
	-$(MAKE) -C sysutex clean
# Reference
	-$(MAKE) -C refex clean

$(TXT) : %.txt: %.dvi

$(DVI) : %.dvi: %.tex

$(PDF) : %.pdf: %.tex

unitex.chk:
ifndef INSTALLDEBUG
	$(MAKE) -C crtex tex
	$(MAKE) -C dosex tex
	$(MAKE) -C go32ex tex
	$(MAKE) -C heapex tex
	$(MAKE) -C ipcex tex
	$(MAKE) -C linuxex tex
	$(MAKE) -C mouseex tex
	$(MAKE) -C objectex tex
	$(MAKE) -C optex tex
	$(MAKE) -C printex tex
	$(MAKE) -C sockex tex
	$(MAKE) -C stringex tex
	$(MAKE) -C sysutex tex
endif
	touch unitex.chk

refex.chk:
ifndef INSTALLDEBUG
	$(MAKE) -C refex tex
endif
	touch refex.chk

messages: 
	msg2inc -TE ../compiler/errore.msg messages.tex

date.inc:
	@echo \\date\{`date +'%B %Y'`\} > date.inc

onechap: onechap.dvi

units.dvi: units.tex date.inc $(CHAPTERS)

ref.dvi: ref.tex date.inc

prog.dvi: prog.tex date.inc

user.dvi: user.tex date.inc

units.pdf: units.tex date.inc $(CHAPTERS)

ref.pdf: ref.tex date.inc

user.pdf: user.tex date.inc

prog.pdf: prog.tex date.inc

dvi : $(DVI)

txt : dvi $(TXT)

ps : dvi $(PS)

pdf : $(PDF)

all : dvi ps pdf txt html

#####################################################################
# Html creation
#####################################################################

user: user.chk

user.chk: user.tex date.inc
ifdef INSTALLDEBUG
	mkdir user
	cp user.tex user/index.html
else
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
         -t "User's guide for Free Pascal" user.tex
	-sed -f foot.sed <user/footnode.html >user/footnote.html
	-mv user/footnote.html user/footnode.html
	-rm -f user/labels.pl user/internals.pl user/.*.pag user/.*.dir
	-rm -f user/images.* user/*.log user/WARNINGS
endif
	touch user.chk

units: units.chk

units.chk: units.tex unitex.chk date.inc
ifdef INSTALLDEBUG
	mkdir units
	cp units.tex units/index.html
else
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 3 -link 2\
        -t "Unit reference for Free Pascal" units.tex
	-sed -f foot.sed <units/footnode.html >units/footnote.html
	-mv units/footnote.html units/footnode.html
	-rm -f units/labels.pl units/internals.pl units/.*.pag units/.*.dir
	-rm -f units/images.* units/*.log units/WARNINGS
endif
	touch units.chk

ref: ref.chk

ref.chk: refex.chk ref.tex date.inc
ifdef INSTALLDEBUG
	mkdir ref
	cp ref.tex ref/index.html
else
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 4 -link 2\
        -t "Free Pascal reference guide" ref.tex
	-sed -f foot.sed <ref/footnode.html >ref/footnote.html
	-mv ref/footnote.html ref/footnode.html
	-rm -f ref/labels.pl ref/internals.pl ref/.*.pag ref/.*.dir
	-rm -f ref/images.* ref/*.log ref/WARNINGS
endif
	touch ref.chk	

prog: prog.chk

prog.chk: prog.tex date.inc
ifdef INSTALLDEBUG
	mkdir prog
	cp prog.tex prog/index.html
else
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
        -t "Free Pascal programmers guide" prog.tex
	-sed -f foot.sed <prog/footnode.html >prog/footnote.html
	-mv prog/footnote.html prog/footnode.html
	-rm -f prog/labels.pl prog/internals.pl prog/.*.pag prog/.*.dir
	-rm -f prog/images.* prog/*.log prog/WARNINGS
endif
	touch prog.chk

internal: internal.chk

internal.chk: internal.tex date.inc
ifdef INSTALLDEBUG
	mkdir internal
	cp internal.tex internal/index.html
else
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 4 -link 2\
        -t "Free Pascal Compiler Documentation" internal.tex
	-sed -f foot.sed <internal/footnode.html >internal/footnote.html
	-mv internal/footnote.html internal/footnode.html
	-rm -f internal/labels.pl internal/internals.pl internal/.*.pag internal/.*.dir
	-rm -f internal/images.* internal/*.log internal/WARNINGS
endif
	touch internal.chk	

html: $(HTML)

#####################################################################
# Installation
#####################################################################

pdfinstall: pdf
	install -d -m 755 $(DOCINSTALLDIR)
	cp $(PDF) $(DOCINSTALLDIR)

htmlinstall: html
	install -d -m 755 $(DOCINSTALLDIR)
	cp fpctoc.html $(DOCINSTALLDIR)
	cp -R buttons $(DOCINSTALLDIR)
	cp -R search $(DOCINSTALLDIR)
	sed s/DOCDIR/"$(subst /,\/,$(DOCINSTALLDIR))"/ $(DOCINSTALLDIR)/search/fpchelp.idksm.linux > $(DOCINSTALLDIR)/search/fpchelp.idksm
	cp -f $(DOCINSTALLDIR)/search/filelist.txt.linux $(DOCINSTALLDIR)/search/filelist.txt
	rm -f $(DOCINSTALLDIR)/search/*.linux
	cp -R $(HTML) $(DOCINSTALLDIR)
	find $(DOCINSTALLDIR) -name 'CVS' | xargs -n1 rm -rf

doshtmlinstall: html
	install -d -m 755 $(DOCINSTALLDIR)
	cp fpctoc.html $(DOCINSTALLDIR)
	cp -R buttons $(DOCINSTALLDIR)
	cp -R search $(DOCINSTALLDIR)
	rm -f $(DOCINSTALLDIR)/search/*.linux
	cp -R $(HTML) $(DOCINSTALLDIR)
	find $(DOCINSTALLDIR) -name 'CVS' | xargs -n1 rm -rf

www-install: psdist htmlinstall
	scp fpcdoc.zip fpcdocps.zip fpcdocps.tar.gz fpcdoc.tar.gz tfdec1:htdocs/fpk
	ssh tfdec1 '(cd htdocs/fpk/docs ; /usr/local/bin/tar -xzf ../fpcdoc.tar.gz)'

install:
	@echo Choose install from:
	@echo pdfinstall,htmlinstall,htmldosinstall
	@exit

#####################################################################
# Archives
#####################################################################

psdist: $(PS)
	tar -cvzf fpcdocps.tar.gz $(PS)
	zip docs-ps $(PS)

pdfdist: pdf
	zip docs-pdf $(PDF)

dvidist: dvi
	zip docs-dvi $(DVI)

txtdist: txt
	zip docs-txt $(TXT)

#####################################################################
# Zips of htmls
#####################################################################

DOCPREFIX=doc
BASEDIR=$(shell pwd)

htmltar: 
	$(MAKE) htmlinstall DOCINSTALLDIR=$(DOCPREFIX)
	cd $(DOCPREFIX) ; tar cz * > $(BASEDIR)/fpcdoc.tar.gz ; cd $(BASEDIR)
	rm -rf $(DOCPREFIX)

htmlzip:
	$(MAKE) doshtmlinstall DOCINSTALLDIR=$(DOCPREFIX)
	zip -D9rq doc-html $(DOCPREFIX)/*
	rm -rf $(DOCPREFIX)

htmzip:
	$(MAKE) doshtmlinstall DOCINSTALLDIR=$(DOCPREFIX)
	makehtm `find $(DOCPREFIX) -name '*.html'`
	zip -D9rq doc-htm `find $(DOCPREFIX) -name '*.htm' -or -name '*.gif' -or -name '*.css'`
	rm -rf $(DOCPREFIX)

htmldist: htmlzip htmltar

htmdist: htmzip

alldist: dvidist psdist txtdist pdfdist htmldist htmdist

distclean: clean
	-rm -f *.tar.gz *.zip

#####################################################################
# Examples
#####################################################################

examples:
	$(MAKE) -C crtex
	$(MAKE) -C dosex
	$(MAKE) -C optex
	$(MAKE) -C printex
	$(MAKE) -C refex
	$(MAKE) -C stringex
	$(MAKE) -C ipcex
	$(MAKE) -C objectex
	$(MAKE) -C sysutex

dosexamples: examples
	$(MAKE) -C go32ex
	$(MAKE) -C mouseex

linuxexamples: examples
	$(MAKE) -C linuxex
	$(MAKE) -C sockex
	$(MAKE) -C ipcex

#
# $Log$
# Revision 1.39  2000-07-09 15:44:15  peter
#   * image_type gif added
#
# Revision 1.38  2000/07/04 21:23:04  michael
# + Added math unit
#
# Revision 1.37  2000/07/04 19:55:48  michael
# + Added ports unit
#
# Revision 1.36  2000/06/14 19:38:43  michael
# + Added gpm units, and corrected L2H options
#
# Revision 1.35  2000/02/20 16:27:38  peter
#   * htmlinstall fixes
#
# Revision 1.33  2000/01/23 20:15:16  peter
#   * rpm/deb target in install dir
#   * install also faq,readme,news for rpm/deb
#
# Revision 1.32  2000/01/14 12:16:40  peter
#   * package generation fixes
#
# Revision 1.31  2000/01/11 09:55:36  peter
#   * docinstalldir is now calculated by asking ppc386 -iV
#
# Revision 1.30  1999/07/29 21:03:41  peter
#   * html 3.2, because 3.1 is buggy !!
#
# Revision 1.29  1999/07/16 10:15:31  peter
#   * install targets renamed
#
# Revision 1.28  1999/06/25 22:12:10  michael
# + Update to version 0.19 of listings package
#
# Revision 1.27  1999/06/15 13:53:16  peter
#   * htmlzip,htmzip,htmltar updates
#   * include search/ in distro
#   * fix paths under linux automaticly
#
# Revision 1.26  1999/06/08 22:11:28  peter
#   * debian updates
#
# Revision 1.25  1999/06/08 18:28:13  peter
#   * fixes for 0.99.12 release
#
# Revision 1.24  1999/06/03 00:06:42  peter
#   * html creation works again
#
# Revision 1.23  1999/06/02 17:41:33  peter
#   * fixes for html creation
#
# Revision 1.22  1999/05/31 16:37:08  peter
#   * fixes for html creation
#
# Revision 1.21  1999/05/28 19:59:37  michael
# Additional changes
#
# Revision 1.20  1999/04/10 14:57:43  michael
# + Added sysutils
#
# Revision 1.19  1999/02/12 14:15:00  michael
# * Last-minute changes so it can be texed
#
# Revision 1.18  1999/02/02 22:47:04  michael
# + Better clean
#
# Revision 1.17  1999/01/17 15:12:50  michael
# Added examples 27-40
#
# Revision 1.16  1998/12/15 23:50:53  michael
# * Some updates
#
# Revision 1.15  1998/11/17 23:42:02  michael
# + too many changes to enumerate
#
# Revision 1.14  1998/10/01 12:57:22  michael
# + Correction in dependencies
#
# Revision 1.13  1998/09/13 19:26:13  michael
# + Added messages target
#
# Revision 1.12  1998/09/13 12:42:05  michael
# Added error messages and used listings package for program examples
#
# Revision 1.11  1998/09/11 12:08:07  michael
# Additional cleans
#
# Revision 1.10  1998/09/11 11:01:10  peter
#   * export a TMP=/tmp for dvips which don't like '.' in the directory
#
#