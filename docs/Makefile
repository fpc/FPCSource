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

# Where do you want the html files to be put ?
ifndef DOCINSTALLDIR
DOCINSTALLDIR=/usr/doc/fpc-0.99.5
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
LATEX2HTML = latex2html
endif

# Options for converter
ifndef LATEX2HTMLOPTS
LATEX2HTMLOPTS = -no_reuse -up_url "../fpctoc.html"\
                 -up_title "Free Pascal manuals" -contents_in_navigation\
                 -html_version 3.0
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

#

#######################################################################
# End of configurable section. Do not edit after this line.
#######################################################################

HTML = user units ref prog

PS = $(addsuffix .ps, $(HTML))
DVI = $(addsuffix .dvi, $(HTML))
TXT = $(addsuffix .txt, $(HTML))
PDF = $(addsuffix .pdf, $(HTML))

CHAPTERS = $(addsuffix .tex, crt dos getopts go32 graph linux printer strings)

# Check if ascii-mode requested
ifeq ($(ASCIIMODE),YES)
LATEX2HTMLOPTS:=$(LATEX2HTMLOPTS) -ascii_mode
endif

#####################################################################
# Conversion from types
#####################################################################

.PHONY: all clean dvi help html ps psdist htmldist htm txt pdf refex

.SUFFIXES: .dvi .tex .ps .txt .pdf

# default show help
all: help

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
	$(PDFLATEX) $*pdf
	-$(MAKEINDEX) $*pdf
	$(PDFLATEX) $*pdf
	-$(MAKEINDEX) $*pdf
	$(PDFLATEX) $*pdf

$(TXT) : %.txt: %.dvi

$(DVI) : %.dvi: %.tex

$(PDF) : %.pdf: %.tex

#####################################################################
# Targets
#####################################################################

help:
	@echo 'Possible targets :'
	@echo ' dvi   : Make documentation using latex.'
	@echo ' ps    : Make documentation using latex and dvips.'
	@echo ' html  : Make documentation using latex2html.'
	@echo ' htm   : Convert .html to .htm files, zip result'
	@echo ' clean : Clean up the mess.'

clean:
	-rm -rf $(HTML)
	-rm -f *.aux *.log *.dvi *.ps *.toc *.i* *.lot *.pdf *.txt *.chk
	-$(MAKE) -C refex clean
	-$(MAKE) -C linuxex clean
	-$(MAKE) -C crtex clean
	-$(MAKE) -C dosex clean
	-$(MAKE) -C optex clean
	-$(MAKE) -C printex clean
	-$(MAKE) -C stringex clean
	-$(MAKE) -C sockex clean

unitex.chk:
	$(MAKE) -C crtex tex
	$(MAKE) -C linuxex tex
	$(MAKE) -C dosex tex
	$(MAKE) -C optex tex
	$(MAKE) -C printex tex
	$(MAKE) -C stringex tex
	$(MAKE) -C sockex tex
	touch unitex.chk

refex.chk:
	$(MAKE) -C refex tex
	touch refex.chk

units.dvi: units.tex $(CHAPTERS) unitex.chk

ref.dvi: ref.tex refex.chk

dvi : $(DVI)

txt : dvi $(TXT)

ps : dvi $(PS)

pdf : $(PDF)

user: user.chk

user.chk: user.tex
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
         -t "User's guide for Free Pascal" user.tex
	sed -f foot.sed <user/footnode.html >user/footnote.html
	mv user/footnote.html user/footnode.html
	rm -f user/labels.pl user/internals.pl user/.*.pag user/.*.dir
	rm -f user/images.* user/*.log
	touch user.chk
	
units: units.chk

units.chk: units.tex unitex.chk
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 3 -link 2\
        -t "Unit reference for Free Pascal" units.tex
	sed -f foot.sed <units/footnode.html >units/footnote.html
	mv units/footnote.html units/footnode.html
	rm -f units/labels.pl units/internals.pl units/.*.pag units/.*.dir
	touch units.chk

ref: ref.chk

ref.chk: refex.chk ref.tex
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 3 -link 2\
        -t "Free Pascal reference guide" ref.tex
	sed -f foot.sed <ref/footnode.html >ref/footnote.html
	mv ref/footnote.html ref/footnode.html
	rm -f ref/labels.pl ref/internals.pl ref/.*.pag ref/.*.dir
	touch ref.chk	

prog: prog.chk

prog.chk: prog.tex 
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
        -t "Free Pascal programmers guide" prog.tex
	sed -f foot.sed <prog/footnode.html >prog/footnote.html
	mv prog/footnote.html prog/footnode.html
	rm -f prog/labels.pl prog/internals.pl prog/.*.pag prog/.*.dir

html: $(HTML)

htm:
	makehtm `find . -name '*.html'`
	zip fpcdoc `find . -name '*.htm'`
	rm `find -name '*.htm'`
	
all-formats: ps txt pdf html
	
#####################################################################
# Installation
#####################################################################
	
install:
	install -d -m 755 $(DOCINSTALLDIR)
	cp fpctoc.html $(DOCINSTALLDIR)
	cp -R buttons $(DOCINSTALLDIR)
	cp -R $(HTML) $(DOCINSTALLDIR)

www-install: psdist htmldist htm
	scp fpcdoc.zip fpcdocps.zip fpcdocps.tar.gz fpcdoc.tar.gz tfdec1:htdocs/fpk
	ssh tfdec1 '(cd htdocs/fpk/docs ; /usr/local/bin/tar -xzf ../fpcdoc.tar.gz)'

#####################################################################
# Distribution
#####################################################################

psdist: ps
	tar -cvzf fpcdocps.tar.gz *.ps
	zip fpcdocps *.ps

pdfdist: pdf
	zip fpcdocpdf *.pdf

htmldist: html
	find . -name '*.html' >htmllist
	find . -name '*.gif' >>htmllist
	tar -czvf fpcdoc.tar.gz --files-from=htmllist
	rm -f htmllist
	
dist:
	-mkdir $(DISTDIR)/docs
	cp Makefile README.DOCS makehtm fpktoc.html $(DISTDIR)/docs
	cp *.tex *.sty *.perl *.doc $(DISTDIR)/docs

distclean: clean
	-rm -f *.tar.gz *.zip
