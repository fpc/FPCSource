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
DOCINSTALLDIR=/usr/doc/fpc-0.99.8
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

# tmpdir
export TMP=/tmp

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

.PHONY: clean dvi help html ps psdist htmldist pdfdist txtdist \
        htm txt pdf refex alldist

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
	$(PDFLATEX) $*pdf
	-$(MAKEINDEX) $*pdf
	$(PDFLATEX) $*pdf
	-$(MAKEINDEX) $*pdf
	$(PDFLATEX) $*pdf
	mv $*pdf.pdf $*.pdf

$(TXT) : %.txt: %.dvi

$(DVI) : %.dvi: %.tex

$(PDF) : %.pdf: %.tex

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
	-rm -f *.aux *.log *.dvi *.ps *.toc *.i* *.lot *.pdf *.txt *.chk
	-$(MAKE) -C crtex clean
	-$(MAKE) -C dosex clean
	-$(MAKE) -C go32ex clean
	-$(MAKE) -C linuxex clean
	-$(MAKE) -C mouseex clean
	-$(MAKE) -C optex clean
	-$(MAKE) -C printex clean
	-$(MAKE) -C sockex clean
	-$(MAKE) -C mouseex clean
	-$(MAKE) -C go32ex clean
	-$(MAKE) -C stringex clean
	-$(MAKE) -C refex clean

unitex.chk:
	$(MAKE) -C crtex tex
	$(MAKE) -C dosex tex
	$(MAKE) -C go32ex tex
	$(MAKE) -C linuxex tex
	$(MAKE) -C optex tex
	$(MAKE) -C mouseex tex
	$(MAKE) -C printex tex
	$(MAKE) -C stringex tex
	$(MAKE) -C sockex tex
	$(MAKE) -C mouseex tex
	$(MAKE) -C go32ex tex
	touch unitex.chk

refex.chk:
	$(MAKE) -C refex tex
	touch refex.chk

units.dvi: units.tex $(CHAPTERS) unitex.chk

ref.dvi: ref.tex refex.chk

units.pdf: units.tex $(CHAPTERS) unitex.chk

ref.pdf: ref.tex refex.chk

dvi : $(DVI)

txt : dvi $(TXT)

ps : dvi $(PS)

pdf : $(PDF)

all : dvi ps pdf txt html

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
	touch prog.chk

html: $(HTML)

htm: html
	makehtm `find . -name '*.html'`
	zip -q fpcdoc `find . -name '*.htm'` `find . -name '*.gif'`
	rm `find -name '*.htm'`

htmdist: htm

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
# Archives
#####################################################################

psdist: $(PS)
	tar -cvzf fpcdocps.tar.gz $(PS)
	zip fpcdocps $(PS)

pdfdist: pdf
	zip fpcdocpdf $(PDF)

dvidist: dvi
	zip fpcdocdvi $(DVI)

htmldist: html
	find . -name '*.html' >htmllist
	find . -name '*.gif' >>htmllist
	tar -czf fpcdoc.tar.gz --files-from=htmllist
	rm -f htmllist

txtdist: txt
	zip fpcdoctxt $(TXT)

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
	$(MAKE) -C sockex
	$(MAKE) -C stringex

dosexamples: examples
	$(MAKE) -C go32ex
	$(MAKE) -C mouseex

linuxexamples: examples
	$(MAKE) -C linuxex
	$(MAKE) -C sockex

#
# $Log$
# Revision 1.11  1998-09-11 12:08:07  michael
# Additional cleans
#
# Revision 1.10  1998/09/11 11:01:10  peter
#   * export a TMP=/tmp for dvips which don't like '.' in the directory
#
#