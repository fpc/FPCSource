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

# Can be 'report','book' or 'html'
PREAMBLETYPE = report

# Where do you want the html files to be put ?
ifndef PREFIXINSTALLDIR
PREFIXINSTALLDIR=/usr
endif

ifndef DOCINSTALLDIR
DOCINSTALLDIR:=$(PREFIXINSTALLDIR)/doc/fpc-$(shell ppc386 -iV)
endif

FPDOC=fpdoc

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
                 -up_title "Free Pascal manuals"\
                 -html_version 4.0 -split 3
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
TOC = $(addsuffix .toc, $(HTML) onechap)
AUX = $(addsuffix .aux, $(HTML) onechap)
LOG = $(addsuffix .log, $(HTML) onechap)
LOT = $(addsuffix .lot, $(HTML) onechap)
OUT = $(addsuffix .out, $(HTML) onechap)
CHK = $(addsuffix .chk, $(HTML) progex unitex refex htex onechap scripts)

CHAPTERS = $(addsuffix .tex, crt dos getopts go32 graph linux printer\
             strings objects heaptrc sysutils ipc mouse gpm dxeload \
             emu387 ports math keyboard typinfo video)

FCLCHAPTERS = $(addsuffix .tex, classes)

GTKPDF=$(addsuffix .pdf,$(addprefix gtk, 1 2 3 4 5))

# Check if ascii-mode requested
ifeq ($(ASCIIMODE),YES)
LATEX2HTMLOPTS:=$(LATEX2HTMLOPTS) -ascii_mode
endif


#####################################################################
# Conversion from types
#####################################################################

.PHONY: clean dvi help html ps psdist htmldist pdfdist txtdist \
        htm txt pdf refex alldist messages onechap gtk

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
	-rm -f preamble.inc date.inc
	-rm -f *.htex
	-rm -rf $(HTML) onechap
	-rm -f $(TOC) $(LOG) $(DVI) $(PDF) $(AUX) $(OUT) $(PS) *.i* $(LOT) $(TXT) $(CHK)
	-rm -f onechap.dvi onechap.pdf onechap.ps onechap.out onechap.log onechap.aux

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
	-$(MAKE) -C mathex clean
	-$(MAKE) -C graphex clean
	-$(MAKE) -C gpmex clean
	-$(MAKE) -C typinfex clean
	-$(MAKE) -C kbdex clean
# Reference
	-$(MAKE) -C refex clean
# Programmers manual
	-$(MAKE) -C progex clean

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
	$(MAKE) -C mmouseex tex
	$(MAKE) -C objectex tex
	$(MAKE) -C optex tex
	$(MAKE) -C printex tex
	$(MAKE) -C sockex tex
	$(MAKE) -C stringex tex
	$(MAKE) -C sysutex tex
	$(MAKE) -C mathex tex
	$(MAKE) -C graphex tex
	$(MAKE) -C gpmex tex
	$(MAKE) -C kbdex tex
	$(MAKE) -C typinfex tex
	$(MAKE) -C videoex tex
endif
	touch unitex.chk

refex.chk:
ifndef INSTALLDEBUG
	$(MAKE) -C refex tex
endif
	touch refex.chk

progex.chk:
ifndef INSTALLDEBUG
	$(MAKE) -C progex tex
endif
	touch progex.chk

messages:
	msg2inc -TE ../compiler/errore.msg messages.tex

#
# Include files
#

date.inc:
	@echo \\date\{`date +'%B %Y'`\} > date.inc

preamble.inc:
	cp -f preamble.$(PREAMBLETYPE) preamble.inc

ifeq ($(OS),Windows_NT)
inWinNT=1
endif

ifndef inWinNT
scripts.chk:
	chmod +x tex2htex
	chmod +x pp2tex
	chmod +x progex/c2tex
	touch scripts.chk
endif

ifndef inWinNT
includes: date.inc preamble.inc scripts.chk
else
includes: date.inc preamble.inc
endif

#####################################################################
# Tex from XML
#####################################################################
FPCLASSESOPT=--descr=classes.xml --input='../fcl/linux/classes.pp -I../fcl/inc'

classes.tex: classes.xml
	$(FPDOC) --output=. $(FPCLASSESOPT) --format=latex

#####################################################################
# DVI files
#####################################################################

onechap: onechap.dvi

units.dvi: units.tex includes $(CHAPTERS)

ref.dvi: ref.tex includes

prog.dvi: prog.tex includes

user.dvi: user.tex includes

units.pdf: units.tex includes $(CHAPTERS)

ref.pdf: ref.tex includes

user.pdf: user.tex includes

prog.pdf: prog.tex includes

onechap.pdf: onechap.tex includes

onechap.dvi: onechap.tex includes


dvi : $(DVI)

txt : dvi $(TXT)

ps : dvi $(PS)

pdf : $(PDF)

all : dvi ps pdf txt html


#####################################################################
# Html creation
#####################################################################

htex: htex.chk
htex.chk: $(wildcard *.tex) includes
	./tex2htex
	touch htex.chk

user: user.chk
user.chk: htex.chk includes
ifdef INSTALLDEBUG
	mkdir user
	cp user.tex user/index.html
else
	cp -f preamble.html preamble.inc
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
	-t "User's guide for Free Pascal" user.htex
	-sed -f foot.sed <user/footnode.html >user/footnote.html
	-mv user/footnote.html user/footnode.html
	-rm -f user/labels.pl user/internals.pl user/.*.pag user/.*.dir
	-rm -f user/images.* user/*.log user/WARNINGS
endif
	touch user.chk

units: units.chk
units.chk: unitex.chk htex.chk includes
ifdef INSTALLDEBUG
	mkdir units
	cp units.tex units/index.html
else
	cp -f preamble.html preamble.inc
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 3 -link 2\
	-t "Unit reference for Free Pascal" units.htex
	-sed -f foot.sed <units/footnode.html >units/footnote.html
	-mv units/footnote.html units/footnode.html
ifndef DEBUG
	-rm -f units/labels.pl units/internals.pl units/.*.pag units/.*.dir
	-rm -f units/images.* units/*.log units/WARNINGS
endif
endif
	touch units.chk

ref: ref.chk
ref.chk: refex.chk htex.chk includes
ifdef INSTALLDEBUG
	mkdir ref
	cp ref.tex ref/index.html
else
	cp -f preamble.html preamble.inc
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 4 -link 2\
	-t "Free Pascal reference guide" ref.htex
	-sed -f foot.sed <ref/footnode.html >ref/footnote.html
	-mv ref/footnote.html ref/footnode.html
ifndef DEBUG
	-rm -f ref/labels.pl ref/internals.pl ref/.*.pag ref/.*.dir
	-rm -f ref/images.* ref/*.log ref/WARNINGS
endif
endif
	touch ref.chk	

prog: prog.chk
prog.chk: progex.chk htex.chk includes
ifdef INSTALLDEBUG
	mkdir prog
	cp prog.tex prog/index.html
else
	cp -f preamble.html preamble.inc
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
	-t "Free Pascal programmers guide" prog.htex
	-sed -f foot.sed <prog/footnode.html >prog/footnote.html
	-mv prog/footnote.html prog/footnode.html
ifndef DEBUG
	-rm -f prog/labels.pl prog/internals.pl prog/.*.pag prog/.*.dir
	-rm -f prog/images.* prog/*.log prog/WARNINGS
endif
endif
	touch prog.chk

internal: internal.chk
internal.chk: htex.chk
ifdef INSTALLDEBUG
	mkdir internal
	cp internal.tex internal/index.html
else
	cp -f preamble.html preamble.inc
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 4 -link 2\
	-t "Free Pascal Compiler Documentation" internal.htex
	-sed -f foot.sed <internal/footnode.html >internal/footnote.html
	-mv internal/footnote.html internal/footnode.html
ifndef DEBUG
	-rm -f internal/labels.pl internal/internals.pl internal/.*.pag internal/.*.dir
	-rm -f internal/images.* internal/*.log internal/WARNINGS
endif
endif
	touch internal.chk	

onechap: onechap.chk
onechap.chk: unitex.chk htex.chk includes
ifdef INSTALLDEBUG
	mkdir onechap
	cp onechap.tex onechap/index.html
else
	cp -f preamble.html preamble.inc
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 4 -link 2\
	-t "Free Pascal reference guide" onechap.htex
	-sed -f foot.sed <onechap/footnode.html >onechap/footnote.html
	-mv onechap/footnote.html onechap/footnode.html
ifndef DEBUG
	-rm -f onechap/labels.pl onechap/internals.pl onechap/.*.pag onechap/.*.dir
	-rm -f onechap/images.* onechap/*.log onechap/WARNINGS
endif
endif
	touch onechap.chk	

html: $(HTML)

classes: classes/index.html

classes/index.html: classes.xml
	$(FPDOC) $(FPCLASSESOPT) --format=html

#####################################################################
# GTK Articles
#####################################################################

gtk: $(GTKPDF)

gtk1.pdf: gtk1.tex

gtk2.pdf: gtk2.tex

gtk3.pdf: gtk3.tex

gtk4.pdf: gtk4.tex

gtk5.pdf: gtk5.tex


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
	cp -R pics $(DOCINSTALLDIR)
	cp -R $(HTML) $(DOCINSTALLDIR)
	find $(DOCINSTALLDIR) -name 'CVS' | xargs -n1 rm -rf

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
	$(MAKE) htmlinstall DOCINSTALLDIR=$(DOCPREFIX)
	zip -D9rq doc-html $(DOCPREFIX)/*
	rm -rf $(DOCPREFIX)

htmzip:
	$(MAKE) htmlinstall DOCINSTALLDIR=$(DOCPREFIX)
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
	$(MAKE) -C objectex
	$(MAKE) -C sysutex
	$(MAKE) -C typinfex
	$(MAKE) -C kbdex

dosexamples: examples
	$(MAKE) -C go32ex
	$(MAKE) -C mouseex

linuxexamples: examples
	$(MAKE) -C linuxex
	$(MAKE) -C sockex
	$(MAKE) -C ipcex


#
# $Log$
# Revision 1.14  2002-04-23 18:03:50  michael
# + removed fcl target in HTML
#
# Revision 1.13  2002/03/19 14:33:18  pierre
#  * remove ipcex directory target from examples
#
# Revision 1.12  2002/03/12 22:13:42  michael
# + Now pics directory is included in HTML
#
# Revision 1.11  2001/11/05 20:10:05  michael
# + Added onechap target
#
# Revision 1.10  2001/10/18 21:41:26  peter
#   * fixed latex2html settings
#
# Revision 1.9  2001/10/11 23:36:54  carl
# + reinstated Windows NT support
#
# Revision 1.8  2001/10/04 21:03:13  michael
# + Added video and keyboard units
#
# Revision 1.7  2001/09/22 11:15:04  peter
#   * chmod the scripts
#   * fixed html installation
#
# Revision 1.6  2001/07/29 20:28:00  peter
#   * move preamble to separate include file so changes only need to
#     be done to this file.
#
# Revision 1.5  2001/07/10 22:01:15  michael
# + Some cleaning up
#
# Revision 1.4  2000/10/12 10:41:58  peter
#   * more examples
#
# Revision 1.3  2000/09/02 21:51:36  peter
#   * fixed pdf creating (merged)
#
# Revision 1.1  2000/07/13 09:10:03  michael
# + Initial import
#
