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
HTML = user units ref prog fpdoc fcl

# Can be 'report','book', 'html', 'hevea', 'ts3' 'ts4'
ifdef USEHEVEA
PREAMBLETYPE = hevea
else
ifdef USEL2H 
PREAMBLETYPE = html
else
PREAMBLETYPE = report
endif
endif

# Where do you want the html files to be put ?
ifndef PREFIXINSTALLDIR
PREFIXINSTALLDIR=/usr
endif

ifndef DOCINSTALLDIR
DOCINSTALLDIR:=$(PREFIXINSTALLDIR)/doc/fpc-$(shell ppc386 -iV)
endif

ifndef FPDOC
FPDOC=fpdoc
endif

ifndef FPCSRCDIR
FPCSRCDIR=..
endif

# Use Ascii mode ? (i.e. no pics in produced HTML ?)
ifndef ASCIIMODE
ASCIIMODE=NO
endif


#######################################################################
# LaTeX Program definitions
#######################################################################
# Latex itself
ifndef LATEX
LATEX = latex
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

#######################################################################
# Latex2html conversion.
#######################################################################
ifdef USEL2H
# Command to use.
ifndef LATEX2HTML
LATEX2HTML = /usr/bin/latex2html
endif
# Default options for converter
ifndef LATEX2HTMLOPTS
LATEX2HTMLOPTS = -no_reuse -up_url "../fpctoc.html"\
                 -up_title "Free Pascal manuals"\
                 -html_version 4.0 -split 3
endif
endif # USEL2H
#######################################################################
# HEVEA conversion.
#######################################################################
ifdef USEHEVEA
# Hevea conversion.
ifndef HEVEA
HEVEA = /usr/bin/hevea
endif
endif # USEHEVEA

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


GTKPDF=$(addsuffix .pdf,$(addprefix gtk, 1 2 3 4 5))

# Check if ascii-mode requested
ifeq ($(ASCIIMODE),YES)
ifdef USEL2H
LATEX2HTMLOPTS+=-ascii_mode
endif
ifdef USEHEVEA
HEVEAOPTS+=-text
endif
endif


#####################################################################
# Conversion from types
#####################################################################

.PHONY: clean dvi help html ps psdist htmldist pdfdist txtdist \
        htm txt pdf refex alldist messages onechap gtk \
        user ref prog units

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
	@echo ' html          : Make HTML documentation using default converter.'
	@echo ' hevea         : Make HTML documentation using hevea'
	@echo ' l2h           : Make HTML documentation using latex2html'
	@echo ' 4ht           : Make HTML documentation using tex4ht'
	@echo ' pdf           : Make documentation using pdflatex'
	@echo ' txt           : dvi, convert to text using dvi2tty'
	@echo ' htm           : Convert .html to .htm files, zip result'
	@echo ' clean         : Clean up the mess.'
	@echo ' examples      : Comple all generic examples'
	@echo ' linuxexamples : Compile all examples for linux'
	@echo ' dosexamples   : Compile all examples for dos'
	@echo ' execute       : Execute examples (DOS/Windows/OS/2 only)'
	@echo ' htmldist      : html, and rchive result.'
	@echo ' psdist        : ps, and archive result.'
	@echo ' pdfdist       : pdf, and archive result.'

clean:
	-rm -f preamble.inc date.inc htex.sed
	-rm -f *.htex *.hind *.haux *.htoc $(addsuffix *.html,$(HTML))
	-rm -f *.4tc *.4ct *.css *.lg *.tmp *.xref
	-rm -rf $(HTML) onechap
	-rm -f $(TOC) $(LOG) $(DVI) $(PDF) $(AUX) $(OUT) $(PS) *.i* $(LOT) $(TXT) $(CHK)
	-rm -f onechap.dvi onechap.pdf onechap.ps onechap.out onechap.log onechap.aux
# Styles
	-rm -f $(notdir $(wildcard styles/*))
# Units
	-$(MAKE) -C crtex clean
	-$(MAKE) -C dosex clean
	-$(MAKE) -C go32ex clean
	-$(MAKE) -C heapex clean
	-$(MAKE) -C ipcex clean
	-$(MAKE) -C linuxex clean
	-$(MAKE) -C mmouseex clean
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
	-$(MAKE) -C videoex clean
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

# Local copy of the required styles
syntax.sty: styles/syntax.sty
	cp $(wildcard styles/*.*) .

ifndef inWinNT
includes: date.inc preamble.inc syntax.sty scripts.chk
else
includes: date.inc preamble.inc syntax.sty
endif


#####################################################################
# Tex from XML
#####################################################################
FCLOPTS=--package=fcl --descr=classes.xml --input='$(FPCSRCDIR)/fcl/linux/classes.pp -Fi$(FPCSRCDIR)/fcl/inc'

fcl.inc: classes.xml
	$(FPDOC) --output=fcl.inc $(FCLOPTS) --format=latex

#####################################################################
# DVI files
#####################################################################

onechap: onechap.dvi

units.dvi: units.tex includes $(CHAPTERS)

ref.dvi: ref.tex includes

prog.dvi: prog.tex includes

user.dvi: user.tex includes

fpdoc.dvi: fpdoc.tex includes

fcl.dvi: fcl.tex fcl.inc includes

units.pdf: units.tex includes $(CHAPTERS)

ref.pdf: ref.tex includes

user.pdf: user.tex includes

prog.pdf: prog.tex includes

onechap.pdf: onechap.tex includes

onechap.dvi: onechap.tex includes

fpdoc.pdf: fpdoc.tex includes

fcl.pdf: fcl.tex fcl.inc includes

dvi : $(DVI)

txt : dvi $(TXT)

ps : dvi $(PS)

pdf : $(PDF)

all : dvi ps pdf txt html


#######################################################################
# HTML creation
#######################################################################

.PHONY: htex user prog units onechap ref internal html hevea

ifdef INSTALLDEBUG
#######################################################################
# Installation debugging
$(HTML):
	mkdir $@
	cp $@.tex $@/index.html
html: $(HTML)
else
#######################################################################
# Automatic selection. Default tex4ht
ifdef USEHEVEA
include Makefile.hev
else
ifdef USEL2H
include Makefile.l2h
else
include Makefile.4ht
endif  # USEL2H
endif  # USEHEVEA
endif  # INSTALLDEBUG
#######################################################################
# Targets to force using a specific tool. 
hevea:
	$(MAKE) html USEHEVEA=1
l2h:
	$(MAKE) html USEL2H=1
4ht:
	$(MAKE) html USETEX4HT=1

fcl: fcl/index.html

fcl/index.html: classes.xml
	$(FPDOC) $(FCLOPTS) --format=html --output=fcl

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
	find $(DOCINSTALLDIR) -name 'CVS' -or -name '*.eps' | xargs -n1 rm -rf

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
ifndef SKIPINSTALL
	$(MAKE) htmlinstall DOCINSTALLDIR=$(DOCPREFIX)
endif
	cd $(DOCPREFIX) ; tar cz * > $(BASEDIR)/fpcdoc.tar.gz ; cd $(BASEDIR)
	rm -rf $(DOCPREFIX)

htmlzip:
ifndef SKIPINSTALL
	$(MAKE) htmlinstall DOCINSTALLDIR=$(DOCPREFIX)
endif
	zip -D9rq doc-html $(DOCPREFIX)/*
	rm -rf $(DOCPREFIX)

htmzip:
ifndef SKIPINSTALL
	$(MAKE) htmlinstall DOCINSTALLDIR=$(DOCPREFIX)
endif
	makehtm `find $(DOCPREFIX) -name '*.html'`
	zip -D9rq doc-htm `find $(DOCPREFIX) -name '*.htm' -or -name '*.gif' -or -name '*.png' -or -name '*.css'`
	rm -rf $(DOCPREFIX)

htmldist: htmlzip htmltar

htmdist: htmzip

alldist: dvidist psdist txtdist pdfdist 
	$(MAKE) htmltar
	$(MAKE) htmlzip SKIPINSTALL=Y
	$(MAKE) htmzip SKIPINSTALL=Y

distclean: clean
	-rm -f *.tar.gz *.zip

#####################################################################
# Examples
#####################################################################

examples:
	$(MAKE) -C crtex
	$(MAKE) -C dosex
	$(MAKE) -C optex
	$(MAKE) -C mathex
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
	
execute:
	$(MAKE) -C dosex all
	$(MAKE) -C dosex execute
	$(MAKE) -C refex all
	$(MAKE) -C refex execute
	$(MAKE) -C mathex all
	$(MAKE) -C mathex execute
	$(MAKE) -C stringex all
	$(MAKE) -C stringex execute
	$(MAKE) -C objectex all
	$(MAKE) -C objectex execute
	$(MAKE) -C sysutex all
	$(MAKE) -C sysutex execute
	$(MAKE) -C typinfex all
	$(MAKE) -C typinfex execute


#
# $Log$
# Revision 1.22  2003-05-29 10:25:51  peter
#   * allow to specify FPCSRCDIR and FPDOC
#
# Revision 1.21  2003/03/18 21:25:23  michael
# + Added fcl docs and fpdoc
#
# Revision 1.20  2003/03/18 00:14:51  michael
# + Added fpdoc
#
# Revision 1.19  2003/03/17 08:27:27  michael
# + Split out html generation
#
# Revision 1.18  2003/03/16 15:24:58  peter
#   * also remove htex.sed
#
# Revision 1.17  2003/03/16 15:22:18  peter
#   * support for hevea
#   * provided local copies or required styles since debian does not
#     supply the listings.sty anymore
#
# Revision 1.16  2003/02/24 23:37:52  michael
# + Preparations for version 1.0.8
#
# Revision 1.15  2002/11/10 12:22:10  carl
#   * update makefiles for testing
#   * update example which did not compile
#
# Revision 1.14  2002/04/23 18:03:50  michael
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
