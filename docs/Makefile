#######################################################################
# Makefile for FPK-Pascal documentation.
# Michael Van Canneyt
#######################################################################
#
# Where do you want the html files to be put ?
DOCINSTALLDIR=/usr/doc/ppc-0.99.0

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
LATEX2HTMLOPTS = -ascii_mode -no_reuse -up_url "../fpctoc.html"\
                 -up_title "Free Pascal manuals" -contents_in_navigation\
                 -html_version 3.0
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

.PHONY: clean dvi help html ps psdist htmldist htm txt pdf refex

.SUFFIXES: .dvi .tex .ps .txt .pdf

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

help:
	@echo 'Possible targets :'
	@echo ' dvi   : Make documentation using latex.'
	@echo ' ps    : Make documentation using latex and dvips.'
	@echo ' html  : Make documentation using latex2html.'
	@echo ' htm   : Convert .html to .htm files, zip result'
	@echo ' clean : Clean up the mess.'


$(TXT) : %.txt: %.dvi

$(DVI) : %.dvi: %.tex

$(PDF) : %.pdf: %.tex

units.dvi: units.tex $(CHAPTERS) unitex

unitex:
	$(MAKE) -C crtex tex
	$(MAKE) -C linuxex tex
	$(MAKE) -C dosex tex
	$(MAKE) -C optex tex
	$(MAKE) -C printex tex
	$(MAKE) -C stringex tex
	$(MAKE) -C sockex tex

ref.dvi: ref.tex refex

refex:
	$(MAKE) -C refex tex

dvi : $(DVI)

txt : dvi $(TXT)

html : dvi $(HTML) 

ps : dvi $(PS)

pdf : $(PDF)

user:
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
         -t "User's guide for FPK Pascal" user.tex
	sed -f foot.sed <user/footnode.html >user/footnote.html
	mv user/footnote.html user/footnode.html

units:
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 3 -link 2\
        -t "Unit reference for FPK Pascal" units.tex
	sed -f foot.sed <units/footnode.html >units/footnote.html
	mv units/footnote.html units/footnode.html

ref:
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 3 -link 2\
        -t "FPK Pascal reference guide" ref.tex
	sed -f foot.sed <ref/footnode.html >ref/footnote.html
	mv ref/footnote.html ref/footnode.html

prog:
	$(LATEX2HTML) $(LATEX2HTMLOPTS) -split 2 -link 2\
        -t "FPK Pascal programmers guide" prog.tex
	sed -f foot.sed <prog/footnode.html >prog/footnote.html
	mv prog/footnote.html prog/footnode.html

clean:
	-rm -f *.aux *.log *.dvi *.ps *.toc *.i* *.lot *.pdf *.txt
	-rm -rf $(HTML)
	-$(MAKE) -C refex clean
	-$(MAKE) -C linuxex clean
	-$(MAKE) -C crtex clean
	-$(MAKE) -C dosex clean
	-$(MAKE) -C optex clean
	-$(MAKE) -C printex clean
	-$(MAKE) -C stringex clean
	-$(MAKE) -C sockex clean

htmldist: html
	find . -name '*.html' >htmllist
	find . -name '*.gif' >>htmllist
	tar -czvf fpcdoc.tar.gz --files-from=htmllist
	rm -f htmllist

psdist: ps
	tar -cvzf fpcdocps.tar.gz *.ps
	zip fpcdocps *.ps

pdfdist: pdf
	zip fpcdocpdf *.pdf


htm:
	makehtm `find . -name '*.html'`
	zip fpcdoc `find . -name '*.htm'`
	rm `find -name '*.htm'`

dist:
	-mkdir $(DISTDIR)/docs
	cp Makefile README.DOCS makehtm fpktoc.html $(DISTDIR)/docs
	cp *.tex *.sty *.perl *.doc $(DISTDIR)/docs

distclean: clean
	-rm -f *.tar.gz *.zip

install: html
	install -d -m 755  $(DOCINSTALLDIR)
	cp -R $(HTML) $(DOCINSTALLDIR)
	cp fpctoc.html $(DOCINSTALLDIR)

www-install: psdist htmldist htm
	rcp fpcdoc.zip fpcdocps.zip fpcdocps.tar.gz fpcdoc.tar.gz tfdec1:htdocs/fpk
	rsh tfdec1 '(cd htdocs/fpk/docs ; /usr/local/bin/tar -xzf ../fpcdoc.tar.gz)'

all-formats: ps txt pdf html
