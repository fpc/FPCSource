#######################################################################
#
# Makefile to compile all examples and convert them to PDF/PS
# 
#######################################################################


# Script to convert the programs to LaTeX examples which can be included.
FIG2DEV=fig2dev -L

#######################################################################
# No need to edit after this line.
#######################################################################

.SUFFIXES: .png .pdf .fig .eps .ps

.PHONY: all tex clean pdfdoc dvidoc psdoc pdfdist dvidist psdist

SOURCEOBJECTS=arch1 arch2 arch3 arch4 arch5 arch6 arch7 arch8 arch9 arch10 arch11

FIGSOURCE=$(addsuffix .fig, $(SOURCEOBJECTS))

PDFOBJECTS=$(addsuffix .pdf, $(SOURCEOBJECTS))

PNGOBJECTS=$(addsuffix .png, $(SOURCEOBJECTS))

EPSOBJECTS=$(addsuffix .eps, $(SOURCEOBJECTS))


all : pdfdoc
#dvidoc psdoc

dist: pdfdist 
#dvidist psdist

# 4 times latex because of longtable. Theoretically it can even be higher, but we
# don't use multicol.

pdfdoc: comparch.pdf

comparch.pdf: $(PDFOBJECTS) comparch.tex
	pdflatex comparch
	pdflatex comparch
	pdflatex comparch
	pdflatex comparch

dvidoc: comparch.dvi

comparch.dvi: $(EPSOBJECTS) comparch.tex
	latex comparch
	latex comparch
	latex comparch
	latex comparch

psdoc: comparch.ps

comparch.ps: comparch.dvi
	dvips comparch

#######################################################################
# zip targets.

dvidist: arch-dvi.zip

arch-dvi.zip: comparch.dvi
	zip arch-dvi comparch.dvi

pdfdist: arch-pdf.zip

arch-pdf.zip: comparch.pdf
	zip arch-pdf comparch.pdf

psdist: arch-ps.zip

arch-ps.zip: comparch.ps
	zip arch-ps comparch.ps

clean : 
	rm -f $(PDFOBJECTS) $(PNGOBJECTS) $(EPSOBJECTS) *.aux *.idx *.toc *.lof *.log *.out comparch.pdf comparch.ps comparch.dvi
	
distclean: clean
	rm -f arch-dvi.zip arch-pdf.zip arch-ps.zip     
 
$(PDFOBJECTS): %.pdf: %.fig
	$(FIG2DEV) pdf $*.fig $*.pdf 

$(EPSOBJECTS): %.eps: %.fig
	$(FIG2DEV) eps $*.fig $*.eps

# Could be handy for latex2html:

$(PNGOBJECTS): %.png: %.fig
	$(FIG2DEV) png $*.fig $*.png 
