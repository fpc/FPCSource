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

.SUFFIXES: .png .pdf .fig .eps

.PHONY: all tex clean

SOURCEOBJECTS=arch1 arch2 arch3 arch4 arch5 arch6 arch7 arch8 arch9 arch10 arch11

FIGSOURCE=$(addsuffix .fig, $(SOURCEOBJECTS))

PDFOBJECTS=$(addsuffix .pdf, $(SOURCEOBJECTS))

PNGOBJECTS=$(addsuffix .png, $(SOURCEOBJECTS))

PSOBJECTS=$(addsuffix .eps, $(SOURCEOBJECTS))

pdf : $(PDFOBJECTS)

png : $(PNGOBJECTS)

eps  : $(PSOBJECTS)

all : pdf png eps

clean : 
	rm -f $(PDFOBJECTS) $(PNGOBJECTS) $(PSOBJECTS)
 
$(PDFOBJECTS): %.pdf: %.fig
	$(FIG2DEV) pdf $*.fig $*.pdf 

$(PNGOBJECTS): %.png: %.fig
	$(FIG2DEV) png $*.fig $*.png 

$(PSOBJECTS): %.eps: %.fig
	$(FIG2DEV) eps $*.fig $*.eps

 
