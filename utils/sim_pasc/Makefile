#	This file is part of the software similarity tester SIM.
#	Written by Dick Grune, Vrije Universiteit, Amsterdam.
#	$Id: Makefile,v 2.17 2007/08/27 09:57:31 dick Exp $
#


#	E N T R Y   P O I N T S
test_sim:

help:
	@echo 'Entry points:'
	@echo 'test_sim:	compile sim_c and run a simple test (default)'
	@echo ''
	@echo 'all:		create all binaries'
	@echo 'sim_X(.exe):	create specific binary for language X'
	@echo 'install_all:	install all binaries'
	@echo 'install.sim_X:	install specific binary for language X'
	@echo 'where X is one of c, java, pasc, m2, lisp, mira, text'
	@echo ''
	@echo 'lint:		lint sim_c sources'
	@echo 'lint.all:	lint all sim sources'
	@echo 'simsim:		run sim on the sim sources'
	@echo ''
	@echo 'simsrc.shr:	create sources shar file'
	@echo 'simsrc.zip:	create sources zip file'
	@echo 'depend:		update dependencies in Makefile'
	@echo 'clean:		remove created files'
	@echo ''
	@echo 'sim_exes:	create DOS executables in MSDOS; set date; make clean'
	@echo 'simexe.zip:	create DOS executables package in UNIX'
	@echo 'VERSION=2.X install_ftp:	install in the FTP directory in UNIX'

VERSION =	2_21

#
# When you modify any of the following flags, do 'make clean'
#

include		sysidf.mk


# Flags
OPTLEVEL =	-O4#			#

CFLAGS =	$(SYSTEM) $(OPTLEVEL) $(TESTTOKEN)
LFLAGS =	#			# loader flags

TESTTOKEN =	#-DTESTTOKEN#		# define to test the token type


#	T E S T   P A R A M E T E R S

# Parameters for two simple test runs, sim.res and stream.res:
TEST_LANG =	c#			# to test sim_X for language X
TEST_OPT =	-f -r 20#		# options to sim_X
TEST_INP =	pass3.c#		# guinea pig input

TEST_OPT =	-p#			# options to sim_X
TEST_INP =	*.l#			# guinea pig input
TEST_INP =	simple*#		# guinea pig input


#	I N T R O D U C T I O N

#	Each module (set of programs that together perform some function)
#	has the following sets of files defined for it:
#		_FLS	all files of that module, for, e.g.,
#			sharring, inventory, etc.
#		_SRC	the source files, from which other files derive
#		_CFS	the C-files, from which the object files derive
#		_OBJ	object files
#		_GRB	garbage files produced by compiling the module
#
#	(This is a feeble attempt at software-engineering a Makefile.)
#

test_sim:	sim.res stream.res	# two simple tests


#	B I N A R I E S

BINARIES =	sim_c$(EXE) sim_java$(EXE) sim_pasc$(EXE) \
		sim_m2$(EXE) sim_lisp$(EXE) sim_mira$(EXE) \
		sim_text$(EXE)

all:		$(BINARIES)


#	C O M P I L A T I O N   R U L E S

.SUFFIXES:	.o
.c.o:
		$(CC) -c $(CFLAGS) $<


#	A U X I L I A R Y   M O D U L E S

# Common modules:
COM_CFS =	token.c lex.c stream.c text.c tokenarray.c error.c
COM_OBJ =	token.o lex.o stream.o text.o tokenarray.o error.o
COM_SRC =	token.h lex.h stream.h text.h tokenarray.h error.h \
		lang.h language.h \
		sortlist.spc sortlist.bdy system.par $(COM_CFS)
COM_FLS =	$(COM_SRC)

# The idf module:
IDF_CFS =	idf.c
IDF_OBJ =	idf.o
IDF_SRC =	idf.h $(IDF_CFS)
IDF_FLS =	$(IDF_SRC)

# The runs package:
RUNS_CFS =	runs.c percentages.c
RUNS_OBJ =	runs.o percentages.o
RUNS_SRC =	runs.h percentages.h $(RUNS_CFS)
RUNS_FLS =	$(RUNS_SRC) aiso.spc aiso.bdy

# The main program:
MAIN_CFS =	sim.c options.c pass1.c hash.c compare.c add_run.c \
		pass2.c pass3.c
MAIN_OBJ =	sim.o options.o pass1.o hash.o compare.o add_run.o \
		pass2.o pass3.o
MAIN_SRC =	sim.h options.h pass1.h hash.h compare.h add_run.h \
		pass2.h pass3.h \
		debug.par settings.par $(MAIN_CFS)
MAIN_FLS =	$(MAIN_SRC)

# The similarity tester without the language part:
SIM_CFS =	$(COM_CFS) $(IDF_CFS) $(RUNS_CFS) $(MAIN_CFS)
SIM_OBJ =	$(COM_OBJ) $(IDF_OBJ) $(RUNS_OBJ) $(MAIN_OBJ)
SIM_SRC =	$(COM_SRC) $(IDF_SRC) $(RUNS_SRC) $(MAIN_SRC)
SIM_FLS =	$(COM_FLS) $(IDF_FLS) $(RUNS_FLS) $(MAIN_FLS)


#	L A N G U A G E S

# The algollike module:
ALG_CFS =	algollike.c
ALG_OBJ =	algollike.o
ALG_SRC =	algollike.h $(ALG_CFS)
ALG_FLS =	$(ALG_SRC)

# The C Language module:					C
CLANG_CFS =	clang.c
CLANG_OBJ =	clang.o
CLANG_SRC =	clang.l
CLANG_FLS =	$(CLANG_SRC)

clang.c:	clang.l
		$(LEX) -t clang.l >$@

SIM_C_CFS =	$(SIM_CFS) $(ALG_CFS) $(CLANG_CFS)
SIM_C_OBJ =	$(SIM_OBJ) $(ALG_OBJ) $(CLANG_OBJ)

sim_c$(EXE):	$(SIM_C_OBJ)
		$(CC) $(LFLAGS) $(SIM_C_OBJ) -o $@

SIM_C_GRB =	clang.c sim_c

install.sim_c:	$(BINDIR)/sim_c$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_c$(EXE):	sim_c$(EXE)
		$(COPY) sim_c$(EXE) $@

# The Java Language module:					Java
JAVALANG_CFS =	javalang.c
JAVALANG_OBJ =	javalang.o
JAVALANG_SRC =	javalang.l
JAVALANG_FLS =	$(JAVALANG_SRC)

javalang.c:	javalang.l
		$(LEX) -t javalang.l >$@

SIM_JAVA_CFS =	$(SIM_CFS) $(ALG_CFS) $(JAVALANG_CFS)
SIM_JAVA_OBJ =	$(SIM_OBJ) $(ALG_OBJ) $(JAVALANG_OBJ)

sim_java$(EXE):	$(SIM_JAVA_OBJ)
		$(CC) $(LFLAGS) $(SIM_JAVA_OBJ) -o $@

SIM_JAVA_GRB =	javalang.c sim_java

install.sim_java:	$(BINDIR)/sim_java$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_java$(EXE):	sim_java$(EXE)
		$(COPY) sim_java$(EXE) $@

# The Pascal Language module:					Pascal
PASCLANG_CFS =	pascallang.c
PASCLANG_OBJ =	pascallang.o
PASCLANG_SRC =	pascallang.l
PASCLANG_FLS =	$(PASCLANG_SRC)

pascallang.c:	pascallang.l
		$(LEX) -t pascallang.l >pascallang.c

SIM_PASC_CFS =	$(SIM_CFS) $(ALG_CFS) $(PASCLANG_CFS)
SIM_PASC_OBJ =	$(SIM_OBJ) $(ALG_OBJ) $(PASCLANG_OBJ)

sim_pasc$(EXE):	$(SIM_PASC_OBJ)
		$(CC) $(LFLAGS) $(SIM_PASC_OBJ) -o $@

SIM_PASC_GRB =	pascallang.c sim_pasc

install.sim_pasc:	$(BINDIR)/sim_pasc$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_pasc$(EXE):	sim_pasc$(EXE)
		$(COPY) sim_pasc$(EXE) $@

# The Modula-2 Language module:					Modula-2
M2LANG_CFS =	m2lang.c
M2LANG_OBJ =	m2lang.o
M2LANG_SRC =	m2lang.l
M2LANG_FLS =	$(M2LANG_SRC)

m2lang.c:	m2lang.l
		$(LEX) -t m2lang.l >$@

SIM_M2_CFS =	$(SIM_CFS) $(ALG_CFS) $(M2LANG_CFS)
SIM_M2_OBJ =	$(SIM_OBJ) $(ALG_OBJ) $(M2LANG_OBJ)

sim_m2$(EXE):	$(SIM_M2_OBJ)
		$(CC) $(LFLAGS) $(SIM_M2_OBJ) -o $@

SIM_M2_GRB =	m2lang.c sim_m2

install.sim_m2:	$(BINDIR)/sim_m2$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_m2$(EXE):	sim_m2$(EXE)
		$(COPY) sim_m2$(EXE) $@

# The Lisp Language module:					Lisp
LISPLANG_CFS =	lisplang.c
LISPLANG_OBJ =	lisplang.o
LISPLANG_SRC =	lisplang.l
LISPLANG_FLS =	$(LISPLANG_SRC)

lisplang.c:	lisplang.l
		$(LEX) -t lisplang.l >$@

SIM_LISP_CFS =	$(SIM_CFS) $(ALG_CFS) $(LISPLANG_CFS)
SIM_LISP_OBJ =	$(SIM_OBJ) $(ALG_OBJ) $(LISPLANG_OBJ)

sim_lisp$(EXE):	$(SIM_LISP_OBJ)
		$(CC) $(LFLAGS) $(SIM_LISP_OBJ) -o $@

SIM_LISP_GRB =	lisplang.c sim_lisp

install.sim_lisp:	$(BINDIR)/sim_lisp$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_lisp$(EXE):	sim_lisp$(EXE)
		$(COPY) sim_lisp$(EXE) $@

# The Miranda Language module:					Miranda
MIRALANG_CFS =	miralang.c
MIRALANG_OBJ =	miralang.o
MIRALANG_SRC =	miralang.l
MIRALANG_FLS =	$(MIRALANG_SRC)

miralang.c:	miralang.l
		$(LEX) -t miralang.l >$@

SIM_MIRA_CFS =	$(SIM_CFS) $(ALG_CFS) $(MIRALANG_CFS)
SIM_MIRA_OBJ =	$(SIM_OBJ) $(ALG_OBJ) $(MIRALANG_OBJ)

sim_mira$(EXE):	$(SIM_MIRA_OBJ)
		$(CC) $(LFLAGS) $(SIM_MIRA_OBJ) -o $@

SIM_MIRA_GRB =	miralang.c sim_mira

install.sim_mira:	$(BINDIR)/sim_mira$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_mira$(EXE):	sim_mira$(EXE)
		$(COPY) sim_mira$(EXE) $@

# The Text module:						Text
TEXTLANG_CFS =	textlang.c
TEXTLANG_OBJ =	textlang.o
TEXTLANG_SRC =	textlang.l
TEXTLANG_FLS =	$(TEXTLANG_SRC)

textlang.c:	textlang.l
		$(LEX) -t textlang.l >$@

SIM_TEXT_CFS =	$(SIM_CFS) $(TEXTLANG_CFS)
SIM_TEXT_OBJ =	$(SIM_OBJ) $(TEXTLANG_OBJ)

sim_text$(EXE):	$(SIM_TEXT_OBJ)
		$(CC) $(LFLAGS) $(SIM_TEXT_OBJ) -o $@

SIM_TEXT_GRB =	textlang.c sim_text

install.sim_text:	$(BINDIR)/sim_text$(EXE) $(MANDIR)/sim.1

$(BINDIR)/sim_text$(EXE):	sim_text$(EXE)
		$(COPY) sim_text$(EXE) $@


#	T E S T S

# Some simple tests:
sim.res:	sim_$(TEST_LANG)$(EXE) $(TEST_INP)
		./sim_$(TEST_LANG)$(EXE) $(TEST_OPT) $(TEST_INP)
#		./sim_$(TEST_LANG)$(EXE) -x $(TEST_OPT) $(TEST_INP)

stream.res:	sim_$(TEST_LANG)$(EXE) $(TEST_INP)
		./sim_$(TEST_LANG)$(EXE) -- $(TEST_INP) >stream.res
		wc stream.res $(TEST_INP)

TEST_GRB =	stream.res

# More simple tests, on the C version only:
simsim:		sim_c$(EXE) $(SRC)
		./sim_c$(EXE) -fr 20 $(SRC)

# Lint
lint:		$(SIM_C_CFS)
		$(LINT) $(LINTFLAGS) $(SIM_C_CFS) | grep -v yy

lint.all:	$(SIM_C_CFS) $(SIM_JAVA_CFS) $(SIM_PASC_CFS) $(SIM_M2_CFS) \
		$(SIM_LISP_CFS) $(SIM_MIRA_CFS) $(SIM_TEXT_CFS)
		$(LINT) $(LINTFLAGS) $(SIM_C_CFS) | grep -v yy
		$(LINT) $(LINTFLAGS) $(SIM_JAVA_CFS) | grep -v yy
		$(LINT) $(LINTFLAGS) $(SIM_PASC_CFS) | grep -v yy
		$(LINT) $(LINTFLAGS) $(SIM_M2_CFS) | grep -v yy
		$(LINT) $(LINTFLAGS) $(SIM_LISP_CFS) | grep -v yy
		$(LINT) $(LINTFLAGS) $(SIM_MIRA_CFS) | grep -v yy
		$(LINT) $(LINTFLAGS) $(SIM_TEXT_CFS) | grep -v yy


#	O T H E R   E N T R I E S

# Sets of files: general, modules, main programs, languages
CFS =		$(SIM_CFS) $(ALG_CFS) \
		$(CLANG_CFS) $(JAVALANG_CFS) $(PASCLANG_CFS) $(M2LANG_CFS) \
		$(LISPLANG_CFS) $(MIRALANG_CFS) $(TEXTLANG_CFS)
OBJ =		$(SIM_OBJ) $(ALG_OBJ) \
		$(CLANG_OBJ) $(JAVALANG_OBJ) $(PASCLANG_OBJ) $(M2LANG_OBJ) \
		$(LISPLANG_OBJ) $(MIRALANG_OBJ) $(TEXTLANG_OBJ)
SRC =		$(SIM_SRC) $(ALG_SRC) \
		$(CLANG_SRC) $(JAVALANG_SRC) $(PASCLANG_SRC) $(M2LANG_SRC) \
		$(LISPLANG_SRC) $(MIRALANG_SRC) $(TEXTLANG_SRC)
FLS =		$(SIM_FLS) $(ALG_FLS) \
		$(CLANG_FLS) $(JAVALANG_FLS) $(PASCLANG_FLS) $(M2LANG_FLS) \
		$(LISPLANG_FLS) $(MIRALANG_FLS) $(TEXTLANG_FLS) \
		sysidf.mk sysidf.msdos sysidf.unix
DOC =		READ_ME READ.ME README.1st sim.1 sim.txt sim.html \
		ChangeLog Answers TechnReport

ALL_FLS =	Makefile $(FLS) $(DOC)

# Create .EXE archive for MSDOS
SIM_EXES =	sim_c.exe sim_java.exe sim_pasc.exe sim_m2.exe \
		sim_lisp.exe sim_mira.exe sim_text.exe
DOSZIP =	READ.ME sim.txt $(SIM_EXES)
sim_exes:	$(SIM_EXES)

simexe.zip:	$(DOSZIP)
		$(ZIP) $@ $(DOSZIP)

DOS_GRB =	simexe.zip

# Install and clean scripts
install_all:	install			# just a synonym
install:	install.sim_c install.sim_java install.sim_pasc \
		install.sim_m2 install.sim_lisp install.sim_mira \
		install.sim_text

$(MANDIR)/sim.1:	sim.1
		$(COPY) sim.1 $@

FTPFILES =	README.1st READ_ME LICENSE.txt TechnReport

install_ftp:	$(FTPFILES) simsrc.shr simexe.zip sim.pdf
		cp -p simsrc.shr sim_$(VERSION).shar
		cp -p simexe.zip sim_$(VERSION).zip
		cp -p $(FTPFILES) sim_$(VERSION).shar sim_$(VERSION).zip \
			README.1st READ.ME READ_ME sim.pdf \
			$(FTPDIR)/.
		rm -f sim_$(VERSION).shar sim_$(VERSION).zip
		ls -l $(FTPDIR)/.

simsrc.shr:	$(ALL_FLS)
		shar $(ALL_FLS) >$@

simsrc.zip:	$(ALL_FLS)
		$(ZIP) $@ $(ALL_FLS)

sim.txt:	sim.1
		nroff -man sim.1 | sed 's/.//g' >$@

sim.pdf:	sim.1
		troff -man sim.1 | devps | ps2pdf -sPAPERSIZE=a4 - $@

INSTALL_GRB =	simsrc.shr simsrc.zip sim.txt sim.pdf

depend:		$(CFS)
		makedepend -w 1 -Dlint $(CFS)

.PHONY:		clean fresh
clean:
		-rm -f *.o
		-rm -f $(SIM_C_GRB)
		-rm -f $(SIM_JAVA_GRB)
		-rm -f $(SIM_PASC_GRB)
		-rm -f $(SIM_M2_GRB)
		-rm -f $(SIM_LISP_GRB)
		-rm -f $(SIM_MIRA_GRB)
		-rm -f $(SIM_TEXT_GRB)
		-rm -f $(TEST_GRB)
		-rm -f $(INSTALL_GRB)
		-rm -f a.out a.exe sim.txt core mon.out

fresh:		clean
		-rm -f $(DOS_GRB)
		-rm -f *.exe

#	D E P E N D E N C I E S

# DO NOT DELETE THIS LINE -- make depend depends on it.

token.o: token.h
lex.o: token.h
lex.o: lex.h
stream.o: system.par
stream.o: token.h
stream.o: lex.h
stream.o: lang.h
stream.o: stream.h
text.o: debug.par
text.o: sim.h
text.o: token.h
text.o: stream.h
text.o: lex.h
text.o: options.h
text.o: error.h
text.o: text.h
tokenarray.o: error.h
tokenarray.o: lex.h
tokenarray.o: token.h
tokenarray.o: tokenarray.h
error.o: sim.h
error.o: error.h
idf.o: system.par
idf.o: token.h
idf.o: idf.h
runs.o: sim.h
runs.o: runs.h
runs.o: aiso.spc
runs.o: aiso.bdy
percentages.o: sim.h
percentages.o: runs.h
percentages.o: aiso.spc
percentages.o: error.h
percentages.o: percentages.h
percentages.o: sortlist.bdy
sim.o: settings.par
sim.o: sim.h
sim.o: options.h
sim.o: language.h
sim.o: token.h
sim.o: error.h
sim.o: hash.h
sim.o: compare.h
sim.o: pass1.h
sim.o: pass2.h
sim.o: pass3.h
sim.o: stream.h
sim.o: lex.h
options.o: options.h
pass1.o: debug.par
pass1.o: sim.h
pass1.o: text.h
pass1.o: tokenarray.h
pass1.o: token.h
pass1.o: lex.h
pass1.o: error.h
pass1.o: pass1.h
hash.o: system.par
hash.o: debug.par
hash.o: sim.h
hash.o: error.h
hash.o: language.h
hash.o: token.h
hash.o: tokenarray.h
hash.o: options.h
hash.o: hash.h
compare.o: sim.h
compare.o: tokenarray.h
compare.o: token.h
compare.o: hash.h
compare.o: language.h
compare.o: options.h
compare.o: add_run.h
compare.o: compare.h
add_run.o: sim.h
add_run.o: runs.h
add_run.o: aiso.spc
add_run.o: percentages.h
add_run.o: options.h
add_run.o: error.h
add_run.o: add_run.h
pass2.o: debug.par
pass2.o: sim.h
pass2.o: text.h
pass2.o: lex.h
pass2.o: token.h
pass2.o: pass2.h
pass2.o: sortlist.bdy
pass3.o: system.par
pass3.o: debug.par
pass3.o: sim.h
pass3.o: runs.h
pass3.o: aiso.spc
pass3.o: error.h
pass3.o: options.h
pass3.o: pass3.h
pass3.o: percentages.h
pass3.o: tokenarray.h
pass3.o: token.h
algollike.o: options.h
algollike.o: token.h
algollike.o: algollike.h
algollike.o: language.h
clang.o: options.h
clang.o: algollike.h
clang.o: language.h
clang.o: token.h
clang.o: idf.h
clang.o: lex.h
clang.o: lang.h
javalang.o: options.h
javalang.o: algollike.h
javalang.o: language.h
javalang.o: token.h
javalang.o: idf.h
javalang.o: lex.h
javalang.o: lang.h
pascallang.o: options.h
pascallang.o: algollike.h
pascallang.o: language.h
pascallang.o: token.h
pascallang.o: idf.h
pascallang.o: lex.h
pascallang.o: lang.h
m2lang.o: options.h
m2lang.o: algollike.h
m2lang.o: language.h
m2lang.o: token.h
m2lang.o: idf.h
m2lang.o: lex.h
m2lang.o: lang.h
lisplang.o: language.h
lisplang.o: token.h
lisplang.o: lex.h
lisplang.o: lang.h
lisplang.o: idf.h
miralang.o: language.h
miralang.o: token.h
miralang.o: lex.h
miralang.o: lang.h
miralang.o: idf.h
textlang.o: language.h
textlang.o: token.h
textlang.o: idf.h
textlang.o: lex.h
textlang.o: lang.h
