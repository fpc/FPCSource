#	I N S T A L L A T I O N   P A R A M E T E R S

BINDIR =	/home/dick/bin.`arch`
MANDIR =	/home/dick/man/man1
FTPDIR =	/usr/local/ftpd/pub/dick/similarity_tester

#	C O M P I L A T I O N   P A R A M E T E R S

EXE =		#
CC =		gcc -pedantic -Wall
LEX =		flex
COPY =		cp -p
ZIP =		zip -o
LINT =		lint -ansi $(SYSTEM)
LINTFLAGS =	-xh

SYSTEM =	-DUNIX
