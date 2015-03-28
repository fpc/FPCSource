
About this Package
===== ==== =======

This is Version 4.1 of TPLY (Turbo Pascal Lex and Yacc), a compiler generator
for Turbo Pascal and compatibles. The package contains two programs, TP Lex
and Yacc, which are approximately compatible with the UNIX utilities Lex and
Yacc, but are written in, and produce code for the Turbo Pascal programming
language. The present version works with all recent flavours of Turbo/Borland
Pascal, including Delphi, and with the Free Pascal Compiler, a GPL'ed Turbo
Pascal-compatible compiler which currently runs on DOS and Linux (other ports
are under development). Recent information about TPLY and the sources are
available from the TPLY homepage:

	http://www.musikwissenschaft.uni-mainz.de/~ag/tply

For information about the Free Pascal Compiler, please refer to:

	http://www.freepascal.org/

The manual can be found in the files tply.tex (TeX version) and tply.doc
(ASCII version) contained in the package. An extended version of the manual
has also been published in the CCAI journal (A. Graef, TP Lex and Yacc: A
compiler generator toolset for Turbo Pascal, Journal of Communication and
Cognition - Artificial Intelligence (CCAI), 12(4), 1995, pp. 383-424).
Furthermore, there is one book I know of which devotes three chapters to TP
Lex/Yacc; unfortunately, it is written in French ;-) (Nino Silverio, Realiser
un compilateur: Les outil Lex et Yacc, Editions Eyrolles, France, 1994, ISBN
2-212-08834-5).


License
=======

Since version 4.0, TPLY and its derivatives are distributed under the GNU
General Public License (Version 2 or later); see the file COPYING for details.


Authors
=======

The original version of the TPLY package was written by Albert Graef
<ag@muwiinfa.geschichte.uni-mainz.de, Dr.Graef@t-online.de> for Turbo Pascal
4.0-6.0. Berend de Boer <berend@pobox.com>, the current maintainer of the
Turbo/Borland Pascal version, adapted TPLY to take advantage of the large
memory models in Borland Pascal 7.0 and Delphi. Michael Van Canneyt
<michael@freepascal.org>, who maintains the Linux version of
the Free Pascal compiler, is the author of the Free Pascal port.


History
=======

*** Version 2.0		Albert Graef <ag@muwiinfa.geschichte.uni-mainz.de>

Around 1990. First public release.

*** Version 3.0		Albert Graef <ag@muwiinfa.geschichte.uni-mainz.de>

1991. Lots of changes to make TPLY more compatible to UNIX Lex/Yacc. Moreover,
all DFA and LALR parser construction algorithms were reimplemented from
scratch in order to improve efficiency.

*** Version 3.0a	Albert Graef <ag@muwiinfa.geschichte.uni-mainz.de>

May 1992. Bug fix release.

*** Version 4.0		Berend de Boer <berend@pobox.com>

Oct 1996. This version differs with the previous release, 3.0a, that it
compiles under Dos, DPMI, Windows, Delphi 16 and Delphi 32. The source is now
maintained by Berend de Boer <berend@pobox.com>.

For the protected mode or win32 platforms Lex and Yacc also have significantly
lager tables. The win32 in fact can have unlimited tables because you have 2GB
to store things :-) The 16-bit DPMI platforms have tables extended as large as
possible without changing basic Lex or Yacc sources.

This version was ported to Free Pascal by Michael Van Canneyt
<michael@freepascal.org> (April 1998).

*** Version 4.1		Michael Van Canneyt <michael@freepascal.org>
			Albert Graef <ag@muwiinfa.geschichte.uni-mainz.de>

May 1998. Merges the Turbo and Free Pascal versions into a single package.


Contents of the Package
======== == === =======

The TP Lex and Yacc programs consist of 23 modules with about 11000 lines of
code. A short description of each of the source modules is given below.

LEX      PAS		TP Lex main program
LEXBASE  PAS		base module (global declarations)
LEXDFA   PAS		DFA construction algorithm
LEXLIB   PAS		TP Lex library unit
LEXLIST  PAS		listing operations
LEXMSGS  PAS		messages and error handling
LEXOPT   PAS		DFA optimization algorithm
LEXPOS   PAS		operations to construct the position table
LEXRULES PAS		parser for TP Lex grammar rules
LEXTABLE PAS		internal tables used by the TP Lex program

YACC     PAS		TP Yacc parser and main program
YACC     Y		TP Yacc source for YACC.PAS
YACCBASE PAS		base module (global declarations)
YACCCLOS PAS		closure and first set construction algorithms
YACCLIB  PAS		TP Yacc library unit
YACCLOOK PAS		LALR lookahead computation algorithm
YACCLR0  PAS		LR(0) set construction algorithm
YACCMSGS PAS		messages and error handling
YACCPARS PAS		parse table construction
YACCSEM  PAS		semantic routines of the TP Yacc parser
YACCTABL PAS		internal tables used by the TP Yacc program

YYLEX    COD		code template for the lexical analyzer routine
YYPARSE  COD		code template for the LALR parser routine

Besides this, the package also contains the following docs:

COPYING			GNU General Public License
README      		this file
TPLY     DOC		ASCII version of the manual
TPLY     TEX		TeX version of the manual

Furthermore, the EXAMPLE subdir contains various sample TP Lex and Yacc
programs, such as a (Standard) Pascal parser and a complete TPLY cross
referencing utility named `yref'. (NB: Many of these examples still do not
work properly with Free Pascal, apparently due to some incompatibilities in
the Free Pascal runtime library concerning the handling of standard
input/output. Programs operating on "real" files seem to be unaffected. I hope
that this will be fixed in a future release of the Free Pascal RTL.)


Installation
============

The items to be installed are the executables of TP Lex and Yacc (compiled
from the lex.pas and yacc.pas programs), the Lex and Yacc code templates
(*.cod files), and the LexLib and YaccLib library units (compiled from
lexlib.pas and yacclib.pas).

The plex and pyacc programs will look for the *.cod files in the following locations:
For unix-like operating systems:
1. Current directory.
2. Directory given by FPCDIR
3. Directory /usr/local/lib/fpc/lexyacc
4. Directory /usr/lib/fpc/lexyacc

For other operating systems (dos/windows-like) : 
1. Current directory.
2. Directory given by FPCDIR 
3. Directory where the executable is located.

For the Free Pascal/Linux version, a Makefile is provided. To install, issue
the command `make' (maybe you have to edit the Makefile before this to reflect
your setup) and then `make install'. Note that in the Linux version the
executables will be named `plex' and `pyacc' to avoid name clashes with the
corresponding UNIX utilities.

For the Turbo/Borland/Free Pascal versions under DOS and Windows, several DOS
batch files are provided:

	MAKEDOS.BAT  - makes a real mode executable. Compiles with
	               Turbo Pascal 6.0 to Borland Pascal 7.0.
	MAKEDPMI.BAT - makes a dos protected mode executable. Needs
	               Borland Pascal 7.0.
	MAKEBPW.BAT  - makes a 16-bit Windows executable. Needs 
	               Borland Pascal 7.0 or Borland Pascal for Windows.
	MAKED16.BAT  - makes a 16-bit Windows executable. 
	               Needs Delphi 1.X.
	MAKED32.BAT  - makes a 32-bit Windows NT or Windows 95 console
	               application. Needs Delphi 2.X.
	MAKEFPC.BAT  - makes a 32 bit executable. Needs the Free Pascal
	               compiler.

These will compile the programs lex.pas and yacc.pas, as well as the units
lexlib.pas and yacclib.pas. To install, copy the executables lex.exe and
yacc.exe along with the code templates yylex.cod and yyparse.cod to a place
somewhere on your DOS path. Furthermore, copy the compiled lexlib and yacclib
units to a directory which is searched for unit files by your compiler.

(NB1: I currently have no means to check whether these batch files work except
for the makedos and makefpc files. If you have problems with any of the other
files, please let me know.)

(NB2: The type of compiler used to compile TP Lex and Yacc affects the sizes
of internal tables of these programs. If you want to be able to compile large
grammars, you should therefore compile TP Lex/Yacc using one of the 32 bit
compilers like BP 7.0 or Free Pascal. Note that the Pascal output generated by
TP Lex and Yacc is independent of the type of compiler with which the programs
were compiled. Thus the generated code can be used with any of the supported
compilers, regardless of the type of compiler used to compile the TP Lex and
Yacc programs themselves. You only have to compile the LexLib and YaccLib
units separately for each type of compiler which will be used to compile TP
Lex/Yacc generated programs.)

To complete the installation, you might also wish to install the contents of
the example subdir in a directory of your choice.

As soon as the installation is finished, you can perform a quick bootstrap
test with the command `yacc yacc.y test.pas' (or `pyacc yacc.y test.pas' for
the Free Pascal/Linux version). You can then compare the distributed
`yacc.pas' against the generated `test.pas' with the DOS command `fc' or the
UNIX `diff' command. The two files should not differ.

That's it! Hope you enjoy using this package.

----
Dr. Albert Gr"af
Dept. of Musicinformatics, Johannes Gutenberg-University Mainz, Germany
Email:  ag@muwiinfa.geschichte.uni-mainz.de, Dr.Graef@t-online.de
WWW:    http://www.musikwissenschaft.uni-mainz.de/~ag
