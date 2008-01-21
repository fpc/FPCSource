/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: stream.c,v 2.7 2001/11/08 12:30:32 dick Exp $
*/

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>

#include	"system.par"
#include	"token.h"
#include	"lex.h"
#include	"lang.h"
#include	"stream.h"

static FILE *fopen_regular_file(const char *fname);

int
OpenStream(const char *fname) {
	int ok;

	lex_nl_cnt = 1;
	lex_tk_cnt = 0;
	lex_non_ascii_cnt = 0;

	/* start the lex machine */
	yyin = fopen_regular_file(fname);
	ok = (yyin != 0);
	if (!ok) {
		/* fake a stream, to simplify the rest of the program */
		yyin = fopen(NULLFILE, "r");
	}
	yystart();
	return ok;
}

static FILE *fopen_regular_file(const char *fname) {
	struct stat buf;
	
	if (stat(fname, &buf) != 0) return 0;
	if ((buf.st_mode & S_IFMT) != S_IFREG) return 0;
	return fopen(fname, "r");
}

int
NextStreamTokenObtained(void) {
	return yylex();
}

void
CloseStream(void) {
	if (yyin) {
		fclose(yyin);
		yyin = 0;
	}
}
