%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: textlang.l,v 1.3 2007/08/29 09:10:36 dick Exp $
*/

/*
	Text front end for the similarity tester.
*/

#include	"language.h"
#include	"token.h"
#include	"idf.h"
#include	"lex.h"
#include	"lang.h"

/* Language-dependent Code */

void
InitLanguage(void) {
}

/*ARGSUSED*/
int
MayBeStartOfRun(TOKEN tk) {
	/* any token is acceptable */
	return 1;
}

/*ARGSUSED*/
unsigned int
CheckRun(const TOKEN *str, unsigned int size) {
	/* any run is acceptable */
	return size;
}

%}

%option nounput
%option never-interactive

Layout		([ \t\r\f])

%%

[^ \t\n]+	{			/* a word */
		/*	a word is defined as anything not containing
			layout
		*/
		return_tk(idf_hashed(yytext));
	}

\n	{				/* count newlines */
		return_eol();
	}

{Layout}	{			/* ignore layout */
	}

%%

/* Language-INdependent Code */

void
yystart(void) {
	BEGIN INITIAL;
}

int
yywrap(void) {
	return 1;
}
