%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: lisplang.l,v 2.9 2007/08/29 09:10:33 dick Exp $
*/

/*
	LISP language front end for the similarity tester.
	Author:	Gertjan Akkerman <akkerm@cs.vu.nl>
	Date:	Thu, 9 Apr 87 11:15:23 MDT
*/

#include	"language.h"
#include	"token.h"
#include	"lex.h"
#include	"lang.h"

/* Language-dependent Code */
#include	"idf.h"

static const struct idf reserved[] = {
	{"append",	NORM('a')},
	{"append1",	NORM('b')},
	{"atom",	NORM('t')},
	{"car",		NORM('h')},
	{"cdr",		NORM('t')},
	{"cond",	NORM('c')},
	{"cons",	NORM('s')},
	{"defun",	NORM('u')},
	{"do",		NORM('d')},
	{"eq",		NORM('e')},
	{"equal",	NORM('e')},		/* See eq */
	{"for",		NORM('f')},
	{"if",		NORM('i')},
	{"list",	NORM('l')},
	{"nconc",	NORM('n')},
	{"rplaca",	NORM('A')},
	{"rplacd",	NORM('D')}
};

/* Token sets for module algollike */
const TOKEN NonFinals[] = {
	NORM('('),
	NORM('['),
	NOTOKEN
};
const TOKEN NonInitials[] = {
	NORM(')'),
	NORM(']'),
	NOTOKEN
};
const TOKEN Openers[] = {
	NORM('('),
	NORM('['),
	NOTOKEN
};
const TOKEN Closers[] = {
	NORM(')'),
	NORM(']'),
	NOTOKEN
};

%}

%option nounput
%option never-interactive

%Start	Comment

Layout		([ \t\r\f])
ASCII95		([- !"#$%&'()*+,./0-9:;<=>?@A-Z\[\\\]^_`a-z{|}~])

AnyQuoted	(\\.)
StrChar		([^"\n\\]|{AnyQuoted})
ChrChar		([^'\\]|{AnyQuoted})

IdfChar		([-!#$%&*+,/0-9:;<=>?@A-Z\\^_`a-z{}~])

EscIdf		(({IdfChar}|\\.)+)
QuotIdf		("|"[^\|\n]*"|")
Idf		({EscIdf}|{QuotIdf})

%%

";".*$	{				/* comment */
	}

\"{StrChar}*\"	{			/* strings */
		return_ch('"');
	}

{Idf}	{				/* identifier */
		return_tk(idf_in_list(yytext, reserved, sizeof reserved, IDF));
	}

\n	{				/* count newlines */
		return_eol();
	}

{Layout}	{			/* ignore layout */
	}

{ASCII95}	{			/* copy other text */
		return_ch(yytext[0]);
	}

.	{				/* count non-ASCII chars */
		lex_non_ascii_cnt++;
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
