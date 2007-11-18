%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: miralang.l,v 1.3 2007/08/29 09:10:34 dick Exp $
*/

/*
	Miranda language front end for the similarity tester.
	Author:	Emma Norling (ejn@cs.mu.oz.au)
	Date:	Nov 1998
*/

#include	"language.h"
#include	"token.h"
#include	"lex.h"
#include	"lang.h"

/* Language-dependent Code */
#include	"idf.h"

static const struct idf reserved[] = {
	{"abstype",	NORM('a')},
	{"bool",	NORM('b')},
	{"char",	NORM('c')},
	{"const",	META('c')},
	{"div",		NORM('d')},
	{"False",	NORM('F')},
	{"if",		NORM('i')},
	{"mod",		NORM('m')},
	{"num",		NORM('n')},
	{"otherwise",	NORM('o')},
	{"readvals",	NORM('r')},
	{"show",	NORM('s')},
	{"sys_message",	META('s')},
	{"True",	NORM('T')},
	{"type",	NORM('t')},
	{"where",	NORM('w')},
	{"with",	META('w')}
};

/* Token sets for module algollike */
const TOKEN NonFinals[] = {
	NORM('('),
	NORM('['),
	NORM('='),
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
	NORM('='),
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

Idf		([A-Za-z][A-Za-z0-9_']*)

%%

"||".*$	{				/* comment */
	}

\"{StrChar}*\"	{			/* strings */
		return_ch('"');
	}

\'{ChrChar}\'	{			/* characters */
		return_ch('\'');
	}

\%{Layout}*include.*	{		/* skip %include line */
	}

\%{Layout}*insert.*	{		/* skip %insert line */
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
