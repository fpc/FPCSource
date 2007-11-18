%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: pascallang.l,v 2.9 2007/08/29 09:10:35 dick Exp $
*/

/*
	PASCAL language front end for the similarity tester.
	Author:	Maarten van der Meulen <maarten@cs.vu.nl>
*/

#include	"options.h"
#include	"algollike.h"
#include	"token.h"
#include	"idf.h"
#include	"lex.h"
#include	"lang.h"

/* Language-dependent Code */

/* Data for module idf */

static const struct idf ppcmd[] = {
	{"define",	META('d')},
	{"else",	META('e')},
	{"endif",	META('E')},
	{"if",		META('i')},
	{"ifdef",	META('I')},
	{"ifndef",	META('x')},
	{"include",	MTCT('I')},
	{"line",	META('l')},
	{"undef",	META('u')}
};

static const struct idf reserved[] = {
	{"and",		NORM('&')},
	{"array",	NORM('A')},
        {"as",          NORM('a')},
	{"begin",	NORM('{')},
	{"case",	NORM('c')},
        {"catch",       META('C')},
        {"class",       META('c')},
	{"continue",	CTRL('C')},
	{"constructor",	NORM('p')},	/* equal to procedure */
	{"const",	NORM('C')},
	{"destructor",	NORM('p')},	/* equal to procedure */
	{"div",		NORM('/')},
	{"do",		NORM('D')},
	{"downto",	NORM('d')},
	{"else",	NORM('e')},
	{"end",		NORM('}')},
	{"extern",	CTRL('E')},
	{"except",	META('E')},
	{"file",	NORM('F')},
        {"finally",     META('F')},
	{"for",		NORM('f')},
	{"function",	NORM('p')},	/* Equal to procedure */
	{"goto",	NORM('g')},
	{"if",		NORM('i')},
	{"in",		NORM('I')},
	{"inherited",	META('i')},
	{"is",		META('I')},
	{"label",	NORM('l')},
	{"mod",		NORM('%')},
	{"nil",		NORM('n')},
	{"not",		NORM('!')},
	{"of",		SKIP},
	{"on",		SKIP},
	{"or",		NORM('|')},
	{"object",	NORM('O')},
	{"override",	NORM('o')},
	{"packed",	NORM('P')},
	{"procedure",	NORM('p')},
	{"program",	SKIP},
	{"private",	META('P')},
	{"protected",	META('p')},
	{"public",	CTRL('P')},
	{"raise",	META('R')},
	{"record",	NORM('r')},
	{"repeat",	NORM('R')},
	{"set",		NORM('s')},
	{"then",	SKIP},
	{"to",		NORM('t')},
	{"type",	NORM('T')},
	{"until",	NORM('u')},
	{"var",		NORM('v')},
	{"virtual",	NORM('V')},
	{"while",	NORM('w')},
	{"with",	NORM('W')}
};

/* Special treatment of identifiers */

static void
lower_case(char *str) {
	/*	Turns upper case into lower case, since Pascal does not
		distinguish between them.
	*/
	register char *s;

	for (s = str; *s; s++) {
		if ('A' <= *s && *s <= 'Z') {
			*s += (-'A' + 'a');
		}
	}
}

static TOKEN
idf2token(int hashing) {
	register TOKEN tk;

	lower_case(yytext);
	tk = idf_in_list(yytext, reserved, sizeof reserved, IDF);
	if (TOKEN_EQ(tk, IDF) && hashing) {
		/* return a one-token hash code */
		tk = idf_hashed(yytext);
	}
	return tk;
}

/* Token sets for module algollike */
const TOKEN NonFinals[] = {
	IDF,		/* identifier */
	NORM('{'),	/* also begin */
	NORM('('),
	NORM('['),
	NORM('A'),	/* array */
	NORM('c'),	/* case */
        META('C'),      /* catch */
        META('c'),      /* class */
	NORM('C'),	/* const */
	NORM('/'),	/* div */
	CTRL('E'),	/* extern */
	NORM('F'),	/* file */
	NORM('f'),	/* for */
	NORM('g'),	/* goto */
	NORM('i'),	/* if */
	META('i'),	/* inherited */
	NORM('l'),	/* label */
	NORM('O'),	/* object */
	NORM('P'),	/* packed */
	NORM('p'),	/* procedure/function/constructor/destructor */
	META('P'),	/* private */
	META('p'),	/* protected */
	CTRL('p'),	/* public */
	META('R'),	/* raise */
	NORM('r'),	/* record */
	NORM('R'),	/* repeat */
	NORM('s'),	/* set */
	NORM('T'),	/* type */
	NORM('v'),	/* var */
	NORM('w'),	/* while */
	NORM('W'),	/* with */
	NOTOKEN
};
const TOKEN NonInitials[] = {
	NORM(')'),
	NORM('}'),
	NORM(';'),
	NOTOKEN
};
const TOKEN Openers[] = {
	NORM('{'),
	NORM('('),
	NORM('['),
	NOTOKEN
};
const TOKEN Closers[] = {
	NORM('}'),
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
StrChar		([^'\n\\]|{AnyQuoted})

StartComment	("{"|"(*")
EndComment	("}"|"*)")
SafeComChar	([^*}\n])
UnsafeComChar	("*")

SingleLineCom   ("//".*)

Digit		([0-9])
Idf		([A-Za-z][A-Za-z0-9_]*)

%%

{StartComment}	{			/* See clang.l */
		BEGIN Comment;
	}

<Comment>{SafeComChar}+	{		/* safe comment chunk */
	}

<Comment>{UnsafeComChar}	{	/* unsafe char, read one by one */
	}

<Comment>"\n"		{		/* to break up long comments */
		return_eol();
	}

<Comment>{EndComment}	{		/* end-of-comment */
		BEGIN INITIAL;
	}

\'{StrChar}*\'	{			/* character strings */
		return_ch('"');
	}

{SingleLineCom}"\n"     {               /* single-line comment */
                return_eol();
        }

^#{Layout}*include.*	{		/* ignore #include lines */
	}

^#{Layout}*{Idf}	{		/* a preprocessor line */
		register char *idf = yytext+1;

		/* skip layout in front of preprocessor identifier */
		while (*idf == ' ' || *idf == '\t') {
			idf++;
		}
		return_tk(idf_in_list(idf, ppcmd, sizeof ppcmd, NORM('#')));
	}

{Digit}+	{			/* numeral, passed as an identifier */
		return_tk(IDF);
	}

{Idf}/"("	{			/* identifier in front of ( */
		register TOKEN tk;

		tk = idf2token(option_set('F'));
		if (!TOKEN_EQ(tk, SKIP)) return_tk(tk);
	}

{Idf}	{				/* identifier */
		register TOKEN tk;

		tk = idf2token(0 /* no hashing */);
		if (!TOKEN_EQ(tk, SKIP)) return_tk(tk);
	}

\;	{				/* semicolon, conditionally ignored */
		if (option_set('f')) return_ch(yytext[0]);
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
