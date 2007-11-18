%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: clang.l,v 2.9 2007/08/29 09:10:31 dick Exp $
*/

/*
	C language front end for the similarity tester.
	Author:	Dick Grune <dick@cs.vu.nl>
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
	{"auto",	NORM('a')},
	{"break",	NORM('b')},
	{"case",	NORM('c')},
	{"char",	NORM('C')},
	{"continue",	CTRL('C')},
	{"default",	NORM('d')},
	{"do",		NORM('D')},
	{"double",	CTRL('D')},
	{"else",	NORM('e')},
	{"enum",	NORM('E')},
	{"extern",	CTRL('E')},
	{"float",	NORM('f')},
	{"for",		NORM('F')},
	{"goto",	NORM('g')},
	{"if",		NORM('i')},
	{"int",		NORM('I')},
	{"long",	NORM('l')},
	{"register",	SKIP},
	{"return",	NORM('r')},
	{"short",	NORM('s')},
	{"sizeof",	NORM('S')},
	{"static",	CTRL('S')},
	{"struct",	META('s')},
	{"switch",	META('S')},
	{"typedef",	NORM('t')},
	{"union",	NORM('u')},
	{"unsigned",	NORM('U')},
	{"void",	SKIP},
	{"while",	NORM('w')}
};

/* Special treatment of identifiers */

static TOKEN
idf2token(int hashing) {
	register TOKEN tk;

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
	NORM('{'),
	NORM('('),
	NORM('a'),	/* auto */
	NORM('b'),	/* break */
	NORM('c'),	/* case */
	NORM('C'),	/* char */
	CTRL('C'),	/* continue */
	NORM('d'),	/* default */
	NORM('D'),	/* do */
	CTRL('D'),	/* double */
	NORM('E'),	/* enum */
	CTRL('E'),	/* extern */
	NORM('f'),	/* float */
	NORM('F'),	/* for */
	NORM('g'),	/* goto */
	NORM('i'),	/* if */
	NORM('I'),	/* int */
	NORM('l'),	/* long */
	NORM('r'),	/* return */
	NORM('s'),	/* short */
	CTRL('S'),	/* static */
	META('s'),	/* struct */
	META('S'),	/* switch */
	NORM('t'),	/* typedef */
	NORM('u'),	/* union */
	NORM('U'),	/* unsigned */
	NORM('w'),	/* while */
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
StrChar		([^"\n\\]|{AnyQuoted})
ChrChar		([^'\n\\]|{AnyQuoted})

StartComment	("/*")
EndComment	("*/")
SafeComChar	([^*\n])
UnsafeComChar	("*")

Digit		([0-9a-fA-F])
Idf		([A-Za-z][A-Za-z0-9_]*)

%%

{StartComment}	{
		/*	We do not have one single pattern to match a comment
			(although one can be written), for two reasons.
			The matched string might overflow lex-internal buffers
			like yysbuf and yytext; and the pattern would be very
			complicated and overtax lex.
			So we break up the string into safe chunks and keep
			track of where we are in a start condition <Comment>.
		*/
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

\"{StrChar}*\"	{			/* strings */
		return_ch('"');
	}

\'{ChrChar}+\'	{			/* characters */
		return_ch('\'');
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

(0x)?{Digit}+("l"|"L")?	{		/* numeral, passed as an identifier */
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
