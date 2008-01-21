%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: javalang.l,v 1.4 2007/08/29 09:10:32 dick Exp $
*/

/*
	Java language front end for the similarity tester.
	Author:	Dick Grune <dick@cs.vu.nl>
*/

#include	"options.h"
#include	"algollike.h"
#include	"token.h"
#include	"idf.h"
#include	"lex.h"
#include	"lang.h"

/* Language-dependent Code */

static const struct idf reserved[] = {
	{"abstract",	NORM('a')},
	{"boolean",	NORM('b')},
	{"break",	NORM('B')},
	{"byte",	CTRL('B')},
	{"case",	NORM('c')},
	{"catch",	NORM('C')},
	{"char",	CTRL('C')},
	{"class",	META('c')},
	{"continue",	META('C')},
	{"default",	NORM('d')},
	{"do",		NORM('D')},
	{"double",	CTRL('D')},
	{"else",	NORM('e')},
	{"extends",	NORM('E')},
	{"false",	NORM('g')},	/* Boolean literal */
	{"final",	NORM('f')},
	{"finally",	NORM('F')},
	{"float",	CTRL('F')},
	{"for",		META('f')},
	{"if",		NORM('i')},
	{"implements",	NORM('I')},
	{"import",	CTRL('I')},
	{"instanceof",	META('i')},
	{"int",		META('I')},
	{"interface",	MTCT('I')},
	{"long",	NORM('l')},
	{"native",	NORM('n')},
	{"new",		NORM('N')},
	{"null",	CTRL('N')},	/* null literal */
	{"package",	NORM('p')},
	{"private",	NORM('P')},
	{"protected",	CTRL('P')},
	{"public",	META('p')},
	{"return",	NORM('r')},
	{"short",	NORM('s')},
	{"static",	NORM('S')},
	{"super",	CTRL('S')},
	{"switch",	META('s')},
	{"synchronized",META('S')},
	{"this",	NORM('t')},
	{"throw",	NORM('T')},
	{"throws",	CTRL('T')},
	{"true",	META('t')},	/* Boolean literal */
	{"void",	NORM('v')},
	{"volatile",	NORM('V')},
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
	NORM('a'),	/* abstract */
	NORM('b'),	/* boolean */
	NORM('B'),	/* break */
	CTRL('B'),	/* byte */
	NORM('c'),	/* case */
	NORM('C'),	/* catch */
	CTRL('C'),	/* char */
	META('c'),	/* class */
	META('C'),	/* continue */
	NORM('d'),	/* default */
	NORM('D'),	/* do */
	CTRL('D'),	/* double */
	NORM('e'),	/* else */
	NORM('E'),	/* extends */
	NORM('f'),	/* final */
	NORM('F'),	/* finally */
	CTRL('F'),	/* float */
	META('f'),	/* for */
	NORM('i'),	/* if */
	NORM('I'),	/* implements */
	CTRL('I'),	/* import */
	META('i'),	/* instanceof */
	META('I'),	/* int */
	MTCT('I'),	/* interface */
	NORM('l'),	/* long */
	NORM('n'),	/* native */
	NORM('N'),	/* new */
	NORM('p'),	/* package */
	NORM('P'),	/* private */
	CTRL('P'),	/* protected */
	META('p'),	/* public */
	NORM('r'),	/* return */
	NORM('s'),	/* short */
	NORM('S'),	/* static */
	CTRL('S'),	/* super */
	META('s'),	/* switch */
	META('S'),	/* synchronized */
	NORM('T'),	/* throw */
	CTRL('T'),	/* throws */
	NORM('v'),	/* void */
	NORM('V'),	/* volatile */
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

Digit		([0-9a-fA-F])

UniCode		(\\u{Digit}{Digit}{Digit}{Digit})
AnyQuoted	((\\.)|{UniCode})
StrChar		([^"\n\\]|{AnyQuoted})
ChrChar		([^'\n\\]|{AnyQuoted})

StartComment	("/*")
EndComment	("*/")
SafeComChar	([^*\n])
UnsafeComChar	("*")

SingleLineCom	("//".*)

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

{SingleLineCom}"\n"	{		/* single-line comment */
		return_eol();
	}

\"{StrChar}*\"	{			/* strings */
		return_ch('"');
	}

\'{ChrChar}+\'	{			/* characters */
		return_ch('\'');
	}

(0x)?{Digit}+("l"|"L")?	{		/* numeral, passed as an identifier */
		return_tk(IDF);
	}

"import"{Layout}[^;]*;	{		/* import statement; ignore */
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
