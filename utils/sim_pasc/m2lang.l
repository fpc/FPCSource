%{
/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: m2lang.l,v 2.9 2007/08/29 09:10:33 dick Exp $
*/

/*
	Modula-2 language front end for the similarity tester.
	Author:	Dick Grune <dick@cs.vu.nl>
*/

#include	"options.h"
#include	"algollike.h"
#include	"token.h"
#include	"idf.h"
#include	"lex.h"
#include	"lang.h"

/* Language-dependent Code */

/*	Most Modula-2 programs start with a number of IMPORTs that look
	very similar from program to program.  These are skipped by ignoring
	the reserved words IMPLEMENTATION, DEFINITION, MODULE, IMPORT
	and FROM, having a flag skip_imports, and start reacting only
	at the first non-ignored reserved word.

	Also, the nesting comments require a state variable.
*/

/* Additional state variables, set in yystart() */
static int skip_imports;
static int comment_level;

/* Data for module idf */

static const struct idf reserved[] = {
	{"AND",		NORM('&')},
	{"ARRAY",	NORM('A')},
	{"BEGIN",	NORM('{')},
	{"BY",		NORM('B')},
	{"CASE",	NORM('c')},
	{"CONST",	NORM('C')},
	{"DEFINITION",	SKIP},
	{"DIV",		NORM('/')},
	{"DO",		NORM('D')},
	{"ELSE",	NORM('e')},
	{"ELSIF",	NORM('e')},
	{"END",		NORM('}')},
	{"EXIT",	NORM('E')},
	{"EXPORT",	CTRL('E')},
	{"FOR",		NORM('F')},
	{"FROM",	SKIP},
	{"IF",		NORM('i')},
	{"IMPLEMENTATION", SKIP},
	{"IMPORT",	SKIP},
	{"IN",		NORM('I')},
	{"LOOP",	NORM('l')},
	{"MOD",		NORM('%')},
	{"MODULE",	SKIP},
	{"NOT",		NORM('~')},
	{"OF",		SKIP},
	{"OR",		NORM('O')},
	{"POINTER",	NORM('p')},
	{"PROCEDURE",	NORM('P')},
	{"QUALIFIED",	NORM('q')},
	{"RECORD",	NORM('r')},
	{"REPEAT",	NORM('R')},
	{"RETURN",	CTRL('r')},
	{"SET",		NORM('s')},
	{"THEN",	SKIP},
	{"TO",		NORM('t')},
	{"TYPE",	NORM('T')},
	{"UNTIL",	NORM('u')},
	{"VAR",		NORM('v')},
	{"WHILE",	NORM('w')},
	{"WITH",	NORM('W')},
};

static const struct idf standard[] = {
	{"ABS",		META('a')},
	{"ADDRESS",	META('A')},
	{"ALLOCATE",	MTCT('A')},
	{"BITSET",	META('b')},
	{"BOOLEAN",	META('B')},
	{"CAP",		META('c')},
	{"CARDINAL",	META('C')},
	{"CHAR",	MTCT('C')},
	{"CHR",		META('x')},
	{"DEALLOCATE",	META('d')},
	{"DEC",		META('D')},
	{"EXCL",	META('e')},
	{"FALSE",	META('f')},
	{"FLOAT",	META('F')},
	{"HALT",	META('h')},
	{"HIGH",	META('H')},
	{"INC",		META('i')},
	{"INCL",	META('I')},
	{"INTEGER",	MTCT('I')},
	{"LONGCARD",	META('L')},
	{"LONGINT",	META('L')},
	{"LONGREAL",	META('L')},
	{"MAX",		META('m')},
	{"MIN",		META('M')},
	{"NEWPROCESS",	META('n')},
	{"NIL",		META('N')},
	{"ODD",		META('o')},
	{"ORD",		META('O')},
	{"PROC",	META('p')},
	{"REAL",	META('r')},
	{"SIZE",	META('s')},
	{"SYSTEM",	META('S')},
	{"TRANSFER",	META('t')},
	{"TRUE",	META('T')},
	{"TRUNC",	MTCT('T')},
	{"VAL",		META('v')},
	{"WORD",	META('w')}
};

/* Special treatment of identifiers */

static TOKEN
idf2token(int hashing) {
	register TOKEN tk;

	/* the token can be on two lists, reserved and standard */
	tk = idf_in_list(yytext, reserved, sizeof reserved, IDF);

	/* is it one of the keywords to be ignored? */
	if (TOKEN_EQ(tk, SKIP)) return tk;

	/*	The statement below is a significant comment
		on the value of state variables.
	*/
	if (!TOKEN_EQ(tk, IDF)) {
		/* reserved word, stop the skipping */
		skip_imports = 0;
	}
	else {
		/* it is an identifier but not a reserved word */
		if (skip_imports) {
			/* skip it */
			tk = 0;
		}
		else {
			/* look further */
			tk = idf_in_list(yytext, standard, sizeof standard, IDF);
			if (TOKEN_EQ(tk, IDF) && hashing) {
				/* return a one-token hash code */
				tk = idf_hashed(yytext);
			}
		}
	}
	return tk;
}

/* Token sets for module algollike */
const TOKEN NonFinals[] = {
	IDF,		/* identifier */
	NORM('{'),	/* also BEGIN */
	NORM('('),
	NORM('['),
	NORM('A'),	/* ARRAY */
	NORM('c'),	/* CASE */
	NORM('C'),	/* CONST */
	NORM('E'),	/* EXIT */
	NORM('F'),	/* FOR */
	NORM('i'),	/* IF */
	NORM('l'),	/* LOOP */
	NORM('p'),	/* POINTER */
	NORM('P'),	/* PROCEDURE */
	NORM('r'),	/* RECORD */
	NORM('R'),	/* REPEAT */
	CTRL('R'),	/* RETURN */
	NORM('s'),	/* SET */
	NORM('T'),	/* TYPE */
	NORM('v'),	/* VAR */
	NORM('w'),	/* WHILE */
	NORM('W'),	/* WITH */
	NOTOKEN
};
const TOKEN NonInitials[] = {
	NORM('}'),
	NORM(')'),
	NORM(']'),
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
QuStrChar	([^"\n\\]|{AnyQuoted})
ApoStrChar	([^'\n\\]|{AnyQuoted})

StartComment	("(*")
EndComment	("*)")
SafeComChar	([^*\n])
UnsafeComChar	("*")

Digit		([0-9a-fA-F])
Idf		([A-Za-z][A-Za-z0-9_]*)

%%

{StartComment}	{			/* See clang.l */
		/*	Lex itself is incapable of handling Modula-2's
			nested comments. So let's help it a bit.
		*/
		if (comment_level == 0) {
			BEGIN Comment;
		}
		comment_level++;
	}

<Comment>{SafeComChar}+	{		/* safe comment chunk */
	}

<Comment>{UnsafeComChar}	{	/* unsafe char, read one by one */
	}

<Comment>"\n"		{		/* to break up long comments */
		return_eol();
	}

<Comment>{EndComment}	{		/* end-of-comment */
		comment_level--;
		if (comment_level == 0) {
			BEGIN INITIAL;
		}
	}

\"{QuStrChar}*\"	{		/* quoted strings */
		return_ch('"');
	}

\'{ApoStrChar}*\'	{		/* apostrophed strings */
		return_ch('"');
	}

{Digit}+("B"|"C"|"H")?	{		/* numeral, passed as an identifier */
		return_tk(IDF);
	}

"END"{Layout}*{Idf}	{		/* ignore identifier after END */
		return_tk(idf_in_list("END", reserved, sizeof reserved, SKIP));
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

"<>"	{				/* <>, special equivalence */
		return_ch('#');
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
		if (!skip_imports) return_ch(yytext[0]);
	}

.	{				/* count non-ASCII chars */
		lex_non_ascii_cnt++;
	}

%%

/* Language-INdependent Code */

void
yystart(void) {
	skip_imports = 1;
	comment_level = 0;
	BEGIN INITIAL;
}

int
yywrap(void) {
	return 1;
}
