/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: lang.h,v 1.2 1998/01/21 14:27:51 dick Exp $
*/

/*
	The token-providing module 'lang' has three interfaces:
	-	lang.h, which provides access to the lowest-level token
			routines, to be used by the next level.
	-	lex.h, which provides the lex variables, to be used by
			all and sundry.
	-	language.h, which provides language-specific info about
			tokens, concerning their suitability as initial
			and final tokens, to be used by higher levels.
			
	This structure is not satisfactory, but it is also unreasonable
	to combine them in one interface.

	There is no single lang.c; rather it is represented by the
	various Xlang.c files generated from the Xlang.l files.
*/

#include	"token.h"

/* useful macros */
#define	return_tk(tk)	{lex_tk_cnt++; lex_token = (tk); return 1;}
#define	return_ch(ch)	{lex_tk_cnt++; lex_token = int2TOKEN((int)(ch)); return 1;}
#define	return_eol()	{lex_nl_cnt++; lex_token = EOL; return 1;}

extern int yylex(void);
extern void yystart(void);
extern FILE *yyin;
