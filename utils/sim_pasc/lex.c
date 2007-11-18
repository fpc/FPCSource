/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: lex.c,v 1.3 1998/02/03 14:28:26 dick Exp $
*/

/*	The communication variables, as set by yylex, NextStreamTokenObtained
	and NextTextTokenObtained.
*/

#include	"token.h"
#include	"lex.h"

TOKEN lex_token;			/* token produced, or EOL */
unsigned int lex_nl_cnt;		/* line count */
unsigned int lex_tk_cnt;		/* token position */
unsigned int lex_non_ascii_cnt;		/* # of non-ASCII chars found */
