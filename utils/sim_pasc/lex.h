/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: lex.h,v 2.5 1998/02/03 14:28:27 dick Exp $
*/

/*	Since the lex_X variables are hoisted unchanged through the levels
	lang, stream, and buff, to be used by pass1, pass2, etc., they
	have to be placed in a module of their own.
*/

#include	"token.h"

/* special tokens */
#define	EOL		NORM(0377)	/* end of line */

extern TOKEN lex_token;			/* token produced, or EOL */
extern unsigned int lex_nl_cnt;		/* line count */
extern unsigned int lex_tk_cnt;		/* token position */
extern unsigned int lex_non_ascii_cnt;	/* # of non-ASCII chars found */
