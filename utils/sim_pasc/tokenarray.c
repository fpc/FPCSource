/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: tokenarray.c,v 1.2 2001/11/13 12:55:59 dick Exp $
*/

#include	<malloc.h>

#include	"error.h"
#include	"lex.h"
#include	"tokenarray.h"

#define	TK_INCR		10000		/* increment of token array size */

TOKEN *TokenArray;			/* to be filled by malloc */
static unsigned int tk_size;		/* size of TokenArray[] */
static unsigned int tk_free;		/* next free position in TokenArray[] */

void
InitTokenArray(void) {
	tk_size = TK_INCR;
	TokenArray = (TOKEN *)malloc(sizeof (TOKEN) * tk_size);
	if (!TokenArray) fatal("out of memory");
	tk_free = 1;		/* don't use position 0 */
}

void
StoreToken(void) {
	if (tk_free == tk_size) {
		/* allocated array is full; try to increase its size */
		unsigned int new_size = tk_size + TK_INCR;
		register TOKEN *new_array = (TOKEN *)realloc(
			(char *)TokenArray,
			sizeof (TOKEN) * new_size
		);

		if (new_size < tk_free)
			fatal("internal error: TK_INCR causes numeric overflow");
		if (!new_array) {
			/* we failed */
			fatal("out of memory");
		}
		TokenArray = new_array, tk_size = new_size;
	}

	/* now we are sure there is room enough */
	TokenArray[tk_free++] = lex_token;
}

unsigned int
TextLength(void) {
	return tk_free;
}
