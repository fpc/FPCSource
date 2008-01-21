/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: algollike.c,v 2.4 2005/02/20 17:02:59 dick Exp $
*/

/*	This module implements the routines InitLanguage, MayBeStartOfRun
	and CheckRun for ALGOL-like languages, in which it is meaningful
	and useful to isolate function bodies.

	It requires the user to define, preferably in Xlang.l, four token
	sets, represented as TOKEN[] and terminated by NOTOKEN:

	TOKEN NonFinals[]	tokens that may not end a chunk
	TOKEN NonInitials[]	tokens that may not start a chunk
	TOKEN Openers[]		openers of parentheses that must balance
					in functions
	TOKEN Closers[]		the corresponding closers, in the same order
*/

#include	"options.h"
#include	"token.h"
#include	"algollike.h"

/*	Arrays for fast identification tests for tokens.  Each token is
	identified by its position in the set + 1.  For example, if T is
	the n-th Opener, openers[TOKEN2int(tk)] == n+1.
*/
static char non_finals[256];
static char non_initials[256];
static char openers[256];
static char closers[256];

static void cvt2bittable(const TOKEN *tl, char bt[256]);
static unsigned int largest_function(const TOKEN *str, unsigned int size);

void
InitLanguage(void) {
	/* convert the token sets to bitmaps */
	cvt2bittable(NonFinals, non_finals);
	cvt2bittable(NonInitials, non_initials);
	cvt2bittable(Openers, openers);
	cvt2bittable(Closers, closers);
}

static void
cvt2bittable(const TOKEN *tl, char bt[256]) {
	int i;
	int cnt = 1;

	for (i = 0; !TOKEN_EQ(tl[i], NOTOKEN); i++) {
		bt[TOKEN2int(tl[i])] = cnt++;
	}
}

int
MayBeStartOfRun(TOKEN tk) {
	return !non_initials[TOKEN2int(tk)];
}

unsigned int
CheckRun(const TOKEN *str, unsigned int size) {
	/*	Checks the run starting at str with length size for
		acceptability in the language.  Cuts from the end if
		necessary and returns the accepted length, which may
		be zero.
	*/

	if (option_set('f')) {
		/* reduce to a function-like form first */
		size = largest_function(str, size);
	}

	while (	/* there is trailing garbage */
		size != 0 && non_finals[TOKEN2int(str[size-1])]
	) {
		/* remove it */
		size--;
	}

	return size;
}

static unsigned int
largest_function(const TOKEN *str, unsigned int size) {
	/*	Returns the size of the longest sequence starting at
		str[0] and not containing unbalanced parentheses.
		Does not check the nesting of the parentheses, but then,
		sim is syntax-free anyway.
	*/
	register unsigned int mrb_size = 0;  /* most recent balancing size */
	register unsigned int pos;
	register int i;
	int balance_count[256];
	int n_imbalances;

	/* clear administration */
	n_imbalances = 0;
	for (i = 0; i < 255; i++) {
		balance_count[i] = 0;
	}

	/* scan str[] and see how far we get */
	for (pos = 0; pos < size; pos++) {
		register int tkval = TOKEN2int(str[pos]);
		register int pp;		/* parenthesis position */

		/* account for openers */
		if ((pp = openers[tkval])) {
			if (balance_count[pp] == 0) {
				/* about to create an imbalance */
				n_imbalances++;
			}
			balance_count[pp]++;
		}

		/* account for closers */
		if ((pp = closers[tkval])) {
			if (balance_count[pp] == 0) {
				/* this is one Closer too many */
				return mrb_size;
			}
			balance_count[pp]--;
			if (balance_count[pp] == 0) {
				/* we just cleared an imbalance */
				n_imbalances--;
			}
		}

		if (n_imbalances == 0) {
			/* register balance point */
			mrb_size = pos + 1;
		}
	}
	return mrb_size;
}
