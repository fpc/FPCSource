/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: compare.c,v 2.5 2001/09/28 09:03:47 dick Exp $
*/

#include	"sim.h"
#include	"tokenarray.h"
#include	"hash.h"
#include	"language.h"
#include	"options.h"
#include	"add_run.h"
#include	"compare.h"

static void compare1text(int, int, int);
static unsigned int lcs(
	struct text *, unsigned int, struct text **, unsigned int *,
	unsigned int, unsigned int
);

/*	The overall structure of the routine Compare() is:

	for all new files
		for all texts it must be compared to
			for all positions in the new file
				for all positions in the text
					for ever increasing sizes
						try to match and keep the best
*/

void
Compare(void) {
	register int n;

	for (n = 0; n < NumberOfNewTexts; n++) {
		register int first =
			(	option_set('S') ? NumberOfNewTexts + 1
			:	option_set('s') ? n + 1
			:	n
			);

		if (option_set('e')) {
			/* from first to NumberOfTexts in steps */
			register int m;

			for (m = first; m < NumberOfTexts; m++) {
				compare1text(n, m, m+1);
			}
		}
		else {
			/* from first to NumberOfTexts in one action */
			if (first < NumberOfTexts) {
				compare1text(n, first, NumberOfTexts);
			}
		}
	}
}

static void
compare1text(
	int n,				/* text to be compared */
	int first,			/* first text to be compared to */
	int limit			/* limit text in comparison */
) {
	register unsigned int i_first = Text[first].tx_start;
	register unsigned int i_limit = Text[limit-1].tx_limit;
	register struct text *txt0 = &Text[n];
	register unsigned int i0 = txt0->tx_start;

	while (	/* there may still be a useful substring */
		i0 + MinRunSize - 1 < txt0->tx_limit
	) {
		/* see if there really is one */
		struct text *txt_best;
		unsigned int i_best;
		register unsigned int size_best =
			lcs(txt0, i0, &txt_best, &i_best, i_first, i_limit);

		if (size_best) {
			/* good run found; enter it */
			add_run(txt0, i0, txt_best, i_best, size_best);
			/* and skip it */
			i0 += size_best;
		}
		else {
			/* we try our luck at the next token */
			i0++;
		}
	}
}

static unsigned int
lcs(	struct text *txt0,		/* input: starting position */
	unsigned int i0,
	struct text **tbp,		/* output: position of best run */
	unsigned int *ibp,
	unsigned int i_first,		/* no comparison before this pos. */
	unsigned int i_limit		/* no comparison after this pos. */
) {
	/*	Finds the longest common substring (not -sequence) in:
			txt0, starting precisely at i0 and
			the text between i_first and i_limit.
		Writes the position in tbp and ibp and returns the size.
		Returns 0 if no common substring is found.
	*/
	register struct text *txt1 = txt0;
	register unsigned int i1 = i0;
	register unsigned int size_best = 0;
	register unsigned int txt0limit = txt0->tx_limit;
	register unsigned int txt1limit = txt1->tx_limit;

	while (	/* there is a next opportunity */
		(i1 = ForwardReference(i1))
	&&	/* it is still in range */
		i1 < i_limit
	) {
		register unsigned int min_size;
		register unsigned int new_size;
		register unsigned int j0;
		register unsigned int j1;

		if (i1 < i_first) {	/* not in range */
			continue;
		}

		/* bump txt1; we may have skipped a text or two */
		while (i1 >= txt1->tx_limit) {
			txt1++;
		}
		txt1limit = txt1->tx_limit;

		min_size = (size_best ? size_best+1 : MinRunSize);
		/* are we looking at something better than we have got? */
		{
			j0 = i0 + min_size - 1;
			j1 = i1 + min_size - 1;
			if (	/* j0 still inside txt0 */
				j0 < txt0limit
			&&	/* j1 still inside txt1 */
				j1 < txt1limit
			&&	/* j0 and j1 don't overlap */
				j0 < j1 - min_size + 1
			) {
				/* there would be room enough */
				register int cnt = min_size;

				/* does the text match? */
				while (	cnt
				&&	TOKEN_EQ(TokenArray[j0], TokenArray[j1])
				) {
					cnt--, j0--, j1--;
				}
				if (cnt) continue;	/* forget it */
			}
			else continue;			/* forget it */
		}

		/* yes, we are; how long can we make it? */
		{
			register unsigned int size = min_size;

			j0 = i0 + min_size;
			j1 = i1 + min_size;
			while (	/* j0 still inside txt0 */
				j0 < txt0limit
			&&	/* j1 still inside txt1 */
				j1 < txt1limit
			&&	/* j0 and j1 don't overlap */
				j0 + size < j1
			&&	/* tokens are the same */
				TOKEN_EQ(TokenArray[j0], TokenArray[j1])
			) {
				j0++, j1++, size++;
			}
			new_size = size;
		}

		/*	offer the run to the Language Department which may
			reject it or may cut its tail
		*/
		new_size = (	MayBeStartOfRun(TokenArray[i0])
			   ?	CheckRun(&TokenArray[i0], new_size)
			   :	0
			   );

		if (	/* we still have something acceptable */
			new_size >= MinRunSize
		&&	/* it is better still than what we had */
			new_size > size_best
		) {
			/* record it */
			*tbp = txt1;
			*ibp = i1;
			size_best = new_size;
		}
	}

	return size_best;
}
