/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: idf.c,v 2.8 2005/02/20 17:03:00 dick Exp $
*/

#include	<string.h>

#include	"system.par"
#include	"token.h"
#include	"idf.h"

TOKEN
idf_in_list(
	const char *str,
	const struct idf list[],
	unsigned int listsize,
	TOKEN dflt
) {
	register int first = 0;
	register int last = (listsize / sizeof (struct idf)) - 1;

	while (first < last) {
		register int middle = (first + last) / 2;

		if (strcmp(str, list[middle].id_tag) > 0) {
			first = middle + 1;
		}
		else {
			last = middle;
		}
	}
	return (strcmp(str, list[first].id_tag) == 0
	?	list[first].id_tr
	:	dflt
	);
}

TOKEN
idf_hashed(const char *str) {
	register int32 h = 0;

	/* let's be careful about ranges; if done wrong it's hard to debug */
	while (*str) {
		/* -1 <= h <= 2^31-1 */
		h = (h << 1) + (*str++&0377);
		/* -2^31 <= h <= 2^31-1 */
		if (h < 0) {
			/* -2^31 <= h <= -1 */
			h += 2147483647;	/* 2^31-1 */
			/* -1 <= h <= 2^31-2 */
		}
		else {
			/* 0 <= h <= 2^31-1 */
		}
		/* -1 <= h <= 2^31-1 */
	}
	/* -1 <= h <= 2^31-1 */
	if (h < 0) {
		/* h = -1 */
		/* a very small chance, but all the same */
		h = 0;
	}
	/* 0 <= h <= 2^31-1 */
	h %= 253;				/* 0 <= h < 253 */
	return NORM(h + 1);			/* 1 <= h < 254 */
	/* this avoids SKIP (0) and EOL (255) */
}
