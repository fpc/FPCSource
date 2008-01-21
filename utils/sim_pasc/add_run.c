/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: add_run.c,v 2.5 2001/11/08 12:30:28 dick Exp $
*/

#include	<malloc.h>

#include	"sim.h"
#include	"runs.h"
#include	"percentages.h"
#include	"options.h"
#include	"error.h"
#include	"add_run.h"

static void set_chunk(
	struct chunk *,
	struct text *,
	unsigned int,
	unsigned int
);

static void set_pos(
	struct position *,
	int,
	struct text *,
	unsigned int
);

void
add_run(struct text *txt0, unsigned int i0,
	struct text *txt1, unsigned int i1,
	unsigned int size
) {
	/*	Adds the run of given size to our collection.
	*/
	register struct run *r = (struct run *)malloc(sizeof (struct run));

	if (!r) fatal("out of memory");
	set_chunk(&r->rn_cn0, txt0, i0 - txt0->tx_start, size);
	set_chunk(&r->rn_cn1, txt1, i1 - txt1->tx_start, size);
	r->rn_size = size;

	if (option_set('p') ? add_to_percentages(r) : add_to_runs(r)) {
		/* OK */
	}
	else	fatal("out of memory");
}

static void
set_chunk(struct chunk *cnk, struct text *txt,
	  unsigned int start, unsigned int size
) {
	/*	Fill the chunk *cnk with info about the piece of text
		in txt starting at start extending over size tokens.
	*/
	cnk->ch_text = txt;
	set_pos(&cnk->ch_first, 0, txt, start);
	set_pos(&cnk->ch_last, 1, txt, start + size - 1);
}

static void
set_pos(struct position *pos, int type, struct text *txt, unsigned int start) {
	/* Fill a single struct position */
	pos->ps_next = txt->tx_pos;
	txt->tx_pos = pos;

	pos->ps_type = type;
	pos->ps_tk_cnt = start;
	pos->ps_nl_cnt = -1;		/* uninitialized */
}
