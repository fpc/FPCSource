/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: percentages.c,v 1.3 2007/08/27 09:57:33 dick Exp $
*/

#include	<stdio.h>
#include	<malloc.h>

#include	"sim.h"
#include	"runs.h"
#include	"error.h"
#include	"percentages.h"

struct match {
	struct match *ma_next;
	struct text *ma_text0;
	struct text *ma_text1;
	unsigned int ma_size;
};

static struct match *match_start;	/* to be allocated by malloc */

int
add_to_percentages(struct run *r) {
	/* fails if out of memory, in line with add_to_run() */
	struct match **match_hook = &match_start;

	/* percentages are only meaningful between different files */
	if (r->rn_cn0.ch_text == r->rn_cn1.ch_text) return 1;

	/* look (text0, text1) combination up in match list */
	while (*match_hook) {
		struct match *m = *match_hook;

		if (	m->ma_text0 == r->rn_cn0.ch_text
		&&	m->ma_text1 == r->rn_cn1.ch_text
		) {
			/* found it; now update it */
			m->ma_size += r->rn_size;
			return 1;
		}
		match_hook = &m->ma_next;
	}

	{	/* it's not there; make a new entry */
		struct match *m = *match_hook =
			(struct match *)malloc(sizeof (struct match));

		if (m == 0) return 0;
		m->ma_next = 0;
		m->ma_text0 = r->rn_cn0.ch_text;
		m->ma_text1 = r->rn_cn1.ch_text;
		m->ma_size = r->rn_size;
		return 1;
	}
}

static void
add_reverse_entries_to_match_list(void) {
	struct match **match_hook = &match_start;

	while (*match_hook) {
		struct match *m = *match_hook;
		struct match *n =
			(struct match *)malloc(sizeof (struct match));

		if (!n) fatal("out of memory");
		/* hook in the double */
		n->ma_next = m->ma_next;
		m->ma_next = n;
		n->ma_text0 = m->ma_text1;
		n->ma_text1 = m->ma_text0;
		n->ma_size = m->ma_size;

		match_hook = &n->ma_next;
	}
}

static float
match_percentage(struct match *m) {
	struct text *text0 = m->ma_text0;
	int size0 = text0->tx_limit - text0->tx_start;

	return (m->ma_size*1.0/size0);
}

/* instantiate sort_match_list() */
#define	SORT_STRUCT		match
#define	SORT_NAME		sort_match_list
#define	SORT_BEFORE(p1,p2)	(match_percentage(p1) > match_percentage(p2))
#define	SORT_NEXT		ma_next
#include	"sortlist.bdy"

static void
print_percentages(void) {
	struct match *m = match_start;

	while (m) {
		fprintf(OutputFile,
			"%s consists for %d %% of %s material\n",
			m->ma_text0->tx_fname,
			(int)(match_percentage(m)*100.0),
			m->ma_text1->tx_fname
		);
		
		m = m->ma_next;
	}
}

void
show_percentages(void) {
	add_reverse_entries_to_match_list();
	sort_match_list(&match_start);
	print_percentages();
}
