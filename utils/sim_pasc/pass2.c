/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: pass2.c,v 2.10 2004/08/05 09:49:46 dick Exp $
*/

#include	<stdio.h>

#include	"debug.par"
#include	"sim.h"
#include	"text.h"
#include	"lex.h"
#include	"pass2.h"

#ifdef	DB_POS
static void db_print_pos_list(const char *, const struct position *);
static void db_print_lex(const char *);
#endif

static void pass2_txt(struct text *txt);
static int next_eol_obtained(void);

void
Pass2(void) {
	int n;

	for (n = 0; n < NumberOfTexts; n++) {
		pass2_txt(&Text[n]);
	}
}

/* instantiate sort_pos_list() */
#define	SORT_STRUCT		position
#define	SORT_NAME		sort_pos_list
#define	SORT_BEFORE(p1,p2)	((p1)->ps_tk_cnt < (p2)->ps_tk_cnt)
#define	SORT_NEXT		ps_next
#include	"sortlist.bdy"

static void
pass2_txt(struct text *txt) {
	register struct position *pos;
	register unsigned int old_nl_cnt;

	if (!txt->tx_pos)	/* no need to scan the file */
		return;

	if (!OpenText(Second, txt)) {
		fprintf(stderr, ">>>> File %s disappeared <<<<\n",
			txt->tx_fname
		);
	}
	/* sets lex_nl_cnt and lex_tk_cnt */

#ifdef	DB_POS
	db_print_pos_list("before sorting", txt->tx_pos);
#endif	/* DB_POS */

	sort_pos_list(&txt->tx_pos);

#ifdef	DB_POS
	db_print_pos_list("after sorting", txt->tx_pos);
#endif	/* DB_POS */

#ifdef	DB_NL_BUFF
	db_print_nl_buff(txt->tx_nl_start, txt->tx_nl_limit);
#endif	/* DB_NL_BUFF */

	old_nl_cnt = 1;
	pos = txt->tx_pos;
	while (pos) {
		/* we scan the pos list and the file in parallel */

		/* find the corresponding line */
		while (pos->ps_tk_cnt >= lex_tk_cnt) {
			/* pos does not refer to this line, try the next */

			/* shift the administration */
			old_nl_cnt = lex_nl_cnt;
			/* and get the next eol position */
			if (!next_eol_obtained()) {
				/* ouch! not enough lines! */
				fprintf(stderr, ">>>> File %s modified <<<<\n",
					txt->tx_fname
				);
				break;
			}
#ifdef	DB_POS
			db_print_lex(txt->tx_fname);
#endif	/* DB_POS */
		}

		/* fill in the pos */
		switch (pos->ps_type) {
		case 0:	/* first token of run */
			pos->ps_nl_cnt = old_nl_cnt;
			break;
		case 1:	/* last token of run */
			pos->ps_nl_cnt = lex_nl_cnt;
			break;
		}
		/* and get the next pos */
		pos = pos->ps_next;
	}

#ifdef	DB_POS
	db_print_pos_list("after scanning", txt->tx_pos);
#endif	/* DB_POS */

	CloseText(Second, txt);
}

static int
next_eol_obtained(void) {
	while (NextTextTokenObtained(Second)) {
		if (TOKEN_EQ(lex_token, EOL)) return 1;
	}
	return 0;
}

#ifdef	DB_POS

static void
db_print_pos(const struct position *pos) {
	fprintf(DebugFile, "pos type: %s; token count: %u",
		(pos->ps_type == 0 ? "first" : " last"),
		pos->ps_tk_cnt
	);
	fprintf(DebugFile, ", line#: ");
	if (pos->ps_nl_cnt == -1) {
		fprintf(DebugFile, "<NOT SET>");
	}
	else {
		fprintf(DebugFile, "%u", pos->ps_nl_cnt);
	}
	fprintf(DebugFile, "\n");
}

static void
db_print_pos_list(const char *msg, const struct position *pos) {
	fprintf(DebugFile, "\n**** DB_PRINT_POS_LIST, %s ****\n", msg);

	while (pos) {
		db_print_pos(pos);
		pos = pos->ps_next;
	}
	fprintf(DebugFile, "\n");
}

static void
db_print_lex(const char *fn) {
	fprintf(DebugFile, "%s: lex_tk_cnt = %u, lex_nl_cnt = %u\n",
		fn, lex_tk_cnt, lex_nl_cnt);
}

#endif	/* DB_POS */
