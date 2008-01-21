/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: text.c,v 1.2 2001/11/13 12:55:58 dick Exp $
*/

#include	<stdio.h>
#include	<malloc.h>

#include	"debug.par"
#include	"sim.h"
#include	"token.h"
#include	"stream.h"
#include	"lex.h"
#include	"options.h"
#include	"error.h"
#include	"text.h"

struct newline {
	unsigned char nl_tk_diff;	/* token position difference */
};

#define	NL_INCR		1000		/* increment of newline buffer size */

static struct newline *nl_buff;		/* to be filled by malloc */
static unsigned int nl_size;		/* size of nl_buff[] */
static unsigned int nl_free;		/* next free position in nl_buff[] */

static unsigned int nl_next, nl_limit;	/* nl_buff[] pointers during pass 2 */

static void store_newline(void);
static void init_nl_buff(void);

/*							TEXT INTERFACE */

static unsigned int last_tk_cnt;	/* token count at newline */
static unsigned int last_nl_cnt;	/* nl counter during pass 2 */

void
InitText(int nfiles) {
	/* allocate the array of text descriptors */
	NumberOfTexts = nfiles;
	Text = (struct text *)
		malloc((unsigned int)(NumberOfTexts*sizeof (struct text)));
	if (!Text) fatal("out of memory");

	init_nl_buff();
}

int
OpenText(enum Pass pass, struct text *txt) {
	switch (pass) {
	case First:
		last_tk_cnt = 0;
		if (nl_buff) {
			txt->tx_nl_start = nl_free;
		}
		break;

	case Second:
		last_tk_cnt = 0;
		if (nl_buff) {
			nl_next = txt->tx_nl_start;
			nl_limit = txt->tx_nl_limit;
			last_nl_cnt = 1;
			lex_nl_cnt = 1;
			lex_tk_cnt = 0;
			return 1;
		}
		break;
	}

	return OpenStream(txt->tx_fname);
}

int
NextTextTokenObtained(enum Pass pass) {
	register int ok = 0;	/* gcc does not understand enum Pass */

	switch (pass) {
	case First:
		ok = NextStreamTokenObtained();
		if (TOKEN_EQ(lex_token, EOL)) {
			store_newline();
			last_tk_cnt = lex_tk_cnt;
		}
		break;

	case Second:
		/* get newline info from the buffer or from the file itself */
		if (nl_buff) {
			if (nl_next == nl_limit) {
				ok = 0;
			}
			else {
				struct newline *nl = &nl_buff[nl_next++];

				lex_nl_cnt = ++last_nl_cnt;
				lex_tk_cnt = (last_tk_cnt += nl->nl_tk_diff);
				lex_token = EOL;
				ok = 1;
			}
		}
		else {
			while (	(ok = NextStreamTokenObtained())
			&&	!TOKEN_EQ(lex_token, EOL)
			) {
				/* skip */
			}
		}
		break;
	}

	return ok;
}

void
CloseText(enum Pass pass, struct text *txt) {
	switch (pass) {
	case First:
		if (nl_buff) {
			if (last_tk_cnt != lex_tk_cnt) {
				/* there were tokens after the last newline */
				store_newline();
			}
			txt->tx_nl_limit = nl_free;
		}
		break;
	case Second:
		break;
	}
	CloseStream();
}

/*							NEWLINE CACHING */

/*	To speed up pass2 which is interested in token positions at line ends,
	the newline buffer keeps this info from pass1. To reduce the size of
	the newline buffer, the info is kept as the differences of the values
	at consecutive line ends. This allows unsigned chars to be used rather
	than integers.

	The recording of token position differences at EOL is optional, and
	is switched off if
	-	there is not room enough for the newline buffer.
	-	a difference would not fit in the field in the struct.
	Switching off is done by freeing the buffer and setting nl_buff to 0.
	Anybody using nl_buff should therefore test for nl_buff being zero.
*/

static void abandon_nl_buff(void);

static void
init_nl_buff(void) {
	/* Allocate the newline buffer, if possible */
	nl_size = 0 + NL_INCR;
	nl_buff = (option_set('x') ? 0 :
		(struct newline *)malloc(sizeof (struct newline) * nl_size)
	);
}

static void
store_newline(void) {
	if (!nl_buff) return;

	if (nl_free == nl_size) {
		/* allocated array is full; try to increase its size */
		unsigned int new_size = nl_size + NL_INCR;
		struct newline *new_buff = (struct newline *)realloc(
			(char *)nl_buff,
			sizeof (struct newline) * new_size
		);

		if (!new_buff) {
			/* we failed */
			abandon_nl_buff();
			return;
		}
		nl_buff = new_buff, nl_size = new_size;
	}

	/* now we are sure there is room enough */
	{
		register struct newline *nl = &nl_buff[nl_free++];
		register unsigned int tk_diff = lex_tk_cnt - last_tk_cnt;

		nl->nl_tk_diff = tk_diff;
		if (nl->nl_tk_diff != tk_diff) {
			/* tk_diff does not fit in nl_tk_diff */
			abandon_nl_buff();
		}
	}
}

static void
abandon_nl_buff(void) {
	if (nl_buff) {
		free((char *)nl_buff);
		nl_buff = 0;
	}
}

#ifdef	DB_NL_BUFF

void
db_print_nl_buff(unsigned int start, unsigned int limit) {
	int i;

	fprintf(DebugFile, "\n**** DB_NL_BUFF ****\n");
	if (!nl_buff) {
		fprintf(DebugFile, ">>>> NO NL_BUFF\n\n");
		return;
	}

	if (start > nl_free) {
		fprintf(DebugFile, ">>>> start (%u) > nl_free (%u)\n\n",
			start, nl_free
		);
		return;
	}
	if (limit > nl_free) {
		fprintf(DebugFile, ">>>> limit (%u) > nl_free (%u)\n\n",
			limit, nl_free
		);
		return;
	}

	fprintf(DebugFile, "nl_buff: %u entries:\n", nl_free);
	for (i = start; i < limit; i++) {
		struct newline *nl = &nl_buff[i];

		fprintf(DebugFile, "nl_tk_diff = %d\n", nl->nl_tk_diff);
	}
	fprintf(DebugFile, "\n");
}

#endif	/* DB_NL_BUFF */
