/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: pass3.c,v 2.11 2005/02/20 17:03:03 dick Exp $
*/

#include	<stdio.h>
#include	<string.h>
#include	<malloc.h>

#include	"system.par"
#include	"debug.par"
#include	"sim.h"
#include	"runs.h"
#include	"error.h"
#include	"options.h"
#include	"pass3.h"
#include	"percentages.h"

#ifdef	DB_RUN
#include	"tokenarray.h"
static void db_run(const struct run *);
#endif

static FILE *open_chunk(const struct chunk *);
static void fill_line(FILE *, char []);
static void clear_line(char []);
static void show_runs(void);
static void show_run(const struct run *);
static void show_2C_line(const char [], const char []);
static void show_1C_line(FILE *, const char *);
static int prhead(const struct chunk *);
static int prs(const char *);
static int pru(unsigned int);
static int unslen(unsigned int);

static int maxline;			/* Actual maximum line length */
static char *line0;			/* by malloc() */
static char *line1;

void
Pass3(void) {
	if (option_set('p')) {
		show_percentages();
	}
	else {
		show_runs();
	}
}

static void
show_runs(void) {
	AisoIter iter;
	struct run *run;

	maxline = PageWidth / 2 - 2;
	line0 = malloc((unsigned int)((maxline + 1) * sizeof (char)));
	line1 = malloc((unsigned int)((maxline + 1) * sizeof (char)));
	if (!line0 || !line1) fatal("out of memory");

	OpenIter(&iter);
	while (GetAisoItem(&iter, &run)) {
#ifdef	DB_RUN
		db_run(run);
#endif	/* DB_RUN */
		show_run(run);
		fprintf(OutputFile, "\n");
	}
	CloseIter(&iter);

	free(line0); line0 = 0;
	free(line1); line1 = 0;
}

static void
show_run(const struct run *run) {
	/* The animals came in two by two ... */
	register const struct chunk *cnk0 = &run->rn_cn0;
	register const struct chunk *cnk1 = &run->rn_cn1;
	register unsigned int nl_cnt0 =
			cnk0->ch_last.ps_nl_cnt - cnk0->ch_first.ps_nl_cnt;
	register unsigned int nl_cnt1 =
			cnk1->ch_last.ps_nl_cnt - cnk1->ch_first.ps_nl_cnt;
	FILE *f0;
	FILE *f1;

	/* display heading of chunk */
	if (!option_set('d')) {
		/* no assumptions about the lengths of the file names! */
		register unsigned int size = run->rn_size;
		register int pos = 0;

		pos += prhead(cnk0);
		while (pos < maxline + 1) {
			pos += prs(" ");
		}
		pos += prs("|");
		pos += prhead(cnk1);
		while (pos < 2*maxline - unslen(size)) {
			pos += prs(" ");
		}
		fprintf(OutputFile, "[%u]\n", size);
	}
	else {
		(void)prhead(cnk0);
		fprintf(OutputFile, "\n");
		(void)prhead(cnk1);
		fprintf(OutputFile, "\n");
	}

	/* stop if that suffices */
	if (option_set('n'))
		return;			/* ... had enough so soon ... */

	/* open the files that hold the chunks */
	f0 = open_chunk(cnk0);
	f1 = open_chunk(cnk1);

	/* display the chunks in the required format */
	if (!option_set('d')) {
		/* fill 2-column lines and print them */
		while (nl_cnt0 != 0 || nl_cnt1 != 0) {
			if (nl_cnt0) {
				fill_line(f0, line0);
				nl_cnt0--;
			}
			else {
				clear_line(line0);
			}
			if (nl_cnt1) {
				fill_line(f1, line1);
				nl_cnt1--;
			}
			else {
				clear_line(line1);
			}
			show_2C_line(line0, line1);
		}
	}
	else {
		/* display the lines in a diff(1)-like format */
		while (nl_cnt0--) {
			show_1C_line(f0, "<");
		}
		fprintf(OutputFile, "---\n");
		while (nl_cnt1--) {
			show_1C_line(f1, ">");
		}
	}

	/* close the pertinent files */
	fclose(f0);
	fclose(f1);
}

static int
prhead(const struct chunk *cnk) {
	register int pos = 0;

	pos += prs(cnk->ch_text->tx_fname);
	pos += prs(": line ");
	pos += pru(cnk->ch_first.ps_nl_cnt);
	pos += prs("-");
	pos += pru(cnk->ch_last.ps_nl_cnt - 1);
	return pos;
}

static int
prs(const char *str) {
	fprintf(OutputFile, "%s", str);
	return strlen(str);
}

static int
pru(unsigned int u) {
	fprintf(OutputFile, "%u", u);
	return unslen(u);
}

static int
unslen(unsigned int u) {
	register int res = 1;

	while (u > 9) {
		u /= 10, res++;
	}
	return res;
}

static FILE *
open_chunk(const struct chunk *cnk) {
	/*	opens the file in which the chunk resides, positions the
		file at the beginning of the chunk and returns the file pointer
	*/
	register char *fname = cnk->ch_text->tx_fname;
	register FILE *f = fopen(fname, "r");
	register unsigned int nl_cnt;

	if (!f) {
		fprintf(stderr, ">>>> File %s disappeared <<<<\n", fname);
		f = fopen(NULLFILE, "r");
	}

	nl_cnt = cnk->ch_first.ps_nl_cnt;
	while (nl_cnt > 1) {
		int ch = getc(f);

		if (ch < 0) break;
		if (ch == '\n') {
			nl_cnt--;
		}
	}

	return f;
}

static void
fill_line(FILE *f, char ln[]) {
	/*	Reads one line from f and puts it in condensed form in ln.
	*/
	register int indent = 0, lpos = 0;
	register int ch;

	/* condense and skip initial blank */
	while ((ch = getc(f)), ch == ' ' || ch == '\t') {
		if (ch == '\t') {
			indent = 8;
		}
		else {
			indent++;
		}
		if (indent == 8) {
			/* every eight blanks give one blank */
			if (lpos < maxline) {
				ln[lpos++] = ' ';
			}
			indent = 0;
		}
	}

	/* store the rest */
	while (ch >= 0 && ch != '\n') {
		if (ch == '\t') {
			/* replace tabs by blanks */
			ch = ' ';
		}
		if (lpos < maxline) {
			ln[lpos++] = ch;
		}
		ch = getc(f);
	}
	ln[lpos] = '\0';		/* always room for this one */
}

static void
clear_line(char ln[]) {
	/* a simple null byte will suffice */
	ln[0] = '\0';
}

static void
show_2C_line(const char ln0[], const char ln1[]) {
	/*	displays the contents of the two lines in a two-column
		format
	*/
	register int i;

	for (i = 0; i < maxline && ln0[i] != '\0'; i++) {
		fputc(ln0[i], OutputFile);
	}
	for (; i < maxline; i++) {
		fputc(' ', OutputFile);
	}
	fprintf(OutputFile, " |");

	for (i = 0; i < maxline && ln1[i] != '\0'; i++) {
		fputc(ln1[i], OutputFile);
	}
	fprintf(OutputFile, "\n");
}

static void
show_1C_line(FILE *f, const char *marker) {
	/*	displays one line from f, preceded by the marker
	*/
	register int ch;

	fprintf(OutputFile, "%s", marker);
	while ((ch = getc(f)), ch > 0 && ch != '\n') {
		fputc(ch, OutputFile);
	}
	fputc('\n', OutputFile);
}

#ifdef	DB_RUN

static void db_chunk(const struct chunk *);

static void
db_run(const struct run *run) {
	/* prints detailed data about a run */
	register const struct chunk *cnk0 = &run->rn_cn0;
	register const struct chunk *cnk1 = &run->rn_cn1;

	fprintf(DebugFile, "File %s / file %s:\n",
		cnk0->ch_text->tx_fname,
		cnk1->ch_text->tx_fname
	);
	fprintf(DebugFile, "from token %u/%u to %u/%u:",
		cnk0->ch_first.ps_tk_cnt, cnk1->ch_first.ps_tk_cnt,
		cnk0->ch_last.ps_tk_cnt, cnk1->ch_last.ps_tk_cnt
	);
	fprintf(DebugFile, " from lines %u/%u to %u/%u:",
		cnk0->ch_first.ps_nl_cnt, cnk1->ch_first.ps_nl_cnt,
		cnk0->ch_last.ps_nl_cnt, cnk1->ch_last.ps_nl_cnt
	);
	fprintf(DebugFile, " %u %s\n",
		run->rn_size,
		(run->rn_size == 1 ? "token" : "tokens")
	);

	db_chunk(cnk0);
	db_chunk(cnk1);
}

static void
db_chunk(const struct chunk *cnk) {
	/*	print the tokens in the chunk, with a one-char margin
	*/
	unsigned int i;
	const struct position *first = &cnk->ch_first;
	const struct position *last = &cnk->ch_last;
	unsigned int start = cnk->ch_text->tx_start;

	if (first->ps_tk_cnt > 0) {
		fprintf(DebugFile, "...");
		print_token(stdout, TokenArray[start + first->ps_tk_cnt - 1]);
		fprintf(DebugFile, "  ");
	}
	else {	/* create same offset as above */
		fprintf(DebugFile, "       ");
	}

	for (i = first->ps_tk_cnt; i <= last->ps_tk_cnt; i++) {
		print_token(stdout, TokenArray[start + i]);
	}

	if (start + last->ps_tk_cnt + 1 < cnk->ch_text->tx_limit) {
		fprintf(DebugFile, "  ");
		print_token(stdout, TokenArray[start + last->ps_tk_cnt + 1]);
		fprintf(DebugFile, "...");
	}

	fprintf(DebugFile, "\n");
}

#endif	/* DB_RUN */
