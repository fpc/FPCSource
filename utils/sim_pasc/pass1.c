/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: pass1.c,v 2.8 2007/08/27 09:57:32 dick Exp $
*/

#include	<stdio.h>
#include	<string.h>

#include	"debug.par"
#include	"sim.h"
#include	"text.h"
#include	"tokenarray.h"
#include	"lex.h"
#include	"error.h"
#include	"pass1.h"

#ifdef	DB_TEXT
static void db_print_text(const struct text *);
#endif

static void print_count(unsigned int cnt, const char *);

void
Pass1(int argc, char *argv[]) {
	register int n;

	InitText(argc);
	InitTokenArray();

	/* assume all texts to be new */
	NumberOfNewTexts = NumberOfTexts;

	/* read the files */
	for (n = 0; n < NumberOfTexts; n++) {
		register char *fname = argv[n];
		register struct text *txt = &Text[n];

		fprintf(OutputFile, "File %s: ", fname);

		txt->tx_fname = fname;
		txt->tx_pos = 0;
		txt->tx_start =
		txt->tx_limit = TextLength();
		if (strcmp(fname, "/") == 0) {
			fprintf(OutputFile, "separator\n");
			NumberOfNewTexts = n;
		}
		else {
			if (!OpenText(First, txt)) {
				fprintf(OutputFile, ">>>> cannot open <<<< ");
				/*	the file has still been opened
					with a null file for uniformity
				*/
			}
			while (NextTextTokenObtained(First)) {
				if (!TOKEN_EQ(lex_token, EOL)) {
					StoreToken();
				}
			}
			CloseText(First, txt);
			txt->tx_limit = TextLength();

			/* report */
			print_count(txt->tx_limit - txt->tx_start, "token");
			if (lex_non_ascii_cnt) {
				fprintf(DebugFile, ", ");
				print_count(lex_non_ascii_cnt,
					"non-ASCII character"
				);
			}
			fprintf(OutputFile, "\n");
#ifdef	DB_TEXT
			db_print_text(txt);
#endif	/* DB_TEXT */
		}
		fflush(OutputFile);
	}

	/* report total */
	fprintf(OutputFile, "Total: ");
	print_count(TextLength() - 1, "token");
	fprintf(OutputFile, "\n\n");
	fflush(OutputFile);
}

static void
print_count(unsigned int cnt, const char *unit) {
	/*	Prints a grammatically correct string "%u %s[s]"
		for units that form their plural by suffixing -s.
	*/
	fprintf(OutputFile, "%u %s%s", cnt, unit, (cnt == 1 ? "" : "s"));
}

#ifdef	DB_TEXT

static void
db_print_text(const struct text *txt) {
	/* prints a text (in compressed form) */
	register int i;

	fprintf(DebugFile, "\n\n**** DB_PRINT_TEXT ****\n");

	fprintf(DebugFile, "File \"%s\", %u tokens, ",
		txt->tx_fname, txt->tx_limit - txt->tx_start
	);
	fprintf(DebugFile, "txt->tx_start = %u, txt->tx_limit = %u\n",
		txt->tx_start, txt->tx_limit
	);

	for (i = txt->tx_start; i < txt->tx_limit; i++) {
		if ((i - txt->tx_start + 1) % 32 == 0) {
			fprintf(DebugFile, "\n");
		}
		print_token(stdout, TokenArray[i]);
	}
	fprintf(DebugFile, "\n");
}

#endif	/* DB_TEXT */
