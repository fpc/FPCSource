/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: sim.c,v 2.12 2007/08/27 09:57:34 dick Exp $
*/

#include	<stdio.h>
#include	<stdlib.h>

#include	"settings.par"
#include	"sim.h"
#include	"options.h"
#include	"language.h"
#include	"error.h"
#include	"hash.h"
#include	"compare.h"
#include	"pass1.h"
#include	"pass2.h"
#include	"pass3.h"
#include	"stream.h"
#include	"lex.h"

unsigned int MinRunSize = DFLT_MIN_RUN_SIZE;
int PageWidth = DFLT_PAGE_WIDTH;
FILE *OutputFile;
FILE *DebugFile;

struct text *Text;			/* to be filled in by malloc */
int NumberOfTexts;			/* number of text records */
int NumberOfNewTexts;			/* number of new text records */

char *progname;				/* for error reporting */

static const char *outputname;		/* for reporting */
static const char *minrunstring;
static const char *pagewidthstring;

static const struct option optlist[] = {
	{'r', "minimum run size", 'N', &minrunstring},
	{'w', "page width", 'N', &pagewidthstring},
	{'f', "function-like forms only", ' ', 0},
	{'d', "use diff format for output", ' ', 0},
	{'p', "use percentage format for output", ' ', 0},
	{'e', "compare each file to each file separately", ' ', 0},
	{'s', "do not compare a file to itself", ' ', 0},
	{'S', "compare new files to old files only", ' ', 0},
	{'F', "keep function identifiers in tact", ' ', 0},
	{'n', "display headings only", ' ', 0},
	{'x', "no pass2 nl_buff allocation", ' ', 0},
	{'o', "write output to file F", 'F', &outputname},
	{'-', "lexical scan output only", ' ', 0},
	{0, 0, 0, 0}
};

static void print_stream(const char *fname);

int
main(int argc, char *argv[]) {
	progname = argv[0];		/* save program name */
	argv++, argc--;			/* and skip it */

	/* Set the default output and debug streams */
	OutputFile = stdout;
	DebugFile = stdout;

	/* Get command line options */
	{	int nop = do_options(progname, optlist, argc, argv);
		argc -= nop, argv += nop;	/* skip them */
	}

	/* Treat the value options */
	if (minrunstring) {
		MinRunSize = strtoul(minrunstring, NULL, 10);
		if (MinRunSize == 0) fatal("bad or zero run size; form is: -r N");
	}
	if (pagewidthstring) {
		PageWidth = atoi(pagewidthstring);
		if (PageWidth == 0) fatal("bad or zero page width; form is: -w N");
	}
	if (outputname) {
		OutputFile = fopen(outputname, "w");
		if (OutputFile == 0) {
			char msg[500];

			sprintf(msg, "cannot open output file %s", outputname);
			fatal(msg);
			/*NOTREACHED*/
		}
	}

	if (option_set('-')) {
		/* it is the lexical scan only */
		while (argv[0]) {
			print_stream(argv[0]);
			argv++;
		}
		return 0;
	}

	/* Start processing */
	InitLanguage();

	/* Read the input files */
	Pass1(argc, argv);

	/* Set up the forward reference table */
	MakeForwardReferences();

	/* Compare the input files to find runs */
	Compare();

	/* Delete forward reference table */
	FreeForwardReferences();

	/* Find positions of the runs found */
	Pass2();

	/* Print the similarities */
	Pass3();

	return 0;
}

static void
print_stream(const char *fname) {
	fprintf(OutputFile, "File %s:", fname);
	if (!OpenStream(fname)) {
		fprintf(OutputFile, " cannot open\n");
		return;
	}

	fprintf(OutputFile, " showing token stream:\nnl_cnt, tk_cnt: tokens");

	lex_token = EOL;
	do {
		if (TOKEN_EQ(lex_token, EOL)) {
			fprintf(OutputFile, "\n%u,%u:",
				lex_nl_cnt, lex_tk_cnt
			);
		}
		else {
			print_token(OutputFile, lex_token);
		}
	} while (NextStreamTokenObtained());

	fprintf(OutputFile, "\n");

	CloseStream();

}
