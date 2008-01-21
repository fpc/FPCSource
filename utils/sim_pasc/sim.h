/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: sim.h,v 2.7 2005/02/20 17:03:03 dick Exp $
*/

#include	<stdio.h>

struct position {
	/* position of first and last token of a chunk */
	struct position *ps_next;
	int ps_type;		/* first = 0, last = 1 */
	unsigned int ps_tk_cnt;	/* in tokens; set by add_run() in Pass 1 */
	unsigned int ps_nl_cnt;	/* same, in line numbers; set by Pass2(),
				   used by Pass3() to report line numbers
				*/
};

struct text {
	char *tx_fname;		/* the file name */
	struct position *tx_pos;/* list of positions in this file that are
				   part of a chunk; sorted and updated by
				   Pass 2
				*/
	unsigned int tx_start;	/* positions in TokenArray[] for the text */
	unsigned int tx_limit;
	unsigned int tx_nl_start;/* possibly newline pointer for pass2 */
	unsigned int tx_nl_limit;
};

extern unsigned int MinRunSize;
extern int PageWidth;
extern FILE *OutputFile;
extern FILE *DebugFile;

extern struct text *Text;		/* Text[], one for each input file */
extern int NumberOfTexts;		/* number of text records */
extern int NumberOfNewTexts;		/* number of new text records */

extern char *progname;			/* for error reporting */
