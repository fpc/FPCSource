/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: options.h,v 1.3 2001/11/13 12:55:53 dick Exp $
*/

/*	Setting and consulting command line options
*/

struct option {
	char op_char;		/* char as in call */
	char *op_text;		/* elucidating text */
	char op_indicator;	/* type indicator, N = int, F = file name */
	const char **op_stringp;/* string value to be picked up */
};

extern int option_set(char ch);
extern int do_options(
	const char *progname, const struct option *optlist,
	int argc, char *argv[]
);
