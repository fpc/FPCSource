/*
 * man2html.c -- convert nroff output to html
 *
 * Convert backspace/overstrikes to bold.
 * Convert backspace/underbar (either order) to italic.
 * Convert bar/backspace/dash to `+' (also handle bold rendering)
 * Convert bar/backspace/equals to `*' (also handle bold rendering)
 * Convert plus/backspace/o to `o'
 * Convert `<' to `&lt';  `>' to `&gt';  and `&' to `&amp'
 * If -u specified, compress duplicate blank lines.
 *
 * $Id: man2html.c,v 1.1 2000/07/13 06:30:22 michael Exp $
 */

#include <stdio.h>

#define BUFSIZE 4096

main(argc, argv)
int argc;
char *argv[];
{
	char buf[BUFSIZE];
	static int blank = 1, uopt = 0, lineno = 0, bksp = 0;

	/*
	 * Options.
 	 */
	if (argc > 2)
		usage();
	if (argc == 2) {
		if (strcmp(argv[1], "-u") == 0)
			uopt = 1;
		else
			usage();
	}

	/*
	 * Enclose in html envelope 
	 */
	puts("<BODY><PRE>");

	/*
	 * Process each line.  Count unresolved backspaces and issue
	 * a warning message at the end.
	 */
	while (fgets(buf, BUFSIZE, stdin)) {
	
		lineno++;

		/* 
		 * if -u specified, compress duplicate blank lines 
		 */
		if (uopt) {
			if (blankline(buf)) {
				if (blank)
					continue;
				else
					blank = 1;
			} else
				blank = 0;
		}

		bksp += html(buf, lineno);
	}

	/*
	 * Close html envelope.
	 */
	puts("</PRE></BODY>");

	/*
	 * Warn about any unresolved backspaces.
	 */
	if (bksp > 0)
		fprintf(stderr, 
		    "man2html: warning: %d unresolved backspaces\n", bksp);

	exit(0);
}

usage()
{
	fprintf(stderr, "man2html: usage: man2html [-u]\n");
	exit(1);
}

/*
 * Given a line, print it out as html.
 * Return the number of unprocessed backspaces in this line.
 */
int
html(s, lineno)
char *s;
int lineno;
{
	char *p;
	int bold, italic, bksp;
	unsigned char buf2[BUFSIZE];

	/* 
	 * two bits in each element of buf2 indicate attributes of 
	 * corresponding character in `s'.
	 *    bit 1, when set, indicates bold
	 *    bit 2, when set, indicates underscore
	 */

	/* 
	 * pass 1: set character attributes (and delete overstrikes)
	 */
	bzero(buf2, BUFSIZE);
	for (p = s; *p; p++) {

		if (p == s || *p != '\b')
			continue;

		/* detect a backspace/overstrike (bold <B>) */
		if (*(p - 1) == *(p + 1)) {

			/* get rid of backspace and overstrike */
			strcpy(p - 1, p + 1);

			/* flag character as bold */
			buf2[p - 1 - s] |= 1;
			
			p--;

		/* detect an underbar/backspace (italic <I>) */ 
		} else if (*(p - 1) == '_') {

			/* get rid of backspace and underbar */
			strcpy(p - 1, p + 1);

			/* flag character as underscored */
			buf2[p - 1 - s] |= 2;

			p--;

		/* detect a backspace/underbar (reverse of above) */
		} else if (*(p + 1) == '_') {

			/* get rid of backspace and underbar */
			strcpy(p, p + 2);

			/* flag character as underscored */
			buf2[p - 1 - s] |= 2;

			p--;

		/* convert bar/backspace/dash to `+' */
		} else if (strncmp(p - 1, "|\b-", 3) == 0
		    ||     strncmp(p - 1, "+\b-", 3) == 0
		    ||     strncmp(p - 1, "+\b|", 3) == 0) {

			/* get rid of backspace and bar, change dash to `+' */
			strcpy(p - 1, p + 1);
			*(p - 1) = '+';

			p--;

 		/* convert bar/backspace/equals to `*' */
		} else if (strncmp(p - 1, "|\b=", 3) == 0
		    ||     strncmp(p - 1, "*\b=", 3) == 0
		    ||     strncmp(p - 1, "*\b|", 3) == 0) {

			strcpy(p - 1, p + 1);
			*(p - 1) = '*';

			p--;

 		/* convert plus/backspace/o to bold `o' */
		} else if (strncmp(p - 1, "+\bo", 3) == 0
		    ||     strncmp(p - 1, "o\b+", 3) == 0) {

			strcpy(p - 1, p + 1);
			*(p - 1) = 'o';

			/* flag character as bold */
			buf2[p - 1 - s] |= 1;

			p--;
		}
	}

	/*
	 * pass 2:  print out line as html
	 */
	bold = italic = bksp = 0;
	for (p = s; *p; p++) {

		/* bold */
		if (buf2[p - s] & 1) {
			if (!bold) {

				/* 
				 * an overstrike/underbar is ambiguous.
				 * change to italic if we are in an italic
				 * context right now
				 */

				if (italic && *p == '_')
					buf2[p - s] |= 2;
				else {
					fputs("<B>", stdout);	
					bold = 1;
				}
			}
		} else {
			if (bold && *p != ' ') {
				fputs("</B>", stdout);
				bold = 0;
			}
		}

		/* italic */
		if (buf2[p - s] & 2) {
			if (!italic) {
				fputs("<I>", stdout);
				italic = 1;
			}
		} else {
			if (italic && *p != ' ') {
				fputs("</I>", stdout);
				italic = 0;
			}
		}

		/* print the char, escaping the three html special chars */
		switch (*p) {
		case '<':
			fputs("&lt", stdout);
			break;
		case '>':
			fputs("&gt", stdout);
			break;
		case '&':
			fputs("&amp", stdout);
			break;
		case '\b':
#ifdef notdef
			fprintf(stderr,
			   "man2html: warning, \\b on line %d\n", lineno); 
#endif
			bksp++;
		default:
			putchar(*p);
		}
	}

	if (bold)
		fputs("</B>", stdout);
	if (italic)
		fputs("</I>", stdout);

	return(bksp);
}

int
blankline(s)
char *s;
{
	while (*s) {
		if (!isspace(*s))
			return(0);
		s++;
	}
	return(1);
}


