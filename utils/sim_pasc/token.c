/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: token.c,v 2.4 2001/11/13 12:55:58 dick Exp $
*/

/*
	Token interface, implementation part.
*/

#include	<stdio.h>

#include	"token.h"

void
print_token(FILE *ofile, TOKEN tk) {
	/*	prints a token, in two characters:
			normal char		meta (bit 8 set)
			^A	cntl		$A	meta-cntl
			 A	printable	#A	meta
			^?	DEL		$?	meta-DEL
	*/
	register int ch =   TOKEN2int(tk) & 0177;
	register int meta = TOKEN2int(tk) & 0200;

	if (' ' <= ch && ch <= '~') {
		fprintf(ofile, "%c%c", (meta ? '#' : ' '), ch);
	}
	else {
		fprintf(ofile, "%c%c",
			(meta ? '$' : '^'),
			(ch == 0177 ? '?' : ch + '@')
		);
	}
}

#ifdef	TESTTOKEN

int
TOKEN_EQ(TOKEN t1, TOKEN t2) {
	/* to make sure TOKEN_EQ is indeed called with two TOKEN parameters */
	return TOKEN2int(t1) == TOKEN2int(t2);
}

#endif	/* TESTTOKEN */
