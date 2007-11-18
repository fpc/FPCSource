/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: token.h,v 2.4 2001/11/13 12:55:59 dick Exp $
*/

/*
	Token interface.
	Since the definition of a token has been a continual source of
	problems, it is now defined as an Abstract Data Type.
	To allow stronger type checking, there is a special version for use
	by lint.
*/

#include	<stdio.h>

#ifndef	TOKEN

#ifdef	lint
#define	TESTTOKEN
#endif

#ifdef	TESTTOKEN				/* strict version */

struct cccc {
	int cccc;
};

typedef struct cccc *lintTOKEN;
#define	TOKEN		lintTOKEN
#define	TOKEN2int(c)	((int)(c))
#define	int2TOKEN(i)	((TOKEN)(i))
extern int TOKEN_EQ(TOKEN t1, TOKEN t2);

#else						/* production version */

#define	TOKEN		unsigned char
#define	TOKEN2int(c)	((c)&0377)
#define	int2TOKEN(i)	((TOKEN)(i))
#define	TOKEN_EQ(t1,t2)	(TOKEN2int(t1) == TOKEN2int(t2))

#endif	/* TESTTOKEN */

#endif	/* TOKEN */

/* Macros for the composition of tokens */
#define	NORM(ch)	int2TOKEN((ch)&0377)
#define	CTRL(ch)	int2TOKEN((ch)&0037)
#define	META(ch)	int2TOKEN((ch)|0200)
#define	MTCT(ch)	int2TOKEN(((ch)&0037)|0200)
#define	NOTOKEN		int2TOKEN(0)

extern void print_token(FILE *ofile, TOKEN tk);	/* in two characters */
