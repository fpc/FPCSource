/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: tokenarray.h,v 1.1 2001/09/28 09:03:42 dick Exp $
*/

#include	"token.h"

/* Interface for the token storage */
extern void InitTokenArray(void);
extern void StoreToken(void);
extern unsigned int TextLength(void);	/* also first free token position */
extern TOKEN *TokenArray;

