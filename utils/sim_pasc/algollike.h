/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: algollike.h,v 1.1 1997/06/20 12:03:11 dick Exp $
*/

/*	The class Algollike is a subclass of Language.  It implements
	the routines InitLanguage, MayBeStartOfRun and CheckRun for
	ALGOL-like languages, in which it is meaningful and useful to
	isolate function bodies.

	It requires the user to define, preferably in Xlang.l, four token
	sets, represented as TOKEN[] and terminated by NOTOKEN:

	TOKEN NonFinals[]	tokens that may not end a chunk
	TOKEN NonInitials[]	tokens that may not start a chunk
	TOKEN Openers[]		openers of parentheses that must balance
					in functions
	TOKEN Closers[]		the corresponding closers, in the same order
*/

#include	"language.h"
#include	"token.h"

extern const TOKEN NonFinals[];
extern const TOKEN NonInitials[];
extern const TOKEN Openers[];
extern const TOKEN Closers[];
