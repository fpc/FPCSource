/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: language.h,v 1.1 1997/06/20 12:03:15 dick Exp $
*/

/*	The abstract class Language contains the routines InitLanguage,
	MayBeStartOfRun and CheckRun which describe in some sense the
	language and which are required by compare.c.
	
	These routines must be provided by all Xlang.l files.
*/

#include	"token.h"

extern void InitLanguage(void);
extern int MayBeStartOfRun(TOKEN ch);
extern unsigned int CheckRun(const TOKEN *str, unsigned int size);
