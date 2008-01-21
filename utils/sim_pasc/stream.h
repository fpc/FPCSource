/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: stream.h,v 2.4 1998/02/03 14:28:36 dick Exp $
*/

/*
	Interface of the stream module.

	Implements the direct interaction with the lexical
	module.  It supplies the routines below.
*/

#include	"token.h"

extern int OpenStream(const char *);
extern int NextStreamTokenObtained(void);
extern void CloseStream(void);
