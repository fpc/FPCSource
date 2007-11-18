/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: error.c,v 2.4 1998/02/03 14:28:22 dick Exp $
*/

#include	<stdio.h>
#include	<stdlib.h>

#include	"sim.h"
#include	"error.h"

void
fatal(const char *msg) {
	fprintf(stderr, "%s: %s\n", progname, msg);
	exit(1);
}
