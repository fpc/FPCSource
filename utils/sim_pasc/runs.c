/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: runs.c,v 1.2 2001/11/08 12:30:30 dick Exp $
*/

#include	"sim.h"
#include	"runs.h"

#define	AISO_BEFORE(r0,r1)	((r0)->rn_size > (r1)->rn_size)

#include	"aiso.bdy"
