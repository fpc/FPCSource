/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: add_run.h,v 1.1 2001/09/28 09:03:39 dick Exp $
*/

/*	Interface between front-end and back-end: all information about
	runs passes through add_run().  Its parameters are the two chunks,
	each identified by their struct text and the position of the common
	segment in TokenArray[], and the number of tokens in the common
	segment.
*/

void add_run(
	struct text *txt0,		/* text of first chunk */
	unsigned int i0,		/* chunk position in TokenArray[] */
	struct text *txt1,		/* text of second chunk */
	unsigned int i1,		/* chunk position in TokenArray[] */
	unsigned int size		/* number of tokens in the chunk */
);
