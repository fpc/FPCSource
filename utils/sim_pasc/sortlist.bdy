/*
	Module:	Sort Linked Lists
	Author:	dick@cs.vu.nl (Dick Grune @ Vrije Universiteit, Amsterdam)
	Version:	Tue Sep 17 17:32:33 1991

Description:
	This is the implementation part of a generic routine that sorts
	linked lists.

Instantiation:
	See sortlist.spc
*/

#ifndef	_SORT_EXTERN_DEFINED
static
#endif
void
SORT_NAME(struct SORT_STRUCT **lh) {
	/*	I've  never known that sorting a linked list was this
		complicated; what am I missing?
	*/
	register struct SORT_STRUCT **listhook = lh;

	while (*listhook) {
		/* 0. the list is not empty -> there must be a smallest one */
		register struct SORT_STRUCT **hsmall;

		/* 1. find (the pointer to) the smallest element */
		{
			register struct SORT_STRUCT **hook = listhook;

			/* assume initially that first element is smallest */
			hsmall = hook;
			while (*hook) {
				if (SORT_BEFORE(*hook, *hsmall)) {
					/* revise opinion */
					hsmall = hook;
				}
				hook = &(*hook)->SORT_NEXT;
			}
		}

		/* 2. move the smallest element to front */
		{
			register struct SORT_STRUCT *smallest = *hsmall;

			/* remove it from the chain */
			*hsmall = smallest->SORT_NEXT;
			/* and insert it before the first element */
			smallest->SORT_NEXT = *listhook;
			*listhook = smallest;
		}

		/* 3. skip over smallest element */
		listhook = &(*listhook)->SORT_NEXT;
	}
}
