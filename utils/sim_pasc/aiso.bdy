/*
	Module:	Arbitrary-In Sorted-Out (AISO)
	Author:	dick@cs.vu.nl (Dick Grune @ Vrije Universiteit, Amsterdam)

Description:
	This is the body of a module that builds an arbitrary-in
	sorted-out data structure, to be used as a heap, a priority queue, etc.
	See aiso.spc for further info.
*/

#include	<malloc.h>

static struct aiso_node *root;		/* root of tree */
#ifdef	AISO_ITERATOR
static struct aiso_node *list;		/* start of linked list */
#endif	/* AISO_ITERATOR */

/* the policy */
static int aiso_size = 0;
static int access_mark = 1;

#define	add_entry()	(aiso_size++)
#define	remove_entry()	(aiso_size--)
#define	reset_access()	(access_mark = 1)
#define	count_access()	(access_mark <<= 1)
#define	must_rotate()	(access_mark > aiso_size)

int
InsertAiso(AISO_TYPE v) {
	register struct aiso_node *new_node;
	register struct aiso_node **hook = &root;
#ifdef	AISO_ITERATOR
	register struct aiso_node **prev = &list;
#endif	/* AISO_ITERATOR */

	new_node = (struct aiso_node *)malloc(sizeof (struct aiso_node));
	if (!new_node) {
		/* avoid modifying the tree */
		return 0;
	}

	while (*hook) {
		register struct aiso_node *an = *hook;

		count_access();
		if (AISO_BEFORE(v, an->an_value)) {
			/* head left */
			if (!an->an_left || !must_rotate()) {
				/* standard action */
				hook = &an->an_left;
			}
			else {
				/* change (l A r) B (C) into (l) A (r B C) */
				register struct aiso_node *anl = an->an_left;

				an->an_left = anl->an_right;
				anl->an_right = an;
				*hook = anl;
				reset_access();
			}
		}
		else {
			/* head right */
			if (!an->an_right || !must_rotate()) {
				/* standard action */
				hook = &an->an_right;
			}
			else {
				/* change (A) B (l C r) into (A B l) C (r) */
				register struct aiso_node *anr = an->an_right;

				an->an_right = anr->an_left;
				anr->an_left = an;
				*hook = anr;
				reset_access();
			}
#ifdef	AISO_ITERATOR
			prev = &an->an_next;
#endif	/* AISO_ITERATOR */
		}
	}

	new_node->an_left = 0;
	new_node->an_right = 0;
#ifdef	AISO_ITERATOR
	new_node->an_next = *prev;
	*prev = new_node;
#endif	/* AISO_ITERATOR */
	new_node->an_value = v;
	*hook = new_node;
	add_entry();
	return 1;
}

#ifdef	AISO_EXTRACTOR

int
ExtractAiso(AISO_TYPE *vp) {
	register struct aiso_node **hook = &root;
	register struct aiso_node *an;

	if (!root) return 0;

	while ((an = *hook), an->an_left) {
		/* head left */
		count_access();
		if (!must_rotate()) {
			/* standard action */
			hook = &an->an_left;
		}
		else {
			/* change (l A r) B (C) into (l) A (r B C) */
			register struct aiso_node *anl = an->an_left;

			an->an_left = anl->an_right;
			anl->an_right = an;
			*hook = anl;
			reset_access();
		}
	}
	/* found the first */
	*vp = an->an_value;
	*hook = an->an_right;
#ifdef	AISO_ITERATOR
	list = an->an_next;
#endif	/* AISO_ITERATOR */
	free((char *)an);
	remove_entry();
	return 1;
}

#endif	/* AISO_EXTRACTOR */

#ifdef	AISO_ITERATOR

void
OpenIter(AisoIter *ip) {
	*ip = list;
}

int
GetAisoItem(AisoIter *ip, AISO_TYPE *vp) {
	register struct aiso_node *an = *ip;

	if (!an) return 0;

	*vp = an->an_value;
	*ip = an->an_next;
	return 1;
}

void
CloseIter(AisoIter *ip) {
	*ip = 0;
}

#endif	/* AISO_ITERATOR */

#ifdef	AISO_DEBUG

#include	<stdio.h>

static void
print_inf(int level, char ch, struct aiso_node *an) {
	register int i;

	if (!an) return;

	print_inf(level+1, '/', an->an_right);
	for (i = 0; i < level; i++) {
		printf("     ");
	}
	printf("%c", ch);
	printf(AISO_FORMAT, an->an_value);
	printf("\n");
	print_inf(level+1, '\\', an->an_left);
}

void
PrintAisoTree(void)
{
	print_inf(0, '-', root);
	printf("================\n");
}

#endif	/* AISO_DEBUG */
