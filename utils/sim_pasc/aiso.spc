/*
	Module:	Arbitrary-In Sorted-Out (AISO)
	Author:	dick@cs.vu.nl (Dick Grune @ Vrije Universiteit, Amsterdam)
	Version:	Tue Aug 23 12:54:22 1988

Description:
	This is the specification of a generic module that builds an
	arbitrary-in sorted-out data structure, to be used as a heap, a
	priority queue, etc. Elements can be inserted, the first element
	extracted and the set scanned at any moment.

Instantiation:
	The module is instantiated as follows.
	Create a file M.h for some M, which contains at least:
	-	a definition of AISO_TYPE, the type of the object to be stored
	-	a possible definition of AISO_EXTRACTOR; see below
	-	a possible definition of AISO_ITERATOR; see below
	-	#include	"aiso.spc"

	This file M.h is to be included in all files that use the aiso
	package.

	Create a file M.c which contains at least:
	-	#include	"M.h"
	-	a definition of a routine
			int AISO_BEFORE(AISO_TYPE v, AISO_TYPE w)
		which yields non-zero if v is to be sorted before w
	-	#include	"aiso.bdy"

	This file compiles into the module object.

Specification:
	The module always supplies:
	int InsertAiso(AISO_TYPE value)
		inserts value in its proper place; fails if out of memory

	If AISO_EXTRACTOR is defined, the module will also supply:
	int ExtractAiso(AISO_TYPE *value)
		yields the first value in the aiso and removes it;
		fails if empty

	If AISO_ITERATOR is defined, the module also supplies a type AisoIter
	which declares an iterator, i.e., a structure that records a position
	in the ordered set, plus routines for manipulating the iterator, thus
	enabling the user to scan the ordered set.  The iterator should be
	declared as:
		AisoIter iter;
	and is manipulated by the following commands:

	void OpenIter(AisoIter *iter)
		opens the iterator for scanning the existing set in order

	int GetAisoItem(AisoIter *iter, AISO_TYPE *value)
		yields the next value in the iterator; fails if exhausted

	void CloseIter(AisoIter *iter)
		closes the iterator

	If AISO_DEBUG is defined the module will also supply:
	void PrintAisoTree(void)
		prints the AISO tree; requires AISO_FORMAT, to be set to
		a format suitable to print a value of type AISO_TYPE

Implementation:
	The AISO implementation is based on a self-adjusting binary tree.
	Degenerate behaviour of the tree is avoided by shaking the tree
	every 'ln aiso_size' node accesses.  This guarantees ln aiso_size
	behaviour in the long run, though it is possible for a single
	operation to take aiso_size node accesses.

	The iterator is implemented as an additional linear linked list
	through the tree.  This is simpler than and at least as efficient as
	clever tree-wiring.

Restrictions:
	Due to built-in fixed names, there can only be one AISO per program.
*/

struct aiso_node {
	struct aiso_node *an_left;
	struct aiso_node *an_right;
#ifdef	AISO_ITERATOR
	struct aiso_node *an_next;
#endif	/* AISO_ITERATOR */
	AISO_TYPE an_value;
};

extern int InsertAiso(AISO_TYPE value);
#ifdef	AISO_EXTRACTOR
extern int ExtractAiso(AISO_TYPE *value);
#endif	/* AISO_EXTRACTOR */

#ifdef	AISO_ITERATOR
typedef	struct aiso_node *AisoIter;
extern void OpenIter(AisoIter *iter);
extern int GetAisoItem(AisoIter *iter, AISO_TYPE *value);
extern void CloseIter(AisoIter *iter);
#endif	/* AISO_ITERATOR */

#ifdef	AISO_DEBUG
extern void PrintAisoTree(void);
#endif	/* AISO_ITERATOR */
