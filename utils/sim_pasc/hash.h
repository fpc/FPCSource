/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: hash.h,v 1.1 1997/06/20 12:03:14 dick Exp $
*/

/*	Creating and consulting the ForwardReference array; to speed up
	the Longest Substring Allgorithm.
*/

extern void MakeForwardReferences(void);
extern void FreeForwardReferences(void);
extern unsigned int ForwardReference(int i);
