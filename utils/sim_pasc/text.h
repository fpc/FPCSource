/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: text.h,v 1.2 2001/09/28 09:03:56 dick Exp $
*/

/*	Implements the access to the lexical scanner.
	Additionally, the module tries to save newline information,
	anticipating a second scan which is interested in this
	information only.
*/

extern void InitText(int nfiles);
enum Pass {First, Second};
extern int OpenText(enum Pass pass, struct text *txt);
extern int NextTextTokenObtained(enum Pass pass);
extern void CloseText(enum Pass pass, struct text *txt);

#ifdef	DB_NL_BUFF
extern void db_print_nl_buff(unsigned int start, unsigned int limit);
#endif
