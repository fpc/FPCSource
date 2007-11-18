/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: idf.h,v 2.5 1998/02/03 14:28:25 dick Exp $
*/

/*	Idf module:
	TOKEN idf_in_list(char *str, struct idf l[], sizeof l, TOKEN dflt);
		looks up a keyword in a list of keywords l, represented as an
		array of struct idf, and returns its translation as a token;
		dflt is returned if the keyword is not found.
	TOKEN idf_hashed(char *str);
		returns a token unequal to SKIP or EOL, derived from the str
		through hashing
	It is assumed that SKIP will be ignored by the user of this module.
*/

#include	"token.h"

/* the struct for keywords etc. */
struct idf {
	char *id_tag;	/* an interesting identifier */
	TOKEN id_tr;	/* with its one-token translation */
};

/* special tokens for the idf module */
#define	SKIP		NORM('\0')
#define	IDF		NORM('@')

/* public functions */
extern TOKEN idf_in_list(const char *, const struct idf [], unsigned int, TOKEN);
extern TOKEN idf_hashed(const char *);
