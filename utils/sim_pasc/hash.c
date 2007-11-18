/*	This file is part of the software similarity tester SIM.
	Written by Dick Grune, Vrije Universiteit, Amsterdam.
	$Id: hash.c,v 2.8 2005/02/20 17:03:00 dick Exp $
*/

/*	Text is compared by comparing every substring to all substrings
	to the right of it; this process is in essence quadratic.  However,
	only substrings of length at least 'MinRunSize' are of interest,
	which gives us the possibility to speed up this process by using
	a hash table.

	For every position in the text, we construct an index which gives
	the next position in the text at which a run of MinRunSize tokens
	starts that has the same hash code, as calculated by hash1().  If
	there is no such run, the index is 0.  These forward references are
	kept in the array forward_references[].

	To construct this array, we use a hash table last_index[] whose size
	is a prime and which is about 8 times smaller than the text array.
	The hash table last_index[] is set up such that last_index[i] is the
	index of the latest token with hash_code i, or 0 if there is none.
	This results in hash chains of an average length of 8.  See
	MakeForwardReferences().

	If there is not enough room for a hash table of the proper size
	(which can be considerable) the hashing is not efficient any more.
	In that case, the forward reference table is scanned a second time,
	eliminating from any chain all references to runs that do not hash to
	the same value under a second hash function, hash2().  For the UNIX
	manuals this reduced the number of matches from 91.9% to 1.9% (of
	which 0.06% was genuine).
*/

#include	<stdio.h>
#include	<malloc.h>

#include	"system.par"
#include	"debug.par"
#include	"sim.h"
#include	"error.h"
#include	"language.h"
#include	"token.h"
#include	"tokenarray.h"
#include	"options.h"
#include	"hash.h"

							/* MAIN ENTRIES */
static unsigned int *forward_references;	/* to be filled by malloc() */
static int n_forward_references;

static void make_forward_references_hash1(void);
static void make_forward_references_hash2(void);

#ifdef	DB_FORW_REF
static void db_forward_references(const char *);
static void make_forward_references_hash3(void);
#endif

void
MakeForwardReferences(void) {
	/*	Constructs the forward references table.
	*/

	n_forward_references = TextLength();
	forward_references =
		(unsigned int *)calloc(
			n_forward_references, sizeof (unsigned int)
		);
	if (!forward_references) {
		fatal("out of memory");
	}
	make_forward_references_hash1();
	make_forward_references_hash2();
#ifdef	DB_FORW_REF
	make_forward_references_hash3();
#endif
}

unsigned int
ForwardReference(int i) {
	if (i <= 0 || i >= n_forward_references) {
		fatal("internal error, bad forward reference");
	}
	return forward_references[i];
}

void
FreeForwardReferences(void) {
	free((char *)forward_references);
}

							/* HASHING */
/*
	We want a hash function whose time cost does not depend on
	MinRunSize, which is a problem since the size of the value
	we derive the hash function from IS equal to MinRunSize!
	Therefore we base the hash function on a sample of at most 24
	tokens from the input string; this works at least as well in
	practice.  These 24 token values will result in exactly 31
	bits under the hashing algorithm used, which avoids an
	overflow test.  So this 24 bears no relation to the default
	run size (although the fit is surprising!)
*/

#define	N_SAMPLES	24
#define	OPERATION	^

/*	An alternative algorithm; does not seem to make any difference.
#define	N_SAMPLES	23
#define	OPERATION	+
*/

/*	Another algorithm; not yet tested
#define	N_SAMPLES	24
#define	OPERATION	+ 613 *
*/

static unsigned int *last_index;
static unsigned int hash_table_size;
static int sample_pos[N_SAMPLES];

static unsigned int
prime[] = {		/* lots of hopefully suitable primes */
	10639,
	21283,
	42571,
	85147,
	170227,
	340451,
	680959,
	1361803,
	2723599,
	5447171,
	10894379,
	21788719,
	43577399,
	87154759,
	174309383,
	348618827,
	697237511,
	1394475011
};

static void
init_hash_table(void) {
	register int n;

	/* find the ideal hash table size */
	n = 0;
	while (prime[n] < TextLength()) {
		n++;
		/* this will always terminate, if prime[] is large enough */
	}

	/* see if we can allocate that much space, and if not, step down */
	last_index = 0;
	while (!last_index && n >= 0) {
		hash_table_size = prime[n];
		last_index = (unsigned int *)
			calloc(hash_table_size, sizeof (unsigned int));
		n--;
	}
	if (!last_index) {
		fatal("out of memory");
	}
	
	/* find sample positions */
	for (n = 0; n < N_SAMPLES; n++) {
		/* straigh-line approximation; uninituitive as usual */
		sample_pos[n] = (
			(2 * n * (MinRunSize - 1) + (N_SAMPLES - 1))
		/	(2 * (N_SAMPLES - 1))
		);
	}
}

static int hash1(const TOKEN *);

static void
make_forward_references_hash1(void) {
	register int n;

	init_hash_table();

	/* set up the forward references using the last_index hash table */
	for (n = 0; n < NumberOfTexts; n++) {
		register struct text *txt = &Text[n];
		register unsigned int j;

		for (	/* all pos'ns in txt except the last MinRunSize-1 */
			j = txt->tx_start;			/* >= 1 */
			j + MinRunSize - 1 < txt->tx_limit;
			j++
		) {
			if (MayBeStartOfRun(TokenArray[j])) {
				register int h = hash1(&TokenArray[j]);

				if (last_index[h]) {
					forward_references[last_index[h]] = j;
				}
				last_index[h] = j;
			}
		}
	}
	free((char *)last_index);

#ifdef	DB_FORW_REF
	db_forward_references("first hashing");
#endif	/* DB_FORW_REF */
}

static int
hash1(const TOKEN *p) {
	/*	hash1(p) returns the hash code of the MinRunSize
		tokens starting at p; caller guarantees that there
		are at least MinRunSize tokens.
	*/
	register int32 h_val;
	register int n;
	
	h_val = 0;
	for (n = 0; n < N_SAMPLES; n++) {
		h_val = (h_val << 1) OPERATION TOKEN2int(p[sample_pos[n]]);
#if	N_SAMPLES > 24
		if (h_val & (1<<31)) {
			h_val ^= (1<<31|1);
		}
#endif
	}
	/* just in case somebody tries wrong N_SAMPLES and OPERATION values: */
	if (h_val < 0) fatal("corrupt hash algorithm in hash1() in hash.c");

	return h_val % hash_table_size;
}

static int hash2(const TOKEN *);

static void
make_forward_references_hash2(void) {
	register unsigned int i;

	/* do a second hash only if the original hash table was reduced */
	/*	Meanwhile, the quality of the primary hashing is so bad
		that we are virtually forced to always do a second scan.
	*/

	/*	Clean out spurious matches, by a quadratic algorithm.
		Note that we do not want to eliminate overlapping
		sequences in this stage, since we might be removing the
		wrong copy.
	*/
	for (i = 0; i+MinRunSize < TextLength(); i++) {
		register unsigned int j = i;
		register int h2 = hash2(&TokenArray[i]);

		/*	Find the first token sequence in the chain
			with same secondary hash code.
		*/
		while (	/* there is still a forward reference */
			(j = forward_references[j])
		&&	/* its hash code does not match */
			hash2(&TokenArray[j]) != h2
		) {
			/* continue searching */
		}
		/* short-circuit forward reference to it, or to zero */
		forward_references[i] = j;
	}

#ifdef	DB_FORW_REF
	db_forward_references("second hashing");
#endif	/* DB_FORW_REF */
}

static int
hash2(const TOKEN *p) {
	/*	a simple-minded hashing for the secondary sweep;
		first and last token combined in a short int
	*/
	return (TOKEN2int(p[0]) << 8) + TOKEN2int(p[MinRunSize-1]);
}

#ifdef	DB_FORW_REF

static int hash3(const TOKEN *, const TOKEN *);

static void
make_forward_references_hash3(void) {
	register unsigned int i;

	/* do a third hash to check up on the previous two */

	/* this time we use a genuine compare */
	for (i = 0; i+MinRunSize < TextLength(); i++) {
		register unsigned int j = i;

		while (	/* there is still a forward reference */
			(j = forward_references[j])
		&&	/* its hash code does not match */
			!hash3(&TokenArray[i], &TokenArray[j])
		) {
			/* continue searching */
		}
		/* short-circuit forward reference to it, or to zero */
		forward_references[i] = j;
	}

	db_forward_references("third hashing");
}

static int
hash3(const TOKEN *p, const TOKEN *q) {
	/* a full comparison for the tertiary sweep */
	int n;
	
	for (n = 0; n < MinRunSize; n++) {
		if (TOKEN2int(*(p+n)) != TOKEN2int(*(q+n))) return 0;
	}
	return 1;
}

static int
db_frw_chain(int n, char *crossed_out) {
	register int chain_len = -1;
		/* if there are two values, the chain length is still 1 */
	register int fw;

	for (fw = n; fw; fw = forward_references[fw]) {
		if (crossed_out[fw]) {
			fprintf(DebugFile,
				">>>> error in forward_references[] <<<<\n"
			);
		}
		chain_len++;
		crossed_out[fw]++;
	}
	fprintf(DebugFile, "n = %d, chain_len = %d\n", n, chain_len);
	
	return chain_len;
}

static void
db_forward_references(const char *msg) {
	int n;
	int n_frw_chains = 0;		/* number of forward ref. chains */
	int tot_frwc_len = 0;
	char *crossed_out;

	fprintf(DebugFile, "\n\n**** DB_FORWARD_REFERENCES, %s ****\n", msg);
	fprintf(DebugFile, "hash_table_size = %u\n", hash_table_size);
	fprintf(DebugFile, "N_SAMPLES = %d\n", N_SAMPLES);

	crossed_out = (char *)calloc(TextLength(), sizeof (char));
	if (!crossed_out) {
		fatal(">>>> no room for db_forward_references debug table <<<<\n");
	}

	/*	Each forward_references[n] starts in principle a new
		chain, and these chains never touch each other.
		We check this property by marking the positions in each
		chain in an array; if we meet a marked entry while
		following a chain, it must have been on an earlier chain
		and we have an error.
		We also determine the lengths of the chains, for statistics.
	*/
	if (forward_references[0]) {
		fprintf(DebugFile,
			">>>> forward_references[0] is not zero <<<<\n"
		);
	}
	for (n = 1; n < TextLength(); n++) {
		if (forward_references[n] && !crossed_out[n]) {
			/* start of a new chain */
			n_frw_chains++;
			tot_frwc_len += db_frw_chain(n, crossed_out);
		}
	}
	free((char *)crossed_out);

	fprintf(DebugFile,
		"text length = %u, # forward chains = %d, total frw chain length = %d\n\n",
		TextLength(), n_frw_chains, tot_frwc_len
	);
}

#endif	/* DB_FORW_REF */
