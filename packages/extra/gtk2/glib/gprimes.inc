{*
 * gprimes.inc
 *
 * depends on gtypes.h
 *}

  { Prime numbers.}

  { This function returns prime numbers spaced by approximately 1.5-2.0
     and is for use in resizing data structures which prefer
     prime-valued sizes.        The closest spaced prime function returns the
     next largest prime, or the highest it knows about which is about
     MAXINT/4.
    }

  function g_spaced_primes_closest(num:guint):guint;cdecl;external gliblib name 'g_spaced_primes_closest';



