{*
 * grand.inc
 *
 * depends on gtypes.inc
 *}



  type
    PGRand = pointer;   // check please

  { GRand - a good and fast random number generator: Mersenne Twister
     see http://www.math.keio.ac.jp/~matumoto/emt.html for more info.
     The range functions return a value in the intervall [begin, end).
     int          -> [0..2^32-1]
     int_range    -> [begin..end-1]
     double       -> [0..1)
     double_range -> [begin..end)
    }

  function  g_rand_new_with_seed(seed:guint32):PGRand;cdecl;external gliblib name 'g_rand_new_with_seed';

  function  g_rand_new:PGRand;cdecl;external gliblib name 'g_rand_new';

  procedure g_rand_free(rand:PGRand);cdecl;external gliblib name 'g_rand_free';

  procedure g_rand_set_seed(rand:PGRand; seed:guint32);cdecl;external gliblib name 'g_rand_set_seed';

  function  g_rand_boolean(rand : PGRand) : gboolean;

  function  g_rand_int(rand:PGRand):guint32;cdecl;external gliblib name 'g_rand_int';

  function  g_rand_int_range(rand:PGRand; _begin:gint32; _end:gint32):gint32;cdecl;external gliblib name 'g_rand_int_range';

  function  g_rand_double(rand:PGRand):gdouble;cdecl;external gliblib name 'g_rand_double';

  function  g_rand_double_range(rand:PGRand; _begin:gdouble; _end:gdouble):gdouble;cdecl;external gliblib name 'g_rand_double_range';

  procedure g_random_set_seed(seed:guint32);cdecl;external gliblib name 'g_random_set_seed';

  function g_random_boolean : gboolean;

  function g_random_int:guint32;cdecl;external gliblib name 'g_random_int';

  function g_random_int_range(_begin:gint32; _end:gint32):gint32;cdecl;external gliblib name 'g_random_int_range';

  function g_random_double:gdouble;cdecl;external gliblib name 'g_random_double';

  function g_random_double_range(_begin:gdouble; _end:gdouble):gdouble;cdecl;external gliblib name 'g_random_double_range';

