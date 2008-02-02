{*
 * gqsort.inc
 *
 * depends on gtypes.inc
 *}


  procedure g_qsort_with_data (pbase        : gconstpointer;
                               total_elems  : gint;
                               size         : gsize;
                               compare_func : TGCompareDataFunc;
                               user_data    : gpointer);cdecl;external gliblib name 'g_qsort_with_data';



