// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGRand = pointer;   // check please
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
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
{$ENDIF read_interface_rest}
// included by glib2.pas
