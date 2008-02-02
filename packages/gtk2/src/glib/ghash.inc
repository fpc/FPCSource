// {$include <glib/gtypes.h>}

    type
      PGHashTable = pointer;

      TGHRFunc = function (key:gpointer; value:gpointer; user_data:gpointer):gboolean;cdecl;

{ Hash tables }

    function g_hash_table_new(hash_func:TGHashFunc; key_equal_func:TGEqualFunc):PGHashTable;cdecl;external gliblib name 'g_hash_table_new';

    function g_hash_table_new_full(hash_func:TGHashFunc; key_equal_func:TGEqualFunc; key_destroy_func:TGDestroyNotify; value_destroy_func:TGDestroyNotify):PGHashTable;cdecl;external gliblib name 'g_hash_table_new_full';

    procedure g_hash_table_destroy(hash_table:PGHashTable);cdecl;external gliblib name 'g_hash_table_destroy';

    procedure g_hash_table_insert(hash_table:PGHashTable; key:gpointer; value:gpointer);cdecl;external gliblib name 'g_hash_table_insert';

    procedure g_hash_table_replace(hash_table:PGHashTable; key:gpointer; value:gpointer);cdecl;external gliblib name 'g_hash_table_replace';

    function g_hash_table_remove(hash_table:PGHashTable; key:gconstpointer):gboolean;cdecl;external gliblib name 'g_hash_table_remove';

    function g_hash_table_steal(hash_table:PGHashTable; key:gconstpointer):gboolean;cdecl;external gliblib name 'g_hash_table_steal';

    function g_hash_table_lookup(hash_table:PGHashTable; key:gconstpointer):gpointer;cdecl;external gliblib name 'g_hash_table_lookup';

    function g_hash_table_lookup_extended(hash_table:PGHashTable; lookup_key:gconstpointer; orig_key:Pgpointer; value:Pgpointer):gboolean;cdecl;external gliblib name 'g_hash_table_lookup_extended';

    procedure g_hash_table_foreach(hash_table:PGHashTable; func:TGHFunc; user_data:gpointer);cdecl;external gliblib name 'g_hash_table_foreach';

    function g_hash_table_foreach_remove(hash_table:PGHashTable; func:TGHRFunc; user_data:gpointer):guint;cdecl;external gliblib name 'g_hash_table_foreach_remove';

    function g_hash_table_foreach_steal(hash_table:PGHashTable; func:TGHRFunc; user_data:gpointer):guint;cdecl;external gliblib name 'g_hash_table_foreach_steal';

    function g_hash_table_size(hash_table:PGHashTable):guint;cdecl;external gliblib name 'g_hash_table_size';


{ Hash Functions  }

    function g_str_equal(v:gconstpointer; v2:gconstpointer):gboolean;cdecl;external gliblib name 'g_str_equal';

    function g_str_hash(v:gconstpointer):guint;cdecl;external gliblib name 'g_str_hash';

    function g_int_equal(v:gconstpointer; v2:gconstpointer):gboolean;cdecl;external gliblib name 'g_int_equal';

    function g_int_hash(v:gconstpointer):guint;cdecl;external gliblib name 'g_int_hash';

    { This "hash" function will just return the key's adress as an
       unsigned integer. Useful for hashing on plain adresses or
       simple integer values.
       passing NULL into g_hash_table_new() as GHashFunc has the
       same effect as passing g_direct_hash().
      }

    function g_direct_hash(v:gconstpointer):guint;cdecl;external gliblib name 'g_direct_hash';

    function g_direct_equal(v:gconstpointer; v2:gconstpointer):gboolean;cdecl;external gliblib name 'g_direct_equal';

