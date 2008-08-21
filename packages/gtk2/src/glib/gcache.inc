// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGCache = pointer;

  TGCacheNewFunc     = function (key:gpointer):gpointer;cdecl;
  TGCacheDupFunc     = function (value:gpointer):gpointer;cdecl;
  TGCacheDestroyFunc = procedure (value:gpointer);cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function g_cache_new(value_new_func:TGCacheNewFunc; value_destroy_func:TGCacheDestroyFunc; key_dup_func:TGCacheDupFunc; key_destroy_func:TGCacheDestroyFunc; hash_key_func:TGHashFunc;
  hash_value_func:TGHashFunc; key_equal_func:TGEqualFunc):PGCache;cdecl;external gliblib name 'g_cache_new';

procedure g_cache_destroy(cache:PGCache);cdecl;external gliblib name 'g_cache_destroy';
function  g_cache_insert(cache:PGCache; key:gpointer):gpointer;cdecl;external gliblib name 'g_cache_insert';
procedure g_cache_remove(cache:PGCache; value:gconstpointer);cdecl;external gliblib name 'g_cache_remove';
procedure g_cache_key_foreach(cache:PGCache; func:TGHFunc; user_data:gpointer);cdecl;external gliblib name 'g_cache_key_foreach';
procedure g_cache_value_foreach(cache:PGCache; func:TGHFunc; user_data:gpointer);cdecl;external gliblib name 'g_cache_value_foreach';

{$ENDIF read_interface_rest}
