// included by glib2.pas
type
   PGTree = pointer;
   TGTraverseFunc = function (key:gpointer; value:gpointer; data:gpointer):gboolean; cdecl;

{ Balanced binary trees }

function g_tree_new(key_compare_func:TGCompareFunc):PGTree; cdecl; external gliblib;
function g_tree_new_with_data(key_compare_func:TGCompareDataFunc; key_compare_data:gpointer):PGTree; cdecl; external gliblib;
function g_tree_new_full(key_compare_func:TGCompareDataFunc; key_compare_data:gpointer; key_destroy_func:TGDestroyNotify; value_destroy_func:TGDestroyNotify):PGTree; cdecl; external gliblib;
procedure g_tree_destroy(tree:PGTree); cdecl; external gliblib;
procedure g_tree_insert(tree:PGTree; key:gpointer; value:gpointer); cdecl; external gliblib;
procedure g_tree_replace(tree:PGTree; key:gpointer; value:gpointer); cdecl; external gliblib;
procedure g_tree_remove(tree:PGTree; key:gconstpointer); cdecl; external gliblib;
procedure g_tree_steal(tree:PGTree; key:gconstpointer); cdecl; external gliblib;
function g_tree_lookup(tree:PGTree; key:gconstpointer):gpointer; cdecl; external gliblib;
function g_tree_lookup_extended(tree:PGTree; lookup_key:gconstpointer; orig_key:Pgpointer; value:Pgpointer):gboolean; cdecl; external gliblib;
procedure g_tree_foreach(tree:PGTree; func:TGTraverseFunc; user_data:gpointer); cdecl; external gliblib;

{$ifndef G_DISABLE_DEPRECATED}
procedure g_tree_traverse(tree:PGTree; traverse_func:TGTraverseFunc; traverse_type:TGTraverseType; user_data:gpointer); cdecl; external gliblib;
{$endif}
{ G_DISABLE_DEPRECATED  }

function g_tree_search(tree:PGTree; search_func:TGCompareFunc; user_data:gconstpointer):gpointer; cdecl; external gliblib;
function g_tree_height(tree:PGTree):gint; cdecl; external gliblib;
function g_tree_nnodes(tree:PGTree):gint; cdecl; external gliblib;

// included by glib2.pas

