// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTreeStore = ^TGtkTreeStore;
   TGtkTreeStore = record
        parent : TGObject;
        stamp : gint;
        root : gpointer;
        last : gpointer;
        n_columns : gint;
        sort_column_id : gint;
        sort_list : PGList;
        order : TGtkSortType;
        column_headers : PGType;
        default_sort_func : TGtkTreeIterCompareFunc;
        default_sort_data : gpointer;
        default_sort_destroy : TGtkDestroyNotify;
        flag0 : word;
     end;


{ Padding for future expansion  }
   PGtkTreeStoreClass = ^TGtkTreeStoreClass;
   TGtkTreeStoreClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTreeStore_columns_dirty = $1;
   bp_TGtkTreeStore_columns_dirty = 0;


function GTK_TYPE_TREE_STORE : GType;
function GTK_TREE_STORE(obj: pointer) : PGtkTreeStore;
function GTK_TREE_STORE_CLASS(klass: pointer) : PGtkTreeStoreClass;
function GTK_IS_TREE_STORE(obj: pointer) : boolean;
function GTK_IS_TREE_STORE_CLASS(klass: pointer) : boolean;
function GTK_TREE_STORE_GET_CLASS(obj: pointer) : PGtkTreeStoreClass;


function columns_dirty(var a : TGtkTreeStore) : guint; overload;
procedure set_columns_dirty(var a : TGtkTreeStore; __columns_dirty : guint); overload;


function gtk_tree_store_get_type:TGtkType; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_tree_store_new(n_columns:gint):PGtkTreeStore; cdecl; overload; external gtklib;
function gtk_tree_store_new(n_columns:gint; args:array of const):PGtkTreeStore; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_tree_store_new(n_columns:gint):PGtkTreeStore; varargs; cdecl; external gtklib;
{$ENDIF}
function gtk_tree_store_newv(n_columns:gint; types:PGType):PGtkTreeStore; cdecl; external gtklib;
procedure gtk_tree_store_set_column_types(tree_store:PGtkTreeStore; n_columns:gint; types:PGType); cdecl; external gtklib;
{ NOTE: use gtk_tree_model_get to get values from a GtkTreeStore  }
procedure gtk_tree_store_set_value(tree_store:PGtkTreeStore; iter:PGtkTreeIter; column:gint; value:PGValue); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_tree_store_set(tree_store:PGtkTreeStore; iter:PGtkTreeIter; args:array of const); cdecl; overload; external gtklib;
procedure gtk_tree_store_set(tree_store:PGtkTreeStore; iter:PGtkTreeIter); cdecl; overload; varargs; external gtklib;
procedure gtk_tree_store_set_valist(tree_store:PGtkTreeStore; iter:PGtkTreeIter; var_args: array of const); cdecl; external gtklib;
{$ELSE}
procedure gtk_tree_store_set(tree_store:PGtkTreeStore; iter:PGtkTreeIter); varargs; cdecl; external gtklib;
procedure gtk_tree_store_set_valist(tree_store:PGtkTreeStore; iter:PGtkTreeIter); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_tree_store_remove(tree_store:PGtkTreeStore; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_insert(tree_store:PGtkTreeStore; iter:PGtkTreeIter; parent:PGtkTreeIter; position:gint); cdecl; external gtklib;
procedure gtk_tree_store_insert_before(tree_store:PGtkTreeStore; iter:PGtkTreeIter; parent:PGtkTreeIter; sibling:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_insert_after(tree_store:PGtkTreeStore; iter:PGtkTreeIter; parent:PGtkTreeIter; sibling:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_prepend(tree_store:PGtkTreeStore; iter:PGtkTreeIter; parent:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_append(tree_store:PGtkTreeStore; iter:PGtkTreeIter; parent:PGtkTreeIter); cdecl; external gtklib;
function gtk_tree_store_is_ancestor(tree_store:PGtkTreeStore; iter:PGtkTreeIter; descendant:PGtkTreeIter):gboolean; cdecl; external gtklib;
function gtk_tree_store_iter_depth(tree_store:PGtkTreeStore; iter:PGtkTreeIter):gint; cdecl; external gtklib;
procedure gtk_tree_store_clear(tree_store:PGtkTreeStore); cdecl; external gtklib;

{$IFDEF GTK2_2}
function gtk_tree_store_iter_is_valid(tree_store:PGtkTreeStore;
  iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
procedure gtk_tree_store_reorder(tree_store:PGtkTreeStore;
  parent:PGtkTreeIter; new_order:pgint); cdecl; external gtklib;
procedure gtk_tree_store_swap(tree_store:PGtkTreeStore;
  a:PGtkTreeIter; b:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_move_before(tree_store:PGtkTreeStore;
  iter:PGtkTreeIter; position:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_move_after(tree_store:PGtkTreeStore;
  iter:PGtkTreeIter; position:PGtkTreeIter); cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_STORE : GType;
begin
   GTK_TYPE_TREE_STORE:=gtk_tree_store_get_type;
end;

function GTK_TREE_STORE(obj: pointer) : PGtkTreeStore;
begin
   GTK_TREE_STORE:=PGtkTreeStore(GTK_CHECK_CAST(obj,GTK_TYPE_TREE_STORE));
end;

function GTK_TREE_STORE_CLASS(klass: pointer) : PGtkTreeStoreClass;
begin
   GTK_TREE_STORE_CLASS:=PGtkTreeStoreClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE_STORE));
end;

function GTK_IS_TREE_STORE(obj: pointer) : boolean;
begin
   GTK_IS_TREE_STORE:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE_STORE);
end;

function GTK_IS_TREE_STORE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_STORE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE_STORE);
end;

function GTK_TREE_STORE_GET_CLASS(obj: pointer) : PGtkTreeStoreClass;
begin
   GTK_TREE_STORE_GET_CLASS:=PGtkTreeStoreClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE_STORE));
end;

function columns_dirty(var a : TGtkTreeStore) : guint;
begin
   columns_dirty:=(a.flag0 and bm_TGtkTreeStore_columns_dirty) shr bp_TGtkTreeStore_columns_dirty;
end;

procedure set_columns_dirty(var a : TGtkTreeStore; __columns_dirty : guint);
begin
   a.flag0:=a.flag0 or ((__columns_dirty shl bp_TGtkTreeStore_columns_dirty) and bm_TGtkTreeStore_columns_dirty);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

