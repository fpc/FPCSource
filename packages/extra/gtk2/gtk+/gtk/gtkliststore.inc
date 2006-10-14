// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< private > }
   PGtkListStore = ^TGtkListStore;
   TGtkListStore = record
        parent : TGObject;
        stamp : gint;
        root : gpointer;
        tail : gpointer;
        sort_list : PGList;
        n_columns : gint;
        sort_column_id : gint;
        order : TGtkSortType;
        column_headers : PGType;
        length : gint;
        default_sort_func : TGtkTreeIterCompareFunc;
        default_sort_data : gpointer;
        default_sort_destroy : TGtkDestroyNotify;
        flag0 : word;
     end;


{ Padding for future expansion  }
   PGtkListStoreClass = ^TGtkListStoreClass;
   TGtkListStoreClass = record
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
   bm_TGtkListStore_columns_dirty = $1;
   bp_TGtkListStore_columns_dirty = 0;
function GTK_TYPE_LIST_STORE : GType;
function GTK_LIST_STORE(obj: pointer) : PGtkListStore;
function GTK_LIST_STORE_CLASS(klass: pointer) : PGtkListStoreClass;
function GTK_IS_LIST_STORE(obj: pointer) : boolean;
function GTK_IS_LIST_STORE_CLASS(klass: pointer) : boolean;
function GTK_LIST_STORE_GET_CLASS(obj: pointer) : PGtkListStoreClass;


function columns_dirty(var a : TGtkListStore) : guint; overload;
procedure set_columns_dirty(var a : TGtkListStore; __columns_dirty : guint); overload;
function gtk_list_store_get_type:TGtkType; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_list_store_new(n_columns:gint; args:array of const):PGtkListStore; cdecl; overload; external gtklib;
function gtk_list_store_new(n_columns:gint):PGtkListStore; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_list_store_new(n_columns:gint):PGtkListStore; varargs; cdecl; external gtklib;
{$ENDIF}
function gtk_list_store_newv(n_columns:gint; types:PGType):PGtkListStore; cdecl; external gtklib;
procedure gtk_list_store_set_column_types(list_store:PGtkListStore; n_columns:gint; types:PGType); cdecl; external gtklib;
{ NOTE: use gtk_tree_model_get to get values from a GtkListStore  }
procedure gtk_list_store_set_value(list_store:PGtkListStore; iter:PGtkTreeIter; column:gint; value:PGValue); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_list_store_set(list_store:PGtkListStore; iter:PGtkTreeIter; args:array of const); cdecl; overload; external gtklib;
procedure gtk_list_store_set(list_store:PGtkListStore; iter:PGtkTreeIter); cdecl; overload; varargs; external gtklib;
procedure gtk_list_store_set_valist(list_store:PGtkListStore; iter:PGtkTreeIter; var_args:array of const); cdecl; external gtklib;
{$ELSE}
procedure gtk_list_store_set(list_store:PGtkListStore; iter:PGtkTreeIter); varargs; cdecl; external gtklib;
procedure gtk_list_store_set_valist(list_store:PGtkListStore; iter:PGtkTreeIter); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_list_store_remove(list_store:PGtkListStore; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_list_store_insert(list_store:PGtkListStore; iter:PGtkTreeIter; position:gint); cdecl; external gtklib;
procedure gtk_list_store_insert_before(list_store:PGtkListStore; iter:PGtkTreeIter; sibling:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_list_store_insert_after(list_store:PGtkListStore; iter:PGtkTreeIter; sibling:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_list_store_prepend(list_store:PGtkListStore; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_list_store_append(list_store:PGtkListStore; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_list_store_clear(list_store:PGtkListStore); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_LIST_STORE : GType;
begin
   GTK_TYPE_LIST_STORE:=gtk_list_store_get_type;
end;

function GTK_LIST_STORE(obj: pointer) : PGtkListStore;
begin
   GTK_LIST_STORE:=PGtkListStore(GTK_CHECK_CAST(obj,GTK_TYPE_LIST_STORE));
end;

function GTK_LIST_STORE_CLASS(klass: pointer) : PGtkListStoreClass;
begin
   GTK_LIST_STORE_CLASS:=PGtkListStoreClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_LIST_STORE));
end;

function GTK_IS_LIST_STORE(obj: pointer) : boolean;
begin
   GTK_IS_LIST_STORE:=GTK_CHECK_TYPE(obj,GTK_TYPE_LIST_STORE);
end;

function GTK_IS_LIST_STORE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_LIST_STORE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_LIST_STORE);
end;

function GTK_LIST_STORE_GET_CLASS(obj: pointer) : PGtkListStoreClass;
begin
   GTK_LIST_STORE_GET_CLASS:=PGtkListStoreClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_LIST_STORE));
end;

function columns_dirty(var a : TGtkListStore) : guint;
begin
   columns_dirty:=(a.flag0 and bm_TGtkListStore_columns_dirty) shr bp_TGtkListStore_columns_dirty;
end;

procedure set_columns_dirty(var a : TGtkListStore; __columns_dirty : guint);
begin
   a.flag0:=a.flag0 or ((__columns_dirty shl bp_TGtkListStore_columns_dirty) and bm_TGtkListStore_columns_dirty);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

