// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkTreeSortable = pointer;

   TGtkTreeIterCompareFunc = function (model:PGtkTreeModel; a:PGtkTreeIter; b:PGtkTreeIter; user_data:gpointer):gint; cdecl;
{ signals  }
{ virtual table  }
   PGtkTreeSortableIface = ^TGtkTreeSortableIface;
   TGtkTreeSortableIface = record
        g_iface : TGTypeInterface;
        sort_column_changed : procedure (sortable:PGtkTreeSortable); cdecl;
        get_sort_column_id : function (sortable:PGtkTreeSortable; sort_column_id:Pgint; order:PGtkSortType):gboolean; cdecl;
        set_sort_column_id : procedure (sortable:PGtkTreeSortable; sort_column_id:gint; order:TGtkSortType); cdecl;
        set_sort_func : procedure (sortable:PGtkTreeSortable; sort_column_id:gint; func:TGtkTreeIterCompareFunc; data:gpointer; destroy:TGtkDestroyNotify); cdecl;
        set_default_sort_func : procedure (sortable:PGtkTreeSortable; func:TGtkTreeIterCompareFunc; data:gpointer; destroy:TGtkDestroyNotify); cdecl;
        has_default_sort_func : function (sortable:PGtkTreeSortable):gboolean; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID = -(1);


function GTK_TYPE_TREE_SORTABLE : GType;
function GTK_TREE_SORTABLE(obj: pointer) : PGtkTreeSortable;
function GTK_TREE_SORTABLE_CLASS(obj: pointer) : PGtkTreeSortableIface;
function GTK_IS_TREE_SORTABLE(obj: pointer) : boolean;
function GTK_TREE_SORTABLE_GET_IFACE(obj: pointer) : PGtkTreeSortableIface;



function gtk_tree_sortable_get_type:GType; cdecl; external gtklib;
procedure gtk_tree_sortable_sort_column_changed(sortable:PGtkTreeSortable); cdecl; external gtklib;
function gtk_tree_sortable_get_sort_column_id(sortable:PGtkTreeSortable; sort_column_id:Pgint; order:PGtkSortType):gboolean; cdecl; external gtklib;
procedure gtk_tree_sortable_set_sort_column_id(sortable:PGtkTreeSortable; sort_column_id:gint; order:TGtkSortType); cdecl; external gtklib;
procedure gtk_tree_sortable_set_sort_func(sortable:PGtkTreeSortable; sort_column_id:gint; sort_func:TGtkTreeIterCompareFunc; user_data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
procedure gtk_tree_sortable_set_default_sort_func(sortable:PGtkTreeSortable; sort_func:TGtkTreeIterCompareFunc; user_data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
function gtk_tree_sortable_has_default_sort_func(sortable:PGtkTreeSortable):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_SORTABLE : GType;
begin
   GTK_TYPE_TREE_SORTABLE:=gtk_tree_sortable_get_type;
end;

function GTK_TREE_SORTABLE(obj: pointer) : PGtkTreeSortable;
begin
   GTK_TREE_SORTABLE:=PGtkTreeSortable(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TREE_SORTABLE));
end;

function GTK_TREE_SORTABLE_CLASS(obj: pointer) : PGtkTreeSortableIface;
begin
   GTK_TREE_SORTABLE_CLASS:=PGtkTreeSortableIface(G_TYPE_CHECK_CLASS_CAST(obj,GTK_TYPE_TREE_SORTABLE));
end;

function GTK_IS_TREE_SORTABLE(obj: pointer) : boolean;
begin
   GTK_IS_TREE_SORTABLE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TREE_SORTABLE);
end;

function GTK_TREE_SORTABLE_GET_IFACE(obj: pointer) : PGtkTreeSortableIface;
begin
   GTK_TREE_SORTABLE_GET_IFACE:=PGtkTreeSortableIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_TREE_SORTABLE));
end;
{$ENDIF read_implementation}
// included by gtk2.pas


