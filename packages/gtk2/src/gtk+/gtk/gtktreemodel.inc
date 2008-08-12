// included by gtk2.pas

{$IFDEF read_forward_definitions}
   // internal type
   PPGtkTreeModel = ^PGtkTreeModel;
   PGtkTreeModel = pointer;
   // internal type
   PGtkTreePath = pointer;

   PGtkTreeIter = ^TGtkTreeIter;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TGtkTreeModelForeachFunc = function (model:PGtkTreeModel; path:PGtkTreePath;
                              iter:PGtkTreeIter; data:gpointer):gboolean; cdecl;

   PGtkTreeModelFlags = ^TGtkTreeModelFlags;
   TGtkTreeModelFlags = longint;

   TGtkTreeIter = record
        stamp : gint;
        user_data : gpointer;
        user_data2 : gpointer;
        user_data3 : gpointer;
     end;

{ Signals  }
{ Virtual Table  }
   PGtkTreeModelIface = ^TGtkTreeModelIface;
   TGtkTreeModelIface = record
        g_iface : TGTypeInterface;
        row_changed : procedure (tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter); cdecl;
        row_inserted : procedure (tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter); cdecl;
        row_has_child_toggled : procedure (tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter); cdecl;
        row_deleted : procedure (tree_model:PGtkTreeModel; path:PGtkTreePath); cdecl;
        rows_reordered : procedure (tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter; new_order:Pgint); cdecl;
        get_flags : function (tree_model:PGtkTreeModel):TGtkTreeModelFlags; cdecl;
        get_n_columns : function (tree_model:PGtkTreeModel):gint; cdecl;
        get_column_type : function (tree_model:PGtkTreeModel; index:gint):GType; cdecl;
        get_iter : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter; path:PGtkTreePath):gboolean; cdecl;
        get_path : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter):PGtkTreePath; cdecl;
        get_value : procedure (tree_model:PGtkTreeModel; iter:PGtkTreeIter; column:gint; value:PGValue); cdecl;
        iter_next : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean; cdecl;
        iter_children : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter; parent:PGtkTreeIter):gboolean; cdecl;
        iter_has_child : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean; cdecl;
        iter_n_children : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter):gint; cdecl;
        iter_nth_child : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter; parent:PGtkTreeIter; n:gint):gboolean; cdecl;
        iter_parent : function (tree_model:PGtkTreeModel; iter:PGtkTreeIter; child:PGtkTreeIter):gboolean; cdecl;
        ref_node : procedure (tree_model:PGtkTreeModel; iter:PGtkTreeIter); cdecl;
        unref_node : procedure (tree_model:PGtkTreeModel; iter:PGtkTreeIter); cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_TREE_MODEL_ITERS_PERSIST = 1 shl 0;
   GTK_TREE_MODEL_LIST_ONLY = 1 shl 1;


function GTK_TYPE_TREE_MODEL : GType;
function GTK_TREE_MODEL(obj: pointer) : PGtkTreeModel;
function GTK_IS_TREE_MODEL(obj: pointer) : boolean;
function GTK_TREE_MODEL_GET_IFACE(obj: pointer) : PGtkTreeModelIface;
function GTK_TYPE_TREE_ITER : GType;
function GTK_TYPE_TREE_PATH : GType;
    { return type might be wrong }

{ GtkTreePath operations  }

function gtk_tree_path_new:PGtkTreePath; cdecl; external gtklib;
function gtk_tree_path_new_from_string(path:Pgchar):PGtkTreePath; cdecl; external gtklib;
function gtk_tree_path_to_string(path:PGtkTreePath):Pgchar; cdecl; external gtklib;
function gtk_tree_path_new_root : PGtkTreePath;

function gtk_tree_path_new_first:PGtkTreePath; cdecl; external gtklib;
procedure gtk_tree_path_append_index(path:PGtkTreePath; index:gint); cdecl; external gtklib;
procedure gtk_tree_path_prepend_index(path:PGtkTreePath; index:gint); cdecl; external gtklib;
function gtk_tree_path_get_depth(path:PGtkTreePath):gint; cdecl; external gtklib;
function gtk_tree_path_get_indices(path:PGtkTreePath):Pgint; cdecl; external gtklib;
procedure gtk_tree_path_free(path:PGtkTreePath); cdecl; external gtklib;
function gtk_tree_path_copy(path:PGtkTreePath):PGtkTreePath; cdecl; external gtklib;
function gtk_tree_path_get_type:GType; cdecl; external gtklib;
function gtk_tree_path_compare(a:PGtkTreePath; b:PGtkTreePath):gint; cdecl; external gtklib;
procedure gtk_tree_path_next(path:PGtkTreePath); cdecl; external gtklib;
function gtk_tree_path_prev(path:PGtkTreePath):gboolean; cdecl; external gtklib;
function gtk_tree_path_up(path:PGtkTreePath):gboolean; cdecl; external gtklib;
procedure gtk_tree_path_down(path:PGtkTreePath); cdecl; external gtklib;
function gtk_tree_path_is_ancestor(path:PGtkTreePath; descendant:PGtkTreePath):gboolean; cdecl; external gtklib;
function gtk_tree_path_is_descendant(path:PGtkTreePath; ancestor:PGtkTreePath):gboolean; cdecl; external gtklib;
{ Row reference (an anObject that tracks model changes so it refers to the same
   row always; a path refers to a position, not a fixed row).  You almost always
   want to call gtk_tree_row_reference_new.
  }
function gtk_tree_row_reference_new(model:PGtkTreeModel; path:PGtkTreePath):PGtkTreeRowReference; cdecl; external gtklib;
function gtk_tree_row_reference_new_proxy(proxy:PGObject; model:PGtkTreeModel; path:PGtkTreePath):PGtkTreeRowReference; cdecl; external gtklib;
function gtk_tree_row_reference_get_path(reference:PGtkTreeRowReference):PGtkTreePath; cdecl; external gtklib;
function gtk_tree_row_reference_valid(reference:PGtkTreeRowReference):gboolean; cdecl; external gtklib;
procedure gtk_tree_row_reference_free(reference:PGtkTreeRowReference); cdecl; external gtklib;
{ These two functions are only needed if you created the row reference with a
   proxy anObject  }
procedure gtk_tree_row_reference_inserted(proxy:PGObject; path:PGtkTreePath); cdecl; external gtklib;
procedure gtk_tree_row_reference_deleted(proxy:PGObject; path:PGtkTreePath); cdecl; external gtklib;
procedure gtk_tree_row_reference_reordered(proxy:PGObject; path:PGtkTreePath; iter:PGtkTreeIter; new_order:Pgint); cdecl; external gtklib;
{ GtkTreeIter operations  }
function gtk_tree_iter_copy(iter:PGtkTreeIter):PGtkTreeIter; cdecl; external gtklib;
procedure gtk_tree_iter_free(iter:PGtkTreeIter); cdecl; external gtklib;
function gtk_tree_iter_get_type:GType; cdecl; external gtklib;
function gtk_tree_model_get_type:TGtkType; cdecl; external gtklib;
function gtk_tree_model_get_flags(tree_model:PGtkTreeModel):TGtkTreeModelFlags; cdecl; external gtklib;
function gtk_tree_model_get_n_columns(tree_model:PGtkTreeModel):gint; cdecl; external gtklib;
function gtk_tree_model_get_column_type(tree_model:PGtkTreeModel; index:gint):GType; cdecl; external gtklib;
{ Iterator movement  }
function gtk_tree_model_get_iter(tree_model:PGtkTreeModel; iter:PGtkTreeIter; path:PGtkTreePath):gboolean; cdecl; external gtklib;
function gtk_tree_model_get_iter_from_string(tree_model:PGtkTreeModel; iter:PGtkTreeIter; path_string:Pgchar):gboolean; cdecl; external gtklib;
function gtk_tree_model_get_iter_root(tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean;

function gtk_tree_model_get_iter_first(tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
function gtk_tree_model_get_path(tree_model:PGtkTreeModel; iter:PGtkTreeIter):PGtkTreePath; cdecl; external gtklib;
procedure gtk_tree_model_get_value(tree_model:PGtkTreeModel; iter:PGtkTreeIter; column:gint; value:PGValue); cdecl; external gtklib;
function gtk_tree_model_iter_next(tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
function gtk_tree_model_iter_children(tree_model:PGtkTreeModel; iter:PGtkTreeIter; parent:PGtkTreeIter):gboolean; cdecl; external gtklib;
function gtk_tree_model_iter_has_child(tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
function gtk_tree_model_iter_n_children(tree_model:PGtkTreeModel; iter:PGtkTreeIter):gint; cdecl; external gtklib;
function gtk_tree_model_iter_nth_child(tree_model:PGtkTreeModel; iter:PGtkTreeIter; parent:PGtkTreeIter; n:gint):gboolean; cdecl; external gtklib;
function gtk_tree_model_iter_parent(tree_model:PGtkTreeModel; iter:PGtkTreeIter; child:PGtkTreeIter):gboolean; cdecl; external gtklib;
procedure gtk_tree_model_ref_node(tree_model:PGtkTreeModel; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_model_unref_node(tree_model:PGtkTreeModel; iter:PGtkTreeIter); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_tree_model_get(tree_model:PGtkTreeModel; iter:PGtkTreeIter; args:array of const); cdecl; overload; external gtklib;
procedure gtk_tree_model_get(tree_model:PGtkTreeModel; iter:PGtkTreeIter); cdecl; overload; varargs; external gtklib;
procedure gtk_tree_model_get_valist(tree_model:PGtkTreeModel; iter:PGtkTreeIter; var_args: array of const); cdecl; varargs; external gtklib;
{$ELSE}
procedure gtk_tree_model_get(tree_model:PGtkTreeModel; iter:PGtkTreeIter); varargs; cdecl; external gtklib;
procedure gtk_tree_model_get_valist(tree_model:PGtkTreeModel; iter:PGtkTreeIter); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_tree_model_foreach(model:PGtkTreeModel; func:TGtkTreeModelForeachFunc; user_data:gpointer); cdecl; external gtklib;
{ Signals  }
procedure gtk_tree_model_row_changed(tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_model_row_inserted(tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_model_row_has_child_toggled(tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_model_row_deleted(tree_model:PGtkTreeModel; path:PGtkTreePath); cdecl; external gtklib;
procedure gtk_tree_model_rows_reordered(tree_model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter; new_order:Pgint); cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_tree_model_get_string_from_iter(tree_model:PGtkTreeModel; iter:PGtkTreeIter):Pgchar; cdecl; external gtklib;
function gtk_tree_path_new_from_indices(first_index: gint): PGtkTreePath; cdecl; varargs; external gtklib;
function gtk_tree_row_reference_copy(reference:PGtkTreeRowReference):PGtkTreeRowReference; cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_MODEL : GType;
begin
   GTK_TYPE_TREE_MODEL:=gtk_tree_model_get_type;
end;

function GTK_TREE_MODEL(obj: pointer) : PGtkTreeModel;
begin
   GTK_TREE_MODEL:=PGtkTreeModel(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TREE_MODEL));
end;

function GTK_IS_TREE_MODEL(obj: pointer) : boolean;
begin
   GTK_IS_TREE_MODEL:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TREE_MODEL);
end;

function GTK_TREE_MODEL_GET_IFACE(obj: pointer) : PGtkTreeModelIface;
begin
   GTK_TREE_MODEL_GET_IFACE:=PGtkTreeModelIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_TREE_MODEL));
end;

function GTK_TYPE_TREE_ITER : GType;
begin
   GTK_TYPE_TREE_ITER:=gtk_tree_iter_get_type;
end;

function GTK_TYPE_TREE_PATH : GType;
begin
   GTK_TYPE_TREE_PATH:=gtk_tree_path_get_type;
end;

function gtk_tree_path_new_root : PGtkTreePath;
begin
   gtk_tree_path_new_root:=gtk_tree_path_new_first;
end;

function gtk_tree_model_get_iter_root(tree_model:PGtkTreeModel; iter:PGtkTreeIter):gboolean;
begin
   gtk_tree_model_get_iter_root:=gtk_tree_model_get_iter_first(tree_model,iter);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

