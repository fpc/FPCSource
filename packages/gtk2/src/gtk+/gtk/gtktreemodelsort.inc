// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ < private >  }
{ sort information  }
{ default sort  }
{ signal ids  }
   PGtkTreeModelSort = ^TGtkTreeModelSort;
   TGtkTreeModelSort = record
        parent : TGObject;
        root : gpointer;
        stamp : gint;
        child_flags : guint;
        child_model : PGtkTreeModel;
        zero_ref_count : gint;
        sort_list : PGList;
        sort_column_id : gint;
        order : TGtkSortType;
        default_sort_func : TGtkTreeIterCompareFunc;
        default_sort_data : gpointer;
        default_sort_destroy : TGtkDestroyNotify;
        changed_id : guint;
        inserted_id : guint;
        has_child_toggled_id : guint;
        deleted_id : guint;
        reordered_id : guint;
     end;

{ Padding for future expansion  }
   PGtkTreeModelSortClass = ^TGtkTreeModelSortClass;
   TGtkTreeModelSortClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TREE_MODEL_SORT : GType;
function GTK_TREE_MODEL_SORT(obj: pointer) : PGtkTreeModelSort;
function GTK_TREE_MODEL_SORT_CLASS(klass: pointer) : PGtkTreeModelSortClass;
function GTK_IS_TREE_MODEL_SORT(obj: pointer) : boolean;
function GTK_IS_TREE_MODEL_SORT_CLASS(klass: pointer) : boolean;
function GTK_TREE_MODEL_SORT_GET_CLASS(obj: pointer) : PGtkTreeModelSortClass;


function gtk_tree_model_sort_get_type:GType; cdecl; external gtklib;
function gtk_tree_model_sort_new_with_model(child_model:PGtkTreeModel):PGtkTreeModel; cdecl; external gtklib;
function gtk_tree_model_sort_get_model(tree_model:PGtkTreeModelSort):PGtkTreeModel; cdecl; external gtklib;
function gtk_tree_model_sort_convert_child_path_to_path(tree_model_sort:PGtkTreeModelSort; child_path:PGtkTreePath):PGtkTreePath; cdecl; external gtklib;
procedure gtk_tree_model_sort_convert_child_iter_to_iter(tree_model_sort:PGtkTreeModelSort; sort_iter:PGtkTreeIter; child_iter:PGtkTreeIter); cdecl; external gtklib;
function gtk_tree_model_sort_convert_path_to_child_path(tree_model_sort:PGtkTreeModelSort; sorted_path:PGtkTreePath):PGtkTreePath; cdecl; external gtklib;
procedure gtk_tree_model_sort_convert_iter_to_child_iter(tree_model_sort:PGtkTreeModelSort; child_iter:PGtkTreeIter; sorted_iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_model_sort_reset_default_sort_func(tree_model_sort:PGtkTreeModelSort); cdecl; external gtklib;
procedure gtk_tree_model_sort_clear_cache(tree_model_sort:PGtkTreeModelSort); cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_tree_model_sort_iter_is_valid(tree_model_sort:PGtkTreeModelSort; iter:PGtkTreeIter): gboolean; cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_MODEL_SORT : GType;
begin
   GTK_TYPE_TREE_MODEL_SORT:=gtk_tree_model_sort_get_type;
end;

function GTK_TREE_MODEL_SORT(obj: pointer) : PGtkTreeModelSort;
begin
   GTK_TREE_MODEL_SORT:=PGtkTreeModelSort(GTK_CHECK_CAST(obj,GTK_TYPE_TREE_MODEL_SORT));
end;

function GTK_TREE_MODEL_SORT_CLASS(klass: pointer) : PGtkTreeModelSortClass;
begin
   GTK_TREE_MODEL_SORT_CLASS:=PGtkTreeModelSortClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE_MODEL_SORT));
end;

function GTK_IS_TREE_MODEL_SORT(obj: pointer) : boolean;
begin
   GTK_IS_TREE_MODEL_SORT:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE_MODEL_SORT);
end;

function GTK_IS_TREE_MODEL_SORT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_MODEL_SORT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE_MODEL_SORT);
end;

function GTK_TREE_MODEL_SORT_GET_CLASS(obj: pointer) : PGtkTreeModelSortClass;
begin
   GTK_TREE_MODEL_SORT_GET_CLASS:=PGtkTreeModelSortClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE_MODEL_SORT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

