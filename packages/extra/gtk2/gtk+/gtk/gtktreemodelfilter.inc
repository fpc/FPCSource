// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type

   TGtkTreeModelFilterVisibleFunc = function (model:PGtkTreeModel; iter:PGtkTreeIter; data:gpointer):gboolean;cdecl;

   TGtkTreeModelFilterModifyFunc = procedure (model:PGtkTreeModel; iter:PGtkTreeIter; value:PGValue; column:gint; data:gpointer);cdecl;

   PGtkTreeModelFilterPrivate = pointer;

   PGtkTreeModelFilter = ^TGtkTreeModelFilter;
   TGtkTreeModelFilter = record
        parent : TGObject;
        priv : PGtkTreeModelFilterPrivate;
     end;

{ Padding for future expansion  }
   PGtkTreeModelFilterClass = ^TGtkTreeModelFilterClass;
   TGtkTreeModelFilterClass = record
        parent_class : TGObjectClass;
        _gtk_reserved0 : procedure ;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_TREE_MODEL_FILTER : GType;
function GTK_TREE_MODEL_FILTER(obj : pointer) : PGtkTreeModelFilter;
function GTK_TREE_MODEL_FILTER_CLASS(vtable : pointer) : PGtkTreeModelFilterClass;
function GTK_IS_TREE_MODEL_FILTER(obj : pointer) : gboolean;
function GTK_IS_TREE_MODEL_FILTER_CLASS(vtable : pointer) : gboolean;
function GTK_TREE_MODEL_FILTER_GET_CLASS(inst : pointer) : PGtkTreeModelFilterClass;


{ base  }

function gtk_tree_model_filter_get_type:GType;cdecl;external gtklib name 'gtk_tree_model_filter_get_type';

function gtk_tree_model_filter_new(child_model:PGtkTreeModel; root:PGtkTreePath):PGtkTreeModel;cdecl;external gtklib name 'gtk_tree_model_filter_new';

procedure gtk_tree_model_filter_set_visible_func(filter:PGtkTreeModelFilter; func:TGtkTreeModelFilterVisibleFunc; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtklib name 'gtk_tree_model_filter_set_visible_func';
procedure gtk_tree_model_filter_set_modify_func(filter:PGtkTreeModelFilter; n_columns:gint; types:PGType; func:TGtkTreeModelFilterModifyFunc; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtklib name 'gtk_tree_model_filter_set_modify_func';

procedure gtk_tree_model_filter_set_visible_column(filter:PGtkTreeModelFilter; column:gint);cdecl;external gtklib name 'gtk_tree_model_filter_set_visible_column';
function gtk_tree_model_filter_get_model(filter:PGtkTreeModelFilter):PGtkTreeModel;cdecl;external gtklib name 'gtk_tree_model_filter_get_model';


{ conversion  }
procedure gtk_tree_model_filter_convert_child_iter_to_iter(filter:PGtkTreeModelFilter; filter_iter:PGtkTreeIter; child_iter:PGtkTreeIter);cdecl;external gtklib name 'gtk_tree_model_filter_convert_child_iter_to_iter';

procedure gtk_tree_model_filter_convert_iter_to_child_iter(filter:PGtkTreeModelFilter; child_iter:PGtkTreeIter; filter_iter:PGtkTreeIter);cdecl;external gtklib name 'gtk_tree_model_filter_convert_iter_to_child_iter';

function gtk_tree_model_filter_convert_child_path_to_path(filter:PGtkTreeModelFilter; child_path:PGtkTreePath):PGtkTreePath;cdecl;external gtklib name 'gtk_tree_model_filter_convert_child_path_to_path';

function gtk_tree_model_filter_convert_path_to_child_path(filter:PGtkTreeModelFilter; filter_path:PGtkTreePath):PGtkTreePath;cdecl;external gtklib name 'gtk_tree_model_filter_convert_path_to_child_path';


{ extras  }
procedure gtk_tree_model_filter_refilter(filter:PGtkTreeModelFilter);cdecl;external gtklib name 'gtk_tree_model_filter_refilter';
procedure gtk_tree_model_filter_clear_cache(filter:PGtkTreeModelFilter);cdecl;external gtklib name 'gtk_tree_model_filter_clear_cache';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_TREE_MODEL_FILTER : GType;
begin
   GTK_TYPE_TREE_MODEL_FILTER:=gtk_tree_model_filter_get_type;
end;

function GTK_TREE_MODEL_FILTER(obj : pointer) : PGtkTreeModelFilter;
begin
   GTK_TREE_MODEL_FILTER:=PGtkTreeModelFilter(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TREE_MODEL_FILTER));
end;

function GTK_TREE_MODEL_FILTER_CLASS(vtable : pointer) : PGtkTreeModelFilterClass;
begin
   GTK_TREE_MODEL_FILTER_CLASS:=PGtkTreeModelFilterClass(G_TYPE_CHECK_CLASS_CAST(vtable,GTK_TYPE_TREE_MODEL_FILTER));
end;

function GTK_IS_TREE_MODEL_FILTER(obj : pointer) : gboolean;
begin
   GTK_IS_TREE_MODEL_FILTER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TREE_MODEL_FILTER);
end;

function GTK_IS_TREE_MODEL_FILTER_CLASS(vtable : pointer) : gboolean;
begin
   GTK_IS_TREE_MODEL_FILTER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(vtable,GTK_TYPE_TREE_MODEL_FILTER);
end;

function GTK_TREE_MODEL_FILTER_GET_CLASS(inst : pointer) : PGtkTreeModelFilterClass;
begin
   GTK_TREE_MODEL_FILTER_GET_CLASS:=PGtkTreeModelFilterClass(G_TYPE_INSTANCE_GET_CLASS(inst,GTK_TYPE_TREE_MODEL_FILTER));
end;


{$endif read_implementation}
