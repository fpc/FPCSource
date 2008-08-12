// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkTreeView = ^TGtkTreeView;

   TGtkTreeViewColumnDropFunc = function (tree_view:PGtkTreeView; column:PGtkTreeViewColumn; prev_column:PGtkTreeViewColumn; next_column:PGtkTreeViewColumn; data:gpointer):gboolean; cdecl;

   TGtkTreeViewMappingFunc = procedure (tree_view:PGtkTreeView; path:PGtkTreePath; user_data:gpointer); cdecl;

   TGtkTreeViewSearchEqualFunc = function (model:PGtkTreeModel; column:gint; key:Pgchar; iter:PGtkTreeIter; search_data:gpointer):gboolean; cdecl;

{ This function should really never be used.  It is just for use by ATK.
  }
   TGtkTreeDestroyCountFunc = procedure (tree_view:PGtkTreeView; path:PGtkTreePath; children:gint; user_data:gpointer); cdecl;

{ drop before/after this row  }
{ drop as a child of this row (with fallback to before or after
     if into is not possible)
    }
   PGtkTreeViewDropPosition = ^TGtkTreeViewDropPosition;
   TGtkTreeViewDropPosition = (
     GTK_TREE_VIEW_DROP_BEFORE,
     GTK_TREE_VIEW_DROP_AFTER,
     GTK_TREE_VIEW_DROP_INTO_OR_BEFORE,
     GTK_TREE_VIEW_DROP_INTO_OR_AFTER
   );

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

   TGtkTreeView = record
        parent : TGtkContainer;
        priv : PGtkTreeViewPrivate;
     end;

{ Key Binding signals  }
{ Padding for future expansion  }
   PGtkTreeViewClass = ^TGtkTreeViewClass;
   TGtkTreeViewClass = record
        parent_class : TGtkContainerClass;
        set_scroll_adjustments : procedure (tree_view:PGtkTreeView; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment); cdecl;
        row_activated : procedure (tree_view:PGtkTreeView; path:PGtkTreePath; column:PGtkTreeViewColumn); cdecl;
        test_expand_row : function (tree_view:PGtkTreeView; iter:PGtkTreeIter; path:PGtkTreePath):gboolean; cdecl;
        test_collapse_row : function (tree_view:PGtkTreeView; iter:PGtkTreeIter; path:PGtkTreePath):gboolean; cdecl;
        row_expanded : procedure (tree_view:PGtkTreeView; iter:PGtkTreeIter; path:PGtkTreePath); cdecl;
        row_collapsed : procedure (tree_view:PGtkTreeView; iter:PGtkTreeIter; path:PGtkTreePath); cdecl;
        columns_changed : procedure (tree_view:PGtkTreeView); cdecl;
        cursor_changed : procedure (tree_view:PGtkTreeView); cdecl;
        move_cursor : function (tree_view:PGtkTreeView; step:TGtkMovementStep; count:gint):gboolean; cdecl;
        select_all : procedure (tree_view:PGtkTreeView); cdecl;
        unselect_all : procedure (tree_view:PGtkTreeView); cdecl;
        select_cursor_row : procedure (tree_view:PGtkTreeView; start_editing:gboolean); cdecl;
        toggle_cursor_row : procedure (tree_view:PGtkTreeView); cdecl;
        expand_collapse_cursor_row : procedure (tree_view:PGtkTreeView; logical:gboolean; expand:gboolean; open_all:gboolean); cdecl;
        select_cursor_parent : procedure (tree_view:PGtkTreeView); cdecl;
        start_interactive_search : procedure (tree_view:PGtkTreeView); cdecl;
        _gtk_reserved0 : procedure ; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TREE_VIEW : GType;
function GTK_TREE_VIEW(obj: pointer) : PGtkTreeView;
function GTK_TREE_VIEW_CLASS(klass: pointer) : PGtkTreeViewClass;
function GTK_IS_TREE_VIEW(obj: pointer) : boolean;
function GTK_IS_TREE_VIEW_CLASS(klass: pointer) : boolean;
function GTK_TREE_VIEW_GET_CLASS(obj: pointer) : PGtkTreeViewClass;

{ Creators  }

function gtk_tree_view_get_type:TGtkType; cdecl; external gtklib;
function gtk_tree_view_new:PGtkWidget; cdecl; external gtklib;
function gtk_tree_view_new_with_model(model:PGtkTreeModel):PGtkWidget; cdecl; external gtklib;
{ Accessors  }
function gtk_tree_view_get_model(tree_view:PGtkTreeView):PGtkTreeModel; cdecl; external gtklib;
procedure gtk_tree_view_set_model(tree_view:PGtkTreeView; model:PGtkTreeModel); cdecl; external gtklib;
function gtk_tree_view_get_selection(tree_view:PGtkTreeView):PGtkTreeSelection; cdecl; external gtklib;
function gtk_tree_view_get_hadjustment(tree_view:PGtkTreeView):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_tree_view_set_hadjustment(tree_view:PGtkTreeView; adjustment:PGtkAdjustment); cdecl; external gtklib;
function gtk_tree_view_get_vadjustment(tree_view:PGtkTreeView):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_tree_view_set_vadjustment(tree_view:PGtkTreeView; adjustment:PGtkAdjustment); cdecl; external gtklib;
function gtk_tree_view_get_headers_visible(tree_view:PGtkTreeView):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_set_headers_visible(tree_view:PGtkTreeView; headers_visible:gboolean); cdecl; external gtklib;
procedure gtk_tree_view_columns_autosize(tree_view:PGtkTreeView); cdecl; external gtklib;
procedure gtk_tree_view_set_headers_clickable(tree_view:PGtkTreeView; setting:gboolean); cdecl; external gtklib;
procedure gtk_tree_view_set_rules_hint(tree_view:PGtkTreeView; setting:gboolean); cdecl; external gtklib;
function gtk_tree_view_get_rules_hint(tree_view:PGtkTreeView):gboolean; cdecl; external gtklib;
{ Column funtions  }
function gtk_tree_view_append_column(tree_view:PGtkTreeView; column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
function gtk_tree_view_remove_column(tree_view:PGtkTreeView; column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
function gtk_tree_view_insert_column(tree_view:PGtkTreeView; column:PGtkTreeViewColumn; position:gint):gint; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_tree_view_insert_column_with_attributes(tree_view:PGtkTreeView; position:gint; title:Pgchar; cell:PGtkCellRenderer; args:array of const):gint; cdecl; overload; external gtklib;
function gtk_tree_view_insert_column_with_attributes(tree_view:PGtkTreeView; position:gint; title:Pgchar; cell:PGtkCellRenderer):gint; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_tree_view_insert_column_with_attributes(tree_view:PGtkTreeView; position:gint; title:Pgchar; cell:PGtkCellRenderer):gint; varargs; cdecl; external gtklib;
{$ENDIF}
function gtk_tree_view_insert_column_with_data_func(tree_view:PGtkTreeView; position:gint; title:Pgchar; cell:PGtkCellRenderer; func:TGtkTreeCellDataFunc;
           data:gpointer; dnotify:TGDestroyNotify):gint; cdecl; external gtklib;
function gtk_tree_view_get_column(tree_view:PGtkTreeView; n:gint):PGtkTreeViewColumn; cdecl; external gtklib;
function gtk_tree_view_get_columns(tree_view:PGtkTreeView):PGList; cdecl; external gtklib;
procedure gtk_tree_view_move_column_after(tree_view:PGtkTreeView; column:PGtkTreeViewColumn; base_column:PGtkTreeViewColumn); cdecl; external gtklib;
procedure gtk_tree_view_set_expander_column(tree_view:PGtkTreeView; column:PGtkTreeViewColumn); cdecl; external gtklib;
function gtk_tree_view_get_expander_column(tree_view:PGtkTreeView):PGtkTreeViewColumn; cdecl; external gtklib;
procedure gtk_tree_view_set_column_drag_function(tree_view:PGtkTreeView; func:TGtkTreeViewColumnDropFunc; user_data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
{ Actions  }
procedure gtk_tree_view_scroll_to_point(tree_view:PGtkTreeView; tree_x:gint; tree_y:gint); cdecl; external gtklib;
procedure gtk_tree_view_scroll_to_cell(tree_view:PGtkTreeView; path:PGtkTreePath; column:PGtkTreeViewColumn; use_align:gboolean; row_align:gfloat;
            col_align:gfloat); cdecl; external gtklib;
procedure gtk_tree_view_row_activated(tree_view:PGtkTreeView; path:PGtkTreePath; column:PGtkTreeViewColumn); cdecl; external gtklib;
procedure gtk_tree_view_expand_all(tree_view:PGtkTreeView); cdecl; external gtklib;
procedure gtk_tree_view_collapse_all(tree_view:PGtkTreeView); cdecl; external gtklib;
function gtk_tree_view_expand_row(tree_view:PGtkTreeView; path:PGtkTreePath; open_all:gboolean):gboolean; cdecl; external gtklib;
function gtk_tree_view_collapse_row(tree_view:PGtkTreeView; path:PGtkTreePath):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_map_expanded_rows(tree_view:PGtkTreeView; func:TGtkTreeViewMappingFunc; data:gpointer); cdecl; external gtklib;
function gtk_tree_view_row_expanded(tree_view:PGtkTreeView; path:PGtkTreePath):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_set_reorderable(tree_view:PGtkTreeView; reorderable:gboolean); cdecl; external gtklib;
function gtk_tree_view_get_reorderable(tree_view:PGtkTreeView):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_set_cursor(tree_view:PGtkTreeView; path:PGtkTreePath; focus_column:PGtkTreeViewColumn; start_editing:gboolean); cdecl; external gtklib;
procedure gtk_tree_view_get_cursor(tree_view:PGtkTreeView; var path:PGtkTreePath; var focus_column:PGtkTreeViewColumn); cdecl; external gtklib;
{ Layout information  }
function gtk_tree_view_get_bin_window(tree_view:PGtkTreeView):PGdkWindow; cdecl; external gtklib;
function gtk_tree_view_get_path_at_pos(tree_view:PGtkTreeView; x:gint; y:gint; var path:PGtkTreePath; var column:PGtkTreeViewColumn;
           cell_x:Pgint; cell_y:Pgint):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_get_cell_area(tree_view:PGtkTreeView; path:PGtkTreePath; column:PGtkTreeViewColumn; rect:PGdkRectangle); cdecl; external gtklib;
procedure gtk_tree_view_get_background_area(tree_view:PGtkTreeView; path:PGtkTreePath; column:PGtkTreeViewColumn; rect:PGdkRectangle); cdecl; external gtklib;
procedure gtk_tree_view_get_visible_rect(tree_view:PGtkTreeView; visible_rect:PGdkRectangle); cdecl; external gtklib;
procedure gtk_tree_view_widget_to_tree_coords(tree_view:PGtkTreeView; wx:gint; wy:gint; tx:Pgint; ty:Pgint); cdecl; external gtklib;
procedure gtk_tree_view_tree_to_widget_coords(tree_view:PGtkTreeView; tx:gint; ty:gint; wx:Pgint; wy:Pgint); cdecl; external gtklib;
{ Drag-and-Drop support  }
procedure gtk_tree_view_enable_model_drag_source(tree_view:PGtkTreeView; start_button_mask:TGdkModifierType; targets:PGtkTargetEntry; n_targets:gint; actions:TGdkDragAction); cdecl; external gtklib;
procedure gtk_tree_view_enable_model_drag_dest(tree_view:PGtkTreeView; targets:PGtkTargetEntry; n_targets:gint; actions:TGdkDragAction); cdecl; external gtklib;
procedure gtk_tree_view_unset_rows_drag_source(tree_view:PGtkTreeView); cdecl; external gtklib;
procedure gtk_tree_view_unset_rows_drag_dest(tree_view:PGtkTreeView); cdecl; external gtklib;
{ These are useful to implement your own custom stuff.  }
procedure gtk_tree_view_set_drag_dest_row(tree_view:PGtkTreeView; path:PGtkTreePath; pos:TGtkTreeViewDropPosition); cdecl; external gtklib;
procedure gtk_tree_view_get_drag_dest_row(tree_view:PGtkTreeView; var path:PGtkTreePath; pos:PGtkTreeViewDropPosition); cdecl; external gtklib;
function gtk_tree_view_get_dest_row_at_pos(tree_view:PGtkTreeView; drag_x:gint; drag_y:gint; var path:PGtkTreePath; pos:PGtkTreeViewDropPosition):gboolean; cdecl; external gtklib;
function gtk_tree_view_create_row_drag_icon(tree_view:PGtkTreeView; path:PGtkTreePath):PGdkPixmap; cdecl; external gtklib;
{ Interactive search  }
procedure gtk_tree_view_set_enable_search(tree_view:PGtkTreeView; enable_search:gboolean); cdecl; external gtklib;
function gtk_tree_view_get_enable_search(tree_view:PGtkTreeView):gboolean; cdecl; external gtklib;
function gtk_tree_view_get_search_column(tree_view:PGtkTreeView):gint; cdecl; external gtklib;
procedure gtk_tree_view_set_search_column(tree_view:PGtkTreeView; column:gint); cdecl; external gtklib;
function gtk_tree_view_get_search_equal_func(tree_view:PGtkTreeView):TGtkTreeViewSearchEqualFunc; cdecl; external gtklib;
procedure gtk_tree_view_set_search_equal_func(tree_view:PGtkTreeView; search_equal_func:TGtkTreeViewSearchEqualFunc; search_user_data:gpointer; search_destroy:TGtkDestroyNotify); cdecl; external gtklib;

procedure gtk_tree_view_set_destroy_count_func(tree_view:PGtkTreeView; func:TGtkTreeDestroyCountFunc; data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;

{$IFDEF HasGTK2_2}
procedure gtk_tree_view_expand_to_path(tree_view:PGtkTreeView; path: PGtkTreePath); cdecl; external gtklib;
procedure gtk_tree_view_set_cursor_on_cell(tree_view:PGtkTreeView; path:PGtkTreePath; focus_column:PGtkTreeViewColumn; focus_cell:PGtkCellRenderer; start_editing:gboolean); cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_VIEW : GType;
begin
   GTK_TYPE_TREE_VIEW:=gtk_tree_view_get_type;
end;

function GTK_TREE_VIEW(obj: pointer) : PGtkTreeView;
begin
   GTK_TREE_VIEW:=PGtkTreeView(GTK_CHECK_CAST(obj,GTK_TYPE_TREE_VIEW));
end;

function GTK_TREE_VIEW_CLASS(klass: pointer) : PGtkTreeViewClass;
begin
   GTK_TREE_VIEW_CLASS:=PGtkTreeViewClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE_VIEW));
end;

function GTK_IS_TREE_VIEW(obj: pointer) : boolean;
begin
   GTK_IS_TREE_VIEW:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE_VIEW);
end;

function GTK_IS_TREE_VIEW_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_VIEW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE_VIEW);
end;

function GTK_TREE_VIEW_GET_CLASS(obj: pointer) : PGtkTreeViewClass;
begin
   GTK_TREE_VIEW_GET_CLASS:=PGtkTreeViewClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE_VIEW));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

