// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkTreeRowReference = pointer;

   PGtkTreeViewFlags = ^TGtkTreeViewFlags;
   TGtkTreeViewFlags = longint;

   TGtkTreeViewSearchDialogPositionFunc = procedure (tree_view:PGtkTreeView; search_dialog:PGtkWidget); cdecl;
   PGtkTreeViewColumnReorder = ^TGtkTreeViewColumnReorder;
   TGtkTreeViewColumnReorder = record
        left_align : gint;
        right_align : gint;
        left_column : PGtkTreeViewColumn;
        right_column : PGtkTreeViewColumn;
     end;

{ tree information  }
{ bin_window offset  }
{ dy == y pos of top_row + top_row_dy  }
{ we cache it for simplicity of the code  }
{ Focus code  }
{ Selection stuff  }
{ Column Resizing  }
{ Prelight information  }
{ The node that's currently being collapsed or expanded  }
{ Selection information  }
{ Header information  }
{ ATK Hack  }
{ Scroll timeout (e.g. during dnd)  }
{ Row drag-and-drop  }
{ Scroll-to functionality when unrealized  }
{ hint to display rows in alternating colors  }
{ interactive search  }
   PGtkTreeViewPrivate = ^TGtkTreeViewPrivate;
   TGtkTreeViewPrivate = record
        model : PGtkTreeModel;
        flags : guint;
        tree : PGtkRBTree;
        button_pressed_node : PGtkRBNode;
        button_pressed_tree : PGtkRBTree;
        children : PGList;
        width : gint;
        height : gint;
        expander_size : gint;
        hadjustment : PGtkAdjustment;
        vadjustment : PGtkAdjustment;
        bin_window : PGdkWindow;
        header_window : PGdkWindow;
        drag_window : PGdkWindow;
        drag_highlight_window : PGdkWindow;
        drag_column : PGtkTreeViewColumn;
        last_button_press : PGtkTreeRowReference;
        last_button_press_2 : PGtkTreeRowReference;
        top_row : PGtkTreeRowReference;
        top_row_dy : gint;
        dy : gint;
        drag_column_x : gint;
        expander_column : PGtkTreeViewColumn;
        edited_column : PGtkTreeViewColumn;
        presize_handler_timer : guint;
        validate_rows_timer : guint;
        scroll_sync_timer : guint;
        focus_column : PGtkTreeViewColumn;
        anchor : PGtkTreeRowReference;
        cursor : PGtkTreeRowReference;
        drag_pos : gint;
        x_drag : gint;
        prelight_node : PGtkRBNode;
        prelight_tree : PGtkRBTree;
        expanded_collapsed_node : PGtkRBNode;
        expanded_collapsed_tree : PGtkRBTree;
        expand_collapse_timeout : guint;
        selection : PGtkTreeSelection;
        n_columns : gint;
        columns : PGList;
        header_height : gint;
        column_drop_func : TGtkTreeViewColumnDropFunc;
        column_drop_func_data : gpointer;
        column_drop_func_data_destroy : TGtkDestroyNotify;
        column_drag_info : PGList;
        cur_reorder : PGtkTreeViewColumnReorder;
        destroy_count_func : TGtkTreeDestroyCountFunc;
        destroy_count_data : gpointer;
        destroy_count_destroy : TGtkDestroyNotify;
        scroll_timeout : guint;
        drag_dest_row : PGtkTreeRowReference;
        drag_dest_pos : TGtkTreeViewDropPosition;
        open_dest_timeout : guint;
        pressed_button : gint;
        press_start_x : gint;
        press_start_y : gint;
        scroll_to_path : PGtkTreeRowReference;
        scroll_to_column : PGtkTreeViewColumn;
        scroll_to_row_align : gfloat;
        scroll_to_col_align : gfloat;
        flag0 : word;
        search_column : gint;
        search_dialog_position_func : TGtkTreeViewSearchDialogPositionFunc;
        search_equal_func : TGtkTreeViewSearchEqualFunc;
        search_user_data : gpointer;
        search_destroy : TGtkDestroyNotify;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   TREE_VIEW_DRAG_WIDTH = 6;

   GTK_TREE_VIEW_IS_LIST = 1 shl 0;
   GTK_TREE_VIEW_SHOW_EXPANDERS = 1 shl 1;
   GTK_TREE_VIEW_IN_COLUMN_RESIZE = 1 shl 2;
   GTK_TREE_VIEW_ARROW_PRELIT = 1 shl 3;
   GTK_TREE_VIEW_HEADERS_VISIBLE = 1 shl 4;
   GTK_TREE_VIEW_DRAW_KEYFOCUS = 1 shl 5;
   GTK_TREE_VIEW_MODEL_SETUP = 1 shl 6;
   GTK_TREE_VIEW_IN_COLUMN_DRAG = 1 shl 7;

   DRAG_COLUMN_WINDOW_STATE_UNSET = 0;
   DRAG_COLUMN_WINDOW_STATE_ORIGINAL = 1;
   DRAG_COLUMN_WINDOW_STATE_ARROW = 2;
   DRAG_COLUMN_WINDOW_STATE_ARROW_LEFT = 3;
   DRAG_COLUMN_WINDOW_STATE_ARROW_RIGHT = 4;

procedure GTK_TREE_VIEW_SET_FLAG(tree_view: PGtkTreeView; flag : guint);
procedure GTK_TREE_VIEW_UNSET_FLAG(tree_view: PGtkTreeView; flag : guint);
function GTK_TREE_VIEW_FLAG_SET(tree_view: PGtkTreeView; flag : guint) : boolean;
function TREE_VIEW_HEADER_HEIGHT(tree_view : PGtkTreeView) : longint;
function TREE_VIEW_COLUMN_REQUESTED_WIDTH(column : PGtkTreeViewColumn) : longint;
function TREE_VIEW_DRAW_EXPANDERS(tree_view : PGtkTreeView) : boolean;

{ This lovely little value is used to determine how far away from the title bar
    you can move the mouse and still have a column drag work.
   }
function TREE_VIEW_COLUMN_DRAG_DEAD_MULTIPLIER(tree_view : PGtkTreeView) : longint;


const
   bm_TGtkTreeViewPrivate_scroll_to_use_align = $1;
   bp_TGtkTreeViewPrivate_scroll_to_use_align = 0;
   bm_TGtkTreeViewPrivate_fixed_height_check = $2;
   bp_TGtkTreeViewPrivate_fixed_height_check = 1;
   bm_TGtkTreeViewPrivate_reorderable = $4;
   bp_TGtkTreeViewPrivate_reorderable = 2;
   bm_TGtkTreeViewPrivate_header_has_focus = $8;
   bp_TGtkTreeViewPrivate_header_has_focus = 3;
   bm_TGtkTreeViewPrivate_drag_column_window_state = $70;
   bp_TGtkTreeViewPrivate_drag_column_window_state = 4;
   bm_TGtkTreeViewPrivate_has_rules = $80;
   bp_TGtkTreeViewPrivate_has_rules = 7;
   bm_TGtkTreeViewPrivate_mark_rows_col_dirty = $100;
   bp_TGtkTreeViewPrivate_mark_rows_col_dirty = 8;
   bm_TGtkTreeViewPrivate_enable_search = $200;
   bp_TGtkTreeViewPrivate_enable_search = 9;
   bm_TGtkTreeViewPrivate_disable_popdown = $400;
   bp_TGtkTreeViewPrivate_disable_popdown = 10;

function scroll_to_use_align(var a : TGtkTreeViewPrivate) : guint;
procedure set_scroll_to_use_align(var a : TGtkTreeViewPrivate; __scroll_to_use_align : guint);
function fixed_height_check(var a : TGtkTreeViewPrivate) : guint;
procedure set_fixed_height_check(var a : TGtkTreeViewPrivate; __fixed_height_check : guint);
function reorderable(var a : TGtkTreeViewPrivate) : guint; overload;
procedure set_reorderable(var a : TGtkTreeViewPrivate; __reorderable : guint); overload;
function header_has_focus(var a : TGtkTreeViewPrivate) : guint;
procedure set_header_has_focus(var a : TGtkTreeViewPrivate; __header_has_focus : guint);
function drag_column_window_state(var a : TGtkTreeViewPrivate) : guint;
procedure set_drag_column_window_state(var a : TGtkTreeViewPrivate; __drag_column_window_state : guint);
function has_rules(var a : TGtkTreeViewPrivate) : guint;
procedure set_has_rules(var a : TGtkTreeViewPrivate; __has_rules : guint);
function mark_rows_col_dirty(var a : TGtkTreeViewPrivate) : guint;
procedure set_mark_rows_col_dirty(var a : TGtkTreeViewPrivate; __mark_rows_col_dirty : guint);
function enable_search(var a : TGtkTreeViewPrivate) : guint;
procedure set_enable_search(var a : TGtkTreeViewPrivate; __enable_search : guint);
function disable_popdown(var a : TGtkTreeViewPrivate) : guint;
procedure set_disable_popdown(var a : TGtkTreeViewPrivate; __disable_popdown : guint);


{ functions that shouldn't be exported }
procedure _gtk_tree_selection_internal_select_node(selection:PGtkTreeSelection; node:PGtkRBNode; tree:PGtkRBTree; path:PGtkTreePath; state:TGdkModifierType;
            override_browse_mode:gboolean); cdecl; external gtklib;
function _gtk_tree_view_find_node(tree_view:PGtkTreeView; path:PGtkTreePath; var tree:PGtkRBTree; var node:PGtkRBNode):gboolean; cdecl; external gtklib;
function _gtk_tree_view_find_path(tree_view:PGtkTreeView; tree:PGtkRBTree; node:PGtkRBNode):PGtkTreePath; cdecl; external gtklib;
procedure _gtk_tree_view_child_move_resize(tree_view:PGtkTreeView; widget:PGtkWidget; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure _gtk_tree_view_queue_draw_node(tree_view:PGtkTreeView; tree:PGtkRBTree; node:PGtkRBNode; clip_rect:PGdkRectangle); cdecl; external gtklib;
procedure _gtk_tree_view_column_realize_button(column:PGtkTreeViewColumn); cdecl; external gtklib;
procedure _gtk_tree_view_column_unrealize_button(column:PGtkTreeViewColumn); cdecl; external gtklib;
procedure _gtk_tree_view_column_set_tree_view(column:PGtkTreeViewColumn; tree_view:PGtkTreeView); cdecl; external gtklib;
procedure _gtk_tree_view_column_unset_tree_view(column:PGtkTreeViewColumn); cdecl; external gtklib;
procedure _gtk_tree_view_column_set_width(column:PGtkTreeViewColumn; width:gint); cdecl; external gtklib;
procedure _gtk_tree_view_column_start_drag(tree_view:PGtkTreeView; column:PGtkTreeViewColumn); cdecl; external gtklib;
function _gtk_tree_view_column_cell_event(tree_column:PGtkTreeViewColumn; var editable_widget:PGtkCellEditable; event:PGdkEvent; path_string:Pgchar; background_area:PGdkRectangle;
           cell_area:PGdkRectangle; flags:guint):gboolean; cdecl; external gtklib;
procedure _gtk_tree_view_column_start_editing(tree_column:PGtkTreeViewColumn; editable_widget:PGtkCellEditable); cdecl; external gtklib;
procedure _gtk_tree_view_column_stop_editing(tree_column:PGtkTreeViewColumn); cdecl; external gtklib;
procedure _gtk_tree_view_install_mark_rows_col_dirty(tree_view:PGtkTreeView); cdecl; external gtklib;
procedure _gtk_tree_view_column_autosize(tree_view:PGtkTreeView; column:PGtkTreeViewColumn); cdecl; external gtklib;
function _gtk_tree_view_column_has_editable_cell(column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;
function _gtk_tree_view_column_get_edited_cell(column:PGtkTreeViewColumn):PGtkCellRenderer; cdecl; external gtklib;
function _gtk_tree_view_column_count_special_cells(column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
function _gtk_tree_view_column_get_cell_at_pos(column:PGtkTreeViewColumn; x:gint):PGtkCellRenderer; cdecl; external gtklib;
function _gtk_tree_selection_new:PGtkTreeSelection; cdecl; external gtklib;
function _gtk_tree_selection_new_with_tree_view(tree_view:PGtkTreeView):PGtkTreeSelection; cdecl; external gtklib;
procedure _gtk_tree_selection_set_tree_view(selection:PGtkTreeSelection; tree_view:PGtkTreeView); cdecl; external gtklib;
procedure _gtk_tree_view_column_cell_render(tree_column:PGtkTreeViewColumn; window:PGdkWindow; background_area:PGdkRectangle; cell_area:PGdkRectangle; expose_area:PGdkRectangle;
            flags:guint); cdecl; external gtklib;
function _gtk_tree_view_column_cell_focus(tree_column:PGtkTreeViewColumn; direction:gint; left:gboolean; right:gboolean):gboolean; cdecl; external gtklib;
procedure _gtk_tree_view_column_cell_draw_focus(tree_column:PGtkTreeViewColumn; window:PGdkWindow; background_area:PGdkRectangle; cell_area:PGdkRectangle; expose_area:PGdkRectangle;
            flags:guint); cdecl; external gtklib;
procedure _gtk_tree_view_column_cell_set_dirty(tree_column:PGtkTreeViewColumn; install_handler:gboolean); cdecl; external gtklib;
procedure _gtk_tree_view_column_get_neighbor_sizes(column:PGtkTreeViewColumn; cell:PGtkCellRenderer; left:Pgint; right:Pgint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TREE_VIEW_FLAG_SET(tree_view: PGtkTreeView; flag : guint) : boolean;
begin
   GTK_TREE_VIEW_FLAG_SET:=((tree_view^.priv^.flags) and flag) = flag;
end;

function TREE_VIEW_HEADER_HEIGHT(tree_view : PGtkTreeView) : longint;
var
   if_local1 : longint;
begin
   if GTK_TREE_VIEW_FLAG_SET(tree_view,GTK_TREE_VIEW_HEADERS_VISIBLE) then
     if_local1:=tree_view^.priv^.header_height
   else
     if_local1:=0;
   TREE_VIEW_HEADER_HEIGHT:=if_local1;
end;

function TREE_VIEW_COLUMN_REQUESTED_WIDTH(column : PGtkTreeViewColumn) : longint;
var
   MinWidth, MaxWidth: integer;
begin
   if column^.min_width<>-1 then
     MinWidth:=column^.min_width
   else
     MinWidth:=column^.requested_width;
   if column^.max_width<>-1 then
     MaxWidth:=column^.max_width
   else
     MaxWidth:=column^.requested_width;
   TREE_VIEW_COLUMN_REQUESTED_WIDTH:=CLAMP(column^.requested_width,MinWidth,MaxWidth);
end;

function TREE_VIEW_DRAW_EXPANDERS(tree_view : PGtkTreeView) : boolean;
begin
   TREE_VIEW_DRAW_EXPANDERS:=(not (GTK_TREE_VIEW_FLAG_SET(tree_view,GTK_TREE_VIEW_IS_LIST)))
            and (GTK_TREE_VIEW_FLAG_SET(tree_view,GTK_TREE_VIEW_SHOW_EXPANDERS));
end;

function TREE_VIEW_COLUMN_DRAG_DEAD_MULTIPLIER(tree_view : PGtkTreeView) : longint;
begin
   TREE_VIEW_COLUMN_DRAG_DEAD_MULTIPLIER:=10 * (TREE_VIEW_HEADER_HEIGHT(tree_view));
end;

function scroll_to_use_align(var a : TGtkTreeViewPrivate) : guint;
begin
   scroll_to_use_align:=(a.flag0 and bm_TGtkTreeViewPrivate_scroll_to_use_align) shr bp_TGtkTreeViewPrivate_scroll_to_use_align;
end;

procedure set_scroll_to_use_align(var a : TGtkTreeViewPrivate; __scroll_to_use_align : guint);
begin
   a.flag0:=a.flag0 or ((__scroll_to_use_align shl bp_TGtkTreeViewPrivate_scroll_to_use_align) and bm_TGtkTreeViewPrivate_scroll_to_use_align);
end;

function fixed_height_check(var a : TGtkTreeViewPrivate) : guint;
begin
   fixed_height_check:=(a.flag0 and bm_TGtkTreeViewPrivate_fixed_height_check) shr bp_TGtkTreeViewPrivate_fixed_height_check;
end;

procedure set_fixed_height_check(var a : TGtkTreeViewPrivate; __fixed_height_check : guint);
begin
   a.flag0:=a.flag0 or ((__fixed_height_check shl bp_TGtkTreeViewPrivate_fixed_height_check) and bm_TGtkTreeViewPrivate_fixed_height_check);
end;

function reorderable(var a : TGtkTreeViewPrivate) : guint;
begin
   reorderable:=(a.flag0 and bm_TGtkTreeViewPrivate_reorderable) shr bp_TGtkTreeViewPrivate_reorderable;
end;

procedure set_reorderable(var a : TGtkTreeViewPrivate; __reorderable : guint);
begin
   a.flag0:=a.flag0 or ((__reorderable shl bp_TGtkTreeViewPrivate_reorderable) and bm_TGtkTreeViewPrivate_reorderable);
end;

function header_has_focus(var a : TGtkTreeViewPrivate) : guint;
begin
   header_has_focus:=(a.flag0 and bm_TGtkTreeViewPrivate_header_has_focus) shr bp_TGtkTreeViewPrivate_header_has_focus;
end;

procedure set_header_has_focus(var a : TGtkTreeViewPrivate; __header_has_focus : guint);
begin
   a.flag0:=a.flag0 or ((__header_has_focus shl bp_TGtkTreeViewPrivate_header_has_focus) and bm_TGtkTreeViewPrivate_header_has_focus);
end;

function drag_column_window_state(var a : TGtkTreeViewPrivate) : guint;
begin
   drag_column_window_state:=(a.flag0 and bm_TGtkTreeViewPrivate_drag_column_window_state) shr bp_TGtkTreeViewPrivate_drag_column_window_state;
end;

procedure set_drag_column_window_state(var a : TGtkTreeViewPrivate; __drag_column_window_state : guint);
begin
   a.flag0:=a.flag0 or ((__drag_column_window_state shl bp_TGtkTreeViewPrivate_drag_column_window_state) and bm_TGtkTreeViewPrivate_drag_column_window_state);
end;

function has_rules(var a : TGtkTreeViewPrivate) : guint;
begin
   has_rules:=(a.flag0 and bm_TGtkTreeViewPrivate_has_rules) shr bp_TGtkTreeViewPrivate_has_rules;
end;

procedure set_has_rules(var a : TGtkTreeViewPrivate; __has_rules : guint);
begin
   a.flag0:=a.flag0 or ((__has_rules shl bp_TGtkTreeViewPrivate_has_rules) and bm_TGtkTreeViewPrivate_has_rules);
end;

function mark_rows_col_dirty(var a : TGtkTreeViewPrivate) : guint;
begin
   mark_rows_col_dirty:=(a.flag0 and bm_TGtkTreeViewPrivate_mark_rows_col_dirty) shr bp_TGtkTreeViewPrivate_mark_rows_col_dirty;
end;

procedure set_mark_rows_col_dirty(var a : TGtkTreeViewPrivate; __mark_rows_col_dirty : guint);
begin
   a.flag0:=a.flag0 or ((__mark_rows_col_dirty shl bp_TGtkTreeViewPrivate_mark_rows_col_dirty) and bm_TGtkTreeViewPrivate_mark_rows_col_dirty);
end;

function enable_search(var a : TGtkTreeViewPrivate) : guint;
begin
   enable_search:=(a.flag0 and bm_TGtkTreeViewPrivate_enable_search) shr bp_TGtkTreeViewPrivate_enable_search;
end;

procedure set_enable_search(var a : TGtkTreeViewPrivate; __enable_search : guint);
begin
   a.flag0:=a.flag0 or ((__enable_search shl bp_TGtkTreeViewPrivate_enable_search) and bm_TGtkTreeViewPrivate_enable_search);
end;

function disable_popdown(var a : TGtkTreeViewPrivate) : guint;
begin
   disable_popdown:=(a.flag0 and bm_TGtkTreeViewPrivate_disable_popdown) shr bp_TGtkTreeViewPrivate_disable_popdown;
end;

procedure set_disable_popdown(var a : TGtkTreeViewPrivate; __disable_popdown : guint);
begin
   a.flag0:=a.flag0 or ((__disable_popdown shl bp_TGtkTreeViewPrivate_disable_popdown) and bm_TGtkTreeViewPrivate_disable_popdown);
end;

procedure GTK_TREE_VIEW_SET_FLAG(tree_view: PGtkTreeView; flag : guint);
begin
  tree_view^.priv^.flags:=tree_view^.priv^.flags or flag;
end;

procedure GTK_TREE_VIEW_UNSET_FLAG(tree_view: PGtkTreeView; flag : guint);
begin
  tree_view^.priv^.flags:=tree_view^.priv^.flags and not flag;
end;


{$ENDIF read_implementation}
// included by gtk2.pas

