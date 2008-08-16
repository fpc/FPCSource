// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkTreeViewColumn = ^TGtkTreeViewColumn;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

   PGtkTreeViewColumnSizing = ^TGtkTreeViewColumnSizing;
   TGtkTreeViewColumnSizing = (
     GTK_TREE_VIEW_COLUMN_GROW_ONLY,
     GTK_TREE_VIEW_COLUMN_AUTOSIZE,
     GTK_TREE_VIEW_COLUMN_FIXED
   );

   TGtkTreeCellDataFunc = procedure (tree_column:PGtkTreeViewColumn;
                                     cell:PGtkCellRenderer;
                                     tree_model:PGtkTreeModel;
                                     iter:PGtkTreeIter; data:gpointer); cdecl;
{ Sizing fields  }
{ see gtk+/doc/tree-column-sizing.txt for more information on them  }
{ dragging columns  }
{ Sorting  }
{ Flags  }
   TGtkTreeViewColumn = record
        parent : TGtkObject;
        tree_view : PGtkWidget;
        button : PGtkWidget;
        child : PGtkWidget;
        arrow : PGtkWidget;
        alignment : PGtkWidget;
        window : PGdkWindow;
        editable_widget : PGtkCellEditable;
        xalign : gfloat;
        property_changed_signal : guint;
        spacing : gint;
        column_type : TGtkTreeViewColumnSizing;
        requested_width : gint;
        button_request : gint;
        resized_width : gint;
        width : gint;
        fixed_width : gint;
        min_width : gint;
        max_width : gint;
        drag_x : gint;
        drag_y : gint;
        title : Pgchar;
        cell_list : PGList;
        sort_clicked_signal : guint;
        sort_column_changed_signal : guint;
        sort_column_id : gint;
        sort_order : TGtkSortType;
        flag0 : word;
     end;


{ Padding for future expansion  }
   PGtkTreeViewColumnClass = ^TGtkTreeViewColumnClass;
   TGtkTreeViewColumnClass = record
        parent_class : TGtkObjectClass;
        clicked : procedure (tree_column:PGtkTreeViewColumn); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTreeViewColumn_visible = $1;
   bp_TGtkTreeViewColumn_visible = 0;
   bm_TGtkTreeViewColumn_resizable = $2;
   bp_TGtkTreeViewColumn_resizable = 1;
   bm_TGtkTreeViewColumn_clickable = $4;
   bp_TGtkTreeViewColumn_clickable = 2;
   bm_TGtkTreeViewColumn_dirty = $8;
   bp_TGtkTreeViewColumn_dirty = 3;
   bm_TGtkTreeViewColumn_show_sort_indicator = $10;
   bp_TGtkTreeViewColumn_show_sort_indicator = 4;
   bm_TGtkTreeViewColumn_maybe_reordered = $20;
   bp_TGtkTreeViewColumn_maybe_reordered = 5;
   bm_TGtkTreeViewColumn_reorderable = $40;
   bp_TGtkTreeViewColumn_reorderable = 6;
   bm_TGtkTreeViewColumn_use_resized_width = $80;
   bp_TGtkTreeViewColumn_use_resized_width = 7;


function GTK_TYPE_TREE_VIEW_COLUMN : GType;
function GTK_TREE_VIEW_COLUMN(obj: pointer) : PGtkTreeViewColumn;
function GTK_TREE_VIEW_COLUMN_CLASS(klass: pointer) : PGtkTreeViewColumnClass;
function GTK_IS_TREE_VIEW_COLUMN(obj: pointer) : boolean;
function GTK_IS_TREE_VIEW_COLUMN_CLASS(klass: pointer) : boolean;
function GTK_TREE_VIEW_COLUMN_GET_CLASS(obj: pointer) : PGtkTreeViewColumnClass;


function visible(var a : TGtkTreeViewColumn) : guint; overload;
procedure set_visible(var a : TGtkTreeViewColumn; __visible : guint); overload;
function resizable(var a : TGtkTreeViewColumn) : guint;
procedure set_resizable(var a : TGtkTreeViewColumn; __resizable : guint);
function clickable(var a : TGtkTreeViewColumn) : guint;
procedure set_clickable(var a : TGtkTreeViewColumn; __clickable : guint);
function dirty(var a : TGtkTreeViewColumn) : guint;
procedure set_dirty(var a : TGtkTreeViewColumn; __dirty : guint);
function show_sort_indicator(var a : TGtkTreeViewColumn) : guint;
procedure set_show_sort_indicator(var a : TGtkTreeViewColumn; __show_sort_indicator : guint);
function maybe_reordered(var a : TGtkTreeViewColumn) : guint;
procedure set_maybe_reordered(var a : TGtkTreeViewColumn; __maybe_reordered : guint);
function reorderable(var a : TGtkTreeViewColumn) : guint; overload;
procedure set_reorderable(var a : TGtkTreeViewColumn; __reorderable : guint); overload;
function use_resized_width(var a : TGtkTreeViewColumn) : guint;
procedure set_use_resized_width(var a : TGtkTreeViewColumn; __use_resized_width : guint);


function gtk_tree_view_column_get_type:TGtkType; cdecl; external gtklib;
function gtk_tree_view_column_new:PGtkTreeViewColumn; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_tree_view_column_new_with_attributes(title:Pgchar; cell:PGtkCellRenderer; args:array of const):PGtkTreeViewColumn; cdecl; overload; external gtklib;
function gtk_tree_view_column_new_with_attributes(title:Pgchar; cell:PGtkCellRenderer):PGtkTreeViewColumn; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_tree_view_column_new_with_attributes(title:Pgchar; cell:PGtkCellRenderer):PGtkTreeViewColumn; varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_tree_view_column_pack_start(tree_column:PGtkTreeViewColumn; cell:PGtkCellRenderer; expand:gboolean); cdecl; external gtklib;
procedure gtk_tree_view_column_pack_end(tree_column:PGtkTreeViewColumn; cell:PGtkCellRenderer; expand:gboolean); cdecl; external gtklib;
procedure gtk_tree_view_column_clear(tree_column:PGtkTreeViewColumn); cdecl; external gtklib;
function gtk_tree_view_column_get_cell_renderers(tree_column:PGtkTreeViewColumn):PGList; cdecl; external gtklib;
procedure gtk_tree_view_column_add_attribute(tree_column:PGtkTreeViewColumn; cell_renderer:PGtkCellRenderer; attribute:Pgchar; column:gint); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_tree_view_column_set_attributes(tree_column:PGtkTreeViewColumn; cell_renderer:PGtkCellRenderer; args:array of const); cdecl; overload; external gtklib;
procedure gtk_tree_view_column_set_attributes(tree_column:PGtkTreeViewColumn; cell_renderer:PGtkCellRenderer); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_tree_view_column_set_attributes(tree_column:PGtkTreeViewColumn; cell_renderer:PGtkCellRenderer); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_tree_view_column_set_cell_data_func(tree_column:PGtkTreeViewColumn; cell_renderer:PGtkCellRenderer; func:TGtkTreeCellDataFunc; func_data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
procedure gtk_tree_view_column_clear_attributes(tree_column:PGtkTreeViewColumn; cell_renderer:PGtkCellRenderer); cdecl; external gtklib;
procedure gtk_tree_view_column_set_spacing(tree_column:PGtkTreeViewColumn; spacing:gint); cdecl; external gtklib;
function gtk_tree_view_column_get_spacing(tree_column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
procedure gtk_tree_view_column_set_visible(tree_column:PGtkTreeViewColumn; visible:gboolean); cdecl; external gtklib;
function gtk_tree_view_column_get_visible(tree_column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_column_set_resizable(tree_column:PGtkTreeViewColumn; resizable:gboolean); cdecl; external gtklib;
function gtk_tree_view_column_get_resizable(tree_column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_column_set_sizing(tree_column:PGtkTreeViewColumn; _type:TGtkTreeViewColumnSizing); cdecl; external gtklib;
function gtk_tree_view_column_get_sizing(tree_column:PGtkTreeViewColumn):TGtkTreeViewColumnSizing; cdecl; external gtklib;
function gtk_tree_view_column_get_width(tree_column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
function gtk_tree_view_column_get_fixed_width(tree_column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
procedure gtk_tree_view_column_set_fixed_width(tree_column:PGtkTreeViewColumn; fixed_width:gint); cdecl; external gtklib;
procedure gtk_tree_view_column_set_min_width(tree_column:PGtkTreeViewColumn; min_width:gint); cdecl; external gtklib;
function gtk_tree_view_column_get_min_width(tree_column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
procedure gtk_tree_view_column_set_max_width(tree_column:PGtkTreeViewColumn; max_width:gint); cdecl; external gtklib;
function gtk_tree_view_column_get_max_width(tree_column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
procedure gtk_tree_view_column_clicked(tree_column:PGtkTreeViewColumn); cdecl; external gtklib;
{ Options for manipulating the column headers
  }
procedure gtk_tree_view_column_set_title(tree_column:PGtkTreeViewColumn; title:Pgchar); cdecl; external gtklib;
function gtk_tree_view_column_get_title(tree_column:PGtkTreeViewColumn):Pgchar; cdecl; external gtklib;
procedure gtk_tree_view_column_set_clickable(tree_column:PGtkTreeViewColumn; clickable:gboolean); cdecl; external gtklib;
function gtk_tree_view_column_get_clickable(tree_column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_column_set_widget(tree_column:PGtkTreeViewColumn; widget:PGtkWidget); cdecl; external gtklib;
function gtk_tree_view_column_get_widget(tree_column:PGtkTreeViewColumn):PGtkWidget; cdecl; external gtklib;
procedure gtk_tree_view_column_set_alignment(tree_column:PGtkTreeViewColumn; xalign:gfloat); cdecl; external gtklib;
function gtk_tree_view_column_get_alignment(tree_column:PGtkTreeViewColumn):gfloat; cdecl; external gtklib;
procedure gtk_tree_view_column_set_reorderable(tree_column:PGtkTreeViewColumn; reorderable:gboolean); cdecl; external gtklib;
function gtk_tree_view_column_get_reorderable(tree_column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;
{ You probably only want to use gtk_tree_view_column_set_sort_column_id.  The
   other sorting functions exist primarily to let others do their own custom sorting.
  }
procedure gtk_tree_view_column_set_sort_column_id(tree_column:PGtkTreeViewColumn; sort_column_id:gint); cdecl; external gtklib;
function gtk_tree_view_column_get_sort_column_id(tree_column:PGtkTreeViewColumn):gint; cdecl; external gtklib;
procedure gtk_tree_view_column_set_sort_indicator(tree_column:PGtkTreeViewColumn; setting:gboolean); cdecl; external gtklib;
function gtk_tree_view_column_get_sort_indicator(tree_column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;
procedure gtk_tree_view_column_set_sort_order(tree_column:PGtkTreeViewColumn; order:TGtkSortType); cdecl; external gtklib;
function gtk_tree_view_column_get_sort_order(tree_column:PGtkTreeViewColumn):TGtkSortType; cdecl; external gtklib;
{ These functions are meant primarily for interaction between the GtkTreeView and the column.
  }
procedure gtk_tree_view_column_cell_set_cell_data(tree_column:PGtkTreeViewColumn; tree_model:PGtkTreeModel; iter:PGtkTreeIter; is_expander:gboolean; is_expanded:gboolean); cdecl; external gtklib;
procedure gtk_tree_view_column_cell_get_size(tree_column:PGtkTreeViewColumn; cell_area:PGdkRectangle; x_offset:Pgint; y_offset:Pgint; width:Pgint;
            height:Pgint); cdecl; external gtklib;
function gtk_tree_view_column_cell_is_visible(tree_column:PGtkTreeViewColumn):gboolean; cdecl; external gtklib;

{$IFDEF HasGTK2_2}
procedure gtk_tree_view_column_focus_cell(tree_column:PGtkTreeViewColumn; cell:PGtkCellRenderer); cdecl; external gtklib;
{$ENDIF}

{$ifdef HasGTK2_4}
procedure gtk_tree_view_column_set_expand(tree_column: PGtkTreeViewColumn; Expand : gboolean); cdecl; external gtklib;
function gtk_tree_view_column_get_expand(tree_column: PGtkTreeViewColumn): gboolean; cdecl; external gtklib;
{$endif}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_VIEW_COLUMN : GType;
begin
   GTK_TYPE_TREE_VIEW_COLUMN:=gtk_tree_view_column_get_type;
end;

function GTK_TREE_VIEW_COLUMN(obj: pointer) : PGtkTreeViewColumn;
begin
   GTK_TREE_VIEW_COLUMN:=PGtkTreeViewColumn(GTK_CHECK_CAST(obj,GTK_TYPE_TREE_VIEW_COLUMN));
end;

function GTK_TREE_VIEW_COLUMN_CLASS(klass: pointer) : PGtkTreeViewColumnClass;
begin
   GTK_TREE_VIEW_COLUMN_CLASS:=PGtkTreeViewColumnClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE_VIEW_COLUMN));
end;

function GTK_IS_TREE_VIEW_COLUMN(obj: pointer) : boolean;
begin
   GTK_IS_TREE_VIEW_COLUMN:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE_VIEW_COLUMN);
end;

function GTK_IS_TREE_VIEW_COLUMN_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_VIEW_COLUMN_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE_VIEW_COLUMN);
end;

function GTK_TREE_VIEW_COLUMN_GET_CLASS(obj: pointer) : PGtkTreeViewColumnClass;
begin
   GTK_TREE_VIEW_COLUMN_GET_CLASS:=PGtkTreeViewColumnClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE_VIEW_COLUMN));
end;

function visible(var a : TGtkTreeViewColumn) : guint;
begin
   visible:=(a.flag0 and bm_TGtkTreeViewColumn_visible) shr bp_TGtkTreeViewColumn_visible;
end;

procedure set_visible(var a : TGtkTreeViewColumn; __visible : guint);
begin
   a.flag0:=a.flag0 or ((__visible shl bp_TGtkTreeViewColumn_visible) and bm_TGtkTreeViewColumn_visible);
end;

function resizable(var a : TGtkTreeViewColumn) : guint;
begin
   resizable:=(a.flag0 and bm_TGtkTreeViewColumn_resizable) shr bp_TGtkTreeViewColumn_resizable;
end;

procedure set_resizable(var a : TGtkTreeViewColumn; __resizable : guint);
begin
   a.flag0:=a.flag0 or ((__resizable shl bp_TGtkTreeViewColumn_resizable) and bm_TGtkTreeViewColumn_resizable);
end;

function clickable(var a : TGtkTreeViewColumn) : guint;
begin
   clickable:=(a.flag0 and bm_TGtkTreeViewColumn_clickable) shr bp_TGtkTreeViewColumn_clickable;
end;

procedure set_clickable(var a : TGtkTreeViewColumn; __clickable : guint);
begin
   a.flag0:=a.flag0 or ((__clickable shl bp_TGtkTreeViewColumn_clickable) and bm_TGtkTreeViewColumn_clickable);
end;

function dirty(var a : TGtkTreeViewColumn) : guint;
begin
   dirty:=(a.flag0 and bm_TGtkTreeViewColumn_dirty) shr bp_TGtkTreeViewColumn_dirty;
end;

procedure set_dirty(var a : TGtkTreeViewColumn; __dirty : guint);
begin
   a.flag0:=a.flag0 or ((__dirty shl bp_TGtkTreeViewColumn_dirty) and bm_TGtkTreeViewColumn_dirty);
end;

function show_sort_indicator(var a : TGtkTreeViewColumn) : guint;
begin
   show_sort_indicator:=(a.flag0 and bm_TGtkTreeViewColumn_show_sort_indicator) shr bp_TGtkTreeViewColumn_show_sort_indicator;
end;

procedure set_show_sort_indicator(var a : TGtkTreeViewColumn; __show_sort_indicator : guint);
begin
   a.flag0:=a.flag0 or ((__show_sort_indicator shl bp_TGtkTreeViewColumn_show_sort_indicator) and bm_TGtkTreeViewColumn_show_sort_indicator);
end;

function maybe_reordered(var a : TGtkTreeViewColumn) : guint;
begin
   maybe_reordered:=(a.flag0 and bm_TGtkTreeViewColumn_maybe_reordered) shr bp_TGtkTreeViewColumn_maybe_reordered;
end;

procedure set_maybe_reordered(var a : TGtkTreeViewColumn; __maybe_reordered : guint);
begin
   a.flag0:=a.flag0 or ((__maybe_reordered shl bp_TGtkTreeViewColumn_maybe_reordered) and bm_TGtkTreeViewColumn_maybe_reordered);
end;

function reorderable(var a : TGtkTreeViewColumn) : guint;
begin
   reorderable:=(a.flag0 and bm_TGtkTreeViewColumn_reorderable) shr bp_TGtkTreeViewColumn_reorderable;
end;

procedure set_reorderable(var a : TGtkTreeViewColumn; __reorderable : guint);
begin
   a.flag0:=a.flag0 or ((__reorderable shl bp_TGtkTreeViewColumn_reorderable) and bm_TGtkTreeViewColumn_reorderable);
end;

function use_resized_width(var a : TGtkTreeViewColumn) : guint;
begin
   use_resized_width:=(a.flag0 and bm_TGtkTreeViewColumn_use_resized_width) shr bp_TGtkTreeViewColumn_use_resized_width;
end;

procedure set_use_resized_width(var a : TGtkTreeViewColumn; __use_resized_width : guint);
begin
   a.flag0:=a.flag0 or ((__use_resized_width shl bp_TGtkTreeViewColumn_use_resized_width) and bm_TGtkTreeViewColumn_use_resized_width);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

