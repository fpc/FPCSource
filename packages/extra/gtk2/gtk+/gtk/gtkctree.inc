// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCTreePos = ^TGtkCTreePos;
   TGtkCTreePos = (
     GTK_CTREE_POS_BEFORE,
     GTK_CTREE_POS_AS_CHILD,
     GTK_CTREE_POS_AFTER
   );

   PGtkCTreeLineStyle = ^TGtkCTreeLineStyle;
   TGtkCTreeLineStyle = (
     GTK_CTREE_LINES_NONE,
     GTK_CTREE_LINES_SOLID,
     GTK_CTREE_LINES_DOTTED,
     GTK_CTREE_LINES_TABBED
   );

   PGtkCTreeExpanderStyle = ^TGtkCTreeExpanderStyle;
   TGtkCTreeExpanderStyle = (
     GTK_CTREE_EXPANDER_NONE,
     GTK_CTREE_EXPANDER_SQUARE,
     GTK_CTREE_EXPANDER_TRIANGLE,
     GTK_CTREE_EXPANDER_CIRCULAR
   );

   PGtkCTreeExpansionType = ^TGtkCTreeExpansionType;
   TGtkCTreeExpansionType = (
     GTK_CTREE_EXPANSION_EXPAND,
     GTK_CTREE_EXPANSION_EXPAND_RECURSIVE,
     GTK_CTREE_EXPANSION_COLLAPSE,
     GTK_CTREE_EXPANSION_COLLAPSE_RECURSIVE,
     GTK_CTREE_EXPANSION_TOGGLE,
     GTK_CTREE_EXPANSION_TOGGLE_RECURSIVE
   );

   PGtkCTree = ^TGtkCTree;
   PGtkCTreeNode = ^TGtkCTreeNode;

   TGtkCTreeFunc = procedure (ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer); cdecl;

   TGtkCTreeGNodeFunc = function (ctree:PGtkCTree; depth:guint; gnode:PGNode; cnode:PGtkCTreeNode; data:gpointer):gboolean; cdecl;

   TGtkCTreeCompareDragFunc = function (ctree:PGtkCTree; source_node:PGtkCTreeNode; new_parent:PGtkCTreeNode; new_sibling:PGtkCTreeNode):gboolean; cdecl;
   TGtkCTree = record
        clist : TGtkCList;
        lines_gc : PGdkGC;
        tree_indent : gint;
        tree_spacing : gint;
        tree_column : gint;
        flag0 : word;
        drag_compare : TGtkCTreeCompareDragFunc;
     end;

   PGtkCTreeClass = ^TGtkCTreeClass;
   TGtkCTreeClass = record
        parent_class : TGtkCListClass;
        tree_select_row : procedure (ctree:PGtkCTree; row:PGtkCTreeNode; column:gint); cdecl;
        tree_unselect_row : procedure (ctree:PGtkCTree; row:PGtkCTreeNode; column:gint); cdecl;
        tree_expand : procedure (ctree:PGtkCTree; node:PGtkCTreeNode); cdecl;
        tree_collapse : procedure (ctree:PGtkCTree; node:PGtkCTreeNode); cdecl;
        tree_move : procedure (ctree:PGtkCTree; node:PGtkCTreeNode; new_parent:PGtkCTreeNode; new_sibling:PGtkCTreeNode); cdecl;
        change_focus_row_expansion : procedure (ctree:PGtkCTree; action:TGtkCTreeExpansionType); cdecl;
     end;

   PGtkCTreeRow = ^TGtkCTreeRow;
   TGtkCTreeRow = record
        row : TGtkCListRow;
        parent : PGtkCTreeNode;
        sibling : PGtkCTreeNode;
        children : PGtkCTreeNode;
        pixmap_closed : PGdkPixmap;
        mask_closed : PGdkBitmap;
        pixmap_opened : PGdkPixmap;
        mask_opened : PGdkBitmap;
        level : guint16;
        flag0 : word;
     end;

   TGtkCTreeNode = record
        list : TGList;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkCTree_line_style = $3;
   bp_TGtkCTree_line_style = 0;
   bm_TGtkCTree_expander_style = $C;
   bp_TGtkCTree_expander_style = 2;
   bm_TGtkCTree_show_stub = $10;
   bp_TGtkCTree_show_stub = 4;

   bm_TGtkCTreeRow_is_leaf = $1;
   bp_TGtkCTreeRow_is_leaf = 0;
   bm_TGtkCTreeRow_expanded = $2;
   bp_TGtkCTreeRow_expanded = 1;


function GTK_TYPE_CTREE : GType;
function GTK_CTREE(obj: pointer) : PGtkCTree;
function GTK_CTREE_CLASS(klass: pointer) : PGtkCTreeClass;
function GTK_IS_CTREE(obj: pointer) : boolean;
function GTK_IS_CTREE_CLASS(klass: pointer) : boolean;
function GTK_CTREE_GET_CLASS(obj: pointer) : PGtkCTreeClass;

function GTK_CTREE_ROW(_node_ : ptrint) : PGtkCTreeRow;
function GTK_CTREE_NODE(_node_ : ptrint) : PGtkCTreeNode;
function GTK_CTREE_NODE_NEXT(_nnode_ : ptrint) : PGtkCTreeNode;
function GTK_CTREE_NODE_PREV(_pnode_ : ptrint) : PGtkCTreeNode;
function GTK_CTREE_FUNC(_func_ : ptrint) : TGtkCTreeFunc;
function GTK_TYPE_CTREE_NODE : GType;

function line_style(var a : TGtkCTree) : guint;
procedure set_line_style(var a : TGtkCTree; __line_style : guint);
function expander_style(var a : TGtkCTree) : guint;
procedure set_expander_style(var a : TGtkCTree; __expander_style : guint);
function show_stub(var a : TGtkCTree) : guint;
procedure set_show_stub(var a : TGtkCTree; __show_stub : guint);

function is_leaf(var a : TGtkCTreeRow) : guint;
procedure set_is_leaf(var a : TGtkCTreeRow; __is_leaf : guint);
function expanded(var a : TGtkCTreeRow) : guint; overload;
procedure set_expanded(var a : TGtkCTreeRow; __expanded : guint); overload;

{
             Creation, insertion, deletion
                                                            }

function gtk_ctree_get_type:TGtkType; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_ctree_new_with_titles(columns:gint; tree_column:gint; titles:array of Pgchar):PGtkWidget; cdecl; external gtklib;
{$ELSE}
function gtk_ctree_new_with_titles(columns:gint; tree_column:gint):PGtkWidget; varargs; cdecl; external gtklib;
{$ENDIF}
function gtk_ctree_new(columns:gint; tree_column:gint):PGtkWidget; cdecl; external gtklib;
function gtk_ctree_insert_node(ctree:PGtkCTree; parent:PGtkCTreeNode; sibling:PGtkCTreeNode; text:array of Pgchar; spacing:guint8;
           pixmap_closed:PGdkPixmap; mask_closed:PGdkBitmap; pixmap_opened:PGdkPixmap; mask_opened:PGdkBitmap; is_leaf:gboolean;
           expanded:gboolean):PGtkCTreeNode; cdecl; external gtklib;
procedure gtk_ctree_remove_node(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
function gtk_ctree_insert_gnode(ctree:PGtkCTree; parent:PGtkCTreeNode; sibling:PGtkCTreeNode; gnode:PGNode; func:TGtkCTreeGNodeFunc;
           data:gpointer):PGtkCTreeNode; cdecl; external gtklib;
function gtk_ctree_export_to_gnode(ctree:PGtkCTree; parent:PGNode; sibling:PGNode; node:PGtkCTreeNode; func:TGtkCTreeGNodeFunc;
           data:gpointer):PGNode; cdecl; external gtklib;
{
    Generic recursive functions, querying / finding tree
    information
                                                            }
procedure gtk_ctree_post_recursive(ctree:PGtkCTree; node:PGtkCTreeNode; func:TGtkCTreeFunc; data:gpointer); cdecl; external gtklib;
procedure gtk_ctree_post_recursive_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint; func:TGtkCTreeFunc; data:gpointer); cdecl; external gtklib;
procedure gtk_ctree_pre_recursive(ctree:PGtkCTree; node:PGtkCTreeNode; func:TGtkCTreeFunc; data:gpointer); cdecl; external gtklib;
procedure gtk_ctree_pre_recursive_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint; func:TGtkCTreeFunc; data:gpointer); cdecl; external gtklib;
function gtk_ctree_is_viewable(ctree:PGtkCTree; node:PGtkCTreeNode):gboolean; cdecl; external gtklib;
function gtk_ctree_last(ctree:PGtkCTree; node:PGtkCTreeNode):PGtkCTreeNode; cdecl; external gtklib;
function gtk_ctree_find_node_ptr(ctree:PGtkCTree; ctree_row:PGtkCTreeRow):PGtkCTreeNode; cdecl; external gtklib;
function gtk_ctree_node_nth(ctree:PGtkCTree; row:guint):PGtkCTreeNode; cdecl; external gtklib;
function gtk_ctree_find(ctree:PGtkCTree; node:PGtkCTreeNode; child:PGtkCTreeNode):gboolean; cdecl; external gtklib;
function gtk_ctree_is_ancestor(ctree:PGtkCTree; node:PGtkCTreeNode; child:PGtkCTreeNode):gboolean; cdecl; external gtklib;
function gtk_ctree_find_by_row_data(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer):PGtkCTreeNode; cdecl; external gtklib;
{ returns a GList of all GtkCTreeNodes with row->data == data.  }
function gtk_ctree_find_all_by_row_data(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer):PGList; cdecl; external gtklib;
function gtk_ctree_find_by_row_data_custom(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer; func:TGCompareFunc):PGtkCTreeNode; cdecl; external gtklib;
{ returns a GList of all GtkCTreeNodes with row->data == data.  }
function gtk_ctree_find_all_by_row_data_custom(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer; func:TGCompareFunc):PGList; cdecl; external gtklib;
function gtk_ctree_is_hot_spot(ctree:PGtkCTree; x:gint; y:gint):gboolean; cdecl; external gtklib;
{
     Tree signals : move, expand, collapse, (un)select
                                                            }
procedure gtk_ctree_move(ctree:PGtkCTree; node:PGtkCTreeNode; new_parent:PGtkCTreeNode; new_sibling:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_expand(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_expand_recursive(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_expand_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint); cdecl; external gtklib;
procedure gtk_ctree_collapse(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_collapse_recursive(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_collapse_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint); cdecl; external gtklib;
procedure gtk_ctree_toggle_expansion(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_toggle_expansion_recursive(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_select(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_select_recursive(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_unselect(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_unselect_recursive(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_real_select_recursive(ctree:PGtkCTree; node:PGtkCTreeNode; state:gint); cdecl; external gtklib;
{
             Analogons of GtkCList functions
                                                            }
procedure gtk_ctree_node_set_text(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:Pgchar); cdecl; external gtklib;
procedure gtk_ctree_node_set_pixmap(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_ctree_node_set_pixtext(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:Pgchar; spacing:guint8;
            pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_ctree_set_node_info(ctree:PGtkCTree; node:PGtkCTreeNode; text:Pgchar; spacing:guint8; pixmap_closed:PGdkPixmap;
            mask_closed:PGdkBitmap; pixmap_opened:PGdkPixmap; mask_opened:PGdkBitmap; is_leaf:gboolean; expanded:gboolean); cdecl; external gtklib;
procedure gtk_ctree_node_set_shift(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; vertical:gint; horizontal:gint); cdecl; external gtklib;
procedure gtk_ctree_node_set_selectable(ctree:PGtkCTree; node:PGtkCTreeNode; selectable:gboolean); cdecl; external gtklib;
function gtk_ctree_node_get_selectable(ctree:PGtkCTree; node:PGtkCTreeNode):gboolean; cdecl; external gtklib;
function gtk_ctree_node_get_cell_type(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint):TGtkCellType; cdecl; external gtklib;
function gtk_ctree_node_get_text(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:PPgchar):gboolean; cdecl; external gtklib;
function gtk_ctree_node_get_pixmap(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; var pixmap:PGdkPixmap; var mask:PGdkBitmap):gboolean; cdecl; external gtklib;
function gtk_ctree_node_get_pixtext(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:PPgchar; spacing:Pguint8;
           var pixmap:PGdkPixmap; var mask:PGdkBitmap):gboolean; cdecl; external gtklib;
function gtk_ctree_get_node_info(ctree:PGtkCTree; node:PGtkCTreeNode; text:PPgchar; spacing:Pguint8; var pixmap_closed:PGdkPixmap;
           var mask_closed:PGdkBitmap; var pixmap_opened:PGdkPixmap; var mask_opened:PGdkBitmap; is_leaf:Pgboolean; expanded:Pgboolean):gboolean; cdecl; external gtklib;
procedure gtk_ctree_node_set_row_style(ctree:PGtkCTree; node:PGtkCTreeNode; style:PGtkStyle); cdecl; external gtklib;
function gtk_ctree_node_get_row_style(ctree:PGtkCTree; node:PGtkCTreeNode):PGtkStyle; cdecl; external gtklib;
procedure gtk_ctree_node_set_cell_style(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; style:PGtkStyle); cdecl; external gtklib;
function gtk_ctree_node_get_cell_style(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint):PGtkStyle; cdecl; external gtklib;
procedure gtk_ctree_node_set_foreground(ctree:PGtkCTree; node:PGtkCTreeNode; color:PGdkColor); cdecl; external gtklib;
procedure gtk_ctree_node_set_background(ctree:PGtkCTree; node:PGtkCTreeNode; color:PGdkColor); cdecl; external gtklib;
procedure gtk_ctree_node_set_row_data(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer); cdecl; external gtklib;
procedure gtk_ctree_node_set_row_data_full(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
function gtk_ctree_node_get_row_data(ctree:PGtkCTree; node:PGtkCTreeNode):gpointer; cdecl; external gtklib;
procedure gtk_ctree_node_moveto(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; row_align:gfloat; col_align:gfloat); cdecl; external gtklib;
function gtk_ctree_node_is_visible(ctree:PGtkCTree; node:PGtkCTreeNode):TGtkVisibility; cdecl; external gtklib;
{
               GtkCTree specific functions
                                                            }
procedure gtk_ctree_set_indent(ctree:PGtkCTree; indent:gint); cdecl; external gtklib;
procedure gtk_ctree_set_spacing(ctree:PGtkCTree; spacing:gint); cdecl; external gtklib;
procedure gtk_ctree_set_show_stub(ctree:PGtkCTree; show_stub:gboolean); cdecl; external gtklib;
procedure gtk_ctree_set_line_style(ctree:PGtkCTree; line_style:TGtkCTreeLineStyle); cdecl; external gtklib;
procedure gtk_ctree_set_expander_style(ctree:PGtkCTree; expander_style:TGtkCTreeExpanderStyle); cdecl; external gtklib;
procedure gtk_ctree_set_drag_compare_func(ctree:PGtkCTree; cmp_func:TGtkCTreeCompareDragFunc); cdecl; external gtklib;
{
               Tree sorting functions
                                                            }
procedure gtk_ctree_sort_node(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_sort_recursive(ctree:PGtkCTree; node:PGtkCTreeNode); cdecl; external gtklib;
procedure gtk_ctree_set_reorderable(t: pointer; r : boolean);

{ GType for the GtkCTreeNode.  This is a boxed type, although it uses
   no-op's for the copy and free routines.  It is defined in order to
   provide type information for the signal arguments
  }
function gtk_ctree_node_get_type:GType; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CTREE : GType;
begin
   GTK_TYPE_CTREE:=gtk_ctree_get_type;
end;

function GTK_CTREE(obj: pointer) : PGtkCTree;
begin
   GTK_CTREE:=PGtkCTree(GTK_CHECK_CAST(obj,GTK_TYPE_CTREE));
end;

function GTK_CTREE_CLASS(klass: pointer) : PGtkCTreeClass;
begin
   GTK_CTREE_CLASS:=PGtkCTreeClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CTREE));
end;

function GTK_IS_CTREE(obj: pointer) : boolean;
begin
   GTK_IS_CTREE:=GTK_CHECK_TYPE(obj,GTK_TYPE_CTREE);
end;

function GTK_IS_CTREE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CTREE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CTREE);
end;

function GTK_CTREE_GET_CLASS(obj: pointer) : PGtkCTreeClass;
begin
   GTK_CTREE_GET_CLASS:=PGtkCTreeClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CTREE));
end;

function GTK_CTREE_ROW(_node_ : ptrint) : PGtkCTreeRow;
begin
   GTK_CTREE_ROW:=PGtkCTreeRow((PGList(_node_))^.data);
end;

function GTK_CTREE_NODE(_node_ : ptrint) : PGtkCTreeNode;
begin
   GTK_CTREE_NODE:=PGtkCTreeNode(_node_);
end;

function GTK_CTREE_NODE_NEXT(_nnode_ : ptrint) : PGtkCTreeNode;
begin
   GTK_CTREE_NODE_NEXT:=PGtkCTreeNode((PGList(_nnode_))^.next);
end;

function GTK_CTREE_NODE_PREV(_pnode_ : ptrint) : PGtkCTreeNode;
begin
   GTK_CTREE_NODE_PREV:=PGtkCTreeNode((PGList(_pnode_))^.prev);
end;

function GTK_CTREE_FUNC(_func_ : ptrint) : TGtkCTreeFunc;
begin
   GTK_CTREE_FUNC:=TGtkCTreeFunc(_func_);
end;

function GTK_TYPE_CTREE_NODE : GType;
begin
   GTK_TYPE_CTREE_NODE:=gtk_ctree_node_get_type;
end;

function line_style(var a : TGtkCTree) : guint;
begin
   line_style:=(a.flag0 and bm_TGtkCTree_line_style) shr bp_TGtkCTree_line_style;
end;

procedure set_line_style(var a : TGtkCTree; __line_style : guint);
begin
   a.flag0:=a.flag0 or ((__line_style shl bp_TGtkCTree_line_style) and bm_TGtkCTree_line_style);
end;

function expander_style(var a : TGtkCTree) : guint;
begin
   expander_style:=(a.flag0 and bm_TGtkCTree_expander_style) shr bp_TGtkCTree_expander_style;
end;

procedure set_expander_style(var a : TGtkCTree; __expander_style : guint);
begin
   a.flag0:=a.flag0 or ((__expander_style shl bp_TGtkCTree_expander_style) and bm_TGtkCTree_expander_style);
end;

function show_stub(var a : TGtkCTree) : guint;
begin
   show_stub:=(a.flag0 and bm_TGtkCTree_show_stub) shr bp_TGtkCTree_show_stub;
end;

procedure set_show_stub(var a : TGtkCTree; __show_stub : guint);
begin
   a.flag0:=a.flag0 or ((__show_stub shl bp_TGtkCTree_show_stub) and bm_TGtkCTree_show_stub);
end;

function is_leaf(var a : TGtkCTreeRow) : guint;
begin
   is_leaf:=(a.flag0 and bm_TGtkCTreeRow_is_leaf) shr bp_TGtkCTreeRow_is_leaf;
end;

procedure set_is_leaf(var a : TGtkCTreeRow; __is_leaf : guint);
begin
   a.flag0:=a.flag0 or ((__is_leaf shl bp_TGtkCTreeRow_is_leaf) and bm_TGtkCTreeRow_is_leaf);
end;

function expanded(var a : TGtkCTreeRow) : guint;
begin
   expanded:=(a.flag0 and bm_TGtkCTreeRow_expanded) shr bp_TGtkCTreeRow_expanded;
end;

procedure set_expanded(var a : TGtkCTreeRow; __expanded : guint);
begin
   a.flag0:=a.flag0 or ((__expanded shl bp_TGtkCTreeRow_expanded) and bm_TGtkCTreeRow_expanded);
end;

procedure gtk_ctree_set_reorderable(t: pointer; r : boolean);
begin
   gtk_clist_set_reorderable(PGtkCList(t),r);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

