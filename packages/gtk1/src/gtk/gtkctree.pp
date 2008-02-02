{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkCTreePos = (GTK_CTREE_POS_BEFORE,GTK_CTREE_POS_AS_CHILD,
       GTK_CTREE_POS_AFTER);

     TGtkCTreeLineStyle = (GTK_CTREE_LINES_NONE,GTK_CTREE_LINES_SOLID,
       GTK_CTREE_LINES_DOTTED,GTK_CTREE_LINES_TABBED
       );

     TGtkCTreeExpanderStyle = (GTK_CTREE_EXPANDER_NONE,GTK_CTREE_EXPANDER_SQUARE,
       GTK_CTREE_EXPANDER_TRIANGLE,GTK_CTREE_EXPANDER_CIRCULAR
       );

     TGtkCTreeExpansionType = (GTK_CTREE_EXPANSION_EXPAND,GTK_CTREE_EXPANSION_EXPAND_RECURSIVE,
       GTK_CTREE_EXPANSION_COLLAPSE,GTK_CTREE_EXPANSION_COLLAPSE_RECURSIVE,
       GTK_CTREE_EXPANSION_TOGGLE,GTK_CTREE_EXPANSION_TOGGLE_RECURSIVE
       );

     PGtkCTree = ^TGtkCTree;
     PGtkCTreeNode = ^TGtkCTreeNode;
     PGtkCTreeRow = ^TGtkCTreeRow;
     PGtkCTreeClass = ^TGtkCTreeClass;

     TGtkCTreeFunc = procedure (ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer);cdecl;

     TGtkCTreeGNodeFunc = function (ctree:PGtkCTree; depth:guint; gnode:PGNode; cnode:PGtkCTreeNode; data:gpointer):gboolean;cdecl;

     TGtkCTreeCompareDragFunc = function (ctree:PGtkCTree; source_node:PGtkCTreeNode; new_parent:PGtkCTreeNode; new_sibling:PGtkCTreeNode):gboolean;cdecl;

     TGtkCTree = record
          clist : TGtkCList;
          lines_gc : PGdkGC;
          tree_indent : gint;
          tree_spacing : gint;
          tree_column : gint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          drag_compare : TGtkCTreeCompareDragFunc;
       end;

     TGtkCTreeNode = record
          list : TGList;
       end;

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
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

     TGtkCTreeClass = record
          parent_class : TGtkCListClass;
          tree_select_row : procedure (ctree:PGtkCTree; row:PGtkCTreeNode; column:gint);cdecl;
          tree_unselect_row : procedure (ctree:PGtkCTree; row:PGtkCTreeNode; column:gint);cdecl;
          tree_expand : procedure (ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;
          tree_collapse : procedure (ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;
          tree_move : procedure (ctree:PGtkCTree; node:PGtkCTreeNode; new_parent:PGtkCTreeNode; new_sibling:PGtkCTreeNode);cdecl;
          change_focus_row_expansion : procedure (ctree:PGtkCTree; action:TGtkCTreeExpansionType);cdecl;
       end;

  const
     bm_TGtkCTree_line_style = $3;
     bp_TGtkCTree_line_style = 0;
     bm_TGtkCTree_expander_style = $C;
     bp_TGtkCTree_expander_style = 2;
     bm_TGtkCTree_show_stub = $10;
     bp_TGtkCTree_show_stub = 4;
function  line_style(var a : TGtkCTree) : guint;
procedure set_line_style(var a : TGtkCTree; __line_style : guint);
function  expander_style(var a : TGtkCTree) : guint;
procedure set_expander_style(var a : TGtkCTree; __expander_style : guint);
function  show_stub(var a : TGtkCTree) : guint;
procedure set_show_stub(var a : TGtkCTree; __show_stub : guint);

  const
     bm_TGtkCTreeRow_is_leaf = $1;
     bp_TGtkCTreeRow_is_leaf = 0;
     bm_TGtkCTreeRow_expanded = $2;
     bp_TGtkCTreeRow_expanded = 1;
function  is_leaf(var a : TGtkCTreeRow) : guint;
procedure set_is_leaf(var a : TGtkCTreeRow; __is_leaf : guint);
function  expanded(var a : TGtkCTreeRow) : guint;
procedure set_expanded(var a : TGtkCTreeRow; __expanded : guint);

function  GTK_CTREE_ROW(_node_ : PGList) : PGtkCTreeRow;
function  GTK_CTREE_NODE(_node_ : PGList) : PGtkCTreeNode;
function  GTK_CTREE_NODE_NEXT(_nnode_ : PGList) : PGtkCTreeNode;
function  GTK_CTREE_NODE_PREV(_pnode_ : PGList) : PGtkCTreeNode;
function  GTK_CTREE_FUNC(_func_ : pointer) : TGtkCTreeFunc;

type
  GTK_CTREE=PGtkCTree;
  GTK_CTREE_CLASS=PGtkCTreeClass;

function  GTK_CTREE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_ctree_get_type';
function  GTK_IS_CTREE(obj:pointer):boolean;
function  GTK_IS_CTREE_CLASS(klass:pointer):boolean;

function  gtk_ctree_get_type:TGtkType;cdecl;external gtkdll name 'gtk_ctree_get_type';
procedure gtk_ctree_construct(ctree:PGtkCTree; columns:gint; tree_column:gint; titles:PPgchar);cdecl;external gtkdll name 'gtk_ctree_construct';
function  gtk_ctree_new_with_titles(columns:gint; tree_column:gint; titles:PPgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_ctree_new_with_titles';
function  gtk_ctree_new(columns:gint; tree_column:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_ctree_new';
function  gtk_ctree_insert_node(ctree:PGtkCTree; parent:PGtkCTreeNode; sibling:PGtkCTreeNode; text:PPgchar; spacing:guint8; pixmap_closed:PGdkPixmap; mask_closed:PGdkBitmap; pixmap_opened:PGdkPixmap; mask_opened:PGdkBitmap; is_leaf:gboolean; expanded:gboolean):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_insert_node';
procedure gtk_ctree_remove_node(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_remove_node';
function  gtk_ctree_insert_gnode(ctree:PGtkCTree; parent:PGtkCTreeNode; sibling:PGtkCTreeNode; gnode:PGNode; func:TGtkCTreeGNodeFunc; data:gpointer):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_insert_gnode';
function  gtk_ctree_export_to_gnode(ctree:PGtkCTree; parent:PGNode; sibling:PGNode; node:PGtkCTreeNode; func:TGtkCTreeGNodeFunc; data:gpointer):PGNode;cdecl;external gtkdll name 'gtk_ctree_export_to_gnode';
procedure gtk_ctree_post_recursive(ctree:PGtkCTree; node:PGtkCTreeNode; func:TGtkCTreeFunc; data:gpointer);cdecl;external gtkdll name 'gtk_ctree_post_recursive';
procedure gtk_ctree_post_recursive_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint; func:TGtkCTreeFunc; data:gpointer);cdecl;external gtkdll name 'gtk_ctree_post_recursive_to_depth';
procedure gtk_ctree_pre_recursive(ctree:PGtkCTree; node:PGtkCTreeNode; func:TGtkCTreeFunc; data:gpointer);cdecl;external gtkdll name 'gtk_ctree_pre_recursive';
procedure gtk_ctree_pre_recursive_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint; func:TGtkCTreeFunc; data:gpointer);cdecl;external gtkdll name 'gtk_ctree_pre_recursive_to_depth';
function  gtk_ctree_is_viewable(ctree:PGtkCTree; node:PGtkCTreeNode):gboolean;cdecl;external gtkdll name 'gtk_ctree_is_viewable';
function  gtk_ctree_last(ctree:PGtkCTree; node:PGtkCTreeNode):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_last';
function  gtk_ctree_find_node_ptr(ctree:PGtkCTree; ctree_row:PGtkCTreeRow):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_find_node_ptr';
function  gtk_ctree_node_nth(ctree:PGtkCTree; row:guint):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_node_nth';
function  gtk_ctree_find(ctree:PGtkCTree; node:PGtkCTreeNode; child:PGtkCTreeNode):gboolean;cdecl;external gtkdll name 'gtk_ctree_find';
function  gtk_ctree_is_ancestor(ctree:PGtkCTree; node:PGtkCTreeNode; child:PGtkCTreeNode):gboolean;cdecl;external gtkdll name 'gtk_ctree_is_ancestor';
function  gtk_ctree_find_by_row_data(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_find_by_row_data';
{//$ifndef gtkwin}
function  gtk_ctree_find_all_by_row_data(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer):PGList;cdecl;external gtkdll name 'gtk_ctree_find_all_by_row_data';
function  gtk_ctree_find_by_row_data_custom(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer; func:TGCompareFunc):PGtkCTreeNode;cdecl;external gtkdll name 'gtk_ctree_find_by_row_data_custom';
function  gtk_ctree_find_all_by_row_data_custom(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer; func:TGCompareFunc):PGList;cdecl;external gtkdll name 'gtk_ctree_find_all_by_row_data_custom';
{//$endif}
function  gtk_ctree_is_hot_spot(ctree:PGtkCTree; x:gint; y:gint):gboolean;cdecl;external gtkdll name 'gtk_ctree_is_hot_spot';
procedure gtk_ctree_node_set_text(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:Pgchar);cdecl;external gtkdll name 'gtk_ctree_node_set_text';
procedure gtk_ctree_node_set_pixmap(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_ctree_node_set_pixmap';
procedure gtk_ctree_node_set_pixtext(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:Pgchar; spacing:guint8; pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_ctree_node_set_pixtext';
procedure gtk_ctree_set_node_info(ctree:PGtkCTree; node:PGtkCTreeNode; text:Pgchar; spacing:guint8; pixmap_closed:PGdkPixmap; mask_closed:PGdkBitmap; pixmap_opened:PGdkPixmap; mask_opened:PGdkBitmap; is_leaf:gboolean; expanded:gboolean);cdecl;external gtkdll name 'gtk_ctree_set_node_info';
procedure gtk_ctree_node_set_shift(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; vertical:gint; horizontal:gint);cdecl;external gtkdll name 'gtk_ctree_node_set_shift';
function  gtk_ctree_node_get_selectable(ctree:PGtkCTree; node:PGtkCTreeNode):gboolean;cdecl;external gtkdll name 'gtk_ctree_node_get_selectable';
function  gtk_ctree_node_get_cell_type(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint):TGtkCellType;cdecl;external gtkdll name 'gtk_ctree_node_get_cell_type';
function  gtk_ctree_node_get_text(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:PPgchar):gint;cdecl;external gtkdll name 'gtk_ctree_node_get_text';
function  gtk_ctree_node_get_pixmap(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; pixmap:PPGdkPixmap; mask:PPGdkBitmap):gint;cdecl;external gtkdll name 'gtk_ctree_node_get_pixmap';
function  gtk_ctree_node_get_pixtext(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; text:PPgchar; spacing:Pguint8; pixmap:PPGdkPixmap; mask:PPGdkBitmap):gint;cdecl;external gtkdll name 'gtk_ctree_node_get_pixtext';
function  gtk_ctree_get_node_info(ctree:PGtkCTree; node:PGtkCTreeNode; text:PPgchar; spacing:Pguint8; pixmap_closed:PPGdkPixmap; mask_closed:PPGdkBitmap; pixmap_opened:PPGdkPixmap; mask_opened:PPGdkBitmap; is_leaf:Pgboolean; expanded:Pgboolean):gint;cdecl;external gtkdll name 'gtk_ctree_get_node_info';
procedure gtk_ctree_node_set_row_style(ctree:PGtkCTree; node:PGtkCTreeNode; style:PGtkStyle);cdecl;external gtkdll name 'gtk_ctree_node_set_row_style';
function  gtk_ctree_node_get_row_style(ctree:PGtkCTree; node:PGtkCTreeNode):PGtkStyle;cdecl;external gtkdll name 'gtk_ctree_node_get_row_style';
procedure gtk_ctree_node_set_cell_style(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; style:PGtkStyle);cdecl;external gtkdll name 'gtk_ctree_node_set_cell_style';
function  gtk_ctree_node_get_cell_style(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint):PGtkStyle;cdecl;external gtkdll name 'gtk_ctree_node_get_cell_style';
procedure gtk_ctree_node_set_foreground(ctree:PGtkCTree; node:PGtkCTreeNode; color:PGdkColor);cdecl;external gtkdll name 'gtk_ctree_node_set_foreground';
procedure gtk_ctree_node_set_background(ctree:PGtkCTree; node:PGtkCTreeNode; color:PGdkColor);cdecl;external gtkdll name 'gtk_ctree_node_set_background';
procedure gtk_ctree_node_set_row_data(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer);cdecl;external gtkdll name 'gtk_ctree_node_set_row_data';
procedure gtk_ctree_node_set_row_data_full(ctree:PGtkCTree; node:PGtkCTreeNode; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtkdll name 'gtk_ctree_node_set_row_data_full';
function  gtk_ctree_node_get_row_data(ctree:PGtkCTree; node:PGtkCTreeNode):gpointer;cdecl;external gtkdll name 'gtk_ctree_node_get_row_data';
procedure gtk_ctree_node_moveto(ctree:PGtkCTree; node:PGtkCTreeNode; column:gint; row_align:gfloat; col_align:gfloat);cdecl;external gtkdll name 'gtk_ctree_node_moveto';
function  gtk_ctree_node_is_visible(ctree:PGtkCTree; node:PGtkCTreeNode):TGtkVisibility;cdecl;external gtkdll name 'gtk_ctree_node_is_visible';
procedure gtk_ctree_set_indent(ctree:PGtkCTree; indent:gint);cdecl;external gtkdll name 'gtk_ctree_set_indent';
procedure gtk_ctree_set_spacing(ctree:PGtkCTree; spacing:gint);cdecl;external gtkdll name 'gtk_ctree_set_spacing';
{//$ifndef gtkwin}
procedure gtk_ctree_set_show_stub(ctree:PGtkCTree; show_stub:gboolean);cdecl;external gtkdll name 'gtk_ctree_set_show_stub';
{//$endif}
procedure gtk_ctree_set_line_style(ctree:PGtkCTree; line_style:TGtkCTreeLineStyle);cdecl;external gtkdll name 'gtk_ctree_set_line_style';
procedure gtk_ctree_set_expander_style(ctree:PGtkCTree; expander_style:TGtkCTreeExpanderStyle);cdecl;external gtkdll name 'gtk_ctree_set_expander_style';
procedure gtk_ctree_set_drag_compare_func(ctree:PGtkCTree; cmp_func:TGtkCTreeCompareDragFunc);cdecl;external gtkdll name 'gtk_ctree_set_drag_compare_func';
procedure gtk_ctree_sort_node(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_sort_node';
procedure gtk_ctree_sort_recursive(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_sort_recursive';

procedure gtk_ctree_move(ctree:PGtkCTree; node:PGtkCTreeNode; new_parent:PGtkCTreeNode; new_sibling:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_move';
procedure gtk_ctree_expand(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_expand';
procedure gtk_ctree_expand_recursive(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_expand_recursive';
procedure gtk_ctree_expand_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint);cdecl;external gtkdll name 'gtk_ctree_expand_to_depth';
procedure gtk_ctree_collapse(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_collapse';
procedure gtk_ctree_collapse_recursive(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_collapse_recursive';
procedure gtk_ctree_collapse_to_depth(ctree:PGtkCTree; node:PGtkCTreeNode; depth:gint);cdecl;external gtkdll name 'gtk_ctree_collapse_to_depth';
procedure gtk_ctree_select(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_select';
procedure gtk_ctree_select_recursive(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_select_recursive';
procedure gtk_ctree_unselect(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_unselect';
procedure gtk_ctree_unselect_recursive(ctree:PGtkCTree; node:PGtkCTreeNode);cdecl;external gtkdll name 'gtk_ctree_unselect_recursive';
procedure gtk_ctree_real_select_recursive(ctree:PGtkCTree; node:PGtkCTreeNode; state:gint);cdecl;external gtkdll name 'gtk_ctree_real_select_recursive';
procedure gtk_ctree_node_set_selectable(ctree:PGtkCTree; node:PGtkCTreeNode; selectable:gboolean);cdecl;external gtkdll name 'gtk_ctree_node_set_selectable';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_CTREE_ROW(_node_ : PGList) : PGtkCTreeRow;
    begin
       GTK_CTREE_ROW:=PGtkCTreeRow((PGList(_node_))^.data);
    end;

function  GTK_CTREE_NODE(_node_ : PGList) : PGtkCTreeNode;
    begin
       GTK_CTREE_NODE:=PGtkCTreeNode(_node_);
    end;

function  GTK_CTREE_NODE_NEXT(_nnode_ : PGList) : PGtkCTreeNode;
    begin
       GTK_CTREE_NODE_NEXT:=PGtkCTreeNode((PGList(_nnode_))^.next);
    end;

function  GTK_CTREE_NODE_PREV(_pnode_ : PGList) : PGtkCTreeNode;
    begin
       GTK_CTREE_NODE_PREV:=PGtkCTreeNode((PGList(_pnode_))^.prev);
    end;

function  GTK_CTREE_FUNC(_func_ : pointer) : TGtkCTreeFunc;
    begin
       GTK_CTREE_FUNC:=TGtkCTreeFunc(_func_);
    end;

function  line_style(var a : TGtkCTree) : guint;
    begin
       line_style:=(a.flag0 and bm_TGtkCTree_line_style) shr bp_TGtkCTree_line_style;
    end;

procedure set_line_style(var a : TGtkCTree; __line_style : guint);
    begin
       a.flag0:=a.flag0 or ((__line_style shl bp_TGtkCTree_line_style) and bm_TGtkCTree_line_style);
    end;

function  expander_style(var a : TGtkCTree) : guint;
    begin
       expander_style:=(a.flag0 and bm_TGtkCTree_expander_style) shr bp_TGtkCTree_expander_style;
    end;

procedure set_expander_style(var a : TGtkCTree; __expander_style : guint);
    begin
       a.flag0:=a.flag0 or ((__expander_style shl bp_TGtkCTree_expander_style) and bm_TGtkCTree_expander_style);
    end;

function  show_stub(var a : TGtkCTree) : guint;
    begin
       show_stub:=(a.flag0 and bm_TGtkCTree_show_stub) shr bp_TGtkCTree_show_stub;
    end;

procedure set_show_stub(var a : TGtkCTree; __show_stub : guint);
    begin
       a.flag0:=a.flag0 or ((__show_stub shl bp_TGtkCTree_show_stub) and bm_TGtkCTree_show_stub);
    end;

function  is_leaf(var a : TGtkCTreeRow) : guint;
    begin
       is_leaf:=(a.flag0 and bm_TGtkCTreeRow_is_leaf) shr bp_TGtkCTreeRow_is_leaf;
    end;

procedure set_is_leaf(var a : TGtkCTreeRow; __is_leaf : guint);
    begin
       a.flag0:=a.flag0 or ((__is_leaf shl bp_TGtkCTreeRow_is_leaf) and bm_TGtkCTreeRow_is_leaf);
    end;

function  expanded(var a : TGtkCTreeRow) : guint;
    begin
       expanded:=(a.flag0 and bm_TGtkCTreeRow_expanded) shr bp_TGtkCTreeRow_expanded;
    end;

procedure set_expanded(var a : TGtkCTreeRow; __expanded : guint);
    begin
       a.flag0:=a.flag0 or ((__expanded shl bp_TGtkCTreeRow_expanded) and bm_TGtkCTreeRow_expanded);
    end;

function  GTK_IS_CTREE(obj:pointer):boolean;
begin
  GTK_IS_CTREE:=(obj<>nil) and GTK_IS_CTREE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CTREE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CTREE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CTREE_TYPE);
end;

{$endif read_implementation}

