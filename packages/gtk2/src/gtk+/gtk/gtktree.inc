// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ default view mode  }
   PGtkTreeViewMode = ^TGtkTreeViewMode;
   TGtkTreeViewMode = (
     GTK_TREE_VIEW_LINE,
     GTK_TREE_VIEW_ITEM
   );

{ owner of selection list  }
   PGtkTree = ^TGtkTree;
   TGtkTree = record
        container : TGtkContainer;
        children : PGList;
        root_tree : PGtkTree;
        tree_owner : PGtkWidget;
        selection : PGList;
        level : guint;
        indent_value : guint;
        current_indent : guint;
        flag0 : word;
     end;

   PGtkTreeClass = ^TGtkTreeClass;
   TGtkTreeClass = record
        parent_class : TGtkContainerClass;
        selection_changed : procedure (tree:PGtkTree); cdecl;
        select_child : procedure (tree:PGtkTree; child:PGtkWidget); cdecl;
        unselect_child : procedure (tree:PGtkTree; child:PGtkWidget); cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTree_selection_mode = $3;
   bp_TGtkTree_selection_mode = 0;
   bm_TGtkTree_view_mode = $4;
   bp_TGtkTree_view_mode = 2;
   bm_TGtkTree_view_line = $8;
   bp_TGtkTree_view_line = 3;

{ set this flag to enable tree debugging output  }
{ $define TREE_DEBUG  }
function GTK_TYPE_TREE : GType;
function GTK_TREE(obj: pointer) : PGtkTree;
function GTK_TREE_CLASS(klass: pointer) : PGtkTreeClass;
function GTK_IS_TREE(obj: pointer) : boolean;
function GTK_IS_TREE_CLASS(klass: pointer) : boolean;
function GTK_TREE_GET_CLASS(obj: pointer) : PGtkTreeClass;
function GTK_IS_ROOT_TREE(obj: pointer) : boolean;
function GTK_TREE_ROOT_TREE(obj: pointer) : PGtkTree;
function GTK_TREE_SELECTION_OLD(obj: pointer) : PGList;

function selection_mode(var a : TGtkTree) : guint; overload;
procedure set_selection_mode(var a : TGtkTree; __selection_mode : guint); overload;
function view_mode(var a : TGtkTree) : guint;
procedure set_view_mode(var a : TGtkTree; __view_mode : guint);
function view_line(var a : TGtkTree) : guint;
procedure set_view_line(var a : TGtkTree; __view_line : guint);


function gtk_tree_get_type:TGtkType; cdecl; external gtklib;
function gtk_tree_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_tree_append(tree:PGtkTree; tree_item:PGtkWidget); cdecl; external gtklib;
procedure gtk_tree_prepend(tree:PGtkTree; tree_item:PGtkWidget); cdecl; external gtklib;
procedure gtk_tree_insert(tree:PGtkTree; tree_item:PGtkWidget; position:gint); cdecl; external gtklib;
procedure gtk_tree_remove_items(tree:PGtkTree; items:PGList); cdecl; external gtklib;
procedure gtk_tree_clear_items(tree:PGtkTree; start:gint; theEnd:gint); cdecl; external gtklib;
procedure gtk_tree_select_item(tree:PGtkTree; item:gint); cdecl; external gtklib;
procedure gtk_tree_unselect_item(tree:PGtkTree; item:gint); cdecl; external gtklib;
procedure gtk_tree_select_child(tree:PGtkTree; tree_item:PGtkWidget); cdecl; external gtklib;
procedure gtk_tree_unselect_child(tree:PGtkTree; tree_item:PGtkWidget); cdecl; external gtklib;
function gtk_tree_child_position(tree:PGtkTree; child:PGtkWidget):gint; cdecl; external gtklib;
procedure gtk_tree_set_selection_mode(tree:PGtkTree; mode:TGtkSelectionMode); cdecl; external gtklib;
procedure gtk_tree_set_view_mode(tree:PGtkTree; mode:TGtkTreeViewMode); cdecl; external gtklib;
procedure gtk_tree_set_view_lines(tree:PGtkTree; flag:gboolean); cdecl; external gtklib;
{ deprecated function, use gtk_container_remove instead.
  }
procedure gtk_tree_remove_item(tree:PGtkTree; child:PGtkWidget); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE : GType;
begin
   GTK_TYPE_TREE:=gtk_tree_get_type;
end;

function GTK_TREE(obj: pointer) : PGtkTree;
begin
   GTK_TREE:=PGtkTree(GTK_CHECK_CAST(obj,GTK_TYPE_TREE));
end;

function GTK_TREE_CLASS(klass: pointer) : PGtkTreeClass;
begin
   GTK_TREE_CLASS:=PGtkTreeClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE));
end;

function GTK_IS_TREE(obj: pointer) : boolean;
begin
   GTK_IS_TREE:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE);
end;

function GTK_IS_TREE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE);
end;

function GTK_TREE_GET_CLASS(obj: pointer) : PGtkTreeClass;
begin
   GTK_TREE_GET_CLASS:=PGtkTreeClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE));
end;

function GTK_IS_ROOT_TREE(obj: pointer) : boolean;
begin
   GTK_IS_ROOT_TREE:=(PGtkObject((GTK_TREE(obj))^.root_tree)) = (PGtkObject(obj));
end;

function GTK_TREE_ROOT_TREE(obj: pointer) : PGtkTree;
begin
   GTK_TREE_ROOT_TREE:=GTK_TREE(obj)^.root_tree;
end;

function GTK_TREE_SELECTION_OLD(obj: pointer) : PGList;
begin
   GTK_TREE_SELECTION_OLD:=(GTK_TREE_ROOT_TREE(obj))^.selection;
end;

function selection_mode(var a : TGtkTree) : guint;
begin
   selection_mode:=(a.flag0 and bm_TGtkTree_selection_mode) shr bp_TGtkTree_selection_mode;
end;

procedure set_selection_mode(var a : TGtkTree; __selection_mode : guint);
begin
   a.flag0:=a.flag0 or ((__selection_mode shl bp_TGtkTree_selection_mode) and bm_TGtkTree_selection_mode);
end;

function view_mode(var a : TGtkTree) : guint;
begin
   view_mode:=(a.flag0 and bm_TGtkTree_view_mode) shr bp_TGtkTree_view_mode;
end;

procedure set_view_mode(var a : TGtkTree; __view_mode : guint);
begin
   a.flag0:=a.flag0 or ((__view_mode shl bp_TGtkTree_view_mode) and bm_TGtkTree_view_mode);
end;

function view_line(var a : TGtkTree) : guint;
begin
   view_line:=(a.flag0 and bm_TGtkTree_view_line) shr bp_TGtkTree_view_line;
end;

procedure set_view_line(var a : TGtkTree; __view_line : guint);
begin
   a.flag0:=a.flag0 or ((__view_line shl bp_TGtkTree_view_line) and bm_TGtkTree_view_line);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

