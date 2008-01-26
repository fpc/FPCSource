{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkTreeViewMode = longint;
  const
     GTK_TREE_VIEW_LINE = 0;
     GTK_TREE_VIEW_ITEM = 1;

  type
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
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkTree_selection_mode = $3;
     bp_TGtkTree_selection_mode = 0;
     bm_TGtkTree_view_mode = $4;
     bp_TGtkTree_view_mode = 2;
     bm_TGtkTree_view_line = $8;
     bp_TGtkTree_view_line = 3;
function  selection_mode(var a : TGtkTree) : guint;
procedure set_selection_mode(var a : TGtkTree; __selection_mode : guint);
function  view_mode(var a : TGtkTree) : guint;
procedure set_view_mode(var a : TGtkTree; __view_mode : guint);
function  view_line(var a : TGtkTree) : guint;
procedure set_view_line(var a : TGtkTree; __view_line : guint);

  type
     PGtkTreeClass = ^TGtkTreeClass;
     TGtkTreeClass = record
          parent_class : TGtkContainerClass;
          selection_changed : procedure (tree:PGtkTree); cdecl;
          select_child : procedure (tree:PGtkTree; child:PGtkWidget); cdecl;
          unselect_child : procedure (tree:PGtkTree; child:PGtkWidget); cdecl;
       end;

Type
  GTK_TREE = PGtkTree;
  GTK_TREE_CLASS = PGtkTreeClass;

function  GTK_IS_ROOT_TREE(obj : PGtkTree) : gboolean;
function  GTK_TREE_ROOT_TREE(obj : PGtkTree) : PGtkTree;
function  GTK_TREE_SELECTION(obj : PGtkTree) : PGList;

function  gtk_tree_get_type:TGtkType;cdecl;external gtkdll name 'gtk_tree_get_type';
function  gtk_tree_new:PGtkWidget;cdecl;external gtkdll name 'gtk_tree_new';
procedure gtk_tree_append(tree:PGtkTree; tree_item:PGtkWidget);cdecl;external gtkdll name 'gtk_tree_append';
procedure gtk_tree_prepend(tree:PGtkTree; tree_item:PGtkWidget);cdecl;external gtkdll name 'gtk_tree_prepend';
procedure gtk_tree_insert(tree:PGtkTree; tree_item:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_tree_insert';
procedure gtk_tree_remove_items(tree:PGtkTree; items:PGList);cdecl;external gtkdll name 'gtk_tree_remove_items';
procedure gtk_tree_clear_items(tree:PGtkTree; start:gint; theend:gint);cdecl;external gtkdll name 'gtk_tree_clear_items';
procedure gtk_tree_select_item(tree:PGtkTree; item:gint);cdecl;external gtkdll name 'gtk_tree_select_item';
procedure gtk_tree_unselect_item(tree:PGtkTree; item:gint);cdecl;external gtkdll name 'gtk_tree_unselect_item';
procedure gtk_tree_select_child(tree:PGtkTree; tree_item:PGtkWidget);cdecl;external gtkdll name 'gtk_tree_select_child';
procedure gtk_tree_unselect_child(tree:PGtkTree; tree_item:PGtkWidget);cdecl;external gtkdll name 'gtk_tree_unselect_child';
function  gtk_tree_child_position(tree:PGtkTree; child:PGtkWidget):gint;cdecl;external gtkdll name 'gtk_tree_child_position';
procedure gtk_tree_set_selection_mode(tree:PGtkTree; mode:TGtkSelectionMode);cdecl;external gtkdll name 'gtk_tree_set_selection_mode';
procedure gtk_tree_set_view_mode(tree:PGtkTree; mode:TGtkTreeViewMode);cdecl;external gtkdll name 'gtk_tree_set_view_mode';
procedure gtk_tree_set_view_lines(tree:PGtkTree; flag:guint);cdecl;external gtkdll name 'gtk_tree_set_view_lines';
procedure gtk_tree_remove_item(tree:PGtkTree; child:PGtkWidget);cdecl;external gtkdll name 'gtk_tree_remove_item';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  selection_mode(var a : TGtkTree) : guint;
    begin
       selection_mode:=(a.flag0 and bm_TGtkTree_selection_mode) shr bp_TGtkTree_selection_mode;
    end;

procedure set_selection_mode(var a : TGtkTree; __selection_mode : guint);
    begin
       a.flag0:=a.flag0 or ((__selection_mode shl bp_TGtkTree_selection_mode) and bm_TGtkTree_selection_mode);
    end;

function  view_mode(var a : TGtkTree) : guint;
    begin
       view_mode:=(a.flag0 and bm_TGtkTree_view_mode) shr bp_TGtkTree_view_mode;
    end;

procedure set_view_mode(var a : TGtkTree; __view_mode : guint);
    begin
       a.flag0:=a.flag0 or ((__view_mode shl bp_TGtkTree_view_mode) and bm_TGtkTree_view_mode);
    end;

function  view_line(var a : TGtkTree) : guint;
    begin
       view_line:=(a.flag0 and bm_TGtkTree_view_line) shr bp_TGtkTree_view_line;
    end;

procedure set_view_line(var a : TGtkTree; __view_line : guint);
    begin
       a.flag0:=a.flag0 or ((__view_line shl bp_TGtkTree_view_line) and bm_TGtkTree_view_line);
    end;

function  GTK_IS_ROOT_TREE(obj : PGtkTree) : gboolean;
begin
  GTK_IS_ROOT_TREE:=(obj^.root_tree=obj);
end;

function  GTK_TREE_ROOT_TREE(obj : PGtkTree) : PGtkTree;
begin
  if (GTK_TREE(obj))^.root_tree<>nil then
    GTK_TREE_ROOT_TREE:=(GTK_TREE(obj))^.root_tree
  else
    GTK_TREE_ROOT_TREE:=GTK_TREE(obj);
end;

function  GTK_TREE_SELECTION(obj : PGtkTree) : PGList;
begin
  GTK_TREE_SELECTION:=(GTK_TREE_ROOT_TREE(obj))^.selection;
end;

{$endif read_implementation}


