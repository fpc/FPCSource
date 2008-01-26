// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ pixmap node for this items color depth  }
   PGtkTreeItem = ^TGtkTreeItem;
   TGtkTreeItem = record
        item : TGtkItem;
        subtree : PGtkWidget;
        pixmaps_box : PGtkWidget;
        plus_pix_widget : PGtkWidget;
        minus_pix_widget : PGtkWidget;
        pixmaps : PGList;
        flag0 : word;
     end;


   PGtkTreeItemClass = ^TGtkTreeItemClass;
   TGtkTreeItemClass = record
        parent_class : TGtkItemClass;
        expand : procedure (tree_item:PGtkTreeItem); cdecl;
        collapse : procedure (tree_item:PGtkTreeItem); cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTreeItem_expanded = $1;
   bp_TGtkTreeItem_expanded = 0;
function GTK_TYPE_TREE_ITEM : GType;
function GTK_TREE_ITEM(obj: pointer) : PGtkTreeItem;
function GTK_TREE_ITEM_CLASS(klass: pointer) : PGtkTreeItemClass;
function GTK_IS_TREE_ITEM(obj: pointer) : boolean;
function GTK_IS_TREE_ITEM_CLASS(klass: pointer) : boolean;
function GTK_TREE_ITEM_GET_CLASS(obj: pointer) : PGtkTreeItemClass;
function GTK_TREE_ITEM_SUBTREE(obj: pointer) : PGtkWidget;


function expanded(var a : TGtkTreeItem) : guint; overload;
procedure set_expanded(var a : TGtkTreeItem; __expanded : guint); overload;


function gtk_tree_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_tree_item_new:PGtkWidget; cdecl; external gtklib;
function gtk_tree_item_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_tree_item_set_subtree(tree_item:PGtkTreeItem; subtree:PGtkWidget); cdecl; external gtklib;
procedure gtk_tree_item_remove_subtree(tree_item:PGtkTreeItem); cdecl; external gtklib;
procedure gtk_tree_item_select(tree_item:PGtkTreeItem); cdecl; external gtklib;
procedure gtk_tree_item_deselect(tree_item:PGtkTreeItem); cdecl; external gtklib;
procedure gtk_tree_item_expand(tree_item:PGtkTreeItem); cdecl; external gtklib;
procedure gtk_tree_item_collapse(tree_item:PGtkTreeItem); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_ITEM : GType;
begin
   GTK_TYPE_TREE_ITEM:=gtk_tree_item_get_type;
end;

function GTK_TREE_ITEM(obj: pointer) : PGtkTreeItem;
begin
   GTK_TREE_ITEM:=PGtkTreeItem(GTK_CHECK_CAST(obj,GTK_TYPE_TREE_ITEM));
end;

function GTK_TREE_ITEM_CLASS(klass: pointer) : PGtkTreeItemClass;
begin
   GTK_TREE_ITEM_CLASS:=PGtkTreeItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE_ITEM));
end;

function GTK_IS_TREE_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_TREE_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE_ITEM);
end;

function GTK_IS_TREE_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE_ITEM);
end;

function GTK_TREE_ITEM_GET_CLASS(obj: pointer) : PGtkTreeItemClass;
begin
   GTK_TREE_ITEM_GET_CLASS:=PGtkTreeItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE_ITEM));
end;

function GTK_TREE_ITEM_SUBTREE(obj: pointer) : PGtkWidget;
begin
   GTK_TREE_ITEM_SUBTREE:=(GTK_TREE_ITEM(obj))^.subtree;
end;

function expanded(var a : TGtkTreeItem) : guint;
begin
   expanded:=(a.flag0 and bm_TGtkTreeItem_expanded) shr bp_TGtkTreeItem_expanded;
end;

procedure set_expanded(var a : TGtkTreeItem; __expanded : guint);
begin
   a.flag0:=a.flag0 or ((__expanded shl bp_TGtkTreeItem_expanded) and bm_TGtkTreeItem_expanded);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

