{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
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

  const
     bm_TGtkTreeItem_expanded = $1;
     bp_TGtkTreeItem_expanded = 0;
function  expanded(var a : TGtkTreeItem) : guint;
procedure set_expanded(var a : TGtkTreeItem; __expanded : guint);

  type
     PGtkTreeItemClass = ^TGtkTreeItemClass;
     TGtkTreeItemClass = record
          parent_class : TGtkItemClass;
          expand : procedure (tree_item:PGtkTreeItem); cdecl;
          collapse : procedure (tree_item:PGtkTreeItem); cdecl;
       end;

Type
  GTK_TREE_ITEM=PGtkTreeItem;
  GTK_TREE_ITEM_CLASS=PGtkTreeItemClass;

function  GTK_TREE_ITEM_SUBTREE(obj : PGtktreeitem) : PGTkWidget;

function  GTK_TREE_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_tree_item_get_type';
function  GTK_IS_TREE_ITEM(obj:pointer):boolean;
function  GTK_IS_TREE_ITEM_CLASS(klass:pointer):boolean;

function  gtk_tree_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_tree_item_get_type';
function  gtk_tree_item_new:PGtkWidget;cdecl;external gtkdll name 'gtk_tree_item_new';
function  gtk_tree_item_new_with_label(thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_tree_item_new_with_label';
procedure gtk_tree_item_set_subtree(tree_item:PGtkTreeItem; subtree:PGtkWidget);cdecl;external gtkdll name 'gtk_tree_item_set_subtree';
procedure gtk_tree_item_remove_subtree(tree_item:PGtkTreeItem);cdecl;external gtkdll name 'gtk_tree_item_remove_subtree';
procedure gtk_tree_item_select(tree_item:PGtkTreeItem);cdecl;external gtkdll name 'gtk_tree_item_select';
procedure gtk_tree_item_deselect(tree_item:PGtkTreeItem);cdecl;external gtkdll name 'gtk_tree_item_deselect';
procedure gtk_tree_item_expand(tree_item:PGtkTreeItem);cdecl;external gtkdll name 'gtk_tree_item_expand';
procedure gtk_tree_item_collapse(tree_item:PGtkTreeItem);cdecl;external gtkdll name 'gtk_tree_item_collapse';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_TREE_ITEM_SUBTREE(obj : PGtktreeitem) : PGtkwidget;
    begin
       GTK_TREE_ITEM_SUBTREE:=obj^.subtree;
    end;

function  expanded(var a : TGtkTreeItem) : guint;
    begin
       expanded:=(a.flag0 and bm_TGtkTreeItem_expanded) shr bp_TGtkTreeItem_expanded;
    end;

procedure set_expanded(var a : TGtkTreeItem; __expanded : guint);
    begin
       a.flag0:=a.flag0 or ((__expanded shl bp_TGtkTreeItem_expanded) and bm_TGtkTreeItem_expanded);
    end;

function  GTK_IS_TREE_ITEM(obj:pointer):boolean;
begin
  GTK_IS_TREE_ITEM:=(obj<>nil) and GTK_IS_TREE_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_TREE_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_TREE_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TREE_ITEM_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:07  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.10  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/07/23 16:13:20  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:39:39  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:20:39  peter
    * cdecl fixes

  Revision 1.6  1999/05/10 09:04:11  peter
    * gtk 1.2 port working

  Revision 1.5  1998/11/09 10:10:41  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/21 22:25:22  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:23:23  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

