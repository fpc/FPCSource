{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkListItem = ^TGtkListItem;
     TGtkListItem = record
          item : TGtkItem;
       end;

     PGtkListItemClass = ^TGtkListItemClass;
     TGtkListItemClass = record
          parent_class : TGtkItemClass;
          toggle_focus_row : procedure (list_item:PGtkListItem);cdecl;
          select_all : procedure (list_item:PGtkListItem);cdecl;
          unselect_all : procedure (list_item:PGtkListItem);cdecl;
          undo_selection : procedure (list_item:PGtkListItem);cdecl;
          start_selection : procedure (list_item:PGtkListItem);cdecl;
          end_selection : procedure (list_item:PGtkListItem);cdecl;
          extend_selection : procedure (list_item:PGtkListItem; scroll_type:TGtkScrollType; position:gfloat; auto_start_selection:gboolean);cdecl;
          scroll_horizontal : procedure (list_item:PGtkListItem; scroll_type:TGtkScrollType; position:gfloat);cdecl;
          scroll_vertical : procedure (list_item:PGtkListItem; scroll_type:TGtkScrollType; position:gfloat);cdecl;
          toggle_add_mode : procedure (list_item:PGtkListItem);cdecl;
       end;

Type
  GTK_LIST_ITEM=PGtkListItem;
  GTK_LIST_ITEM_CLASS=PGtkListItemClass;

function  GTK_LIST_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_list_item_get_type';
function  GTK_IS_LIST_ITEM(obj:pointer):boolean;
function  GTK_IS_LIST_ITEM_CLASS(klass:pointer):boolean;

function  gtk_list_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_list_item_get_type';
function  gtk_list_item_new:PGtkWidget;cdecl;external gtkdll name 'gtk_list_item_new';
function  gtk_list_item_new_with_label(thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_list_item_new_with_label';
procedure gtk_list_item_select(list_item:PGtkListItem);cdecl;external gtkdll name 'gtk_list_item_select';
procedure gtk_list_item_deselect(list_item:PGtkListItem);cdecl;external gtkdll name 'gtk_list_item_deselect';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_LIST_ITEM(obj:pointer):boolean;
begin
  GTK_IS_LIST_ITEM:=(obj<>nil) and GTK_IS_LIST_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_LIST_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_LIST_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_LIST_ITEM_TYPE);
end;

{$endif read_implementation}


