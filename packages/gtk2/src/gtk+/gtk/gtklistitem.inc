// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ifndef GTK_DISABLE_DEPRECATED}
   PGtkListItem = ^TGtkListItem;
   TGtkListItem = record
        item : TGtkItem;
     end;

   PGtkListItemClass = ^TGtkListItemClass;
   TGtkListItemClass = record
        parent_class : TGtkItemClass;
        toggle_focus_row : procedure (list_item:PGtkListItem); cdecl;
        select_all : procedure (list_item:PGtkListItem); cdecl;
        unselect_all : procedure (list_item:PGtkListItem); cdecl;
        undo_selection : procedure (list_item:PGtkListItem); cdecl;
        start_selection : procedure (list_item:PGtkListItem); cdecl;
        end_selection : procedure (list_item:PGtkListItem); cdecl;
        extend_selection : procedure (list_item:PGtkListItem; scroll_type:TGtkScrollType; position:gfloat; auto_start_selection:gboolean); cdecl;
        scroll_horizontal : procedure (list_item:PGtkListItem; scroll_type:TGtkScrollType; position:gfloat); cdecl;
        scroll_vertical : procedure (list_item:PGtkListItem; scroll_type:TGtkScrollType; position:gfloat); cdecl;
        toggle_add_mode : procedure (list_item:PGtkListItem); cdecl;
     end;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{$ifndef GTK_DISABLE_DEPRECATED}
function GTK_TYPE_LIST_ITEM : GType;
function GTK_LIST_ITEM(obj: pointer) : PGtkListItem;
function GTK_LIST_ITEM_CLASS(klass: pointer) : PGtkListItemClass;
function GTK_IS_LIST_ITEM(obj: pointer) : boolean;
function GTK_IS_LIST_ITEM_CLASS(klass: pointer) : boolean;
function GTK_LIST_ITEM_GET_CLASS(obj: pointer) : PGtkListItemClass;

function gtk_list_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_list_item_new:PGtkWidget; cdecl; external gtklib;
function gtk_list_item_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_list_item_select(list_item:PGtkListItem); cdecl; external gtklib;
procedure gtk_list_item_deselect(list_item:PGtkListItem); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_LIST_ITEM : GType;
begin
   GTK_TYPE_LIST_ITEM:=gtk_list_item_get_type;
end;

function GTK_LIST_ITEM(obj: pointer) : PGtkListItem;
begin
   GTK_LIST_ITEM:=PGtkListItem(GTK_CHECK_CAST(obj,GTK_TYPE_LIST_ITEM));
end;

function GTK_LIST_ITEM_CLASS(klass: pointer) : PGtkListItemClass;
begin
   GTK_LIST_ITEM_CLASS:=PGtkListItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_LIST_ITEM));
end;

function GTK_IS_LIST_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_LIST_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_LIST_ITEM);
end;

function GTK_IS_LIST_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_LIST_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_LIST_ITEM);
end;

function GTK_LIST_ITEM_GET_CLASS(obj: pointer) : PGtkListItemClass;
begin
   GTK_LIST_ITEM_GET_CLASS:=PGtkListItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_LIST_ITEM));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

