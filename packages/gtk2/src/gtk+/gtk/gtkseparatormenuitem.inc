// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkSeparatorMenuItem = ^TGtkSeparatorMenuItem;
   TGtkSeparatorMenuItem = record
        menu_item : TGtkMenuItem;
     end;

   PGtkSeparatorMenuItemClass = ^TGtkSeparatorMenuItemClass;
   TGtkSeparatorMenuItemClass = record
        parent_class : TGtkMenuItemClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_SEPARATOR_MENU_ITEM : GType;
function GTK_SEPARATOR_MENU_ITEM(obj: pointer) : PGtkSeparatorMenuItem;
function GTK_SEPARATOR_MENU_ITEM_CLASS(klass: pointer) : PGtkSeparatorMenuItemClass;
function GTK_IS_SEPARATOR_MENU_ITEM(obj: pointer) : boolean;
function GTK_IS_SEPARATOR_MENU_ITEM_CLASS(klass: pointer) : boolean;
function GTK_SEPARATOR_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkSeparatorMenuItemClass;


function gtk_separator_menu_item_get_type:GType; cdecl; external gtklib;
function gtk_separator_menu_item_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SEPARATOR_MENU_ITEM : GType;
begin
   GTK_TYPE_SEPARATOR_MENU_ITEM:=gtk_separator_menu_item_get_type;
end;

function GTK_SEPARATOR_MENU_ITEM(obj: pointer) : PGtkSeparatorMenuItem;
begin
   GTK_SEPARATOR_MENU_ITEM:=PGtkSeparatorMenuItem(GTK_CHECK_CAST(obj,GTK_TYPE_SEPARATOR_MENU_ITEM));
end;

function GTK_SEPARATOR_MENU_ITEM_CLASS(klass: pointer) : PGtkSeparatorMenuItemClass;
begin
   GTK_SEPARATOR_MENU_ITEM_CLASS:=PGtkSeparatorMenuItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SEPARATOR_MENU_ITEM));
end;

function GTK_IS_SEPARATOR_MENU_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_SEPARATOR_MENU_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_SEPARATOR_MENU_ITEM);
end;

function GTK_IS_SEPARATOR_MENU_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SEPARATOR_MENU_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SEPARATOR_MENU_ITEM);
end;

function GTK_SEPARATOR_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkSeparatorMenuItemClass;
begin
   GTK_SEPARATOR_MENU_ITEM_GET_CLASS:=PGtkSeparatorMenuItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SEPARATOR_MENU_ITEM));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

