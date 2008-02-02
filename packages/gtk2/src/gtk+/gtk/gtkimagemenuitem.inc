// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< private > }
   PGtkImageMenuItem = ^TGtkImageMenuItem;
   TGtkImageMenuItem = record
        menu_item : TGtkMenuItem;
        image : PGtkWidget;
     end;

   PGtkImageMenuItemClass = ^TGtkImageMenuItemClass;
   TGtkImageMenuItemClass = record
        parent_class : TGtkMenuItemClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_IMAGE_MENU_ITEM : GType;
function GTK_IMAGE_MENU_ITEM(obj: pointer) : PGtkImageMenuItem;
function GTK_IMAGE_MENU_ITEM_CLASS(klass: pointer) : PGtkImageMenuItemClass;
function GTK_IS_IMAGE_MENU_ITEM(obj: pointer) : boolean;
function GTK_IS_IMAGE_MENU_ITEM_CLASS(klass: pointer) : boolean;
function GTK_IMAGE_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkImageMenuItemClass;


function gtk_image_menu_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_image_menu_item_new:PGtkWidget; cdecl; external gtklib;
function gtk_image_menu_item_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_image_menu_item_new_with_mnemonic(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_image_menu_item_new_from_stock(stock_id:Pgchar; accel_group:PGtkAccelGroup):PGtkWidget; cdecl; external gtklib;
procedure gtk_image_menu_item_set_image(image_menu_item:PGtkImageMenuItem; image:PGtkWidget); cdecl; external gtklib;
function gtk_image_menu_item_get_image(image_menu_item:PGtkImageMenuItem):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_IMAGE_MENU_ITEM : GType;
begin
   GTK_TYPE_IMAGE_MENU_ITEM:=gtk_image_menu_item_get_type;
end;

function GTK_IMAGE_MENU_ITEM(obj: pointer) : PGtkImageMenuItem;
begin
   GTK_IMAGE_MENU_ITEM:=PGtkImageMenuItem(GTK_CHECK_CAST(obj,GTK_TYPE_IMAGE_MENU_ITEM));
end;

function GTK_IMAGE_MENU_ITEM_CLASS(klass: pointer) : PGtkImageMenuItemClass;
begin
   GTK_IMAGE_MENU_ITEM_CLASS:=PGtkImageMenuItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_IMAGE_MENU_ITEM));
end;

function GTK_IS_IMAGE_MENU_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_IMAGE_MENU_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_IMAGE_MENU_ITEM);
end;

function GTK_IS_IMAGE_MENU_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_IMAGE_MENU_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_IMAGE_MENU_ITEM);
end;

function GTK_IMAGE_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkImageMenuItemClass;
begin
   GTK_IMAGE_MENU_ITEM_GET_CLASS:=PGtkImageMenuItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_IMAGE_MENU_ITEM));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

