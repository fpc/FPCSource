// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkRadioMenuItem = ^TGtkRadioMenuItem;
   TGtkRadioMenuItem = record
        check_menu_item : TGtkCheckMenuItem;
        group : PGSList;
     end;

{ Padding for future expansion  }
   PGtkRadioMenuItemClass = ^TGtkRadioMenuItemClass;
   TGtkRadioMenuItemClass = record
        parent_class : TGtkCheckMenuItemClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_RADIO_MENU_ITEM : GType;
function GTK_RADIO_MENU_ITEM(obj: pointer) : PGtkRadioMenuItem;
function GTK_RADIO_MENU_ITEM_CLASS(klass: pointer) : PGtkRadioMenuItemClass;
function GTK_IS_RADIO_MENU_ITEM(obj: pointer) : boolean;
function GTK_IS_RADIO_MENU_ITEM_CLASS(klass: pointer) : boolean;
function GTK_RADIO_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkRadioMenuItemClass;


function gtk_radio_menu_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_radio_menu_item_new(group:PGSList):PGtkWidget; cdecl; external gtklib;
function gtk_radio_menu_item_new_with_label(group:PGSList; _label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_radio_menu_item_new_with_mnemonic(group:PGSList; _label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_radio_menu_item_get_group(radio_menu_item:PGtkRadioMenuItem):PGSList; cdecl; external gtklib;
procedure gtk_radio_menu_item_set_group(radio_menu_item:PGtkRadioMenuItem; group:PGSList); cdecl; external gtklib;

{$ifndef G_DISABLE_DEPRECATED}
function gtk_radio_menu_item_group(radio_menu_item:PGtkRadioMenuItem):PGSList;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_RADIO_MENU_ITEM : GType;
begin
   GTK_TYPE_RADIO_MENU_ITEM:=gtk_radio_menu_item_get_type;
end;

function GTK_RADIO_MENU_ITEM(obj: pointer) : PGtkRadioMenuItem;
begin
   GTK_RADIO_MENU_ITEM:=PGtkRadioMenuItem(GTK_CHECK_CAST(obj,GTK_TYPE_RADIO_MENU_ITEM));
end;

function GTK_RADIO_MENU_ITEM_CLASS(klass: pointer) : PGtkRadioMenuItemClass;
begin
   GTK_RADIO_MENU_ITEM_CLASS:=PGtkRadioMenuItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_RADIO_MENU_ITEM));
end;

function GTK_IS_RADIO_MENU_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_RADIO_MENU_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_RADIO_MENU_ITEM);
end;

function GTK_IS_RADIO_MENU_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_RADIO_MENU_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_RADIO_MENU_ITEM);
end;

function GTK_RADIO_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkRadioMenuItemClass;
begin
   GTK_RADIO_MENU_ITEM_GET_CLASS:=PGtkRadioMenuItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_RADIO_MENU_ITEM));
end;

{$ifndef G_DISABLE_DEPRECATED}
function gtk_radio_menu_item_group(radio_menu_item:PGtkRadioMenuItem):PGSList;
begin
   gtk_radio_menu_item_group:=gtk_radio_menu_item_get_group(radio_menu_item);
end;
{$endif}

{$ENDIF read_implementation}
// included by gtk2.pas

