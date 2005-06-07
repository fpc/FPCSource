{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}


  type
     PGtkRadioMenuItem = ^TGtkRadioMenuItem;
     TGtkRadioMenuItem = record
          check_menu_item : TGtkCheckMenuItem;
          group : PGSList;
       end;

     PGtkRadioMenuItemClass = ^TGtkRadioMenuItemClass;
     TGtkRadioMenuItemClass = record
          parent_class : TGtkCheckMenuItemClass;
       end;

Type
  GTK_RADIO_MENU_ITEM=PGtkRadioMenuItem;
  GTK_RADIO_MENU_ITEM_CLASS=PGtkRadioMenuItemClass;

function  GTK_RADIO_MENU_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_radio_menu_item_get_type';
function  GTK_IS_RADIO_MENU_ITEM(obj:pointer):boolean;
function  GTK_IS_RADIO_MENU_ITEM_CLASS(klass:pointer):boolean;

function  gtk_radio_menu_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_radio_menu_item_get_type';
function  gtk_radio_menu_item_new(group:PGSList):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_menu_item_new';
function  gtk_radio_menu_item_new_with_label(group:PGSList; thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_menu_item_new_with_label';
function  gtk_radio_menu_item_group(radio_menu_item:PGtkRadioMenuItem):PGSList;cdecl;external gtkdll name 'gtk_radio_menu_item_group';
procedure gtk_radio_menu_item_set_group(radio_menu_item:PGtkRadioMenuItem; group:PGSList);cdecl;external gtkdll name 'gtk_radio_menu_item_set_group';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_RADIO_MENU_ITEM(obj:pointer):boolean;
begin
  GTK_IS_RADIO_MENU_ITEM:=(obj<>nil) and GTK_IS_RADIO_MENU_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_RADIO_MENU_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_RADIO_MENU_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_RADIO_MENU_ITEM_TYPE);
end;

{$endif read_implementation}


