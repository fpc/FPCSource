{
   $Id$
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


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:59  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:15  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:12  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:43  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:24  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:03  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

