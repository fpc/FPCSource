{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkOptionMenu = ^TGtkOptionMenu;
     TGtkOptionMenu = record
          button : TGtkButton;
          menu : PGtkWidget;
          menu_item : PGtkWidget;
          width : guint16;
          height : guint16;
       end;

     PGtkOptionMenuClass = ^TGtkOptionMenuClass;
     TGtkOptionMenuClass = record
          parent_class : TGtkButtonClass;
       end;

Type
  GTK_OPTION_MENU=PGtkOptionMenu;
  GTK_OPTION_MENU_CLASS=PGtkOptionMenuClass;

function  GTK_OPTION_MENU_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_option_menu_get_type';
function  GTK_IS_OPTION_MENU(obj:pointer):boolean;
function  GTK_IS_OPTION_MENU_CLASS(klass:pointer):boolean;

function  gtk_option_menu_get_type:TGtkType;cdecl;external gtkdll name 'gtk_option_menu_get_type';
function  gtk_option_menu_new:PGtkWidget;cdecl;external gtkdll name 'gtk_option_menu_new';
function  gtk_option_menu_get_menu(option_menu:PGtkOptionMenu):PGtkWidget;cdecl;external gtkdll name 'gtk_option_menu_get_menu';
procedure gtk_option_menu_set_menu(option_menu:PGtkOptionMenu; menu:PGtkWidget);cdecl;external gtkdll name 'gtk_option_menu_set_menu';
procedure gtk_option_menu_remove_menu(option_menu:PGtkOptionMenu);cdecl;external gtkdll name 'gtk_option_menu_remove_menu';
procedure gtk_option_menu_set_history(option_menu:PGtkOptionMenu; index:guint);cdecl;external gtkdll name 'gtk_option_menu_set_history';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_OPTION_MENU(obj:pointer):boolean;
begin
  GTK_IS_OPTION_MENU:=(obj<>nil) and GTK_IS_OPTION_MENU_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_OPTION_MENU_CLASS(klass:pointer):boolean;
begin
  GTK_IS_OPTION_MENU_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_OPTION_MENU_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:49  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:06  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:59  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:35  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:18  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:56  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

