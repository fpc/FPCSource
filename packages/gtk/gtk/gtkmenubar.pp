{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkMenuBar = ^TGtkMenuBar;
     TGtkMenuBar = record
          menu_shell : TGtkMenuShell;
          shadow_type : TGtkShadowType;
       end;

     PGtkMenuBarClass = ^TGtkMenuBarClass;
     TGtkMenuBarClass = record
          parent_class : TGtkMenuShellClass;
       end;

Type
  GTK_MENU_BAR=PGtkMenuBar;
  GTK_MENU_BAR_CLASS=PGtkMenuBarClass;

function  GTK_MENU_BAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_menu_bar_get_type';
function  GTK_IS_MENU_BAR(obj:pointer):boolean;
function  GTK_IS_MENU_BAR_CLASS(klass:pointer):boolean;

function  gtk_menu_bar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_menu_bar_get_type';
function  gtk_menu_bar_new:PGtkWidget;cdecl;external gtkdll name 'gtk_menu_bar_new';
procedure gtk_menu_bar_append(menu_bar:PGtkMenuBar; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_bar_append';
procedure gtk_menu_bar_prepend(menu_bar:PGtkMenuBar; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_bar_prepend';
procedure gtk_menu_bar_insert(menu_bar:PGtkMenuBar; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_menu_bar_insert';
procedure gtk_menu_bar_set_shadow_type(menu_bar:PGtkMenuBar; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_menu_bar_set_shadow_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_MENU_BAR(obj:pointer):boolean;
begin
  GTK_IS_MENU_BAR:=(obj<>nil) and GTK_IS_MENU_BAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_MENU_BAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_MENU_BAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_MENU_BAR_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.11  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.10  1999/10/05 09:28:27  peter
    * patches from Frank Loemker

  Revision 1.9  1999/07/23 16:12:42  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:38:58  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:19:50  peter
    * cdecl fixes

  Revision 1.6  1999/05/10 09:03:25  peter
    * gtk 1.2 port working

  Revision 1.5  1999/05/07 17:40:29  peter
    * more updates

  Revision 1.4  1998/11/09 10:10:11  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:49  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

