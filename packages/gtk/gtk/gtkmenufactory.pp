{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type

     TGtkMenuCallback = procedure (widget:PGtkWidget; user_data:gpointer); cdecl;

     PGtkMenuEntry = ^TGtkMenuEntry;
     TGtkMenuEntry = record
          path : Pgchar;
          accelerator : Pgchar;
          callback : TGtkMenuCallback;
          callback_data : gpointer;
          widget : PGtkWidget;
       end;

     PGtkMenuPath = ^TGtkMenuPath;
     TGtkMenuPath = record
          path : Pgchar;
          widget : PGtkWidget;
       end;

     PGtkMenuFactory = ^TGtkMenuFactory;
     TGtkMenuFactory = record
          path : Pgchar;
          thetype : TGtkMenuFactoryType;
          accel_group :  PGtkAccelGroup;
          widget : PGtkWidget;
          subfactories : PGList;
       end;

function  gtk_menu_factory_new(thetype:TGtkMenuFactoryType):PGtkMenuFactory;cdecl;external gtkdll name 'gtk_menu_factory_new';
procedure gtk_menu_factory_destroy(factory:PGtkMenuFactory);cdecl;external gtkdll name 'gtk_menu_factory_destroy';
procedure gtk_menu_factory_add_entries(factory:PGtkMenuFactory; entries:PGtkMenuEntry; nentries:longint);cdecl;external gtkdll name 'gtk_menu_factory_add_entries';
procedure gtk_menu_factory_add_subfactory(factory:PGtkMenuFactory; subfactory:PGtkMenuFactory; path:pchar);cdecl;external gtkdll name 'gtk_menu_factory_add_subfactory';
procedure gtk_menu_factory_remove_paths(factory:PGtkMenuFactory; paths:PPgchar; npaths:longint);cdecl;external gtkdll name 'gtk_menu_factory_remove_paths';
procedure gtk_menu_factory_remove_entries(factory:PGtkMenuFactory; entries:PGtkMenuEntry; nentries:longint);cdecl;external gtkdll name 'gtk_menu_factory_remove_entries';
procedure gtk_menu_factory_remove_subfactory(factory:PGtkMenuFactory; subfactory:PGtkMenuFactory; path:pchar);cdecl;external gtkdll name 'gtk_menu_factory_remove_subfactory';
function  gtk_menu_factory_find(factory:PGtkMenuFactory; path:Pgchar):PGtkMenuPath;cdecl;external gtkdll name 'gtk_menu_factory_find';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.8  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.7  1999/07/23 16:12:43  peter
    * use packrecords C

  Revision 1.6  1999/05/11 00:38:59  peter
    * win32 fixes

  Revision 1.5  1999/05/10 15:19:51  peter
    * cdecl fixes

  Revision 1.4  1999/05/10 09:03:26  peter
    * gtk 1.2 port working

  Revision 1.3  1998/10/21 20:22:50  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

