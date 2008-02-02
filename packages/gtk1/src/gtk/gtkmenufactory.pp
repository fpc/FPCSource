{
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


