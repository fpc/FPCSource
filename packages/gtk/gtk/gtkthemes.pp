{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

{********************************
   Types inserted in gtkwidget
********************************}

{$ifndef gtkwin}
function  gtk_theme_engine_get(name:Pgchar):PGtkThemeEngine;cdecl;external gtkdll name 'gtk_theme_engine_get';
procedure gtk_theme_engine_ref(engine:PGtkThemeEngine);cdecl;external gtkdll name 'gtk_theme_engine_ref';
procedure gtk_theme_engine_unref(engine:PGtkThemeEngine);cdecl;external gtkdll name 'gtk_theme_engine_unref';
procedure gtk_themes_init(argc:plongint; argv:pppchar);cdecl;external gtkdll name 'gtk_themes_init';
procedure gtk_themes_exit(error_code:gint);cdecl;external gtkdll name 'gtk_themes_exit';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:06  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.4  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.3  1999/05/11 00:39:33  peter
    * win32 fixes

  Revision 1.2  1999/05/10 15:20:32  peter
    * cdecl fixes

  Revision 1.1  1999/05/10 09:14:00  peter
    + new gtk 1.2 files

}

