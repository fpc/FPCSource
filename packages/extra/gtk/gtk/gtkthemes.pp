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
  Revision 1.1  2002-01-29 17:55:14  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:24  michael
  + removed logs
 
}
