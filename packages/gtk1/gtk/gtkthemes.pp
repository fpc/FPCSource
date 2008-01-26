{
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
{$ifndef gtkdarwin}
procedure gtk_themes_init(argc:plongint; argv:pppchar);cdecl;external gtkdll name 'gtk_themes_init';
procedure gtk_themes_exit(error_code:gint);cdecl;external gtkdll name 'gtk_themes_exit';
{$endif not gtkdarwin}
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


