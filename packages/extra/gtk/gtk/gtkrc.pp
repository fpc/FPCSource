{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

{******************************
  types inserted in gtkwidget
******************************}

procedure gtk_rc_init;cdecl;external gtkdll name 'gtk_rc_init';
procedure gtk_rc_add_default_file(filename:Pgchar);cdecl;external gtkdll name 'gtk_rc_add_default_file';
procedure gtk_rc_set_default_files(filenames:PPgchar);cdecl;external gtkdll name 'gtk_rc_set_default_files';
function  gtk_rc_get_default_files:PPgchar;cdecl;external gtkdll name 'gtk_rc_get_default_files';
procedure gtk_rc_parse(filename:Pgchar);cdecl;external gtkdll name 'gtk_rc_parse';
procedure gtk_rc_parse_string(rc_string:Pgchar);cdecl;external gtkdll name 'gtk_rc_parse_string';
function  gtk_rc_get_style(widget:PGtkWidget):PGtkStyle;cdecl;external gtkdll name 'gtk_rc_get_style';
procedure gtk_rc_add_widget_name_style(rc_style:PGtkRcStyle; pattern:Pgchar);cdecl;external gtkdll name 'gtk_rc_add_widget_name_style';
procedure gtk_rc_add_widget_class_style(rc_style:PGtkRcStyle; pattern:Pgchar);cdecl;external gtkdll name 'gtk_rc_add_widget_class_style';
procedure gtk_rc_add_class_style(rc_style:PGtkRcStyle; pattern:Pgchar);cdecl;external gtkdll name 'gtk_rc_add_class_style';
function  gtk_rc_style_new:PGtkRcStyle;cdecl;external gtkdll name 'gtk_rc_style_new';
procedure gtk_rc_style_ref(rc_style:PGtkRcStyle);cdecl;external gtkdll name 'gtk_rc_style_ref';
procedure gtk_rc_style_unref(rc_style:PGtkRcStyle);cdecl;external gtkdll name 'gtk_rc_style_unref';

  type
     TGtkImageLoader = function (window:PGdkWindow; colormap:PGdkColormap; mask:PPGdkBitmap; transparent_color:PGdkColor; filename:Pgchar):PGdkPixmap;cdecl;

procedure gtk_rc_set_image_loader(loader:TGtkImageLoader);cdecl;external gtkdll name 'gtk_rc_set_image_loader';
function  gtk_rc_load_image(colormap:PGdkColormap; transparent_color:PGdkColor; filename:Pgchar):PGdkPixmap;cdecl;external gtkdll name 'gtk_rc_load_image';
function  gtk_rc_find_pixmap_in_path(scanner:PGScanner; pixmap_file:Pgchar):Pgchar;cdecl;external gtkdll name 'gtk_rc_find_pixmap_in_path';
function  gtk_rc_find_module_in_path(module_file:Pgchar):Pgchar;cdecl;external gtkdll name 'gtk_rc_find_module_in_path';
function  gtk_rc_get_theme_dir:Pgchar;cdecl;external gtkdll name 'gtk_rc_get_theme_dir';
function  gtk_rc_get_module_dir:Pgchar;cdecl;external gtkdll name 'gtk_rc_get_module_dir';

  type
     TGtkRcTokenType = (GTK_RC_TOKEN_INVALID := G_TOKEN_LAST,GTK_RC_TOKEN_INCLUDE,
       GTK_RC_TOKEN_NORMAL,GTK_RC_TOKEN_ACTIVE,
       GTK_RC_TOKEN_PRELIGHT,GTK_RC_TOKEN_SELECTED,
       GTK_RC_TOKEN_INSENSITIVE,GTK_RC_TOKEN_FG,
       GTK_RC_TOKEN_BG,GTK_RC_TOKEN_BASE,GTK_RC_TOKEN_TEXT,
       GTK_RC_TOKEN_FONT,GTK_RC_TOKEN_FONTSET,
       GTK_RC_TOKEN_BG_PIXMAP,GTK_RC_TOKEN_PIXMAP_PATH,
       GTK_RC_TOKEN_STYLE,GTK_RC_TOKEN_BINDING,
       GTK_RC_TOKEN_BIND,GTK_RC_TOKEN_WIDGET,
       GTK_RC_TOKEN_WIDGET_CLASS,GTK_RC_TOKEN_CLASS,
       GTK_RC_TOKEN_LOWEST,GTK_RC_TOKEN_GTK,
       GTK_RC_TOKEN_APPLICATION,GTK_RC_TOKEN_RC,
       GTK_RC_TOKEN_HIGHEST,GTK_RC_TOKEN_ENGINE,
       GTK_RC_TOKEN_MODULE_PATH,GTK_RC_TOKEN_LAST
       );

function  gtk_rc_parse_color(scanner:PGScanner; color:PGdkColor):guint;cdecl;external gtkdll name 'gtk_rc_parse_color';
function  gtk_rc_parse_state(scanner:PGScanner; state:PGtkStateType):guint;cdecl;external gtkdll name 'gtk_rc_parse_state';
function  gtk_rc_parse_priority(scanner:PGScanner; priority:PGtkPathPriorityType):guint;cdecl;external gtkdll name 'gtk_rc_parse_priority';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


