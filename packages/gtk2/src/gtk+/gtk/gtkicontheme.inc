// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkIconThemePrivate = pointer;
   PGtkIconInfo = pointer;
   PPGtkIconInfo = ^PGtkIconInfo;

   PGtkIconTheme = ^TGtkIconTheme;
   TGtkIconTheme = record
        parent_instance : TGObject;
        priv : PGtkIconThemePrivate;
     end;

   PGtkIconThemeClass = ^TGtkIconThemeClass;
   TGtkIconThemeClass = record
        parent_class : TGObjectClass;
        changed : procedure (icon_theme:PGtkIconTheme);cdecl;
     end;

{
   GtkIconLookupFlags:
   @GTK_ICON_LOOKUP_NO_SVG: Never return SVG icons, even if gdk-pixbuf
     supports them. Cannot be used together with %GTK_ICON_LOOKUP_FORCE_SVG.
   @GTK_ICON_LOOKUP_FORCE_SVG: Return SVG icons, even if gdk-pixbuf
     doesn't support them.
     Cannot be used together with %GTK_ICON_LOOKUP_NO_SVG.
   @GTK_ICON_LOOKUP_USE_BUILTIN: When passed to
     gtk_icon_theme_lookup_icon() includes builtin icons
     as well as files. For a builtin icon, gdk_icon_info_get_filename()
     returns %NULL and you need to call gdk_icon_info_get_builtin_pixbuf().

   Used to specify options for gtk_icon_theme_lookup_icon()
   }

   PGtkIconLookupFlags = ^TGtkIconLookupFlags;
   TGtkIconLookupFlags = (GTK_ICON_LOOKUP_NO_SVG := 1 shl 0,
                          GTK_ICON_LOOKUP_FORCE_SVG := 1 shl 1,
                          GTK_ICON_LOOKUP_USE_BUILTIN := 1 shl 2);

{
   GtkIconThemeError:
   @GTK_ICON_THEME_NOT_FOUND: The icon specified does not exist in the theme
   @GTK_ICON_THEME_FAILED: An unspecified error occurred.

   Error codes for GtkIconTheme operations.
   }
type

   PGtkIconThemeError = ^TGtkIconThemeError;
   TGtkIconThemeError = (GTK_ICON_THEME_NOT_FOUND,
                         GTK_ICON_THEME_FAILED);
{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_ICON_INFO : GType;
function GTK_TYPE_ICON_THEME : GType;
function GTK_ICON_THEME(obj : pointer) : PGtkIconTheme;
function GTK_ICON_THEME_CLASS(klass : pointer) : PGtkIconThemeClass;
function GTK_IS_ICON_THEME(obj : pointer) : gboolean;
function GTK_IS_ICON_THEME_CLASS(klass : pointer) : gboolean;
function GTK_ICON_THEME_GET_CLASS(obj : pointer) : PGtkIconThemeClass;
function GTK_ICON_THEME_ERROR: TGQuark;


function gtk_icon_theme_error_quark:TGQuark;cdecl;external gtklib name 'gtk_icon_theme_error_quark';

function gtk_icon_theme_get_type:GType;cdecl;external gtklib name 'gtk_icon_theme_get_type';

function gtk_icon_theme_new:PGtkIconTheme;cdecl;external gtklib name 'gtk_icon_theme_new';

function gtk_icon_theme_get_default:PGtkIconTheme;cdecl;external gtklib name 'gtk_icon_theme_get_default';

function gtk_icon_theme_get_for_screen(screen:PGdkScreen):PGtkIconTheme;cdecl;external gtklib name 'gtk_icon_theme_get_for_screen';
procedure gtk_icon_theme_set_screen(icon_theme:PGtkIconTheme; screen:PGdkScreen);cdecl;external gtklib name 'gtk_icon_theme_set_screen';


procedure gtk_icon_theme_set_search_path(icon_theme:PGtkIconTheme; path:array of Pgchar; n_elements:gint);cdecl;external gtklib name 'gtk_icon_theme_set_search_path';
procedure gtk_icon_theme_get_search_path(icon_theme:PGtkIconTheme; path:array of PPgchar; n_elements:Pgint);cdecl;external gtklib name 'gtk_icon_theme_get_search_path';


procedure gtk_icon_theme_append_search_path(icon_theme:PGtkIconTheme; path:Pgchar);cdecl;external gtklib name 'gtk_icon_theme_append_search_path';
procedure gtk_icon_theme_prepend_search_path(icon_theme:PGtkIconTheme; path:Pgchar);cdecl;external gtklib name 'gtk_icon_theme_prepend_search_path';


procedure gtk_icon_theme_set_custom_theme(icon_theme:PGtkIconTheme; theme_name:Pgchar);cdecl;external gtklib name 'gtk_icon_theme_set_custom_theme';


function gtk_icon_theme_has_icon(icon_theme:PGtkIconTheme; icon_name:Pgchar):gboolean;cdecl;external gtklib name 'gtk_icon_theme_has_icon';

function gtk_icon_theme_lookup_icon(icon_theme:PGtkIconTheme; icon_name:Pgchar; size:gint; flags:TGtkIconLookupFlags):PGtkIconInfo;cdecl;external gtklib name 'gtk_icon_theme_lookup_icon';

function gtk_icon_theme_load_icon(icon_theme:PGtkIconTheme; icon_name:Pgchar; size:gint; flags:TGtkIconLookupFlags; error:PPGError):PGdkPixbuf;cdecl;external gtklib name 'gtk_icon_theme_load_icon';

function gtk_icon_theme_list_icons(icon_theme:PGtkIconTheme; context:Pgchar):PGList;cdecl;external gtklib name 'gtk_icon_theme_list_icons';

function gtk_icon_theme_get_example_icon_name(icon_theme:PGtkIconTheme):Pchar;cdecl;external gtklib name 'gtk_icon_theme_get_example_icon_name';

function gtk_icon_theme_rescan_if_needed(icon_theme:PGtkIconTheme):gboolean;cdecl;external gtklib name 'gtk_icon_theme_rescan_if_needed';

procedure gtk_icon_theme_add_builtin_icon(icon_name:Pgchar; size:gint; pixbuf:PGdkPixbuf);cdecl;external gtklib name 'gtk_icon_theme_add_builtin_icon';

function gtk_icon_info_get_type:GType;cdecl;external gtklib name 'gtk_icon_info_get_type';
function gtk_icon_info_copy(icon_info:PGtkIconInfo):PGtkIconInfo;cdecl;external gtklib name 'gtk_icon_info_copy';
procedure gtk_icon_info_free(icon_info:PGtkIconInfo);cdecl;external gtklib name 'gtk_icon_info_free';
function gtk_icon_info_get_base_size(icon_info:PGtkIconInfo):gint;cdecl;external gtklib name 'gtk_icon_info_get_base_size';
function gtk_icon_info_get_filename(icon_info:PGtkIconInfo):Pgchar;cdecl;external gtklib name 'gtk_icon_info_get_filename';
function gtk_icon_info_get_builtin_pixbuf(icon_info:PGtkIconInfo):PGdkPixbuf;cdecl;external gtklib name 'gtk_icon_info_get_builtin_pixbuf';
function gtk_icon_info_load_icon(icon_info:PGtkIconInfo; error:PPGError):PGdkPixbuf;cdecl;external gtklib name 'gtk_icon_info_load_icon';
procedure gtk_icon_info_set_raw_coordinates(icon_info:PGtkIconInfo; raw_coordinates:gboolean);cdecl;external gtklib name 'gtk_icon_info_set_raw_coordinates';
function gtk_icon_info_get_embedded_rect(icon_info:PGtkIconInfo; rectangle:PGdkRectangle):gboolean;cdecl;external gtklib name 'gtk_icon_info_get_embedded_rect';
function gtk_icon_info_get_attach_points(icon_info:PGtkIconInfo; points:PPGdkPoint; n_points:Pgint):gboolean;cdecl;external gtklib name 'gtk_icon_info_get_attach_points';
function gtk_icon_info_get_display_name(icon_info:PGtkIconInfo):Pgchar;cdecl;external gtklib name 'gtk_icon_info_get_display_name';

{$ENDIF read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_ICON_INFO : GType;
begin
   GTK_TYPE_ICON_INFO:=gtk_icon_info_get_type;
end;


function GTK_TYPE_ICON_THEME : GType;
begin
   GTK_TYPE_ICON_THEME:=gtk_icon_theme_get_type;
end;

function GTK_ICON_THEME(obj : pointer) : PGtkIconTheme;
begin
   GTK_ICON_THEME:=PGtkIconTheme(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_ICON_THEME));
end;

function GTK_ICON_THEME_CLASS(klass : pointer) : PGtkIconThemeClass;
begin
   GTK_ICON_THEME_CLASS:=PGtkIconThemeClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_ICON_THEME));
end;

function GTK_IS_ICON_THEME(obj : pointer) : gboolean;
begin
   GTK_IS_ICON_THEME:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_ICON_THEME);
end;

function GTK_IS_ICON_THEME_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_ICON_THEME_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_ICON_THEME);
end;


function GTK_ICON_THEME_GET_CLASS(obj : pointer) : PGtkIconThemeClass;
begin
   GTK_ICON_THEME_GET_CLASS:=PGtkIconThemeClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_ICON_THEME));
end;


function GTK_ICON_THEME_ERROR: TGQuark;
begin
   GTK_ICON_THEME_ERROR:=gtk_icon_theme_error_quark;
end;

{$endif read_implementation}
