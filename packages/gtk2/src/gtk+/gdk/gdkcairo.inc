// included by gdk2.pp

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function gdk_cairo_create(drawable:PGdkDrawable):Pcairo_t;cdecl;external gdklib;
procedure gdk_cairo_set_source_color(cr:Pcairo_t; color:PGdkColor);cdecl;external gdklib;
procedure gdk_cairo_set_source_pixbuf(cr:Pcairo_t; pixbuf:PGdkPixbuf; pixbuf_x:double; pixbuf_y:double);cdecl;external gdklib;
procedure gdk_cairo_set_source_pixmap(cr:Pcairo_t; pixmap:PGdkPixmap; pixmap_x:double; pixmap_y:double);cdecl;external gdklib;
procedure gdk_cairo_rectangle(cr:Pcairo_t; rectangle:PGdkRectangle);cdecl;external gdklib;
procedure gdk_cairo_region(cr:Pcairo_t; region:PGdkRegion);cdecl;external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF}

