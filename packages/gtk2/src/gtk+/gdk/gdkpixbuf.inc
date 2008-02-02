// included by gdk2.pp

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ Rendering to a drawable  }

procedure gdk_pixbuf_render_threshold_alpha(pixbuf:PGdkPixbuf; bitmap:PGdkBitmap; src_x:longint; src_y:longint; dest_x:longint;
            dest_y:longint; width:longint; height:longint; alpha_threshold:longint); cdecl; external gdklib;
procedure gdk_pixbuf_render_to_drawable(pixbuf:PGdkPixbuf; drawable:PGdkDrawable; gc:PGdkGC; src_x:longint; src_y:longint;
            dest_x:longint; dest_y:longint; width:longint; height:longint; dither:TGdkRgbDither;
            x_dither:longint; y_dither:longint); cdecl; external gdklib;
procedure gdk_pixbuf_render_to_drawable_alpha(pixbuf:PGdkPixbuf; drawable:PGdkDrawable; src_x:longint; src_y:longint; dest_x:longint;
            dest_y:longint; width:longint; height:longint; alpha_mode:TGdkPixbufAlphaMode; alpha_threshold:longint;
            dither:TGdkRgbDither; x_dither:longint; y_dither:longint); cdecl; external gdklib;
procedure gdk_pixbuf_render_pixmap_and_mask_for_colormap(pixbuf:PGdkPixbuf; colormap:PGdkColormap; var pixmap_return:PGdkPixmap; var mask_return:PGdkBitmap; alpha_threshold:longint); cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
procedure gdk_pixbuf_render_pixmap_and_mask(pixbuf:PGdkPixbuf; var pixmap_return:PGdkPixmap; var mask_return:PGdkBitmap; alpha_threshold:longint); cdecl; external gdklib;
{$endif}


{ Fetching a region from a drawable  }

function gdk_pixbuf_get_from_drawable(dest:PGdkPixbuf; src:PGdkDrawable; cmap:PGdkColormap; src_x:longint; src_y:longint;
           dest_x:longint; dest_y:longint; width:longint; height:longint):PGdkPixbuf; cdecl; external gdklib;
function gdk_pixbuf_get_from_image(dest:PGdkPixbuf; src:PGdkImage; cmap:PGdkColormap; src_x:longint; src_y:longint;
           dest_x:longint; dest_y:longint; width:longint; height:longint):PGdkPixbuf; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF}
