// included by gdk2.pp

{$IFDEF read_forward_definitions}
   PGdkRgbDither = ^TGdkRgbDither;
   TGdkRgbDither = (
     GDK_RGB_DITHER_NONE,
     GDK_RGB_DITHER_NORMAL,
     GDK_RGB_DITHER_MAX
   );
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< private > }
   PGdkRgbCmap = ^TGdkRgbCmap;
   TGdkRgbCmap = record
        colors : array[0..255] of guint32;
        n_colors : gint;
        info_list : PGSList;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_rgb_init; cdecl; external gdklib;
function gdk_rgb_xpixel_from_rgb(rgb:guint32):gulong; cdecl; external gdklib;
procedure gdk_rgb_gc_set_foreground(gc:PGdkGC; rgb:guint32); cdecl; external gdklib;
procedure gdk_rgb_gc_set_background(gc:PGdkGC; rgb:guint32); cdecl; external gdklib;
{$endif}


{ GDK_DISABLE_DEPRECATED  }

procedure gdk_rgb_find_color(colormap:PGdkColormap; color:PGdkColor); cdecl; external gdklib;

procedure gdk_draw_rgb_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint;
            height:gint; dith:TGdkRgbDither; rgb_buf:Pguchar; rowstride:gint); cdecl; external gdklib;
procedure gdk_draw_rgb_image_dithalign(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint;
            height:gint; dith:TGdkRgbDither; rgb_buf:Pguchar; rowstride:gint; xdith:gint;
            ydith:gint); cdecl; external gdklib;
procedure gdk_draw_rgb_32_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint;
            height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint); cdecl; external gdklib;
procedure gdk_draw_rgb_32_image_dithalign(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint;
            height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint; xdith:gint;
            ydith:gint); cdecl; external gdklib;
procedure gdk_draw_gray_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint;
            height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint); cdecl; external gdklib;
procedure gdk_draw_indexed_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint;
            height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint; cmap:PGdkRgbCmap); cdecl; external gdklib;
function gdk_rgb_cmap_new(colors:Pguint32; n_colors:gint):PGdkRgbCmap; cdecl; external gdklib;
procedure gdk_rgb_cmap_free(cmap:PGdkRgbCmap); cdecl; external gdklib;
procedure gdk_rgb_set_verbose(verbose:gboolean); cdecl; external gdklib;


{ experimental colormap stuff  }

procedure gdk_rgb_set_install(install:gboolean); cdecl; external gdklib;
procedure gdk_rgb_set_min_colors(min_colors:gint); cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_rgb_get_colormap:PGdkColormap; cdecl; external gdklib;
function gdk_rgb_get_visual:PGdkVisual; cdecl; external gdklib;
function gdk_rgb_ditherable:gboolean; cdecl; external gdklib;
{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_rgb_get_cmap:PGdkColormap;
{$endif GDK_DISABLE_DEPRECATED}
{$endif GDK_MULTIHEAD_SAFE}

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function gdk_rgb_get_cmap:PGdkColormap;
begin
  gdk_rgb_get_cmap:=gdk_rgb_get_colormap;
end;
{$ENDIF}

