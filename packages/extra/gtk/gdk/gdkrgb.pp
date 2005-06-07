{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGdkRgbCmap = ^TGdkRgbCmap;
     TGdkRgbCmap = record
          colors : array[0..255] of guint32;
          lut : array[0..255] of guchar;
       end;

procedure gdk_rgb_init;cdecl;external gdkdll name 'gdk_rgb_init';
function  gdk_rgb_xpixel_from_rgb(rgb:guint32):gulong;cdecl;external gdkdll name 'gdk_rgb_xpixel_from_rgb';
procedure gdk_rgb_gc_set_foreground(gc:PGdkGC; rgb:guint32);cdecl;external gdkdll name 'gdk_rgb_gc_set_foreground';
procedure gdk_rgb_gc_set_background(gc:PGdkGC; rgb:guint32);cdecl;external gdkdll name 'gdk_rgb_gc_set_background';

  type
     TGdkRgbDither = (GDK_RGB_DITHER_NONE,GDK_RGB_DITHER_NORMAL,GDK_RGB_DITHER_MAX);

procedure gdk_draw_rgb_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint; height:gint; dith:TGdkRgbDither; rgb_buf:Pguchar; rowstride:gint);cdecl;external gdkdll name 'gdk_draw_rgb_image';
procedure gdk_draw_rgb_image_dithalign(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint; height:gint; dith:TGdkRgbDither; rgb_buf:Pguchar; rowstride:gint; xdith:gint; ydith:gint);cdecl;external gdkdll name 'gdk_draw_rgb_image_dithalign';
procedure gdk_draw_rgb_32_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint; height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint);cdecl;external gdkdll name 'gdk_draw_rgb_32_image';
procedure gdk_draw_gray_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint; height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint);cdecl;external gdkdll name 'gdk_draw_gray_image';
function  gdk_rgb_cmap_new(colors:Pguint32; n_colors:gint):PGdkRgbCmap;cdecl;external gdkdll name 'gdk_rgb_cmap_new';
procedure gdk_rgb_cmap_free(cmap:PGdkRgbCmap);cdecl;external gdkdll name 'gdk_rgb_cmap_free';
procedure gdk_draw_indexed_image(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; width:gint; height:gint; dith:TGdkRgbDither; buf:Pguchar; rowstride:gint; cmap:PGdkRgbCmap);cdecl;external gdkdll name 'gdk_draw_indexed_image';
function  gdk_rgb_ditherable:gboolean;cdecl;external gdkdll name 'gdk_rgb_ditherable';
procedure gdk_rgb_set_verbose(verbose:gboolean);cdecl;external gdkdll name 'gdk_rgb_set_verbose';
procedure gdk_rgb_set_install(install:gboolean);cdecl;external gdkdll name 'gdk_rgb_set_install';
procedure gdk_rgb_set_min_colors(min_colors:gint);cdecl;external gdkdll name 'gdk_rgb_set_min_colors';
function  gdk_rgb_get_cmap:PGdkColormap;cdecl;external gdkdll name 'gdk_rgb_get_cmap';
function  gdk_rgb_get_visual:PGdkVisual;cdecl;external gdkdll name 'gdk_rgb_get_visual';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


