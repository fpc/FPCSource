{
   $Id$
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


{
  $Log$
  Revision 1.1  2000-07-13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:33  peter
    * moved to packages dir

  Revision 1.7  1999/10/21 14:35:23  peter
    * removed glibdll constant

  Revision 1.6  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp gdkdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           gdkdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.5  1999/10/06 17:42:47  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.4  1999/05/11 00:37:53  peter
    * win32 fixes

  Revision 1.3  1999/05/10 09:02:38  peter
    * gtk 1.2 port working

  Revision 1.2  1999/05/07 15:09:43  peter
    * more fixes

  Revision 1.1  1999/05/07 10:40:20  peter
    * first things for 1.2

}

