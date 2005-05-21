{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglfont.h }

{$ifdef read_interface_rest}

{$ifndef GDK_MULTIHEAD_SAFE}
{TODO
PangoFont *gdk_gl_font_use_pango_font             (const PangoFontDescription *font_desc,
                                                   int                         first,
                                                   int                         count,
                                                   int                         list_base);}
{$endif GDK_MULTIHEAD_SAFE}

{$ifdef GDKGLEXT_MULTIHEAD_SUPPORT}
{TODO
PangoFont *gdk_gl_font_use_pango_font_for_display (GdkDisplay                 *display,
                                                   const PangoFontDescription *font_desc,
                                                   int                         first,
                                                   int                         count,
                                                   int                         list_base);}
{$endif GDKGLEXT_MULTIHEAD_SUPPORT}

{$endif read_interface_rest}
