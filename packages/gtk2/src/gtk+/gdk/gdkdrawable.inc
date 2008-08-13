// included by gdk2.pp

{$IFDEF read_forward_definitions}
   PGdkDrawable = ^TGdkDrawable;
   TGdkDrawable = record
        parent_instance : TGObject;
     end;

   PGdkWindow = ^TGdkWindow;
   TGdkWindow = TGdkDrawable;

   PGdkPixmap = ^TGdkPixmap;
   TGdkPixmap = TGdkDrawable;

   PGdkBitmap = ^TGdkBitmap;
   TGdkBitmap = TGdkDrawable;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

{ Padding for future expansion  }
   PGdkDrawableClass = ^TGdkDrawableClass;
   TGdkDrawableClass = record
        parent_class : TGObjectClass;
        create_gc : function (drawable:PGdkDrawable; values:PGdkGCValues; mask:TGdkGCValuesMask):PGdkGC; cdecl;
        draw_rectangle : procedure (drawable:PGdkDrawable; gc:PGdkGC; filled:gint; x:gint; y:gint;
                      width:gint; height:gint); cdecl;
        draw_arc : procedure (drawable:PGdkDrawable; gc:PGdkGC; filled:gint; x:gint; y:gint;
                      width:gint; height:gint; angle1:gint; angle2:gint); cdecl;
        draw_polygon : procedure (drawable:PGdkDrawable; gc:PGdkGC; filled:gint; points:PGdkPoint; npoints:gint); cdecl;
        draw_text : procedure (drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint;
                      text:Pgchar; text_length:gint); cdecl;
        draw_text_wc : procedure (drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint;
                      text:PGdkWChar; text_length:gint); cdecl;
        draw_drawable : procedure (drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint;
                      xdest:gint; ydest:gint; width:gint; height:gint); cdecl;
        draw_points : procedure (drawable:PGdkDrawable; gc:PGdkGC; points:PGdkPoint; npoints:gint); cdecl;
        draw_segments : procedure (drawable:PGdkDrawable; gc:PGdkGC; segs:PGdkSegment; nsegs:gint); cdecl;
        draw_lines : procedure (drawable:PGdkDrawable; gc:PGdkGC; points:PGdkPoint; npoints:gint); cdecl;
        draw_glyphs : procedure (drawable:PGdkDrawable; gc:PGdkGC; font:PPangoFont; x:gint; y:gint;
                      glyphs:PPangoGlyphString); cdecl;
        draw_image : procedure (drawable:PGdkDrawable; gc:PGdkGC; image:PGdkImage; xsrc:gint; ysrc:gint;
                      xdest:gint; ydest:gint; width:gint; height:gint); cdecl;
        get_depth : function (drawable:PGdkDrawable):gint; cdecl;
        get_size : procedure (drawable:PGdkDrawable; width:Pgint; height:Pgint); cdecl;
        set_colormap : procedure (drawable:PGdkDrawable; cmap:PGdkColormap); cdecl;
        get_colormap : function (drawable:PGdkDrawable):PGdkColormap; cdecl;
        get_visual : function (drawable:PGdkDrawable):PGdkVisual; cdecl;
        get_screen : function (drawable:PGdkDrawable):PGdkScreen; cdecl;
        get_image : function (drawable:PGdkDrawable; x:gint; y:gint; width:gint; height:gint):PGdkImage; cdecl;
        get_clip_region : function (drawable:PGdkDrawable):PGdkRegion; cdecl;
        get_visible_region : function (drawable:PGdkDrawable):PGdkRegion; cdecl;
        get_composite_drawable : function (drawable:PGdkDrawable; x:gint; y:gint; width:gint; height:gint;
                     composite_x_offset:Pgint; composite_y_offset:Pgint):PGdkDrawable; cdecl;
        _draw_pixbuf : procedure (drawable:PGdkDrawable; gc:PGdkGC; pixbuf:PGdkPixbuf; src_x:gint; src_y:gint;
                      dest_x:gint; dest_y:gint; width:gint; height:gint; dither:TGdkRgbDither;
                      x_dither:gint; y_dither:gint); cdecl;
        _copy_to_image : function (drawable:PGdkDrawable; image:PGdkImage; src_x:gint; src_y:gint; dest_x:gint;
                     dest_y:gint; width:gint; height:gint):PGdkImage; cdecl;
        _gdk_reserved1 : procedure ; cdecl;
        _gdk_reserved2 : procedure ; cdecl;
        _gdk_reserved3 : procedure ; cdecl;
        _gdk_reserved4 : procedure ; cdecl;
        _gdk_reserved5 : procedure ; cdecl;
        _gdk_reserved6 : procedure ; cdecl;
        _gdk_reserved7 : procedure ; cdecl;
        _gdk_reserved9 : procedure ; cdecl;
        _gdk_reserved10 : procedure ; cdecl;
        _gdk_reserved11 : procedure ; cdecl;
        _gdk_reserved12 : procedure ; cdecl;
        _gdk_reserved13 : procedure ; cdecl;
        _gdk_reserved14 : procedure ; cdecl;
        _gdk_reserved15 : procedure ; cdecl;
        _gdk_reserved16 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function GDK_TYPE_DRAWABLE : GType;
function GDK_DRAWABLE(anObject : Pointer) : PGdkDrawable;
function GDK_DRAWABLE_CLASS(klass : Pointer) : PGdkDrawableClass;
function GDK_IS_DRAWABLE(anObject : Pointer) : boolean;
function GDK_IS_DRAWABLE_CLASS(klass : Pointer) : boolean;
function GDK_DRAWABLE_GET_CLASS(obj : Pointer) : PGdkDrawableClass;


function gdk_drawable_get_type:GType; cdecl; external gdklib;
{ Manipulation of drawables }

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_drawable_set_data(drawable:PGdkDrawable; key:Pgchar; data:gpointer; destroy_func:TGDestroyNotify); cdecl; external gdklib;
function gdk_drawable_get_data(drawable:PGdkDrawable; key:Pgchar):gpointer; cdecl; external gdklib;
{$endif}
{ GDK_DISABLE_DEPRECATED  }

procedure gdk_drawable_get_size(drawable:PGdkDrawable; width:Pgint; height:Pgint); cdecl; external gdklib;
procedure gdk_drawable_set_colormap(drawable:PGdkDrawable; colormap:PGdkColormap); cdecl; external gdklib;
function gdk_drawable_get_colormap(drawable:PGdkDrawable):PGdkColormap; cdecl; external gdklib;
function gdk_drawable_get_visual(drawable:PGdkDrawable):PGdkVisual; cdecl; external gdklib;
function gdk_drawable_get_depth(drawable:PGdkDrawable):gint; cdecl; external gdklib;
function gdk_drawable_get_screen(drawable:PGdkDrawable):PGdkScreen; cdecl; external gdklib;
function gdk_drawable_get_display(drawable:PGdkDrawable):PGdkDisplay; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_drawable_ref(drawable:PGdkDrawable):PGdkDrawable; cdecl; external gdklib;
procedure gdk_drawable_unref(drawable:PGdkDrawable); cdecl; external gdklib;
{$endif}
{ GDK_DISABLE_DEPRECATED  }


{ Drawing }

procedure gdk_draw_point(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint); cdecl; external gdklib;
procedure gdk_draw_line(drawable:PGdkDrawable; gc:PGdkGC; x1:gint; y1:gint; x2:gint;
            y2:gint); cdecl; external gdklib;
procedure gdk_draw_rectangle(drawable:PGdkDrawable; gc:PGdkGC; filled:gint; x:gint; y:gint;
            width:gint; height:gint); cdecl; external gdklib;
procedure gdk_draw_arc(drawable:PGdkDrawable; gc:PGdkGC; filled:gint; x:gint; y:gint;
            width:gint; height:gint; angle1:gint; angle2:gint); cdecl; external gdklib;
procedure gdk_draw_polygon(drawable:PGdkDrawable; gc:PGdkGC; filled:gint; points:PGdkPoint; npoints:gint); cdecl; external gdklib;
procedure gdk_draw_pixbuf(drawable: PGdkDrawable; gc:PGdkGC; src:PGdkPixbuf;
            xsrc:gint; ysrc:gint; xdest:gint; ydest:gint;
            width:gint; height:gint; dither:TGdkRgbDither;
            x_dither:gint; y_dither:gint); cdecl; external gdklib;


{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_draw_string(drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint;
            _string:Pgchar); cdecl; external gdklib;
procedure gdk_draw_text(drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint;
            text:Pgchar; text_length:gint); cdecl; external gdklib;
procedure gdk_draw_text_wc(drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint;
            text:PGdkWChar; text_length:gint); cdecl; external gdklib;
{$endif}


{ GDK_DISABLE_DEPRECATED  }

procedure gdk_draw_drawable(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint;
            xdest:gint; ydest:gint; width:gint; height:gint); cdecl; external gdklib;
procedure gdk_draw_image(drawable:PGdkDrawable; gc:PGdkGC; image:PGdkImage; xsrc:gint; ysrc:gint;
            xdest:gint; ydest:gint; width:gint; height:gint); cdecl; external gdklib;
procedure gdk_draw_points(drawable:PGdkDrawable; gc:PGdkGC; points:PGdkPoint; npoints:gint); cdecl; external gdklib;
procedure gdk_draw_segments(drawable:PGdkDrawable; gc:PGdkGC; segs:PGdkSegment; nsegs:gint); cdecl; external gdklib;
procedure gdk_draw_lines(drawable:PGdkDrawable; gc:PGdkGC; points:PGdkPoint; npoints:gint); cdecl; external gdklib;
procedure gdk_draw_glyphs(drawable:PGdkDrawable; gc:PGdkGC; font:PPangoFont; x:gint; y:gint;
            glyphs:PPangoGlyphString); cdecl; external gdklib;
procedure gdk_draw_layout_line(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; line:PPangoLayoutLine); cdecl; external gdklib;
procedure gdk_draw_layout(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; layout:PPangoLayout); cdecl; external gdklib;
procedure gdk_draw_layout_line_with_colors(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; line:PPangoLayoutLine;
            foreground:PGdkColor; background:PGdkColor); cdecl; external gdklib;
procedure gdk_draw_layout_with_colors(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint; layout:PPangoLayout;
            foreground:PGdkColor; background:PGdkColor); cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_draw_pixmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint;
            xdest:gint; ydest:gint; width:gint; height:gint);
procedure gdk_draw_bitmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint;
            xdest:gint; ydest:gint; width:gint; height:gint);
{$endif}
{ GDK_DISABLE_DEPRECATED  }

function gdk_drawable_get_image(drawable:PGdkDrawable; x:gint; y:gint; width:gint; height:gint):PGdkImage; cdecl; external gdklib;
function gdk_drawable_copy_to_image(drawable:PGdkDrawable; image: PGdkImage; src_x: gint; src_y: gint; dest_x: gint; dest_y: gint;
           width: gint; height: gint):PGdkImage; cdecl; external gdklib;

function gdk_drawable_get_clip_region(drawable:PGdkDrawable):PGdkRegion; cdecl; external gdklib;
function gdk_drawable_get_visible_region(drawable:PGdkDrawable):PGdkRegion; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_DRAWABLE : GType;
begin
  GDK_TYPE_DRAWABLE:=gdk_drawable_get_type;
end;

function GDK_DRAWABLE(anObject : Pointer) : PGdkDrawable;
begin
  GDK_DRAWABLE:=PGdkDrawable(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                            GDK_TYPE_DRAWABLE));
end;

function GDK_DRAWABLE_CLASS(klass : Pointer) : PGdkDrawableClass;
begin
  GDK_DRAWABLE_CLASS:=PGdkDrawableClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                            GDK_TYPE_DRAWABLE));
end;

function GDK_IS_DRAWABLE(anObject : Pointer) : boolean;
begin
  GDK_IS_DRAWABLE:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_DRAWABLE);
end;

function GDK_IS_DRAWABLE_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_DRAWABLE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DRAWABLE);
end;

function GDK_DRAWABLE_GET_CLASS(obj : Pointer) : PGdkDrawableClass;
begin
  GDK_DRAWABLE_GET_CLASS:=PGdkDrawableClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                            GDK_TYPE_DRAWABLE));
end;

procedure gdk_draw_pixmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable;
  xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);
begin
  gdk_draw_drawable(drawable,gc,src,xsrc,ysrc,xdest,ydest,width,height);
end;

procedure gdk_draw_bitmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable;
  xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);
begin
  gdk_draw_drawable(drawable,gc,src,xsrc,ysrc,xdest,ydest,width,height);
end;
{$ENDIF}

