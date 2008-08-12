// included by gdk2.pp

{$IFDEF read_forward_definitions}
{ The color type.
     A color consists of red, green and blue values in the
      range 0-65535 and a pixel value. The pixel value is highly
      dependent on the depth and colormap which this color will
      be used to draw into. Therefore, sharing colors between
      colormaps is a bad idea.
  }
   PGdkColor = ^TGdkColor;
   TGdkColor = record
        pixel : guint32;
        red : guint16;
        green : guint16;
        blue : guint16;
     end;

   PGdkColormap = ^TGdkColormap;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGdkColormapClass = ^TGdkColormapClass;
   TGdkColormapClass = record
        parent_class : TGObjectClass;
     end;

   TGdkColormap = record
        parent_instance : TGObject;
        size : gint;
        colors : PGdkColor;
        visual : PGdkVisual;
        windowing_data : gpointer;
        screen : PGdkScreen;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ The colormap type. }
function GDK_TYPE_COLORMAP : GType;
function GDK_COLORMAP(anObject : pointer) : PGdkColormap;
function GDK_COLORMAP_CLASS(klass : pointer) : PGdkColormapClass;
function GDK_IS_COLORMAP(anObject : pointer) : boolean;
function GDK_IS_COLORMAP_CLASS(klass : pointer) : boolean;
function GDK_COLORMAP_GET_CLASS(obj : pointer) : PGdkColormapClass;

function GDK_TYPE_COLOR : GType;

function gdk_colormap_get_type:GType; cdecl; external gdklib;
function gdk_colormap_new(visual:PGdkVisual; allocate:gboolean):PGdkColormap; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_colormap_ref(cmap:PGdkColormap):PGdkColormap; cdecl; external gdklib;
procedure gdk_colormap_unref(cmap:PGdkColormap); cdecl; external gdklib;
{$endif}

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_colormap_get_system:PGdkColormap; cdecl; external gdklib;
{$endif}

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_colormap_get_system_size:gint; cdecl; external gdklib;
procedure gdk_colormap_change(colormap:PGdkColormap; ncolors:gint); cdecl; external gdklib;
{$endif}

function gdk_colormap_alloc_colors(colormap:PGdkColormap; colors:PGdkColor; ncolors:gint; writeable:gboolean; best_match:gboolean;
           success:Pgboolean):gint; cdecl; external gdklib;
function gdk_colormap_alloc_color(colormap:PGdkColormap; color:PGdkColor; writeable:gboolean; best_match:gboolean):gboolean; cdecl; external gdklib;
procedure gdk_colormap_free_colors(colormap:PGdkColormap; colors:PGdkColor; ncolors:gint); cdecl; external gdklib;
procedure gdk_colormap_query_color(colormap:PGdkColormap; pixel:gulong; result:PGdkColor); cdecl; external gdklib;
function gdk_colormap_get_screen(cmap: PGdkColormap):PGdkScreen; cdecl; external gdklib;
function gdk_colormap_get_visual(colormap:PGdkColormap):PGdkVisual; cdecl; external gdklib;
function gdk_color_copy(color:PGdkColor):PGdkColor; cdecl; external gdklib;
procedure gdk_color_free(color:PGdkColor); cdecl; external gdklib;
function gdk_color_parse(spec:Pgchar; color:PGdkColor):gint; cdecl; external gdklib;
function gdk_color_hash(colora:PGdkColor):guint; cdecl; external gdklib;
function gdk_color_equal(colora:PGdkColor; colorb:PGdkColor):gboolean; cdecl; external gdklib;
function gdk_color_get_type:GType; cdecl; external gdklib;

{ The following functions are deprecated  }
{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_colors_store(colormap:PGdkColormap; colors:PGdkColor; ncolors:gint); cdecl; external gdklib;
function gdk_colors_alloc(colormap:PGdkColormap; contiguous:gboolean; planes:Pgulong; nplanes:gint; pixels:Pgulong;
           npixels:gint):gint; cdecl; external gdklib;
procedure gdk_colors_free(colormap:PGdkColormap; pixels:Pgulong; npixels:gint; planes:gulong); cdecl; external gdklib;
function gdk_color_white(colormap:PGdkColormap; color:PGdkColor):gint; cdecl; external gdklib;
function gdk_color_black(colormap:PGdkColormap; color:PGdkColor):gint; cdecl; external gdklib;
function gdk_color_alloc(colormap:PGdkColormap; color:PGdkColor):gint; cdecl; external gdklib;
function gdk_color_change(colormap:PGdkColormap; color:PGdkColor):gint; cdecl; external gdklib;
{$endif}

{ GDK_DISABLE_DEPRECATED  }
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_COLORMAP : GType;
begin
  GDK_TYPE_COLORMAP:=gdk_colormap_get_type;
end;

function GDK_COLORMAP(anObject : pointer) : PGdkColormap;
begin
  GDK_COLORMAP:=PGdkColormap(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                        GDK_TYPE_COLORMAP));
end;

function GDK_COLORMAP_CLASS(klass : pointer) : PGdkColormapClass;
begin
  GDK_COLORMAP_CLASS:=PGdkColormapClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                            GDK_TYPE_COLORMAP));
end;

function GDK_IS_COLORMAP(anObject : pointer) : boolean;
begin
  GDK_IS_COLORMAP:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_COLORMAP);
end;

function GDK_IS_COLORMAP_CLASS(klass : pointer) : boolean;
begin
  GDK_IS_COLORMAP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_COLORMAP);
end;

function GDK_COLORMAP_GET_CLASS(obj : pointer) : PGdkColormapClass;
begin
  GDK_COLORMAP_GET_CLASS:=PGdkColormapClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                            GDK_TYPE_COLORMAP));
end;

function GDK_TYPE_COLOR : GType;
begin
  GDK_TYPE_COLOR:=gdk_color_get_type;
end;
{$ENDIF}

