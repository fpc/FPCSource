// included by gdk2.pp

{$IFDEF read_forward_definitions}
   PGdkPangoRenderer = ^TGdkPangoRenderer;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGdkPangoRendererPrivate = ^TGdkPangoRendererPrivate;

   TGdkPangoRenderer = record
     // private
     parent_instance: TPangoRenderer;

     priv: PGdkPangoRendererPrivate;
   end;


   PGdkPangoRendererClass = ^TGdkPangoRendererClass;
   TGdkPangoRendererClass = record
     // private
     parent_class: TPangoRendererClass;
   end;
   
   TGdkPangoRendererPrivate = record
     screen: PGdkScreen;

     // GdkPangoRenderer specific state
     override_color: array[TPangoRenderPart] of TPangoColor;
     override_color_set: array[TPangoRenderPart] of gboolean;
     
     stipple: array[TPangoRenderPart] of PGdkBitmap;
     emboss_color: TPangoColor;
     embossed: gboolean;

     // cr: Pcairo_t;
     // last_part: TPangoRenderPart;

     // Current target
     // drawable: PGdkDrawable;
     // base_gc: PGdkGC;

     // gc_changed: gboolean;
   end;

{ Attributes use to render insensitive text in GTK+.  }
   PGdkPangoAttrStipple = ^TGdkPangoAttrStipple;
   TGdkPangoAttrStipple = record
        attr : TPangoAttribute;
        stipple : PGdkBitmap;
     end;

   PGdkPangoAttrEmbossed = ^TGdkPangoAttrEmbossed;
   TGdkPangoAttrEmbossed = record
        attr : TPangoAttribute;
        embossed : gboolean;
     end;


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ Pango renderer }

function GDK_TYPE_PANGO_RENDERER: GType;
function GDK_PANGO_RENDERER(anObject: pointer): PGdkPangoRenderer;
function GDK_IS_PANGO_RENDERER(anObject: pointer): boolean;
function GDK_PANGO_RENDERER_CLASS(klass: pointer): PGdkPangoRendererClass;
function GDK_IS_PANGO_RENDERER_CLASS(klass: pointer): boolean;
function GDK_PANGO_RENDERER_GET_CLASS(obj: pointer): PGdkPangoRendererClass;

function gdk_pango_renderer_get_type: GType; cdecl; external gdklib;
function gdk_pango_renderer_new(screen:PGdkScreen):PGdkPangoRenderer; cdecl; external gdklib;
function gdk_pango_renderer_get_default(screen:PGdkScreen):PGdkPangoRenderer; cdecl; external gdklib;
procedure gdk_pango_renderer_set_drawable(gdk_renderer:PGdkPangoRenderer; drawable:PGdkDrawable); cdecl; external gdklib;
procedure gdk_pango_renderer_set_gc(gdk_renderer:PGdkPangoRenderer; gc:PGdkGC); cdecl; external gdklib;
procedure gdk_pango_renderer_set_stipple(gdk_renderer:PGdkPangoRenderer; part:TPangoRenderPart; stipple:PGdkBitmap); cdecl; external gdklib;
procedure gdk_pango_renderer_set_override_color(gdk_renderer:PGdkPangoRenderer; part:TPangoRenderPart; color:PGdkColor); cdecl; external gdklib;

{ Pango interaction  }

function gdk_pango_context_get_for_screen(screen:PGdkScreen):PPangoContext; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_pango_context_get:PPangoContext; cdecl; external gdklib;
{$endif}

procedure gdk_pango_context_set_colormap(context:PPangoContext; colormap:PGdkColormap); cdecl; external gdklib;

{ Get a clip region to draw only part of a layout or
   line. index_ranges contains alternating range starts/stops. The
   region is the region which contains the given ranges, i.e. if you
   draw with the region as clip, only the given ranges are drawn.
  }
function gdk_pango_layout_line_get_clip_region(line:PPangoLayoutLine; x_origin:gint; y_origin:gint; index_ranges:Pgint; n_ranges:gint):PGdkRegion; cdecl; external gdklib;
function gdk_pango_layout_get_clip_region(layout:PPangoLayout; x_origin:gint; y_origin:gint; index_ranges:Pgint; n_ranges:gint):PGdkRegion; cdecl; external gdklib;

function gdk_pango_attr_stipple_new(stipple:PGdkBitmap):PPangoAttribute; cdecl; external gdklib;
function gdk_pango_attr_embossed_new(embossed:gboolean):PPangoAttribute; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_PANGO_RENDERER: GType;
begin
  GDK_TYPE_PANGO_RENDERER:=gdk_pango_renderer_get_type;
end;

function GDK_PANGO_RENDERER(anObject: pointer): PGdkPangoRenderer;
begin
  GDK_PANGO_RENDERER:=PGdkPangoRenderer(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_PANGO_RENDERER));
end;

function GDK_IS_PANGO_RENDERER(anObject: pointer): boolean;
begin
  GDK_IS_PANGO_RENDERER:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_PANGO_RENDERER);
end;

function GDK_PANGO_RENDERER_CLASS(klass: pointer): PGdkPangoRendererClass;
begin
  GDK_PANGO_RENDERER_CLASS:=PGdkPangoRendererClass(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_PANGO_RENDERER));
end;

function GDK_IS_PANGO_RENDERER_CLASS(klass: pointer) : boolean;
begin
  GDK_IS_PANGO_RENDERER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_PANGO_RENDERER);
end;

function GDK_PANGO_RENDERER_GET_CLASS(obj: pointer): PGdkPangoRendererClass;
begin
  GDK_PANGO_RENDERER_GET_CLASS:=PGdkPangoRendererClass(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_PANGO_RENDERER));
end;
{$ENDIF}
