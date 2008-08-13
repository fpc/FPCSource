// included by pango.pp

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
const N_RENDER_PARTS = 4;
type
   PPangoRendererPrivate = ^TPangoRendererPrivate;

   PPangoRenderPart = ^TPangoRenderPart;
   TPangoRenderPart = (
     PANGO_RENDER_PART_FOREGROUND,
     PANGO_RENDER_PART_BACKGROUND,
     PANGO_RENDER_PART_UNDERLINE,
     PANGO_RENDER_PART_STRIKETHROUGH
   );

   PPangoRenderer = ^TPangoRenderer;
   TPangoRenderer = record
        parent_instance : TGObject;
        underline : TPangoUnderline;
        strikethrough : gboolean;
        active_count : longint;
        matrix : PPangoMatrix;
        priv : PPangoRendererPrivate;
     end;

     PPangoRendererClass = ^TPangoRendererClass;
     TPangoRendererClass = record
          parent_class : TGObjectClass;
          draw_glyphs : procedure (renderer:PPangoRenderer; font:PPangoFont; glyphs:PPangoGlyphString; x:longint; y:longint);cdecl;
          draw_rectangle : procedure (renderer:PPangoRenderer; part:TPangoRenderPart; x:longint; y:longint; width:longint; 
                        height:longint); cdecl;
          draw_error_underline : procedure (renderer:PPangoRenderer; x:longint; y:longint; width:longint; height:longint); cdecl;
          draw_shape : procedure (renderer:PPangoRenderer; attr:PPangoAttrShape; x:longint; y:longint); cdecl;
          draw_trapezoid : procedure (renderer:PPangoRenderer; part:TPangoRenderPart; y1_:double; x11:double; x21:double; 
                        y2:double; x12:double; x22:double); cdecl;
          draw_glyph : procedure (renderer:PPangoRenderer; font:PPangoFont; glyph:TPangoGlyph; x:double; y:double); cdecl;
          part_changed : procedure (renderer:PPangoRenderer; part:TPangoRenderPart); cdecl;
          _begin : procedure (renderer:PPangoRenderer); cdecl;
          _end : procedure (renderer:PPangoRenderer); cdecl;
          prepare_run : procedure (renderer:PPangoRenderer; run:PPangoLayoutRun); cdecl;
          _pango_reserved1 : procedure ; cdecl;
          _pango_reserved2 : procedure ; cdecl;
          _pango_reserved3 : procedure ; cdecl;
          _pango_reserved4 : procedure ; cdecl;
       end;

     TPangoRendererPrivate = record
       color: array[0..N_RENDER_PARTS-1] of TPangoColor;
       color_set: array[0..N_RENDER_PARTS-1] of gboolean;

       line: PPangoLayoutLine;
       // line_state: PLineState;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function PANGO_TYPE_RENDERER : GType;
function PANGO_RENDERER(anObject : pointer) : PPangoRenderer;
function PANGO_IS_RENDERER(anObject : pointer) : boolean;
function PANGO_RENDERER_CLASS(klass : pointer) : PPangoRendererClass;
function PANGO_IS_RENDERER_CLASS(klass : pointer) : boolean;
function PANGO_RENDERER_GET_CLASS(obj : pointer) : PPangoRendererClass;

function pango_renderer_get_type: GType; cdecl; external pangolib;
procedure pango_renderer_draw_layout(renderer:PPangoRenderer; layout:PPangoLayout; x:longint; y:longint);cdecl; external pangolib;
procedure pango_renderer_draw_layout_line(renderer:PPangoRenderer; line:PPangoLayoutLine; x:longint; y:longint);cdecl; external pangolib;
procedure pango_renderer_draw_glyphs(renderer:PPangoRenderer; font:PPangoFont; glyphs:PPangoGlyphString; x:longint; y:longint);cdecl; external pangolib;
procedure pango_renderer_draw_rectangle(renderer:PPangoRenderer; part:TPangoRenderPart; x:longint; y:longint; width:longint; 
            height:longint);cdecl; external pangolib;
procedure pango_renderer_draw_error_underline(renderer:PPangoRenderer; x:longint; y:longint; width:longint; height:longint);cdecl; external pangolib;
procedure pango_renderer_draw_trapezoid(renderer:PPangoRenderer; part:TPangoRenderPart; y1_:double; x11:double; x21:double; 
            y2:double; x12:double; x22:double);cdecl; external pangolib;
procedure pango_renderer_draw_glyph(renderer:PPangoRenderer; font:PPangoFont; glyph:TPangoGlyph; x:double; y:double);cdecl; external pangolib;
procedure pango_renderer_activate(renderer:PPangoRenderer);cdecl; external pangolib;
procedure pango_renderer_deactivate(renderer:PPangoRenderer);cdecl; external pangolib;
procedure pango_renderer_part_changed(renderer:PPangoRenderer; part:TPangoRenderPart);cdecl; external pangolib;
procedure pango_renderer_set_color(renderer:PPangoRenderer; part:TPangoRenderPart; color:PPangoColor);cdecl; external pangolib;
function pango_renderer_get_color(renderer:PPangoRenderer; part:TPangoRenderPart):PPangoColor;cdecl; external pangolib;
procedure pango_renderer_set_matrix(renderer:PPangoRenderer; matrix:PPangoMatrix);cdecl; external pangolib;
function pango_renderer_get_matrix(renderer:PPangoRenderer):PPangoMatrix;cdecl; external pangolib;
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function PANGO_TYPE_RENDERER : GType;
begin
  PANGO_TYPE_RENDERER:=pango_renderer_get_type;
end;

function PANGO_RENDERER(anObject : pointer) : PPangoRenderer;
begin
  PANGO_RENDERER:=PPangoRenderer(G_TYPE_CHECK_INSTANCE_CAST(anObject,PANGO_TYPE_RENDERER));
end;

function PANGO_IS_RENDERER(anObject : pointer) : boolean;
begin
  PANGO_IS_RENDERER:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,PANGO_TYPE_RENDERER);
end;

function PANGO_RENDERER_CLASS(klass : pointer) : PPangoRendererClass;
begin
  PANGO_RENDERER_CLASS:=PPangoRendererClass(G_TYPE_CHECK_CLASS_CAST(klass,PANGO_TYPE_RENDERER));
end;

function PANGO_IS_RENDERER_CLASS(klass : pointer) : boolean;
begin
  PANGO_IS_RENDERER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,PANGO_TYPE_RENDERER);
end;

function PANGO_RENDERER_GET_CLASS(obj : pointer) : PPangoRendererClass;
begin
  PANGO_RENDERER_GET_CLASS:=PPangoRendererClass(G_TYPE_INSTANCE_GET_CLASS(obj,PANGO_TYPE_RENDERER));
end;
{$ENDIF}
