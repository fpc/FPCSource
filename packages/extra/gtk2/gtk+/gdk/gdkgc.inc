// included by gdk2.pp

{$IFDEF read_forward_definitions}
{ GC function types.
     Copy: Overwrites destination pixels with the source pixels.
     Invert: Inverts the destination pixels.
     Xor: Xor's the destination pixels with the source pixels.
     Clear: set pixels to 0
     And: source AND destination
     And Reverse: source AND (NOT destination)
     And Invert: (NOT source) AND destination
     Noop: destination
     Or: source OR destination
     Nor: (NOT source) AND (NOT destination)
     Equiv: (NOT source) XOR destination
     Xor Reverse: source OR (NOT destination)
     Copy Inverted: NOT source
     Xor Inverted: (NOT source) OR destination
     Nand: (NOT source) OR (NOT destination)
     Set: set pixels to 1
  }
   PGdkFunction = ^TGdkFunction;
   TGdkFunction = (
     GDK_COPY,
     GDK_INVERT,
     GDK_XOR,
     GDK_CLEAR,
     GDK_AND,
     GDK_AND_REVERSE,
     GDK_AND_INVERT,
     GDK_NOOP,
     GDK_OR,
     GDK_EQUIV,
     GDK_OR_REVERSE,
     GDK_COPY_INVERT,
     GDK_OR_INVERT,
     GDK_NAND,
     GDK_NOR,
     GDK_SET
   );

{ GC cap styles
    CapNotLast:
    CapButt:
    CapRound:
    CapProjecting:
  }
   PGdkCapStyle = ^TGdkCapStyle;
   TGdkCapStyle = (
     GDK_CAP_NOT_LAST,
     GDK_CAP_BUTT,
     GDK_CAP_ROUND,
     GDK_CAP_PROJECTING
   );

{ GC fill types.
    Solid:
    Tiled:
    Stippled:
    OpaqueStippled:
  }
   PGdkFill = ^TGdkFill;
   TGdkFill = (
     GDK_SOLID,
     GDK_TILED,
     GDK_STIPPLED,
     GDK_OPAQUE_STIPPLED
   );

{ GC join styles
    JoinMiter:
    JoinRound:
    JoinBevel:
  }
   PGdkJoinStyle = ^TGdkJoinStyle;
   TGdkJoinStyle = (
     GDK_JOIN_MITER,
     GDK_JOIN_ROUND,
     GDK_JOIN_BEVEL
     );

{ GC line styles
    Solid:
    OnOffDash:
    DoubleDash:
  }
   PGdkLineStyle = ^TGdkLineStyle;
   TGdkLineStyle = (
     GDK_LINE_SOLID,
     GDK_LINE_ON_OFF_DASH,
     GDK_LINE_DOUBLE_DASH
     );

   PGdkSubwindowMode = ^TGdkSubwindowMode;
   TGdkSubwindowMode = integer; // see below for possible values

   PGdkGCValuesMask = ^TGdkGCValuesMask;
   TGdkGCValuesMask = longint; // see below for possible values

   PGdkGCValues = ^TGdkGCValues;
   TGdkGCValues = record
        foreground : TGdkColor;
        background : TGdkColor;
        font : PGdkFont;
        _function : TGdkFunction;
        fill : TGdkFill;
        tile : PGdkPixmap;
        stipple : PGdkPixmap;
        clip_mask : PGdkPixmap;
        subwindow_mode : TGdkSubwindowMode;
        ts_x_origin : gint;
        ts_y_origin : gint;
        clip_x_origin : gint;
        clip_y_origin : gint;
        graphics_exposures : gint;
        line_width : gint;
        line_style : TGdkLineStyle;
        cap_style : TGdkCapStyle;
        join_style : TGdkJoinStyle;
     end;

   PGdkGC = ^TGdkGC;
   TGdkGC = record
        parent_instance : TGObject;
        clip_x_origin : gint;
        clip_y_origin : gint;
        ts_x_origin : gint;
        ts_y_origin : gint;
        colormap : PGdkColormap;
     end;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Padding for future expansion  }
   PGdkGCClass = ^TGdkGCClass;
   TGdkGCClass = record
        parent_class : TGObjectClass;
        get_values : procedure (gc:PGdkGC; values:PGdkGCValues); cdecl;
        set_values : procedure (gc:PGdkGC; values:PGdkGCValues; mask:TGdkGCValuesMask); cdecl;
        set_dashes : procedure (gc:PGdkGC; dash_offset:gint; dash_list:array of gint8; n:gint); cdecl;
        _gdk_reserved1 : procedure ; cdecl;
        _gdk_reserved2 : procedure ; cdecl;
        _gdk_reserved3 : procedure ; cdecl;
        _gdk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  // values for TGdkGCValuesMask
  GDK_GC_FOREGROUND = 1 shl 0;
  GDK_GC_BACKGROUND = 1 shl 1;
  GDK_GC_FONT = 1 shl 2;
  GDK_GC_FUNCTION = 1 shl 3;
  GDK_GC_FILL = 1 shl 4;
  GDK_GC_TILE = 1 shl 5;
  GDK_GC_STIPPLE = 1 shl 6;
  GDK_GC_CLIP_MASK = 1 shl 7;
  GDK_GC_SUBWINDOW = 1 shl 8;
  GDK_GC_TS_X_ORIGIN = 1 shl 9;
  GDK_GC_TS_Y_ORIGIN = 1 shl 10;
  GDK_GC_CLIP_X_ORIGIN = 1 shl 11;
  GDK_GC_CLIP_Y_ORIGIN = 1 shl 12;
  GDK_GC_EXPOSURES = 1 shl 13;
  GDK_GC_LINE_WIDTH = 1 shl 14;
  GDK_GC_LINE_STYLE = 1 shl 15;
  GDK_GC_CAP_STYLE = 1 shl 16;
  GDK_GC_JOIN_STYLE = 1 shl 17;

  //  values for TGdkSubwindowMode
  GDK_CLIP_BY_CHILDREN = 0;
  GDK_INCLUDE_INFERIORS = 1;


function GDK_TYPE_GC : GType;
function GDK_GC(anObject : Pointer) : PGdkGC;
function GDK_GC_CLASS(klass : Pointer) : PGdkGCClass;
function GDK_IS_GC(anObject : Pointer) : boolean;
function GDK_IS_GC_CLASS(klass : Pointer) : boolean;
function GDK_GC_GET_CLASS(obj : Pointer) : PGdkGCClass;


function gdk_gc_get_type:GType; cdecl; external gdklib;
function gdk_gc_new(drawable:PGdkDrawable):PGdkGC; cdecl; external gdklib;
function gdk_gc_new_with_values(drawable:PGdkDrawable; values:PGdkGCValues; values_mask:TGdkGCValuesMask):PGdkGC; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_gc_ref(gc:PGdkGC):PGdkGC; cdecl; external gdklib;
procedure gdk_gc_unref(gc:PGdkGC); cdecl; external gdklib;
{$endif}

procedure gdk_gc_get_values(gc:PGdkGC; values:PGdkGCValues); cdecl; external gdklib;
procedure gdk_gc_set_values(gc:PGdkGC; values:PGdkGCValues; values_mask:TGdkGCValuesMask); cdecl; external gdklib;
procedure gdk_gc_set_foreground(gc:PGdkGC; color:PGdkColor); cdecl; external gdklib;
procedure gdk_gc_set_background(gc:PGdkGC; color:PGdkColor); cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_gc_set_font(gc:PGdkGC; font:PGdkFont); cdecl; external gdklib;
{$endif}
{ GDK_DISABLE_DEPRECATED  }

procedure gdk_gc_set_function(gc:PGdkGC; _function:TGdkFunction); cdecl; external gdklib;
procedure gdk_gc_set_fill(gc:PGdkGC; fill:TGdkFill); cdecl; external gdklib;
procedure gdk_gc_set_tile(gc:PGdkGC; tile:PGdkPixmap); cdecl; external gdklib;
procedure gdk_gc_set_stipple(gc:PGdkGC; stipple:PGdkPixmap); cdecl; external gdklib;
procedure gdk_gc_set_ts_origin(gc:PGdkGC; x:gint; y:gint); cdecl; external gdklib;
procedure gdk_gc_set_clip_origin(gc:PGdkGC; x:gint; y:gint); cdecl; external gdklib;
procedure gdk_gc_set_clip_mask(gc:PGdkGC; mask:PGdkBitmap); cdecl; external gdklib;
procedure gdk_gc_set_clip_rectangle(gc:PGdkGC; rectangle:PGdkRectangle); cdecl; external gdklib;
procedure gdk_gc_set_clip_region(gc:PGdkGC; region:PGdkRegion); cdecl; external gdklib;
procedure gdk_gc_set_subwindow(gc:PGdkGC; mode:TGdkSubwindowMode); cdecl; external gdklib;
procedure gdk_gc_set_exposures(gc:PGdkGC; exposures:gboolean); cdecl; external gdklib;
procedure gdk_gc_set_line_attributes(gc:PGdkGC; line_width:gint; line_style:TGdkLineStyle; cap_style:TGdkCapStyle; join_style:TGdkJoinStyle); cdecl; external gdklib;
procedure gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint; dash_list:array of gint8; n:gint); cdecl; external gdklib;
procedure gdk_gc_offset(gc:PGdkGC; x_offset:gint; y_offset:gint); cdecl; external gdklib;
procedure gdk_gc_copy(dst_gc:PGdkGC; src_gc:PGdkGC); cdecl; external gdklib;
procedure gdk_gc_set_colormap(gc:PGdkGC; colormap:PGdkColormap); cdecl; external gdklib;
function  gdk_gc_get_colormap(gc:PGdkGC):PGdkColormap; cdecl; external gdklib;
procedure gdk_gc_set_rgb_fg_color(gc:PGdkGC; color:PGdkColor); cdecl; external gdklib;
procedure gdk_gc_set_rgb_bg_color(gc:PGdkGC; color:PGdkColor); cdecl; external gdklib;
function  gdk_gc_get_screen(gc:PGdkGC):PGdkScreen; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_gc_destroy(gc:PGdkGC);
{$endif}
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_GC : GType;
begin
  GDK_TYPE_GC:=gdk_gc_get_type;
end;

function GDK_GC(anObject : Pointer) : PGdkGC;
begin
  GDK_GC:=PGdkGC(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_GC));
end;

function GDK_GC_CLASS(klass : Pointer) : PGdkGCClass;
begin
  GDK_GC_CLASS:=PGdkGCClass(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_GC));
end;

function GDK_IS_GC(anObject : Pointer) : boolean;
begin
  GDK_IS_GC:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_GC);
end;

function GDK_IS_GC_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_GC_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_GC);
end;

function GDK_GC_GET_CLASS(obj : Pointer) : PGdkGCClass;
begin
  GDK_GC_GET_CLASS:=PGdkGCClass(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_GC));
end;

procedure gdk_gc_destroy(gc:PGdkGC);
begin
  g_object_unref(G_OBJECT(gc));
end;
{$ENDIF}

