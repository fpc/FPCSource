// included by gdk2.pp
{$IFDEF read_forward_definitions}
{ Types of visuals.
     StaticGray:
     Grayscale:
     StaticColor:
     PseudoColor:
     TrueColor:
     DirectColor:
  }
   PGdkVisualType = ^TGdkVisualType;
   TGdkVisualType = (
     GDK_VISUAL_STATIC_GRAY,
     GDK_VISUAL_GRAYSCALE,
     GDK_VISUAL_STATIC_COLOR,
     GDK_VISUAL_PSEUDO_COLOR,
     GDK_VISUAL_TRUE_COLOR,
     GDK_VISUAL_DIRECT_COLOR
     );

{ The visual type.
     "type" is the type of visual this is (PseudoColor, TrueColor, etc).
     "depth" is the bit depth of this visual.
     "colormap_size" is the size of a colormap for this visual.
     "bits_per_rgb" is the number of significant bits per red, green and blue.
    The red, green and blue masks, shifts and precisions refer
     to value needed to calculate pixel values in TrueColor and DirectColor
     visuals. The "mask" is the significant bits within the pixel. The
     "shift" is the number of bits left we must shift a primary for it
     to be in position (according to the "mask"). "prec" refers to how
     much precision the pixel value contains for a particular primary.
  }
   PGdkVisual = ^TGdkVisual;
   TGdkVisual = record
        parent_instance : TGObject;
        TheType : TGdkVisualType;
        depth : gint;
        byte_order : TGdkByteOrder;
        colormap_size : gint;
        bits_per_rgb : gint;
        red_mask : guint32;
        red_shift : gint;
        red_prec : gint;
        green_mask : guint32;
        green_shift : gint;
        green_prec : gint;
        blue_mask : guint32;
        blue_shift : gint;
        blue_prec : gint;
        screen : PGdkScreen;
     end;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GDK_TYPE_VISUAL : GType;
function GDK_VISUAL(anObject : Pointer) : PGdkVisual;
function GDK_VISUAL_CLASS(klass : Pointer) : PGdkVisualClass;
function GDK_IS_VISUAL(anObject : Pointer) : boolean;
function GDK_IS_VISUAL_CLASS(klass : Pointer) : boolean;
function GDK_VISUAL_GET_CLASS(obj : Pointer) : PGdkVisualClass;

function gdk_visual_get_type:GType; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_visual_get_best_depth:gint; cdecl; external gdklib;
function gdk_visual_get_best_type:TGdkVisualType; cdecl; external gdklib;
function gdk_visual_get_system:PGdkVisual; cdecl; external gdklib;
function gdk_visual_get_best:PGdkVisual; cdecl; external gdklib;
function gdk_visual_get_best_with_depth(depth:gint):PGdkVisual; cdecl; external gdklib;
function gdk_visual_get_best_with_type(visual_type:TGdkVisualType):PGdkVisual; cdecl; external gdklib;
function gdk_visual_get_best_with_both(depth:gint; visual_type:TGdkVisualType):PGdkVisual; cdecl; external gdklib;
procedure gdk_query_depths(var depths:Pgint; count:Pgint); cdecl; external gdklib;
procedure gdk_query_visual_types(var visual_types:PGdkVisualType; count:Pgint); cdecl; external gdklib;
function gdk_list_visuals:PGList; cdecl; external gdklib;
{$endif}

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_visual_ref(v : PGdkVisual);
procedure gdk_visual_unref(v : PGdkVisual);
{$endif}
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_VISUAL : GType;
begin
  GDK_TYPE_VISUAL:=gdk_visual_get_type;
end;

function GDK_VISUAL(anObject : Pointer) : PGdkVisual;
begin
  GDK_VISUAL:=PGdkVisual(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_VISUAL));
end;

function GDK_VISUAL_CLASS(klass : Pointer) : PGdkVisualClass;
begin
  GDK_VISUAL_CLASS:=PGdkVisualClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                              GDK_TYPE_VISUAL));
end;

function GDK_IS_VISUAL(anObject : Pointer) : boolean;
begin
  GDK_IS_VISUAL:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_VISUAL);
end;

function GDK_IS_VISUAL_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_VISUAL_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_VISUAL);
end;

function GDK_VISUAL_GET_CLASS(obj : Pointer) : PGdkVisualClass;
begin
  GDK_VISUAL_GET_CLASS:=PGdkVisualClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                              GDK_TYPE_VISUAL));
end;

procedure gdk_visual_ref(v : PGdkVisual);
begin
  g_object_ref(v);
end;

procedure gdk_visual_unref(v : PGdkVisual);
begin
  g_object_unref(v);
end;
{$ENDIF}

