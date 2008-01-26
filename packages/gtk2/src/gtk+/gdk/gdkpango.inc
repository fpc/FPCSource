// included by gdk2.pp

{$IFDEF read_interface_types}
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
{$ENDIF}
