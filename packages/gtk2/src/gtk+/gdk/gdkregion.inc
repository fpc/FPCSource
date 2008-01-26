// included by gdk2.pp

{$IFDEF read_interface_types}
{ GC fill rule for polygons
    EvenOddRule
    WindingRule
  }
   PGdkFillRule = ^TGdkFillRule;
   TGdkFillRule = (
     GDK_EVEN_ODD_RULE,
     GDK_WINDING_RULE
   );

{ Types of overlapping between a rectangle and a region
   GDK_OVERLAP_RECTANGLE_IN: rectangle is in region
   GDK_OVERLAP_RECTANGLE_OUT: rectangle in not in region
   GDK_OVERLAP_RECTANGLE_PART: rectangle in partially in region
  }
   PGdkOverlapType = ^TGdkOverlapType;
   TGdkOverlapType = (
     GDK_OVERLAP_RECTANGLE_IN,
     GDK_OVERLAP_RECTANGLE_OUT,
     GDK_OVERLAP_RECTANGLE_PART
   );

   TGdkSpanFunc = procedure (span:PGdkSpan; data:gpointer); cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function gdk_region_new:PGdkRegion; cdecl; external gdklib;
function gdk_region_polygon(points:PGdkPoint; npoints:gint; fill_rule:TGdkFillRule):PGdkRegion; cdecl; external gdklib;
function gdk_region_copy(region:PGdkRegion):PGdkRegion; cdecl; external gdklib;
function gdk_region_rectangle(rectangle:PGdkRectangle):PGdkRegion; cdecl; external gdklib;
procedure gdk_region_destroy(region:PGdkRegion); cdecl; external gdklib;
procedure gdk_region_get_clipbox(region:PGdkRegion; rectangle:PGdkRectangle); cdecl; external gdklib;
procedure gdk_region_get_rectangles(region:PGdkRegion; var rectangles:PGdkRectangle; n_rectangles:Pgint); cdecl; external gdklib;
function gdk_region_empty(region:PGdkRegion):gboolean; cdecl; external gdklib;
function gdk_region_equal(region1:PGdkRegion; region2:PGdkRegion):gboolean; cdecl; external gdklib;
function gdk_region_point_in(region:PGdkRegion; x:longint; y:longint):gboolean; cdecl; external gdklib;
function gdk_region_rect_in(region:PGdkRegion; rect:PGdkRectangle):TGdkOverlapType; cdecl; external gdklib;
procedure gdk_region_offset(region:PGdkRegion; dx:gint; dy:gint); cdecl; external gdklib;
procedure gdk_region_shrink(region:PGdkRegion; dx:gint; dy:gint); cdecl; external gdklib;
procedure gdk_region_union_with_rect(region:PGdkRegion; rect:PGdkRectangle); cdecl; external gdklib;
procedure gdk_region_intersect(source1:PGdkRegion; source2:PGdkRegion); cdecl; external gdklib;
procedure gdk_region_union(source1:PGdkRegion; source2:PGdkRegion); cdecl; external gdklib;
procedure gdk_region_subtract(source1:PGdkRegion; source2:PGdkRegion); cdecl; external gdklib;
procedure gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion); cdecl; external gdklib;
procedure gdk_region_spans_intersect_foreach(region:PGdkRegion; spans:PGdkSpan; n_spans:longint; sorted:gboolean; _function:TGdkSpanFunc;
            data:gpointer); cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF}

