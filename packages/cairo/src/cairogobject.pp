unit cairogobject;
{
  Cairo-gobject header translation by Dennis Golovan. Proper header to be inserted here.
}
interface
{$mode ObjFpc}
{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

uses cairo;

const 
{$ifdef MSWINDOWS}
  LIB_CAIROGOBJECT = LIB_CAIRO;
  {$IFDEF FPC}
    {$ifndef NO_SMART_LINK}
      {$smartlink on}
    {$endif}
  {$ENDIF}
{$else}
  {$ifdef darwin}
    LIB_CAIROGOBJECT = 'cairo-gobject';
    {$linklib cairo-gobject}
  {$else}
    {$ifdef UseCustomLibs}
    LIB_CAIROGOBJECT = '';
    {$else}
    LIB_CAIROGOBJECT = 'libcairo-gobject.so.2';
    {$endif}
  {$endif}
{$endif}

(* GObject Functions - structs *)

function CAIRO_GOBJECT_TYPE_CONTEXT: GType;
function cairo_gobject_context_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_DEVICE: GType;
function cairo_gobject_device_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_PATTERN: GType;
function cairo_gobject_pattern_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_SURFACE: GType;
function cairo_gobject_surface_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_RECTANGLE: GType;
function cairo_gobject_rectangle_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_SCALED_FONT: GType;
function cairo_gobject_scaled_font_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FONT_FACE: GType;
function cairo_gobject_font_face_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FONT_OPTIONS: GType;
function cairo_gobject_font_options_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_RECTANGLE_INT: GType;
function cairo_gobject_rectangle_int_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_REGION: GType;
function cairo_gobject_region_get_type: GType; cdecl; external LIB_CAIROGOBJECT;

(* GObject Functions - enums *)

function CAIRO_GOBJECT_TYPE_STATUS: GType;
function cairo_gobject_status_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_CONTENT: GType;
function cairo_gobject_content_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_OPERATOR: GType;
function cairo_gobject_operator_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_ANTIALIAS: GType;
function cairo_gobject_antialias_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FILL_RULE: GType;
function cairo_gobject_fill_rule_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_LINE_CAP: GType;
function cairo_gobject_line_cap_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_LINE_JOIN: GType;
function cairo_gobject_line_join_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_CLUSTER_FLAGS: GType;
function cairo_gobject_text_cluster_flags_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FONT_SLANT: GType;
function cairo_gobject_font_slant_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FONT_WEIGHT: GType;
function cairo_gobject_font_weight_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_SUBPIXEL_ORDER: GType;
function cairo_gobject_subpixel_order_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_HINT_STYLE: GType;
function cairo_gobject_hint_style_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_HNT_METRICS: GType;
function cairo_gobject_hint_metrics_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FONT_TYPE: GType;
function cairo_gobject_font_type_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_PATH_DATA_TYPE: GType;
function cairo_gobject_path_data_type_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_DEVICE_TYPE: GType;
function cairo_gobject_device_type_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_SURFACE_TYPE: GType;
function cairo_gobject_surface_type_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FORMAT: GType;
function cairo_gobject_format_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_PATTERN_TYPE: GType;
function cairo_gobject_pattern_type_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_EXTEND: GType;
function cairo_gobject_extend_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_FILTER: GType;
function cairo_gobject_filter_get_type: GType; cdecl; external LIB_CAIROGOBJECT;
function CAIRO_GOBJECT_TYPE_REGION_OVERLAP: GType;
function cairo_gobject_region_overlap_get_type: GType; cdecl; external LIB_CAIROGOBJECT;

implementation

function CAIRO_GOBJECT_TYPE_CONTEXT: GType;
begin
  Result := cairo_gobject_context_get_type;
end;

function CAIRO_GOBJECT_TYPE_DEVICE: GType;
begin
  Result :=  cairo_gobject_device_get_type;
end;

function CAIRO_GOBJECT_TYPE_PATTERN: GType;
begin
  Result := cairo_gobject_pattern_get_type;
end;

function CAIRO_GOBJECT_TYPE_SURFACE: GType;
begin
  Result := cairo_gobject_surface_get_type;
end;

function CAIRO_GOBJECT_TYPE_RECTANGLE: GType;
begin
  Result := cairo_gobject_rectangle_get_type;
end;

function CAIRO_GOBJECT_TYPE_SCALED_FONT: GType;
begin
  Result := cairo_gobject_scaled_font_get_type;
end;

function CAIRO_GOBJECT_TYPE_FONT_FACE: GType;
begin
  Result := cairo_gobject_font_face_get_type;
end;

function CAIRO_GOBJECT_TYPE_FONT_OPTIONS: GType;
begin
  Result := cairo_gobject_font_options_get_type;
end;

function CAIRO_GOBJECT_TYPE_RECTANGLE_INT: GType;
begin
  Result := cairo_gobject_rectangle_int_get_type;
end;

function CAIRO_GOBJECT_TYPE_REGION: GType;
begin
  Result := cairo_gobject_region_get_type;
end;

function CAIRO_GOBJECT_TYPE_STATUS: GType;
begin
  Result := cairo_gobject_status_get_type;
end;

function CAIRO_GOBJECT_TYPE_CONTENT: GType;
begin
  Result := cairo_gobject_content_get_type;
end;

function CAIRO_GOBJECT_TYPE_OPERATOR: GType;
begin
  Result := cairo_gobject_operator_get_type;
end;

function CAIRO_GOBJECT_TYPE_ANTIALIAS: GType;
begin
  Result := cairo_gobject_antialias_get_type;
end;

function CAIRO_GOBJECT_TYPE_FILL_RULE: GType;
begin
  Result := cairo_gobject_fill_rule_get_type;
end;

function CAIRO_GOBJECT_TYPE_LINE_CAP: GType;
begin
  Result := cairo_gobject_line_cap_get_type;
end;

function CAIRO_GOBJECT_TYPE_LINE_JOIN: GType;
begin
  Result := cairo_gobject_line_join_get_type;
end;

function CAIRO_GOBJECT_TYPE_CLUSTER_FLAGS: GType;
begin
  Result := cairo_gobject_text_cluster_flags_get_type;
end;

function CAIRO_GOBJECT_TYPE_FONT_SLANT: GType;
begin
  Result := cairo_gobject_font_slant_get_type;
end;

function CAIRO_GOBJECT_TYPE_FONT_WEIGHT: GType;
begin
  Result := cairo_gobject_font_weight_get_type;
end;

function CAIRO_GOBJECT_TYPE_SUBPIXEL_ORDER: GType;
begin
  Result := cairo_gobject_subpixel_order_get_type;
end;

function CAIRO_GOBJECT_TYPE_HINT_STYLE: GType;
begin
  Result := cairo_gobject_hint_style_get_type;
end;

function CAIRO_GOBJECT_TYPE_HNT_METRICS: GType;
begin
  Result := cairo_gobject_hint_metrics_get_type;
end;

function CAIRO_GOBJECT_TYPE_FONT_TYPE: GType;
begin
  Result := cairo_gobject_font_type_get_type;
end;

function CAIRO_GOBJECT_TYPE_PATH_DATA_TYPE: GType;
begin
  Result := cairo_gobject_path_data_type_get_type;
end;

function CAIRO_GOBJECT_TYPE_DEVICE_TYPE: GType;
begin
  Result := cairo_gobject_device_type_get_type;
end;

function CAIRO_GOBJECT_TYPE_SURFACE_TYPE: GType;
begin
  Result := cairo_gobject_surface_type_get_type;
end;

function CAIRO_GOBJECT_TYPE_FORMAT: GType;
begin
  Result := cairo_gobject_format_get_type;
end;

function CAIRO_GOBJECT_TYPE_PATTERN_TYPE: GType;
begin
  Result := cairo_gobject_pattern_type_get_type;
end;

function CAIRO_GOBJECT_TYPE_EXTEND: GType;
begin
  Result := cairo_gobject_extend_get_type;
end;

function CAIRO_GOBJECT_TYPE_FILTER: GType;
begin
  Result := cairo_gobject_filter_get_type;
end;

function CAIRO_GOBJECT_TYPE_REGION_OVERLAP: GType;
begin
  Result := cairo_gobject_region_overlap_get_type;
end;

end.