// included by pango.pp
{$IFDEF read_forward_definitions}
   PPangoGlyphString = ^TPangoGlyphString;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   { 1000ths of a device unit  }
   PPangoGlyphUnit = ^TPangoGlyphUnit;
   TPangoGlyphUnit = gint32;

   { Positioning information about a glyph }
   PPangoGlyphGeometry = ^TPangoGlyphGeometry;
   TPangoGlyphGeometry = record
        width : TPangoGlyphUnit;
        x_offset : TPangoGlyphUnit;
        y_offset : TPangoGlyphUnit;
     end;

   { Visual attributes of a glyph }
   PPangoGlyphVisAttr = ^TPangoGlyphVisAttr;
   TPangoGlyphVisAttr = record
        flag0 : word;
     end;

{ A single glyph }
   PPangoGlyphInfo = ^TPangoGlyphInfo;
   TPangoGlyphInfo = record
        glyph : TPangoGlyph;
        geometry : TPangoGlyphGeometry;
        attr : TPangoGlyphVisAttr;
     end;

{ A string of glyphs with positional information and visual attributes -
   ready for drawing
  }
{ This is a memory inefficient way of representing the information
     here - each value gives the byte index within the text
     corresponding to the glyph string of the start of the cluster to
     which the glyph belongs.
    }
{< private > }
   TPangoGlyphString = record
        num_glyphs : gint;
        glyphs : PPangoGlyphInfo;
        log_clusters : Pgint;
        space : gint;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
const
   bm_TPangoGlyphVisAttr_is_cluster_start = $1;
   bp_TPangoGlyphVisAttr_is_cluster_start = 0;

function is_cluster_start(var a : TPangoGlyphVisAttr) : guint;
procedure set_is_cluster_start(var a : TPangoGlyphVisAttr; __is_cluster_start : guint);

function PANGO_TYPE_GLYPH_STRING : GType;
function pango_glyph_string_new:PPangoGlyphString; cdecl; external pangolib;
procedure pango_glyph_string_set_size(_string:PPangoGlyphString; new_len:gint); cdecl; external pangolib;
function pango_glyph_string_get_type:GType; cdecl; external pangolib;
function pango_glyph_string_copy(_string:PPangoGlyphString):PPangoGlyphString; cdecl; external pangolib;
procedure pango_glyph_string_free(_string:PPangoGlyphString); cdecl; external pangolib;
procedure pango_glyph_string_extents(glyphs:PPangoGlyphString; font:PPangoFont; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_glyph_string_extents_range(glyphs:PPangoGlyphString; start:longint; theEnd:longint; font:PPangoFont; ink_rect:PPangoRectangle;
            logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_glyph_string_get_logical_widths(glyphs:PPangoGlyphString; text:Pchar; length:longint; embedding_level:longint; logical_widths:Plongint); cdecl; external pangolib;
procedure pango_glyph_string_index_to_x(glyphs:PPangoGlyphString; text:Pchar; length:longint; analysis:PPangoAnalysis; index:longint;
            trailing:gboolean; x_pos:Plongint); cdecl; external pangolib;
procedure pango_glyph_string_x_to_index(glyphs:PPangoGlyphString; text:Pchar; length:longint; analysis:PPangoAnalysis; x_pos:longint;
            index:Plongint; trailing:Plongint); cdecl; external pangolib;
{ Turn a string of characters into a string of glyphs
  }
procedure pango_shape(text:Pgchar; length:gint; analysis:PPangoAnalysis; glyphs:PPangoGlyphString); cdecl; external pangolib;
function pango_reorder_items(logical_items:PGList):PGList; cdecl; external pangolib;
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function is_cluster_start(var a : TPangoGlyphVisAttr) : guint;
begin
  is_cluster_start:=(a.flag0 and bm_TPangoGlyphVisAttr_is_cluster_start)
                    shr bp_TPangoGlyphVisAttr_is_cluster_start;
end;

procedure set_is_cluster_start(var a : TPangoGlyphVisAttr;
  __is_cluster_start : guint);
begin
  a.flag0:=a.flag0
          or ((__is_cluster_start shl bp_TPangoGlyphVisAttr_is_cluster_start)
          and bm_TPangoGlyphVisAttr_is_cluster_start);
end;

function PANGO_TYPE_GLYPH_STRING : GType;
begin
  PANGO_TYPE_GLYPH_STRING:=pango_glyph_string_get_type;
end;
{$ENDIF}

