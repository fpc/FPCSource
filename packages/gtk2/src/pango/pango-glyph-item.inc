// included by pango.pp
{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

PPangoGlyphItem = ^TPangoGlyphItem;
TPangoGlyphItem = record
  item : PPangoItem;
  glyphs : PPangoGlyphString;
end;
PPangoGlyphItemIter = ^TPangoGlyphItemIter;
TPangoGlyphItemIter = record
  glyph_item : PPangoGlyphItem;
  text : Pgchar;
  start_glyph : longint;
  start_index : longint;
  start_char : longint;
  end_glyph : longint;
  end_index : longint;
  end_char : longint;
end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}

function PANGO_TYPE_GLYPH_ITEM : GType;
  
function pango_glyph_item_get_type():GType;cdecl;external pangolib;
function pango_glyph_item_split(orig:PPangoGlyphItem; text:Pchar; split_index:longint):PPangoGlyphItem;cdecl;external pangolib;
function pango_glyph_item_copy(orig:PPangoGlyphItem):PPangoGlyphItem;cdecl;external pangolib;
procedure pango_glyph_item_free(glyph_item:PPangoGlyphItem);cdecl;external pangolib;
function pango_glyph_item_apply_attrs(glyph_item:PPangoGlyphItem; text:Pchar; list:PPangoAttrList):PGSList;cdecl;external pangolib;
procedure pango_glyph_item_letter_space(glyph_item:PPangoGlyphItem; text:Pchar; log_attrs:PPangoLogAttr; letter_spacing:longint);cdecl;external pangolib;

function PANGO_TYPE_GLYPH_ITEM_ITER : GType;
function pango_glyph_item_iter_get_type():GType;cdecl;external pangolib;
function pango_glyph_item_iter_copy(orig:PPangoGlyphItemIter):PPangoGlyphItemIter;cdecl;external pangolib;
procedure pango_glyph_item_iter_free(iter:PPangoGlyphItemIter);cdecl;external pangolib;
function pango_glyph_item_iter_init_start(iter:PPangoGlyphItemIter; glyph_item:PPangoGlyphItem; text:Pchar):gboolean;cdecl;external pangolib;
function pango_glyph_item_iter_init_end(iter:PPangoGlyphItemIter; glyph_item:PPangoGlyphItem; text:Pchar):gboolean;cdecl;external pangolib;
function pango_glyph_item_iter_next_cluster(iter:PPangoGlyphItemIter):gboolean;cdecl;external pangolib;
function pango_glyph_item_iter_prev_cluster(iter:PPangoGlyphItemIter):gboolean;cdecl;external pangolib;

{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

function PANGO_TYPE_GLYPH_ITEM : GType;
begin
  PANGO_TYPE_GLYPH_ITEM:=pango_glyph_item_get_type;
end;

function PANGO_TYPE_GLYPH_ITEM_ITER : GType;
begin
  PANGO_TYPE_GLYPH_ITEM_ITER:=pango_glyph_item_iter_get_type;
end;

{$ENDIF}
