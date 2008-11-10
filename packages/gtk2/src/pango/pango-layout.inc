// included by pango.pp
{$IFDEF read_interface_types}
   PPangoAlignment = ^TPangoAlignment;
   TPangoAlignment = (
     PANGO_ALIGN_LEFT,
     PANGO_ALIGN_CENTER,
     PANGO_ALIGN_RIGHT
   );

   PPangoWrapMode = ^TPangoWrapMode;
   TPangoWrapMode = (
     PANGO_WRAP_WORD,
     PANGO_WRAP_CHAR,
     PANGO_WRAP_WORD_CHAR 
   );

   PPangoEllipsizeMode = ^TPangoEllipsizeMode;
   TPangoEllipsizeMode = (
     PANGO_ELLIPSIZE_NONE,
     PANGO_ELLIPSIZE_START,
     PANGO_ELLIPSIZE_MIDDLE,
     PANGO_ELLIPSIZE_END
   );

   { start of line as byte index into layout->text  }
   { length of line in bytes  }
   PPangoLayoutLine = ^TPangoLayoutLine;
   TPangoLayoutLine = record
        layout : PPangoLayout;
        start_index : gint;
        length : gint;
        runs : PGSList;
     end;

   PPangoLayoutRun = ^TPangoLayoutRun;
   TPangoLayoutRun = record
        item : PPangoItem;
        glyphs : PPangoGlyphString;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function PANGO_TYPE_LAYOUT : GType;
function PANGO_LAYOUT(anObject : pointer) : PPangoLayout;
function PANGO_LAYOUT_CLASS(klass : pointer) : PPangoLayoutClass;
function PANGO_IS_LAYOUT(anObject : pointer) : boolean;
function PANGO_IS_LAYOUT_CLASS(klass : pointer) : boolean;
function PANGO_LAYOUT_GET_CLASS(obj : PPangoLayout) : PPangoLayoutClass;

{ The PangoLayout and PangoLayoutClass structs are private; if you
   need to create a subclass of these, mail otaylor@redhat.com
  }
function pango_layout_get_type:GType; cdecl; external pangolib;
function pango_layout_new(context:PPangoContext):PPangoLayout; cdecl; external pangolib;
function pango_layout_copy(src:PPangoLayout):PPangoLayout; cdecl; external pangolib;
function pango_layout_get_context(layout:PPangoLayout):PPangoContext; cdecl; external pangolib;
procedure pango_layout_set_attributes(layout:PPangoLayout; attrs:PPangoAttrList); cdecl; external pangolib;
function pango_layout_get_attributes(layout:PPangoLayout):PPangoAttrList; cdecl; external pangolib;
procedure pango_layout_set_text(layout:PPangoLayout; text:Pchar; length:longint); cdecl; external pangolib;
function pango_layout_get_text(layout:PPangoLayout):Pchar; cdecl; external pangolib;
procedure pango_layout_set_markup(layout:PPangoLayout; markup:Pchar; length:longint); cdecl; external pangolib;
procedure pango_layout_set_markup_with_accel(layout:PPangoLayout; markup:Pchar; length:longint; accel_marker:gunichar; accel_char:Pgunichar); cdecl; external pangolib;
function pango_layout_get_font_description(layout:PPangoLayout):PPangoFontDescription; cdecl; external pangolib;
procedure pango_layout_set_font_description(layout:PPangoLayout; desc:PPangoFontDescription); cdecl; external pangolib;
procedure pango_layout_set_width(layout:PPangoLayout; width:longint); cdecl; external pangolib;
function pango_layout_get_width(layout:PPangoLayout):longint; cdecl; external pangolib;
procedure pango_layout_set_wrap(layout:PPangoLayout; wrap:TPangoWrapMode); cdecl; external pangolib;
function pango_layout_get_wrap(layout:PPangoLayout):TPangoWrapMode; cdecl; external pangolib;
procedure pango_layout_set_indent(layout:PPangoLayout; indent:longint); cdecl; external pangolib;
function pango_layout_get_indent(layout:PPangoLayout):longint; cdecl; external pangolib;
procedure pango_layout_set_spacing(layout:PPangoLayout; spacing:longint); cdecl; external pangolib;
function pango_layout_get_spacing(layout:PPangoLayout):longint; cdecl; external pangolib;
procedure pango_layout_set_justify(layout:PPangoLayout; justify:gboolean); cdecl; external pangolib;
function pango_layout_get_justify(layout:PPangoLayout):gboolean; cdecl; external pangolib;
procedure pango_layout_set_alignment(layout:PPangoLayout; alignment:TPangoAlignment); cdecl; external pangolib;
function pango_layout_get_alignment(layout:PPangoLayout):TPangoAlignment; cdecl; external pangolib;
procedure pango_layout_set_tabs(layout:PPangoLayout; tabs:PPangoTabArray); cdecl; external pangolib;
function pango_layout_get_tabs(layout:PPangoLayout):PPangoTabArray; cdecl; external pangolib;
procedure pango_layout_set_single_paragraph_mode(layout:PPangoLayout; setting:gboolean); cdecl; external pangolib;
function pango_layout_get_single_paragraph_mode(layout:PPangoLayout):gboolean; cdecl; external pangolib;
procedure pango_layout_context_changed(layout:PPangoLayout); cdecl; external pangolib;
procedure pango_layout_get_log_attrs(layout:PPangoLayout; var attrs:PPangoLogAttr; n_attrs:Pgint); cdecl; external pangolib;
procedure pango_layout_index_to_pos(layout:PPangoLayout; index:longint; pos:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_get_cursor_pos(layout:PPangoLayout; index:longint; strong_pos:PPangoRectangle; weak_pos:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_move_cursor_visually(layout:PPangoLayout; strong:gboolean; old_index:longint; old_trailing:longint; direction:longint;
            new_index:Plongint; new_trailing:Plongint); cdecl; external pangolib;
function pango_layout_xy_to_index(layout:PPangoLayout; x:longint; y:longint; index:Plongint; trailing:Plongint):gboolean; cdecl; external pangolib;
procedure pango_layout_get_extents(layout:PPangoLayout; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_get_pixel_extents(layout:PPangoLayout; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_get_size(layout:PPangoLayout; width:Plongint; height:Plongint); cdecl; external pangolib;
procedure pango_layout_get_pixel_size(layout:PPangoLayout; width:Plongint; height:Plongint); cdecl; external pangolib;
function pango_layout_get_line_count(layout:PPangoLayout):longint; cdecl; external pangolib;
function pango_layout_get_line(layout:PPangoLayout; line:longint):PPangoLayoutLine; cdecl; external pangolib;
function pango_layout_get_lines(layout:PPangoLayout):PGSList; cdecl; external pangolib;
procedure pango_layout_line_ref(line:PPangoLayoutLine); cdecl; external pangolib;
procedure pango_layout_line_unref(line:PPangoLayoutLine); cdecl; external pangolib;
function pango_layout_line_x_to_index(line:PPangoLayoutLine; x_pos:longint; index:Plongint; trailing:Plongint):gboolean; cdecl; external pangolib;
procedure pango_layout_line_index_to_x(line:PPangoLayoutLine; index:longint; trailing:gboolean; x_pos:Plongint); cdecl; external pangolib;
procedure pango_layout_line_get_x_ranges(line:PPangoLayoutLine; start_index:longint; end_index:longint; var ranges:Plongint; n_ranges:Plongint); cdecl; external pangolib;
procedure pango_layout_line_get_extents(line:PPangoLayoutLine; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_line_get_pixel_extents(layout_line:PPangoLayoutLine; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;

function pango_layout_get_iter(layout:PPangoLayout):PPangoLayoutIter; cdecl; external pangolib;
procedure pango_layout_iter_free(iter:PPangoLayoutIter); cdecl; external pangolib;
function pango_layout_iter_get_index(iter:PPangoLayoutIter):longint; cdecl; external pangolib;
function pango_layout_iter_get_run(iter:PPangoLayoutIter):PPangoLayoutRun; cdecl; external pangolib;
function pango_layout_iter_get_line(iter:PPangoLayoutIter):PPangoLayoutLine; cdecl; external pangolib;
function pango_layout_iter_at_last_line(iter:PPangoLayoutIter):gboolean; cdecl; external pangolib;
function pango_layout_iter_next_char(iter:PPangoLayoutIter):gboolean; cdecl; external pangolib;
function pango_layout_iter_next_cluster(iter:PPangoLayoutIter):gboolean; cdecl; external pangolib;
function pango_layout_iter_next_run(iter:PPangoLayoutIter):gboolean; cdecl; external pangolib;
function pango_layout_iter_next_line(iter:PPangoLayoutIter):gboolean; cdecl; external pangolib;
procedure pango_layout_iter_get_char_extents(iter:PPangoLayoutIter; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_iter_get_cluster_extents(iter:PPangoLayoutIter; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_iter_get_run_extents(iter:PPangoLayoutIter; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
procedure pango_layout_iter_get_line_extents(iter:PPangoLayoutIter; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;

{ All the yranges meet, unlike the logical_rect's (i.e. the yranges
   assign between-line spacing to the nearest line)
  }
procedure pango_layout_iter_get_line_yrange(iter:PPangoLayoutIter; y0:Plongint; y1:Plongint); cdecl; external pangolib;
procedure pango_layout_iter_get_layout_extents(iter:PPangoLayoutIter; ink_rect:PPangoRectangle; logical_rect:PPangoRectangle); cdecl; external pangolib;
function pango_layout_iter_get_baseline(iter:PPangoLayoutIter):longint; cdecl; external pangolib;
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function PANGO_TYPE_LAYOUT : GType;
begin
  PANGO_TYPE_LAYOUT:=pango_layout_get_type;
end;

function PANGO_LAYOUT(anObject : pointer) : PPangoLayout;
begin
  PANGO_LAYOUT:=PPangoLayout(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                        PANGO_TYPE_LAYOUT));
end;

function PANGO_LAYOUT_CLASS(klass : pointer) : PPangoLayoutClass;
begin
  PANGO_LAYOUT_CLASS:=PPangoLayoutClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                            PANGO_TYPE_LAYOUT));
end;

function PANGO_IS_LAYOUT(anObject : pointer) : boolean;
begin
  PANGO_IS_LAYOUT:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,PANGO_TYPE_LAYOUT);
end;

function PANGO_IS_LAYOUT_CLASS(klass : pointer) : boolean;
begin
  PANGO_IS_LAYOUT_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,PANGO_TYPE_LAYOUT);
end;

function PANGO_LAYOUT_GET_CLASS(obj : PPangoLayout) : PPangoLayoutClass;
begin
  PANGO_LAYOUT_GET_CLASS:=PPangoLayoutClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                            PANGO_TYPE_LAYOUT));
end;
{$ENDIF}

