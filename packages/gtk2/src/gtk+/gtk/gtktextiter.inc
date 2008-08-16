// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Possible future plans: SEARCH_CASE_INSENSITIVE, SEARCH_REGEXP  }
   PGtkTextSearchFlags = ^TGtkTextSearchFlags;
   TGtkTextSearchFlags = longint;

{ GtkTextIter is an opaque datatype; ignore all these fields.
     Initialize the iter with gtk_text_buffer_get_iter_
     functions
    }
{< private > }
{ padding  }
   PGtkTextIter = ^TGtkTextIter;
   TGtkTextIter = record
        dummy1 : gpointer;
        dummy2 : gpointer;
        dummy3 : gint;
        dummy4 : gint;
        dummy5 : gint;
        dummy6 : gint;
        dummy7 : gint;
        dummy8 : gint;
        dummy9 : gpointer;
        dummy10 : gpointer;
        dummy11 : gint;
        dummy12 : gint;
        dummy13 : gint;
        dummy14 : gpointer;
     end;

   TGtkTextCharPredicate = function (ch:gunichar; user_data:gpointer):gboolean; cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
   GTK_TEXT_SEARCH_VISIBLE_ONLY = 0;  (* values of TGtkTextSearchFlags *)
   GTK_TEXT_SEARCH_TEXT_ONLY    = 1;

{
   Iter: represents a location in the text. Becomes invalid if the
   characters/pixmaps/widgets (indexable objects) in the text buffer
   are changed.
  }
function GTK_TYPE_TEXT_ITER : GType;

{ This is primarily intended for language bindings that want to avoid
  a "buffer" argument to text insertions, deletions, etc.  }
function gtk_text_iter_get_buffer(iter:PGtkTextIter):PGtkTextBuffer; cdecl; external gtklib;
{
   Life cycle
  }
function gtk_text_iter_copy(iter:PGtkTextIter):PGtkTextIter; cdecl; external gtklib;
procedure gtk_text_iter_free(iter:PGtkTextIter); cdecl; external gtklib;
function gtk_text_iter_get_type:GType; cdecl; external gtklib;
{
   Convert to different kinds of index
  }
function gtk_text_iter_get_offset(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_line(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_line_offset(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_line_index(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_visible_line_offset(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_visible_line_index(iter:PGtkTextIter):gint; cdecl; external gtklib;
{
   "Dereference" operators
  }

function gtk_text_iter_get_char(iter:PGtkTextIter):gunichar; cdecl; external gtklib;

{ includes the 0xFFFC char for pixmaps/widgets, so char offsets
   into the returned string map properly into buffer char offsets
  }
function gtk_text_iter_get_slice(start:PGtkTextIter; theEnd:PGtkTextIter):Pgchar; cdecl; external gtklib;

{ includes only text, no 0xFFFC  }
function gtk_text_iter_get_text(start:PGtkTextIter; theEnd:PGtkTextIter):Pgchar; cdecl; external gtklib;

{ exclude invisible chars  }
function gtk_text_iter_get_visible_slice(start:PGtkTextIter; theEnd:PGtkTextIter):Pgchar; cdecl; external gtklib;
function gtk_text_iter_get_visible_text(start:PGtkTextIter; theEnd:PGtkTextIter):Pgchar; cdecl; external gtklib;
function gtk_text_iter_get_pixbuf(iter:PGtkTextIter):PGdkPixbuf; cdecl; external gtklib;
function gtk_text_iter_get_marks(iter:PGtkTextIter):PGSList; cdecl; external gtklib;
function gtk_text_iter_get_child_anchor(iter:PGtkTextIter):PGtkTextChildAnchor; cdecl; external gtklib;
{ Return list of tags toggled at this point (toggled_on determines
   whether the list is of on-toggles or off-toggles)
  }
function gtk_text_iter_get_toggled_tags(iter:PGtkTextIter; toggled_on:gboolean):PGSList; cdecl; external gtklib;
function gtk_text_iter_begins_tag(iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function gtk_text_iter_ends_tag(iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function gtk_text_iter_toggles_tag(iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function gtk_text_iter_has_tag(iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function gtk_text_iter_get_tags(iter:PGtkTextIter):PGSList; cdecl; external gtklib;
function gtk_text_iter_editable(iter:PGtkTextIter; default_setting:gboolean):gboolean; cdecl; external gtklib;
function gtk_text_iter_can_insert(iter:PGtkTextIter; default_editability:gboolean):gboolean; cdecl; external gtklib;
function gtk_text_iter_starts_word(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_ends_word(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_inside_word(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_starts_sentence(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_ends_sentence(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_inside_sentence(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_starts_line(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_ends_line(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_is_cursor_position(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_get_chars_in_line(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_bytes_in_line(iter:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_get_attributes(iter:PGtkTextIter; values:PGtkTextAttributes):gboolean; cdecl; external gtklib;
function gtk_text_iter_get_language(iter:PGtkTextIter):PPangoLanguage; cdecl; external gtklib;
function gtk_text_iter_is_end(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_is_start(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
{
   Moving around the buffer
  }
function gtk_text_iter_forward_char(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_char(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_chars(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_chars(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_line(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_line(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_lines(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_lines(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_word_end(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_word_start(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_word_ends(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_word_starts(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_sentence_end(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_sentence_start(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_sentence_ends(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_sentence_starts(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
{ cursor positions are almost equivalent to chars, but not quite;
   in some languages, you can't put the cursor between certain
   chars. Also, you can't put the cursor between \r\n at the theEnd
   of a line.
  }
function gtk_text_iter_forward_cursor_position(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_cursor_position(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_cursor_positions(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_cursor_positions(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
procedure gtk_text_iter_set_offset(iter:PGtkTextIter; char_offset:gint); cdecl; external gtklib;
procedure gtk_text_iter_set_line(iter:PGtkTextIter; line_number:gint); cdecl; external gtklib;
procedure gtk_text_iter_set_line_offset(iter:PGtkTextIter; char_on_line:gint); cdecl; external gtklib;
procedure gtk_text_iter_set_line_index(iter:PGtkTextIter; byte_on_line:gint); cdecl; external gtklib;
procedure gtk_text_iter_forward_to_end(iter:PGtkTextIter); cdecl; external gtklib;
function gtk_text_iter_forward_to_line_end(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
procedure gtk_text_iter_set_visible_line_offset(iter:PGtkTextIter; char_on_line:gint); cdecl; external gtklib;
procedure gtk_text_iter_set_visible_line_index(iter:PGtkTextIter; byte_on_line:gint); cdecl; external gtklib;
{ returns TRUE if a toggle was found; NULL for the tag pointer
   means "any tag toggle", otherwise the next toggle of the
   specified tag is located.
  }
function gtk_text_iter_forward_to_tag_toggle(iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_to_tag_toggle(iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_find_char(iter:PGtkTextIter; pred:TGtkTextCharPredicate; user_data:gpointer; limit:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_find_char(iter:PGtkTextIter; pred:TGtkTextCharPredicate; user_data:gpointer; limit:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_search(iter:PGtkTextIter; str:Pgchar; flags:TGtkTextSearchFlags; match_start:PGtkTextIter; match_end:PGtkTextIter;
           limit:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_search(iter:PGtkTextIter; str:Pgchar; flags:TGtkTextSearchFlags; match_start:PGtkTextIter; match_end:PGtkTextIter;
           limit:PGtkTextIter):gboolean; cdecl; external gtklib;
{
   Comparisons
  }
function gtk_text_iter_equal(lhs:PGtkTextIter; rhs:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_compare(lhs:PGtkTextIter; rhs:PGtkTextIter):gint; cdecl; external gtklib;
function gtk_text_iter_in_range(iter:PGtkTextIter; start:PGtkTextIter; theEnd:PGtkTextIter):gboolean; cdecl; external gtklib;
{ Put these two in ascending order  }
procedure gtk_text_iter_order(first:PGtkTextIter; second:PGtkTextIter); cdecl; external gtklib;

{$ifdef HasGTK2_4}
function gtk_text_iter_backward_visible_cursor_position(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_visible_cursor_positions(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_visible_word_start(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_backward_visible_word_starts(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_visible_cursor_position(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_visible_cursor_positions(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_visible_word_end(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_iter_forward_visible_word_ends(iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_ITER : GType;
begin
   GTK_TYPE_TEXT_ITER:=gtk_text_iter_get_type;
end;

{$ENDIF read_implementation}
// included by gtk2.pas

