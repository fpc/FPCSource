// included by gtk2.pas

{$IFDEF read_forward_definitions}
   // internal type
   PGtkTextBTreeNode = pointer;

   // internal type
   PGtkTextBTree = pointer;

   PGtkTextLine = ^TGtkTextLine;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

{ Lines  }
{ Chunk of data associated with a line; views can use this to store
   info at the line. They should "subclass" the header struct here.  }
   PGtkTextLineData = ^TGtkTextLineData;
   TGtkTextLineData = record
        view_id : gpointer;
        next : PGtkTextLineData;
        height : gint;
        flag0 : longint;
     end;

{
   The data structure below defines a single line of text (from newline
   to newline, not necessarily what appears on one line of the screen).

   You can consider this line a "paragraph" also
  }
{ Pointer to parent node containing
                                           line.  }
{ Next in linked list of lines with
                                   same parent node in B-tree.  NULL
                                   means theEnd of list.  }
{ First in ordered list of segments
                                   that make up the line.  }
{ data stored here by views  }
   TGtkTextLine = record
        parent : PGtkTextBTreeNode;
        next : PGtkTextLine;
        segments : PGtkTextLineSegment;
        views : PGtkTextLineData;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function _gtk_text_btree_new(table:PGtkTextTagTable; buffer:PGtkTextBuffer):PGtkTextBTree; cdecl; external gtklib;
procedure _gtk_text_btree_ref(tree:PGtkTextBTree); cdecl; external gtklib;
procedure _gtk_text_btree_unref(tree:PGtkTextBTree); cdecl; external gtklib;
function _gtk_text_btree_get_buffer(tree:PGtkTextBTree):PGtkTextBuffer; cdecl; external gtklib;
function _gtk_text_btree_get_chars_changed_stamp(tree:PGtkTextBTree):guint; cdecl; external gtklib;
function _gtk_text_btree_get_segments_changed_stamp(tree:PGtkTextBTree):guint; cdecl; external gtklib;
procedure _gtk_text_btree_segments_changed(tree:PGtkTextBTree); cdecl; external gtklib;
function _gtk_text_btree_is_end(tree:PGtkTextBTree; line:PGtkTextLine; seg:PGtkTextLineSegment; byte_index:longint; char_offset:longint):gboolean; cdecl; external gtklib;

{ Indexable segment mutation  }
procedure _gtk_text_btree_delete(start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;

procedure _gtk_text_btree_insert(iter:PGtkTextIter; text:Pgchar; len:gint); cdecl; external gtklib;
procedure _gtk_text_btree_insert_pixbuf(iter:PGtkTextIter; pixbuf:PGdkPixbuf); cdecl; external gtklib;
procedure _gtk_text_btree_insert_child_anchor(iter:PGtkTextIter; anchor:PGtkTextChildAnchor); cdecl; external gtklib;
procedure _gtk_text_btree_unregister_child_anchor(anchor:PGtkTextChildAnchor); cdecl; external gtklib;

{ View stuff  }
function _gtk_text_btree_find_line_by_y(tree:PGtkTextBTree; view_id:gpointer; ypixel:gint; line_top_y:Pgint):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_btree_find_line_top(tree:PGtkTextBTree; line:PGtkTextLine; view_id:gpointer):gint; cdecl; external gtklib;
procedure _gtk_text_btree_add_view(tree:PGtkTextBTree; layout:PGtkTextLayout); cdecl; external gtklib;
procedure _gtk_text_btree_remove_view(tree:PGtkTextBTree; view_id:gpointer); cdecl; external gtklib;

procedure _gtk_text_btree_invalidate_region(tree:PGtkTextBTree; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
procedure _gtk_text_btree_get_view_size(tree:PGtkTextBTree; view_id:gpointer; width:Pgint; height:Pgint); cdecl; external gtklib;
function _gtk_text_btree_is_valid(tree:PGtkTextBTree; view_id:gpointer):gboolean; cdecl; external gtklib;
function _gtk_text_btree_validate(tree:PGtkTextBTree; view_id:gpointer; max_pixels:gint; y:Pgint; old_height:Pgint;
           new_height:Pgint):gboolean; cdecl; external gtklib;
procedure _gtk_text_btree_validate_line(tree:PGtkTextBTree; line:PGtkTextLine; view_id:gpointer); cdecl; external gtklib;
{ Tag  }
procedure _gtk_text_btree_tag(start:PGtkTextIter; theEnd:PGtkTextIter; tag:PGtkTextTag; apply:gboolean); cdecl; external gtklib;
{ "Getters"  }
function _gtk_text_btree_get_line(tree:PGtkTextBTree; line_number:gint; real_line_number:Pgint):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_btree_get_line_no_last(tree:PGtkTextBTree; line_number:gint; real_line_number:Pgint):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_btree_get_end_iter_line(tree:PGtkTextBTree):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_btree_get_line_at_char(tree:PGtkTextBTree; char_index:gint; line_start_index:Pgint; real_char_index:Pgint):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_btree_get_tags(iter:PGtkTextIter; num_tags:Pgint):PPGtkTextTag; cdecl; external gtklib;
function _gtk_text_btree_get_text(start:PGtkTextIter; theEnd:PGtkTextIter; include_hidden:gboolean; include_nonchars:gboolean):Pgchar; cdecl; external gtklib;
function _gtk_text_btree_line_count(tree:PGtkTextBTree):gint; cdecl; external gtklib;
function _gtk_text_btree_char_count(tree:PGtkTextBTree):gint; cdecl; external gtklib;

function _gtk_text_btree_char_is_invisible(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
{ Get iterators (these are implemented in gtktextiter.c)  }
procedure _gtk_text_btree_get_iter_at_char(tree:PGtkTextBTree; iter:PGtkTextIter; char_index:gint); cdecl; external gtklib;
procedure _gtk_text_btree_get_iter_at_line_char(tree:PGtkTextBTree; iter:PGtkTextIter; line_number:gint; char_index:gint); cdecl; external gtklib;
procedure _gtk_text_btree_get_iter_at_line_byte(tree:PGtkTextBTree; iter:PGtkTextIter; line_number:gint; byte_index:gint); cdecl; external gtklib;

function _gtk_text_btree_get_iter_from_string(tree:PGtkTextBTree; iter:PGtkTextIter; _string:Pgchar):gboolean; cdecl; external gtklib;

function _gtk_text_btree_get_iter_at_mark_name(tree:PGtkTextBTree; iter:PGtkTextIter; mark_name:Pgchar):gboolean; cdecl; external gtklib;
procedure _gtk_text_btree_get_iter_at_mark(tree:PGtkTextBTree; iter:PGtkTextIter; mark:PGtkTextMark); cdecl; external gtklib;
procedure _gtk_text_btree_get_end_iter(tree:PGtkTextBTree; iter:PGtkTextIter); cdecl; external gtklib;
procedure _gtk_text_btree_get_iter_at_line(tree:PGtkTextBTree; iter:PGtkTextIter; line:PGtkTextLine; byte_offset:gint); cdecl; external gtklib;
function _gtk_text_btree_get_iter_at_first_toggle(tree:PGtkTextBTree; iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function _gtk_text_btree_get_iter_at_last_toggle(tree:PGtkTextBTree; iter:PGtkTextIter; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
procedure _gtk_text_btree_get_iter_at_child_anchor(tree:PGtkTextBTree; iter:PGtkTextIter; anchor:PGtkTextChildAnchor); cdecl; external gtklib;
{ Manipulate marks  }
function _gtk_text_btree_set_mark(tree:PGtkTextBTree; existing_mark:PGtkTextMark; name:Pgchar; left_gravity:gboolean; index:PGtkTextIter;
           should_exist:gboolean):PGtkTextMark; cdecl; external gtklib;

procedure _gtk_text_btree_remove_mark_by_name(tree:PGtkTextBTree; name:Pgchar); cdecl; external gtklib;
procedure _gtk_text_btree_remove_mark(tree:PGtkTextBTree; segment:PGtkTextMark); cdecl; external gtklib;
function _gtk_text_btree_get_selection_bounds(tree:PGtkTextBTree; start:PGtkTextIter; theEnd:PGtkTextIter):gboolean; cdecl; external gtklib;

procedure _gtk_text_btree_place_cursor(tree:PGtkTextBTree; where:PGtkTextIter); cdecl; external gtklib;
function _gtk_text_btree_mark_is_insert(tree:PGtkTextBTree; segment:PGtkTextMark):gboolean; cdecl; external gtklib;
function _gtk_text_btree_mark_is_selection_bound(tree:PGtkTextBTree; segment:PGtkTextMark):gboolean; cdecl; external gtklib;

function _gtk_text_btree_get_mark_by_name(tree:PGtkTextBTree; name:Pgchar):PGtkTextMark; cdecl; external gtklib;
function _gtk_text_btree_first_could_contain_tag(tree:PGtkTextBTree; tag:PGtkTextTag):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_btree_last_could_contain_tag(tree:PGtkTextBTree; tag:PGtkTextTag):PGtkTextLine; cdecl; external gtklib;

const
   bm_TGtkTextLineData_width = $FFFFFF;
   bp_TGtkTextLineData_width = 0;
   bm_TGtkTextLineData_valid = $FF000000;
   bp_TGtkTextLineData_valid = 24;

function width(a : PGtkTextLineData) : gint;
procedure set_width(a : PGtkTextLineData; NewWidth : gint);
function valid(a : PGtkTextLineData) : gint;
procedure set_valid(a : PGtkTextLineData; __valid : gint);

function _gtk_text_line_get_number(line:PGtkTextLine):gint; cdecl; external gtklib;
function _gtk_text_line_char_has_tag(line:PGtkTextLine; tree:PGtkTextBTree; char_in_line:gint; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function _gtk_text_line_byte_has_tag(line:PGtkTextLine; tree:PGtkTextBTree; byte_in_line:gint; tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function _gtk_text_line_is_last(line:PGtkTextLine; tree:PGtkTextBTree):gboolean; cdecl; external gtklib;
function _gtk_text_line_contains_end_iter(line:PGtkTextLine; tree:PGtkTextBTree):gboolean; cdecl; external gtklib;
function _gtk_text_line_next(line:PGtkTextLine):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_line_next_excluding_last(line:PGtkTextLine):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_line_previous(line:PGtkTextLine):PGtkTextLine; cdecl; external gtklib;
procedure _gtk_text_line_add_data(line:PGtkTextLine; data:PGtkTextLineData); cdecl; external gtklib;
function _gtk_text_line_remove_data(line:PGtkTextLine; view_id:gpointer):gpointer; cdecl; external gtklib;
function _gtk_text_line_get_data(line:PGtkTextLine; view_id:gpointer):gpointer; cdecl; external gtklib;
procedure _gtk_text_line_invalidate_wrap(line:PGtkTextLine; ld:PGtkTextLineData); cdecl; external gtklib;
function _gtk_text_line_char_count(line:PGtkTextLine):gint; cdecl; external gtklib;
function _gtk_text_line_byte_count(line:PGtkTextLine):gint; cdecl; external gtklib;
function _gtk_text_line_char_index(line:PGtkTextLine):gint; cdecl; external gtklib;
function _gtk_text_line_byte_to_segment(line:PGtkTextLine; byte_offset:gint; seg_offset:Pgint):PGtkTextLineSegment; cdecl; external gtklib;
function _gtk_text_line_char_to_segment(line:PGtkTextLine; char_offset:gint; seg_offset:Pgint):PGtkTextLineSegment; cdecl; external gtklib;
function _gtk_text_line_byte_locate(line:PGtkTextLine; byte_offset:gint; var segment:PGtkTextLineSegment; var any_segment:PGtkTextLineSegment; var seg_byte_offset:gint;
           line_byte_offset:Pgint):gboolean; cdecl; external gtklib;
function _gtk_text_line_char_locate(line:PGtkTextLine; char_offset:gint; var segment:PGtkTextLineSegment; var any_segment:PGtkTextLineSegment; var seg_char_offset:gint;
           line_char_offset:Pgint):gboolean; cdecl; external gtklib;
procedure _gtk_text_line_byte_to_char_offsets(line:PGtkTextLine; byte_offset:gint; line_char_offset:Pgint; seg_char_offset:Pgint); cdecl; external gtklib;
procedure _gtk_text_line_char_to_byte_offsets(line:PGtkTextLine; char_offset:gint; line_byte_offset:Pgint; seg_byte_offset:Pgint); cdecl; external gtklib;
function _gtk_text_line_byte_to_any_segment(line:PGtkTextLine; byte_offset:gint; seg_offset:Pgint):PGtkTextLineSegment; cdecl; external gtklib;
function _gtk_text_line_char_to_any_segment(line:PGtkTextLine; char_offset:gint; seg_offset:Pgint):PGtkTextLineSegment; cdecl; external gtklib;
function _gtk_text_line_byte_to_char(line:PGtkTextLine; byte_offset:gint):gint; cdecl; external gtklib;
function _gtk_text_line_char_to_byte(line:PGtkTextLine; char_offset:gint):gint; cdecl; external gtklib;
function _gtk_text_line_next_could_contain_tag(line:PGtkTextLine; tree:PGtkTextBTree; tag:PGtkTextTag):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_line_previous_could_contain_tag(line:PGtkTextLine; tree:PGtkTextBTree; tag:PGtkTextTag):PGtkTextLine; cdecl; external gtklib;
function _gtk_text_line_data_new(layout:PGtkTextLayout; line:PGtkTextLine):PGtkTextLineData; cdecl; external gtklib;
{ Debug  }
procedure _gtk_text_btree_check(tree:PGtkTextBTree); cdecl; external gtklib;
procedure _gtk_text_btree_spew(tree:PGtkTextBTree); cdecl; external gtklib;

{ ignore, exported only for gtktextsegment.c  }
procedure _gtk_toggle_segment_check_func(segPtr:PGtkTextLineSegment; line:PGtkTextLine); cdecl; external gtklib;
procedure _gtk_change_node_toggle_count(node:PGtkTextBTreeNode; info:PGtkTextTagInfo; delta:gint); cdecl; external gtklib;
{ for gtktextmark.c  }
procedure _gtk_text_btree_release_mark_segment(tree:PGtkTextBTree; segment:PGtkTextLineSegment); cdecl; external gtklib;
{ for coordination with the tag table  }
procedure _gtk_text_btree_notify_will_remove_tag(tree:PGtkTextBTree; tag:PGtkTextTag); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function width(a : PGtkTextLineData) : gint;
begin
  width:=a^.flag0 and bm_TGtkTextLineData_width;
end;

procedure set_width(a : PGtkTextLineData; NewWidth : gint);
begin
  a^.flag0:=(bm_TGtkTextLineData_width and NewWidth) or a^.flag0;
end;

function valid(a : PGtkTextLineData) : gint;
begin
  valid:=(a^.flag0 and bm_TGtkTextLineData_valid) shr bp_TGtkTextLineData_valid;
end;

procedure set_valid(a : PGtkTextLineData; __valid : gint);
begin
  a^.flag0:=a^.flag0 or ((__valid shl bp_TGtkTextLineData_valid) and bm_TGtkTextLineData_valid);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

