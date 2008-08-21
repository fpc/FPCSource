// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkTextLogAttrCache = pointer;

{ Whether the buffer has been modified since last save  }
   PGtkTextBuffer = ^TGtkTextBuffer;
   TGtkTextBuffer = record
        parent_instance : TGObject;
        tag_table : PGtkTextTagTable;
        btree : PGtkTextBTree;
        clipboard_contents_buffers : PGSList;
        selection_clipboards : PGSList;
        log_attr_cache : PGtkTextLogAttrCache;
        user_action_count : guint;
        flag0 : word;
     end;

{ Only for text/widgets/pixbuf changed, marks/tags don't cause this
     to be emitted
    }
{ New value for the modified flag  }
{ Mark moved or created  }
{ Called at the start and theEnd of an atomic user action  }
{ Padding for future expansion  }
   PGtkTextBufferClass = ^TGtkTextBufferClass;
   TGtkTextBufferClass = record
        parent_class : TGObjectClass;
        insert_text : procedure (buffer:PGtkTextBuffer; pos:PGtkTextIter; text:Pgchar; length:gint); cdecl;
        insert_pixbuf : procedure (buffer:PGtkTextBuffer; pos:PGtkTextIter; pixbuf:PGdkPixbuf); cdecl;
        insert_child_anchor : procedure (buffer:PGtkTextBuffer; pos:PGtkTextIter; anchor:PGtkTextChildAnchor); cdecl;
        delete_range : procedure (buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl;
        changed : procedure (buffer:PGtkTextBuffer); cdecl;
        modified_changed : procedure (buffer:PGtkTextBuffer); cdecl;
        mark_set : procedure (buffer:PGtkTextBuffer; location:PGtkTextIter; mark:PGtkTextMark); cdecl;
        mark_deleted : procedure (buffer:PGtkTextBuffer; mark:PGtkTextMark); cdecl;
        apply_tag : procedure (buffer:PGtkTextBuffer; tag:PGtkTextTag; start_char:PGtkTextIter; end_char:PGtkTextIter); cdecl;
        remove_tag : procedure (buffer:PGtkTextBuffer; tag:PGtkTextTag; start_char:PGtkTextIter; end_char:PGtkTextIter); cdecl;
        begin_user_action : procedure (buffer:PGtkTextBuffer); cdecl;
        end_user_action : procedure (buffer:PGtkTextBuffer); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
        _gtk_reserved5 : procedure ; cdecl;
        _gtk_reserved6 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTextBuffer_modified = $1;
   bp_TGtkTextBuffer_modified = 0;


{
   This is the PUBLIC representation of a text buffer.
   GtkTextBTree is the PRIVATE internal representation of it.
  }
function GTK_TYPE_TEXT_BUFFER : GType;
function GTK_TEXT_BUFFER(obj: pointer) : PGtkTextBuffer;
function GTK_TEXT_BUFFER_CLASS(klass: pointer) : PGtkTextBufferClass;
function GTK_IS_TEXT_BUFFER(obj: pointer) : boolean;
function GTK_IS_TEXT_BUFFER_CLASS(klass: pointer) : boolean;
function GTK_TEXT_BUFFER_GET_CLASS(obj: pointer) : PGtkTextBufferClass;

function modified(var a : TGtkTextBuffer) : guint;
procedure set_modified(var a : TGtkTextBuffer; __modified : guint);

function gtk_text_buffer_get_type:GType; cdecl; external gtklib;
{ table is NULL to create a new one  }
function gtk_text_buffer_new(table:PGtkTextTagTable):PGtkTextBuffer; cdecl; external gtklib;
function gtk_text_buffer_get_line_count(buffer:PGtkTextBuffer):gint; cdecl; external gtklib;
function gtk_text_buffer_get_char_count(buffer:PGtkTextBuffer):gint; cdecl; external gtklib;
function gtk_text_buffer_get_tag_table(buffer:PGtkTextBuffer):PGtkTextTagTable; cdecl; external gtklib;
{ Delete whole buffer, then insert  }
procedure gtk_text_buffer_set_text(buffer:PGtkTextBuffer; text:Pgchar; len:gint); cdecl; external gtklib;
{ Insert into the buffer  }
procedure gtk_text_buffer_insert(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint); cdecl; external gtklib;
procedure gtk_text_buffer_insert_at_cursor(buffer:PGtkTextBuffer; text:Pgchar; len:gint); cdecl; external gtklib;
function gtk_text_buffer_insert_interactive(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; default_editable:gboolean):gboolean; cdecl; external gtklib;
function gtk_text_buffer_insert_interactive_at_cursor(buffer:PGtkTextBuffer; text:Pgchar; len:gint; default_editable:gboolean):gboolean; cdecl; external gtklib;
procedure gtk_text_buffer_insert_range(buffer:PGtkTextBuffer; iter:PGtkTextIter; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
function gtk_text_buffer_insert_range_interactive(buffer:PGtkTextBuffer; iter:PGtkTextIter; start:PGtkTextIter; theEnd:PGtkTextIter; default_editable:gboolean):gboolean; cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_text_buffer_insert_with_tags(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; first_tag:PGtkTextTag;
            args:array of const); cdecl; overload; external gtklib;
procedure gtk_text_buffer_insert_with_tags(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; first_tag:PGtkTextTag); cdecl; overload; varargs; external gtklib;
procedure gtk_text_buffer_insert_with_tags_by_name(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; first_tag_name:Pgchar;
            args:array of const); cdecl; overload; external gtklib;
procedure gtk_text_buffer_insert_with_tags_by_name(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; first_tag_name:Pgchar); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_text_buffer_insert_with_tags(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; first_tag:PGtkTextTag); varargs; cdecl; external gtklib;
procedure gtk_text_buffer_insert_with_tags_by_name(buffer:PGtkTextBuffer; iter:PGtkTextIter; text:Pgchar; len:gint; first_tag_name:Pgchar); varargs; cdecl; external gtklib;
{$ENDIF}
{ Delete from the buffer  }
procedure gtk_text_buffer_delete(buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
function gtk_text_buffer_delete_interactive(buffer:PGtkTextBuffer; start_iter:PGtkTextIter; end_iter:PGtkTextIter; default_editable:gboolean):gboolean; cdecl; external gtklib;
{ Obtain strings from the buffer  }
function gtk_text_buffer_get_text(buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter; include_hidden_chars:gboolean):Pgchar; cdecl; external gtklib;
function gtk_text_buffer_get_slice(buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter; include_hidden_chars:gboolean):Pgchar; cdecl; external gtklib;
{ Insert a pixbuf  }
procedure gtk_text_buffer_insert_pixbuf(buffer:PGtkTextBuffer; iter:PGtkTextIter; pixbuf:PGdkPixbuf); cdecl; external gtklib;
{ Insert a child anchor  }
procedure gtk_text_buffer_insert_child_anchor(buffer:PGtkTextBuffer; iter:PGtkTextIter; anchor:PGtkTextChildAnchor); cdecl; external gtklib;
{ Convenience, create and insert a child anchor  }
function gtk_text_buffer_create_child_anchor(buffer:PGtkTextBuffer; iter:PGtkTextIter):PGtkTextChildAnchor; cdecl; external gtklib;
{ Mark manipulation  }
function gtk_text_buffer_create_mark(buffer:PGtkTextBuffer; mark_name:Pgchar; where:PGtkTextIter; left_gravity:gboolean):PGtkTextMark; cdecl; external gtklib;
procedure gtk_text_buffer_move_mark(buffer:PGtkTextBuffer; mark:PGtkTextMark; where:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_delete_mark(buffer:PGtkTextBuffer; mark:PGtkTextMark); cdecl; external gtklib;
function gtk_text_buffer_get_mark(buffer:PGtkTextBuffer; name:Pgchar):PGtkTextMark; cdecl; external gtklib;
procedure gtk_text_buffer_move_mark_by_name(buffer:PGtkTextBuffer; name:Pgchar; where:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_delete_mark_by_name(buffer:PGtkTextBuffer; name:Pgchar); cdecl; external gtklib;
function gtk_text_buffer_get_insert(buffer:PGtkTextBuffer):PGtkTextMark; cdecl; external gtklib;
function gtk_text_buffer_get_selection_bound(buffer:PGtkTextBuffer):PGtkTextMark; cdecl; external gtklib;
{ efficiently move insert and selection_bound to same location  }
procedure gtk_text_buffer_place_cursor(buffer:PGtkTextBuffer; where:PGtkTextIter); cdecl; external gtklib;
{ Tag manipulation  }
procedure gtk_text_buffer_apply_tag(buffer:PGtkTextBuffer; tag:PGtkTextTag; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_remove_tag(buffer:PGtkTextBuffer; tag:PGtkTextTag; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_apply_tag_by_name(buffer:PGtkTextBuffer; name:Pgchar; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_remove_tag_by_name(buffer:PGtkTextBuffer; name:Pgchar; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_remove_all_tags(buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
{ You can either ignore the return value, or use it to
   set the attributes of the tag. tag_name can be NULL
  }
{$IFNDEF KYLIX}
function gtk_text_buffer_create_tag(buffer:PGtkTextBuffer; tag_name:Pgchar; first_property_name:Pgchar; args:array of const):PGtkTextTag; cdecl; overload; external gtklib;
function gtk_text_buffer_create_tag(buffer:PGtkTextBuffer; tag_name:Pgchar; first_property_name:Pgchar):PGtkTextTag; cdecl; overload; external gtklib;
{$ELSE}
function gtk_text_buffer_create_tag(buffer:PGtkTextBuffer; tag_name:Pgchar; first_property_name:Pgchar):PGtkTextTag; varargs; cdecl; external gtklib;
{$ENDIF}
{ Obtain iterators pointed at various places, then you can move the
   iterator around using the GtkTextIter operators
  }
procedure gtk_text_buffer_get_iter_at_line_offset(buffer:PGtkTextBuffer; iter:PGtkTextIter; line_number:gint; char_offset:gint); cdecl; external gtklib;
procedure gtk_text_buffer_get_iter_at_line_index(buffer:PGtkTextBuffer; iter:PGtkTextIter; line_number:gint; byte_index:gint); cdecl; external gtklib;
procedure gtk_text_buffer_get_iter_at_offset(buffer:PGtkTextBuffer; iter:PGtkTextIter; char_offset:gint); cdecl; external gtklib;
procedure gtk_text_buffer_get_iter_at_line(buffer:PGtkTextBuffer; iter:PGtkTextIter; line_number:gint); cdecl; external gtklib;
procedure gtk_text_buffer_get_start_iter(buffer:PGtkTextBuffer; iter:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_get_end_iter(buffer:PGtkTextBuffer; iter:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_get_bounds(buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter); cdecl; external gtklib;
procedure gtk_text_buffer_get_iter_at_mark(buffer:PGtkTextBuffer; iter:PGtkTextIter; mark:PGtkTextMark); cdecl; external gtklib;
procedure gtk_text_buffer_get_iter_at_child_anchor(buffer:PGtkTextBuffer; iter:PGtkTextIter; anchor:PGtkTextChildAnchor); cdecl; external gtklib;
{ There's no get_first_iter because you just get the iter for
   line or char 0  }
{ Used to keep track of whether the buffer needs saving; anytime the
   buffer contents change, the modified flag is turned on. Whenever
   you save, turn it off. Tags and marks do not affect the modified
   flag, but if you would like them to you can connect a handler to
   the tag/mark signals and call set_modified in your handler  }
function gtk_text_buffer_get_modified(buffer:PGtkTextBuffer):gboolean; cdecl; external gtklib;
procedure gtk_text_buffer_set_modified(buffer:PGtkTextBuffer; setting:gboolean); cdecl; external gtklib;
procedure gtk_text_buffer_add_selection_clipboard(buffer:PGtkTextBuffer; clipboard:PGtkClipboard); cdecl; external gtklib;
procedure gtk_text_buffer_remove_selection_clipboard(buffer:PGtkTextBuffer; clipboard:PGtkClipboard); cdecl; external gtklib;
procedure gtk_text_buffer_cut_clipboard(buffer:PGtkTextBuffer; clipboard:PGtkClipboard; default_editable:gboolean); cdecl; external gtklib;
procedure gtk_text_buffer_copy_clipboard(buffer:PGtkTextBuffer; clipboard:PGtkClipboard); cdecl; external gtklib;
procedure gtk_text_buffer_paste_clipboard(buffer:PGtkTextBuffer; clipboard:PGtkClipboard; override_location:PGtkTextIter; default_editable:gboolean); cdecl; external gtklib;
function gtk_text_buffer_get_selection_bounds(buffer:PGtkTextBuffer; start:PGtkTextIter; theEnd:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_buffer_delete_selection(buffer:PGtkTextBuffer; interactive:gboolean; default_editable:gboolean):gboolean; cdecl; external gtklib;
{ Called to specify atomic user actions, used to implement undo  }
procedure gtk_text_buffer_begin_user_action(buffer:PGtkTextBuffer); cdecl; external gtklib;
procedure gtk_text_buffer_end_user_action(buffer:PGtkTextBuffer); cdecl; external gtklib;
{ INTERNAL private stuff  }
procedure _gtk_text_buffer_spew(buffer:PGtkTextBuffer); cdecl; external gtklib;
function _gtk_text_buffer_get_btree(buffer:PGtkTextBuffer):PGtkTextBTree; cdecl; external gtklib;
function _gtk_text_buffer_get_line_log_attrs(buffer:PGtkTextBuffer; anywhere_in_line:PGtkTextIter; char_len:Pgint):PPangoLogAttr; cdecl; external gtklib;
procedure _gtk_text_buffer_notify_will_remove_tag(buffer:PGtkTextBuffer; tag:PGtkTextTag); cdecl; external gtklib;

{$ifdef HasGTK2_4}
procedure gtk_text_buffer_select_range(buffer: PGtkTextBuffer; ins: PGtkTextIter; bound: PGtkTextIter); cdecl; external gtklib;
{$endif}

{$ifdef HasGTK2_6}
function gtk_text_buffer_backspace(buffer:PGtkTextBuffer; iter:PGtkTextIter; interactive:gboolean; default_editable:gboolean):gboolean; cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_BUFFER : GType;
begin
   GTK_TYPE_TEXT_BUFFER:=gtk_text_buffer_get_type;
end;

function GTK_TEXT_BUFFER(obj: pointer) : PGtkTextBuffer;
begin
   GTK_TEXT_BUFFER:=PGtkTextBuffer(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TEXT_BUFFER));
end;

function GTK_TEXT_BUFFER_CLASS(klass: pointer) : PGtkTextBufferClass;
begin
   GTK_TEXT_BUFFER_CLASS:=PGtkTextBufferClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT_BUFFER));
end;

function GTK_IS_TEXT_BUFFER(obj: pointer) : boolean;
begin
   GTK_IS_TEXT_BUFFER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TEXT_BUFFER);
end;

function GTK_IS_TEXT_BUFFER_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_BUFFER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT_BUFFER);
end;

function GTK_TEXT_BUFFER_GET_CLASS(obj: pointer) : PGtkTextBufferClass;
begin
   GTK_TEXT_BUFFER_GET_CLASS:=PGtkTextBufferClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TEXT_BUFFER));
end;

function modified(var a : TGtkTextBuffer) : guint;
begin
   modified:=(a.flag0 and bm_TGtkTextBuffer_modified) shr bp_TGtkTextBuffer_modified;
end;

procedure set_modified(var a : TGtkTextBuffer; __modified : guint);
begin
   a.flag0:=a.flag0 or ((__modified shl bp_TGtkTextBuffer_modified) and bm_TGtkTextBuffer_modified);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

