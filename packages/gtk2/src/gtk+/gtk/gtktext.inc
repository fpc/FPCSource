// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkTextFont = pointer;

{ Position in list.  }
{ Offset into that property.  }
{ Current index.  }
   PGtkPropertyMark = ^TGtkPropertyMark;
   TGtkPropertyMark = record
        _property : PGList;
        offset : guint;
        index : guint;
     end;

{ GAPPED TEXT SEGMENT  }
{ The text, a single segment of text a'la emacs, with a gap
     where insertion occurs.  }
{ The allocated length of the text segment.  }
{ The gap position, index into address where a char
     should be inserted.  }
{ The gap size, s.t.  (text + gap_position + gap_size) is
     the first valid character following the gap.  }
{ The last character position, index into address where a
     character should be appeneded.  Thus, text_end - gap_size
     is the length of the actual data.  }
{ LINE START CACHE  }
{ A cache of line-start information.  Data is a LineParam .  }
{ Index to the start of the first visible line.  }
{ The number of pixels cut off of the top line.  }
{ First visible horizontal pixel.  }
{ First visible vertical pixel.  }
{ FLAGS  }
{ True iff this buffer is wrapping lines, otherwise it is using a
     horizontal scrollbar.  }
{ If a fontset is supplied for the widget, use_wchar become true,
     and we use GdkWchar as the encoding of text.  }
{ Frozen, don't do updates. @@@ fixme  }
{ TEXT PROPERTIES  }
{ A doubly-linked-list containing TextProperty objects.  }
{ The theEnd of this list.  }
{ The first node before or on the point along with its offset to
     the point and the buffer's current point.  This is the only
     PropertyMark whose index is guaranteed to remain correct
     following a buffer insertion or deletion.  }
{ SCRATCH AREA  }
{ SCROLLING  }
{ CURSOR  }
{ Position of cursor.  }
{ Baseline of line cursor is drawn on.  }
{ Where it is in the buffer.  }
{ Character to redraw.  }
{ Distance from baseline of the font.  }
{ Where it would be if it could be.  }
{ How many people have undrawn.  }
{ Current Line  }
{ Tab Stops  }
{ Text font for current style  }
{ Timer used for auto-scrolling off ends  }
{ currently pressed mouse button  }
{ gc for drawing background pixmap  }
   PGtkText = ^TGtkText;
   TGtkText = record
        old_editable : TGtkOldEditable;
        text_area : PGdkWindow;
        hadj : PGtkAdjustment;
        vadj : PGtkAdjustment;
        gc : PGdkGC;
        line_wrap_bitmap : PGdkPixmap;
        line_arrow_bitmap : PGdkPixmap;
        text : record
            case longint of
               0 : ( wc : PGdkWChar );
               1 : ( ch : Pguchar );
            end;
        text_len : guint;
        gap_position : guint;
        gap_size : guint;
        text_end : guint;
        line_start_cache : PGList;
        first_line_start_index : guint;
        first_cut_pixels : guint;
        first_onscreen_hor_pixel : guint;
        first_onscreen_ver_pixel : guint;
        flag0 : word;
        freeze_count : guint;
        text_properties : PGList;
        text_properties_end : PGList;
        point : TGtkPropertyMark;
        scratch_buffer : record
            case longint of
               0 : ( wc : PGdkWChar );
               1 : ( ch : Pguchar );
            end;
        scratch_buffer_len : guint;
        last_ver_value : gint;
        cursor_pos_x : gint;
        cursor_pos_y : gint;
        cursor_mark : TGtkPropertyMark;
        cursor_char : TGdkWChar;
        cursor_char_offset : gchar;
        cursor_virtual_x : gint;
        cursor_drawn_level : gint;
        current_line : PGList;
        tab_stops : PGList;
        default_tab_width : gint;
        current_font : PGtkTextFont;
        timer : gint;
        button : guint;
        bg_gc : PGdkGC;
     end;


   PGtkTextClass = ^TGtkTextClass;
   TGtkTextClass = record
        parent_class : TGtkOldEditableClass;
        set_scroll_adjustments : procedure (text:PGtkText; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment); cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkText_line_wrap = $1;
   bp_TGtkText_line_wrap = 0;
   bm_TGtkText_word_wrap = $2;
   bp_TGtkText_word_wrap = 1;
   bm_TGtkText_use_wchar = $4;
   bp_TGtkText_use_wchar = 2;
function GTK_TYPE_TEXT : GType;
function GTK_TEXT(obj: pointer) : PGtkText;
function GTK_TEXT_CLASS(klass: pointer) : PGtkTextClass;
function GTK_IS_TEXT(obj: pointer) : boolean;
function GTK_IS_TEXT_CLASS(klass: pointer) : boolean;
function GTK_TEXT_GET_CLASS(obj: pointer) : PGtkTextClass;


function line_wrap(a : PGtkText) : guint;
procedure set_line_wrap(a : PGtkText; __line_wrap : guint);
function word_wrap(a : PGtkText) : guint;
procedure set_word_wrap(a : PGtkText; __word_wrap : guint);
function use_wchar(a : PGtkText) : gboolean;
procedure set_use_wchar(a : PGtkText; __use_wchar : gboolean);


function gtk_text_get_type:TGtkType; cdecl; external gtklib;
function gtk_text_new(hadj:PGtkAdjustment; vadj:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
procedure gtk_text_set_editable(text:PGtkText; editable:gboolean); cdecl; external gtklib;
procedure gtk_text_set_word_wrap(text:PGtkText; word_wrap:gboolean); cdecl; external gtklib;
procedure gtk_text_set_line_wrap(text:PGtkText; line_wrap:gboolean); cdecl; external gtklib;
procedure gtk_text_set_adjustments(text:PGtkText; hadj:PGtkAdjustment; vadj:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_text_set_point(text:PGtkText; index:guint); cdecl; external gtklib;
function gtk_text_get_point(text:PGtkText):guint; cdecl; external gtklib;
function gtk_text_get_length(text:PGtkText):guint; cdecl; external gtklib;
procedure gtk_text_freeze(text:PGtkText); cdecl; external gtklib;
procedure gtk_text_thaw(text:PGtkText); cdecl; external gtklib;

procedure gtk_text_insert(text:PGtkText; font:PGdkFont; fore:PGdkColor; back:PGdkColor; chars:Pchar;
            length:gint); cdecl; external gtklib;
function gtk_text_backward_delete(text:PGtkText; nchars:guint):gboolean; cdecl; external gtklib;
function gtk_text_forward_delete(text:PGtkText; nchars:guint):gboolean; cdecl; external gtklib;

function GTK_TEXT_INDEX_WCHAR(t: PGtkText; index: guint): guint32;
function GTK_TEXT_INDEX_UCHAR(t: PGtkText; index: guint): GUChar;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT : GType;
begin
   GTK_TYPE_TEXT:=gtk_text_get_type;
end;

function GTK_TEXT(obj: pointer) : PGtkText;
begin
   GTK_TEXT:=PGtkText(GTK_CHECK_CAST(obj,GTK_TYPE_TEXT));
end;

function GTK_TEXT_CLASS(klass: pointer) : PGtkTextClass;
begin
   GTK_TEXT_CLASS:=PGtkTextClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT));
end;

function GTK_IS_TEXT(obj: pointer) : boolean;
begin
   GTK_IS_TEXT:=GTK_CHECK_TYPE(obj,GTK_TYPE_TEXT);
end;

function GTK_IS_TEXT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT);
end;

function GTK_TEXT_GET_CLASS(obj: pointer) : PGtkTextClass;
begin
   GTK_TEXT_GET_CLASS:=PGtkTextClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TEXT));
end;

function line_wrap(a : PGtkText) : guint;
begin
   line_wrap:=(a^.flag0 and bm_TGtkText_line_wrap) shr bp_TGtkText_line_wrap;
end;

procedure set_line_wrap(a : PGtkText; __line_wrap : guint);
begin
   a^.flag0:=a^.flag0 or ((__line_wrap shl bp_TGtkText_line_wrap)
               and bm_TGtkText_line_wrap);
end;

function word_wrap(a : PGtkText) : guint;
begin
   word_wrap:=(a^.flag0 and bm_TGtkText_word_wrap) shr bp_TGtkText_word_wrap;
end;

procedure set_word_wrap(a : PGtkText; __word_wrap : guint);
begin
   a^.flag0:=a^.flag0 or ((__word_wrap shl bp_TGtkText_word_wrap)
                    and bm_TGtkText_word_wrap);
end;

function use_wchar(a : PGtkText) : gboolean;
begin
   use_wchar:=((a^.flag0 and bm_TGtkText_use_wchar) shr bp_TGtkText_use_wchar)>0;
end;

procedure set_use_wchar(a : PGtkText; __use_wchar : gboolean);
begin
  if __use_wchar then
    a^.flag0:=a^.flag0 or bm_TGtkText_use_wchar
  else
    a^.flag0:=a^.flag0 and not bm_TGtkText_use_wchar;
end;

function GTK_TEXT_INDEX_WCHAR(t: PGtkText; index: guint): guint32;
begin
{$IFNDEF KYLIX}
  if Use_WChar(t) then begin
    if index < t^.gap_position then begin
      Result:=t^.text.wc[index];
    end else begin
      Result:=t^.text.wc[index+t^.gap_size];
    end;
  end else begin
    writeln('GTK_TEXT_INDEX_WCHAR ERROR: t does not use wide char');
    Result:=0;
  end;
{$ENDIF}
end;

function GTK_TEXT_INDEX_UCHAR(t: PGtkText; index: guint): GUChar;
begin
{$IFNDEF KYLIX}
  if Use_WChar(t) then begin
    writeln('GTK_TEXT_INDEX_WCHAR ERROR: t does not use unsigned char');
    Result:=0;
  end else begin
    if index < t^.gap_position then begin
      Result:=t^.text.ch[index];
    end else begin
      Result:=t^.text.ch[index+t^.gap_size];
    end;
  end;
{$ENDIF}
end;

{$ENDIF read_implementation}
// included by gtk2.pas

