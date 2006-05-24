// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Dragging within the selection  }
{ length in use, in chars  }
{< private > }
{ Flag so we don't select all when clicking in entry to focus in  }
{ Only used by GtkCellRendererText  }
{ font ascent, in pango units   }
{ font descent, in pango units   }
{ allocated size, in bytes  }
{ length in use, in bytes  }
{ length of preedit string, in bytes  }
{ offset of cursor within preedit string, in chars  }
{ In chars, -1 == no DND cursor  }
   PGtkEntry = ^TGtkEntry;
   TGtkEntry = record
        widget : TGtkWidget;
        text : Pgchar;
        flag0 : word;
        text_length : guint16;
        text_max_length : guint16;
        text_area : PGdkWindow;
        im_context : PGtkIMContext;
        popup_menu : PGtkWidget;
        current_pos : gint;
        selection_bound : gint;
        cached_layout : PPangoLayout;
        flag1 : word;
        button : guint;
        blink_timeout : guint;
        recompute_idle : guint;
        scroll_offset : gint;
        ascent : gint;
        descent : gint;
        text_size : guint16;
        n_bytes : guint16;
        preedit_length : guint16;
        preedit_cursor : guint16;
        dnd_position : gint;
        drag_start_x : gint;
        drag_start_y : gint;
        invisible_char : gunichar;
        width_chars : gint;
     end;

{ Hook to customize right-click popup  }
{ Action signals
    }
{ Padding for future expansion  }
   PGtkEntryClass = ^TGtkEntryClass;
   TGtkEntryClass = record
        parent_class : TGtkWidgetClass;
        populate_popup : procedure (entry:PGtkEntry; menu:PGtkMenu); cdecl;
        activate : procedure (entry:PGtkEntry); cdecl;
        move_cursor : procedure (entry:PGtkEntry; step:TGtkMovementStep; count:gint; extend_selection:gboolean); cdecl;
        insert_at_cursor : procedure (entry:PGtkEntry; str:Pgchar); cdecl;
        delete_from_cursor : procedure (entry:PGtkEntry; _type:TGtkDeleteType; count:gint); cdecl;
        cut_clipboard : procedure (entry:PGtkEntry); cdecl;
        copy_clipboard : procedure (entry:PGtkEntry); cdecl;
        paste_clipboard : procedure (entry:PGtkEntry); cdecl;
        toggle_overwrite : procedure (entry:PGtkEntry); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkEntry_editable = $1;
   bp_TGtkEntry_editable = 0;
   bm_TGtkEntry_visible = $2;
   bp_TGtkEntry_visible = 1;
   bm_TGtkEntry_overwrite_mode = $4;
   bp_TGtkEntry_overwrite_mode = 2;
   bm_TGtkEntry_in_drag = $8;
   bp_TGtkEntry_in_drag = 3;
   bm_TGtkEntry_cache_includes_preedit = $1;
   bp_TGtkEntry_cache_includes_preedit = 0;
   bm_TGtkEntry_need_im_reset = $2;
   bp_TGtkEntry_need_im_reset = 1;
   bm_TGtkEntry_has_frame = $4;
   bp_TGtkEntry_has_frame = 2;
   bm_TGtkEntry_activates_default = $8;
   bp_TGtkEntry_activates_default = 3;
   bm_TGtkEntry_cursor_visible = $10;
   bp_TGtkEntry_cursor_visible = 4;
   bm_TGtkEntry_in_click = $20;
   bp_TGtkEntry_in_click = 5;
   bm_TGtkEntry_is_cell_renderer = $40;
   bp_TGtkEntry_is_cell_renderer = 6;
   bm_TGtkEntry_editing_canceled = $80;
   bp_TGtkEntry_editing_canceled = 7;
   bm_TGtkEntry_mouse_cursor_obscured = $100;
   bp_TGtkEntry_mouse_cursor_obscured = 8;


function GTK_TYPE_ENTRY : GType;
function GTK_ENTRY(obj: pointer) : PGtkEntry;
function GTK_ENTRY_CLASS(klass: pointer) : PGtkEntryClass;
function GTK_IS_ENTRY(obj: pointer) : boolean;
function GTK_IS_ENTRY_CLASS(klass: pointer) : boolean;
function GTK_ENTRY_GET_CLASS(obj: pointer) : PGtkEntryClass;


function editable(var a : TGtkEntry) : guint; overload;
procedure set_editable(var a : TGtkEntry; __editable : guint); overload;
function visible(var a : TGtkEntry) : guint; overload;
procedure set_visible(var a : TGtkEntry; __visible : guint); overload;
function overwrite_mode(var a : TGtkEntry) : guint; overload;
procedure set_overwrite_mode(var a : TGtkEntry; __overwrite_mode : guint); overload;
function in_drag(var a : TGtkEntry) : guint; overload;
procedure set_in_drag(var a : TGtkEntry; __in_drag : guint); overload;
function cache_includes_preedit(var a : TGtkEntry) : guint;
procedure set_cache_includes_preedit(var a : TGtkEntry; __cache_includes_preedit : guint);
function need_im_reset(var a : TGtkEntry) : guint; overload;
procedure set_need_im_reset(var a : TGtkEntry; __need_im_reset : guint); overload;
function has_frame(var a : TGtkEntry) : guint; overload;
procedure set_has_frame(var a : TGtkEntry; __has_frame : guint); overload;
function activates_default(var a : TGtkEntry) : guint;
procedure set_activates_default(var a : TGtkEntry; __activates_default : guint);
function cursor_visible(var a : TGtkEntry) : guint; overload;
procedure set_cursor_visible(var a : TGtkEntry; __cursor_visible : guint); overload;
function in_click(var a : TGtkEntry) : guint;
procedure set_in_click(var a : TGtkEntry; __in_click : guint);
function is_cell_renderer(var a : TGtkEntry) : guint;
procedure set_is_cell_renderer(var a : TGtkEntry; __is_cell_renderer : guint);
function editing_canceled(var a : TGtkEntry) : guint;
procedure set_editing_canceled(var a : TGtkEntry; __editing_canceled : guint);
function mouse_cursor_obscured(var a : TGtkEntry) : guint; overload;
procedure set_mouse_cursor_obscured(var a : TGtkEntry; __mouse_cursor_obscured : guint); overload;

function gtk_entry_get_type:TGtkType; cdecl; external gtklib;
function gtk_entry_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_entry_set_visibility(entry:PGtkEntry; visible:gboolean); cdecl; external gtklib;
function gtk_entry_get_visibility(entry:PGtkEntry):gboolean; cdecl; external gtklib;
procedure gtk_entry_set_invisible_char(entry:PGtkEntry; ch:gunichar); cdecl; external gtklib;
function gtk_entry_get_invisible_char(entry:PGtkEntry):gunichar; cdecl; external gtklib;
procedure gtk_entry_set_has_frame(entry:PGtkEntry; setting:gboolean); cdecl; external gtklib;
function gtk_entry_get_has_frame(entry:PGtkEntry):gboolean; cdecl; external gtklib;
{ text is truncated if needed  }
procedure gtk_entry_set_max_length(entry:PGtkEntry; max:gint); cdecl; external gtklib;
function gtk_entry_get_max_length(entry:PGtkEntry):gint; cdecl; external gtklib;
procedure gtk_entry_set_activates_default(entry:PGtkEntry; setting:gboolean); cdecl; external gtklib;
function gtk_entry_get_activates_default(entry:PGtkEntry):gboolean; cdecl; external gtklib;
procedure gtk_entry_set_width_chars(entry:PGtkEntry; n_chars:gint); cdecl; external gtklib;
function gtk_entry_get_width_chars(entry:PGtkEntry):gint; cdecl; external gtklib;
{ Somewhat more convenient than the GtkEditable generic functions
  }
procedure gtk_entry_set_text(entry:PGtkEntry; text:Pgchar); cdecl; external gtklib;
{ returns a reference to the text  }
function gtk_entry_get_text(entry:PGtkEntry):Pgchar; cdecl; external gtklib;
function gtk_entry_get_layout(entry:PGtkEntry):PPangoLayout; cdecl; external gtklib;
procedure gtk_entry_get_layout_offsets(entry:PGtkEntry; x:Pgint; y:Pgint); cdecl; external gtklib;

{$IFDEF HasGTK2_4}
function gtk_entry_get_alignment(window:PGtkEntry): GFloat; cdecl; external gtklib;
procedure gtk_entry_set_alignment(window:PGtkEntry; xalign: GFloat); cdecl; external gtklib;
function gtk_entry_get_completion(entry:PGtkEntry): PGtkEntryCompletion; cdecl; external gtklib;
procedure gtk_entry_set_completion(entry:PGtkEntry; entryCompletion: PGtkEntryCompletion); cdecl; external gtklib;
{$ENDIF HasGTK2_4}

{ Deprecated compatibility functions }
{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_entry_new_with_max_length(max:gint):PGtkWidget; cdecl; external gtklib;
procedure gtk_entry_append_text(entry:PGtkEntry; text:Pgchar); cdecl; external gtklib;
procedure gtk_entry_prepend_text(entry:PGtkEntry; text:Pgchar); cdecl; external gtklib;
procedure gtk_entry_set_position(entry:PGtkEntry; position:gint); cdecl; external gtklib;
procedure gtk_entry_select_region(entry:PGtkEntry; start:gint; theEnd:gint); cdecl; external gtklib;
procedure gtk_entry_set_editable(entry:PGtkEntry; editable:gboolean); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ENTRY : GType;
begin
   GTK_TYPE_ENTRY:=gtk_entry_get_type;
end;

function GTK_ENTRY(obj: pointer) : PGtkEntry;
begin
   GTK_ENTRY:=PGtkEntry(GTK_CHECK_CAST(obj,GTK_TYPE_ENTRY));
end;

function GTK_ENTRY_CLASS(klass: pointer) : PGtkEntryClass;
begin
   GTK_ENTRY_CLASS:=PGtkEntryClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ENTRY));
end;

function GTK_IS_ENTRY(obj: pointer) : boolean;
begin
   GTK_IS_ENTRY:=GTK_CHECK_TYPE(obj,GTK_TYPE_ENTRY);
end;

function GTK_IS_ENTRY_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ENTRY_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ENTRY);
end;

function GTK_ENTRY_GET_CLASS(obj: pointer) : PGtkEntryClass;
begin
   GTK_ENTRY_GET_CLASS:=PGtkEntryClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ENTRY));
end;

function editable(var a : TGtkEntry) : guint;
begin
   editable:=(a.flag0 and bm_TGtkEntry_editable) shr bp_TGtkEntry_editable;
end;

procedure set_editable(var a : TGtkEntry; __editable : guint);
begin
   a.flag0:=a.flag0 or ((__editable shl bp_TGtkEntry_editable) and bm_TGtkEntry_editable);
end;

function visible(var a : TGtkEntry) : guint;
begin
   visible:=(a.flag0 and bm_TGtkEntry_visible) shr bp_TGtkEntry_visible;
end;

procedure set_visible(var a : TGtkEntry; __visible : guint);
begin
   a.flag0:=a.flag0 or ((__visible shl bp_TGtkEntry_visible) and bm_TGtkEntry_visible);
end;

function overwrite_mode(var a : TGtkEntry) : guint;
begin
   overwrite_mode:=(a.flag0 and bm_TGtkEntry_overwrite_mode) shr bp_TGtkEntry_overwrite_mode;
end;

procedure set_overwrite_mode(var a : TGtkEntry; __overwrite_mode : guint);
begin
   a.flag0:=a.flag0 or ((__overwrite_mode shl bp_TGtkEntry_overwrite_mode) and bm_TGtkEntry_overwrite_mode);
end;

function in_drag(var a : TGtkEntry) : guint;
begin
   in_drag:=(a.flag0 and bm_TGtkEntry_in_drag) shr bp_TGtkEntry_in_drag;
end;

procedure set_in_drag(var a : TGtkEntry; __in_drag : guint);
begin
   a.flag0:=a.flag0 or ((__in_drag shl bp_TGtkEntry_in_drag) and bm_TGtkEntry_in_drag);
end;

function cache_includes_preedit(var a : TGtkEntry) : guint;
begin
   cache_includes_preedit:=(a.flag1 and bm_TGtkEntry_cache_includes_preedit) shr bp_TGtkEntry_cache_includes_preedit;
end;

procedure set_cache_includes_preedit(var a : TGtkEntry; __cache_includes_preedit : guint);
begin
   a.flag1:=a.flag1 or ((__cache_includes_preedit shl bp_TGtkEntry_cache_includes_preedit) and bm_TGtkEntry_cache_includes_preedit);
end;

function need_im_reset(var a : TGtkEntry) : guint;
begin
   need_im_reset:=(a.flag1 and bm_TGtkEntry_need_im_reset) shr bp_TGtkEntry_need_im_reset;
end;

procedure set_need_im_reset(var a : TGtkEntry; __need_im_reset : guint);
begin
   a.flag1:=a.flag1 or ((__need_im_reset shl bp_TGtkEntry_need_im_reset) and bm_TGtkEntry_need_im_reset);
end;

function has_frame(var a : TGtkEntry) : guint;
begin
   has_frame:=(a.flag1 and bm_TGtkEntry_has_frame) shr bp_TGtkEntry_has_frame;
end;

procedure set_has_frame(var a : TGtkEntry; __has_frame : guint);
begin
   a.flag1:=a.flag1 or ((__has_frame shl bp_TGtkEntry_has_frame) and bm_TGtkEntry_has_frame);
end;

function activates_default(var a : TGtkEntry) : guint;
begin
   activates_default:=(a.flag1 and bm_TGtkEntry_activates_default) shr bp_TGtkEntry_activates_default;
end;

procedure set_activates_default(var a : TGtkEntry; __activates_default : guint);
begin
   a.flag1:=a.flag1 or ((__activates_default shl bp_TGtkEntry_activates_default) and bm_TGtkEntry_activates_default);
end;

function cursor_visible(var a : TGtkEntry) : guint;
begin
   cursor_visible:=(a.flag1 and bm_TGtkEntry_cursor_visible) shr bp_TGtkEntry_cursor_visible;
end;

procedure set_cursor_visible(var a : TGtkEntry; __cursor_visible : guint);
begin
   a.flag1:=a.flag1 or ((__cursor_visible shl bp_TGtkEntry_cursor_visible) and bm_TGtkEntry_cursor_visible);
end;

function in_click(var a : TGtkEntry) : guint;
begin
   in_click:=(a.flag1 and bm_TGtkEntry_in_click) shr bp_TGtkEntry_in_click;
end;

procedure set_in_click(var a : TGtkEntry; __in_click : guint);
begin
   a.flag1:=a.flag1 or ((__in_click shl bp_TGtkEntry_in_click) and bm_TGtkEntry_in_click);
end;

function is_cell_renderer(var a : TGtkEntry) : guint;
begin
   is_cell_renderer:=(a.flag1 and bm_TGtkEntry_is_cell_renderer) shr bp_TGtkEntry_is_cell_renderer;
end;

procedure set_is_cell_renderer(var a : TGtkEntry; __is_cell_renderer : guint);
begin
   a.flag1:=a.flag1 or ((__is_cell_renderer shl bp_TGtkEntry_is_cell_renderer) and bm_TGtkEntry_is_cell_renderer);
end;

function editing_canceled(var a : TGtkEntry) : guint;
begin
   editing_canceled:=(a.flag1 and bm_TGtkEntry_editing_canceled) shr bp_TGtkEntry_editing_canceled;
end;

procedure set_editing_canceled(var a : TGtkEntry; __editing_canceled : guint);
begin
   a.flag1:=a.flag1 or ((__editing_canceled shl bp_TGtkEntry_editing_canceled) and bm_TGtkEntry_editing_canceled);
end;

function mouse_cursor_obscured(var a : TGtkEntry) : guint;
begin
   mouse_cursor_obscured:=(a.flag1 and bm_TGtkEntry_mouse_cursor_obscured) shr bp_TGtkEntry_mouse_cursor_obscured;
end;

procedure set_mouse_cursor_obscured(var a : TGtkEntry; __mouse_cursor_obscured : guint);
begin
   a.flag1:=a.flag1 or ((__mouse_cursor_obscured shl bp_TGtkEntry_mouse_cursor_obscured) and bm_TGtkEntry_mouse_cursor_obscured);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

