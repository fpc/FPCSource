// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkTextWindow = pointer;
   // internal type
   PGtkTextPendingScroll = pointer;

   PGtkTextWindowType = ^TGtkTextWindowType;
   TGtkTextWindowType = (
     GTK_TEXT_WINDOW_PRIVATE,
     GTK_TEXT_WINDOW_WIDGET,
     GTK_TEXT_WINDOW_TEXT,
     GTK_TEXT_WINDOW_LEFT,
     GTK_TEXT_WINDOW_RIGHT,
     GTK_TEXT_WINDOW_TOP,
     GTK_TEXT_WINDOW_BOTTOM
   );

{ Internal private types.  }
{ Default style settings  }
{ If we have reset the IM since the last character entered  }
{ just selected a word or line via double/triple click  }
{ disable scrolling to cursor on focus  }
{ debug flag - means that we've validated onscreen since the
     last "invalidate" signal from the layout
    }
{ Offsets between widget coordinates and buffer coordinates  }
{ Width and height of the buffer  }
{ The virtual cursor position is normally the same as the
     actual (strong) cursor position, except in two circumstances:

     a) When the cursor is moved vertically with the keyboard
     b) When the text view is scrolled with the keyboard

     In case a), virtual_cursor_x is preserved, but not virtual_cursor_y
     In case b), both virtual_cursor_x and virtual_cursor_y are preserved.
    }
{ -1 means use actual cursor position  }
{ -1 means use actual cursor position  }
{ Mark at the beginning of the first onscreen paragraph  }
{ Offset of top of screen in the first onscreen paragraph  }
{ Idle to revalidate onscreen portion, runs before resize  }
{ Idle to revalidate offscreen portions, runs after redraw  }
   PGtkTextView = ^TGtkTextView;
   TGtkTextView = record
        parent_instance : TGtkContainer;
        layout : PGtkTextLayout;
        buffer : PGtkTextBuffer;
        selection_drag_handler : guint;
        scroll_timeout : guint;
        pixels_above_lines : gint;
        pixels_below_lines : gint;
        pixels_inside_wrap : gint;
        wrap_mode : TGtkWrapMode;
        justify : TGtkJustification;
        left_margin : gint;
        right_margin : gint;
        indent : gint;
        tabs : PPangoTabArray;
        flag0 : word;
        text_window : PGtkTextWindow;
        left_window : PGtkTextWindow;
        right_window : PGtkTextWindow;
        top_window : PGtkTextWindow;
        bottom_window : PGtkTextWindow;
        hadjustment : PGtkAdjustment;
        vadjustment : PGtkAdjustment;
        xoffset : gint;
        yoffset : gint;
        width : gint;
        height : gint;
        virtual_cursor_x : gint;
        virtual_cursor_y : gint;
        first_para_mark : PGtkTextMark;
        first_para_pixels : gint;
        dnd_mark : PGtkTextMark;
        blink_timeout : guint;
        first_validate_idle : guint;
        incremental_validate_idle : guint;
        im_context : PGtkIMContext;
        popup_menu : PGtkWidget;
        drag_start_x : gint;
        drag_start_y : gint;
        children : PGSList;
        pending_scroll : PGtkTextPendingScroll;
        pending_place_cursor_button : gint;
     end;

{ These are all RUN_ACTION signals for keybindings  }
{ move insertion point  }
{ FIXME should be deprecated in favor of adding GTK_MOVEMENT_HORIZONTAL_PAGES
     or something in GTK 2.2, was put in to avoid adding enum values during
     the freeze.
    }
{ move the "anchor" (what Emacs calls the mark) to the cursor position  }
{ Edits  }
{ cut copy paste  }
{ overwrite  }
{ propagates to GtkWindow move_focus  }
{ Padding for future expansion  }
   PGtkTextViewClass = ^TGtkTextViewClass;
   TGtkTextViewClass = record
        parent_class : TGtkContainerClass;
        set_scroll_adjustments : procedure (text_view:PGtkTextView; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment); cdecl;
        populate_popup : procedure (text_view:PGtkTextView; menu:PGtkMenu); cdecl;
        move_cursor : procedure (text_view:PGtkTextView; step:TGtkMovementStep; count:gint; extend_selection:gboolean); cdecl;
        page_horizontally : procedure (text_view:PGtkTextView; count:gint; extend_selection:gboolean); cdecl;
        set_anchor : procedure (text_view:PGtkTextView); cdecl;
        insert_at_cursor : procedure (text_view:PGtkTextView; str:Pgchar); cdecl;
        delete_from_cursor : procedure (text_view:PGtkTextView; _type:TGtkDeleteType; count:gint); cdecl;
        cut_clipboard : procedure (text_view:PGtkTextView); cdecl;
        copy_clipboard : procedure (text_view:PGtkTextView); cdecl;
        paste_clipboard : procedure (text_view:PGtkTextView); cdecl;
        toggle_overwrite : procedure (text_view:PGtkTextView); cdecl;
        move_focus : procedure (text_view:PGtkTextView; direction:TGtkDirectionType); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
        _gtk_reserved5 : procedure ; cdecl;
        _gtk_reserved6 : procedure ; cdecl;
        _gtk_reserved7 : procedure ; cdecl;
        _gtk_reserved8 : procedure ; cdecl;
     end;


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_TEXT_VIEW_PRIORITY_VALIDATE = GDK_PRIORITY_REDRAW + 5;

   bm_TGtkTextView_editable = $1;
   bp_TGtkTextView_editable = 0;
   bm_TGtkTextView_overwrite_mode = $2;
   bp_TGtkTextView_overwrite_mode = 1;
   bm_TGtkTextView_cursor_visible = $4;
   bp_TGtkTextView_cursor_visible = 2;
   bm_TGtkTextView_need_im_reset = $8;
   bp_TGtkTextView_need_im_reset = 3;
   bm_TGtkTextView_just_selected_element = $10;
   bp_TGtkTextView_just_selected_element = 4;
   bm_TGtkTextView_disable_scroll_on_focus = $20;
   bp_TGtkTextView_disable_scroll_on_focus = 5;
   bm_TGtkTextView_onscreen_validated = $40;
   bp_TGtkTextView_onscreen_validated = 6;
   bm_TGtkTextView_mouse_cursor_obscured = $80;
   bp_TGtkTextView_mouse_cursor_obscured = 7;


function GTK_TYPE_TEXT_VIEW : GType;
function GTK_TEXT_VIEW(obj: pointer) : PGtkTextView;
function GTK_TEXT_VIEW_CLASS(klass: pointer) : PGtkTextViewClass;
function GTK_IS_TEXT_VIEW(obj: pointer) : boolean;
function GTK_IS_TEXT_VIEW_CLASS(klass: pointer) : boolean;
function GTK_TEXT_VIEW_GET_CLASS(obj: pointer) : PGtkTextViewClass;

function editable(var a : TGtkTextView) : guint; overload;
procedure set_editable(var a : TGtkTextView; __editable : guint); overload;
function overwrite_mode(var a : TGtkTextView) : guint; overload;
procedure set_overwrite_mode(var a : TGtkTextView; __overwrite_mode : guint); overload;
function cursor_visible(var a : TGtkTextView) : guint; overload;
procedure set_cursor_visible(var a : TGtkTextView; __cursor_visible : guint); overload;
function need_im_reset(var a : TGtkTextView) : guint; overload;
procedure set_need_im_reset(var a : TGtkTextView; __need_im_reset : guint); overload;
function just_selected_element(var a : TGtkTextView) : guint;
procedure set_just_selected_element(var a : TGtkTextView; __just_selected_element : guint);
function disable_scroll_on_focus(var a : TGtkTextView) : guint;
procedure set_disable_scroll_on_focus(var a : TGtkTextView; __disable_scroll_on_focus : guint);
function onscreen_validated(var a : TGtkTextView) : guint;
procedure set_onscreen_validated(var a : TGtkTextView; __onscreen_validated : guint);
function mouse_cursor_obscured(var a : TGtkTextView) : guint; overload;
procedure set_mouse_cursor_obscured(var a : TGtkTextView; __mouse_cursor_obscured : guint); overload;

function gtk_text_view_get_type:TGtkType; cdecl; external gtklib;
function gtk_text_view_new:PGtkWidget; cdecl; external gtklib;
function gtk_text_view_new_with_buffer(buffer:PGtkTextBuffer):PGtkWidget; cdecl; external gtklib;
procedure gtk_text_view_set_buffer(text_view:PGtkTextView; buffer:PGtkTextBuffer); cdecl; external gtklib;
function gtk_text_view_get_buffer(text_view:PGtkTextView):PGtkTextBuffer; cdecl; external gtklib;
function gtk_text_view_scroll_to_iter(text_view:PGtkTextView; iter:PGtkTextIter; within_margin:gdouble; use_align:gboolean; xalign:gdouble;
           yalign:gdouble):gboolean; cdecl; external gtklib;
procedure gtk_text_view_scroll_to_mark(text_view:PGtkTextView; mark:PGtkTextMark; within_margin:gdouble; use_align:gboolean; xalign:gdouble;
            yalign:gdouble); cdecl; external gtklib;
procedure gtk_text_view_scroll_mark_onscreen(text_view:PGtkTextView; mark:PGtkTextMark); cdecl; external gtklib;
function gtk_text_view_move_mark_onscreen(text_view:PGtkTextView; mark:PGtkTextMark):gboolean; cdecl; external gtklib;
function gtk_text_view_place_cursor_onscreen(text_view:PGtkTextView):gboolean; cdecl; external gtklib;
procedure gtk_text_view_get_visible_rect(text_view:PGtkTextView; visible_rect:PGdkRectangle); cdecl; external gtklib;
procedure gtk_text_view_set_cursor_visible(text_view:PGtkTextView; setting:gboolean); cdecl; external gtklib;
function gtk_text_view_get_cursor_visible(text_view:PGtkTextView):gboolean; cdecl; external gtklib;
procedure gtk_text_view_get_iter_location(text_view:PGtkTextView; iter:PGtkTextIter; location:PGdkRectangle); cdecl; external gtklib;
procedure gtk_text_view_get_iter_at_location(text_view:PGtkTextView; iter:PGtkTextIter; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_text_view_get_line_yrange(text_view:PGtkTextView; iter:PGtkTextIter; y:Pgint; height:Pgint); cdecl; external gtklib;
procedure gtk_text_view_get_line_at_y(text_view:PGtkTextView; target_iter:PGtkTextIter; y:gint; line_top:Pgint); cdecl; external gtklib;
procedure gtk_text_view_buffer_to_window_coords(text_view:PGtkTextView; win:TGtkTextWindowType; buffer_x:gint; buffer_y:gint; window_x:Pgint;
            window_y:Pgint); cdecl; external gtklib;
procedure gtk_text_view_window_to_buffer_coords(text_view:PGtkTextView; win:TGtkTextWindowType; window_x:gint; window_y:gint; buffer_x:Pgint;
            buffer_y:Pgint); cdecl; external gtklib;
function gtk_text_view_get_window(text_view:PGtkTextView; win:TGtkTextWindowType):PGdkWindow; cdecl; external gtklib;
function gtk_text_view_get_window_type(text_view:PGtkTextView; window:PGdkWindow):TGtkTextWindowType; cdecl; external gtklib;
procedure gtk_text_view_set_border_window_size(text_view:PGtkTextView; _type:TGtkTextWindowType; size:gint); cdecl; external gtklib;
function gtk_text_view_get_border_window_size(text_view:PGtkTextView; _type:TGtkTextWindowType):gint; cdecl; external gtklib;
function gtk_text_view_forward_display_line(text_view:PGtkTextView; iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_view_backward_display_line(text_view:PGtkTextView; iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_view_forward_display_line_end(text_view:PGtkTextView; iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_view_backward_display_line_start(text_view:PGtkTextView; iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_view_starts_display_line(text_view:PGtkTextView; iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function gtk_text_view_move_visually(text_view:PGtkTextView; iter:PGtkTextIter; count:gint):gboolean; cdecl; external gtklib;
{ Adding child widgets  }
procedure gtk_text_view_add_child_at_anchor(text_view:PGtkTextView; child:PGtkWidget; anchor:PGtkTextChildAnchor); cdecl; external gtklib;
{ window coordinates  }
procedure gtk_text_view_add_child_in_window(text_view:PGtkTextView; child:PGtkWidget; which_window:TGtkTextWindowType; xpos:gint; ypos:gint); cdecl; external gtklib;
{ window coordinates  }
procedure gtk_text_view_move_child(text_view:PGtkTextView; child:PGtkWidget; xpos:gint; ypos:gint); cdecl; external gtklib;
{ Default style settings (fallbacks if no tag affects the property)  }
procedure gtk_text_view_set_wrap_mode(text_view:PGtkTextView; wrap_mode:TGtkWrapMode); cdecl; external gtklib;
function gtk_text_view_get_wrap_mode(text_view:PGtkTextView):TGtkWrapMode; cdecl; external gtklib;
procedure gtk_text_view_set_editable(text_view:PGtkTextView; setting:gboolean); cdecl; external gtklib;
function gtk_text_view_get_editable(text_view:PGtkTextView):gboolean; cdecl; external gtklib;
procedure gtk_text_view_set_pixels_above_lines(text_view:PGtkTextView; pixels_above_lines:gint); cdecl; external gtklib;
function gtk_text_view_get_pixels_above_lines(text_view:PGtkTextView):gint; cdecl; external gtklib;
procedure gtk_text_view_set_pixels_below_lines(text_view:PGtkTextView; pixels_below_lines:gint); cdecl; external gtklib;
function gtk_text_view_get_pixels_below_lines(text_view:PGtkTextView):gint; cdecl; external gtklib;
procedure gtk_text_view_set_pixels_inside_wrap(text_view:PGtkTextView; pixels_inside_wrap:gint); cdecl; external gtklib;
function gtk_text_view_get_pixels_inside_wrap(text_view:PGtkTextView):gint; cdecl; external gtklib;
procedure gtk_text_view_set_justification(text_view:PGtkTextView; justification:TGtkJustification); cdecl; external gtklib;
function gtk_text_view_get_justification(text_view:PGtkTextView):TGtkJustification; cdecl; external gtklib;
procedure gtk_text_view_set_left_margin(text_view:PGtkTextView; left_margin:gint); cdecl; external gtklib;
function gtk_text_view_get_left_margin(text_view:PGtkTextView):gint; cdecl; external gtklib;
procedure gtk_text_view_set_right_margin(text_view:PGtkTextView; right_margin:gint); cdecl; external gtklib;
function gtk_text_view_get_right_margin(text_view:PGtkTextView):gint; cdecl; external gtklib;
procedure gtk_text_view_set_indent(text_view:PGtkTextView; indent:gint); cdecl; external gtklib;
function gtk_text_view_get_indent(text_view:PGtkTextView):gint; cdecl; external gtklib;
procedure gtk_text_view_set_tabs(text_view:PGtkTextView; tabs:PPangoTabArray); cdecl; external gtklib;
function gtk_text_view_get_tabs(text_view:PGtkTextView):PPangoTabArray; cdecl; external gtklib;
{ note that the return value of this changes with the theme  }
function gtk_text_view_get_default_attributes(text_view:PGtkTextView):PGtkTextAttributes; cdecl; external gtklib;

{$IFDEF HasGTK2_4}
procedure gtk_text_view_set_overwrite(text_view: PGtkTextView; overwrite: gboolean ); cdecl; external gtklib;
function gtk_text_view_get_overwrite(text_view: PGtkTextView):gboolean; cdecl; external gtklib;
procedure gtk_text_view_set_accepts_tab(text_view: PGtkTextView; accepts_tab: gboolean );     cdecl; external gtklib;
function  gtk_text_view_get_accepts_tab(text_view: PGtkTextView): gboolean; cdecl; external gtklib;
{$ENDIF HasGTK2_4}

{$ifdef HasGTK2_6}
procedure gtk_text_view_get_iter_at_position(text_view:PGtkTextView; iter:PGtkTextIter; trailing:Pgint; x:gint; y:gint); cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_VIEW : GType;
begin
   GTK_TYPE_TEXT_VIEW:=gtk_text_view_get_type;
end;

function GTK_TEXT_VIEW(obj: pointer) : PGtkTextView;
begin
   GTK_TEXT_VIEW:=PGtkTextView(GTK_CHECK_CAST(obj,GTK_TYPE_TEXT_VIEW));
end;

function GTK_TEXT_VIEW_CLASS(klass: pointer) : PGtkTextViewClass;
begin
   GTK_TEXT_VIEW_CLASS:=PGtkTextViewClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT_VIEW));
end;

function GTK_IS_TEXT_VIEW(obj: pointer) : boolean;
begin
   GTK_IS_TEXT_VIEW:=GTK_CHECK_TYPE(obj,GTK_TYPE_TEXT_VIEW);
end;

function GTK_IS_TEXT_VIEW_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_VIEW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT_VIEW);
end;

function GTK_TEXT_VIEW_GET_CLASS(obj: pointer) : PGtkTextViewClass;
begin
   GTK_TEXT_VIEW_GET_CLASS:=PGtkTextViewClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TEXT_VIEW));
end;

function editable(var a : TGtkTextView) : guint;
begin
   editable:=(a.flag0 and bm_TGtkTextView_editable) shr bp_TGtkTextView_editable;
end;

procedure set_editable(var a : TGtkTextView; __editable : guint);
begin
   a.flag0:=a.flag0 or ((__editable shl bp_TGtkTextView_editable) and bm_TGtkTextView_editable);
end;

function overwrite_mode(var a : TGtkTextView) : guint;
begin
   overwrite_mode:=(a.flag0 and bm_TGtkTextView_overwrite_mode) shr bp_TGtkTextView_overwrite_mode;
end;

procedure set_overwrite_mode(var a : TGtkTextView; __overwrite_mode : guint);
begin
   a.flag0:=a.flag0 or ((__overwrite_mode shl bp_TGtkTextView_overwrite_mode) and bm_TGtkTextView_overwrite_mode);
end;

function cursor_visible(var a : TGtkTextView) : guint;
begin
   cursor_visible:=(a.flag0 and bm_TGtkTextView_cursor_visible) shr bp_TGtkTextView_cursor_visible;
end;

procedure set_cursor_visible(var a : TGtkTextView; __cursor_visible : guint);
begin
   a.flag0:=a.flag0 or ((__cursor_visible shl bp_TGtkTextView_cursor_visible) and bm_TGtkTextView_cursor_visible);
end;

function need_im_reset(var a : TGtkTextView) : guint;
begin
   need_im_reset:=(a.flag0 and bm_TGtkTextView_need_im_reset) shr bp_TGtkTextView_need_im_reset;
end;

procedure set_need_im_reset(var a : TGtkTextView; __need_im_reset : guint);
begin
   a.flag0:=a.flag0 or ((__need_im_reset shl bp_TGtkTextView_need_im_reset) and bm_TGtkTextView_need_im_reset);
end;

function just_selected_element(var a : TGtkTextView) : guint;
begin
   just_selected_element:=(a.flag0 and bm_TGtkTextView_just_selected_element) shr bp_TGtkTextView_just_selected_element;
end;

procedure set_just_selected_element(var a : TGtkTextView; __just_selected_element : guint);
begin
   a.flag0:=a.flag0 or ((__just_selected_element shl bp_TGtkTextView_just_selected_element) and bm_TGtkTextView_just_selected_element);
end;

function disable_scroll_on_focus(var a : TGtkTextView) : guint;
begin
   disable_scroll_on_focus:=(a.flag0 and bm_TGtkTextView_disable_scroll_on_focus) shr bp_TGtkTextView_disable_scroll_on_focus;
end;

procedure set_disable_scroll_on_focus(var a : TGtkTextView; __disable_scroll_on_focus : guint);
begin
   a.flag0:=a.flag0 or ((__disable_scroll_on_focus shl bp_TGtkTextView_disable_scroll_on_focus) and bm_TGtkTextView_disable_scroll_on_focus);
end;

function onscreen_validated(var a : TGtkTextView) : guint;
begin
   onscreen_validated:=(a.flag0 and bm_TGtkTextView_onscreen_validated) shr bp_TGtkTextView_onscreen_validated;
end;

procedure set_onscreen_validated(var a : TGtkTextView; __onscreen_validated : guint);
begin
   a.flag0:=a.flag0 or ((__onscreen_validated shl bp_TGtkTextView_onscreen_validated) and bm_TGtkTextView_onscreen_validated);
end;

function mouse_cursor_obscured(var a : TGtkTextView) : guint;
begin
   mouse_cursor_obscured:=(a.flag0 and bm_TGtkTextView_mouse_cursor_obscured) shr bp_TGtkTextView_mouse_cursor_obscured;
end;

procedure set_mouse_cursor_obscured(var a : TGtkTextView; __mouse_cursor_obscured : guint);
begin
   a.flag0:=a.flag0 or ((__mouse_cursor_obscured shl bp_TGtkTextView_mouse_cursor_obscured) and bm_TGtkTextView_mouse_cursor_obscured);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

