// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTextTagClass = ^TGtkTextTagClass;
   PGtkTextAttributes = ^TGtkTextAttributes;

{ Name of this tag.  This field is actually
                                   a pointer to the key from the entry in
                                   tkxt->tagTable, so it needn't be freed
                                   explicitly.  }
{ Priority of this tag within widget.  0
                           means lowest priority.  Exactly one tag
                           has each integer value between 0 and
                           numTags-1.  }
{
     Information for displaying text with this tag.  The information
     belows acts as an override on information specified by lower-priority
     tags.  If no value is specified, then the next-lower-priority tag
     on the text determins the value.  The text widget itself provides
     defaults if no tag specifies an override.
    }
{ Flags for whether a given value is set; if a value is unset, then
     this tag does not affect it.
    }
   PGtkTextTag = ^TGtkTextTag;
   PPGtkTextTag = ^PGtkTextTag;
   TGtkTextTag = record
        parent_instance : TGObject;
        table : PGtkTextTagTable;
        name : Pchar;
        priority : longint;
        values : PGtkTextAttributes;
        flag0 : longint;
     end;


{ widget, canvas item, whatever  }
{ the event itself  }

{ location of event in buffer  }
{ Padding for future expansion  }
   TGtkTextTagClass = record
        parent_class : TGObjectClass;
        event : function (tag:PGtkTextTag; event_object:PGObject; event:PGdkEvent; iter:PGtkTextIter):gboolean; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{
   Style anObject created by folding a set of tags together
  }
{ super/subscript rise, can be negative  }
{ I'm not sure this can really be used without breaking some things
     an app might do :-/
    }
{ PangoUnderline  }
{ Whether to use background-related values; this is irrelevant for
     the values struct when in a tag, but is used for the composite
     values struct; it's true if any of the tags being composited
     had background stuff set.
    }
{ These are only used when we are actually laying out and rendering
     a paragraph; not when a GtkTextAppearance is part of a
     GtkTextAttributes.
    }
   PGtkTextAppearance = ^TGtkTextAppearance;
   TGtkTextAppearance = record
        bg_color : TGdkColor;
        fg_color : TGdkColor;
        bg_stipple : PGdkBitmap;
        fg_stipple : PGdkBitmap;
        rise : gint;
        padding1 : gpointer;
        flag0 : word;
     end;

{ Individual chunks of this can be set/unset as a group  }
{ How to handle wrap-around for this tag.
                                   Must be GTK_WRAPMODE_CHAR,
                                   GTK_WRAPMODE_NONE, GTK_WRAPMODE_WORD
                                  }
{ I'm not sure this can really be used without breaking some things
     an app might do :-/
    }
{ hide the text   }
{ Background is fit to full line height rather than
     baseline +/- ascent/descent (font height)
    }
{ can edit this text  }
{ colors are allocated etc.  }
   TGtkTextAttributes = record
        refcount : guint;
        appearance : TGtkTextAppearance;
        justification : TGtkJustification;
        direction : TGtkTextDirection;
        font : PPangoFontDescription;
        font_scale : gdouble;
        left_margin : gint;
        indent : gint;
        right_margin : gint;
        pixels_above_lines : gint;
        pixels_below_lines : gint;
        pixels_inside_wrap : gint;
        tabs : PPangoTabArray;
        wrap_mode : TGtkWrapMode;
        language : PPangoLanguage;
        padding1 : gpointer;
        flag0 : word;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TEXT_TAG : GType;
function GTK_TEXT_TAG(obj: pointer) : PGtkTextTag;
function GTK_TEXT_TAG_CLASS(klass: pointer) : PGtkTextTagClass;
function GTK_IS_TEXT_TAG(obj: pointer) : boolean;
function GTK_IS_TEXT_TAG_CLASS(klass: pointer) : boolean;
function GTK_TEXT_TAG_GET_CLASS(obj: pointer) : PGtkTextTagClass;
function GTK_TYPE_TEXT_ATTRIBUTES : GType;

function gtk_text_tag_get_type:GType; cdecl; external gtklib;

function gtk_text_tag_new(name:Pgchar):PGtkTextTag; cdecl; external gtklib;
function gtk_text_tag_get_priority(tag:PGtkTextTag):gint; cdecl; external gtklib;
procedure gtk_text_tag_set_priority(tag:PGtkTextTag; priority:gint); cdecl; external gtklib;

function gtk_text_tag_event(tag:PGtkTextTag; event_object:PGObject; event:PGdkEvent; iter:PGtkTextIter):gboolean; cdecl; external gtklib;

function gtk_text_attributes_new:PGtkTextAttributes; cdecl; external gtklib;
function gtk_text_attributes_copy(src:PGtkTextAttributes):PGtkTextAttributes; cdecl; external gtklib;
procedure gtk_text_attributes_copy_values(src:PGtkTextAttributes; dest:PGtkTextAttributes); cdecl; external gtklib;
procedure gtk_text_attributes_unref(values:PGtkTextAttributes); cdecl; external gtklib;
procedure gtk_text_attributes_ref(values:PGtkTextAttributes); cdecl; external gtklib;
function gtk_text_attributes_get_type:GType; cdecl; external gtklib;

const
   bm_TGtkTextTag_bg_color_set = $1;
   bp_TGtkTextTag_bg_color_set = 0;
   bm_TGtkTextTag_bg_stipple_set = $2;
   bp_TGtkTextTag_bg_stipple_set = 1;
   bm_TGtkTextTag_fg_color_set = $4;
   bp_TGtkTextTag_fg_color_set = 2;
   bm_TGtkTextTag_scale_set = $8;
   bp_TGtkTextTag_scale_set = 3;
   bm_TGtkTextTag_fg_stipple_set = $10;
   bp_TGtkTextTag_fg_stipple_set = 4;
   bm_TGtkTextTag_justification_set = $20;
   bp_TGtkTextTag_justification_set = 5;
   bm_TGtkTextTag_left_margin_set = $40;
   bp_TGtkTextTag_left_margin_set = 6;
   bm_TGtkTextTag_indent_set = $80;
   bp_TGtkTextTag_indent_set = 7;
   bm_TGtkTextTag_rise_set = $100;
   bp_TGtkTextTag_rise_set = 8;
   bm_TGtkTextTag_strikethrough_set = $200;
   bp_TGtkTextTag_strikethrough_set = 9;
   bm_TGtkTextTag_right_margin_set = $400;
   bp_TGtkTextTag_right_margin_set = 10;
   bm_TGtkTextTag_pixels_above_lines_set = $800;
   bp_TGtkTextTag_pixels_above_lines_set = 11;
   bm_TGtkTextTag_pixels_below_lines_set = $1000;
   bp_TGtkTextTag_pixels_below_lines_set = 12;
   bm_TGtkTextTag_pixels_inside_wrap_set = $2000;
   bp_TGtkTextTag_pixels_inside_wrap_set = 13;
   bm_TGtkTextTag_tabs_set = $4000;
   bp_TGtkTextTag_tabs_set = 14;
   bm_TGtkTextTag_underline_set = $8000;
   bp_TGtkTextTag_underline_set = 15;
   bm_TGtkTextTag_wrap_mode_set = $10000;
   bp_TGtkTextTag_wrap_mode_set = 16;
   bm_TGtkTextTag_bg_full_height_set = $20000;
   bp_TGtkTextTag_bg_full_height_set = 17;
   bm_TGtkTextTag_invisible_set = $40000;
   bp_TGtkTextTag_invisible_set = 18;
   bm_TGtkTextTag_editable_set = $80000;
   bp_TGtkTextTag_editable_set = 19;
   bm_TGtkTextTag_language_set = $100000;
   bp_TGtkTextTag_language_set = 20;
   bm_TGtkTextTag_pad1 = $200000;
   bp_TGtkTextTag_pad1 = 21;
   bm_TGtkTextTag_pad2 = $400000;
   bp_TGtkTextTag_pad2 = 22;
   bm_TGtkTextTag_pad3 = $800000;
   bp_TGtkTextTag_pad3 = 23;

function bg_color_set(var a : TGtkTextTag) : guint;
procedure set_bg_color_set(var a : TGtkTextTag; __bg_color_set : guint);
function bg_stipple_set(var a : TGtkTextTag) : guint;
procedure set_bg_stipple_set(var a : TGtkTextTag; __bg_stipple_set : guint);
function fg_color_set(var a : TGtkTextTag) : guint;
procedure set_fg_color_set(var a : TGtkTextTag; __fg_color_set : guint);
function scale_set(var a : TGtkTextTag) : guint; overload;
procedure set_scale_set(var a : TGtkTextTag; __scale_set : guint); overload;
function fg_stipple_set(var a : TGtkTextTag) : guint;
procedure set_fg_stipple_set(var a : TGtkTextTag; __fg_stipple_set : guint);
function justification_set(var a : TGtkTextTag) : guint;
procedure set_justification_set(var a : TGtkTextTag; __justification_set : guint);
function left_margin_set(var a : TGtkTextTag) : guint;
procedure set_left_margin_set(var a : TGtkTextTag; __left_margin_set : guint);
function indent_set(var a : TGtkTextTag) : guint;
procedure set_indent_set(var a : TGtkTextTag; __indent_set : guint);
function rise_set(var a : TGtkTextTag) : guint; overload;
procedure set_rise_set(var a : TGtkTextTag; __rise_set : guint); overload;
function strikethrough_set(var a : TGtkTextTag) : guint; overload;
procedure set_strikethrough_set(var a : TGtkTextTag; __strikethrough_set : guint); overload;
function right_margin_set(var a : TGtkTextTag) : guint;
procedure set_right_margin_set(var a : TGtkTextTag; __right_margin_set : guint);
function pixels_above_lines_set(var a : TGtkTextTag) : guint;
procedure set_pixels_above_lines_set(var a : TGtkTextTag; __pixels_above_lines_set : guint);
function pixels_below_lines_set(var a : TGtkTextTag) : guint;
procedure set_pixels_below_lines_set(var a : TGtkTextTag; __pixels_below_lines_set : guint);
function pixels_inside_wrap_set(var a : TGtkTextTag) : guint;
procedure set_pixels_inside_wrap_set(var a : TGtkTextTag; __pixels_inside_wrap_set : guint);
function tabs_set(var a : TGtkTextTag) : guint;
procedure set_tabs_set(var a : TGtkTextTag; __tabs_set : guint);
function underline_set(var a : TGtkTextTag) : guint; overload;
procedure set_underline_set(var a : TGtkTextTag; __underline_set : guint); overload;
function wrap_mode_set(var a : TGtkTextTag) : guint;
procedure set_wrap_mode_set(var a : TGtkTextTag; __wrap_mode_set : guint);
function bg_full_height_set(var a : TGtkTextTag) : guint;
procedure set_bg_full_height_set(var a : TGtkTextTag; __bg_full_height_set : guint);
function invisible_set(var a : TGtkTextTag) : guint;
procedure set_invisible_set(var a : TGtkTextTag; __invisible_set : guint);
function editable_set(var a : TGtkTextTag) : guint; overload;
procedure set_editable_set(var a : TGtkTextTag; __editable_set : guint); overload;
function language_set(var a : TGtkTextTag) : guint;
procedure set_language_set(var a : TGtkTextTag; __language_set : guint);
function pad1(var a : TGtkTextTag) : guint; overload;
procedure set_pad1(var a : TGtkTextTag; __pad1 : guint); overload;
function pad2(var a : TGtkTextTag) : guint; overload;
procedure set_pad2(var a : TGtkTextTag; __pad2 : guint); overload;
function pad3(var a : TGtkTextTag) : guint; overload;
procedure set_pad3(var a : TGtkTextTag; __pad3 : guint); overload;

const
   bm_TGtkTextAppearance_underline = $F;
   bp_TGtkTextAppearance_underline = 0;
   bm_TGtkTextAppearance_strikethrough = $10;
   bp_TGtkTextAppearance_strikethrough = 4;
   bm_TGtkTextAppearance_draw_bg = $20;
   bp_TGtkTextAppearance_draw_bg = 5;
   bm_TGtkTextAppearance_inside_selection = $40;
   bp_TGtkTextAppearance_inside_selection = 6;
   bm_TGtkTextAppearance_is_text = $80;
   bp_TGtkTextAppearance_is_text = 7;
   bm_TGtkTextAppearance_pad1 = $100;
   bp_TGtkTextAppearance_pad1 = 8;
   bm_TGtkTextAppearance_pad2 = $200;
   bp_TGtkTextAppearance_pad2 = 9;
   bm_TGtkTextAppearance_pad3 = $400;
   bp_TGtkTextAppearance_pad3 = 10;
   bm_TGtkTextAppearance_pad4 = $800;
   bp_TGtkTextAppearance_pad4 = 11;

function underline(var a : TGtkTextAppearance) : guint;
procedure set_underline(var a : TGtkTextAppearance; __underline : guint);
function strikethrough(var a : TGtkTextAppearance) : guint; overload;
procedure set_strikethrough(var a : TGtkTextAppearance; __strikethrough : guint); overload;
function draw_bg(var a : TGtkTextAppearance) : guint;
procedure set_draw_bg(var a : TGtkTextAppearance; __draw_bg : guint);
function inside_selection(var a : TGtkTextAppearance) : guint;
procedure set_inside_selection(var a : TGtkTextAppearance; __inside_selection : guint);
function is_text(var a : TGtkTextAppearance) : guint;
procedure set_is_text(var a : TGtkTextAppearance; __is_text : guint);
function pad1(var a : TGtkTextAppearance) : guint; overload;
procedure set_pad1(var a : TGtkTextAppearance; __pad1 : guint); overload;
function pad2(var a : TGtkTextAppearance) : guint; overload;
procedure set_pad2(var a : TGtkTextAppearance; __pad2 : guint); overload;
function pad3(var a : TGtkTextAppearance) : guint; overload;
procedure set_pad3(var a : TGtkTextAppearance; __pad3 : guint); overload;
function pad4(var a : TGtkTextAppearance) : guint; overload;
procedure set_pad4(var a : TGtkTextAppearance; __pad4 : guint); overload;

const
   bm_TGtkTextAttributes_invisible = $1;
   bp_TGtkTextAttributes_invisible = 0;
   bm_TGtkTextAttributes_bg_full_height = $2;
   bp_TGtkTextAttributes_bg_full_height = 1;
   bm_TGtkTextAttributes_editable = $4;
   bp_TGtkTextAttributes_editable = 2;
   bm_TGtkTextAttributes_realized = $8;
   bp_TGtkTextAttributes_realized = 3;
   bm_TGtkTextAttributes_pad1 = $10;
   bp_TGtkTextAttributes_pad1 = 4;
   bm_TGtkTextAttributes_pad2 = $20;
   bp_TGtkTextAttributes_pad2 = 5;
   bm_TGtkTextAttributes_pad3 = $40;
   bp_TGtkTextAttributes_pad3 = 6;
   bm_TGtkTextAttributes_pad4 = $80;
   bp_TGtkTextAttributes_pad4 = 7;

function invisible(var a : TGtkTextAttributes) : guint;
procedure set_invisible(var a : TGtkTextAttributes; __invisible : guint);
function bg_full_height(var a : TGtkTextAttributes) : guint;
procedure set_bg_full_height(var a : TGtkTextAttributes; __bg_full_height : guint);
function editable(var a : TGtkTextAttributes) : guint; overload;
procedure set_editable(var a : TGtkTextAttributes; __editable : guint); overload;
function realized(var a : TGtkTextAttributes) : guint;
procedure set_realized(var a : TGtkTextAttributes; __realized : guint);
function pad1(var a : TGtkTextAttributes) : guint; overload;
procedure set_pad1(var a : TGtkTextAttributes; __pad1 : guint); overload;
function pad2(var a : TGtkTextAttributes) : guint; overload;
procedure set_pad2(var a : TGtkTextAttributes; __pad2 : guint); overload;
function pad3(var a : TGtkTextAttributes) : guint; overload;
procedure set_pad3(var a : TGtkTextAttributes; __pad3 : guint); overload;
function pad4(var a : TGtkTextAttributes) : guint; overload;
procedure set_pad4(var a : TGtkTextAttributes; __pad4 : guint); overload;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_TAG : GType;
begin
   GTK_TYPE_TEXT_TAG:=gtk_text_tag_get_type;
end;

function GTK_TEXT_TAG(obj: pointer) : PGtkTextTag;
begin
   GTK_TEXT_TAG:=PGtkTextTag(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TEXT_TAG));
end;

function GTK_TEXT_TAG_CLASS(klass: pointer) : PGtkTextTagClass;
begin
   GTK_TEXT_TAG_CLASS:=PGtkTextTagClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT_TAG));
end;

function GTK_IS_TEXT_TAG(obj: pointer) : boolean;
begin
   GTK_IS_TEXT_TAG:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TEXT_TAG);
end;

function GTK_IS_TEXT_TAG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_TAG_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT_TAG);
end;

function GTK_TEXT_TAG_GET_CLASS(obj: pointer) : PGtkTextTagClass;
begin
   GTK_TEXT_TAG_GET_CLASS:=PGtkTextTagClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TEXT_TAG));
end;

function GTK_TYPE_TEXT_ATTRIBUTES : GType;
begin
   GTK_TYPE_TEXT_ATTRIBUTES:=gtk_text_attributes_get_type;
end;

function bg_color_set(var a : TGtkTextTag) : guint;
begin
   bg_color_set:=(a.flag0 and bm_TGtkTextTag_bg_color_set) shr bp_TGtkTextTag_bg_color_set;
end;

procedure set_bg_color_set(var a : TGtkTextTag; __bg_color_set : guint);
begin
   a.flag0:=a.flag0 or ((__bg_color_set shl bp_TGtkTextTag_bg_color_set) and bm_TGtkTextTag_bg_color_set);
end;

function bg_stipple_set(var a : TGtkTextTag) : guint;
begin
   bg_stipple_set:=(a.flag0 and bm_TGtkTextTag_bg_stipple_set) shr bp_TGtkTextTag_bg_stipple_set;
end;

procedure set_bg_stipple_set(var a : TGtkTextTag; __bg_stipple_set : guint);
begin
   a.flag0:=a.flag0 or ((__bg_stipple_set shl bp_TGtkTextTag_bg_stipple_set) and bm_TGtkTextTag_bg_stipple_set);
end;

function fg_color_set(var a : TGtkTextTag) : guint;
begin
   fg_color_set:=(a.flag0 and bm_TGtkTextTag_fg_color_set) shr bp_TGtkTextTag_fg_color_set;
end;

procedure set_fg_color_set(var a : TGtkTextTag; __fg_color_set : guint);
begin
   a.flag0:=a.flag0 or ((__fg_color_set shl bp_TGtkTextTag_fg_color_set) and bm_TGtkTextTag_fg_color_set);
end;

function scale_set(var a : TGtkTextTag) : guint;
begin
   scale_set:=(a.flag0 and bm_TGtkTextTag_scale_set) shr bp_TGtkTextTag_scale_set;
end;

procedure set_scale_set(var a : TGtkTextTag; __scale_set : guint);
begin
   a.flag0:=a.flag0 or ((__scale_set shl bp_TGtkTextTag_scale_set) and bm_TGtkTextTag_scale_set);
end;

function fg_stipple_set(var a : TGtkTextTag) : guint;
begin
   fg_stipple_set:=(a.flag0 and bm_TGtkTextTag_fg_stipple_set) shr bp_TGtkTextTag_fg_stipple_set;
end;

procedure set_fg_stipple_set(var a : TGtkTextTag; __fg_stipple_set : guint);
begin
   a.flag0:=a.flag0 or ((__fg_stipple_set shl bp_TGtkTextTag_fg_stipple_set) and bm_TGtkTextTag_fg_stipple_set);
end;

function justification_set(var a : TGtkTextTag) : guint;
begin
   justification_set:=(a.flag0 and bm_TGtkTextTag_justification_set) shr bp_TGtkTextTag_justification_set;
end;

procedure set_justification_set(var a : TGtkTextTag; __justification_set : guint);
begin
   a.flag0:=a.flag0 or ((__justification_set shl bp_TGtkTextTag_justification_set) and bm_TGtkTextTag_justification_set);
end;

function left_margin_set(var a : TGtkTextTag) : guint;
begin
   left_margin_set:=(a.flag0 and bm_TGtkTextTag_left_margin_set) shr bp_TGtkTextTag_left_margin_set;
end;

procedure set_left_margin_set(var a : TGtkTextTag; __left_margin_set : guint);
begin
   a.flag0:=a.flag0 or ((__left_margin_set shl bp_TGtkTextTag_left_margin_set) and bm_TGtkTextTag_left_margin_set);
end;

function indent_set(var a : TGtkTextTag) : guint;
begin
   indent_set:=(a.flag0 and bm_TGtkTextTag_indent_set) shr bp_TGtkTextTag_indent_set;
end;

procedure set_indent_set(var a : TGtkTextTag; __indent_set : guint);
begin
   a.flag0:=a.flag0 or ((__indent_set shl bp_TGtkTextTag_indent_set) and bm_TGtkTextTag_indent_set);
end;

function rise_set(var a : TGtkTextTag) : guint;
begin
   rise_set:=(a.flag0 and bm_TGtkTextTag_rise_set) shr bp_TGtkTextTag_rise_set;
end;

procedure set_rise_set(var a : TGtkTextTag; __rise_set : guint);
begin
   a.flag0:=a.flag0 or ((__rise_set shl bp_TGtkTextTag_rise_set) and bm_TGtkTextTag_rise_set);
end;

function strikethrough_set(var a : TGtkTextTag) : guint;
begin
   strikethrough_set:=(a.flag0 and bm_TGtkTextTag_strikethrough_set) shr bp_TGtkTextTag_strikethrough_set;
end;

procedure set_strikethrough_set(var a : TGtkTextTag; __strikethrough_set : guint);
begin
   a.flag0:=a.flag0 or ((__strikethrough_set shl bp_TGtkTextTag_strikethrough_set) and bm_TGtkTextTag_strikethrough_set);
end;

function right_margin_set(var a : TGtkTextTag) : guint;
begin
   right_margin_set:=(a.flag0 and bm_TGtkTextTag_right_margin_set) shr bp_TGtkTextTag_right_margin_set;
end;

procedure set_right_margin_set(var a : TGtkTextTag; __right_margin_set : guint);
begin
   a.flag0:=a.flag0 or ((__right_margin_set shl bp_TGtkTextTag_right_margin_set) and bm_TGtkTextTag_right_margin_set);
end;

function pixels_above_lines_set(var a : TGtkTextTag) : guint;
begin
   pixels_above_lines_set:=(a.flag0 and bm_TGtkTextTag_pixels_above_lines_set) shr bp_TGtkTextTag_pixels_above_lines_set;
end;

procedure set_pixels_above_lines_set(var a : TGtkTextTag; __pixels_above_lines_set : guint);
begin
   a.flag0:=a.flag0 or ((__pixels_above_lines_set shl bp_TGtkTextTag_pixels_above_lines_set) and bm_TGtkTextTag_pixels_above_lines_set);
end;

function pixels_below_lines_set(var a : TGtkTextTag) : guint;
begin
   pixels_below_lines_set:=(a.flag0 and bm_TGtkTextTag_pixels_below_lines_set) shr bp_TGtkTextTag_pixels_below_lines_set;
end;

procedure set_pixels_below_lines_set(var a : TGtkTextTag; __pixels_below_lines_set : guint);
begin
   a.flag0:=a.flag0 or ((__pixels_below_lines_set shl bp_TGtkTextTag_pixels_below_lines_set) and bm_TGtkTextTag_pixels_below_lines_set);
end;

function pixels_inside_wrap_set(var a : TGtkTextTag) : guint;
begin
   pixels_inside_wrap_set:=(a.flag0 and bm_TGtkTextTag_pixels_inside_wrap_set) shr bp_TGtkTextTag_pixels_inside_wrap_set;
end;

procedure set_pixels_inside_wrap_set(var a : TGtkTextTag; __pixels_inside_wrap_set : guint);
begin
   a.flag0:=a.flag0 or ((__pixels_inside_wrap_set shl bp_TGtkTextTag_pixels_inside_wrap_set) and bm_TGtkTextTag_pixels_inside_wrap_set);
end;

function tabs_set(var a : TGtkTextTag) : guint;
begin
   tabs_set:=(a.flag0 and bm_TGtkTextTag_tabs_set) shr bp_TGtkTextTag_tabs_set;
end;

procedure set_tabs_set(var a : TGtkTextTag; __tabs_set : guint);
begin
   a.flag0:=a.flag0 or ((__tabs_set shl bp_TGtkTextTag_tabs_set) and bm_TGtkTextTag_tabs_set);
end;

function underline_set(var a : TGtkTextTag) : guint;
begin
   underline_set:=(a.flag0 and bm_TGtkTextTag_underline_set) shr bp_TGtkTextTag_underline_set;
end;

procedure set_underline_set(var a : TGtkTextTag; __underline_set : guint);
begin
   a.flag0:=a.flag0 or ((__underline_set shl bp_TGtkTextTag_underline_set) and bm_TGtkTextTag_underline_set);
end;

function wrap_mode_set(var a : TGtkTextTag) : guint;
begin
   wrap_mode_set:=(a.flag0 and bm_TGtkTextTag_wrap_mode_set) shr bp_TGtkTextTag_wrap_mode_set;
end;

procedure set_wrap_mode_set(var a : TGtkTextTag; __wrap_mode_set : guint);
begin
   a.flag0:=a.flag0 or ((__wrap_mode_set shl bp_TGtkTextTag_wrap_mode_set) and bm_TGtkTextTag_wrap_mode_set);
end;

function bg_full_height_set(var a : TGtkTextTag) : guint;
begin
   bg_full_height_set:=(a.flag0 and bm_TGtkTextTag_bg_full_height_set) shr bp_TGtkTextTag_bg_full_height_set;
end;

procedure set_bg_full_height_set(var a : TGtkTextTag; __bg_full_height_set : guint);
begin
   a.flag0:=a.flag0 or ((__bg_full_height_set shl bp_TGtkTextTag_bg_full_height_set) and bm_TGtkTextTag_bg_full_height_set);
end;

function invisible_set(var a : TGtkTextTag) : guint;
begin
   invisible_set:=(a.flag0 and bm_TGtkTextTag_invisible_set) shr bp_TGtkTextTag_invisible_set;
end;

procedure set_invisible_set(var a : TGtkTextTag; __invisible_set : guint);
begin
   a.flag0:=a.flag0 or ((__invisible_set shl bp_TGtkTextTag_invisible_set) and bm_TGtkTextTag_invisible_set);
end;

function editable_set(var a : TGtkTextTag) : guint;
begin
   editable_set:=(a.flag0 and bm_TGtkTextTag_editable_set) shr bp_TGtkTextTag_editable_set;
end;

procedure set_editable_set(var a : TGtkTextTag; __editable_set : guint);
begin
   a.flag0:=a.flag0 or ((__editable_set shl bp_TGtkTextTag_editable_set) and bm_TGtkTextTag_editable_set);
end;

function language_set(var a : TGtkTextTag) : guint;
begin
   language_set:=(a.flag0 and bm_TGtkTextTag_language_set) shr bp_TGtkTextTag_language_set;
end;

procedure set_language_set(var a : TGtkTextTag; __language_set : guint);
begin
   a.flag0:=a.flag0 or ((__language_set shl bp_TGtkTextTag_language_set) and bm_TGtkTextTag_language_set);
end;

function pad1(var a : TGtkTextTag) : guint;
begin
   pad1:=(a.flag0 and bm_TGtkTextTag_pad1) shr bp_TGtkTextTag_pad1;
end;

procedure set_pad1(var a : TGtkTextTag; __pad1 : guint);
begin
   a.flag0:=a.flag0 or ((__pad1 shl bp_TGtkTextTag_pad1) and bm_TGtkTextTag_pad1);
end;

function pad2(var a : TGtkTextTag) : guint;
begin
   pad2:=(a.flag0 and bm_TGtkTextTag_pad2) shr bp_TGtkTextTag_pad2;
end;

procedure set_pad2(var a : TGtkTextTag; __pad2 : guint);
begin
   a.flag0:=a.flag0 or ((__pad2 shl bp_TGtkTextTag_pad2) and bm_TGtkTextTag_pad2);
end;

function pad3(var a : TGtkTextTag) : guint;
begin
   pad3:=(a.flag0 and bm_TGtkTextTag_pad3) shr bp_TGtkTextTag_pad3;
end;

procedure set_pad3(var a : TGtkTextTag; __pad3 : guint);
begin
   a.flag0:=a.flag0 or ((__pad3 shl bp_TGtkTextTag_pad3) and bm_TGtkTextTag_pad3);
end;

function underline(var a : TGtkTextAppearance) : guint;
begin
   underline:=(a.flag0 and bm_TGtkTextAppearance_underline) shr bp_TGtkTextAppearance_underline;
end;

procedure set_underline(var a : TGtkTextAppearance; __underline : guint);
begin
   a.flag0:=a.flag0 or ((__underline shl bp_TGtkTextAppearance_underline) and bm_TGtkTextAppearance_underline);
end;

function strikethrough(var a : TGtkTextAppearance) : guint;
begin
   strikethrough:=(a.flag0 and bm_TGtkTextAppearance_strikethrough) shr bp_TGtkTextAppearance_strikethrough;
end;

procedure set_strikethrough(var a : TGtkTextAppearance; __strikethrough : guint);
begin
   a.flag0:=a.flag0 or ((__strikethrough shl bp_TGtkTextAppearance_strikethrough) and bm_TGtkTextAppearance_strikethrough);
end;

function draw_bg(var a : TGtkTextAppearance) : guint;
begin
   draw_bg:=(a.flag0 and bm_TGtkTextAppearance_draw_bg) shr bp_TGtkTextAppearance_draw_bg;
end;

procedure set_draw_bg(var a : TGtkTextAppearance; __draw_bg : guint);
begin
   a.flag0:=a.flag0 or ((__draw_bg shl bp_TGtkTextAppearance_draw_bg) and bm_TGtkTextAppearance_draw_bg);
end;

function inside_selection(var a : TGtkTextAppearance) : guint;
begin
   inside_selection:=(a.flag0 and bm_TGtkTextAppearance_inside_selection) shr bp_TGtkTextAppearance_inside_selection;
end;

procedure set_inside_selection(var a : TGtkTextAppearance; __inside_selection : guint);
begin
   a.flag0:=a.flag0 or ((__inside_selection shl bp_TGtkTextAppearance_inside_selection) and bm_TGtkTextAppearance_inside_selection);
end;

function is_text(var a : TGtkTextAppearance) : guint;
begin
   is_text:=(a.flag0 and bm_TGtkTextAppearance_is_text) shr bp_TGtkTextAppearance_is_text;
end;

procedure set_is_text(var a : TGtkTextAppearance; __is_text : guint);
begin
   a.flag0:=a.flag0 or ((__is_text shl bp_TGtkTextAppearance_is_text) and bm_TGtkTextAppearance_is_text);
end;

function pad1(var a : TGtkTextAppearance) : guint;
begin
   pad1:=(a.flag0 and bm_TGtkTextAppearance_pad1) shr bp_TGtkTextAppearance_pad1;
end;

procedure set_pad1(var a : TGtkTextAppearance; __pad1 : guint);
begin
   a.flag0:=a.flag0 or ((__pad1 shl bp_TGtkTextAppearance_pad1) and bm_TGtkTextAppearance_pad1);
end;

function pad2(var a : TGtkTextAppearance) : guint;
begin
   pad2:=(a.flag0 and bm_TGtkTextAppearance_pad2) shr bp_TGtkTextAppearance_pad2;
end;

procedure set_pad2(var a : TGtkTextAppearance; __pad2 : guint);
begin
   a.flag0:=a.flag0 or ((__pad2 shl bp_TGtkTextAppearance_pad2) and bm_TGtkTextAppearance_pad2);
end;

function pad3(var a : TGtkTextAppearance) : guint;
begin
   pad3:=(a.flag0 and bm_TGtkTextAppearance_pad3) shr bp_TGtkTextAppearance_pad3;
end;

procedure set_pad3(var a : TGtkTextAppearance; __pad3 : guint);
begin
   a.flag0:=a.flag0 or ((__pad3 shl bp_TGtkTextAppearance_pad3) and bm_TGtkTextAppearance_pad3);
end;

function pad4(var a : TGtkTextAppearance) : guint;
begin
   pad4:=(a.flag0 and bm_TGtkTextAppearance_pad4) shr bp_TGtkTextAppearance_pad4;
end;

procedure set_pad4(var a : TGtkTextAppearance; __pad4 : guint);
begin
   a.flag0:=a.flag0 or ((__pad4 shl bp_TGtkTextAppearance_pad4) and bm_TGtkTextAppearance_pad4);
end;

function invisible(var a : TGtkTextAttributes) : guint;
begin
   invisible:=(a.flag0 and bm_TGtkTextAttributes_invisible) shr bp_TGtkTextAttributes_invisible;
end;

procedure set_invisible(var a : TGtkTextAttributes; __invisible : guint);
begin
   a.flag0:=a.flag0 or ((__invisible shl bp_TGtkTextAttributes_invisible) and bm_TGtkTextAttributes_invisible);
end;

function bg_full_height(var a : TGtkTextAttributes) : guint;
begin
   bg_full_height:=(a.flag0 and bm_TGtkTextAttributes_bg_full_height) shr bp_TGtkTextAttributes_bg_full_height;
end;

procedure set_bg_full_height(var a : TGtkTextAttributes; __bg_full_height : guint);
begin
   a.flag0:=a.flag0 or ((__bg_full_height shl bp_TGtkTextAttributes_bg_full_height) and bm_TGtkTextAttributes_bg_full_height);
end;

function editable(var a : TGtkTextAttributes) : guint;
begin
   editable:=(a.flag0 and bm_TGtkTextAttributes_editable) shr bp_TGtkTextAttributes_editable;
end;

procedure set_editable(var a : TGtkTextAttributes; __editable : guint);
begin
   a.flag0:=a.flag0 or ((__editable shl bp_TGtkTextAttributes_editable) and bm_TGtkTextAttributes_editable);
end;

function realized(var a : TGtkTextAttributes) : guint;
begin
   realized:=(a.flag0 and bm_TGtkTextAttributes_realized) shr bp_TGtkTextAttributes_realized;
end;

procedure set_realized(var a : TGtkTextAttributes; __realized : guint);
begin
   a.flag0:=a.flag0 or ((__realized shl bp_TGtkTextAttributes_realized) and bm_TGtkTextAttributes_realized);
end;

function pad1(var a : TGtkTextAttributes) : guint;
begin
   pad1:=(a.flag0 and bm_TGtkTextAttributes_pad1) shr bp_TGtkTextAttributes_pad1;
end;

procedure set_pad1(var a : TGtkTextAttributes; __pad1 : guint);
begin
   a.flag0:=a.flag0 or ((__pad1 shl bp_TGtkTextAttributes_pad1) and bm_TGtkTextAttributes_pad1);
end;

function pad2(var a : TGtkTextAttributes) : guint;
begin
   pad2:=(a.flag0 and bm_TGtkTextAttributes_pad2) shr bp_TGtkTextAttributes_pad2;
end;

procedure set_pad2(var a : TGtkTextAttributes; __pad2 : guint);
begin
   a.flag0:=a.flag0 or ((__pad2 shl bp_TGtkTextAttributes_pad2) and bm_TGtkTextAttributes_pad2);
end;

function pad3(var a : TGtkTextAttributes) : guint;
begin
   pad3:=(a.flag0 and bm_TGtkTextAttributes_pad3) shr bp_TGtkTextAttributes_pad3;
end;

procedure set_pad3(var a : TGtkTextAttributes; __pad3 : guint);
begin
   a.flag0:=a.flag0 or ((__pad3 shl bp_TGtkTextAttributes_pad3) and bm_TGtkTextAttributes_pad3);
end;

function pad4(var a : TGtkTextAttributes) : guint;
begin
   pad4:=(a.flag0 and bm_TGtkTextAttributes_pad4) shr bp_TGtkTextAttributes_pad4;
end;

procedure set_pad4(var a : TGtkTextAttributes; __pad4 : guint);
begin
   a.flag0:=a.flag0 or ((__pad4 shl bp_TGtkTextAttributes_pad4) and bm_TGtkTextAttributes_pad4);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

