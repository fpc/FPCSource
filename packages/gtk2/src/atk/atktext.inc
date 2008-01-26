// included by atk.pp

{
   AtkAttributeSet:

   This is a singly-linked list (a #GSList) of #AtkAttribute. It is
   used by atk_text_get_run_attributes(), atk_text_get_default_attributes()
   and atk_editable_text_set_run_attributes()
   }

{$IFDEF read_forward_definitions}
   PAtkAttributeSet = ^TAtkAttributeSet;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TAtkAttributeSet = TGSList;

{
   AtkAttribute:
   @name: The attribute name. Call atk_text_attr_get_name()
   @value: the value of the attribute, represented as a string.
   Call atk_text_attr_get_value() for those which are strings.
   For values which are numbers, the string representation of the number
   is in value.

   A string name/value pair representing a text attribute.
   }
   PAtkAttribute = ^TAtkAttribute;
   TAtkAttribute = record
        name : Pgchar;
        value : Pgchar;
     end;

{
  AtkTextAttribute
  @ATK_TEXT_ATTR_INVALID: Invalid attribute
  @ATK_TEXT_ATTR_LEFT_MARGIN: The pixel width of the left margin
  @ATK_TEXT_ATTR_RIGHT_MARGIN: The pixel width of the right margin
  @ATK_TEXT_ATTR_INDENT: The number of pixels that the text is indented
  @ATK_TEXT_ATTR_INVISIBLE: Either "true" or "false" indicating whether text is visible or not
  @ATK_TEXT_ATTR_EDITABLE: Either "true" or "false" indicating whether text is editable or not
  @ATK_TEXT_ATTR_PIXELS_ABOVE_LINES: Pixels of blank space to leave above each newline-terminated line.
  @ATK_TEXT_ATTR_PIXELS_BELOW_LINES: Pixels of blank space to leave below each newline-terminated line.
  @ATK_TEXT_ATTR_PIXELS_INSIDE_WRAP: Pixels of blank space to leave between wrapped lines inside the same newline-terminated line (paragraph).
  @ATK_TEXT_ATTR_BG_FULL_HEIGHT: "true" or "false" whether to make the background color for each character the height of the highest font used on the current line, or the height of the font used for the current character.
  @ATK_TEXT_ATTR_RISE: Number of pixels that the characters are risen above the baseline
  @ATK_TEXT_ATTR_UNDERLINE: "none", "single", "double" or "low"
  @ATK_TEXT_ATTR_STRIKETHROUGH: "true" or "false" whether the text is strikethrough
  @ATK_TEXT_ATTR_SIZE: The size of the characters.
  @ATK_TEXT_ATTR_SCALE: The scale of the characters. The value is a string representation of a double
  @ATK_TEXT_ATTR_WEIGHT: The weight of the characters.
  @ATK_TEXT_ATTR_LANGUAGE: The language used
  @ATK_TEXT_ATTR_FAMILY_NAME: The font family name
  @ATK_TEXT_ATTR_BG_COLOR: The background color. The value is an RGB value of the format "%u,%u,%u"
  @ATK_TEXT_ATTR_FG_COLOR:The foreground color. The value is an RGB value of the format "%u,%u,%u"
  @ATK_TEXT_ATTR_BG_STIPPLE: "true" if a #GdkBitmap is set for stippling the background color.
  @ATK_TEXT_ATTR_FG_STIPPLE: "true" if a #GdkBitmap is set for stippling the foreground color.
  @ATK_TEXT_ATTR_WRAP_MODE: The wrap mode of the text, if any. Values are "none", "char" or "word"
  @ATK_TEXT_ATTR_DIRECTION: The direction of the text, if set. Values are "none", "ltr" or "rtl"
  @ATK_TEXT_ATTR_JUSTIFICATION: The justification of the text, if set. Values are "left", "right", "center" or "fill"
  @ATK_TEXT_ATTR_STRETCH: The stretch of the text, if set. Values are "ultra_condensed", "extra_condensed", "condensed", "semi_condensed", "normal", "semi_expanded", "expanded", "extra_expanded" or "ultra_expanded"
  @ATK_TEXT_ATTR_VARIANT: The capitalization variant of the text, if set. Values are "normal" or "small_caps"
  @ATK_TEXT_ATTR_STYLE: The slant style of the text, if set. Values are "normal", "oblique" or "italic"
  @ATK_TEXT_ATTR_LAST_DEFINED: not a valid text attribute, used for finding theEnd of enumeration

   Describes the text attributes supported
   }

   PAtkTextAttribute = ^TAtkTextAttribute;
   TAtkTextAttribute = (
     ATK_TEXT_ATTR_INVALID {:= 0},
     ATK_TEXT_ATTR_LEFT_MARGIN,
     ATK_TEXT_ATTR_RIGHT_MARGIN,
     ATK_TEXT_ATTR_INDENT,
     ATK_TEXT_ATTR_INVISIBLE,
     ATK_TEXT_ATTR_EDITABLE,
     ATK_TEXT_ATTR_PIXELS_ABOVE_LINES,
     ATK_TEXT_ATTR_PIXELS_BELOW_LINES,
     ATK_TEXT_ATTR_PIXELS_INSIDE_WRAP,
     ATK_TEXT_ATTR_BG_FULL_HEIGHT,
     ATK_TEXT_ATTR_RISE,
     ATK_TEXT_ATTR_UNDERLINE,
     ATK_TEXT_ATTR_STRIKETHROUGH,
     ATK_TEXT_ATTR_SIZE,
     ATK_TEXT_ATTR_SCALE,
     ATK_TEXT_ATTR_WEIGHT,
     ATK_TEXT_ATTR_LANGUAGE,
     ATK_TEXT_ATTR_FAMILY_NAME,
     ATK_TEXT_ATTR_BG_COLOR,
     ATK_TEXT_ATTR_FG_COLOR,
     ATK_TEXT_ATTR_BG_STIPPLE,
     ATK_TEXT_ATTR_FG_STIPPLE,
     ATK_TEXT_ATTR_WRAP_MODE,
     ATK_TEXT_ATTR_DIRECTION,
     ATK_TEXT_ATTR_JUSTIFICATION,
     ATK_TEXT_ATTR_STRETCH,
     ATK_TEXT_ATTR_VARIANT,
     ATK_TEXT_ATTR_STYLE,
     ATK_TEXT_ATTR_LAST_DEFINED
   );


{
  AtkTextBoundary:
  @ATK_TEXT_BOUNDARY_CHAR:
  @ATK_TEXT_BOUNDARY_WORD_START:
  @ATK_TEXT_BOUNDARY_WORD_END:
  @ATK_TEXT_BOUNDARY_SENTENCE_START:
  @ATK_TEXT_BOUNDARY_SENTENCE_END:
  @ATK_TEXT_BOUNDARY_LINE_START:
  @ATK_TEXT_BOUNDARY_LINE_END:

  Text boundary types used for specifying boundaries for regions of text
   }

   PAtkTextBoundary = ^TAtkTextBoundary;
   TAtkTextBoundary = (
     ATK_TEXT_BOUNDARY_CHAR,
     ATK_TEXT_BOUNDARY_WORD_START,
     ATK_TEXT_BOUNDARY_WORD_END,
     ATK_TEXT_BOUNDARY_SENTENCE_START,
     ATK_TEXT_BOUNDARY_SENTENCE_END,
     ATK_TEXT_BOUNDARY_LINE_START,
     ATK_TEXT_BOUNDARY_LINE_END
   );
{
     signal handlers
    }
   PAtkTextIface = ^TAtkTextIface;
   TAtkTextIface = record
        parent : TGTypeInterface;
        get_text : function (text:PAtkText; start_offset:gint; end_offset:gint):Pgchar; cdecl;
        get_text_after_offset : function (text:PAtkText; offset:gint; boundary_type:TAtkTextBoundary; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl;
        get_text_at_offset : function (text:PAtkText; offset:gint; boundary_type:TAtkTextBoundary; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl;
        get_character_at_offset : function (text:PAtkText; offset:gint):gunichar; cdecl;
        get_text_before_offset : function (text:PAtkText; offset:gint; boundary_type:TAtkTextBoundary; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl;
        get_caret_offset : function (text:PAtkText):gint; cdecl;
        get_run_attributes : function (text:PAtkText; offset:gint; start_offset:Pgint; end_offset:Pgint):PAtkAttributeSet; cdecl;
        get_default_attributes : function (text:PAtkText):PAtkAttributeSet; cdecl;
        get_character_extents : procedure (text:PAtkText; offset:gint; x:Pgint; y:Pgint; width:Pgint;
                      height:Pgint; coords:TAtkCoordType); cdecl;
        get_character_count : function (text:PAtkText):gint; cdecl;
        get_offset_at_point : function (text:PAtkText; x:gint; y:gint; coords:TAtkCoordType):gint; cdecl;
        get_n_selections : function (text:PAtkText):gint; cdecl;
        get_selection : function (text:PAtkText; selection_num:gint; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl;
        add_selection : function (text:PAtkText; start_offset:gint; end_offset:gint):gboolean; cdecl;
        remove_selection : function (text:PAtkText; selection_num:gint):gboolean; cdecl;
        set_selection : function (text:PAtkText; selection_num:gint; start_offset:gint; end_offset:gint):gboolean; cdecl;
        set_caret_offset : function (text:PAtkText; offset:gint):gboolean; cdecl;
        text_changed : procedure (text:PAtkText; position:gint; length:gint); cdecl;
        text_caret_moved : procedure (text:PAtkText; location:gint); cdecl;
        text_selection_changed : procedure (text:PAtkText); cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function atk_text_attribute_register(name:Pgchar):TAtkTextAttribute; cdecl; external atklib;

function ATK_TYPE_TEXT : GType;
function ATK_IS_TEXT(obj: pointer) : boolean;
function ATK_TEXT(obj: pointer) : PAtkText;
function ATK_TEXT_GET_IFACE(obj: pointer) : PAtkTextIface;


{
   Additional AtkObject properties used by AtkText:
      "accessible_text" (accessible text has changed)
      "accessible_caret" (accessible text cursor position changed:
                           editable text only)
  }
function atk_text_get_type:GType; cdecl; external atklib;
function atk_text_get_text(text:PAtkText; start_offset:gint; end_offset:gint):Pgchar; cdecl; external atklib;
function atk_text_get_character_at_offset(text:PAtkText; offset:gint):gunichar; cdecl; external atklib;
function atk_text_get_text_after_offset(text:PAtkText; offset:gint; boundary_type:TAtkTextBoundary; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl; external atklib;
function atk_text_get_text_at_offset(text:PAtkText; offset:gint; boundary_type:TAtkTextBoundary; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl; external atklib;
function atk_text_get_text_before_offset(text:PAtkText; offset:gint; boundary_type:TAtkTextBoundary; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl; external atklib;
function atk_text_get_caret_offset(text:PAtkText):gint; cdecl; external atklib;
procedure atk_text_get_character_extents(text:PAtkText; offset:gint; x:Pgint; y:Pgint; width:Pgint;
            height:Pgint; coords:TAtkCoordType); cdecl; external atklib;
function atk_text_get_run_attributes(text:PAtkText; offset:gint; start_offset:Pgint; end_offset:Pgint):PAtkAttributeSet; cdecl; external atklib;
function atk_text_get_default_attributes(text:PAtkText):PAtkAttributeSet; cdecl; external atklib;
function atk_text_get_character_count(text:PAtkText):gint; cdecl; external atklib;
function atk_text_get_offset_at_point(text:PAtkText; x:gint; y:gint; coords:TAtkCoordType):gint; cdecl; external atklib;
function atk_text_get_n_selections(text:PAtkText):gint; cdecl; external atklib;
function atk_text_get_selection(text:PAtkText; selection_num:gint; start_offset:Pgint; end_offset:Pgint):Pgchar; cdecl; external atklib;
function atk_text_add_selection(text:PAtkText; start_offset:gint; end_offset:gint):gboolean; cdecl; external atklib;
function atk_text_remove_selection(text:PAtkText; selection_num:gint):gboolean; cdecl; external atklib;
function atk_text_set_selection(text:PAtkText; selection_num:gint; start_offset:gint; end_offset:gint):gboolean; cdecl; external atklib;
function atk_text_set_caret_offset(text:PAtkText; offset:gint):gboolean; cdecl; external atklib;
procedure atk_attribute_set_free(attrib_set:PAtkAttributeSet); cdecl; external atklib;
function atk_text_attribute_get_name(attr:TAtkTextAttribute):Pgchar; cdecl; external atklib;
function atk_text_attribute_for_name(name:Pgchar):TAtkTextAttribute; cdecl; external atklib;
function atk_text_attribute_get_value(attr:TAtkTextAttribute; index:gint):Pgchar; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_TEXT : GType;
begin
   ATK_TYPE_TEXT:=atk_text_get_type;
end;

function ATK_IS_TEXT(obj: pointer) : boolean;
begin
   ATK_IS_TEXT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_TEXT);
end;

function ATK_TEXT(obj: pointer) : PAtkText;
begin
   ATK_TEXT:=PAtkText(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_TEXT));
end;

function ATK_TEXT_GET_IFACE(obj: pointer) : PAtkTextIface;
begin
   ATK_TEXT_GET_IFACE:=PAtkTextIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_TEXT));
end;

{$ENDIF read_implementation}

