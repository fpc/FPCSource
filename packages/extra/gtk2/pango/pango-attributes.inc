// included by pango.pp
{$IFDEF read_interface_types}
{ PangoColor  }
   PPangoColor = ^TPangoColor;
   TPangoColor = record
        red : guint16;
        green : guint16;
        blue : guint16;
     end;

   PPangoAttrType = ^TPangoAttrType;
   TPangoAttrType = longint;

   PPangoUnderline = ^TPangoUnderline;
   TPangoUnderline = longint;

   PPangoAttribute = ^TPangoAttribute;
   PPangoAttrClass = ^TPangoAttrClass;

   TPangoAttribute = record
        klass : PPangoAttrClass;
        start_index : guint;
        end_index : guint;
     end;

   TPangoAttrClass = record
        _type : TPangoAttrType;
        copy : function (attr:PPangoAttribute):PPangoAttribute; cdecl;
        destroy : procedure (attr:PPangoAttribute); cdecl;
        equal : function (attr1:PPangoAttribute; attr2:PPangoAttribute):gboolean; cdecl;
     end;

   PPangoAttrString = ^TPangoAttrString;
   TPangoAttrString = record
        attr : TPangoAttribute;
        value : Pchar;
     end;

   PPangoAttrLanguage = ^TPangoAttrLanguage;
   TPangoAttrLanguage = record
        attr : TPangoAttribute;
        value : PPangoLanguage;
     end;

   PPangoAttrInt = ^TPangoAttrInt;
   TPangoAttrInt = record
        attr : TPangoAttribute;
        value : longint;
     end;

   PPangoAttrFloat = ^TPangoAttrFloat;
   TPangoAttrFloat = record
        attr : TPangoAttribute;
        value : gdouble;
     end;

   PPangoAttrColor = ^TPangoAttrColor;
   TPangoAttrColor = record
        attr : TPangoAttribute;
        color : TPangoColor;
     end;

   PPangoAttrShape = ^TPangoAttrShape;
   TPangoAttrShape = record
        attr : TPangoAttribute;
        ink_rect : TPangoRectangle;
        logical_rect : TPangoRectangle;
     end;

   PPangoAttrFontDesc = ^TPangoAttrFontDesc;
   TPangoAttrFontDesc = record
        attr : TPangoAttribute;
        desc : PPangoFontDescription;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
  const
     PANGO_ATTR_INVALID       = 0;
     PANGO_ATTR_LANGUAGE      = 1;
     PANGO_ATTR_FAMILY        = 2;
     PANGO_ATTR_STYLE         = 3;
     PANGO_ATTR_WEIGHT        = 4;
     PANGO_ATTR_VARIANT       = 5;
     PANGO_ATTR_STRETCH       = 6;
     PANGO_ATTR_SIZE          = 7;
     PANGO_ATTR_FONT_DESC     = 8;
     PANGO_ATTR_FOREGROUND    = 9;
     PANGO_ATTR_BACKGROUND    = 10;
     PANGO_ATTR_UNDERLINE     = 11;
     PANGO_ATTR_STRIKETHROUGH = 12;
     PANGO_ATTR_RISE          = 13;
     PANGO_ATTR_SHAPE         = 14;
     PANGO_ATTR_SCALE         = 15;

     PANGO_UNDERLINE_NONE     = 0;
     PANGO_UNDERLINE_SINGLE   = 1;
     PANGO_UNDERLINE_DOUBLE   = 2;
     PANGO_UNDERLINE_LOW      = 3;


function PANGO_TYPE_COLOR : GType;
function pango_color_get_type:GType; cdecl; external pangolib;
function pango_color_copy(src:PPangoColor):PPangoColor; cdecl; external pangolib;
procedure pango_color_free(color:PPangoColor); cdecl; external pangolib;
function pango_color_parse(color:PPangoColor; spec:Pchar):gboolean; cdecl; external pangolib;

{ Attributes  }

function PANGO_TYPE_ATTR_LIST : GType;

function pango_attr_type_register(name:Pgchar):TPangoAttrType; cdecl; external pangolib;
function pango_attribute_copy(attr:PPangoAttribute):PPangoAttribute; cdecl; external pangolib;
procedure pango_attribute_destroy(attr:PPangoAttribute); cdecl; external pangolib;
function pango_attribute_equal(attr1:PPangoAttribute; attr2:PPangoAttribute):gboolean; cdecl; external pangolib;
function pango_attr_language_new(language:PPangoLanguage):PPangoAttribute; cdecl; external pangolib;
function pango_attr_family_new(family:Pchar):PPangoAttribute; cdecl; external pangolib;
function pango_attr_foreground_new(red:guint16; green:guint16; blue:guint16):PPangoAttribute; cdecl; external pangolib;
function pango_attr_background_new(red:guint16; green:guint16; blue:guint16):PPangoAttribute; cdecl; external pangolib;
function pango_attr_size_new(size:longint):PPangoAttribute; cdecl; external pangolib;
function pango_attr_style_new(style:TPangoStyle):PPangoAttribute; cdecl; external pangolib;
function pango_attr_weight_new(weight:TPangoWeight):PPangoAttribute; cdecl; external pangolib;
function pango_attr_variant_new(variant:TPangoVariant):PPangoAttribute; cdecl; external pangolib;
function pango_attr_stretch_new(stretch:TPangoStretch):PPangoAttribute; cdecl; external pangolib;
function pango_attr_font_desc_new(desc:PPangoFontDescription):PPangoAttribute; cdecl; external pangolib;
function pango_attr_underline_new(underline:TPangoUnderline):PPangoAttribute; cdecl; external pangolib;
function pango_attr_strikethrough_new(strikethrough:gboolean):PPangoAttribute; cdecl; external pangolib;
function pango_attr_rise_new(rise:longint):PPangoAttribute; cdecl; external pangolib;
function pango_attr_shape_new(ink_rect:PPangoRectangle; logical_rect:PPangoRectangle):PPangoAttribute; cdecl; external pangolib;
function pango_attr_scale_new(scale_factor:gdouble):PPangoAttribute; cdecl; external pangolib;
function pango_attr_list_get_type:GType; cdecl; external pangolib;
function pango_attr_list_new:PPangoAttrList; cdecl; external pangolib;
procedure pango_attr_list_ref(list:PPangoAttrList); cdecl; external pangolib;
procedure pango_attr_list_unref(list:PPangoAttrList); cdecl; external pangolib;
function pango_attr_list_copy(list:PPangoAttrList):PPangoAttrList; cdecl; external pangolib;
procedure pango_attr_list_insert(list:PPangoAttrList; attr:PPangoAttribute); cdecl; external pangolib;
procedure pango_attr_list_insert_before(list:PPangoAttrList; attr:PPangoAttribute); cdecl; external pangolib;
procedure pango_attr_list_change(list:PPangoAttrList; attr:PPangoAttribute); cdecl; external pangolib;
procedure pango_attr_list_splice(list:PPangoAttrList; other:PPangoAttrList; pos:gint; len:gint); cdecl; external pangolib;
function pango_attr_list_get_iterator(list:PPangoAttrList):PPangoAttrIterator; cdecl; external pangolib;
procedure pango_attr_iterator_range(iterator:PPangoAttrIterator; start:Pgint; theEnd:Pgint); cdecl; external pangolib;
function pango_attr_iterator_next(iterator:PPangoAttrIterator):gboolean; cdecl; external pangolib;
function pango_attr_iterator_copy(iterator:PPangoAttrIterator):PPangoAttrIterator; cdecl; external pangolib;
procedure pango_attr_iterator_destroy(iterator:PPangoAttrIterator); cdecl; external pangolib;
function pango_attr_iterator_get(iterator:PPangoAttrIterator; _type:TPangoAttrType):PPangoAttribute; cdecl; external pangolib;
procedure pango_attr_iterator_get_font(iterator:PPangoAttrIterator; desc:PPangoFontDescription; var language:PPangoLanguage; extra_attrs:PPGSList); cdecl; external pangolib;
function pango_parse_markup(markup_text:Pchar; length:longint; accel_marker:gunichar; var attr_list:PPangoAttrList; text:PPchar;
           accel_char:Pgunichar; error:PPGError):gboolean; cdecl; external pangolib;
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

function PANGO_TYPE_COLOR : GType;
begin
  PANGO_TYPE_COLOR:=pango_color_get_type;
end;

function PANGO_TYPE_ATTR_LIST : GType;
begin
  PANGO_TYPE_ATTR_LIST:=pango_attr_list_get_type;
end;
{$ENDIF read_implementation}

