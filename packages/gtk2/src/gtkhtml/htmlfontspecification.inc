// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  THtmlFontStyleType = (HTML_FONT_STYLE_NORMAL := 0,HTML_FONT_STYLE_ITALIC, HTML_FONT_STYLE_OBLIQUE);
  THtmlFontVariantType = (HTML_FONT_VARIANT_NORMAL := 0,HTML_FONT_VARIANT_SMALL_CAPS);
  THtmlFontWeightType = (HTML_FONT_WEIGHT_100,HTML_FONT_WEIGHT_200,
    HTML_FONT_WEIGHT_300,HTML_FONT_WEIGHT_400, HTML_FONT_WEIGHT_500,HTML_FONT_WEIGHT_600,
    HTML_FONT_WEIGHT_700,HTML_FONT_WEIGHT_800, HTML_FONT_WEIGHT_900);

const
  HTML_FONT_WEIGHT_NORMAL = HTML_FONT_WEIGHT_400;
  HTML_FONT_WEIGHT_BOLD = HTML_FONT_WEIGHT_700;

type
  THtmlFontStretchType = (HTML_FONT_STRETCH_NORMAL := 0,HTML_FONT_STRETCH_ULTRA_CONDENSED,
    HTML_FONT_STRETCH_EXTRA_CONDENSED,HTML_FONT_STRETCH_CONDENSED,
    HTML_FONT_STRETCH_SEMI_CONDENSED,HTML_FONT_STRETCH_SEMI_EXPANDED,
    HTML_FONT_STRETCH_EXPANDED,HTML_FONT_STRETCH_EXTRA_EXPANDED,
    HTML_FONT_STRETCH_ULTRA_EXPANDED);
  THtmlFontDecorationType = (HTML_FONT_DECORATION_NONE := 0,HTML_FONT_DECORATION_UNDERLINE := 1 shl 0,
    HTML_FONT_DECORATION_OVERLINE := 1 shl 1, HTML_FONT_DECORATION_LINETHROUGH := 1 shl 2);

  PHtmlFontSpecification = ^THtmlFontSpecification;
  THtmlFontSpecification = record
    family : pgchar;
    size : gint;
    flag0 : word;
  end;//THtmlFontSpecification

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function weight(var a : _HtmlFontSpecification) : THtmlFontWeightType;
  procedure set_weight(var a : _HtmlFontSpecification; __weight : THtmlFontWeightType);
  function style(var a : _HtmlFontSpecification) : THtmlFontStyleType;
  procedure set_style(var a : _HtmlFontSpecification; __style : THtmlFontStyleType);
  function variant(var a : _HtmlFontSpecification) : THtmlFontVariantType;
  procedure set_variant(var a : _HtmlFontSpecification; __variant : THtmlFontVariantType);
  function stretch(var a : _HtmlFontSpecification) : THtmlFontStretchType;
  procedure set_stretch(var a : _HtmlFontSpecification; __stretch : THtmlFontStretchType);
  function decoration(var a : _HtmlFontSpecification) : THtmlFontDecorationType;
  procedure set_decoration(var a : _HtmlFontSpecification; __decoration : THtmlFontDecorationType);

  function html_font_specification_new(family:Pgchar; style:THtmlFontStyleType; variant:THtmlFontVariantType; weight:HtmlFontWeightType; stretch:HtmlFontStretchType;
    decoration:THtmlFontDecorationType; size:gint):PHtmlFontSpecification;cdecl; external gtkhtmllib;
  function html_font_specification_ref(spec:PHtmlFontSpecification):PHtmlFontSpecification;cdecl; external gtkhtmllib;
  procedure html_font_specification_unref(spec:PHtmlFontSpecification);cdecl; external gtkhtmllib;
  function html_font_specification_dup(spec:PHtmlFontSpecification):PHtmlFontSpecification;cdecl; external gtkhtmllib;
  function html_font_description_equal(a:PHtmlFontSpecification; b:PHtmlFontSpecification):gboolean;cdecl; external gtkhtmllib;
  function html_font_specification_get_pango_font_description(spec:PHtmlFontSpecification):PPangoFontDescription;cdecl; external gtkhtmllib;
  procedure html_font_specification_get_extra_attributes(spec:PHtmlFontSpecification; attrs:PPangoAttrList; start_index:gint; end_index:gint);cdecl; external gtkhtmllib;
  procedure html_font_specification_get_all_attributes(spec:PHtmlFontSpecification; attrs:PPangoAttrList; start_index:gint; end_index:gint; magnification:gdouble);cdecl; external gtkhtmllib;
  function html_font_specification_get_html_size(spec:PHtmlFontSpecification):gint;cdecl; external gtkhtmllib;
  function html_font_description_html_size_to_pt(font_size:gint):gint;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function weight(var a : THtmlFontSpecification) : THtmlFontWeightType;
  begin
    weight:=(a.flag0 and bm__HtmlFontSpecification_weight) shr bp__HtmlFontSpecification_weight;
  end;//weight

  procedure set_weight(var a : THtmlFontSpecification; __weight : THtmlFontWeightType);
  begin
    a.flag0:=a.flag0 or ((__weight shl bp__HtmlFontSpecification_weight) and bm__HtmlFontSpecification_weight);
  end;//set_weight

  function style(var a : THtmlFontSpecification) : THtmlFontStyleType;
  begin
    style:=(a.flag0 and bm__HtmlFontSpecification_style) shr bp__HtmlFontSpecification_style;
  end;//style

  procedure set_style(var a : THtmlFontSpecification; __style : THtmlFontStyleType);
  begin
    a.flag0:=a.flag0 or ((__style shl bp__HtmlFontSpecification_style) and bm__HtmlFontSpecification_style);
  end;//set_style

  function variant(var a : THtmlFontSpecification) : THtmlFontVariantType;
  begin
    variant:=(a.flag0 and bm__HtmlFontSpecification_variant) shr bp__HtmlFontSpecification_variant;
  end;//variant

  procedure set_variant(var a : THtmlFontSpecification; __variant : THtmlFontVariantType);
  begin
    a.flag0:=a.flag0 or ((__variant shl bp__HtmlFontSpecification_variant) and bm__HtmlFontSpecification_variant);
  end;//set_variant

  function stretch(var a : THtmlFontSpecification) : THtmlFontStretchType;
  begin
    stretch:=(a.flag0 and bm__HtmlFontSpecification_stretch) shr bp__HtmlFontSpecification_stretch;
  end;//stretch

  procedure set_stretch(var a : THtmlFontSpecification; __stretch : THtmlFontStretchType);
  begin
    a.flag0:=a.flag0 or ((__stretch shl bp__HtmlFontSpecification_stretch) and bm__HtmlFontSpecification_stretch);
  end;//set_stretch

  function decoration(var a : THtmlFontSpecification) : THtmlFontDecorationType;
  begin
    decoration:=(a.flag0 and bm__HtmlFontSpecification_decoration) shr bp__HtmlFontSpecification_decoration;
  end;//decoration

  procedure set_decoration(var a : THtmlFontSpecification; __decoration : THtmlFontDecorationType);
  begin
    a.flag0:=a.flag0 or ((__decoration shl bp__HtmlFontSpecification_decoration) and bm__HtmlFontSpecification_decoration);
  end;//set_decoration

{$ENDIF read_implementation}
// included by gtkhtml.pas
