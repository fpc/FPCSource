// included by gdk2.pp
{$IFDEF read_forward_definitions}
{ Types of font.
     GDK_FONT_FONT: the font is an XFontStruct.
     GDK_FONT_FONTSET: the font is an XFontSet used for I18N.
  }
   PGdkFontType = ^TGdkFontType;
   TGdkFontType = (
     GDK_FONT_FONT,
     GDK_FONT_FONTSET
   );

   PGdkFont = ^TGdkFont;
   TGdkFont = record
        _type : TGdkFontType;
        ascent : gint;
        descent : gint;
     end;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GDK_TYPE_FONT : GType;


function gdk_font_get_type:GType; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_font_load(font_name:Pgchar):PGdkFont; cdecl; external gdklib;
function gdk_fontset_load(fontset_name:Pgchar):PGdkFont; cdecl; external gdklib;
function gdk_font_from_description(font_desc:PPangoFontDescription):PGdkFont; cdecl; external gdklib;
{$endif}

function gdk_font_load_for_display(display:PGdkDisplay; font_name:Pgchar):PGdkFont; cdecl; external gdklib;
function gdk_fontset_load_for_display(display:PGdkDisplay; fontset_name:Pgchar):PGdkFont; cdecl; external gdklib;
function gdk_font_from_description_for_display(display:PGdkDisplay; font_desc:PPangoFontDescription):PGdkFont; cdecl; external gdklib;
function gdk_font_ref(font:PGdkFont):PGdkFont; cdecl; external gdklib;
procedure gdk_font_unref(font:PGdkFont); cdecl; external gdklib;
function gdk_font_id(font:PGdkFont):gint; cdecl; external gdklib;
function gdk_font_equal(fonta:PGdkFont; fontb:PGdkFont):gboolean; cdecl; external gdklib;
function gdk_string_width(font:PGdkFont; _string:Pgchar):gint; cdecl; external gdklib;
function gdk_text_width(font:PGdkFont; text:Pgchar; text_length:gint):gint; cdecl; external gdklib;
function gdk_text_width_wc(font:PGdkFont; text:PGdkWChar; text_length:gint):gint; cdecl; external gdklib;
function gdk_char_width(font:PGdkFont; character:gchar):gint; cdecl; external gdklib;
function gdk_char_width_wc(font:PGdkFont; character:TGdkWChar):gint; cdecl; external gdklib;
function gdk_string_measure(font:PGdkFont; _string:Pgchar):gint; cdecl; external gdklib;
function gdk_text_measure(font:PGdkFont; text:Pgchar; text_length:gint):gint; cdecl; external gdklib;
function gdk_char_measure(font:PGdkFont; character:gchar):gint; cdecl; external gdklib;
function gdk_string_height(font:PGdkFont; _string:Pgchar):gint; cdecl; external gdklib;
function gdk_text_height(font:PGdkFont; text:Pgchar; text_length:gint):gint; cdecl; external gdklib;
function gdk_char_height(font:PGdkFont; character:gchar):gint; cdecl; external gdklib;
procedure gdk_text_extents(font:PGdkFont; text:Pgchar; text_length:gint; lbearing:Pgint; rbearing:Pgint;
            width:Pgint; ascent:Pgint; descent:Pgint); cdecl; external gdklib;
procedure gdk_text_extents_wc(font:PGdkFont; text:PGdkWChar; text_length:gint; lbearing:Pgint; rbearing:Pgint;
            width:Pgint; ascent:Pgint; descent:Pgint); cdecl; external gdklib;
procedure gdk_string_extents(font:PGdkFont; _string:Pgchar; lbearing:Pgint; rbearing:Pgint; width:Pgint;
            ascent:Pgint; descent:Pgint); cdecl; external gdklib;
function gdk_font_get_display(font:PGdkFont):PGdkDisplay; cdecl; external gdklib;

{$ifdef GDK_WINDOWING_WIN32}

{ Ditto temporary  }

function gdk_font_full_name_get(font:PGdkFont):Pgchar; cdecl; external gdklib;
procedure gdk_font_full_name_free(name:Pgchar); cdecl; external gdklib;
{$endif GDK_WINDOWING_WIN32}

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_FONT : GType;
begin
  GDK_TYPE_FONT:=gdk_font_get_type;
end;
{$ENDIF}

