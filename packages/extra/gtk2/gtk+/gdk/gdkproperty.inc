// included by gdk2.pp

{$IFDEF read_interface_types}
   PGdkPropMode = ^TGdkPropMode;
   TGdkPropMode = (
     GDK_PROP_MODE_REPLACE,
     GDK_PROP_MODE_PREPEND,
     GDK_PROP_MODE_APPEND
   );

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function gdk_atom_intern(atom_name:Pgchar; only_if_exists:gboolean):TGdkAtom; cdecl; external gdklib;
function gdk_atom_name(atom:TGdkAtom):Pgchar; cdecl; external gdklib;
function gdk_property_get(window:PGdkWindow; _property:TGdkAtom; _type:TGdkAtom; offset:gulong; length:gulong;
           pdelete:gint; actual_property_type:PGdkAtom; actual_format:Pgint; actual_length:Pgint; data:PPguchar):gboolean; cdecl; external gdklib;
procedure gdk_property_change(window:PGdkWindow; _property:TGdkAtom; _type:TGdkAtom; format:gint; mode:TGdkPropMode;
            data:Pguchar; nelements:gint); cdecl; external gdklib;
procedure gdk_property_delete(window:PGdkWindow; _property:TGdkAtom); cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_text_property_to_text_list(encoding:TGdkAtom; format:gint; text:Pguchar; length:gint; var list:PPgchar):gint; cdecl; external gdklib;
function gdk_text_property_to_utf8_list(encoding:TGdkAtom; format:gint; text:Pguchar; length:gint; var list:PPgchar):gint; cdecl; external gdklib;
function gdk_utf8_to_compound_text(str:Pgchar; encoding:PGdkAtom; format:Pgint; ctext:PPguchar; length:Pgint):gboolean; cdecl; external gdklib;
function gdk_string_to_compound_text(str:Pgchar; encoding:PGdkAtom; format:Pgint; ctext:PPguchar; length:Pgint):gint; cdecl; external gdklib;
{$endif}

function gdk_text_property_to_text_list_for_display(display:PGdkDisplay; encoding:TGdkAtom; format:gint; text:Pguchar; length:gint;
           var list:PPgchar):gint; cdecl; external gdklib;
function gdk_text_property_to_utf8_list_for_display(display:PGdkDisplay; encoding:TGdkAtom; format:gint; text:Pguchar; length:gint;
           var list:PPgchar):gint; cdecl; external gdklib;
function gdk_utf8_to_string_target(str:Pgchar):Pgchar; cdecl; external gdklib;
function gdk_string_to_compound_text_for_display(display:PGdkDisplay; str:Pgchar; encoding:PGdkAtom; format:Pgint; ctext:PPguchar;
           length:Pgint):gint; cdecl; external gdklib;
function gdk_utf8_to_compound_text_for_display(display:PGdkDisplay; str:Pgchar; encoding:PGdkAtom; format:Pgint; ctext:PPguchar;
           length:Pgint):gboolean; cdecl; external gdklib;
procedure gdk_free_text_list(list:PPgchar); cdecl; external gdklib;
procedure gdk_free_compound_text(ctext:Pguchar); cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF}
