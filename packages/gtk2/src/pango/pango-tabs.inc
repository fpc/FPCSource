// included by pango.pp
{$IFDEF read_interface_types}
   { These are not supported now, but may be in the
     future.

      PANGO_TAB_RIGHT,
      PANGO_TAB_CENTER,
      PANGO_TAB_NUMERIC
    }
   PPangoTabAlign = ^TPangoTabAlign;
   TPangoTabAlign = (
     PANGO_TAB_LEFT
   );
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function PANGO_TYPE_TAB_ARRAY : GType;

function pango_tab_array_new(initial_size:gint; positions_in_pixels:gboolean):PPangoTabArray; cdecl; external pangolib;
{$IFNDEF KYLIX}
function pango_tab_array_new_with_positions(size:gint; positions_in_pixels:gboolean; first_alignment:TPangoTabAlign; first_position:gint; args:array of const):PPangoTabArray; cdecl; overload; external pangolib;
function pango_tab_array_new_with_positions(size:gint; positions_in_pixels:gboolean; first_alignment:TPangoTabAlign; first_position:gint):PPangoTabArray; cdecl; overload; varargs; external pangolib;
{$ELSE}
function pango_tab_array_new_with_positions(size:gint; positions_in_pixels:gboolean; first_alignment:TPangoTabAlign; first_position:gint):PPangoTabArray; varargs; cdecl; external pangolib;
{$ENDIF}
function pango_tab_array_get_type:GType; cdecl; external pangolib;
function pango_tab_array_copy(src:PPangoTabArray):PPangoTabArray; cdecl; external pangolib;
procedure pango_tab_array_free(tab_array:PPangoTabArray); cdecl; external pangolib;
function pango_tab_array_get_size(tab_array:PPangoTabArray):gint; cdecl; external pangolib;
procedure pango_tab_array_resize(tab_array:PPangoTabArray; new_size:gint); cdecl; external pangolib;
procedure pango_tab_array_set_tab(tab_array:PPangoTabArray; tab_index:gint; alignment:TPangoTabAlign; location:gint); cdecl; external pangolib;
procedure pango_tab_array_get_tab(tab_array:PPangoTabArray; tab_index:gint; alignment:PPangoTabAlign; location:Pgint); cdecl; external pangolib;
procedure pango_tab_array_get_tabs(tab_array:PPangoTabArray; var alignments:PPangoTabAlign; var locations:Pgint); cdecl; external pangolib;
function pango_tab_array_get_positions_in_pixels(tab_array:PPangoTabArray):gboolean; cdecl; external pangolib;
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function PANGO_TYPE_TAB_ARRAY : GType;
begin
  PANGO_TYPE_TAB_ARRAY:=pango_tab_array_get_type;
end;
{$ENDIF read_implementation}

