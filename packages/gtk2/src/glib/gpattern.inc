// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGPatternSpec = pointer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function  g_pattern_spec_new(pattern:Pgchar):PGPatternSpec;cdecl;external gliblib name 'g_pattern_spec_new';
procedure g_pattern_spec_free(pspec:PGPatternSpec);cdecl;external gliblib name 'g_pattern_spec_free';
function  g_pattern_spec_equal(pspec1:PGPatternSpec; pspec2:PGPatternSpec):gboolean;cdecl;external gliblib name 'g_pattern_spec_equal';
function  g_pattern_match(pspec:PGPatternSpec; string_length:guint; _string:Pgchar; string_reversed:Pgchar):gboolean;cdecl;external gliblib name 'g_pattern_match';
function  g_pattern_match_string(pspec:PGPatternSpec; _string:Pgchar):gboolean;cdecl;external gliblib name 'g_pattern_match_string';
function  g_pattern_match_simple(pattern:Pgchar; _string:Pgchar):gboolean;cdecl;external gliblib name 'g_pattern_match_simple';
{$ENDIF read_interface_rest}
// included by glib2.pas

