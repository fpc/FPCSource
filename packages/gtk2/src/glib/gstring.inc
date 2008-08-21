// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGString = ^TGString;
  TGString = record
    str : Pgchar;
    len : gsize;
    allocated_len : gsize;
  end;

  PGStringChunk = pointer; // structure not known
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
    { String Chunks }

    function g_string_chunk_new(size:gsize):PGStringChunk;cdecl;external gliblib name 'g_string_chunk_new';
    procedure g_string_chunk_free(chunk:PGStringChunk);cdecl;external gliblib name 'g_string_chunk_free';
    function g_string_chunk_insert(chunk:PGStringChunk; _string:Pgchar):Pgchar;cdecl;external gliblib name 'g_string_chunk_insert';
    function g_string_chunk_insert_const(chunk:PGStringChunk; _string:Pgchar):Pgchar;cdecl;external gliblib name 'g_string_chunk_insert_const';

    { Strings }
    function g_string_new(init:Pgchar):PGString;cdecl;external gliblib name 'g_string_new';
    function g_string_new_len(init:Pgchar; len:gssize):PGString;cdecl;external gliblib name 'g_string_new_len';
    function g_string_sized_new(dfl_size:gsize):PGString;cdecl;external gliblib name 'g_string_sized_new';
    function g_string_free(_string:PGString; free_segment:gboolean):Pgchar;cdecl;external gliblib name 'g_string_free';
    function g_string_equal(v:PGString; v2:PGString):gboolean;cdecl;external gliblib name 'g_string_equal';
    function g_string_hash(str:PGString):guint;cdecl;external gliblib name 'g_string_hash';
    function g_string_assign(_string:PGString; rval:Pgchar):PGString;cdecl;external gliblib name 'g_string_assign';
    function g_string_truncate(_string:PGString; len:gsize):PGString;cdecl;external gliblib name 'g_string_truncate';
    function g_string_set_size(_string:PGString; len:gsize):PGString;cdecl;external gliblib name 'g_string_set_size';
    function g_string_insert_len(_string:PGString; pos:gssize; val:Pgchar; len:gssize):PGString;cdecl;external gliblib name 'g_string_insert_len';
    function g_string_append(_string:PGString; val:Pgchar):PGString;cdecl;external gliblib name 'g_string_append';
    function g_string_append_len(_string:PGString; val:Pgchar; len:gssize):PGString;cdecl;external gliblib name 'g_string_append_len';
    function g_string_append_c(_string:PGString; c:gchar):PGString;cdecl;external gliblib name 'g_string_append_c';
    function g_string_append_unichar(_string:PGString; wc:gunichar):PGString;cdecl;external gliblib name 'g_string_append_unichar';
    function g_string_prepend(_string:PGString; val:Pgchar):PGString;cdecl;external gliblib name 'g_string_prepend';
    function g_string_prepend_c(_string:PGString; c:gchar):PGString;cdecl;external gliblib name 'g_string_prepend_c';
    function g_string_prepend_unichar(_string:PGString; wc:gunichar):PGString;cdecl;external gliblib name 'g_string_prepend_unichar';
    function g_string_prepend_len(_string:PGString; val:Pgchar; len:gssize):PGString;cdecl;external gliblib name 'g_string_prepend_len';
    function g_string_insert(_string:PGString; pos:gssize; val:Pgchar):PGString;cdecl;external gliblib name 'g_string_insert';
    function g_string_insert_c(_string:PGString; pos:gssize; c:gchar):PGString;cdecl;external gliblib name 'g_string_insert_c';
    function g_string_insert_unichar(_string:PGString; pos:gssize; wc:gunichar):PGString;cdecl;external gliblib name 'g_string_insert_unichar';
    function g_string_erase(_string:PGString; pos:gssize; len:gssize):PGString;cdecl;external gliblib name 'g_string_erase';
    function g_string_ascii_down(_string:PGString):PGString;cdecl;external gliblib name 'g_string_ascii_down';
    function g_string_ascii_up(_string:PGString):PGString;cdecl;external gliblib name 'g_string_ascii_up';

{$IFNDEF KYLIX}
    procedure g_string_printf(_string:PGString; format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_string_printf';
    procedure g_string_printf(_string:PGString; format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_string_printf';

    procedure g_string_append_printf(_string:PGString; format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_string_append_printf';
    procedure g_string_append_printf(_string:PGString; format:Pgchar);cdecl;varargs;overload;external gliblib name 'g_string_append_printf';
{$ELSE}
    procedure g_string_printf(_string:PGString; format:Pgchar);varargs;cdecl;external gliblib name 'g_string_printf';
    procedure g_string_append_printf(_string:PGString; format:Pgchar);varargs;cdecl;external gliblib name 'g_string_append_printf';
{$ENDIF}

{DEPRECATED functions}

function g_string_down(_string:PGString):PGString;cdecl;external gliblib name 'g_string_down';
function g_string_up(_string:PGString):PGString;cdecl;external gliblib name 'g_string_up';

{$ENDIF read_interface_rest}
// included by glib2.pas


