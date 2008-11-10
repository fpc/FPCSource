// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   Pgchararray = ^Tgchararray;
   Tgchararray = gchar;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ --- type macros ---  }

function G_VALUE_HOLDS_CHAR(value : PGValue) : boolean;

function G_VALUE_HOLDS_UCHAR(value : PGValue) : boolean;

function G_VALUE_HOLDS_BOOLEAN(value : PGValue) : boolean;

function G_VALUE_HOLDS_INT(value : PGValue) : boolean;

function G_VALUE_HOLDS_UINT(value : PGValue) : boolean;

function G_VALUE_HOLDS_LONG(value : PGValue) : boolean;

function G_VALUE_HOLDS_ULONG(value : PGValue) : boolean;

function G_VALUE_HOLDS_INT64(value : PGValue) : boolean;

function G_VALUE_HOLDS_UINT64(value : PGValue) : boolean;

function G_VALUE_HOLDS_FLOAT(value : PGValue) : boolean;

function G_VALUE_HOLDS_DOUBLE(value : PGValue) : boolean;

function G_VALUE_HOLDS_STRING(value : PGValue) : boolean;

function G_VALUE_HOLDS_POINTER(value : PGValue) : boolean;

{ --- prototypes ---  }
procedure g_value_set_char(value:PGValue; v_char:gchar); cdecl; external gobjectlib;

function g_value_get_char(value:PGValue):gchar; cdecl; external gobjectlib;
procedure g_value_set_uchar(value:PGValue; v_uchar:guchar); cdecl; external gobjectlib;

function g_value_get_uchar(value:PGValue):guchar; cdecl; external gobjectlib;
procedure g_value_set_boolean(value:PGValue; v_boolean:gboolean); cdecl; external gobjectlib;

function g_value_get_boolean(value:PGValue):gboolean; cdecl; external gobjectlib;
procedure g_value_set_int(value:PGValue; v_int:gint); cdecl; external gobjectlib;

function g_value_get_int(value:PGValue):gint; cdecl; external gobjectlib;
procedure g_value_set_uint(value:PGValue; v_uint:guint); cdecl; external gobjectlib;

function g_value_get_uint(value:PGValue):guint; cdecl; external gobjectlib;
procedure g_value_set_long(value:PGValue; v_long:glong); cdecl; external gobjectlib;

function g_value_get_long(value:PGValue):glong; cdecl; external gobjectlib;
procedure g_value_set_ulong(value:PGValue; v_ulong:gulong); cdecl; external gobjectlib;

function g_value_get_ulong(value:PGValue):gulong; cdecl; external gobjectlib;
procedure g_value_set_int64(value:PGValue; v_int64:gint64); cdecl; external gobjectlib;

function g_value_get_int64(value:PGValue):gint64; cdecl; external gobjectlib;
procedure g_value_set_uint64(value:PGValue; v_uint64:guint64); cdecl; external gobjectlib;

function g_value_get_uint64(value:PGValue):guint64; cdecl; external gobjectlib;
procedure g_value_set_float(value:PGValue; v_float:gfloat); cdecl; external gobjectlib;

function g_value_get_float(value:PGValue):gfloat; cdecl; external gobjectlib;
procedure g_value_set_double(value:PGValue; v_double:gdouble); cdecl; external gobjectlib;

function g_value_get_double(value:PGValue):gdouble; cdecl; external gobjectlib;

procedure g_value_set_string(value:PGValue; v_string:Pgchar); cdecl; external gobjectlib;

procedure g_value_set_static_string(value:PGValue; v_string:Pgchar); cdecl; external gobjectlib;

function g_value_get_string(value:PGValue):Pgchar; cdecl; external gobjectlib;

function g_value_dup_string(value:PGValue):Pgchar; cdecl; external gobjectlib;
procedure g_value_set_pointer(value:PGValue; v_pointer:gpointer); cdecl; external gobjectlib;

function g_value_get_pointer(value:PGValue):gpointer; cdecl; external gobjectlib;
{ Convenience for registering new pointer types  }

function g_pointer_type_register_static(name:Pgchar):GType; cdecl; external gobjectlib;
{ debugging aid, describe value contents as string  }

function g_strdup_value_contents(value:PGValue):Pgchar; cdecl; external gobjectlib;
{ --- marshaller specific ---  }
procedure g_value_set_string_take_ownership(value:PGValue; v_string:Pgchar); cdecl; external gobjectlib;
{ humpf, need a C representable type name for G_TYPE_STRING  }

{$ENDIF read_interface_rest}
// included by glib2.pas
