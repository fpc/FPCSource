// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ --- enum/flag values & classes ---  }
type
   PGEnumClass = ^TGEnumClass;
   PGEnumValue = ^TGEnumValue;

   TGEnumClass = record
        g_type_class : TGTypeClass;
        minimum : gint;
        maximum : gint;
        n_values : guint;
        values : PGEnumValue;
     end;

   TGEnumValue = record
        value : gint;
        value_name : Pgchar;
        value_nick : Pgchar;
     end;

   PGFlagsClass = ^TGFlagsClass;
   PGFlagsValue = ^TGFlagsValue;

   TGFlagsClass = record
        g_type_class : TGTypeClass;
        mask : guint;
        n_values : guint;
        values : PGFlagsValue;
     end;

   TGFlagsValue = record
        value : guint;
        value_name : Pgchar;
        value_nick : Pgchar;
     end;

{ --- type macros ---  }
function G_TYPE_IS_ENUM(_type : GType) : gboolean;
function G_ENUM_CLASS(_class : pointer) : PGEnumClass;
function G_IS_ENUM_CLASS(_class : pointer) : gboolean;
function G_ENUM_CLASS_TYPE(_class : pointer) : GType;
function G_ENUM_CLASS_TYPE_NAME(_class : pointer) : PGChar;
function G_TYPE_IS_FLAGS(_type : GType) : gboolean;
function G_FLAGS_CLASS(_class : pointer) : PGFlagsClass;
function G_IS_FLAGS_CLASS(_class : pointer) : gboolean;
function G_FLAGS_CLASS_TYPE(_class : pointer) : GType;
function G_FLAGS_CLASS_TYPE_NAME(_class : pointer) : PGChar;
function G_VALUE_HOLDS_ENUM(value : pointer) : gboolean;
function G_VALUE_HOLDS_FLAGS(value : pointer) : gboolean;


{ --- prototypes ---  }

function g_enum_get_value(enum_class:PGEnumClass; value:gint):PGEnumValue; cdecl; external gliblib;
function g_enum_get_value_by_name(enum_class:PGEnumClass; name:Pgchar):PGEnumValue; cdecl; external gliblib;
function g_enum_get_value_by_nick(enum_class:PGEnumClass; nick:Pgchar):PGEnumValue; cdecl; external gliblib;
function g_flags_get_first_value(flags_class:PGFlagsClass; value:guint):PGFlagsValue; cdecl; external gliblib;
function g_flags_get_value_by_name(flags_class:PGFlagsClass; name:Pgchar):PGFlagsValue; cdecl; external gliblib;
function g_flags_get_value_by_nick(flags_class:PGFlagsClass; nick:Pgchar):PGFlagsValue; cdecl; external gliblib;
procedure g_value_set_enum(value:PGValue; v_enum:gint); cdecl; external gliblib;
function g_value_get_enum(value:PGValue):gint; cdecl; external gliblib;
procedure g_value_set_flags(value:PGValue; v_flags:guint); cdecl; external gliblib;
function g_value_get_flags(value:PGValue):guint; cdecl; external gliblib;
{ --- registration functions ---  }
{ const_static_values is a NULL terminated array of enum/flags
   values that is taken over!
  }
function g_enum_register_static(name:Pgchar; const_static_values:PGEnumValue):GType; cdecl; external gliblib;
function g_flags_register_static(name:Pgchar; const_static_values:PGFlagsValue):GType; cdecl; external gliblib;
{ functions to complete the type information
   for enums/flags implemented by plugins
  }
procedure g_enum_complete_type_info(g_enum_type:GType; info:PGTypeInfo; const_values:PGEnumValue); cdecl; external gliblib;
procedure g_flags_complete_type_info(g_flags_type:GType; info:PGTypeInfo; const_values:PGFlagsValue); cdecl; external gliblib;

{$ENDIF read_interface_rest}

// included by glib2.pas


