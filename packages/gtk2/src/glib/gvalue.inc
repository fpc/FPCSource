// included by glib2.pas

{$IFDEF read_forward_definitions}
{< private > }
{ public for GTypeValueTable methods  }
   PGValue = ^TGValue;
   TGValue = record
        g_type : GType;
        data : array[0..1] of record
            case longint of
               0 : ( v_int : gint );
               1 : ( v_uint : guint );
               2 : ( v_long : glong );
               3 : ( v_ulong : gulong );
               4 : ( v_int64 : gint64 );
               5 : ( v_uint64 : guint64 );
               6 : ( v_float : gfloat );
               7 : ( v_double : gdouble );
               8 : ( v_pointer : gpointer );
            end;
     end;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}


{ --- type macros ---  }

function G_TYPE_IS_VALUE(_type : GType) : boolean;
function G_IS_VALUE(value : pointer) : boolean;
function G_VALUE_TYPE(value : Pointer) : GType;
function G_VALUE_TYPE_NAME(value : Pointer) : PGChar;
function G_VALUE_HOLDS(value: pointer; g_type : GType) : boolean;

{ --- typedefs & structures ---  }
type
   TGValueTransform = procedure (src_value:PGValue; dest_value:PGValue); cdecl;


{ --- prototypes ---  }

function g_value_init(value:PGValue; g_type:GType):PGValue; cdecl; external gobjectlib;
procedure g_value_copy(src_value:PGValue; dest_value:PGValue); cdecl; external gobjectlib;
function g_value_reset(value:PGValue):PGValue; cdecl; external gobjectlib;
procedure g_value_unset(value:PGValue); cdecl; external gobjectlib;
procedure g_value_set_instance(value:PGValue; instance:gpointer); cdecl; external gobjectlib;
{ --- private ---  }
function g_value_fits_pointer(value:PGValue):gboolean; cdecl; external gobjectlib;
function g_value_peek_pointer(value:PGValue):gpointer; cdecl; external gobjectlib;
{ --- implementation details ---  }
function g_value_type_compatible(src_type:GType; dest_type:GType):gboolean; cdecl; external gobjectlib;
function g_value_type_transformable(src_type:GType; dest_type:GType):gboolean; cdecl; external gobjectlib;
function g_value_transform(src_value:PGValue; dest_value:PGValue):gboolean; cdecl; external gobjectlib;
procedure g_value_register_transform_func(src_type:GType; dest_type:GType; transform_func:TGValueTransform); cdecl; external gobjectlib;

const
   G_VALUE_NOCOPY_CONTENTS = 1 shl 27;

{$ENDIF read_interface_rest}
// included by glib2.pas

