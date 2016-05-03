// included by glib2.pas

{$IFDEF read_forward_definitions}
  PGParamFlags = ^TGParamFlags;
  TGParamFlags = longint;

  PGParamSpec = ^TGParamSpec;
  PPGParamSpec = ^PGParamSpec;

  PGParamSpecClass = ^TGParamSpecClass;

  PGParameter = ^TGParameter;

  PGParamSpecTypeInfo = ^TGParamSpecTypeInfo;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  TGParamSpec = record
    g_type_instance : TGTypeInstance;
    name : Pgchar;
    flags : TGParamFlags;
    value_type : GType;
    owner_type : GType;
    _nick : Pgchar;
    _blurb : Pgchar;
    qdata : PGData;
    ref_count : guint;
    param_id : guint;
  end;

  TGParamSpecClass = record
    g_type_class : TGTypeClass;
    value_type : GType;
    finalize : procedure (pspec:PGParamSpec); cdecl;
    value_set_default : procedure (pspec:PGParamSpec; value:PGValue); cdecl;
    value_validate : function (pspec:PGParamSpec; value:PGValue):gboolean; cdecl;
    values_cmp : function (pspec:PGParamSpec; value1:PGValue; value2:PGValue):gint; cdecl;
    dummy : array[0..3] of gpointer;
  end;

  TGParameter = record
    name : Pgchar;
    value : TGValue;
  end;

  TGParamSpecTypeInfo = record
    instance_size : guint16;
    n_preallocs : guint16;
    instance_init : procedure (pspec:PGParamSpec); cdecl;
    value_type : GType;
    finalize : procedure (pspec:PGParamSpec); cdecl;
    value_set_default : procedure (pspec:PGParamSpec; value:PGValue); cdecl;
    value_validate : function (pspec:PGParamSpec; value:PGValue):gboolean; cdecl;
    values_cmp : function (pspec:PGParamSpec; value1:PGValue; value2:PGValue):gint; cdecl;
  end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ --- standard type macros ---  }

function G_TYPE_IS_PARAM(_type : GType) : boolean;
function G_PARAM_SPEC(pspec : Pointer) : PGParamSpec;
function G_IS_PARAM_SPEC(pspec : Pointer) : boolean;
function G_PARAM_SPEC_CLASS(pclass : Pointer) : PGParamSpecClass;
function G_IS_PARAM_SPEC_CLASS(pclass : Pointer) : boolean;
function G_PARAM_SPEC_GET_CLASS(pspec : Pointer) : PGParamSpecClass;

{ --- convenience macros ---  }
function G_PARAM_SPEC_TYPE(pspec : Pointer) : GType;
function G_PARAM_SPEC_TYPE_NAME(pspec : Pointer) : PGChar;
function G_PARAM_SPEC_VALUE_TYPE(pspec : Pointer) : GType;
function G_VALUE_HOLDS_PARAM(value : Pointer) : boolean;

const
   G_PARAM_READABLE = 1 shl 0;
   G_PARAM_WRITABLE = 1 shl 1;
   G_PARAM_CONSTRUCT = 1 shl 2;
   G_PARAM_CONSTRUCT_ONLY = 1 shl 3;
   G_PARAM_LAX_VALIDATION = 1 shl 4;
   G_PARAM_PRIVATE = 1 shl 5;
   G_PARAM_READWRITE = G_PARAM_READABLE or G_PARAM_WRITABLE;
   G_PARAM_MASK = $000000ff;

{ bits in the range 0xffffff00 are reserved for 3rd party usage  }
   G_PARAM_USER_SHIFT = 8;



{ --- prototypes ---  }

function g_param_spec_ref(pspec:PGParamSpec):PGParamSpec; cdecl; external gliblib;
procedure g_param_spec_unref(pspec:PGParamSpec); cdecl; external gliblib;
procedure g_param_spec_sink(pspec:PGParamSpec); cdecl; external gliblib;
function g_param_spec_get_qdata(pspec:PGParamSpec; quark:TGQuark):gpointer; cdecl; external gliblib;
procedure g_param_spec_set_qdata(pspec:PGParamSpec; quark:TGQuark; data:gpointer); cdecl; external gliblib;
procedure g_param_spec_set_qdata_full(pspec:PGParamSpec; quark:TGQuark; data:gpointer; destroy:TGDestroyNotify); cdecl; external gliblib;
function g_param_spec_steal_qdata(pspec:PGParamSpec; quark:TGQuark):gpointer; cdecl; external gliblib;
procedure g_param_value_set_default(pspec:PGParamSpec; value:PGValue); cdecl; external gliblib;
function g_param_value_defaults(pspec:PGParamSpec; value:PGValue):gboolean; cdecl; external gliblib;
function g_param_value_validate(pspec:PGParamSpec; value:PGValue):gboolean; cdecl; external gliblib;
function g_param_value_convert(pspec:PGParamSpec; src_value:PGValue; dest_value:PGValue; strict_validation:gboolean):gboolean; cdecl; external gliblib;
function g_param_values_cmp(pspec:PGParamSpec; value1:PGValue; value2:PGValue):gint; cdecl; external gliblib;
function g_param_spec_get_name(pspec:PGParamSpec):Pgchar; cdecl; external gliblib;
function g_param_spec_get_nick(pspec:PGParamSpec):Pgchar; cdecl; external gliblib;
function g_param_spec_get_blurb(pspec:PGParamSpec):Pgchar; cdecl; external gliblib;
procedure g_value_set_param(value:PGValue; param:PGParamSpec); cdecl; external gliblib;
function g_value_get_param(value:PGValue):PGParamSpec; cdecl; external gliblib;
function g_value_dup_param(value:PGValue):PGParamSpec; cdecl; external gliblib;
{ --- marshaller specific ---  }
procedure g_value_set_param_take_ownership(value:PGValue; param:PGParamSpec); cdecl; external gliblib;
{ --- convenience functions ---  }


function g_param_type_register_static(name:Pgchar; pspec_info:PGParamSpecTypeInfo):GType; cdecl; external gliblib;
{ For registering builting types  }
function _g_param_type_register_static_constant(name:Pgchar; pspec_info:PGParamSpecTypeInfo; opt_type:GType):GType; cdecl; external gliblib;
{ --- protected ---  }
function g_param_spec_internal(param_type:GType; name:Pgchar; nick:Pgchar; blurb:Pgchar; flags:TGParamFlags):gpointer; cdecl; external gliblib;
function g_param_spec_pool_new(type_prefixing:gboolean):PGParamSpecPool; cdecl; external gliblib;
procedure g_param_spec_pool_insert(pool:PGParamSpecPool; pspec:PGParamSpec; owner_type:GType); cdecl; external gliblib;
procedure g_param_spec_pool_remove(pool:PGParamSpecPool; pspec:PGParamSpec); cdecl; external gliblib;
function g_param_spec_pool_lookup(pool:PGParamSpecPool; param_name:Pgchar; owner_type:GType; walk_ancestors:gboolean):PGParamSpec; cdecl; external gliblib;
function g_param_spec_pool_list_owned(pool:PGParamSpecPool; owner_type:GType):PGList; cdecl; external gliblib;
function g_param_spec_pool_list(pool:PGParamSpecPool; owner_type:GType; n_pspecs_p:Pguint):PPGParamSpec; cdecl; external gliblib;

{ contracts:

   gboolean value_validate (GParamSpec  pspec,
                            GValue      value):
        modify value contents in the least destructive way, so
        that it complies with pspec's requirements (i.e.
        according to minimum/maximum ranges etc...). return
        whether modification was necessary.

   gint values_cmp (GParamSpec    pspec,
                    const GValue  value1,
                    const GValue  value2):
        return value1 - value2, i.e. (-1) if value1 < value2,
        (+1) if value1 > value2, and (0) otherwise (equality)
  }
{$ENDIF read_interface_rest}

// included by glib2.pas

