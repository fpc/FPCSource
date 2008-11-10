// included by glib2.pas

{$IFDEF read_forward_definitions}
   PGType = ^GType;
   GType = gulong;

   PGTypeClass = ^TGTypeClass;
   TGTypeClass = record
     g_type : GType;
   end;

   PGTypeInstance = ^TGTypeInstance;
   TGTypeInstance = record
     g_class : PGTypeClass;
   end;

   PGTypeInterface = ^TGTypeInterface;
   TGTypeInterface = record
     g_type : GType;
     g_instance_type : GType;
   end;

   PGTypeQuery = ^TGTypeQuery;
   TGTypeQuery = record
     _type : GType;
     type_name : Pgchar;
     class_size : guint;
     instance_size : guint;
   end;
{$ENDIF read_forward_definitions}


//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

   PGTypeDebugFlags = ^TGTypeDebugFlags;
   TGTypeDebugFlags = longint;

   TGBaseInitFunc = procedure (g_class:gpointer); cdecl;
   TGBaseFinalizeFunc = procedure (g_class:gpointer); cdecl;
   TGClassInitFunc = procedure (g_class:gpointer; class_data:gpointer); cdecl;
   TGClassFinalizeFunc = procedure (g_class:gpointer; class_data:gpointer); cdecl;
   TGInstanceInitFunc = procedure (instance:PGTypeInstance; g_class:gpointer); cdecl;
   TGInterfaceInitFunc = procedure (g_iface:gpointer; iface_data:gpointer); cdecl;
   TGInterfaceFinalizeFunc = procedure (g_iface:gpointer; iface_data:gpointer); cdecl;
   TGTypeClassCacheFunc = function (cache_data:gpointer; g_class:PGTypeClass):gboolean; cdecl;
   TGTranslateFunc = function (str:Pgchar;data:gpointer):Pgchar; cdecl;

   PGTypeFundamentalFlags = ^TGTypeFundamentalFlags;
   TGTypeFundamentalFlags = longint;

   PGTypeFlags = ^TGTypeFlags;
   TGTypeFlags = longint;

   PGTypeValueTable = ^TGTypeValueTable;
   TGTypeValueTable = record
        value_init : procedure (value:PGValue); cdecl;
        value_free : procedure (value:PGValue); cdecl;
        value_copy : procedure (src_value:PGValue; dest_value:PGValue); cdecl;
        value_peek_pointer : function (value:PGValue):gpointer; cdecl;
        collect_format : Pgchar;
        collect_value : function (value:PGValue; n_collect_values:guint; collect_values:PGTypeCValue; collect_flags:guint):Pgchar; cdecl;
        lcopy_format : Pgchar;
        lcopy_value : function (value:PGValue; n_collect_values:guint; collect_values:PGTypeCValue; collect_flags:guint):Pgchar; cdecl;
     end;


   PGTypeInfo = ^TGTypeInfo;
   TGTypeInfo = record
        class_size : guint16;
        base_init : TGBaseInitFunc;
        base_finalize : TGBaseFinalizeFunc;
        class_init : TGClassInitFunc;
        class_finalize : TGClassFinalizeFunc;
        class_data : gconstpointer;
        instance_size : guint16;
        n_preallocs : guint16;
        instance_init : TGInstanceInitFunc;
        value_table : PGTypeValueTable;
     end;

   PGTypeFundamentalInfo = ^TGTypeFundamentalInfo;
   TGTypeFundamentalInfo = record
        type_flags : TGTypeFundamentalFlags;
     end;

   PGInterfaceInfo = ^TGInterfaceInfo;
   TGInterfaceInfo = record
        interface_init : TGInterfaceInitFunc;
        interface_finalize : TGInterfaceFinalizeFunc;
        interface_data : gpointer;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ Basic Type Macros }

const
   G_TYPE_FUNDAMENTAL_SHIFT = 2;
   G_TYPE_FUNDAMENTAL_MAX = 255 shl G_TYPE_FUNDAMENTAL_SHIFT;

{ Constant fundamental types,
   introduced by g_type_init(). }
   G_TYPE_INVALID = GType(0 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_NONE = GType(1 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_INTERFACE = GType(2 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_CHAR = GType(3 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_UCHAR = GType(4 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_BOOLEAN = GType(5 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_INT = GType(6 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_UINT = GType(7 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_LONG = GType(8 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_ULONG = GType(9 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_INT64 = GType(10 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_UINT64 = GType(11 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_ENUM = GType(12 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_FLAGS = GType(13 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_FLOAT = GType(14 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_DOUBLE = GType(15 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_STRING = GType(16 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_POINTER = GType(17 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_BOXED = GType(18 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_PARAM = GType(19 shl G_TYPE_FUNDAMENTAL_SHIFT);
   G_TYPE_OBJECT = GType(20 shl G_TYPE_FUNDAMENTAL_SHIFT);

{ Reserved fundamental type numbers to create new fundamental
   type IDs with G_TYPE_MAKE_FUNDAMENTAL().
   Send email to gtk-devel-list@redhat.com for reservations.
  }

function G_TYPE_MAKE_FUNDAMENTAL(x : longint) : GType;

const
   G_TYPE_RESERVED_GLIB_FIRST = 21;
   G_TYPE_RESERVED_GLIB_LAST = 31;
   G_TYPE_RESERVED_BSE_FIRST = 32;
   G_TYPE_RESERVED_BSE_LAST = 48;
   G_TYPE_RESERVED_USER_FIRST = 49;


{ Type Checking Macros }

function G_TYPE_IS_FUNDAMENTAL(_type : GType) : boolean;
function G_TYPE_IS_DERIVED(_type : GType) : boolean;
function G_TYPE_IS_INTERFACE(_type : GType) : boolean;
function G_TYPE_IS_CLASSED(_type : GType) : gboolean;
function G_TYPE_IS_INSTANTIATABLE(_type : GType) : boolean;
function G_TYPE_IS_DERIVABLE(_type : GType) : boolean;
function G_TYPE_IS_DEEP_DERIVABLE(_type : GType) : boolean;
function G_TYPE_IS_ABSTRACT(_type : GType) : boolean;
function G_TYPE_IS_VALUE_ABSTRACT(_type : GType) : boolean;
function G_TYPE_IS_VALUE_TYPE(_type : GType) : boolean;
function G_TYPE_HAS_VALUE_TABLE(_type : GType) : boolean;

{ Casts, checks and accessors for structured types
   usage of these macros is reserved to type implementations only }
{< protected > }
function G_TYPE_CHECK_INSTANCE(instance : Pointer) : gboolean;
function G_TYPE_CHECK_INSTANCE_CAST(instance: Pointer; g_type: GType) : PGTypeInstance;
function G_TYPE_CHECK_INSTANCE_TYPE(instance: Pointer; g_type: GType) : boolean;
function G_TYPE_INSTANCE_GET_CLASS(instance: Pointer; g_type: GType) : PGTypeClass;
function G_TYPE_INSTANCE_GET_INTERFACE(instance: Pointer; g_type: GType) : Pointer;
function G_TYPE_CHECK_CLASS_CAST(g_class: pointer; g_type: GType) : Pointer;
function G_TYPE_CHECK_CLASS_TYPE(g_class: pointer; g_type : GType) : boolean;
function G_TYPE_CHECK_VALUE(value : Pointer) : boolean;
function G_TYPE_CHECK_VALUE_TYPE(value: pointer; g_type : GType) : boolean;
function G_TYPE_FROM_INSTANCE(instance : Pointer) : GType;
function G_TYPE_FROM_CLASS(g_class : Pointer) : GType;
function G_TYPE_FROM_INTERFACE(g_iface : Pointer) : GType;

{ debug flags for g_type_init_with_debug_flags()  }
{< skip > }
const
   G_TYPE_DEBUG_NONE = 0;
   G_TYPE_DEBUG_OBJECTS = 1 shl 0;
   G_TYPE_DEBUG_SIGNALS = 1 shl 1;
   G_TYPE_DEBUG_MASK = $03;


{ --- prototypes ---  }

procedure g_type_init; cdecl; external gobjectlib;
procedure g_type_init_with_debug_flags(debug_flags:TGTypeDebugFlags); cdecl; external gobjectlib;
function g_type_name(_type:GType):Pgchar; cdecl; external gobjectlib;
function g_type_qname(_type:GType):TGQuark; cdecl; external gobjectlib;
function g_type_from_name(name:Pgchar):GType; cdecl; external gobjectlib;
function g_type_parent(_type:GType):GType; cdecl; external gobjectlib;
function g_type_depth(_type:GType):guint; cdecl; external gobjectlib;
function g_type_next_base(leaf_type:GType; root_type:GType):GType; cdecl; external gobjectlib;
function g_type_is_a(_type:GType; is_a_type:GType):gboolean; cdecl; external gobjectlib;
function g_type_class_ref(_type:GType):gpointer; cdecl; external gobjectlib;
function g_type_class_peek(_type:GType):gpointer; cdecl; external gobjectlib;
procedure g_type_class_unref(g_class:gpointer); cdecl; external gobjectlib;
function g_type_class_peek_parent(g_class:gpointer):gpointer; cdecl; external gobjectlib;
function g_type_interface_peek(instance_class:gpointer; iface_type:GType):gpointer; cdecl; external gobjectlib;
function g_type_interface_peek_parent(g_iface:gpointer):gpointer; cdecl; external gobjectlib;
{ g_free() the returned arrays  }
function g_type_children(_type:GType; n_children:Pguint):PGType; cdecl; external gobjectlib;
function g_type_interfaces(_type:GType; n_interfaces:Pguint):PGType; cdecl; external gobjectlib;
{ per-type _static_ data  }
procedure g_type_set_qdata(_type:GType; quark:TGQuark; data:gpointer); cdecl; external gobjectlib;
function g_type_get_qdata(_type:GType; quark:TGQuark):gpointer; cdecl; external gobjectlib;
procedure g_type_query(_type:GType; query:PGTypeQuery); cdecl; external gobjectlib;


const
   G_TYPE_FLAG_CLASSED        = 1 shl 0;
   G_TYPE_FLAG_INSTANTIATABLE = 1 shl 1;
   G_TYPE_FLAG_DERIVABLE      = 1 shl 2;
   G_TYPE_FLAG_DEEP_DERIVABLE = 1 shl 3;

const
   G_TYPE_FLAG_ABSTRACT = 1 shl 4;
   G_TYPE_FLAG_VALUE_ABSTRACT = 1 shl 5;

function g_type_register_static(parent_type:GType; type_name:Pgchar; info:PGTypeInfo; flags:TGTypeFlags):GType; cdecl; external gobjectlib;
function g_type_register_dynamic(parent_type:GType; type_name:Pgchar; plugin:PGTypePlugin; flags:TGTypeFlags):GType; cdecl; external gobjectlib;
function g_type_register_fundamental(type_id:GType; type_name:Pgchar; info:PGTypeInfo; finfo:PGTypeFundamentalInfo; flags:TGTypeFlags):GType; cdecl; external gobjectlib;
procedure g_type_add_interface_static(instance_type:GType; interface_type:GType; info:PGInterfaceInfo); cdecl; external gobjectlib;
procedure g_type_add_interface_dynamic(instance_type:GType; interface_type:GType; plugin:PGTypePlugin); cdecl; external gobjectlib;
procedure g_type_interface_add_prerequisite(interface_type:GType; prerequisite_type:GType); cdecl; external gobjectlib;
{ --- protected (for fundamental type implementations) ---  }
function g_type_get_plugin(_type:GType):PGTypePlugin; cdecl; external gobjectlib;
function g_type_interface_get_plugin(instance_type:GType; implementation_type:GType):PGTypePlugin; cdecl; external gobjectlib;
function g_type_fundamental_next:GType; cdecl; external gobjectlib;
function g_type_fundamental(type_id:GType):GType; cdecl; external gobjectlib;
function g_type_create_instance(_type:GType):PGTypeInstance; cdecl; external gobjectlib;
procedure g_type_free_instance(instance:PGTypeInstance); cdecl; external gobjectlib;
procedure g_type_add_class_cache_func(cache_data:gpointer; cache_func:TGTypeClassCacheFunc); cdecl; external gobjectlib;
procedure g_type_remove_class_cache_func(cache_data:gpointer; cache_func:TGTypeClassCacheFunc); cdecl; external gobjectlib;
procedure g_type_class_unref_uncached(g_class:gpointer); cdecl; external gobjectlib;
function g_type_value_table_peek(_type:GType):PGTypeValueTable; cdecl; external gobjectlib;
{< private > }
function private_g_type_check_instance(instance:PGTypeInstance):gboolean; cdecl; external gobjectlib name 'g_type_check_instance';
function private_g_type_check_instance_cast(instance:PGTypeInstance; iface_type:GType):PGTypeInstance; cdecl; external gobjectlib name 'g_type_check_instance_cast';
function private_g_type_check_instance_is_a(instance:PGTypeInstance; iface_type:GType):gboolean; cdecl; external gobjectlib name 'g_type_check_instance_is_a';
function private_g_type_check_class_cast(g_class:PGTypeClass; is_a_type:GType):PGTypeClass; cdecl; external gobjectlib name 'g_type_check_class_cast';
function private_g_type_check_class_is_a(g_class:PGTypeClass; is_a_type:GType):gboolean; cdecl; external gobjectlib name 'g_type_check_class_is_a';
function private_g_type_check_is_value_type(_type:GType):gboolean; cdecl; external gobjectlib name 'g_type_check_is_value_type';
function private_g_type_check_value(value:PGValue):gboolean; cdecl; external gobjectlib name 'g_type_check_value';
function private_g_type_check_value_holds(value:PGValue; _type:GType):gboolean; cdecl; external gobjectlib name 'g_type_check_value_holds';
function private_g_type_test_flags(_type:GType; flags:guint):gboolean; cdecl; external gobjectlib name 'g_type_test_flags';
{ --- debugging functions ---  }
function g_type_name_from_instance(instance:PGTypeInstance):Pgchar; cdecl; external gobjectlib;
function g_type_name_from_class(g_class:PGTypeClass):Pgchar; cdecl; external gobjectlib;
{ --- implementation bits ---  }

const
  G_TYPE_FLAG_RESERVED_ID_BIT = GType(1 shl 0);

{$ENDIF read_interface_rest}

// included by glib2.pas

