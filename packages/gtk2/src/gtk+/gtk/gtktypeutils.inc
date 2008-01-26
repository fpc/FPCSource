// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkArg = ^TGtkArg;

{ glib type wrappers (compatibility)  }
   PGtkType = ^TGtkType;
   TGtkType = GType;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Fundamental Types
   many of these are just aliases for GLib types to maintain
   compatibility
  }
{< skip > }
{ GtkArg types  }
   PGtkFundamentalType = ^TGtkFundamentalType;
   TGtkFundamentalType = GType;


{$ifndef GTK_DISABLE_DEPRECATED}
   PGtkTypeObject = ^TGtkTypeObject;
   TGtkTypeObject = TGTypeInstance;

   PGtkTypeClass = ^TGtkTypeClass;
   TGtkTypeClass = TGTypeClass;

   PGtkClassInitFunc = ^TGtkClassInitFunc;
   TGtkClassInitFunc = TGBaseInitFunc;

   PGtkObjectInitFunc = ^TGtkObjectInitFunc;
   TGtkObjectInitFunc = TGInstanceInitFunc;
{$endif}


{ --- typedefs ---  }
{ here we come with some necessary forward declarations for structures and
   provide some fundamental function signatures
  }
{ anObject forward declaration  }

   TGtkFunction = function (data:gpointer):gboolean; cdecl;

   TGtkDestroyNotify = procedure (data:gpointer); cdecl;

   TGtkCallbackMarshal = procedure (anObject:PGtkObject; data:gpointer; n_args:guint; args:PGtkArg); cdecl;

   TGtkSignalFuncProc = procedure;
   TGtkSignalFunc = procedure (para1: TGtkSignalFuncProc); cdecl;



   PGtkSignalMarshaller = ^TGtkSignalMarshaller;
   TGtkSignalMarshaller = TGSignalCMarshaller;
{ GtkArg, used to hold differently typed values  }
{ this union only defines the required storage types for
     the possibile values, thus there is no gint enum_data field,
     because that would just be a mere alias for gint int_data.
     use the GTK_VALUE_ () and GTK_RETLOC_ () macros to access
     the discrete memebers.
    }
{ flat values  }
{ structured values  }
   TGtkArgSignalData = record
                    f : TGtkSignalFunc;
                    d : gpointer;
                 end;

   TGtkArg = record
        _type : TGtkType;
        name : Pgchar;
        d : record
            case longint of
               0 : ( char_data : gchar );
               1 : ( uchar_data : guchar );
               2 : ( bool_data : gboolean );
               3 : ( int_data : gint );
               4 : ( uint_data : guint );
               5 : ( long_data : glong );
               6 : ( ulong_data : gulong );
               7 : ( float_data : gfloat );
               8 : ( double_data : gdouble );
               9 : ( string_data : Pgchar );
               10 : ( object_data : PGtkObject );
               11 : ( pointer_data : gpointer );
               12 : ( signal_data : TGtkArgSignalData);
            end;
     end;

{ GTK_RETLOC_SIGNAL() - no such thing  }
{ type registration, it is recommended to use
   g_type_register_static() or
   g_type_register_dynamic() instead
  }
   PGtkTypeInfo = ^TGtkTypeInfo;
   TGtkTypeInfo = record
        type_name : Pgchar;
        object_size : guint;
        class_size : guint;
        class_init_func : TGtkClassInitFunc;
        object_init_func : TGtkObjectInitFunc;
        reserved_1 : gpointer;
        reserved_2 : gpointer;
        base_class_init_func : TGtkClassInitFunc;
     end;


   PGtkEnumValue = ^TGtkEnumValue;
   TGtkEnumValue = TGEnumValue;

   PGtkFlagValue = ^TGtkFlagValue;
   TGtkFlagValue = TGFlagsValue;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_TYPE_INVALID = G_TYPE_INVALID;
   GTK_TYPE_NONE = G_TYPE_NONE;
   GTK_TYPE_ENUM = G_TYPE_ENUM;
   GTK_TYPE_FLAGS = G_TYPE_FLAGS;
   GTK_TYPE_CHAR = G_TYPE_CHAR;
   GTK_TYPE_UCHAR = G_TYPE_UCHAR;
   GTK_TYPE_BOOL = G_TYPE_BOOLEAN;
   GTK_TYPE_INT = G_TYPE_INT;
   GTK_TYPE_UINT = G_TYPE_UINT;
   GTK_TYPE_LONG = G_TYPE_LONG;
   GTK_TYPE_ULONG = G_TYPE_ULONG;
   GTK_TYPE_FLOAT = G_TYPE_FLOAT;
   GTK_TYPE_DOUBLE = G_TYPE_DOUBLE;
   GTK_TYPE_STRING = G_TYPE_STRING;
   GTK_TYPE_BOXED = G_TYPE_BOXED;
   GTK_TYPE_POINTER = G_TYPE_POINTER;

{$ifndef GTK_DISABLE_DEPRECATED}
const
   // G_TYPE_LAST_RESERVED_FUNDAMENTAL is a glib version dependent macro
   // GTK_TYPE_FUNDAMENTAL_LAST = G_TYPE_LAST_RESERVED_FUNDAMENTAL - 1;
   GTK_TYPE_FUNDAMENTAL_MAX = G_TYPE_FUNDAMENTAL_MAX;
   // This is special c macro: GTK_STRUCT_OFFSET = G_STRUCT_OFFSET;

function GTK_FUNDAMENTAL_TYPE(_type: GType): GType; // = G_TYPE_FUNDAMENTAL;
function GTK_CHECK_CAST(instance: Pointer; g_type: GType) : PGTypeInstance; // = G_TYPE_CHECK_INSTANCE_CAST;
function GTK_CHECK_CLASS_CAST(g_class: pointer; g_type: GType) : Pointer; // = G_TYPE_CHECK_CLASS_CAST;
function GTK_CHECK_GET_CLASS(instance: Pointer; g_type: GType) : PGTypeClass; // = G_TYPE_INSTANCE_GET_CLASS;
function GTK_CHECK_TYPE(instance: Pointer; g_type: GType) : boolean; // = G_TYPE_CHECK_INSTANCE_TYPE;
function GTK_CHECK_CLASS_TYPE(g_class: pointer; g_type : GType) : boolean; // = G_TYPE_CHECK_CLASS_TYPE;


{ --- type macros ---  }
function GTK_CLASS_NAME(_class : pointer) : PGChar;
function GTK_CLASS_TYPE(_class : pointer) :GType;
function GTK_TYPE_IS_OBJECT(_type : GType) : gboolean;

{ outdated macros that really shouldn't e used anymore,
   use the GLib type system instead
  }
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{ urg  }
function GTK_TYPE_IDENTIFIER : GType;
function gtk_identifier_get_type:GType; cdecl; external gtklib;
function GTK_SIGNAL_FUNC(f : pointer) : TGtkSignalFunc;


function gtk_type_class(_type:TGtkType):gpointer; cdecl; external gtklib;


{$ifndef GTK_DISABLE_DEPRECATED}
{ argument value access macros, these must not contain casts,
   to allow the usage of these macros in combination with the
   adress operator, e.g. &GTK_VALUE_CHAR ( arg)
  }
function GTK_VALUE_CHAR(a : TGtkArg) : gchar;
function GTK_VALUE_UCHAR(a : TGtkArg) : guchar;
function GTK_VALUE_BOOL(a : TGtkArg) : gboolean;
function GTK_VALUE_INT(a : TGtkArg) : gint;
function GTK_VALUE_UINT(a : TGtkArg) : guint;
function GTK_VALUE_LONG(a : TGtkArg) : glong;
function GTK_VALUE_ULONG(a : TGtkArg) : gulong;
function GTK_VALUE_FLOAT(a : TGtkArg) : gfloat;
function GTK_VALUE_DOUBLE(a : TGtkArg) : gdouble;
function GTK_VALUE_STRING(a : TGtkArg) : PGChar;
function GTK_VALUE_ENUM(a : TGtkArg) : gint;
function GTK_VALUE_FLAGS(a : TGtkArg) : guint;
function GTK_VALUE_BOXED(a : TGtkArg) : gpointer;
function GTK_VALUE_OBJECT(a : TGtkArg) : PGtkObject;
function GTK_VALUE_POINTER(a : TGtkArg) : GPointer;
function GTK_VALUE_SIGNAL(a : TGtkArg) : TGtkArgSignalData;

{ return location macros, these all narrow down to
   pointer types, because return values need to be
   passed by reference
  }
function GTK_RETLOC_CHAR(a : TGtkArg) : Pgchar;
function GTK_RETLOC_UCHAR(a : TGtkArg) : Pguchar;
function GTK_RETLOC_BOOL(a : TGtkArg) : Pgboolean;
function GTK_RETLOC_INT(a : TGtkArg) : Pgint;
function GTK_RETLOC_UINT(a : TGtkArg) : Pguint;
function GTK_RETLOC_LONG(a : TGtkArg) : Pglong;
function GTK_RETLOC_ULONG(a : TGtkArg) : Pgulong;
function GTK_RETLOC_FLOAT(a : TGtkArg) : Pgfloat;
function GTK_RETLOC_DOUBLE(a : TGtkArg) : Pgdouble;
function GTK_RETLOC_STRING(a : TGtkArg) : Ppgchar;
function GTK_RETLOC_ENUM(a : TGtkArg) : Pgint;
function GTK_RETLOC_FLAGS(a : TGtkArg) : Pguint;
function GTK_RETLOC_BOXED(a : TGtkArg) : Pgpointer;
function GTK_RETLOC_OBJECT(a : TGtkArg) : PPGtkObject;
function GTK_RETLOC_POINTER(a : TGtkArg) : Pgpointer;


function gtk_type_unique(parent_type:TGtkType; gtkinfo:PGtkTypeInfo):TGtkType; cdecl; external gtklib;
function gtk_type_new(_type:TGtkType):gpointer; cdecl; external gtklib;
{ --- compatibility defines ---  }
function gtk_type_name(_type : GType) : Pgchar;
function gtk_type_from_name(name : Pgchar) : GType;
function gtk_type_parent(_type : GType) : GType;
function gtk_type_is_a(_type,is_a_type : GType) : gboolean;

{ deprecated, use g_type_init() instead  }
procedure gtk_type_init(debug_flags:TGTypeDebugFlags); cdecl; external gtklib;
{ enum/flags compatibility functions, we strongly
   recommend to use the glib enum/flags classes directly
  }

function gtk_type_enum_get_values(enum_type:TGtkType):PGtkEnumValue; cdecl; external gtklib;
function gtk_type_flags_get_values(flags_type:TGtkType):PGtkFlagValue; cdecl; external gtklib;
function gtk_type_enum_find_value(enum_type:TGtkType; value_name:Pgchar):PGtkEnumValue; cdecl; external gtklib;
function gtk_type_flags_find_value(flags_type:TGtkType; value_name:Pgchar):PGtkFlagValue; cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_CLASS_NAME(_class : pointer) : PGChar;
begin
   GTK_CLASS_NAME:=g_type_name(G_TYPE_FROM_CLASS(_class));
end;

function GTK_CLASS_TYPE(_class : pointer) :GType;
begin
   GTK_CLASS_TYPE:=G_TYPE_FROM_CLASS(_class);
end;

function GTK_TYPE_IS_OBJECT(_type : GType) : gboolean;
begin
   GTK_TYPE_IS_OBJECT:=g_type_is_a(_type,GTK_TYPE_OBJECT);
end;

function GTK_TYPE_IDENTIFIER : GType;
begin
   GTK_TYPE_IDENTIFIER:=gtk_identifier_get_type;
end;

function GTK_SIGNAL_FUNC(f : pointer) : TGtkSignalFunc;
begin
   GTK_SIGNAL_FUNC:=TGtkSignalFunc(f);
end;

function gtk_type_name(_type : GType) : Pgchar;
begin
   gtk_type_name:=g_type_name(_type);
end;

function gtk_type_from_name(name : Pgchar) : GType;
begin
   gtk_type_from_name:=g_type_from_name(name);
end;

function gtk_type_parent(_type : GType) : GType;
begin
   gtk_type_parent:=g_type_parent(_type);
end;

function gtk_type_is_a(_type,is_a_type : GType) : gboolean;
begin
   gtk_type_is_a:=g_type_is_a(_type,is_a_type);
end;

function GTK_FUNDAMENTAL_TYPE(_type: GType): GType;
begin
   GTK_FUNDAMENTAL_TYPE:=G_TYPE_FUNDAMENTAL(_type);
end;

function GTK_CHECK_CAST(instance: Pointer; g_type: GType) : PGTypeInstance;
begin
   GTK_CHECK_CAST:=G_TYPE_CHECK_INSTANCE_CAST(instance,g_type);
end;

function GTK_CHECK_CLASS_CAST(g_class: pointer; g_type: GType) : Pointer;
begin
   GTK_CHECK_CLASS_CAST:=G_TYPE_CHECK_CLASS_CAST(g_class,g_type);
end;

function GTK_CHECK_GET_CLASS(instance: Pointer; g_type: GType) : PGTypeClass;
begin
   GTK_CHECK_GET_CLASS:=G_TYPE_INSTANCE_GET_CLASS(instance,g_type);
end;

function GTK_CHECK_TYPE(instance: Pointer; g_type: GType) : boolean;
begin
   GTK_CHECK_TYPE:=G_TYPE_CHECK_INSTANCE_TYPE(instance,g_type);
end;

function GTK_CHECK_CLASS_TYPE(g_class: pointer; g_type : GType) : boolean;
begin
   GTK_CHECK_CLASS_TYPE:=G_TYPE_CHECK_CLASS_TYPE(g_class,g_type);
end;


function GTK_VALUE_CHAR(a : TGtkArg) : gchar;
begin
  Result:=a.d.char_data;
end;

function GTK_VALUE_UCHAR(a : TGtkArg) : guchar;
begin
  Result:=a.d.uchar_data;
end;

function GTK_VALUE_BOOL(a : TGtkArg) : gboolean;
begin
  Result:=a.d.bool_data;
end;

function GTK_VALUE_INT(a : TGtkArg) : gint;
begin
  Result:=a.d.int_data;
end;

function GTK_VALUE_UINT(a : TGtkArg) : guint;
begin
  Result:=a.d.uint_data;
end;

function GTK_VALUE_LONG(a : TGtkArg) : glong;
begin
  Result:=a.d.long_data;
end;

function GTK_VALUE_ULONG(a : TGtkArg) : gulong;
begin
  Result:=a.d.ulong_data;
end;

function GTK_VALUE_FLOAT(a : TGtkArg) : gfloat;
begin
  Result:=a.d.float_data;
end;

function GTK_VALUE_DOUBLE(a : TGtkArg) : gdouble;
begin
  Result:=a.d.double_data;
end;

function GTK_VALUE_STRING(a : TGtkArg) : PGChar;
begin
  Result:=a.d.string_data;
end;

function GTK_VALUE_ENUM(a : TGtkArg) : gint;
begin
  Result:=a.d.int_data;
end;

function GTK_VALUE_FLAGS(a : TGtkArg) : guint;
begin
  Result:=a.d.uint_data;
end;

function GTK_VALUE_BOXED(a : TGtkArg) : gpointer;
begin
  Result:=a.d.pointer_data;
end;

function GTK_VALUE_OBJECT(a : TGtkArg) : PGtkObject;
begin
  Result:=a.d.object_data;
end;

function GTK_VALUE_POINTER(a : TGtkArg) : GPointer;
begin
  Result:=a.d.pointer_data;
end;

function GTK_VALUE_SIGNAL(a : TGtkArg) : TGtkArgSignalData;
begin
  Result:=a.d.signal_data;
end;

function GTK_RETLOC_CHAR(a : TGtkArg) : Pgchar;
begin
  Result:=PGChar(a.d.pointer_data);
end;

function GTK_RETLOC_UCHAR(a : TGtkArg) : Pguchar;
begin
  Result:=PGUChar(a.d.pointer_data);
end;

function GTK_RETLOC_BOOL(a : TGtkArg) : Pgboolean;
begin
  Result:=PGboolean(a.d.pointer_data);
end;

function GTK_RETLOC_INT(a : TGtkArg) : Pgint;
begin
  Result:=PGInt(a.d.pointer_data);
end;

function GTK_RETLOC_UINT(a : TGtkArg) : Pguint;
begin
  Result:=PGUInt(a.d.pointer_data);
end;

function GTK_RETLOC_LONG(a : TGtkArg) : Pglong;
begin
  Result:=PGLong(a.d.pointer_data);
end;

function GTK_RETLOC_ULONG(a : TGtkArg) : Pgulong;
begin
  Result:=PGULong(a.d.pointer_data);
end;

function GTK_RETLOC_FLOAT(a : TGtkArg) : Pgfloat;
begin
  Result:=PGFloat(a.d.pointer_data);
end;

function GTK_RETLOC_DOUBLE(a : TGtkArg) : Pgdouble;
begin
  Result:=PGDouble(a.d.pointer_data);
end;

function GTK_RETLOC_STRING(a : TGtkArg) : Ppgchar;
begin
  Result:=PPGChar(a.d.pointer_data);
end;

function GTK_RETLOC_ENUM(a : TGtkArg) : Pgint;
begin
  Result:=PGInt(a.d.pointer_data);
end;

function GTK_RETLOC_FLAGS(a : TGtkArg) : Pguint;
begin
  Result:=PGUint(a.d.pointer_data);
end;

function GTK_RETLOC_BOXED(a : TGtkArg) : Pgpointer;
begin
  Result:=PGPointer(a.d.pointer_data);
end;

function GTK_RETLOC_OBJECT(a : TGtkArg) : PPGtkObject;
begin
  Result:=PPGtkObject(a.d.pointer_data);
end;

function GTK_RETLOC_POINTER(a : TGtkArg) : Pgpointer;
begin
  Result:=PGPointer(a.d.pointer_data);
end;


{$ENDIF read_implementation}
// included by gtk2.pas

