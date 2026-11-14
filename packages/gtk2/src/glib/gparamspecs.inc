// included by glib2.pas

{$IFDEF read_forward_definitions}
  PGParamSpecChar = ^TGParamSpecChar;
  PGParamSpecUChar = ^TGParamSpecUChar;
  PGParamSpecBoolean = ^TGParamSpecBoolean;
  PGParamSpecInt = ^TGParamSpecInt;
  PGParamSpecUInt = ^TGParamSpecUInt;
  PGParamSpecLong = ^TGParamSpecLong;
  PGParamSpecULong = ^TGParamSpecULong;
  PGParamSpecInt64 = ^TGParamSpecInt64;
  PGParamSpecUInt64 = ^TGParamSpecUInt64;
  PGParamSpecUnichar = ^TGParamSpecUnichar;
  PGParamSpecEnum = ^TGParamSpecEnum;
  PGParamSpecFlags = ^TGParamSpecFlags;
  PGParamSpecFloat = ^TGParamSpecFloat;
  PGParamSpecDouble = ^TGParamSpecDouble;
  PGParamSpecString = ^TGParamSpecString;
  PGParamSpecParam = ^TGParamSpecParam;
  PGParamSpecBoxed = ^TGParamSpecBoxed;
  PGParamSpecPointer = ^TGParamSpecPointer;
  PGParamSpecValueArray = ^TGParamSpecValueArray;
  PGParamSpecObject = ^TGParamSpecObject;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  TGParamSpecChar = record
    parent_instance : TGParamSpec;
    minimum : gint8;
    maximum : gint8;
    default_value : gint8;
  end;

  TGParamSpecUChar = record
    parent_instance : TGParamSpec;
    minimum : guint8;
    maximum : guint8;
    default_value : guint8;
  end;

  TGParamSpecBoolean = record
    parent_instance : TGParamSpec;
    default_value : gboolean;
  end;

  TGParamSpecInt = record
    parent_instance : TGParamSpec;
    minimum : gint;
    maximum : gint;
    default_value : gint;
  end;

  TGParamSpecUInt = record
    parent_instance : TGParamSpec;
    minimum : guint;
    maximum : guint;
    default_value : guint;
  end;

  TGParamSpecLong = record
    parent_instance : TGParamSpec;
    minimum : glong;
    maximum : glong;
    default_value : glong;
  end;

  TGParamSpecULong = record
    parent_instance : TGParamSpec;
    minimum : gulong;
    maximum : gulong;
    default_value : gulong;
  end;

  TGParamSpecInt64 = record
    parent_instance : TGParamSpec;
    minimum : gint64;
    maximum : gint64;
    default_value : gint64;
  end;

  TGParamSpecUInt64 = record
    parent_instance : TGParamSpec;
    minimum : guint64;
    maximum : guint64;
    default_value : guint64;
  end;

  TGParamSpecUnichar = record
    parent_instance : TGParamSpec;
    default_value : gunichar;
  end;

  TGParamSpecEnum = record
    parent_instance : TGParamSpec;
    enum_class : PGEnumClass;
    default_value : gint;
  end;

  TGParamSpecFlags = record
    parent_instance : TGParamSpec;
    flags_class : PGFlagsClass;
    default_value : guint;
  end;

  TGParamSpecFloat = record
    parent_instance : TGParamSpec;
    minimum : gfloat;
    maximum : gfloat;
    default_value : gfloat;
    epsilon : gfloat;
  end;

  TGParamSpecDouble = record
    parent_instance : TGParamSpec;
    minimum : gdouble;
    maximum : gdouble;
    default_value : gdouble;
    epsilon : gdouble;
  end;

  TGParamSpecString = record
    parent_instance : TGParamSpec;
    default_value : Pgchar;
    cset_first : Pgchar;
    cset_nth : Pgchar;
    substitutor : gchar;
    // Bit fields null_fold_if_empty and ensure_non_null
    {$IFDEF windows}
    flag0 : guint;
    {$ELSE}
    flag0 : guint8;
    {$ENDIF}
  end;

  TGParamSpecParam = record
    parent_instance : TGParamSpec;
  end;

  TGParamSpecBoxed = record
    parent_instance : TGParamSpec;
  end;

  TGParamSpecPointer = record
    parent_instance : TGParamSpec;
  end;

  TGParamSpecValueArray = record
    parent_instance : TGParamSpec;
    element_spec : PGParamSpec;
    fixed_n_elements : guint;
  end;

  TGParamSpecObject = record
    parent_instance : TGParamSpec;
  end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
{$IFDEF ENDIAN_BIG}
  bm_TGParamSpecString_null_fold_if_empty = $80;
  bp_TGParamSpecString_null_fold_if_empty = 7;
  bm_TGParamSpecString_ensure_non_null = $40;
  bp_TGParamSpecString_ensure_non_null = 6;
{$ELSE}
  bm_TGParamSpecString_null_fold_if_empty = $1;
  bp_TGParamSpecString_null_fold_if_empty = 0;
  bm_TGParamSpecString_ensure_non_null = $2;
  bp_TGParamSpecString_ensure_non_null = 1;
{$ENDIF}

var
  g_param_spec_types : PGType; external gobjectlib name 'g_param_spec_types';

{ --- type macros ---  }
function G_TYPE_PARAM_CHAR : GType;
function G_IS_PARAM_SPEC_CHAR(pspec : Pointer) : boolean;
function G_PARAM_SPEC_CHAR(pspec : Pointer) : PGParamSpecChar;
function G_TYPE_PARAM_UCHAR : GType;
function G_IS_PARAM_SPEC_UCHAR(pspec : Pointer) : boolean;
function G_PARAM_SPEC_UCHAR(pspec : Pointer) : PGParamSpecUChar;
function G_TYPE_PARAM_BOOLEAN : GType;
function G_IS_PARAM_SPEC_BOOLEAN(pspec : Pointer) : boolean;
function G_PARAM_SPEC_BOOLEAN(pspec : Pointer) : PGParamSpecBoolean;
function G_TYPE_PARAM_INT : GType;
function G_IS_PARAM_SPEC_INT(pspec : Pointer) : boolean;
function G_PARAM_SPEC_INT(pspec : Pointer) : PGParamSpecInt;
function G_TYPE_PARAM_UINT : GType;
function G_IS_PARAM_SPEC_UINT(pspec : Pointer) : boolean;
function G_PARAM_SPEC_UINT(pspec : Pointer) : PGParamSpecUInt;
function G_TYPE_PARAM_LONG : GType;
function G_IS_PARAM_SPEC_LONG(pspec : Pointer) : boolean;
function G_PARAM_SPEC_LONG(pspec : Pointer) : PGParamSpecLong;
function G_TYPE_PARAM_ULONG : GType;
function G_IS_PARAM_SPEC_ULONG(pspec : Pointer) : boolean;
function G_PARAM_SPEC_ULONG(pspec : Pointer) : PGParamSpecULong;
function G_TYPE_PARAM_INT64 : GType;
function G_IS_PARAM_SPEC_INT64(pspec : Pointer) : boolean;
function G_PARAM_SPEC_INT64(pspec : Pointer) : PGParamSpecInt64;
function G_TYPE_PARAM_UINT64 : GType;
function G_IS_PARAM_SPEC_UINT64(pspec : Pointer) : boolean;
function G_PARAM_SPEC_UINT64(pspec : Pointer) : PGParamSpecUInt64;
function G_TYPE_PARAM_UNICHAR : GType;
function G_IS_PARAM_SPEC_UNICHAR(pspec : Pointer) : boolean;
function G_PARAM_SPEC_UNICHAR(pspec : Pointer) : PGParamSpecUnichar;
function G_TYPE_PARAM_ENUM : GType;
function G_IS_PARAM_SPEC_ENUM(pspec : Pointer) : boolean;
function G_PARAM_SPEC_ENUM(pspec : Pointer) : PGParamSpecEnum;
function G_TYPE_PARAM_FLAGS : GType;
function G_IS_PARAM_SPEC_FLAGS(pspec : Pointer) : boolean;
function G_PARAM_SPEC_FLAGS(pspec : Pointer) : PGParamSpecFlags;
function G_TYPE_PARAM_FLOAT : GType;
function G_IS_PARAM_SPEC_FLOAT(pspec : Pointer) : boolean;
function G_PARAM_SPEC_FLOAT(pspec : Pointer) : PGParamSpecFloat;
function G_TYPE_PARAM_DOUBLE : GType;
function G_IS_PARAM_SPEC_DOUBLE(pspec : Pointer) : boolean;
function G_PARAM_SPEC_DOUBLE(pspec : Pointer) : PGParamSpecDouble;
function G_TYPE_PARAM_STRING : GType;
function G_IS_PARAM_SPEC_STRING(pspec : Pointer) : boolean;
function G_PARAM_SPEC_STRING(pspec : Pointer) : PGParamSpecString;
function G_TYPE_PARAM_PARAM : GType;
function G_IS_PARAM_SPEC_PARAM(pspec : Pointer) : boolean;
function G_PARAM_SPEC_PARAM(pspec : Pointer) : PGParamSpecParam;
function G_TYPE_PARAM_BOXED : GType;
function G_IS_PARAM_SPEC_BOXED(pspec : Pointer) : boolean;
function G_PARAM_SPEC_BOXED(pspec : Pointer) : PGParamSpecBoxed;
function G_TYPE_PARAM_POINTER : GType;
function G_IS_PARAM_SPEC_POINTER(pspec : Pointer) : boolean;
function G_PARAM_SPEC_POINTER(pspec : Pointer) : PGParamSpecPointer;
function G_TYPE_PARAM_VALUE_ARRAY : GType;
function G_IS_PARAM_SPEC_VALUE_ARRAY(pspec : Pointer) : boolean;
function G_PARAM_SPEC_VALUE_ARRAY(pspec : Pointer) : PGParamSpecValueArray;
function G_TYPE_PARAM_OBJECT : GType;
function G_IS_PARAM_SPEC_OBJECT(pspec : Pointer) : boolean;
function G_PARAM_SPEC_OBJECT(pspec : Pointer) : PGParamSpecObject;

{ --- bit fields of TGParamSpecString ---  }
function null_fold_if_empty(var a : TGParamSpecString) : guint;
procedure set_null_fold_if_empty(var a : TGParamSpecString; __null_fold_if_empty : guint);
function ensure_non_null(var a : TGParamSpecString) : guint;
procedure set_ensure_non_null(var a : TGParamSpecString; __ensure_non_null : guint);

{ --- GParamSpec prototypes ---  }
function g_param_spec_char(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gint8; maximum:gint8;
  default_value:gint8; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_char';
function g_param_spec_uchar(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:guint8; maximum:guint8;
  default_value:guint8; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_uchar';
function g_param_spec_boolean(name:Pgchar; nick:Pgchar; blurb:Pgchar; default_value:gboolean;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_boolean';
function g_param_spec_int(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gint; maximum:gint;
  default_value:gint; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_int';
function g_param_spec_uint(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:guint; maximum:guint;
  default_value:guint; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_uint';
function g_param_spec_long(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:glong; maximum:glong;
  default_value:glong; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_long';
function g_param_spec_ulong(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gulong; maximum:gulong;
  default_value:gulong; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_ulong';
function g_param_spec_int64(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gint64; maximum:gint64;
  default_value:gint64; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_int64';
function g_param_spec_uint64(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:guint64; maximum:guint64;
  default_value:guint64; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_uint64';
function g_param_spec_unichar(name:Pgchar; nick:Pgchar; blurb:Pgchar; default_value:gunichar;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_unichar';
function g_param_spec_enum(name:Pgchar; nick:Pgchar; blurb:Pgchar; enum_type:GType; default_value:gint;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_enum';
function g_param_spec_flags(name:Pgchar; nick:Pgchar; blurb:Pgchar; flags_type:GType; default_value:guint;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_flags';
function g_param_spec_float(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gfloat; maximum:gfloat;
  default_value:gfloat; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_float';
function g_param_spec_double(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gdouble; maximum:gdouble;
  default_value:gdouble; flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_double';
function g_param_spec_string(name:Pgchar; nick:Pgchar; blurb:Pgchar; default_value:Pgchar;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_string';
function g_param_spec_param(name:Pgchar; nick:Pgchar; blurb:Pgchar; param_type:GType;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_param';
function g_param_spec_boxed(name:Pgchar; nick:Pgchar; blurb:Pgchar; boxed_type:GType;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_boxed';
function g_param_spec_pointer(name:Pgchar; nick:Pgchar; blurb:Pgchar;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_pointer';
function g_param_spec_value_array(name:Pgchar; nick:Pgchar; blurb:Pgchar; element_spec:PGParamSpec;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_value_array';
function g_param_spec_object(name:Pgchar; nick:Pgchar; blurb:Pgchar; object_type:GType;
  flags:TGParamFlags):PGParamSpec; cdecl; external gobjectlib name 'g_param_spec_object';
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function G_TYPE_PARAM_CHAR : GType;
begin
  G_TYPE_PARAM_CHAR:=g_param_spec_types[0];
end;

function G_IS_PARAM_SPEC_CHAR(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_CHAR:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_CHAR);
end;

function G_PARAM_SPEC_CHAR(pspec : Pointer) : PGParamSpecChar;
begin
  G_PARAM_SPEC_CHAR:=PGParamSpecChar(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_CHAR));
end;

function G_TYPE_PARAM_UCHAR : GType;
begin
  G_TYPE_PARAM_UCHAR:=g_param_spec_types[1];
end;

function G_IS_PARAM_SPEC_UCHAR(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_UCHAR:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UCHAR);
end;

function G_PARAM_SPEC_UCHAR(pspec : Pointer) : PGParamSpecUChar;
begin
  G_PARAM_SPEC_UCHAR:=PGParamSpecUChar(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UCHAR));
end;

function G_TYPE_PARAM_BOOLEAN : GType;
begin
  G_TYPE_PARAM_BOOLEAN:=g_param_spec_types[2];
end;

function G_IS_PARAM_SPEC_BOOLEAN(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_BOOLEAN:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_BOOLEAN);
end;

function G_PARAM_SPEC_BOOLEAN(pspec : Pointer) : PGParamSpecBoolean;
begin
  G_PARAM_SPEC_BOOLEAN:=PGParamSpecBoolean(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_BOOLEAN));
end;

function G_TYPE_PARAM_INT : GType;
begin
  G_TYPE_PARAM_INT:=g_param_spec_types[3];
end;

function G_IS_PARAM_SPEC_INT(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_INT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_INT);
end;

function G_PARAM_SPEC_INT(pspec : Pointer) : PGParamSpecInt;
begin
  G_PARAM_SPEC_INT:=PGParamSpecInt(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_INT));
end;

function G_TYPE_PARAM_UINT : GType;
begin
  G_TYPE_PARAM_UINT:=g_param_spec_types[4];
end;

function G_IS_PARAM_SPEC_UINT(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_UINT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UINT);
end;

function G_PARAM_SPEC_UINT(pspec : Pointer) : PGParamSpecUInt;
begin
  G_PARAM_SPEC_UINT:=PGParamSpecUInt(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UINT));
end;

function G_TYPE_PARAM_LONG : GType;
begin
  G_TYPE_PARAM_LONG:=g_param_spec_types[5];
end;

function G_IS_PARAM_SPEC_LONG(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_LONG:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_LONG);
end;

function G_PARAM_SPEC_LONG(pspec : Pointer) : PGParamSpecLong;
begin
  G_PARAM_SPEC_LONG:=PGParamSpecLong(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_LONG));
end;

function G_TYPE_PARAM_ULONG : GType;
begin
  G_TYPE_PARAM_ULONG:=g_param_spec_types[6];
end;

function G_IS_PARAM_SPEC_ULONG(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_ULONG:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_ULONG);
end;

function G_PARAM_SPEC_ULONG(pspec : Pointer) : PGParamSpecULong;
begin
  G_PARAM_SPEC_ULONG:=PGParamSpecULong(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_ULONG));
end;

function G_TYPE_PARAM_INT64 : GType;
begin
  G_TYPE_PARAM_INT64:=g_param_spec_types[7];
end;

function G_IS_PARAM_SPEC_INT64(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_INT64:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_INT64);
end;

function G_PARAM_SPEC_INT64(pspec : Pointer) : PGParamSpecInt64;
begin
  G_PARAM_SPEC_INT64:=PGParamSpecInt64(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_INT64));
end;

function G_TYPE_PARAM_UINT64 : GType;
begin
  G_TYPE_PARAM_UINT64:=g_param_spec_types[8];
end;

function G_IS_PARAM_SPEC_UINT64(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_UINT64:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UINT64);
end;

function G_PARAM_SPEC_UINT64(pspec : Pointer) : PGParamSpecUInt64;
begin
  G_PARAM_SPEC_UINT64:=PGParamSpecUInt64(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UINT64));
end;

function G_TYPE_PARAM_UNICHAR : GType;
begin
  G_TYPE_PARAM_UNICHAR:=g_param_spec_types[9];
end;

function G_IS_PARAM_SPEC_UNICHAR(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_UNICHAR:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UNICHAR);
end;

function G_PARAM_SPEC_UNICHAR(pspec : Pointer) : PGParamSpecUnichar;
begin
  G_PARAM_SPEC_UNICHAR:=PGParamSpecUnichar(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UNICHAR));
end;

function G_TYPE_PARAM_ENUM : GType;
begin
  G_TYPE_PARAM_ENUM:=g_param_spec_types[10];
end;

function G_IS_PARAM_SPEC_ENUM(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_ENUM:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_ENUM);
end;

function G_PARAM_SPEC_ENUM(pspec : Pointer) : PGParamSpecEnum;
begin
  G_PARAM_SPEC_ENUM:=PGParamSpecEnum(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_ENUM));
end;

function G_TYPE_PARAM_FLAGS : GType;
begin
  G_TYPE_PARAM_FLAGS:=g_param_spec_types[11];
end;

function G_IS_PARAM_SPEC_FLAGS(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_FLAGS:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_FLAGS);
end;

function G_PARAM_SPEC_FLAGS(pspec : Pointer) : PGParamSpecFlags;
begin
  G_PARAM_SPEC_FLAGS:=PGParamSpecFlags(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_FLAGS));
end;

function G_TYPE_PARAM_FLOAT : GType;
begin
  G_TYPE_PARAM_FLOAT:=g_param_spec_types[12];
end;

function G_IS_PARAM_SPEC_FLOAT(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_FLOAT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_FLOAT);
end;

function G_PARAM_SPEC_FLOAT(pspec : Pointer) : PGParamSpecFloat;
begin
  G_PARAM_SPEC_FLOAT:=PGParamSpecFloat(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_FLOAT));
end;

function G_TYPE_PARAM_DOUBLE : GType;
begin
  G_TYPE_PARAM_DOUBLE:=g_param_spec_types[13];
end;

function G_IS_PARAM_SPEC_DOUBLE(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_DOUBLE:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_DOUBLE);
end;

function G_PARAM_SPEC_DOUBLE(pspec : Pointer) : PGParamSpecDouble;
begin
  G_PARAM_SPEC_DOUBLE:=PGParamSpecDouble(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_DOUBLE));
end;

function G_TYPE_PARAM_STRING : GType;
begin
  G_TYPE_PARAM_STRING:=g_param_spec_types[14];
end;

function G_IS_PARAM_SPEC_STRING(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_STRING:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_STRING);
end;

function G_PARAM_SPEC_STRING(pspec : Pointer) : PGParamSpecString;
begin
  G_PARAM_SPEC_STRING:=PGParamSpecString(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_STRING));
end;

function G_TYPE_PARAM_PARAM : GType;
begin
  G_TYPE_PARAM_PARAM:=g_param_spec_types[15];
end;

function G_IS_PARAM_SPEC_PARAM(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_PARAM:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_PARAM);
end;

function G_PARAM_SPEC_PARAM(pspec : Pointer) : PGParamSpecParam;
begin
  G_PARAM_SPEC_PARAM:=PGParamSpecParam(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_PARAM));
end;

function G_TYPE_PARAM_BOXED : GType;
begin
  G_TYPE_PARAM_BOXED:=g_param_spec_types[16];
end;

function G_IS_PARAM_SPEC_BOXED(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_BOXED:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_BOXED);
end;

function G_PARAM_SPEC_BOXED(pspec : Pointer) : PGParamSpecBoxed;
begin
  G_PARAM_SPEC_BOXED:=PGParamSpecBoxed(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_BOXED));
end;

function G_TYPE_PARAM_POINTER : GType;
begin
  G_TYPE_PARAM_POINTER:=g_param_spec_types[17];
end;

function G_IS_PARAM_SPEC_POINTER(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_POINTER:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_POINTER);
end;

function G_PARAM_SPEC_POINTER(pspec : Pointer) : PGParamSpecPointer;
begin
  G_PARAM_SPEC_POINTER:=PGParamSpecPointer(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_POINTER));
end;

function G_TYPE_PARAM_VALUE_ARRAY : GType;
begin
  G_TYPE_PARAM_VALUE_ARRAY:=g_param_spec_types[18];
end;

function G_IS_PARAM_SPEC_VALUE_ARRAY(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_VALUE_ARRAY:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_VALUE_ARRAY);
end;

function G_PARAM_SPEC_VALUE_ARRAY(pspec : Pointer) : PGParamSpecValueArray;
begin
  G_PARAM_SPEC_VALUE_ARRAY:=PGParamSpecValueArray(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_VALUE_ARRAY));
end;

function G_TYPE_PARAM_OBJECT : GType;
begin
  G_TYPE_PARAM_OBJECT:=g_param_spec_types[19];
end;

function G_IS_PARAM_SPEC_OBJECT(pspec : Pointer) : boolean;
begin
  G_IS_PARAM_SPEC_OBJECT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_OBJECT);
end;

function G_PARAM_SPEC_OBJECT(pspec : Pointer) : PGParamSpecObject;
begin
  G_PARAM_SPEC_OBJECT:=PGParamSpecObject(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_OBJECT));
end;

function null_fold_if_empty(var a : TGParamSpecString) : guint;
begin
  null_fold_if_empty:=(a.flag0 and bm_TGParamSpecString_null_fold_if_empty) shr bp_TGParamSpecString_null_fold_if_empty;
end;

procedure set_null_fold_if_empty(var a : TGParamSpecString; __null_fold_if_empty : guint);
begin
  a.flag0:=(a.flag0 and not bm_TGParamSpecString_null_fold_if_empty)
           or ((__null_fold_if_empty shl bp_TGParamSpecString_null_fold_if_empty) and bm_TGParamSpecString_null_fold_if_empty);
end;

function ensure_non_null(var a : TGParamSpecString) : guint;
begin
  ensure_non_null:=(a.flag0 and bm_TGParamSpecString_ensure_non_null) shr bp_TGParamSpecString_ensure_non_null;
end;

procedure set_ensure_non_null(var a : TGParamSpecString; __ensure_non_null : guint);
begin
  a.flag0:=(a.flag0 and not bm_TGParamSpecString_ensure_non_null)
           or ((__ensure_non_null shl bp_TGParamSpecString_ensure_non_null) and bm_TGParamSpecString_ensure_non_null);
end;
{$ENDIF read_implementation}

// included by glib2.pas
