{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}
  type
     PGtkFlagValue = pointer;

     TGtkFundamentalType = longint;
const
     GTK_TYPE_INVALID = 0;
     GTK_TYPE_NONE = 1;
     GTK_TYPE_CHAR = 2;
       GTK_TYPE_UCHAR = 3;
       GTK_TYPE_BOOL = 4;
       GTK_TYPE_INT = 5;
       GTK_TYPE_UINT = 6;
       GTK_TYPE_LONG = 7;
       GTK_TYPE_ULONG = 8;
       GTK_TYPE_FLOAT = 9;
       GTK_TYPE_DOUBLE = 10;
       GTK_TYPE_STRING = 11;
       GTK_TYPE_ENUM = 12;
       GTK_TYPE_FLAGS = 13;
       GTK_TYPE_BOXED = 14;
       GTK_TYPE_POINTER = 15;
       GTK_TYPE_SIGNAL = 16;
       GTK_TYPE_ARGS = 17;
       GTK_TYPE_CALLBACK = 18;
       GTK_TYPE_C_CALLBACK = 19;
       GTK_TYPE_FOREIGN = 20;
       GTK_TYPE_OBJECT = 21;

{********************************
  Types inserted in gtkobjects
********************************}

  const
     GTK_TYPE_FLAT_FIRST = GTK_TYPE_CHAR;
     GTK_TYPE_FLAT_LAST = GTK_TYPE_POINTER;
     GTK_TYPE_STRUCTURED_FIRST = GTK_TYPE_SIGNAL;
     GTK_TYPE_STRUCTURED_LAST = GTK_TYPE_FOREIGN;
     GTK_TYPE_FUNDAMENTAL_LAST = GTK_TYPE_OBJECT;
     GTK_TYPE_FUNDAMENTAL_MAX = 32;

function  GTK_CHECK_CLASS_TYPE(type_class:PGtkTypeClass;otype:TGtkType) : boolean;cdecl;

{$ifndef gtkwin}
function  GTK_TYPE_IDENTIFIER:TGtkType;cdecl;external gtkdll name 'gtk_identifier_get_type';
function  GTK_IDENTIFIER_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_identifier_get_type';
function  GTK_IS_IDENTIFIER(obj:pointer):boolean;
function  GTK_IS_IDENTIFIER_CLASS(klass:pointer):boolean;

function  gtk_identifier_get_type:TGtkType;cdecl;external gtkdll name 'gtk_identifier_get_type';
{$endif}

function  GTK_TYPE_MAKE(parent_t,seqno:TGtkType):TGtkType;
function  GTK_FUNDAMENTAL_TYPE(thetype : TGtkType) : TGtkFundamentalType;
function  GTK_TYPE_SEQNO(thetype : TGtkType) : TGtkType;

function  GTK_VALUE_CHAR(const a : TGtkArg) : gchar;
function  GTK_VALUE_UCHAR(const a : TGtkArg) : guchar;
function  GTK_VALUE_BOOL(const a : TGtkArg) : gboolean;
function  GTK_VALUE_INT(const a : TGtkArg) : gint;
function  GTK_VALUE_UINT(const a : TGtkArg) : guint;
function  GTK_VALUE_LONG(const a : TGtkArg) : glong;
function  GTK_VALUE_ULONG(const a : TGtkArg) : gulong;
function  GTK_VALUE_FLOAT(const a : TGtkArg) : gfloat;
function  GTK_VALUE_DOUBLE(const a : TGtkArg) : gdouble;
function  GTK_VALUE_ENUM(const a : TGtkArg) : longint;
function  GTK_VALUE_FLAGS(const a : TGtkArg) : longint;
function  GTK_VALUE_BOXED(const a : TGtkArg) : gpointer;
function  GTK_VALUE_POINTER(const a : TGtkArg) : gpointer;
function  GTK_VALUE_OBJECT(const a : TGtkArg): gpointer;
{function GTK_VALUE_SIGNAL(const a : TGtkArg) : gpointer;
function  GTK_VALUE_ARGS(const a : TGtkArg) : longint;
function  GTK_VALUE_CALLBACK(const a : TGtkArg) : gpointer;
function  GTK_VALUE_C_CALLBACK(const a : TGtkArg) : gpointer;
function  GTK_VALUE_FOREIGN(const a : TGtkArg) : gpointer;}

function  GTK_RETLOC_CHAR(const a : TGtkArg) : Pgchar;
function  GTK_RETLOC_UCHAR(const a : TGtkArg) : Pguchar;
function  GTK_RETLOC_BOOL(const a : TGtkArg) : Pgboolean;
function  GTK_RETLOC_INT(const a : TGtkArg) : Pgint;
function  GTK_RETLOC_UINT(const a : TGtkArg) : Pguint;
function  GTK_RETLOC_LONG(const a : TGtkArg) : Pglong;
function  GTK_RETLOC_ULONG(const a : TGtkArg) : Pgulong;
function  GTK_RETLOC_FLOAT(const a : TGtkArg) : Pgfloat;
function  GTK_RETLOC_DOUBLE(const a : TGtkArg) : Pgdouble;
function  GTK_RETLOC_ENUM(const a : TGtkArg) : Pgint;
function  GTK_RETLOC_FLAGS(const a : TGtkArg) : Pguint;
function  GTK_RETLOC_BOXED(const a : TGtkArg) : Pgpointer;

    type
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

       PGtkTypeQuery = ^TGtkTypeQuery;
       TGtkTypeQuery = record
            thetype : TGtkType;
            type_name : Pgchar;
            object_size : guint;
            class_size : guint;
         end;

       PGtkEnumValue = ^TGtkEnumValue;
       TGtkEnumValue = record
            value : guint;
            value_name : Pgchar;
            value_nick : Pgchar;
         end;


procedure gtk_type_init;cdecl;external gtkdll name 'gtk_type_init';
function  gtk_type_unique(parent_thetype:TGtkType; type_info:PGtkTypeInfo):TGtkType;cdecl;external gtkdll name 'gtk_type_unique';
procedure gtk_type_set_chunk_alloc(thetype:TGtkType; n_chunks:guint);cdecl;external gtkdll name 'gtk_type_set_chunk_alloc';
function  gtk_type_name(thetype:guint):Pgchar;cdecl;external gtkdll name 'gtk_type_name';
function  gtk_type_from_name(name:Pgchar):TGtkType;cdecl;external gtkdll name 'gtk_type_from_name';
function  gtk_type_parent(thetype:TGtkType):TGtkType;cdecl;external gtkdll name 'gtk_type_parent';
function  gtk_type_class(thetype:TGtkType):gpointer;cdecl;external gtkdll name 'gtk_type_class';
function  gtk_type_parent_class(thetype:TGtkType):gpointer;cdecl;external gtkdll name 'gtk_type_parent_class';
function  gtk_type_children_types(thetype:TGtkType):PGList;cdecl;external gtkdll name 'gtk_type_children_types';
function  gtk_type_new(thetype:TGtkType):gpointer;cdecl;external gtkdll name 'gtk_type_new';
procedure gtk_type_free(thetype:TGtkType; mem:gpointer);cdecl;external gtkdll name 'gtk_type_free';
procedure gtk_type_describe_heritage(thetype:TGtkType);cdecl;external gtkdll name 'gtk_type_describe_heritage';
procedure gtk_type_describe_tree(thetype:TGtkType; show_size:gboolean);cdecl;external gtkdll name 'gtk_type_describe_tree';
function  gtk_type_is_a(thetype:TGtkType; is_a_thetype:TGtkType):gboolean;cdecl;external gtkdll name 'gtk_type_is_a';
function  gtk_type_check_object_cast(type_object:PGtkTypeObject; cast_thetype:TGtkType):PGtkTypeObject;cdecl;external gtkdll name 'gtk_type_check_object_cast';
function  gtk_type_check_class_cast(klass:PGtkTypeClass; cast_thetype:TGtkType):PGtkTypeClass;cdecl;external gtkdll name 'gtk_type_check_class_cast';
function  gtk_type_register_enum(type_name:Pgchar; values:PGtkEnumValue):TGtkType;cdecl;external gtkdll name 'gtk_type_register_enum';
function  gtk_type_register_flags(type_name:Pgchar; values:PGtkFlagValue):TGtkType;cdecl;external gtkdll name 'gtk_type_register_flags';
function  gtk_type_enum_get_values(enum_thetype:TGtkType):PGtkEnumValue;cdecl;external gtkdll name 'gtk_type_enum_get_values';
function  gtk_type_flags_get_values(flags_thetype:TGtkType):PGtkFlagValue;cdecl;external gtkdll name 'gtk_type_flags_get_values';
function  gtk_type_enum_find_value(enum_thetype:TGtkType; value_name:Pgchar):PGtkEnumValue;cdecl;external gtkdll name 'gtk_type_enum_find_value';
function  gtk_type_flags_find_value(flag_thetype:TGtkType; value_name:Pgchar):PGtkFlagValue;cdecl;external gtkdll name 'gtk_type_flags_find_value';
procedure gtk_type_set_varargs_type(foreign_thetype:TGtkType; varargs_thetype:TGtkType);cdecl;external gtkdll name 'gtk_type_set_varargs_type';
function  gtk_type_get_varargs_type(foreign_thetype:TGtkType):TGtkType;cdecl;external gtkdll name 'gtk_type_get_varargs_type';
function  gtk_type_query(thetype:TGtkType):PGtkTypeQuery;cdecl;external gtkdll name 'gtk_type_query';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_CHECK_CLASS_TYPE(type_class:PGtkTypeClass;otype:TGtkType) : boolean;cdecl;
begin
  GTK_CHECK_CLASS_TYPE:=(type_class<>nil) and (gtk_type_is_a(type_class^.thetype,otype));
end;

{$ifndef gtkwin}
function  GTK_IS_IDENTIFIER(obj:pointer):boolean;
begin
  GTK_IS_IDENTIFIER:=(obj<>nil) and GTK_IS_IDENTIFIER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_IDENTIFIER_CLASS(klass:pointer):boolean;
begin
  GTK_IS_IDENTIFIER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_IDENTIFIER_TYPE);
end;
{$endif}

function  GTK_TYPE_MAKE(parent_t,seqno:TGtkType):TGtkType;
begin
  GTK_TYPE_MAKE:=(seqno shl 8) or (longint(GTK_FUNDAMENTAL_TYPE(parent_t)));
end;

function  GTK_FUNDAMENTAL_TYPE(thetype : TGtkType) : TGtkFundamentalType;
begin
  GTK_FUNDAMENTAL_TYPE:=TGtkFundamentalType(longint(thetype) and $FF);
end;

function  GTK_TYPE_SEQNO(thetype : TGtkType) : TGtkType;
begin
  if longint(thetype)>$FF then
    GTK_TYPE_SEQNO:=longint(thetype) shr 8
  else
    GTK_TYPE_SEQNO:=thetype;
end;

function  GTK_VALUE_CHAR(const a : TGtkArg) : gchar;
begin
  GTK_VALUE_CHAR:=a.d.char_data;
end;

function  GTK_VALUE_UCHAR(const a : TGtkArg) : guchar;
begin
  GTK_VALUE_UCHAR:=a.d.uchar_data;
end;

function  GTK_VALUE_BOOL(const a : TGtkArg) : gboolean;
begin
  GTK_VALUE_BOOL:=a.d.bool_data;
end;

function  GTK_VALUE_INT(const a : TGtkArg) : gint;
begin
  GTK_VALUE_INT:=a.d.int_data;
end;

function  GTK_VALUE_UINT(const a : TGtkArg) : guint;
begin
  GTK_VALUE_UINT:=a.d.uint_data;
end;

function  GTK_VALUE_LONG(const a : TGtkArg) : glong;
begin
  GTK_VALUE_LONG:=a.d.long_data;
end;

function  GTK_VALUE_ULONG(const a : TGtkArg) : gulong;
begin
  GTK_VALUE_ULONG:=a.d.ulong_data;
end;

function  GTK_VALUE_FLOAT(const a : TGtkArg) : gfloat;
begin
  GTK_VALUE_FLOAT:=a.d.float_data;
end;

function  GTK_VALUE_DOUBLE(const a : TGtkArg) : gdouble;
begin
  GTK_VALUE_DOUBLE:=a.d.double_data;
end;

function  GTK_VALUE_ENUM(const a : TGtkArg) : longint;
begin
  GTK_VALUE_ENUM:=a.d.int_data;
end;

function  GTK_VALUE_FLAGS(const a : TGtkArg) : longint;
begin
  GTK_VALUE_FLAGS:=a.d.uint_data;
end;

function  GTK_VALUE_BOXED(const a : TGtkArg) : gpointer;
begin
  GTK_VALUE_BOXED:=a.d.pointer_data;
end;

function  GTK_VALUE_POINTER(const a : TGtkArg) : gpointer;
begin
  GTK_VALUE_POINTER:=a.d.pointer_data;
end;

function  GTK_VALUE_OBJECT(const a : TGtkArg): gpointer;
begin
  GTK_VALUE_OBJECT:=a.d.object_data;
end;

{function GTK_VALUE_SIGNAL(const a : TGtkArg) : gpointer;
begin
  GTK_VALUE_SIGNAL:=a.d.signal_data;
end;

function  GTK_VALUE_ARGS(const a : TGtkArg) : longint;
begin
  GTK_VALUE_ARGS:=a.d.args_data;
end;

function  GTK_VALUE_CALLBACK(const a : TGtkArg) : gpointer;
begin
  GTK_VALUE_CALLBACK:=a.d.callback_data;
end;

function  GTK_VALUE_C_CALLBACK(const a : TGtkArg) : gpointer;
begin
  GTK_VALUE_C_CALLBACK:=a.d.c_callback_data;
end;

function  GTK_VALUE_FOREIGN(const a : TGtkArg) : gpointer;
begin
  GTK_VALUE_FOREIGN:=a.d.foreign_data;
end;}

function  GTK_RETLOC_CHAR(const a : TGtkArg) : Pgchar;
      begin
         GTK_RETLOC_CHAR:=Pgchar(a.d.pointer_data);
      end;

function  GTK_RETLOC_UCHAR(const a : TGtkArg) : Pguchar;
      begin
         GTK_RETLOC_UCHAR:=Pguchar(a.d.pointer_data);
      end;

function  GTK_RETLOC_BOOL(const a : TGtkArg) : Pgboolean;
      begin
         GTK_RETLOC_BOOL:=Pgboolean(a.d.pointer_data);
      end;

function  GTK_RETLOC_INT(const a : TGtkArg) : Pgint;
      begin
         GTK_RETLOC_INT:=Pgint(a.d.pointer_data);
      end;

function  GTK_RETLOC_UINT(const a : TGtkArg) : Pguint;
      begin
         GTK_RETLOC_UINT:=Pguint(a.d.pointer_data);
      end;

function  GTK_RETLOC_LONG(const a : TGtkArg) : Pglong;
      begin
         GTK_RETLOC_LONG:=Pglong(a.d.pointer_data);
      end;

function  GTK_RETLOC_ULONG(const a : TGtkArg) : Pgulong;
      begin
         GTK_RETLOC_ULONG:=Pgulong(a.d.pointer_data);
      end;

function  GTK_RETLOC_FLOAT(const a : TGtkArg) : Pgfloat;
      begin
         GTK_RETLOC_FLOAT:=Pgfloat(a.d.pointer_data);
      end;

function  GTK_RETLOC_DOUBLE(const a : TGtkArg) : Pgdouble;
      begin
         GTK_RETLOC_DOUBLE:=Pgdouble(a.d.pointer_data);
      end;

function  GTK_RETLOC_ENUM(const a : TGtkArg) : Pgint;
      begin
         GTK_RETLOC_ENUM:=Pgint(a.d.pointer_data);
      end;

function  GTK_RETLOC_FLAGS(const a : TGtkArg) : Pguint;
      begin
         GTK_RETLOC_FLAGS:=Pguint(a.d.pointer_data);
      end;

function  GTK_RETLOC_BOXED(const a : TGtkArg) : Pgpointer;
      begin
         GTK_RETLOC_BOXED:=Pgpointer(a.d.pointer_data);
      end;

function  GTK_RETLOC_POINTER(const a : TGtkArg) : Pgpointer;
      begin
         GTK_RETLOC_POINTER:=Pgpointer(a.d.pointer_data);
      end;

{$endif read_implementation}


