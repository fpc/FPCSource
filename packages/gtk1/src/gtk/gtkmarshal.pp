{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

{$ifdef gtkdarwin}
{$define gtkwin}
{$endif gtkdarwin}

procedure gtk_marshal_BOOL__NONE(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__NONE';
procedure gtk_marshal_BOOL__POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER';
procedure gtk_marshal_BOOL__POINTER_POINTER_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER_POINTER_INT_INT';
procedure gtk_marshal_BOOL__POINTER_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER_INT_INT';
{$ifndef gtkwin}
procedure gtk_marshal_BOOL__POINTER_INT_INT_UINT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER_INT_INT_UINT';
{$endif}
procedure gtk_marshal_BOOL__POINTER_INT_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER_INT_INT_INT';
{$ifndef gtkwin}
procedure gtk_marshal_BOOL__POINTER_STRING_STRING_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER_STRING_STRING_POINTER';
{$endif}
procedure gtk_marshal_BOOL__POINTER_POINTER_POINTER_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_BOOL__POINTER_POINTER_POINTER_POINTER';
{$ifndef gtkwin}
procedure gtk_marshal_ENUM__ENUM(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_ENUM__ENUM';
{$endif}
procedure gtk_marshal_INT__INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_INT__INT';
procedure gtk_marshal_INT__POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_INT__POINTER';
procedure gtk_marshal_INT__POINTER_CHAR_CHAR(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_INT__POINTER_CHAR_CHAR';
procedure gtk_marshal_NONE__BOOL(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__BOOL';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__BOXED(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__BOXED';
{$endif}
procedure gtk_marshal_NONE__POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER';
procedure gtk_marshal_NONE__C_CALLBACK(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__C_CALLBACK';
procedure gtk_marshal_NONE__C_CALLBACK_C_CALLBACK(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__C_CALLBACK_C_CALLBACK';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__ENUM(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__ENUM';
{$endif}
procedure gtk_marshal_NONE__INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__ENUM_FLOAT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__ENUM_FLOAT';
{$endif}
procedure gtk_marshal_NONE__INT_FLOAT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_FLOAT';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__ENUM_FLOAT_BOOL(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__ENUM_FLOAT_BOOL';
{$endif}
procedure gtk_marshal_NONE__INT_FLOAT_BOOL(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_FLOAT_BOOL';
procedure gtk_marshal_NONE__INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_INT';
procedure gtk_marshal_NONE__INT_INT_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_INT_POINTER';
procedure gtk_marshal_NONE__NONE(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__NONE';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__OBJECT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__OBJECT';
{$endif}
procedure gtk_marshal_NONE__POINTER_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_INT';
procedure gtk_marshal_NONE__POINTER_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_POINTER';
procedure gtk_marshal_NONE__POINTER_POINTER_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_POINTER_POINTER';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__POINTER_STRING_STRING(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_STRING_STRING';
procedure gtk_marshal_NONE__POINTER_UINT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_UINT';
procedure gtk_marshal_NONE__POINTER_UINT_ENUM(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_UINT_ENUM';
{$endif}
procedure gtk_marshal_NONE__POINTER_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_INT_INT';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__POINTER_POINTER_UINT_UINT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_POINTER_UINT_UINT';
{$endif}
procedure gtk_marshal_NONE__POINTER_POINTER_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_POINTER_INT_INT';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__POINTER_INT_INT_POINTER_UINT_UINT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_INT_INT_POINTER_UINT_UINT';
{$endif}
procedure gtk_marshal_NONE__POINTER_INT_INT_POINTER_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_INT_INT_POINTER_INT_INT';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__POINTER_UINT_UINT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_UINT_UINT';
procedure gtk_marshal_NONE__STRING(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__STRING';
procedure gtk_marshal_NONE__STRING_INT_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__STRING_INT_POINTER';
{$endif}
procedure gtk_marshal_NONE__POINTER_INT_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__POINTER_INT_POINTER';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__UINT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__UINT';
procedure gtk_marshal_NONE__UINT_POINTER_UINT_ENUM_ENUM_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__UINT_POINTER_UINT_ENUM_ENUM_POINTER';
{$endif}
procedure gtk_marshal_NONE__INT_POINTER_INT_INT_INT_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_POINTER_INT_INT_INT_POINTER';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__UINT_POINTER_UINT_UINT_ENUM(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__UINT_POINTER_UINT_UINT_ENUM';
{$endif}
procedure gtk_marshal_NONE__INT_POINTER_INT_INT_INT(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_POINTER_INT_INT_INT';
{$ifndef gtkwin}
procedure gtk_marshal_NONE__UINT_STRING(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__UINT_STRING';
{$endif}
procedure gtk_marshal_NONE__INT_POINTER(theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_marshal_NONE__INT_POINTER';

{$ifdef gtkdarwin}
{$undef gtkwin}
{$endif gtkdarwin}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


