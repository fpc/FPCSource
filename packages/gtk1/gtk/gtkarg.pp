{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

{********************************
  Types inserted in gtkobjects
********************************}

{$ifndef gtkwin}
function  gtk_arg_new(arg_type:TGtkType):PGtkArg;cdecl;external gtkdll name 'gtk_arg_new';
function  gtk_arg_copy(src_arg:PGtkArg; dest_arg:PGtkArg):PGtkArg;cdecl;external gtkdll name 'gtk_arg_copy';
procedure gtk_arg_free(arg:PGtkArg; free_contents:gboolean);cdecl;external gtkdll name 'gtk_arg_free';
procedure gtk_arg_reset(arg:PGtkArg);cdecl;external gtkdll name 'gtk_arg_reset';
function  gtk_arg_values_equal(arg1:PGtkArg; arg2:PGtkArg):gboolean;cdecl;external gtkdll name 'gtk_arg_values_equal';
function  gtk_args_collect(object_type:TGtkType; arg_info_hash_table:PGHashTable; arg_list_p:PPGSList; info_list_p:PPGSList; first_arg_name:Pgchar; var_args:array of const):Pgchar;cdecl;external gtkdll name 'gtk_args_collect';
procedure gtk_args_collect_cleanup(arg_list:PGSList; info_list:PGSList);cdecl;external gtkdll name 'gtk_args_collect_cleanup';
function  gtk_arg_get_info(object_type:TGtkType; arg_info_hash_table:PGHashTable; arg_name:Pgchar; info_p:PPGtkArgInfo):Pgchar;cdecl;external gtkdll name 'gtk_arg_get_info';
function  gtk_arg_type_new_static(base_class_type:TGtkType; arg_name:Pgchar; class_n_args_offset:guint; arg_info_hash_table:PGHashTable; arg_type:TGtkType; arg_flags:guint; arg_id:guint):PGtkArgInfo;cdecl;external gtkdll name 'gtk_arg_type_new_static';
function  gtk_args_query(class_type:TGtkType; arg_info_hash_table:PGHashTable; arg_flags:PPguint32; n_args_p:Pguint):PGtkArg;cdecl;external gtkdll name 'gtk_args_query';
function  gtk_arg_name_strip_type(arg_name:Pgchar):Pgchar;cdecl;external gtkdll name 'gtk_arg_name_strip_type';
function  gtk_arg_info_equal(arg_info_1:gconstpointer; arg_info_2:gconstpointer):gint;cdecl;external gtkdll name 'gtk_arg_info_equal';
function  gtk_arg_info_hash(arg_info:gconstpointer):guint;cdecl;external gtkdll name 'gtk_arg_info_hash';
procedure gtk_arg_to_valueloc(arg:PGtkArg; value_pointer:gpointer);cdecl;external gtkdll name 'gtk_arg_to_valueloc';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


