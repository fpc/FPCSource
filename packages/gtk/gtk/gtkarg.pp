{
   $Id$
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


{
  $Log$
  Revision 1.1  2000-07-13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.6  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.5  1999/07/23 16:11:53  peter
    * use packrecords C

  Revision 1.4  1999/05/11 00:38:05  peter
    * win32 fixes

  Revision 1.3  1999/05/10 15:18:52  peter
    * cdecl fixes

  Revision 1.2  1999/05/10 09:02:55  peter
    * gtk 1.2 port working

  Revision 1.1  1999/05/07 10:40:25  peter
    * first things for 1.2

}

