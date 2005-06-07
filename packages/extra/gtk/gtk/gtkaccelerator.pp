{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkAcceleratorTable = ^TGtkAcceleratorTable;
       TGtkAcceleratorTable = record
            entries : array[0..255] of PGList;
            ref_count : guint;
            modifier_mask : guint8;
         end;

{$ifndef win32}
function  gtk_accelerator_table_new : PGtkAcceleratorTable;cdecl;external gtkdll name 'gtk_accelerator_table_new';
function  gtk_accelerator_table_find (obj:PGtkObject; signal_name:Pgchar; accelerator_key:guchar; accelerator_mods:guint8): PGtkAcceleratorTable;cdecl;external gtkdll name 'gtk_accelerator_table_find';
function  gtk_accelerator_table_ref (table:PGtkAcceleratorTable):PGtkAcceleratorTable;cdecl;external gtkdll name 'gtk_accelerator_table_ref';
procedure gtk_accelerator_table_unref(table:PGtkAcceleratorTable);cdecl;external gtkdll name 'gtk_accelerator_table_unref';
procedure gtk_accelerator_table_install(table:PGtkAcceleratorTable; obj:PGtkObject; signal_name:Pgchar; accelerator_key:guchar; accelerator_mods:guint8);cdecl;external gtkdll name 'gtk_accelerator_table_install';
procedure gtk_accelerator_table_remove(table:PGtkAcceleratorTable; obj:PGtkObject; signal_name:Pgchar);cdecl;external gtkdll name 'gtk_accelerator_table_remove';
function  gtk_accelerator_table_check(table:PGtkAcceleratorTable; accelerator_key:guchar; accelerator_mods:guint8):gint;cdecl;external gtkdll name 'gtk_accelerator_table_check';
procedure gtk_accelerator_tables_delete(obj:PGtkObject);cdecl;external gtkdll name 'gtk_accelerator_tables_delete';
procedure gtk_accelerator_table_set_mod_mask(table:PGtkAcceleratorTable; modifier_mask:guint8);cdecl;external gtkdll name 'gtk_accelerator_table_set_mod_mask';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}

