{
   $Id$
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

{
  $Log$
  Revision 1.1  2000-07-13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.10  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.9  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/05/11 00:38:00  peter
    * win32 fixes

  Revision 1.7  1999/05/10 19:18:26  peter
    * more fixes for the examples to work

  Revision 1.6  1999/05/10 15:18:47  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:09:49  peter
    * more fixes

  Revision 1.4  1998/10/21 22:25:14  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:22:04  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

