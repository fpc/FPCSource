{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

function  gtk_gc_get (depth:gint; colormap:PGdkColormap; values:PGdkGCValues; values_mask:TGdkGCValuesMask): PGdkGC;cdecl;external gtkdll name 'gtk_gc_get';
procedure gtk_gc_release(gc:PGdkGC);cdecl;external gtkdll name 'gtk_gc_release';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.8  1999/10/21 08:42:01  florian
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

  Revision 1.7  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.6  1999/05/11 00:38:37  peter
    * win32 fixes

  Revision 1.5  1999/05/10 15:19:27  peter
    * cdecl fixes

  Revision 1.4  1999/05/10 09:03:14  peter
    * gtk 1.2 port working

  Revision 1.3  1998/10/21 20:22:32  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

