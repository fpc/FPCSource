{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkTargetEntry = ^TGtkTargetEntry;
     TGtkTargetEntry = record
          target : Pgchar;
          flags : guint;
          info : guint;
       end;

     PGtkTargetList = ^TGtkTargetList;
     TGtkTargetList = record
          list : PGList;
          ref_count : guint;
       end;

     PGtkTargetPair = ^TGtkTargetPair;
     TGtkTargetPair = record
          target : TGdkAtom;
          flags : guint;
          info : guint;
       end;

{$ifndef gtkwin}
function  gtk_target_list_new(targets:PGtkTargetEntry; ntargets:guint):PGtkTargetList;cdecl;external gtkdll name 'gtk_target_list_new';
procedure gtk_target_list_ref(list:PGtkTargetList);cdecl;external gtkdll name 'gtk_target_list_ref';
procedure gtk_target_list_unref(list:PGtkTargetList);cdecl;external gtkdll name 'gtk_target_list_unref';
procedure gtk_target_list_add(list:PGtkTargetList; target:TGdkAtom; flags:guint; info:guint);cdecl;external gtkdll name 'gtk_target_list_add';
procedure gtk_target_list_add_table(list:PGtkTargetList; targets:PGtkTargetEntry; ntargets:guint);cdecl;external gtkdll name 'gtk_target_list_add_table';
procedure gtk_target_list_remove(list:PGtkTargetList; target:TGdkAtom);cdecl;external gtkdll name 'gtk_target_list_remove';
function  gtk_target_list_find(list:PGtkTargetList; target:TGdkAtom; info:Pguint):gboolean;cdecl;external gtkdll name 'gtk_target_list_find';
{$endif}
function  gtk_selection_owner_set(widget:PGtkWidget; selection:TGdkAtom; time:guint32):gint;cdecl;external gtkdll name 'gtk_selection_owner_set';
procedure gtk_selection_add_target(widget:PGtkWidget; selection:TGdkAtom; target:TGdkAtom; info:guint);cdecl;external gtkdll name 'gtk_selection_add_target';
procedure gtk_selection_add_targets(widget:PGtkWidget; selection:TGdkAtom; targets:PGtkTargetEntry; ntargets:guint);cdecl;external gtkdll name 'gtk_selection_add_targets';
function  gtk_selection_convert(widget:PGtkWidget; selection:TGdkAtom; target:TGdkAtom; time:guint32):gint;cdecl;external gtkdll name 'gtk_selection_convert';
procedure gtk_selection_data_set(selection_data:PGtkSelectionData; thetype:TGdkAtom; format:gint; data:Pguchar; length:gint);cdecl;external gtkdll name 'gtk_selection_data_set';
procedure gtk_selection_remove_all(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_selection_remove_all';
function  gtk_selection_clear(widget:PGtkWidget; event:PGdkEventSelection):gint;cdecl;external gtkdll name 'gtk_selection_clear';
function  gtk_selection_request(widget:PGtkWidget; event:PGdkEventSelection):gint;cdecl;external gtkdll name 'gtk_selection_request';
function  gtk_selection_incr_event(window:PGdkWindow; event:PGdkEventProperty):gint;cdecl;external gtkdll name 'gtk_selection_incr_event';
function  gtk_selection_notify(widget:PGtkWidget; event:PGdkEventSelection):gint;cdecl;external gtkdll name 'gtk_selection_notify';
function  gtk_selection_property_notify(widget:PGtkWidget; event:PGdkEventProperty):gint;cdecl;external gtkdll name 'gtk_selection_property_notify';
function  gtk_selection_data_copy(data:PGtkSelectionData):PGtkSelectionData;cdecl;external gtkdll name 'gtk_selection_data_copy';
procedure gtk_selection_data_free(data:PGtkSelectionData);cdecl;external gtkdll name 'gtk_selection_data_free';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.11  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.10  1999/07/23 16:13:05  peter
    * use packrecords C

  Revision 1.9  1999/05/11 00:39:22  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:20:21  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:03:52  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 17:40:33  peter
    * more updates

  Revision 1.5  1999/05/07 15:10:12  peter
    * more fixes

  Revision 1.4  1998/11/12 11:35:52  peter
    + array of const

  Revision 1.3  1998/10/21 20:23:10  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

