{
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

function  gtk_target_list_new(targets:PGtkTargetEntry; ntargets:guint):PGtkTargetList;cdecl;external gtkdll name 'gtk_target_list_new';
procedure gtk_target_list_unref(list:PGtkTargetList);cdecl;external gtkdll name 'gtk_target_list_unref';
{$ifndef gtkwin}
procedure gtk_target_list_ref(list:PGtkTargetList);cdecl;external gtkdll name 'gtk_target_list_ref';
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


