// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkSelectionData = ^TGtkSelectionData;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ The contents of a selection are returned in a GtkSelectionData
   structure. selection/target identify the request.  type specifies
   the type of the return; if length < 0, and the data should be
   ignored. This structure has anObject semantics - no fields should be
   modified directly, they should not be created directly, and
   pointers to them should not be stored beyond the duration of a
   callback. (If the last is changed, we'll need to add reference
   counting.) The time field gives the timestamp at which the data was
   sent.
  }
   TGtkSelectionData = record
        selection : TGdkAtom;
        target : TGdkAtom;
        _type : TGdkAtom;
        format : gint;
        data : Pguchar;
        length : gint;
        display : PGdkDisplay;
     end;

   PGtkTargetEntry = ^TGtkTargetEntry;
   TGtkTargetEntry = record
        target : Pgchar;
        flags : guint;
        info : guint;
     end;

{ These structures not public, and are here only for the convenience of
   gtkdnd.c
  }
{ This structure is a list of destinations, and associated guint id's  }
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

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_SELECTION_DATA : GType;

function gtk_target_list_new(targets:PGtkTargetEntry; ntargets:guint):PGtkTargetList; cdecl; external gtklib;
procedure gtk_target_list_ref(list:PGtkTargetList); cdecl; external gtklib;
procedure gtk_target_list_unref(list:PGtkTargetList); cdecl; external gtklib;
procedure gtk_target_list_add(list:PGtkTargetList; target:TGdkAtom; flags:guint; info:guint); cdecl; external gtklib;
procedure gtk_target_list_add_table(list:PGtkTargetList; targets:PGtkTargetEntry; ntargets:guint); cdecl; external gtklib;
procedure gtk_target_list_remove(list:PGtkTargetList; target:TGdkAtom); cdecl; external gtklib;
function gtk_target_list_find(list:PGtkTargetList; target:TGdkAtom; info:Pguint):gboolean; cdecl; external gtklib;
{ Public interface  }
function gtk_selection_owner_set(widget:PGtkWidget; selection:TGdkAtom; time:guint32):gboolean; cdecl; external gtklib;
procedure gtk_selection_add_target(widget:PGtkWidget; selection:TGdkAtom; target:TGdkAtom; info:guint); cdecl; external gtklib;
procedure gtk_selection_add_targets(widget:PGtkWidget; selection:TGdkAtom; targets:PGtkTargetEntry; ntargets:guint); cdecl; external gtklib;
procedure gtk_selection_clear_targets(widget:PGtkWidget; selection:TGdkAtom); cdecl; external gtklib;
function gtk_selection_convert(widget:PGtkWidget; selection:TGdkAtom; target:TGdkAtom; time:guint32):gboolean; cdecl; external gtklib;
procedure gtk_selection_data_set(selection_data:PGtkSelectionData; _type:TGdkAtom; format:gint; data:Pguchar; length:gint); cdecl; external gtklib;
function gtk_selection_data_set_text(selection_data:PGtkSelectionData; str:Pgchar; len:gint):gboolean; cdecl; external gtklib;
function gtk_selection_data_get_text(selection_data:PGtkSelectionData):Pguchar; cdecl; external gtklib;
function gtk_selection_data_get_targets(selection_data:PGtkSelectionData; var targets:PGdkAtom; var n_atoms:gint):gboolean; cdecl; external gtklib;
function gtk_selection_data_targets_include_text(selection_data:PGtkSelectionData):gboolean; cdecl; external gtklib;
{ Called when a widget is destroyed  }
procedure gtk_selection_remove_all(widget:PGtkWidget); cdecl; external gtklib;
{ Event handlers  }
function gtk_selection_request(widget:PGtkWidget; event:PGdkEventSelection):gboolean; cdecl; external gtklib;
function gtk_selection_incr_event(window:PGdkWindow; event:PGdkEventProperty):gboolean; cdecl; external gtklib;
function gtk_selection_notify(widget:PGtkWidget; event:PGdkEventSelection):gboolean; cdecl; external gtklib;
function gtk_selection_property_notify(widget:PGtkWidget; event:PGdkEventProperty):gboolean; cdecl; external gtklib;
function gtk_selection_data_get_type:GType; cdecl; external gtklib;
function gtk_selection_data_copy(data:PGtkSelectionData):PGtkSelectionData; cdecl; external gtklib;
procedure gtk_selection_data_free(data:PGtkSelectionData); cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_selection_clear(widget:PGtkWidget; event:PGdkEventSelection):gboolean; cdecl; external gtklib;
function gtk_selection_owner_set_for_display(display:PGdkDisplay; widget:PGtkWidget; selection:TGdkAtom; time:guint32):gboolean; cdecl; external gtklib;
{$ENDIF}

{$ifdef HasGTK2_6}
function gtk_selection_data_get_pixbuf(selection_data:PGtkSelectionData):PGdkPixbuf; cdecl; external gtklib;
function gtk_selection_data_get_uris(selection_data:PGtkSelectionData):PPgchar; cdecl; external gtklib;
function gtk_selection_data_set_pixbuf(selection_data:PGtkSelectionData; pixbuf:PGdkPixbuf):gboolean; cdecl; external gtklib;
function gtk_selection_data_set_uris(selection_data:PGtkSelectionData; uris:PPgchar):gboolean; cdecl; external gtklib;
function gtk_selection_data_targets_include_image(selection_data:PGtkSelectionData; writable:gboolean):gboolean; cdecl; external gtklib;

procedure gtk_target_list_add_image_targets(list:PGtkTargetList; info:guint; writable:gboolean); cdecl; external gtklib;
procedure gtk_target_list_add_text_targets(list:PGtkTargetList; info:guint); cdecl; external gtklib;
procedure gtk_target_list_add_uri_targets(list:PGtkTargetList; info:guint); cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SELECTION_DATA : GType;
begin
   GTK_TYPE_SELECTION_DATA:=gtk_selection_data_get_type;
end;

{$ENDIF read_implementation}
// included by gtk2.pas

