{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkDestDefaults = (GTK_DEST_DEFAULT_MOTION := 1 shl 0,GTK_DEST_DEFAULT_HIGHLIGHT := 1 shl 1,
       GTK_DEST_DEFAULT_DROP := 1 shl 2,GTK_DEST_DEFAULT_ALL := $07
       );

     TGtkTargetFlags = (GTK_TARGET_SAME_APP := 1 shl 0,GTK_TARGET_SAME_WIDGET := 1 shl 1
       );

procedure gtk_drag_get_data(widget:PGtkWidget; context:PGdkDragContext; target:TGdkAtom; time:guint32);cdecl;external gtkdll name 'gtk_drag_get_data';
procedure gtk_drag_finish(context:PGdkDragContext; success:gboolean; del:gboolean; time:guint32);cdecl;external gtkdll name 'gtk_drag_finish';
function  gtk_drag_get_source_widget(context:PGdkDragContext):PGtkWidget;cdecl;external gtkdll name 'gtk_drag_get_source_widget';
procedure gtk_drag_highlight(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_drag_highlight';
procedure gtk_drag_unhighlight(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_drag_unhighlight';
procedure gtk_drag_dest_set(widget:PGtkWidget; flags:TGtkDestDefaults; targets:PGtkTargetEntry; n_targets:gint; actions:TGdkDragAction);cdecl;external gtkdll name 'gtk_drag_dest_set';
procedure gtk_drag_dest_set_proxy(widget:PGtkWidget; proxy_window:PGdkWindow; protocol:TGdkDragProtocol; use_coordinates:gboolean);cdecl;external gtkdll name 'gtk_drag_dest_set_proxy';
procedure gtk_drag_dest_unset(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_drag_dest_unset';
procedure gtk_drag_source_set(widget:PGtkWidget; start_button_mask:TGdkModifierType; targets:PGtkTargetEntry; n_targets:gint; actions:TGdkDragAction);cdecl;external gtkdll name 'gtk_drag_source_set';
procedure gtk_drag_source_unset(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_drag_source_unset';
procedure gtk_drag_source_set_icon(widget:PGtkWidget; colormap:PGdkColormap; pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_drag_source_set_icon';
function  gtk_drag_begin(widget:PGtkWidget; targets:PGtkTargetList; actions:TGdkDragAction; button:gint; event:PGdkEvent):PGdkDragContext;cdecl;external gtkdll name 'gtk_drag_begin';
procedure gtk_drag_set_icon_widget(context:PGdkDragContext; widget:PGtkWidget; hot_x:gint; hot_y:gint);cdecl;external gtkdll name 'gtk_drag_set_icon_widget';
procedure gtk_drag_set_icon_pixmap(context:PGdkDragContext; colormap:PGdkColormap; pixmap:PGdkPixmap; mask:PGdkBitmap; hot_x:gint; hot_y:gint);cdecl;external gtkdll name 'gtk_drag_set_icon_pixmap';
procedure gtk_drag_set_icon_default(context:PGdkDragContext);cdecl;external gtkdll name 'gtk_drag_set_icon_default';
procedure gtk_drag_set_default_icon(colormap:PGdkColormap; pixmap:PGdkPixmap; mask:PGdkBitmap; hot_x:gint; hot_y:gint);cdecl;external gtkdll name 'gtk_drag_set_default_icon';
procedure gtk_drag_source_handle_event(widget:PGtkWidget; event:PGdkEvent);cdecl;external gtkdll name 'gtk_drag_source_handle_event';
procedure gtk_drag_dest_handle_event(toplevel:PGtkWidget; event:PGdkEvent);cdecl;external gtkdll name 'gtk_drag_dest_handle_event';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


