// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ respond to "drag_motion"  }
{ auto-highlight  }
{ respond to "drag_drop"  }
   PGtkDestDefaults = ^TGtkDestDefaults;
   TGtkDestDefaults = longint;
{ Flags for the GtkTargetEntry on the destination side
  }
{< nick=same-app > }
{< nick=same-widget > }

   PGtkTargetFlags = ^TGtkTargetFlags;
   TGtkTargetFlags = longint;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_DEST_DEFAULT_MOTION = 1 shl 0;
   GTK_DEST_DEFAULT_HIGHLIGHT = 1 shl 1;
   GTK_DEST_DEFAULT_DROP = 1 shl 2;
   GTK_DEST_DEFAULT_ALL = $07;

   GTK_TARGET_SAME_APP = 1 shl 0;
   GTK_TARGET_SAME_WIDGET = 1 shl 1;

{ Destination side  }

procedure gtk_drag_get_data(widget:PGtkWidget; context:PGdkDragContext; target:TGdkAtom; time:guint32); cdecl; external gtklib;
procedure gtk_drag_finish(context:PGdkDragContext; success:gboolean; del:gboolean; time:guint32); cdecl; external gtklib;
function gtk_drag_get_source_widget(context:PGdkDragContext):PGtkWidget; cdecl; external gtklib;
procedure gtk_drag_highlight(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_drag_unhighlight(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_drag_dest_set(widget:PGtkWidget; flags:TGtkDestDefaults; targets:PGtkTargetEntry; n_targets:gint; actions:TGdkDragAction); cdecl; external gtklib;
procedure gtk_drag_dest_set_proxy(widget:PGtkWidget; proxy_window:PGdkWindow; protocol:TGdkDragProtocol; use_coordinates:gboolean); cdecl; external gtklib;
procedure gtk_drag_dest_unset(widget:PGtkWidget); cdecl; external gtklib;
function gtk_drag_dest_find_target(widget:PGtkWidget; context:PGdkDragContext; target_list:PGtkTargetList):TGdkAtom; cdecl; external gtklib;
function gtk_drag_dest_get_target_list(widget:PGtkWidget):PGtkTargetList; cdecl; external gtklib;
procedure gtk_drag_dest_set_target_list(widget:PGtkWidget; target_list:PGtkTargetList); cdecl; external gtklib;
{ Source side  }
procedure gtk_drag_source_set(widget:PGtkWidget; start_button_mask:TGdkModifierType; targets:PGtkTargetEntry; n_targets:gint; actions:TGdkDragAction); cdecl; external gtklib;
procedure gtk_drag_source_unset(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_drag_source_set_icon(widget:PGtkWidget; colormap:PGdkColormap; pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_drag_source_set_icon_pixbuf(widget:PGtkWidget; pixbuf:PGdkPixbuf); cdecl; external gtklib;
procedure gtk_drag_source_set_icon_stock(widget:PGtkWidget; stock_id:Pgchar); cdecl; external gtklib;
{ There probably should be functions for setting the targets
   as a GtkTargetList
  }
function gtk_drag_begin(widget:PGtkWidget; targets:PGtkTargetList; actions:TGdkDragAction; button:gint; event:PGdkEvent):PGdkDragContext; cdecl; external gtklib;
{ Set the image being dragged around
  }
procedure gtk_drag_set_icon_widget(context:PGdkDragContext; widget:PGtkWidget; hot_x:gint; hot_y:gint); cdecl; external gtklib;
procedure gtk_drag_set_icon_pixmap(context:PGdkDragContext; colormap:PGdkColormap; pixmap:PGdkPixmap; mask:PGdkBitmap; hot_x:gint;
            hot_y:gint); cdecl; external gtklib;
procedure gtk_drag_set_icon_pixbuf(context:PGdkDragContext; pixbuf:PGdkPixbuf; hot_x:gint; hot_y:gint); cdecl; external gtklib;
procedure gtk_drag_set_icon_stock(context:PGdkDragContext; stock_id:Pgchar; hot_x:gint; hot_y:gint); cdecl; external gtklib;
procedure gtk_drag_set_icon_default(context:PGdkDragContext); cdecl; external gtklib;
function gtk_drag_check_threshold(widget:PGtkWidget; start_x:gint; start_y:gint; current_x:gint; current_y:gint):gboolean; cdecl; external gtklib;
{ Internal functions  }
procedure _gtk_drag_source_handle_event(widget:PGtkWidget; event:PGdkEvent); cdecl; external gtklib;
procedure _gtk_drag_dest_handle_event(toplevel:PGtkWidget; event:PGdkEvent); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_drag_set_default_icon(colormap:PGdkColormap; pixmap:PGdkPixmap; mask:PGdkBitmap; hot_x:gint; hot_y:gint); cdecl; external gtklib;
{$endif}
{ !GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}
// included by gtk2.pas

