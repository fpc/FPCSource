{
   $Id$
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
{$ifndef win32}
procedure gtk_drag_source_unset(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_drag_source_unset';
{$endif}
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


{
  $Log$
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.6  1999/10/21 14:31:21  peter
    * made clear_unset ifndef win32

  Revision 1.5  1999/10/21 08:42:01  florian
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

  Revision 1.4  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.3  1999/05/11 00:38:26  peter
    * win32 fixes

  Revision 1.2  1999/05/10 15:19:14  peter
    * cdecl fixes

  Revision 1.1  1999/05/10 09:13:59  peter
    + new gtk 1.2 files

}

