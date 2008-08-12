// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkWidget = ^TGtkWidget;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

{ The flags that are used by GtkWidget on top of the
   flags field of GtkObject.
  }
{ widget is allowed to receive the default via gtk_widget_grab_default
     and will reserve space to draw the default if possible
    }
{ the widget currently is receiving the default action and should be drawn
     appropriately if possible
    }
{ the widget when focused will receive the default action and have
     HAS_DEFAULT set even if there is a different widget set as default
    }
   PGtkWidgetFlags = ^TGtkWidgetFlags;
   TGtkWidgetFlags = longint;


{ Kinds of widget-specific help  }

   PGtkWidgetHelpType = ^TGtkWidgetHelpType;
   TGtkWidgetHelpType = (GTK_WIDGET_HELP_TOOLTIP,GTK_WIDGET_HELP_WHATS_THIS
     );


{ forward declaration to avoid excessive includes (and concurrent includes)
  }
   PGtkAllocation = ^TGtkAllocation;
   TGtkAllocation = TGdkRectangle;

   TGtkCallback = procedure (widget:PGtkWidget; data:gpointer); cdecl;
{ A requisition is a desired amount of space which a
    widget may request.
  }
   PGtkRequisition = ^TGtkRequisition;
   TGtkRequisition = record
        width : gint;
        height : gint;
     end;

{ The widget is the base of the tree for displayable objects.
    (A displayable anObject is one which takes up some amount
    of screen real estate). It provides a common base and interface
    which actual widgets must adhere to.
  }
{ The anObject structure needs to be the first
      element in the widget structure in order for
      the anObject mechanism to work correctly. This
      allows a GtkWidget pointer to be cast to a
      GtkObject pointer.
    }
{ 16 bits of internally used private flags.
     this will be packed into the same 4 byte alignment frame that
     state and saved_state go. we therefore don't waste any new
     space on this.
    }
{ The state of the widget. There are actually only
      5 widget states (defined in "gtkenums.h").
    }
{ The saved state of the widget. When a widgets state
      is changed to GTK_STATE_INSENSITIVE via
      "gtk_widget_set_state" or "gtk_widget_set_sensitive"
      the old state is kept around in this field. The state
      will be restored once the widget gets sensitive again.
    }
{ The widgets name. If the widget does not have a name
      (the name is NULL), then its name (as returned by
      "gtk_widget_get_name") is its classes name.
     Among other things, the widget name is used to determine
      the style to use for a widget.
    }
{ The style for the widget. The style contains the
      colors the widget should be drawn in for each state
      along with graphics contexts used to draw with and
      the font to use for text.
    }
{ The widgets desired size.
    }
{ The widgets allocated size.
    }
{ The widgets window or its parent window if it does
      not have a window. (Which will be indicated by the
      GTK_NO_WINDOW flag being set).
    }
{ The widgets parent.
    }
   TGtkWidget = record
        anObject : TGtkObject;
        private_flags : guint16;
        state : guint8;
        saved_state : guint8;
        name : Pgchar;
        style : PGtkStyle;
        requisition : TGtkRequisition;
        allocation : TGtkAllocation;
        window : PGdkWindow;
        parent : PGtkWidget;
     end;

{ The anObject class structure needs to be the first
      element in the widget class structure in order for
      the class mechanism to work correctly. This allows a
      GtkWidgetClass pointer to be cast to a GtkObjectClass
      pointer.
    }
{ The signal to emit when a widget of this class is activated,
     gtk_widget_activate() handles the emission.
     Implementation of this signal is optional.
    }
{ This signal is emitted  when a widget of this class is added
     to a scrolling aware parent, gtk_widget_set_scroll_adjustments()
     handles the emission.
     Implementation of this signal is optional.
    }
{ seldomly overidden  }
{ basics  }
{ Mnemonics  }
{ explicit focus  }
{ events  }
{ selection  }
{ Source side drag signals  }
{ Target side drag signals  }
{ Signals used only for keybindings  }
{ If a widget has multiple tooltips/whatsthis, it should show the
     one for the current focus location, or if that doesn't make
     sense, should cycle through them showing each tip alongside
     whatever piece of the widget it applies to.
    }
{ accessibility support
    }
{ Padding for future expansion  }
   PGtkWidgetClass = ^TGtkWidgetClass;
   TGtkWidgetClass = record
        parent_class : TGtkObjectClass;
        activate_signal : guint;
        set_scroll_adjustments_signal : guint;
        dispatch_child_properties_changed : procedure (widget:PGtkWidget; n_pspecs:guint; pspecs:PPGParamSpec); cdecl;
        show : procedure (widget:PGtkWidget); cdecl;
        show_all : procedure (widget:PGtkWidget); cdecl;
        hide : procedure (widget:PGtkWidget); cdecl;
        hide_all : procedure (widget:PGtkWidget); cdecl;
        map : procedure (widget:PGtkWidget); cdecl;
        unmap : procedure (widget:PGtkWidget); cdecl;
        realize : procedure (widget:PGtkWidget); cdecl;
        unrealize : procedure (widget:PGtkWidget); cdecl;
        size_request : procedure (widget:PGtkWidget; requisition:PGtkRequisition); cdecl;
        size_allocate : procedure (widget:PGtkWidget; allocation:PGtkAllocation); cdecl;
        state_changed : procedure (widget:PGtkWidget; previous_state:TGtkStateType); cdecl;
        parent_set : procedure (widget:PGtkWidget; previous_parent:PGtkWidget); cdecl;
        hierarchy_changed : procedure (widget:PGtkWidget; previous_toplevel:PGtkWidget); cdecl;
        style_set : procedure (widget:PGtkWidget; previous_style:PGtkStyle); cdecl;
        direction_changed : procedure (widget:PGtkWidget; previous_direction:TGtkTextDirection); cdecl;
        grab_notify : procedure (widget:PGtkWidget; was_grabbed:gboolean); cdecl;
        child_notify : procedure (widget:PGtkWidget; pspec:PGParamSpec); cdecl;
        mnemonic_activate : function (widget:PGtkWidget; group_cycling:gboolean):gboolean; cdecl;
        grab_focus : procedure (widget:PGtkWidget); cdecl;
        focus : function (widget:PGtkWidget; direction:TGtkDirectionType):gboolean; cdecl;
        event : function (widget:PGtkWidget; event:PGdkEvent):gboolean; cdecl;
        button_press_event : function (widget:PGtkWidget; event:PGdkEventButton):gboolean; cdecl;
        button_release_event : function (widget:PGtkWidget; event:PGdkEventButton):gboolean; cdecl;
        scroll_event : function (widget:PGtkWidget; event:PGdkEventScroll):gboolean; cdecl;
        motion_notify_event : function (widget:PGtkWidget; event:PGdkEventMotion):gboolean; cdecl;
        delete_event : function (widget:PGtkWidget; event:PGdkEventAny):gboolean; cdecl;
        destroy_event : function (widget:PGtkWidget; event:PGdkEventAny):gboolean; cdecl;
        expose_event : function (widget:PGtkWidget; event:PGdkEventExpose):gboolean; cdecl;
        key_press_event : function (widget:PGtkWidget; event:PGdkEventKey):gboolean; cdecl;
        key_release_event : function (widget:PGtkWidget; event:PGdkEventKey):gboolean; cdecl;
        enter_notify_event : function (widget:PGtkWidget; event:PGdkEventCrossing):gboolean; cdecl;
        leave_notify_event : function (widget:PGtkWidget; event:PGdkEventCrossing):gboolean; cdecl;
        configure_event : function (widget:PGtkWidget; event:PGdkEventConfigure):gboolean; cdecl;
        focus_in_event : function (widget:PGtkWidget; event:PGdkEventFocus):gboolean; cdecl;
        focus_out_event : function (widget:PGtkWidget; event:PGdkEventFocus):gboolean; cdecl;
        map_event : function (widget:PGtkWidget; event:PGdkEventAny):gboolean; cdecl;
        unmap_event : function (widget:PGtkWidget; event:PGdkEventAny):gboolean; cdecl;
        property_notify_event : function (widget:PGtkWidget; event:PGdkEventProperty):gboolean; cdecl;
        selection_clear_event : function (widget:PGtkWidget; event:PGdkEventSelection):gboolean; cdecl;
        selection_request_event : function (widget:PGtkWidget; event:PGdkEventSelection):gboolean; cdecl;
        selection_notify_event : function (widget:PGtkWidget; event:PGdkEventSelection):gboolean; cdecl;
        proximity_in_event : function (widget:PGtkWidget; event:PGdkEventProximity):gboolean; cdecl;
        proximity_out_event : function (widget:PGtkWidget; event:PGdkEventProximity):gboolean; cdecl;
        visibility_notify_event : function (widget:PGtkWidget; event:PGdkEventVisibility):gboolean; cdecl;
        client_event : function (widget:PGtkWidget; event:PGdkEventClient):gboolean; cdecl;
        no_expose_event : function (widget:PGtkWidget; event:PGdkEventAny):gboolean; cdecl;
        window_state_event : function (widget:PGtkWidget; event:PGdkEventWindowState):gboolean; cdecl;
        selection_get : procedure (widget:PGtkWidget; selection_data:PGtkSelectionData; info:guint; time:guint); cdecl;
        selection_received : procedure (widget:PGtkWidget; selection_data:PGtkSelectionData; time:guint); cdecl;
        drag_begin : procedure (widget:PGtkWidget; context:PGdkDragContext); cdecl;
        drag_end : procedure (widget:PGtkWidget; context:PGdkDragContext); cdecl;
        drag_data_get : procedure (widget:PGtkWidget; context:PGdkDragContext; selection_data:PGtkSelectionData; info:guint; time:guint); cdecl;
        drag_data_delete : procedure (widget:PGtkWidget; context:PGdkDragContext); cdecl;
        drag_leave : procedure (widget:PGtkWidget; context:PGdkDragContext; time:guint); cdecl;
        drag_motion : function (widget:PGtkWidget; context:PGdkDragContext; x:gint; y:gint; time:guint):gboolean; cdecl;
        drag_drop : function (widget:PGtkWidget; context:PGdkDragContext; x:gint; y:gint; time:guint):gboolean; cdecl;
        drag_data_received : procedure (widget:PGtkWidget; context:PGdkDragContext; x:gint; y:gint; selection_data:PGtkSelectionData;
                      info:guint; time:guint); cdecl;
        popup_menu : function (widget:PGtkWidget):gboolean; cdecl;
        show_help : function (widget:PGtkWidget; help_type:TGtkWidgetHelpType):gboolean; cdecl;
        get_accessible : function (widget:PGtkWidget):PAtkObject; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
        _gtk_reserved5 : procedure ; cdecl;
        _gtk_reserved6 : procedure ; cdecl;
        _gtk_reserved7 : procedure ; cdecl;
        _gtk_reserved8 : procedure ; cdecl;
     end;

   PGtkWidgetAuxInfo = ^TGtkWidgetAuxInfo;
   TGtkWidgetAuxInfo = record
        x : gint;
        y : gint;
        width : gint;
        height : gint;
        flag0 : word;
     end;


   PGtkWidgetShapeInfo = ^TGtkWidgetShapeInfo;
   TGtkWidgetShapeInfo = record
        offset_x : gint16;
        offset_y : gint16;
        shape_mask : PGdkBitmap;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_TOPLEVEL = 1 shl 4;
   GTK_NO_WINDOW = 1 shl 5;
   GTK_REALIZED = 1 shl 6;
   GTK_MAPPED = 1 shl 7;
   GTK_VISIBLE = 1 shl 8;
   GTK_SENSITIVE = 1 shl 9;
   GTK_PARENT_SENSITIVE = 1 shl 10;
   GTK_CAN_FOCUS = 1 shl 11;
   GTK_HAS_FOCUS = 1 shl 12;
   GTK_CAN_DEFAULT = 1 shl 13;
   GTK_HAS_DEFAULT = 1 shl 14;
   GTK_HAS_GRAB = 1 shl 15;
   GTK_RC_STYLE = 1 shl 16;
   GTK_COMPOSITE_CHILD = 1 shl 17;
   GTK_NO_REPARENT = 1 shl 18;
   GTK_APP_PAINTABLE = 1 shl 19;
   GTK_RECEIVES_DEFAULT = 1 shl 20;
   GTK_DOUBLE_BUFFERED = 1 shl 21;


const
   bm_TGtkWidgetAuxInfo_x_set = $1;
   bp_TGtkWidgetAuxInfo_x_set = 0;
   bm_TGtkWidgetAuxInfo_y_set = $2;
   bp_TGtkWidgetAuxInfo_y_set = 1;


{ Macro for casting a pointer to a GtkWidget or GtkWidgetClass pointer.
   Macros for testing whether `widget' or `klass' are of type GTK_TYPE_WIDGET.
  }
function GTK_TYPE_WIDGET : GType;
function GTK_WIDGET(widget : pointer) : PGtkWidget;
function GTK_WIDGET_CLASS(klass: pointer) : PGtkWidgetClass;
function GTK_IS_WIDGET(widget : pointer) : boolean;
function GTK_IS_WIDGET_CLASS(klass: pointer) : boolean;
function GTK_WIDGET_GET_CLASS(obj: pointer) : PGtkWidgetClass;

{ Macros for extracting various fields from GtkWidget and GtkWidgetClass.
  }
function GTK_WIDGET_TYPE(wid : pointer) : GType;
function GTK_WIDGET_STATE(wid : pointer) : longint;
function GTK_WIDGET_SAVED_STATE(wid : pointer) : longint;

{ Macros for extracting the widget flags from GtkWidget.
  }
function GTK_WIDGET_FLAGS(wid : pointer) : guint32;
function GTK_WIDGET_TOPLEVEL(wid : pointer) : gboolean;
function GTK_WIDGET_NO_WINDOW(wid : pointer) : gboolean;
function GTK_WIDGET_REALIZED(wid : pointer) : gboolean;
function GTK_WIDGET_MAPPED(wid : pointer) : gboolean;
function GTK_WIDGET_VISIBLE(wid : pointer) : gboolean;
function GTK_WIDGET_DRAWABLE(wid : pointer) : gboolean;
function GTK_WIDGET_SENSITIVE(wid : pointer) : gboolean;
function GTK_WIDGET_PARENT_SENSITIVE(wid : pointer) : gboolean;
function GTK_WIDGET_IS_SENSITIVE(wid : pointer) : gboolean;
function GTK_WIDGET_CAN_FOCUS(wid : pointer) : gboolean;
function GTK_WIDGET_HAS_FOCUS(wid : pointer) : gboolean;
function GTK_WIDGET_CAN_DEFAULT(wid : pointer) : gboolean;
function GTK_WIDGET_HAS_DEFAULT(wid : pointer) : gboolean;
function GTK_WIDGET_HAS_GRAB(wid : pointer) : gboolean;
function GTK_WIDGET_RC_STYLE(wid : pointer) : gboolean;
function GTK_WIDGET_COMPOSITE_CHILD(wid : pointer) : gboolean;
function GTK_WIDGET_APP_PAINTABLE(wid : pointer) : gboolean;
function GTK_WIDGET_RECEIVES_DEFAULT(wid : pointer) : gboolean;
function GTK_WIDGET_DOUBLE_BUFFERED(wid : pointer) : gboolean;

{ Macros for setting and clearing widget flags. }
function GTK_WIDGET_SET_FLAGS (wid   : PGtkWidget;
                               flags : TGtkWidgetFlags): TGtkWidgetFlags;

function GTK_WIDGET_UNSET_FLAGS (wid   : PGtkWidget;
                                 flags : TGtkWidgetFlags): TGtkWidgetFlags;

function GTK_TYPE_REQUISITION : GType;
function x_set(var a : TGtkWidgetAuxInfo) : guint;
procedure set_x_set(var a : TGtkWidgetAuxInfo; __x_set : guint);
function y_set(var a : TGtkWidgetAuxInfo) : guint;
procedure set_y_set(var a : TGtkWidgetAuxInfo; __y_set : guint);


function gtk_widget_get_type:TGtkType; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_widget_new(_type:TGtkType; first_property_name:Pgchar; args:array of const):PGtkWidget; cdecl; overload; external gtklib;
function gtk_widget_new(_type:TGtkType; first_property_name:Pgchar):PGtkWidget; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_widget_new(_type:TGtkType; first_property_name:Pgchar):PGtkWidget; varargs; cdecl; external gtklib;
{$ENDIF}
function gtk_widget_ref(widget:PGtkWidget):PGtkWidget; cdecl; external gtklib;
procedure gtk_widget_unref(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_destroy(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_destroyed(widget:PGtkWidget; var widget_pointer:PGtkWidget); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
{$IFNDEF KYLIX}
procedure gtk_widget_set(widget:PGtkWidget; first_property_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_widget_set(widget:PGtkWidget; first_property_name:Pgchar); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_widget_set(widget:PGtkWidget; first_property_name:Pgchar); varargs; cdecl; external gtklib;
{$ENDIF}
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_widget_unparent(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_show(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_show_now(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_hide(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_show_all(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_hide_all(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_map(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_unmap(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_realize(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_unrealize(widget:PGtkWidget); cdecl; external gtklib;
{ Queuing draws  }
procedure gtk_widget_queue_draw(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_queue_draw_area(widget:PGtkWidget; x:gint; y:gint; width:gint; height:gint); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_widget_queue_clear(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_queue_clear_area(widget:PGtkWidget; x:gint; y:gint; width:gint; height:gint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_widget_queue_resize(widget:PGtkWidget); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_widget_draw(widget:PGtkWidget; area:PGdkRectangle); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_widget_size_request(widget:PGtkWidget; requisition:PGtkRequisition); cdecl; external gtklib;
procedure gtk_widget_size_allocate(widget:PGtkWidget; allocation:PGtkAllocation); cdecl; external gtklib;
procedure gtk_widget_get_child_requisition(widget:PGtkWidget; requisition:PGtkRequisition); cdecl; external gtklib;
procedure gtk_widget_add_accelerator(widget:PGtkWidget; accel_signal:Pgchar; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType;
            accel_flags:TGtkAccelFlags); cdecl; external gtklib;
function gtk_widget_remove_accelerator(widget:PGtkWidget; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType):gboolean; cdecl; external gtklib;
procedure gtk_widget_set_accel_path(widget:PGtkWidget; accel_path:Pgchar; accel_group:PGtkAccelGroup); cdecl; external gtklib;
function _gtk_widget_get_accel_path(widget:PGtkWidget; locked:Pgboolean):Pgchar; cdecl; external gtklib;
function gtk_widget_list_accel_closures(widget:PGtkWidget):PGList; cdecl; external gtklib;
function gtk_widget_mnemonic_activate(widget:PGtkWidget; group_cycling:gboolean):gboolean; cdecl; external gtklib;
function gtk_widget_event(widget:PGtkWidget; event:PGdkEvent):gboolean; cdecl; external gtklib;
function gtk_widget_send_expose(widget:PGtkWidget; event:PGdkEvent):gint; cdecl; external gtklib;
function gtk_widget_activate(widget:PGtkWidget):gboolean; cdecl; external gtklib;
function gtk_widget_set_scroll_adjustments(widget:PGtkWidget; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):gboolean; cdecl; external gtklib;
procedure gtk_widget_reparent(widget:PGtkWidget; new_parent:PGtkWidget); cdecl; external gtklib;
function gtk_widget_intersect(widget:PGtkWidget; area:PGdkRectangle; intersection:PGdkRectangle):gboolean; cdecl; external gtklib;
function gtk_widget_region_intersect(widget:PGtkWidget; region:PGdkRegion):PGdkRegion; cdecl; external gtklib;
procedure gtk_widget_freeze_child_notify(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_child_notify(widget:PGtkWidget; child_property:Pgchar); cdecl; external gtklib;
procedure gtk_widget_thaw_child_notify(widget:PGtkWidget); cdecl; external gtklib;
function gtk_widget_is_focus(widget:PGtkWidget):gboolean; cdecl; external gtklib;
procedure gtk_widget_grab_focus(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_grab_default(widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_set_name(widget:PGtkWidget; name:Pgchar); cdecl; external gtklib;
function gtk_widget_get_name(widget:PGtkWidget):Pgchar; cdecl; external gtklib;
procedure gtk_widget_set_state(widget:PGtkWidget; state:TGtkStateType); cdecl; external gtklib;
procedure gtk_widget_set_sensitive(widget:PGtkWidget; sensitive:gboolean); cdecl; external gtklib;
procedure gtk_widget_set_app_paintable(widget:PGtkWidget; app_paintable:gboolean); cdecl; external gtklib;
procedure gtk_widget_set_double_buffered(widget:PGtkWidget; double_buffered:gboolean); cdecl; external gtklib;
procedure gtk_widget_set_redraw_on_allocate(widget:PGtkWidget; redraw_on_allocate:gboolean); cdecl; external gtklib;
procedure gtk_widget_set_parent(widget:PGtkWidget; parent:PGtkWidget); cdecl; external gtklib;
procedure gtk_widget_set_parent_window(widget:PGtkWidget; parent_window:PGdkWindow); cdecl; external gtklib;
procedure gtk_widget_set_child_visible(widget:PGtkWidget; is_visible:gboolean); cdecl; external gtklib;
function gtk_widget_get_child_visible(widget:PGtkWidget):gboolean; cdecl; external gtklib;
function gtk_widget_get_parent(widget:PGtkWidget):PGtkWidget; cdecl; external gtklib;
function gtk_widget_get_parent_window(widget:PGtkWidget):PGdkWindow; cdecl; external gtklib;
function gtk_widget_child_focus(widget:PGtkWidget; direction:TGtkDirectionType):gboolean; cdecl; external gtklib;
procedure gtk_widget_set_size_request(widget:PGtkWidget; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_widget_get_size_request(widget:PGtkWidget; width:Pgint; height:Pgint); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_widget_set_uposition(widget:PGtkWidget; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_widget_set_usize(widget:PGtkWidget; width:gint; height:gint); cdecl; external gtklib;
{$endif}

procedure gtk_widget_set_events(widget:PGtkWidget; events:gint); cdecl; external gtklib;
procedure gtk_widget_add_events(widget:PGtkWidget; events:gint); cdecl; external gtklib;
procedure gtk_widget_set_extension_events(widget:PGtkWidget; mode:TGdkExtensionMode); cdecl; external gtklib;
function gtk_widget_get_extension_events(widget:PGtkWidget):TGdkExtensionMode; cdecl; external gtklib;
function gtk_widget_get_toplevel(widget:PGtkWidget):PGtkWidget; cdecl; external gtklib;
function gtk_widget_get_ancestor(widget:PGtkWidget; widget_type:TGtkType):PGtkWidget; cdecl; external gtklib;
function gtk_widget_get_colormap(widget:PGtkWidget):PGdkColormap; cdecl; external gtklib;
function gtk_widget_get_visual(widget:PGtkWidget):PGdkVisual; cdecl; external gtklib;
function gtk_widget_get_settings(widget:PGtkWidget):PGtkSettings; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_widget_set_visual(widget,visual : pointer);
procedure gtk_widget_push_visual(visual : pointer);
procedure gtk_widget_pop_visual;
procedure gtk_widget_set_default_visual(visual : pointer);
{$endif}

{ GTK_DISABLE_DEPRECATED  }
{ Accessibility support  }

function gtk_widget_get_accessible(widget:PGtkWidget):PAtkObject; cdecl; external gtklib;
{ The following functions must not be called on an already
   realized widget. Because it is possible that somebody
   can call get_colormap() or get_visual() and save the
   result, these functions are probably only safe to
   call in a widget's init() function.
  }
procedure gtk_widget_set_colormap(widget:PGtkWidget; colormap:PGdkColormap); cdecl; external gtklib;
function gtk_widget_get_events(widget:PGtkWidget):gint; cdecl; external gtklib;
procedure gtk_widget_get_pointer(widget:PGtkWidget; x:Pgint; y:Pgint); cdecl; external gtklib;
function gtk_widget_is_ancestor(widget:PGtkWidget; ancestor:PGtkWidget):gboolean; cdecl; external gtklib;
function gtk_widget_translate_coordinates(src_widget:PGtkWidget; dest_widget:PGtkWidget; src_x:gint; src_y:gint; dest_x:Pgint;
           dest_y:Pgint):gboolean; cdecl; external gtklib;
{ Hide widget and return TRUE.
  }
function gtk_widget_hide_on_delete(widget:PGtkWidget):gboolean; cdecl; external gtklib;
{ Widget styles.
  }
procedure gtk_widget_set_style(widget:PGtkWidget; style:PGtkStyle); cdecl; external gtklib;
procedure gtk_widget_ensure_style(widget:PGtkWidget); cdecl; external gtklib;
function gtk_widget_get_style(widget:PGtkWidget):PGtkStyle; cdecl; external gtklib;
procedure gtk_widget_modify_style(widget:PGtkWidget; style:PGtkRcStyle); cdecl; external gtklib;
function gtk_widget_get_modifier_style(widget:PGtkWidget):PGtkRcStyle; cdecl; external gtklib;
procedure gtk_widget_modify_fg(widget:PGtkWidget; state:TGtkStateType; color:PGdkColor); cdecl; external gtklib;
procedure gtk_widget_modify_bg(widget:PGtkWidget; state:TGtkStateType; color:PGdkColor); cdecl; external gtklib;
procedure gtk_widget_modify_text(widget:PGtkWidget; state:TGtkStateType; color:PGdkColor); cdecl; external gtklib;
procedure gtk_widget_modify_base(widget:PGtkWidget; state:TGtkStateType; color:PGdkColor); cdecl; external gtklib;
procedure gtk_widget_modify_font(widget:PGtkWidget; font_desc:PPangoFontDescription); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_widget_set_rc_style(widget : pointer);
procedure gtk_widget_restore_default_style(widget : pointer);
{$endif}

function gtk_widget_create_pango_context(widget:PGtkWidget):PPangoContext; cdecl; external gtklib;
function gtk_widget_get_pango_context(widget:PGtkWidget):PPangoContext; cdecl; external gtklib;
function gtk_widget_create_pango_layout(widget:PGtkWidget; text:Pgchar):PPangoLayout; cdecl; external gtklib;
function gtk_widget_render_icon(widget:PGtkWidget; stock_id:Pgchar; size:TGtkIconSize; detail:Pgchar):PGdkPixbuf; cdecl; external gtklib;
{ handle composite names for GTK_COMPOSITE_CHILD widgets,
   the returned name is newly allocated.
  }

procedure gtk_widget_set_composite_name(widget:PGtkWidget; name:Pgchar); cdecl; external gtklib;
function gtk_widget_get_composite_name(widget:PGtkWidget):Pgchar; cdecl; external gtklib;
{ Descend recursively and set rc-style on all widgets without user styles  }
procedure gtk_widget_reset_rc_styles(widget:PGtkWidget); cdecl; external gtklib;
{ Push/pop pairs, to change default values upon a widget's creation.
   This will override the values that got set by the
   gtk_widget_set_default_  () functions.
  }
procedure gtk_widget_push_colormap(cmap:PGdkColormap); cdecl; external gtklib;
procedure gtk_widget_push_composite_child; cdecl; external gtklib;
procedure gtk_widget_pop_composite_child; cdecl; external gtklib;
procedure gtk_widget_pop_colormap; cdecl; external gtklib;
{ widget style properties
  }
procedure gtk_widget_class_install_style_property(klass:PGtkWidgetClass; pspec:PGParamSpec); cdecl; external gtklib;
procedure gtk_widget_class_install_style_property_parser(klass:PGtkWidgetClass; pspec:PGParamSpec; parser:TGtkRcPropertyParser); cdecl; external gtklib;
procedure gtk_widget_style_get_property(widget:PGtkWidget; property_name:Pgchar; value:PGValue); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_widget_style_get_valist(widget:PGtkWidget; first_property_name:Pgchar; var_args: array of const); cdecl; external gtklib;
procedure gtk_widget_style_get(widget:PGtkWidget; first_property_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_widget_style_get(widget:PGtkWidget; first_property_name:Pgchar); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_widget_style_get_valist(widget:PGtkWidget; first_property_name:Pgchar); varargs; cdecl; external gtklib;
procedure gtk_widget_style_get(widget:PGtkWidget; first_property_name:Pgchar); varargs; cdecl; external gtklib;
{$ENDIF}
{ Set certain default values to be used at widget creation time.
  }
procedure gtk_widget_set_default_colormap(colormap:PGdkColormap); cdecl; external gtklib;
function gtk_widget_get_default_style:PGtkStyle; cdecl; external gtklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gtk_widget_get_default_colormap:PGdkColormap; cdecl; external gtklib;
function gtk_widget_get_default_visual:PGdkVisual; cdecl; external gtklib;
{$endif}
{ Functions for setting directionality for widgets
  }

procedure gtk_widget_set_direction(widget:PGtkWidget; dir:TGtkTextDirection); cdecl; external gtklib;
function gtk_widget_get_direction(widget:PGtkWidget):TGtkTextDirection; cdecl; external gtklib;
procedure gtk_widget_set_default_direction(dir:TGtkTextDirection); cdecl; external gtklib;
function gtk_widget_get_default_direction:TGtkTextDirection; cdecl; external gtklib;
{ Counterpart to gdk_window_shape_combine_mask.
  }
procedure gtk_widget_shape_combine_mask(widget:PGtkWidget; shape_mask:PGdkBitmap; offset_x:gint; offset_y:gint); cdecl; external gtklib;
{ internal function  }
procedure gtk_widget_reset_shapes(widget:PGtkWidget); cdecl; external gtklib;
{ Compute a widget's path in the form "GtkWindow.MyLabel", and
   return newly alocated strings.
  }
procedure gtk_widget_path(widget:PGtkWidget; path_length:Pguint; path:PPgchar; path_reversed:PPgchar); cdecl; external gtklib;
procedure gtk_widget_class_path(widget:PGtkWidget; path_length:Pguint; path:PPgchar; path_reversed:PPgchar); cdecl; external gtklib;
function gtk_requisition_get_type:GType; cdecl; external gtklib;
function gtk_requisition_copy(requisition:PGtkRequisition):PGtkRequisition; cdecl; external gtklib;
procedure gtk_requisition_free(requisition:PGtkRequisition); cdecl; external gtklib;

function _gtk_widget_get_aux_info(widget:PGtkWidget; create:gboolean):PGtkWidgetAuxInfo; cdecl; external gtklib;
procedure _gtk_widget_propagate_hierarchy_changed(widget:PGtkWidget; previous_toplevel:PGtkWidget); cdecl; external gtklib;
function _gtk_widget_peek_colormap:PGdkColormap; cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_widget_class_find_style_property(klass:PGtkWidgetClass; property_name:Pgchar):PGParamSpec; cdecl; external gtklib;
function gtk_widget_class_list_style_properties(klass:PGtkWidgetClass; n_properties:Pguint):PPGParamSpec; cdecl; external gtklib;
function gtk_widget_get_clipboard(widget:PGtkWidget; selection:TGdkAtom):PGtkClipboard; cdecl; external gtklib;
function gtk_widget_get_display(widget:PGtkWidget):PGdkDisplay; cdecl; external gtklib;
function gtk_widget_get_root_window(widget:PGtkWidget):PGdkWindow; cdecl; external gtklib;
function gtk_widget_get_screen(widget:PGtkWidget):PGdkScreen; cdecl; external gtklib;
function gtk_widget_has_screen(widget:PGtkWidget):gboolean; cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_WIDGET : GType;
begin
   GTK_TYPE_WIDGET:=gtk_widget_get_type;
end;

function GTK_WIDGET(widget : pointer) : PGtkWidget;
begin
   GTK_WIDGET:=PGtkWidget(GTK_CHECK_CAST(widget,GTK_TYPE_WIDGET));
end;

function GTK_WIDGET_CLASS(klass: pointer) : PGtkWidgetClass;
begin
   GTK_WIDGET_CLASS:=PGtkWidgetClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_WIDGET));
end;

function GTK_IS_WIDGET(widget : pointer) : boolean;
begin
   GTK_IS_WIDGET:=GTK_CHECK_TYPE(widget,GTK_TYPE_WIDGET);
end;

function GTK_IS_WIDGET_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_WIDGET_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_WIDGET);
end;

function GTK_WIDGET_GET_CLASS(obj: pointer) : PGtkWidgetClass;
begin
   GTK_WIDGET_GET_CLASS:=PGtkWidgetClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_WIDGET));
end;

function GTK_WIDGET_TYPE(wid : pointer) : GType;
begin
   GTK_WIDGET_TYPE:=GTK_OBJECT_TYPE(wid);
end;

function GTK_WIDGET_STATE(wid : pointer) : longint;
begin
   GTK_WIDGET_STATE:=(GTK_WIDGET(wid))^.state;
end;

function GTK_WIDGET_SAVED_STATE(wid : pointer) : longint;
begin
   GTK_WIDGET_SAVED_STATE:=(GTK_WIDGET(wid))^.saved_state;
end;

function GTK_WIDGET_FLAGS(wid : pointer) : guint32;
begin
   GTK_WIDGET_FLAGS:=GTK_OBJECT_FLAGS(wid);
end;

function GTK_WIDGET_TOPLEVEL(wid : pointer) : gboolean;
begin
   GTK_WIDGET_TOPLEVEL:=((GTK_WIDGET_FLAGS(wid)) and GTK_TOPLEVEL) <> 0;
end;

function GTK_WIDGET_NO_WINDOW(wid : pointer) : gboolean;
begin
   GTK_WIDGET_NO_WINDOW:=((GTK_WIDGET_FLAGS(wid)) and GTK_NO_WINDOW) <> 0;
end;

function GTK_WIDGET_REALIZED(wid : pointer) : gboolean;
begin
   GTK_WIDGET_REALIZED:=((GTK_WIDGET_FLAGS(wid)) and GTK_REALIZED) <> 0;
end;

function GTK_WIDGET_MAPPED(wid : pointer) : gboolean;
begin
   GTK_WIDGET_MAPPED:=((GTK_WIDGET_FLAGS(wid)) and GTK_MAPPED) <> 0;
end;

function GTK_WIDGET_VISIBLE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_VISIBLE:=((GTK_WIDGET_FLAGS(wid)) and GTK_VISIBLE) <> 0;
end;

function GTK_WIDGET_DRAWABLE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_DRAWABLE:=(GTK_WIDGET_VISIBLE(wid)) and (GTK_WIDGET_MAPPED(wid));
end;

function GTK_WIDGET_SENSITIVE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_SENSITIVE:=((GTK_WIDGET_FLAGS(wid)) and GTK_SENSITIVE) <> 0;
end;

function GTK_WIDGET_PARENT_SENSITIVE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_PARENT_SENSITIVE:=((GTK_WIDGET_FLAGS(wid)) and GTK_PARENT_SENSITIVE) <> 0;
end;

function GTK_WIDGET_IS_SENSITIVE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_IS_SENSITIVE:=(GTK_WIDGET_SENSITIVE(wid)) and (GTK_WIDGET_PARENT_SENSITIVE(wid));
end;

function GTK_WIDGET_CAN_FOCUS(wid : pointer) : gboolean;
begin
   GTK_WIDGET_CAN_FOCUS:=((GTK_WIDGET_FLAGS(wid)) and GTK_CAN_FOCUS) <> 0;
end;

function GTK_WIDGET_HAS_FOCUS(wid : pointer) : gboolean;
begin
   GTK_WIDGET_HAS_FOCUS:=((GTK_WIDGET_FLAGS(wid)) and GTK_HAS_FOCUS) <> 0;
end;

function GTK_WIDGET_CAN_DEFAULT(wid : pointer) : gboolean;
begin
   GTK_WIDGET_CAN_DEFAULT:=((GTK_WIDGET_FLAGS(wid)) and GTK_CAN_DEFAULT) <> 0;
end;

function GTK_WIDGET_HAS_DEFAULT(wid : pointer) : gboolean;
begin
   GTK_WIDGET_HAS_DEFAULT:=((GTK_WIDGET_FLAGS(wid)) and GTK_HAS_DEFAULT) <> 0;
end;

function GTK_WIDGET_HAS_GRAB(wid : pointer) : gboolean;
begin
   GTK_WIDGET_HAS_GRAB:=((GTK_WIDGET_FLAGS(wid)) and GTK_HAS_GRAB) <> 0;
end;

function GTK_WIDGET_RC_STYLE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_RC_STYLE:=((GTK_WIDGET_FLAGS(wid)) and GTK_RC_STYLE) <> 0;
end;

function GTK_WIDGET_COMPOSITE_CHILD(wid : pointer) : gboolean;
begin
   GTK_WIDGET_COMPOSITE_CHILD:=((GTK_WIDGET_FLAGS(wid)) and GTK_COMPOSITE_CHILD) <> 0;
end;

function GTK_WIDGET_APP_PAINTABLE(wid : pointer) : gboolean;
begin
   GTK_WIDGET_APP_PAINTABLE:=((GTK_WIDGET_FLAGS(wid)) and GTK_APP_PAINTABLE) <> 0;
end;

function GTK_WIDGET_RECEIVES_DEFAULT(wid : pointer) : gboolean;
begin
   GTK_WIDGET_RECEIVES_DEFAULT:=((GTK_WIDGET_FLAGS(wid)) and GTK_RECEIVES_DEFAULT) <> 0;
end;

function GTK_WIDGET_DOUBLE_BUFFERED(wid : pointer) : gboolean;
begin
   GTK_WIDGET_DOUBLE_BUFFERED:=((GTK_WIDGET_FLAGS(wid)) and GTK_DOUBLE_BUFFERED) <> 0;
end;

function GTK_TYPE_REQUISITION : GType;
begin
   GTK_TYPE_REQUISITION:=gtk_requisition_get_type;
end;

function x_set(var a : TGtkWidgetAuxInfo) : guint;
begin
   x_set:=(a.flag0 and bm_TGtkWidgetAuxInfo_x_set) shr bp_TGtkWidgetAuxInfo_x_set;
end;

procedure set_x_set(var a : TGtkWidgetAuxInfo; __x_set : guint);
begin
   a.flag0:=a.flag0 or ((__x_set shl bp_TGtkWidgetAuxInfo_x_set) and bm_TGtkWidgetAuxInfo_x_set);
end;

function y_set(var a : TGtkWidgetAuxInfo) : guint;
begin
   y_set:=(a.flag0 and bm_TGtkWidgetAuxInfo_y_set) shr bp_TGtkWidgetAuxInfo_y_set;
end;

procedure set_y_set(var a : TGtkWidgetAuxInfo; __y_set : guint);
begin
   a.flag0:=a.flag0 or ((__y_set shl bp_TGtkWidgetAuxInfo_y_set) and bm_TGtkWidgetAuxInfo_y_set);
end;

procedure gtk_widget_set_visual(widget,visual : pointer);
begin
   // Dummy statement:
   if (Widget<>nil) and (visual<>nil) then ;
end;

procedure gtk_widget_push_visual(visual : pointer);
begin
   // Dummy statement:
   if (visual<>nil) then ;
end;

procedure gtk_widget_pop_visual;
begin
   ;
end;

procedure gtk_widget_set_default_visual(visual : pointer);
begin
   // Dummy statement:
   if (visual<>nil) then ;
end;

procedure gtk_widget_set_rc_style(widget : pointer);
begin
   gtk_widget_set_style(widget,NULL);
end;

procedure gtk_widget_restore_default_style(widget : pointer);
begin
   gtk_widget_set_style(widget,NULL);
end;

function GTK_WIDGET_SET_FLAGS (wid   : PGtkWidget;
                               flags : TGtkWidgetFlags): TGtkWidgetFlags;
begin
  pGtkObject(wid)^.flags := pGtkObject(wid)^.flags or flags;
  GTK_WIDGET_SET_FLAGS   := pGtkObject(wid)^.flags;
end;

function GTK_WIDGET_UNSET_FLAGS (wid   : PGtkWidget;
                                 flags : TGtkWidgetFlags): TGtkWidgetFlags;
begin
  pGtkObject(wid)^.flags := pGtkObject(wid)^.flags and (not flags);
  GTK_WIDGET_UNSET_FLAGS   := pGtkObject(wid)^.flags;
end;

{$ENDIF read_implementation}
// included by gtk2.pas

