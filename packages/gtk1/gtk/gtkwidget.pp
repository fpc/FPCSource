{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  Type
     GtkWidgetFlags = longint;
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

    type
       TGtkRcFlags = longint;
    const
       GTK_RC_FG = 1;
       GTK_RC_BG = 2;
       GTK_RC_TEXT = 4;
       GTK_RC_BASE = 8;

    type
       PGtkRequisition = ^TGtkRequisition;
       TGtkRequisition = record
            width : gint16;
            height : gint16;
         end;

       PGtkAllocation = ^TGtkAllocation;
       TGtkAllocation = record
            x : gint16;
            y : gint16;
            width : guint16;
            height : guint16;
         end;

       PGtkSelectionData = ^TGtkSelectionData;
       TGtkSelectionData = record
            selection : TGdkAtom;
            target : TGdkAtom;
            thetype : TGdkAtom;
            format : gint;
            data : Pguchar;
            length : gint;
         end;

       PGtkStyle = ^TGtkStyle;

       PGtkWidget = ^TGtkWidget;
       PPGtkWidget = ^PGTKWidget;
       TGtkWidget = record
            theobject :TGtkObject;
            private_flags : guint16;
            state : guint8;
            saved_state : guint8;
            name : Pgchar;
            thestyle : pointer; {PGtkStyle}
            requisition : TGtkRequisition;
            allocation : TGtkAllocation;
            window : PGdkWindow;
            parent : PGtkWidget;
         end;

       PGtkWidgetClass = ^TGtkWidgetClass;
       TGtkWidgetClass = record
            parent_class :TGtkObjectClass;
            activate_signal : guint;
            set_scroll_adjustments_signal : guint;
            show : procedure (widget:PGtkWidget); cdecl;
            show_all : procedure (widget:PGtkWidget); cdecl;
            hide : procedure (widget:PGtkWidget); cdecl;
            hide_all : procedure (widget:PGtkWidget); cdecl;
            map : procedure (widget:PGtkWidget); cdecl;
            unmap : procedure (widget:PGtkWidget); cdecl;
            realize : procedure (widget:PGtkWidget); cdecl;
            unrealize : procedure (widget:PGtkWidget); cdecl;
            draw : procedure (widget:PGtkWidget; area:PGdkRectangle); cdecl;
            draw_focus : procedure (widget:PGtkWidget); cdecl;
            draw_default : procedure (widget:PGtkWidget); cdecl;
            size_request : procedure (widget:PGtkWidget; requisition:PGtkRequisition); cdecl;
            size_allocate : procedure (widget:PGtkWidget; allocation:PGtkAllocation); cdecl;
            state_changed : procedure (widget:PGtkWidget; previous_state:TGtkStateType); cdecl;
            parent_set : procedure (widget:PGtkWidget; previous_parent:PGtkWidget); cdecl;
            style_set : procedure (widget:PGtkWidget; previous_style:pointer{PGtkStyle}); cdecl;
            add_accelerator : function (widget:PGtkWidget; accel_signal_id:guint; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType; accel_flags:TGtkAccelFlags):gint; cdecl;
            remove_accelerator : procedure (widget:PGtkWidget; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType);cdecl;
            grab_focus : procedure (widget:PGtkWidget);cdecl;
            event : function (widget:PGtkWidget; event:PGdkEvent):gint; cdecl;
            button_press_event : function (widget:PGtkWidget; event:PGdkEventButton):gint; cdecl;
            button_release_event : function (widget:PGtkWidget; event:PGdkEventButton):gint; cdecl;
            motion_notify_event : function (widget:PGtkWidget; event:PGdkEventMotion):gint; cdecl;
            delete_event : function (widget:PGtkWidget; event:PGdkEventAny):gint; cdecl;
            destroy_event : function (widget:PGtkWidget; event:PGdkEventAny):gint; cdecl;
            expose_event : function (widget:PGtkWidget; event:PGdkEventExpose):gint; cdecl;
            key_press_event : function (widget:PGtkWidget; event:PGdkEventKey):gint; cdecl;
            key_release_event : function (widget:PGtkWidget; event:PGdkEventKey):gint; cdecl;
            enter_notify_event : function (widget:PGtkWidget; event:PGdkEventCrossing):gint; cdecl;
            leave_notify_event : function (widget:PGtkWidget; event:PGdkEventCrossing):gint; cdecl;
            configure_event : function (widget:PGtkWidget; event:PGdkEventConfigure):gint; cdecl;
            focus_in_event : function (widget:PGtkWidget; event:PGdkEventFocus):gint; cdecl;
            focus_out_event : function (widget:PGtkWidget; event:PGdkEventFocus):gint; cdecl;
            map_event : function (widget:PGtkWidget; event:PGdkEventAny):gint; cdecl;
            unmap_event : function (widget:PGtkWidget; event:PGdkEventAny):gint; cdecl;
            property_notify_event : function (widget:PGtkWidget; event:PGdkEventProperty):gint; cdecl;
            selection_clear_event : function (widget:PGtkWidget; event:PGdkEventSelection):gint; cdecl;
            selection_request_event : function (widget:PGtkWidget; event:PGdkEventSelection):gint; cdecl;
            selection_notify_event : function (widget:PGtkWidget; event:PGdkEventSelection):gint; cdecl;
            proximity_in_event : function (widget:PGtkWidget; event:PGdkEventProximity):gint; cdecl;
            proximity_out_event : function (widget:PGtkWidget; event:PGdkEventProximity):gint; cdecl;
            visibility_notify_event : function (widget:PGtkWidget; event:PGdkEventVisibility):gint;cdecl;
            client_event : function (widget:PGtkWidget; event:PGdkEventClient):gint;cdecl;
            no_expose_event : function (widget:PGtkWidget; event:PGdkEventAny):gint;cdecl;
            selection_get : procedure (widget:PGtkWidget; selection_data:PGtkSelectionData; info:guint; time:guint);cdecl;
            selection_received : procedure (widget:PGtkWidget; selection_data:PGtkSelectionData; time:guint);cdecl;
            drag_begin : procedure (widget:PGtkWidget; context:PGdkDragContext);cdecl;
            drag_end : procedure (widget:PGtkWidget; context:PGdkDragContext);cdecl;
            drag_data_get : procedure (widget:PGtkWidget; context:PGdkDragContext; selection_data:PGtkSelectionData; info:guint; time:guint);cdecl;
            drag_data_delete : procedure (widget:PGtkWidget; context:PGdkDragContext);cdecl;
            drag_leave : procedure (widget:PGtkWidget; context:PGdkDragContext; time:guint);cdecl;
            drag_motion : function (widget:PGtkWidget; context:PGdkDragContext; x:gint; y:gint; time:guint):gboolean;cdecl;
            drag_drop : function (widget:PGtkWidget; context:PGdkDragContext; x:gint; y:gint; time:guint):gboolean;cdecl;
            drag_data_received : procedure (widget:PGtkWidget; context:PGdkDragContext; x:gint; y:gint; selection_data:PGtkSelectionData; info:guint; time:guint);cdecl;
            debug_msg : procedure (widget:PGtkWidget; thestring:Pgchar);cdecl;
            pad1 :TGtkfunction;
            pad2 :TGtkfunction;
            pad3 :TGtkfunction;
            pad4 :TGtkfunction;
         end;

       PGtkWidgetAuxInfo = ^TGtkWidgetAuxInfo;
       TGtkWidgetAuxInfo = record
            x : gint16;
            y : gint16;
            width : gint16;
            height : gint16;
         end;

       PGtkWidgetShapeInfo = ^TGtkWidgetShapeInfo;
       TGtkWidgetShapeInfo = record
            offset_x : gint16;
            offset_y : gint16;
            shape_mask : PGdkBitmap;
         end;

       TGTKCallBack = procedure (Widget : PGTKwidget; data : gpointer); cdecl;

{*************************************
      Insertion from gtkrc
*************************************}

     PGtkThemeEngine = ^TGtkThemeEngine;

     PGtkRcStyle = ^TGtkRcStyle;
     TGtkRcStyle = record
          name : Pgchar;
          font_name : Pgchar;
          fontset_name : Pgchar;
          bg_pixmap_name : array[0..4] of Pgchar;
          color_flags : array[0..4] of TGtkRcFlags;
          fg : array[0..4] of TGdkColor;
          bg : array[0..4] of TGdkColor;
          text : array[0..4] of TGdkColor;
          base : array[0..4] of TGdkColor;
          engine : PGtkThemeEngine;
          engine_data : gpointer;
       end;


{*************************************
      Insertion from gtktheme
*************************************}

     TGtkThemeEngine = record
          parse_rc_style : function (scanner:PGScanner; rc_style:PGtkRcStyle):guint;cdecl;
          merge_rc_style : procedure (dest:PGtkRcStyle; src:PGtkRcStyle);cdecl;
          rc_style_to_style : procedure (style:PGtkStyle; rc_style:PGtkRcStyle);cdecl;
          duplicate_style : procedure (dest:PGtkStyle; src:PGtkStyle);cdecl;
          realize_style : procedure (new_style:PGtkStyle);cdecl;
          unrealize_style : procedure (new_style:PGtkStyle);cdecl;
          destroy_rc_style : procedure (rc_style:PGtkRcStyle);cdecl;
          destroy_style : procedure (style:PGtkStyle);cdecl;
          set_background : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType);cdecl;
       end;

{*************************************
      Insertion from gtkstyle
*************************************}

       PGtkStyleClass = ^TGtkStyleClass;

       TGtkStyle = record
            klass : PGtkStyleClass;
            fg : array[0..4] of TGdkColor;
            bg : array[0..4] of TGdkColor;
            light : array[0..4] of TGdkColor;
            dark : array[0..4] of TGdkColor;
            mid : array[0..4] of TGdkColor;
            text : array[0..4] of TGdkColor;
            base : array[0..4] of TGdkColor;
            black : TGdkColor;
            white : TGdkColor;
            font : PGdkFont;
            fg_gc : array[0..4] of PGdkGC;
            bg_gc : array[0..4] of PGdkGC;
            light_gc : array[0..4] of PGdkGC;
            dark_gc : array[0..4] of PGdkGC;
            mid_gc : array[0..4] of PGdkGC;
            text_gc : array[0..4] of PGdkGC;
            base_gc : array[0..4] of PGdkGC;
            black_gc : PGdkGC;
            white_gc : PGdkGC;
            bg_pixmap : array[0..4] of PGdkPixmap;
            ref_count : gint;
            attach_count : gint;
            depth : gint;
            colormap : PGdkColormap;
            engine : PGtkThemeEngine;
            engine_data : gpointer;
            rc_style : PGtkRcStyle;
            styles : PGSList;
         end;

       TGtkStyleClass = record
          xthickness : gint;
          ythickness : gint;
          draw_hline : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x1:gint; x2:gint; y:gint);cdecl;
          draw_vline : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; y1:gint; y2:gint; x:gint);cdecl;
          draw_shadow : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_polygon : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; point:PGdkPoint; npoints:gint; fill:gboolean);cdecl;
          draw_arrow : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; arrow_type:TGtkArrowType; fill:gboolean; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_diamond : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_oval : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_string : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; thestring:Pgchar);cdecl;
          draw_box : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_flat_box : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_check : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_option : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_cross : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_ramp : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; arrow_type:TGtkArrowType; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_tab : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_shadow_gap : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint);cdecl;
          draw_box_gap : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint);cdecl;
          draw_extension : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType);cdecl;
          draw_focus : procedure (style:PGtkStyle; window:PGdkWindow; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;
          draw_slider : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; orientation:TGtkOrientation);cdecl;
          draw_handle : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; orientation:TGtkOrientation);cdecl;
        end;

{*************************************
          End of insertion
*************************************}


function  GTK_WIDGET_STATE(wid : PGtkwidget) : ptrint;
function  GTK_WIDGET_SAVED_STATE(wid : PGtkwidget) : ptrint;
function  GTK_WIDGET_FLAGS(wid : PGtkwidget) : ptrint;
function  GTK_WIDGET_TOPLEVEL(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_NO_WINDOW(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_REALIZED(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_MAPPED(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_VISIBLE(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_DRAWABLE(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_SENSITIVE(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_PARENT_SENSITIVE(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_IS_SENSITIVE(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_CAN_FOCUS(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_HAS_FOCUS(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_CAN_DEFAULT(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_HAS_DEFAULT(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_HAS_GRAB(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_RC_STYLE(wid : PGtkwidget) : boolean;
function  GTK_WIDGET_COMPOSITE_CHILD(wid : PGtkWidget) : boolean;
function  GTK_WIDGET_APP_PAINTABLE(wid : PGtkWidget) : boolean;
function  GTK_WIDGET_RECEIVES_DEFAULT(wid : PGtkWidget) : boolean;
procedure GTK_WIDGET_SET_FLAGS(wid : PGtkwidget;flag:ptrint);
procedure GTK_WIDGET_UNSET_FLAGS(wid : PGtkwidget;flag:ptrint);

Type
  GTK_WIDGET=PGtkWidget;
  GTK_WIDGET_CLASS=PGtkWidgetClass;

function  GTK_WIDGET_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_widget_get_type';
function  GTK_IS_WIDGET(obj:pointer):boolean;
function  GTK_IS_WIDGET_CLASS(klass:pointer):boolean;

function  gtk_widget_get_type:TGtkType;cdecl;external gtkdll name 'gtk_widget_get_type';
function  gtk_widget_new(thetype:TGtkType; first_arg_name:Pgchar; args:array of const):PGtkWidget;cdecl;external gtkdll name 'gtk_widget_new';
function  gtk_widget_newv(thetype:TGtkType; nargs:guint; args:PGtkArg):PGtkWidget;cdecl;external gtkdll name 'gtk_widget_newv';
procedure gtk_widget_ref(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_ref';
procedure gtk_widget_unref(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_unref';
procedure gtk_widget_destroy(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_destroy';
procedure gtk_widget_destroyed(widget:PGtkWidget; widget_pointer:PPGtkWidget);cdecl;external gtkdll name 'gtk_widget_destroyed';
procedure gtk_widget_get(widget:PGtkWidget; arg:PGtkArg);cdecl;external gtkdll name 'gtk_widget_get';
procedure gtk_widget_getv(widget:PGtkWidget; nargs:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_widget_getv';
procedure gtk_widget_set(widget:PGtkWidget; first_arg_name:Pgchar; args:array of const);cdecl;external gtkdll name 'gtk_widget_set';
procedure gtk_widget_setv(widget:PGtkWidget; nargs:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_widget_setv';
procedure gtk_widget_unparent(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_unparent';
procedure gtk_widget_show(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_show';
procedure gtk_widget_show_now(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_show_now';
procedure gtk_widget_hide(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_hide';
procedure gtk_widget_show_all(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_show_all';
procedure gtk_widget_hide_all(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_hide_all';
procedure gtk_widget_map(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_map';
procedure gtk_widget_unmap(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_unmap';
procedure gtk_widget_realize(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_realize';
procedure gtk_widget_unrealize(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_unrealize';
procedure gtk_widget_queue_draw(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_queue_draw';
procedure gtk_widget_queue_draw_area(widget:PGtkWidget; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_widget_queue_draw_area';
procedure gtk_widget_queue_clear(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_queue_clear';
procedure gtk_widget_queue_clear_area(widget:PGtkWidget; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_widget_queue_clear_area';
procedure gtk_widget_queue_resize(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_queue_resize';
procedure gtk_widget_draw(widget:PGtkWidget; area:PGdkRectangle);cdecl;external gtkdll name 'gtk_widget_draw';
procedure gtk_widget_draw_focus(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_draw_focus';
procedure gtk_widget_draw_default(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_draw_default';
procedure gtk_widget_size_request(widget:PGtkWidget; requisition:PGtkRequisition);cdecl;external gtkdll name 'gtk_widget_size_request';
procedure gtk_widget_size_allocate(widget:PGtkWidget; allocation:PGtkAllocation);cdecl;external gtkdll name 'gtk_widget_size_allocate';
procedure gtk_widget_get_child_requisition(widget:PGtkWidget; requisition:PGtkRequisition);cdecl;external gtkdll name 'gtk_widget_get_child_requisition';
procedure gtk_widget_add_accelerator(widget:PGtkWidget; accel_signal:Pgchar; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:guint; accel_flags:TGtkAccelFlags);cdecl;external gtkdll name 'gtk_widget_add_accelerator';
procedure gtk_widget_remove_accelerator(widget:PGtkWidget; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:guint);cdecl;external gtkdll name 'gtk_widget_remove_accelerator';
procedure gtk_widget_remove_accelerators(widget:PGtkWidget; accel_signal:Pgchar; visible_only:gboolean);cdecl;external gtkdll name 'gtk_widget_remove_accelerators';
function  gtk_widget_accelerator_signal(widget:PGtkWidget; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:guint):guint;cdecl;external gtkdll name 'gtk_widget_accelerator_signal';
procedure gtk_widget_lock_accelerators(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_lock_accelerators';
procedure gtk_widget_unlock_accelerators(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_unlock_accelerators';
function  gtk_widget_accelerators_locked(widget:PGtkWidget):gboolean;cdecl;external gtkdll name 'gtk_widget_accelerators_locked';
function  gtk_widget_event(widget:PGtkWidget; event:PGdkEvent):gint;cdecl;external gtkdll name 'gtk_widget_event';
function  gtk_widget_activate(widget:PGtkWidget):gboolean;cdecl;external gtkdll name 'gtk_widget_activate';
function  gtk_widget_set_scroll_adjustments(widget:PGtkWidget; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):gboolean;cdecl;external gtkdll name 'gtk_widget_set_scroll_adjustments';
procedure gtk_widget_reparent(widget:PGtkWidget; new_parent:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_reparent';
procedure gtk_widget_popup(widget:PGtkWidget; x:gint; y:gint);cdecl;external gtkdll name 'gtk_widget_popup';
function  gtk_widget_intersect(widget:PGtkWidget; area:PGdkRectangle; intersection:PGdkRectangle):gint;cdecl;external gtkdll name 'gtk_widget_intersect';
procedure gtk_widget_grab_focus(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_grab_focus';
procedure gtk_widget_grab_default(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_grab_default';
procedure gtk_widget_set_name(widget:PGtkWidget; name:Pgchar);cdecl;external gtkdll name 'gtk_widget_set_name';
function  gtk_widget_get_name (widget:PGtkWidget):Pgchar;cdecl;external gtkdll name 'gtk_widget_get_name';
procedure gtk_widget_set_state(widget:PGtkWidget; state:TGtkStateType);cdecl;external gtkdll name 'gtk_widget_set_state';
procedure gtk_widget_set_sensitive(widget:PGtkWidget; sensitive:gboolean);cdecl;external gtkdll name 'gtk_widget_set_sensitive';
procedure gtk_widget_set_app_paintable(widget:PGtkWidget; app_paintable:gboolean);cdecl;external gtkdll name 'gtk_widget_set_app_paintable';
procedure gtk_widget_set_parent(widget:PGtkWidget; parent:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_set_parent';
procedure gtk_widget_set_parent_window(widget:PGtkWidget; parent_window:PGdkWindow);cdecl;external gtkdll name 'gtk_widget_set_parent_window';
function  gtk_widget_get_parent_window (widget:PGtkWidget):PGdkWindow;cdecl;external gtkdll name 'gtk_widget_get_parent_window';
procedure gtk_widget_set_uposition(widget:PGtkWidget; x:gint; y:gint);cdecl;external gtkdll name 'gtk_widget_set_uposition';
procedure gtk_widget_set_usize(widget:PGtkWidget; width:gint; height:gint);cdecl;external gtkdll name 'gtk_widget_set_usize';
procedure gtk_widget_set_events(widget:PGtkWidget; events:gint);cdecl;external gtkdll name 'gtk_widget_set_events';
procedure gtk_widget_add_events(widget:PGtkWidget; events:gint);cdecl;external gtkdll name 'gtk_widget_add_events';
procedure gtk_widget_set_extension_events(widget:PGtkWidget; mode:TGdkExtensionMode);cdecl;external gtkdll name 'gtk_widget_set_extension_events';
function  gtk_widget_get_extension_events(widget:PGtkWidget):TGdkExtensionMode;cdecl;external gtkdll name 'gtk_widget_get_extension_events';
function  gtk_widget_get_toplevel (widget:PGtkWidget):PGtkWidget;cdecl;external gtkdll name 'gtk_widget_get_toplevel';
function  gtk_widget_get_ancestor (widget:PGtkWidget; widget_thetype:TGtkType):PGtkWidget;cdecl;external gtkdll name 'gtk_widget_get_ancestor';
function  gtk_widget_get_colormap (widget:PGtkWidget):PGdkColormap;cdecl;external gtkdll name 'gtk_widget_get_colormap';
function  gtk_widget_get_visual (widget:PGtkWidget):PGdkVisual;cdecl;external gtkdll name 'gtk_widget_get_visual';
procedure gtk_widget_set_colormap(widget:PGtkWidget; colormap:PGdkColormap);cdecl;external gtkdll name 'gtk_widget_set_colormap';
procedure gtk_widget_set_visual(widget:PGtkWidget; visual:PGdkVisual);cdecl;external gtkdll name 'gtk_widget_set_visual';
function  gtk_widget_get_events(widget:PGtkWidget):gint;cdecl;external gtkdll name 'gtk_widget_get_events';
procedure gtk_widget_get_pointer(widget:PGtkWidget; x:Pgint; y:Pgint);cdecl;external gtkdll name 'gtk_widget_get_pointer';
function  gtk_widget_is_ancestor(widget:PGtkWidget; ancestor:PGtkWidget):gint;cdecl;external gtkdll name 'gtk_widget_is_ancestor';
function  gtk_widget_hide_on_delete(widget:PGtkWidget):gint;cdecl;external gtkdll name 'gtk_widget_hide_on_delete';
procedure gtk_widget_set_style(widget:PGtkWidget; style:PGtkStyle);cdecl;external gtkdll name 'gtk_widget_set_style';
procedure gtk_widget_set_rc_style(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_set_rc_style';
procedure gtk_widget_ensure_style(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_ensure_style';
function  gtk_widget_get_style (widget:PGtkWidget):PGtkStyle;cdecl;external gtkdll name 'gtk_widget_get_style';
procedure gtk_widget_restore_default_style(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_restore_default_style';
procedure gtk_widget_modify_style(widget:PGtkWidget; style:PGtkRcStyle);cdecl;external gtkdll name 'gtk_widget_modify_style';
procedure gtk_widget_set_composite_name(widget:PGtkWidget; name:Pgchar);cdecl;external gtkdll name 'gtk_widget_set_composite_name';
function  gtk_widget_get_composite_name(widget:PGtkWidget):Pgchar;cdecl;external gtkdll name 'gtk_widget_get_composite_name';
procedure gtk_widget_reset_rc_styles(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_widget_reset_rc_styles';
procedure gtk_widget_push_style(style:PGtkStyle);cdecl;external gtkdll name 'gtk_widget_push_style';
procedure gtk_widget_push_colormap(cmap:PGdkColormap);cdecl;external gtkdll name 'gtk_widget_push_colormap';
procedure gtk_widget_push_visual(visual:PGdkVisual);cdecl;external gtkdll name 'gtk_widget_push_visual';
procedure gtk_widget_push_composite_child;cdecl;external gtkdll name 'gtk_widget_push_composite_child';
procedure gtk_widget_pop_composite_child;cdecl;external gtkdll name 'gtk_widget_pop_composite_child';
procedure gtk_widget_pop_style;cdecl;external gtkdll name 'gtk_widget_pop_style';
procedure gtk_widget_pop_colormap;cdecl;external gtkdll name 'gtk_widget_pop_colormap';
procedure gtk_widget_pop_visual;cdecl;external gtkdll name 'gtk_widget_pop_visual';
procedure gtk_widget_set_default_style(style:PGtkStyle);cdecl;external gtkdll name 'gtk_widget_set_default_style';
procedure gtk_widget_set_default_colormap(colormap:PGdkColormap);cdecl;external gtkdll name 'gtk_widget_set_default_colormap';
procedure gtk_widget_set_default_visual(visual:PGdkVisual);cdecl;external gtkdll name 'gtk_widget_set_default_visual';
function  gtk_widget_get_default_style : PGtkStyle;cdecl;external gtkdll name 'gtk_widget_get_default_style';
function  gtk_widget_get_default_colormap : PGdkColormap;cdecl;external gtkdll name 'gtk_widget_get_default_colormap';
function  gtk_widget_get_default_visual : PGdkVisual;cdecl;external gtkdll name 'gtk_widget_get_default_visual';
procedure gtk_widget_shape_combine_mask(widget:PGtkWidget; shape_mask:PGdkBitmap; offset_x:gint; offset_y:gint);cdecl;external gtkdll name 'gtk_widget_shape_combine_mask';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_WIDGET_STATE(wid : PGtkwidget) : ptrint;
      begin
         GTK_WIDGET_STATE:=wid^.state;
      end;

function  GTK_WIDGET_SAVED_STATE(wid : PGtkwidget) : ptrint;
      begin
         GTK_WIDGET_SAVED_STATE:=wid^.saved_state;
      end;

function  GTK_WIDGET_FLAGS(wid : PGtkwidget) : ptrint;
      begin
         GTK_WIDGET_FLAGS:=PGtkobject(wid)^.flags;
      end;

function  GTK_WIDGET_TOPLEVEL(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_TOPLEVEL:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_TOPLEVEL)) <> 0;
      end;

function  GTK_WIDGET_NO_WINDOW(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_NO_WINDOW:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_NO_WINDOW)) <> 0;
      end;

function  GTK_WIDGET_REALIZED(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_REALIZED:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_REALIZED)) <> 0;
      end;

function  GTK_WIDGET_MAPPED(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_MAPPED:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_MAPPED)) <> 0;
      end;

function  GTK_WIDGET_VISIBLE(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_VISIBLE:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_VISIBLE)) <> 0;
      end;

function  GTK_WIDGET_DRAWABLE(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_DRAWABLE:=(GTK_WIDGET_VISIBLE(wid) and GTK_WIDGET_MAPPED(wid));
      end;

function  GTK_WIDGET_SENSITIVE(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_SENSITIVE:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_SENSITIVE)) <> 0;
      end;

function  GTK_WIDGET_PARENT_SENSITIVE(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_PARENT_SENSITIVE:=((GTK_WIDGET_FLAGS(wid)) and
                                        ptrint(GTK_PARENT_SENSITIVE)) <> 0;
      end;

function  GTK_WIDGET_IS_SENSITIVE(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_IS_SENSITIVE:=(GTK_WIDGET_SENSITIVE(wid) and
                                   GTK_WIDGET_PARENT_SENSITIVE(wid));
      end;

function  GTK_WIDGET_CAN_FOCUS(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_CAN_FOCUS:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_CAN_FOCUS)) <> 0;
      end;

function  GTK_WIDGET_HAS_FOCUS(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_HAS_FOCUS:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_HAS_FOCUS)) <> 0;
      end;

function  GTK_WIDGET_CAN_DEFAULT(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_CAN_DEFAULT:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_CAN_DEFAULT)) <> 0;
      end;

function  GTK_WIDGET_HAS_DEFAULT(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_HAS_DEFAULT:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_HAS_DEFAULT)) <> 0;
      end;

function  GTK_WIDGET_HAS_GRAB(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_HAS_GRAB:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_HAS_GRAB)) <> 0;
      end;

function  GTK_WIDGET_RC_STYLE(wid : PGtkwidget) : boolean;
      begin
         GTK_WIDGET_RC_STYLE:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_RC_STYLE)) <> 0;
      end;

function  GTK_WIDGET_COMPOSITE_CHILD(wid : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_COMPOSITE_CHILD:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_COMPOSITE_CHILD)) <> 0;
    end;

function  GTK_WIDGET_APP_PAINTABLE(wid : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_APP_PAINTABLE:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_APP_PAINTABLE)) <> 0;
    end;

function  GTK_WIDGET_RECEIVES_DEFAULT(wid : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_RECEIVES_DEFAULT:=((GTK_WIDGET_FLAGS(wid)) and ptrint(GTK_RECEIVES_DEFAULT)) <> 0;
    end;

procedure GTK_WIDGET_SET_FLAGS(wid : PGtkwidget;flag:ptrint);
begin
  PGtkobject(wid)^.flags:=PGtkobject(wid)^.flags or flag;
end;

procedure GTK_WIDGET_UNSET_FLAGS(wid : PGtkwidget;flag:ptrint);
begin
  PGtkobject(wid)^.flags:=PGtkobject(wid)^.flags and (not flag);
end;

function  GTK_IS_WIDGET(obj:pointer):boolean;
begin
  GTK_IS_WIDGET:=(obj<>nil) and GTK_IS_WIDGET_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_WIDGET_CLASS(klass:pointer):boolean;
begin
  GTK_IS_WIDGET_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_WIDGET_TYPE);
end;

{$endif read_implementation}


