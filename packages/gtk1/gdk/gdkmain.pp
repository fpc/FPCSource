{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

type
  PPGdkVisualType = ^PGdkVisualType;

procedure gdk_init(argc:Pgint; argv:pPPgchar);cdecl;external gdkdll name 'gdk_init';
function  gdk_init_check(argc:Pgint; argv:pPPgchar):gboolean;cdecl;external gdkdll name 'gdk_init_check';
procedure gdk_exit(error_code:gint);cdecl;external gdkdll name 'gdk_exit';
procedure gdk_error_trap_push;cdecl;external gdkdll name 'gdk_error_trap_push';
function  gdk_error_trap_pop:gint;cdecl;external gdkdll name 'gdk_error_trap_pop';
function  gdk_events_pending:gboolean;cdecl;external gdkdll name 'gdk_events_pending';
function  gdk_event_get:PGdkEvent;cdecl;external gdkdll name 'gdk_event_get';
function  gdk_event_peek:PGdkEvent;cdecl;external gdkdll name 'gdk_event_peek';
function  gdk_event_get_graphics_expose(window:PGdkWindow):PGdkEvent;cdecl;external gdkdll name 'gdk_event_get_graphics_expose';
procedure gdk_event_put(event:PGdkEvent);cdecl;external gdkdll name 'gdk_event_put';
function  gdk_event_copy(event:PGdkEvent):PGdkEvent;cdecl;external gdkdll name 'gdk_event_copy';
procedure gdk_event_free(event:PGdkEvent);cdecl;external gdkdll name 'gdk_event_free';
function  gdk_event_get_time(event:PGdkEvent):guint32;cdecl;external gdkdll name 'gdk_event_get_time';
procedure gdk_event_handler_set(func:TGdkEventFunc; data:gpointer; notify:TGDestroyNotify);cdecl;external gdkdll name 'gdk_event_handler_set';
procedure gdk_set_show_events(show_events:gint);cdecl;external gdkdll name 'gdk_set_show_events';
procedure gdk_set_use_xshm(use_xshm:gint);cdecl;external gdkdll name 'gdk_set_use_xshm';
function  gdk_get_show_events:gint;cdecl;external gdkdll name 'gdk_get_show_events';
function  gdk_get_use_xshm:gint;cdecl;external gdkdll name 'gdk_get_use_xshm';
function  gdk_get_display:Pgchar;cdecl;external gdkdll name 'gdk_get_display';
{$ifndef win32}
function  gdk_time_get:guint32;cdecl;external gdkdll name 'gdk_time_get';
function  gdk_timer_get:guint32;cdecl;external gdkdll name 'gdk_timer_get';
procedure gdk_timer_set(milliseconds:guint32);cdecl;external gdkdll name 'gdk_timer_set';
procedure gdk_timer_enable;cdecl;external gdkdll name 'gdk_timer_enable';
procedure gdk_timer_disable;cdecl;external gdkdll name 'gdk_timer_disable';
{$endif}
function  gdk_input_add_full(source:gint; condition:TGdkInputCondition; thefunction:TGdkInputfunction; data:gpointer; thedestroy:TGdkDestroyNotify):gint;cdecl;external gdkdll name 'gdk_input_add_full';
function  gdk_input_add(source:gint; condition:TGdkInputCondition; thefunction:TGdkInputfunction; data:gpointer):gint;cdecl;external gdkdll name 'gdk_input_add';
procedure gdk_input_remove(tag:gint);cdecl;external gdkdll name 'gdk_input_remove';
function  gdk_pointer_grab(window:PGdkWindow; owner_events:gint; event_mask:TGdkEventMask; confine_to:PGdkWindow; cursor:PGdkCursor; time:guint32):gint;cdecl;external gdkdll name 'gdk_pointer_grab';
procedure gdk_pointer_ungrab(time:guint32);cdecl;external gdkdll name 'gdk_pointer_ungrab';
function  gdk_keyboard_grab(window:PGdkWindow; owner_events:gint; time:guint32):gint;cdecl;external gdkdll name 'gdk_keyboard_grab';
procedure gdk_keyboard_ungrab(time:guint32);cdecl;external gdkdll name 'gdk_keyboard_ungrab';
function  gdk_pointer_is_grabbed:gint;cdecl;external gdkdll name 'gdk_pointer_is_grabbed';
function  gdk_screen_width:gint;cdecl;external gdkdll name 'gdk_screen_width';
function  gdk_screen_height:gint;cdecl;external gdkdll name 'gdk_screen_height';
function  gdk_screen_width_mm:gint;cdecl;external gdkdll name 'gdk_screen_width_mm';
function  gdk_screen_height_mm:gint;cdecl;external gdkdll name 'gdk_screen_height_mm';
procedure gdk_flush;cdecl;external gdkdll name 'gdk_flush';
procedure gdk_beep;cdecl;external gdkdll name 'gdk_beep';
procedure gdk_key_repeat_disable;cdecl;external gdkdll name 'gdk_key_repeat_disable';
procedure gdk_key_repeat_restore;cdecl;external gdkdll name 'gdk_key_repeat_restore';
function  gdk_visual_get_best_depth:gint;cdecl;external gdkdll name 'gdk_visual_get_best_depth';
function  gdk_visual_get_best_type:TGdkVisualType;cdecl;external gdkdll name 'gdk_visual_get_best_type';
function  gdk_visual_get_system:PGdkVisual;cdecl;external gdkdll name 'gdk_visual_get_system';
function  gdk_visual_get_best:PGdkVisual;cdecl;external gdkdll name 'gdk_visual_get_best';
function  gdk_visual_get_best_with_depth(depth:gint):PGdkVisual;cdecl;external gdkdll name 'gdk_visual_get_best_with_depth';
function  gdk_visual_get_best_with_type(visual_type:TGdkVisualType):PGdkVisual;cdecl;external gdkdll name 'gdk_visual_get_best_with_type';
function  gdk_visual_get_best_with_both(depth:gint; visual_type:TGdkVisualType):PGdkVisual;cdecl;external gdkdll name 'gdk_visual_get_best_with_both';
function  gdk_visual_ref(visual:PGdkVisual):PGdkVisual;cdecl;external gdkdll name 'gdk_visual_ref';
procedure gdk_visual_unref(visual:PGdkVisual);cdecl;external gdkdll name 'gdk_visual_unref';
procedure gdk_query_depths(depths:PPgint; count:Pgint);cdecl;external gdkdll name 'gdk_query_depths';
procedure gdk_query_visual_types(visual_types:PPGdkVisualType; count:Pgint);cdecl;external gdkdll name 'gdk_query_visual_types';
function  gdk_list_visuals:PGList;cdecl;external gdkdll name 'gdk_list_visuals';
function  gdk_window_new(parent:PGdkWindow; attributes:PGdkWindowAttr; attributes_mask:gint):PGdkWindow;cdecl;external gdkdll name 'gdk_window_new';
procedure gdk_window_destroy(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_destroy';
{$ifndef win32}
function  gdk_window_ref(window:PGdkWindow):PGdkWindow;cdecl;external gdkdll name 'gdk_window_ref';
procedure gdk_window_unref(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_unref';
{$else}
function  gdk_window_ref(window:PGdkWindow):PGdkWindow;cdecl;external gdkdll name 'gdk_drawable_ref';
procedure gdk_window_unref(window:PGdkWindow);cdecl;external gdkdll name 'gdk_drawable_unref';
{$endif}
function  gdk_window_at_pointer(win_x:Pgint; win_y:Pgint):PGdkWindow;cdecl;external gdkdll name 'gdk_window_at_pointer';
procedure gdk_window_show(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_show';
procedure gdk_window_hide(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_hide';
procedure gdk_window_withdraw(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_withdraw';
procedure gdk_window_move(window:PGdkWindow; x:gint; y:gint);cdecl;external gdkdll name 'gdk_window_move';
procedure gdk_window_resize(window:PGdkWindow; width:gint; height:gint);cdecl;external gdkdll name 'gdk_window_resize';
procedure gdk_window_move_resize(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_window_move_resize';
procedure gdk_window_reparent(window:PGdkWindow; new_parent:PGdkWindow; x:gint; y:gint);cdecl;external gdkdll name 'gdk_window_reparent';
procedure gdk_window_clear(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_clear';
procedure gdk_window_clear_area(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_window_clear_area';
procedure gdk_window_clear_area_e(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_window_clear_area_e';
{$ifndef win32}
procedure gdk_window_copy_area(window:PGdkWindow; gc:PGdkGC; x:gint; y:gint; source_window:PGdkWindow; source_x:gint; source_y:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_window_copy_area';
{$endif}
procedure gdk_window_raise(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_raise';
procedure gdk_window_lower(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_lower';
procedure gdk_window_set_user_data(window:PGdkWindow; user_data:gpointer);cdecl;external gdkdll name 'gdk_window_set_user_data';
procedure gdk_window_set_override_redirect(window:PGdkWindow; override_redirect:gboolean);cdecl;external gdkdll name 'gdk_window_set_override_redirect';
procedure gdk_window_add_filter(window:PGdkWindow; thefunction:TGdkFilterFunc; data:gpointer);cdecl;external gdkdll name 'gdk_window_add_filter';
procedure gdk_window_remove_filter(window:PGdkWindow; thefunction:TGdkFilterFunc; data:gpointer);cdecl;external gdkdll name 'gdk_window_remove_filter';
procedure gdk_window_shape_combine_mask(window:PGdkWindow; shape_mask:PGdkBitmap; offset_x:gint; offset_y:gint);cdecl;external gdkdll name 'gdk_window_shape_combine_mask';
procedure gdk_window_set_child_shapes(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_set_child_shapes';
procedure gdk_window_merge_child_shapes(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_merge_child_shapes';
function  gdk_window_is_visible(window:PGdkWindow):gboolean;cdecl;external gdkdll name 'gdk_window_is_visible';
function  gdk_window_is_viewable(window:PGdkWindow):gboolean;cdecl;external gdkdll name 'gdk_window_is_viewable';
function  gdk_window_set_static_gravities(window:PGdkWindow; use_static:gboolean):gboolean;cdecl;external gdkdll name 'gdk_window_set_static_gravities';
procedure gdk_add_client_message_filter(message_type:TGdkAtom; func:TGdkFilterFunc; data:gpointer);cdecl;external gdkdll name 'gdk_add_client_message_filter';
function  gdk_drag_context_new:PGdkDragContext;cdecl;external gdkdll name 'gdk_drag_context_new';
procedure gdk_drag_context_ref(context:PGdkDragContext);cdecl;external gdkdll name 'gdk_drag_context_ref';
procedure gdk_drag_context_unref(context:PGdkDragContext);cdecl;external gdkdll name 'gdk_drag_context_unref';
procedure gdk_drag_status(context:PGdkDragContext; action:TGdkDragAction; time:guint32);cdecl;external gdkdll name 'gdk_drag_status';
procedure gdk_drop_reply(context:PGdkDragContext; ok:gboolean; time:guint32);cdecl;external gdkdll name 'gdk_drop_reply';
procedure gdk_drop_finish(context:PGdkDragContext; success:gboolean; time:guint32);cdecl;external gdkdll name 'gdk_drop_finish';
function  gdk_drag_get_selection(context:PGdkDragContext):TGdkAtom;cdecl;external gdkdll name 'gdk_drag_get_selection';
function  gdk_drag_begin(window:PGdkWindow; targets:PGList):PGdkDragContext;cdecl;external gdkdll name 'gdk_drag_begin';
function  gdk_drag_get_protocol(xid:guint32; protocol:PGdkDragProtocol):guint32;cdecl;external gdkdll name 'gdk_drag_get_protocol';
procedure gdk_drag_find_window(context:PGdkDragContext; drag_window:PGdkWindow; x_root:gint; y_root:gint; dest_window:PPGdkWindow;protocol:PGdkDragProtocol);cdecl;external gdkdll name 'gdk_drag_find_window';
function  gdk_drag_motion(context:PGdkDragContext; dest_window:PGdkWindow; protocol:TGdkDragProtocol; x_root:gint; y_root:gint;suggested_action:TGdkDragAction; possible_actions:TGdkDragAction; time:guint32):gboolean;cdecl;
    external gdkdll name 'gdk_drag_motion';
procedure gdk_drag_abort(context:PGdkDragContext; time:guint32);cdecl;external gdkdll name 'gdk_drag_abort';
procedure gdk_window_set_hints(window:PGdkWindow; x:gint; y:gint; min_width:gint; min_height:gint; max_width:gint; max_height:gint; flags:gint);cdecl;external gdkdll name 'gdk_window_set_hints';
procedure gdk_window_set_geometry_hints(window:PGdkWindow; geometry:PGdkGeometry; flags:TGdkWindowHints);cdecl;external gdkdll name 'gdk_window_set_geometry_hints';
{$ifndef gtkwin}
procedure gdk_set_sm_client_id(sm_client_id:Pgchar);cdecl;external gdkdll name 'gdk_set_sm_client_id';
{$endif}
procedure gdk_window_set_title(window:PGdkWindow; title:Pgchar);cdecl;external gdkdll name 'gdk_window_set_title';
procedure gdk_window_set_role(window:PGdkWindow; role:Pgchar);cdecl;external gdkdll name 'gdk_window_set_role';
procedure gdk_window_set_transient_for(window:PGdkWindow; leader:PGdkWindow);cdecl;external gdkdll name 'gdk_window_set_transient_for';
procedure gdk_window_set_background(window:PGdkWindow; color:PGdkColor);cdecl;external gdkdll name 'gdk_window_set_background';
procedure gdk_window_set_back_pixmap(window:PGdkWindow; pixmap:PGdkPixmap; parent_relative:gint);cdecl;external gdkdll name 'gdk_window_set_back_pixmap';
procedure gdk_window_set_cursor(window:PGdkWindow; cursor:PGdkCursor);cdecl;external gdkdll name 'gdk_window_set_cursor';
{$ifdef win32}
{In gtk 1.3 these functions have been renamed/replaced.}
procedure gdk_window_set_colormap(window:PGdkWindow; colormap:PGdkColormap);cdecl;external gdkdll name 'gdk_drawable_set_colormap';
{$else}
procedure gdk_window_set_colormap(window:PGdkWindow; colormap:PGdkColormap);cdecl;external gdkdll name 'gdk_window_set_colormap';
{$endif}
procedure gdk_window_get_user_data(window:PGdkWindow; data:Pgpointer);cdecl;external gdkdll name 'gdk_window_get_user_data';
procedure gdk_window_get_geometry(window:PGdkWindow; x:Pgint; y:Pgint; width:Pgint; height:Pgint; depth:Pgint);cdecl;external gdkdll name 'gdk_window_get_geometry';
procedure gdk_window_get_position(window:PGdkWindow; x:Pgint; y:Pgint);cdecl;external gdkdll name 'gdk_window_get_position';
{$ifdef win32}
{In gtk 1.3 these functions have been renamed/replaced.}
procedure gdk_window_get_size(window:PGdkWindow; width:Pgint; height:Pgint);cdecl;external gdkdll name 'gdk_drawable_get_size';
function  gdk_window_get_visual(window:PGdkWindow):PGdkVisual;cdecl;external gdkdll name 'gdk_drawable_get_visual';
function  gdk_window_get_colormap(window:PGdkWindow):PGdkColormap;cdecl;external gdkdll name 'gdk_drawable_get_colormap';
function  gdk_window_get_type(window:PGdkWindow):TGdkWindowType;cdecl;external gdkdll name 'gdk_drawable_get_type';
{$else}
procedure gdk_window_get_size(window:PGdkWindow; width:Pgint; height:Pgint);cdecl;external gdkdll name 'gdk_window_get_size';
function  gdk_window_get_visual(window:PGdkWindow):PGdkVisual;cdecl;external gdkdll name 'gdk_window_get_visual';
function  gdk_window_get_colormap(window:PGdkWindow):PGdkColormap;cdecl;external gdkdll name 'gdk_window_get_colormap';
function  gdk_window_get_type(window:PGdkWindow):TGdkWindowType;cdecl;external gdkdll name 'gdk_window_get_type';
{$endif}
function  gdk_window_get_origin(window:PGdkWindow; x:Pgint; y:Pgint):gint;cdecl;external gdkdll name 'gdk_window_get_origin';
function  gdk_window_get_deskrelative_origin(window:PGdkWindow; x:Pgint; y:Pgint):gboolean;cdecl;external gdkdll name 'gdk_window_get_deskrelative_origin';
procedure gdk_window_get_root_origin(window:PGdkWindow; x:Pgint; y:Pgint);cdecl;external gdkdll name 'gdk_window_get_root_origin';
function  gdk_window_get_pointer(window:PGdkWindow; x:Pgint; y:Pgint; mask:PGdkModifierType):PGdkWindow;cdecl;external gdkdll name 'gdk_window_get_pointer';
function  gdk_window_get_parent(window:PGdkWindow):PGdkWindow;cdecl;external gdkdll name 'gdk_window_get_parent';
function  gdk_window_get_toplevel(window:PGdkWindow):PGdkWindow;cdecl;external gdkdll name 'gdk_window_get_toplevel';
function  gdk_window_get_children(window:PGdkWindow):PGList;cdecl;external gdkdll name 'gdk_window_get_children';
function  gdk_window_get_events(window:PGdkWindow):TGdkEventMask;cdecl;external gdkdll name 'gdk_window_get_events';
procedure gdk_window_set_events(window:PGdkWindow; event_mask:TGdkEventMask);cdecl;external gdkdll name 'gdk_window_set_events';
procedure gdk_window_set_icon(window:PGdkWindow; icon_window:PGdkWindow; pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;external gdkdll name 'gdk_window_set_icon';
procedure gdk_window_set_icon_name(window:PGdkWindow; name:Pgchar);cdecl;external gdkdll name 'gdk_window_set_icon_name';
procedure gdk_window_set_group(window:PGdkWindow; leader:PGdkWindow);cdecl;external gdkdll name 'gdk_window_set_group';
procedure gdk_window_set_decorations(window:PGdkWindow; decorations:TGdkWMDecoration);cdecl;external gdkdll name 'gdk_window_set_decorations';
procedure gdk_window_set_functions(window:PGdkWindow; functions:TGdkWMfunction);cdecl;external gdkdll name 'gdk_window_set_functions';
function  gdk_window_get_toplevels:PGList;cdecl;external gdkdll name 'gdk_window_get_toplevels';
procedure gdk_window_register_dnd(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_register_dnd';
procedure gdk_drawable_set_data(drawable:PGdkDrawable; key:Pgchar; data:gpointer; destroy_func:TGDestroyNotify);cdecl;external gdkdll name 'gdk_drawable_set_data';
function  gdk_cursor_new(cursor_type:TGdkCursorType):PGdkCursor;cdecl;external gdkdll name 'gdk_cursor_new';
function  gdk_cursor_new_from_pixmap(source:PGdkPixmap; mask:PGdkPixmap; fg:PGdkColor; bg:PGdkColor; x:gint; y:gint):PGdkCursor;cdecl;external gdkdll name 'gdk_cursor_new_from_pixmap';
{$ifdef win32}
{In gtk 1.3 these functions have been renamed/replaced.}
procedure gdk_cursor_destroy(cursor:PGdkCursor);cdecl;external gdkdll name 'gdk_cursor_unref';
{$else}
procedure gdk_cursor_destroy(cursor:PGdkCursor);cdecl;external gdkdll name 'gdk_cursor_destroy';
{$endif}
function  gdk_gc_new(window:PGdkWindow):PGdkGC;cdecl;external gdkdll name 'gdk_gc_new';
function  gdk_gc_new_with_values(window:PGdkWindow; values:PGdkGCValues; values_mask:TGdkGCValuesMask):PGdkGC;cdecl;external gdkdll name 'gdk_gc_new_with_values';
function  gdk_gc_ref(gc:PGdkGC):PGdkGC;cdecl;external gdkdll name 'gdk_gc_ref';
procedure gdk_gc_unref(gc:PGdkGC);cdecl;external gdkdll name 'gdk_gc_unref';
{$ifdef win32}
{In gtk 1.3 these functions have been renamed/replaced.}
procedure gdk_gc_destroy(gc:PGdkGC);cdecl;external gdkdll name 'gdk_gc_unref';
{$else}
procedure gdk_gc_destroy(gc:PGdkGC);cdecl;external gdkdll name 'gdk_gc_destroy';
{$endif}
procedure gdk_gc_get_values(gc:PGdkGC; values:PGdkGCValues);cdecl;external gdkdll name 'gdk_gc_get_values';
procedure gdk_gc_set_foreground(gc:PGdkGC; color:PGdkColor);cdecl;external gdkdll name 'gdk_gc_set_foreground';
procedure gdk_gc_set_background(gc:PGdkGC; color:PGdkColor);cdecl;external gdkdll name 'gdk_gc_set_background';
procedure gdk_gc_set_font(gc:PGdkGC; font:PGdkFont);cdecl;external gdkdll name 'gdk_gc_set_font';
procedure gdk_gc_set_function(gc:PGdkGC; thefunction:TGdkfunction);cdecl;external gdkdll name 'gdk_gc_set_function';
procedure gdk_gc_set_fill(gc:PGdkGC; fill:TGdkFill);cdecl;external gdkdll name 'gdk_gc_set_fill';
procedure gdk_gc_set_tile(gc:PGdkGC; tile:PGdkPixmap);cdecl;external gdkdll name 'gdk_gc_set_tile';
procedure gdk_gc_set_stipple(gc:PGdkGC; stipple:PGdkPixmap);cdecl;external gdkdll name 'gdk_gc_set_stipple';
procedure gdk_gc_set_ts_origin(gc:PGdkGC; x:gint; y:gint);cdecl;external gdkdll name 'gdk_gc_set_ts_origin';
procedure gdk_gc_set_clip_origin(gc:PGdkGC; x:gint; y:gint);cdecl;external gdkdll name 'gdk_gc_set_clip_origin';
procedure gdk_gc_set_clip_mask(gc:PGdkGC; mask:PGdkBitmap);cdecl;external gdkdll name 'gdk_gc_set_clip_mask';
procedure gdk_gc_set_clip_rectangle(gc:PGdkGC; rectangle:PGdkRectangle);cdecl;external gdkdll name 'gdk_gc_set_clip_rectangle';
procedure gdk_gc_set_clip_region(gc:PGdkGC; region:PGdkRegion);cdecl;external gdkdll name 'gdk_gc_set_clip_region';
procedure gdk_gc_set_subwindow(gc:PGdkGC; mode:TGdkSubwindowMode);cdecl;external gdkdll name 'gdk_gc_set_subwindow';
procedure gdk_gc_set_exposures(gc:PGdkGC; exposures:gint);cdecl;external gdkdll name 'gdk_gc_set_exposures';
procedure gdk_gc_set_line_attributes(gc:PGdkGC; line_width:gint; line_style:TGdkLineStyle; cap_style:TGdkCapStyle; join_style:TGdkJoinStyle);cdecl;external gdkdll name 'gdk_gc_set_line_attributes';
procedure gdk_gc_set_dashes(gc:PGdkGC;dash_offset:gint;dashlist:array of gint8;n:gint);cdecl;external gdkdll name 'gdk_gc_set_dashes';
procedure gdk_gc_copy(dst_gc:PGdkGC; src_gc:PGdkGC);cdecl;external gdkdll name 'gdk_gc_copy';
function  gdk_pixmap_new(window:PGdkWindow; width:gint; height:gint; depth:gint):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_new';
function  gdk_bitmap_create_from_data(window:PGdkWindow; data:Pgchar; width:gint; height:gint):PGdkBitmap;cdecl;external gdkdll name 'gdk_bitmap_create_from_data';
function  gdk_pixmap_create_from_data(window:PGdkWindow; data:Pgchar; width:gint; height:gint; depth:gint; fg:PGdkColor; bg:PGdkColor):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_create_from_data';
function  gdk_pixmap_create_from_xpm(window:PGdkWindow; mask:PPGdkBitmap; transparent_color:PGdkColor; filename:Pgchar):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_create_from_xpm';
function  gdk_pixmap_colormap_create_from_xpm(window:PGdkWindow; colormap:PGdkColormap; mask:PPGdkBitmap; transparent_color:PGdkColor; filename:Pgchar):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_colormap_create_from_xpm';
function  gdk_pixmap_create_from_xpm_d(window:PGdkWindow; mask:PPGdkBitmap; transparent_color:PGdkColor; data:PPgchar):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_create_from_xpm_d';
function  gdk_pixmap_colormap_create_from_xpm_d(window:PGdkWindow; colormap:PGdkColormap; mask:PPGdkBitmap; transparent_color:PGdkColor; data:PPgchar):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_colormap_create_from_xpm_d';
{$ifdef win32}
{In gtk 1.3 these functions have been renamed/replaced.}
function  gdk_pixmap_ref(pixmap:PGdkPixmap):PGdkPixmap;cdecl;external gdkdll name 'gdk_drawable_ref';
procedure gdk_pixmap_unref(pixmap:PGdkPixmap);cdecl;external gdkdll name 'gdk_drawable_unref';
function  gdk_bitmap_ref(pixmap:PGdkBitmap):PGdkBitmap;cdecl;external gdkdll name 'gdk_drawable_ref';
procedure gdk_bitmap_unref(pixmap:PGdkBitmap);cdecl;external gdkdll name 'gdk_drawable_unref';
{$else}
function  gdk_pixmap_ref(pixmap:PGdkPixmap):PGdkPixmap;cdecl;external gdkdll name 'gdk_pixmap_ref';
procedure gdk_pixmap_unref(pixmap:PGdkPixmap);cdecl;external gdkdll name 'gdk_pixmap_unref';
function  gdk_bitmap_ref(pixmap:PGdkBitmap):PGdkBitmap;cdecl;external gdkdll name 'gdk_bitmap_ref';
procedure gdk_bitmap_unref(pixmap:PGdkBitmap);cdecl;external gdkdll name 'gdk_bitmap_unref';
{$endif}
function  gdk_image_new_bitmap(visual:PGdkVisual; data:gpointer; width:gint; height:gint):PGdkImage;cdecl;external gdkdll name 'gdk_image_new_bitmap';
function  gdk_image_new(thetype:TGdkImageType; visual:PGdkVisual; width:gint; height:gint):PGdkImage;cdecl;external gdkdll name 'gdk_image_new';
function  gdk_image_get(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint):PGdkImage;cdecl;external gdkdll name 'gdk_image_get';
procedure gdk_image_put_pixel(image:PGdkImage; x:gint; y:gint; pixel:guint32);cdecl;external gdkdll name 'gdk_image_put_pixel';
function  gdk_image_get_pixel(image:PGdkImage; x:gint; y:gint):guint32;cdecl;external gdkdll name 'gdk_image_get_pixel';
{$ifdef win32}
{In gtk 1.3 these functions have been renamed/replaced.}
procedure gdk_image_destroy(image:PGdkImage);cdecl;external gdkdll name 'gdk_image_unref';
{$else}
procedure gdk_image_destroy(image:PGdkImage);cdecl;external gdkdll name 'gdk_image_destroy';
{$endif}
function  gdk_colormap_new(visual:PGdkVisual; allocate:gint):PGdkColormap;cdecl;external gdkdll name 'gdk_colormap_new';
function  gdk_colormap_ref(cmap:PGdkColormap):PGdkColormap;cdecl;external gdkdll name 'gdk_colormap_ref';
procedure gdk_colormap_unref(cmap:PGdkColormap);cdecl;external gdkdll name 'gdk_colormap_unref';
function  gdk_colormap_get_system:PGdkColormap;cdecl;external gdkdll name 'gdk_colormap_get_system';
function  gdk_colormap_get_system_size:gint;cdecl;external gdkdll name 'gdk_colormap_get_system_size';
procedure gdk_colormap_change(colormap:PGdkColormap; ncolors:gint);cdecl;external gdkdll name 'gdk_colormap_change';
function  gdk_colormap_alloc_colors(colormap:PGdkColormap; colors:PGdkColor; ncolors:gint; writeable:gboolean; best_match:gboolean; success:Pgboolean):gint;cdecl;external gdkdll name 'gdk_colormap_alloc_colors';
function  gdk_colormap_alloc_color(colormap:PGdkColormap; color:PGdkColor; writeable:gboolean; best_match:gboolean):gboolean;cdecl;external gdkdll name 'gdk_colormap_alloc_color';
procedure gdk_colormap_free_colors(colormap:PGdkColormap; colors:PGdkColor; ncolors:gint);cdecl;external gdkdll name 'gdk_colormap_free_colors';
function  gdk_colormap_get_visual(colormap:PGdkColormap):PGdkVisual;cdecl;external gdkdll name 'gdk_colormap_get_visual';
function  gdk_color_copy(color:PGdkColor):PGdkColor;cdecl;external gdkdll name 'gdk_color_copy';
procedure gdk_color_free(color:PGdkColor);cdecl;external gdkdll name 'gdk_color_free';
function  gdk_color_parse(spec:Pgchar; color:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_parse';
function  gdk_color_hash(colora:PGdkColor; colorb:PGdkColor):guint;cdecl;external gdkdll name 'gdk_color_hash';
function  gdk_color_equal(colora:PGdkColor; colorb:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_equal';
procedure gdk_colors_store(colormap:PGdkColormap; colors:PGdkColor; ncolors:gint);cdecl;external gdkdll name 'gdk_colors_store';
function  gdk_colors_alloc(colormap:PGdkColormap; contiguous:gint; planes:Pgulong; nplanes:gint; pixels:Pgulong; npixels:gint):gint;cdecl;external gdkdll name 'gdk_colors_alloc';
procedure gdk_colors_free(colormap:PGdkColormap; pixels:Pgulong; npixels:gint; planes:gulong);cdecl;external gdkdll name 'gdk_colors_free';
function  gdk_color_white(colormap:PGdkColormap; color:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_white';
function  gdk_color_black(colormap:PGdkColormap; color:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_black';
function  gdk_color_alloc(colormap:PGdkColormap; color:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_alloc';
function  gdk_color_change(colormap:PGdkColormap; color:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_change';
function  gdk_font_load(font_name:Pgchar):PGdkFont;cdecl;external gdkdll name 'gdk_font_load';
function  gdk_fontset_load(fontset_name:Pgchar):PGdkFont;cdecl;external gdkdll name 'gdk_fontset_load';
function  gdk_font_ref(font:PGdkFont):PGdkFont;cdecl;external gdkdll name 'gdk_font_ref';
procedure gdk_font_unref(font:PGdkFont);cdecl;external gdkdll name 'gdk_font_unref';
function  gdk_font_id(font:PGdkFont):gint;cdecl;external gdkdll name 'gdk_font_id';
function  gdk_font_equal(fonta:PGdkFont; fontb:PGdkFont):gint;cdecl;external gdkdll name 'gdk_font_equal';
function  gdk_string_width(font:PGdkFont; thestring:Pgchar):gint;cdecl;external gdkdll name 'gdk_string_width';
function  gdk_text_width(font:PGdkFont; text:Pgchar; text_length:gint):gint;cdecl;external gdkdll name 'gdk_text_width';
function  gdk_text_width_wc(font:PGdkFont; text:PGdkWChar; text_length:gint):gint;cdecl;external gdkdll name 'gdk_text_width_wc';
function  gdk_char_width(font:PGdkFont; character:gchar):gint;cdecl;external gdkdll name 'gdk_char_width';
function  gdk_char_width_wc(font:PGdkFont; character:TGdkWChar):gint;cdecl;external gdkdll name 'gdk_char_width_wc';
function  gdk_string_measure(font:PGdkFont; thestring:Pgchar):gint;cdecl;external gdkdll name 'gdk_string_measure';
function  gdk_text_measure(font:PGdkFont; text:Pgchar; text_length:gint):gint;cdecl;external gdkdll name 'gdk_text_measure';
function  gdk_char_measure(font:PGdkFont; character:gchar):gint;cdecl;external gdkdll name 'gdk_char_measure';
function  gdk_string_height(font:PGdkFont; thestring:Pgchar):gint;cdecl;external gdkdll name 'gdk_string_height';
function  gdk_text_height(font:PGdkFont; text:Pgchar; text_length:gint):gint;cdecl;external gdkdll name 'gdk_text_height';
function  gdk_char_height(font:PGdkFont; character:gchar):gint;cdecl;external gdkdll name 'gdk_char_height';
procedure gdk_text_extents(font:PGdkFont; text:Pgchar; text_length:gint; lbearing:Pgint; rbearing:Pgint; width:Pgint; ascent:Pgint; descent:Pgint);cdecl;external gdkdll name 'gdk_text_extents';
procedure gdk_text_extents_wc(font:PGdkFont; text:PGdkWChar; text_length:gint; lbearing:Pgint; rbearing:Pgint; width:Pgint; ascent:Pgint; descent:Pgint);cdecl;external gdkdll name 'gdk_text_extents_wc';
procedure gdk_string_extents(font:PGdkFont; thestring:Pgchar; lbearing:Pgint; rbearing:Pgint; width:Pgint; ascent:Pgint; descent:Pgint);cdecl;external gdkdll name 'gdk_string_extents';
procedure gdk_draw_point(drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint);cdecl;external gdkdll name 'gdk_draw_point';
procedure gdk_draw_line(drawable:PGdkDrawable; gc:PGdkGC; x1:gint; y1:gint; x2:gint; y2:gint);cdecl;external gdkdll name 'gdk_draw_line';
procedure gdk_draw_rectangle(drawable:PGdkDrawable; gc:PGdkGC; filled:gint; x:gint; y:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_draw_rectangle';
procedure gdk_draw_arc(drawable:PGdkDrawable; gc:PGdkGC; filled:gint; x:gint; y:gint; width:gint; height:gint; angle1:gint; angle2:gint);cdecl;external gdkdll name 'gdk_draw_arc';
procedure gdk_draw_polygon(drawable:PGdkDrawable; gc:PGdkGC; filled:gint; points:PGdkPoint; npoints:gint);cdecl;external gdkdll name 'gdk_draw_polygon';
procedure gdk_draw_string(drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint; thestring:Pgchar);cdecl;external gdkdll name 'gdk_draw_string';
procedure gdk_draw_text(drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint; text:Pgchar; text_length:gint);cdecl;external gdkdll name 'gdk_draw_text';
procedure gdk_draw_text_wc(drawable:PGdkDrawable; font:PGdkFont; gc:PGdkGC; x:gint; y:gint; text:PGdkWChar; text_length:gint);cdecl;external gdkdll name 'gdk_draw_text_wc';
{$ifdef gtkwin}
{In gtk 1.3 these functions have been renamed/replaced.}
procedure gdk_draw_pixmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_draw_drawable';
procedure gdk_draw_bitmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_draw_drawable';
{$else}
procedure gdk_draw_pixmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_draw_pixmap';
{$ifdef dummy}
{ this routine doesn't exist in gdk 1.2, it's an error in the headers }
procedure gdk_draw_bitmap(drawable:PGdkDrawable; gc:PGdkGC; src:PGdkDrawable; xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_draw_bitmap';
{$endif dummy}
{$endif}
procedure gdk_draw_image(drawable:PGdkDrawable; gc:PGdkGC; image:PGdkImage; xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);cdecl;external gdkdll name 'gdk_draw_image';
procedure gdk_draw_points(drawable:PGdkDrawable; gc:PGdkGC; points:PGdkPoint; npoints:gint);cdecl;external gdkdll name 'gdk_draw_points';
procedure gdk_draw_segments(drawable:PGdkDrawable; gc:PGdkGC; segs:PGdkSegment; nsegs:gint);cdecl;external gdkdll name 'gdk_draw_segments';
procedure gdk_draw_lines(drawable:PGdkDrawable; gc:PGdkGC; points:PGdkPoint; npoints:gint);cdecl;external gdkdll name 'gdk_draw_lines';
function  gdk_selection_owner_set(owner:PGdkWindow; selection:TGdkAtom; time:guint32; send_event:gint):gint;cdecl;external gdkdll name 'gdk_selection_owner_set';
function  gdk_selection_owner_get(selection:TGdkAtom):PGdkWindow;cdecl;external gdkdll name 'gdk_selection_owner_get';
procedure gdk_selection_convert(requestor:PGdkWindow; selection:TGdkAtom; target:TGdkAtom; time:guint32);cdecl;external gdkdll name 'gdk_selection_convert';
function  gdk_selection_property_get(requestor:PGdkWindow; data:PPguchar; prop_type:PTGdkAtom; prop_format:Pgint):gint;cdecl;external gdkdll name 'gdk_selection_property_get';
procedure gdk_selection_send_notify(requestor:guint32; selection:TGdkAtom; target:TGdkAtom; theproperty:TGdkAtom; time:guint32);cdecl;external gdkdll name 'gdk_selection_send_notify';
function  gdk_text_property_to_text_list(encoding:TGdkAtom; format:gint; text:Pguchar; length:gint; list:PPPgchar):gint;cdecl;external gdkdll name 'gdk_text_property_to_text_list';
procedure gdk_free_text_list(list:PPgchar);cdecl;external gdkdll name 'gdk_free_text_list';
function  gdk_string_to_compound_text(str:Pgchar; encoding:PTGdkAtom; format:Pgint; ctext:PPguchar; length:Pgint):gint;cdecl;external gdkdll name 'gdk_string_to_compound_text';
procedure gdk_free_compound_text(ctext:Pguchar);cdecl;external gdkdll name 'gdk_free_compound_text';
function  gdk_atom_intern(atom_name:Pgchar; only_if_exists:gint):TGdkAtom;cdecl;external gdkdll name 'gdk_atom_intern';
function  gdk_atom_name(atom:TGdkAtom):Pgchar;cdecl;external gdkdll name 'gdk_atom_name';
function  gdk_property_get(window:PGdkWindow; theproperty:TGdkAtom; thetype:TGdkAtom; offset:gulong; length:gulong; pdelete:gint; actual_property_type:PTGdkAtom; actual_format:Pgint; actual_length:Pgint; data:PPguchar):gint;cdecl;
   external gdkdll name 'gdk_property_get';
procedure gdk_property_change(window:PGdkWindow; theproperty:TGdkAtom; thetype:TGdkAtom; format:gint; mode:TGdkPropMode; data:Pguchar; nelements:gint);cdecl;external gdkdll name 'gdk_property_change';
procedure gdk_property_delete(window:PGdkWindow; theproperty:TGdkAtom);cdecl;external gdkdll name 'gdk_property_delete';
function  gdk_rectangle_intersect(src1:PGdkRectangle; src2:PGdkRectangle; dest:PGdkRectangle):gint;cdecl;external gdkdll name 'gdk_rectangle_intersect';
procedure gdk_rectangle_union(src1:PGdkRectangle; src2:PGdkRectangle; dest:PGdkRectangle);cdecl;external gdkdll name 'gdk_rectangle_union';
procedure gdk_input_init;cdecl;external gdkdll name 'gdk_input_init';
procedure gdk_input_exit;cdecl;external gdkdll name 'gdk_input_exit';
function  gdk_input_list_devices:PGList;cdecl;external gdkdll name 'gdk_input_list_devices';
procedure gdk_input_set_extension_events(window:PGdkWindow; mask:gint; mode:TGdkExtensionMode);cdecl;external gdkdll name 'gdk_input_set_extension_events';
procedure gdk_input_set_source(deviceid:guint32; source:TGdkInputSource);cdecl;external gdkdll name 'gdk_input_set_source';
function  gdk_input_set_mode(deviceid:guint32; mode:TGdkInputMode):gint;cdecl;external gdkdll name 'gdk_input_set_mode';
procedure gdk_input_set_axes(deviceid:guint32; axes:PGdkAxisUse);cdecl;external gdkdll name 'gdk_input_set_axes';
procedure gdk_input_set_key(deviceid:guint32; index:guint; keyval:guint; modifiers:TGdkModifierType);cdecl;external gdkdll name 'gdk_input_set_key';
procedure gdk_input_window_get_pointer(window:PGdkWindow; deviceid:guint32; x:Pgdouble; y:Pgdouble; pressure:Pgdouble; xtilt:Pgdouble; ytilt:Pgdouble; mask:PGdkModifierType);cdecl;external gdkdll name 'gdk_input_window_get_pointer';
function  gdk_input_motion_events(window:PGdkWindow; deviceid:guint32; start:guint32; stop:guint32; nevents_return:Pgint):PGdkTimeCoord;cdecl;external gdkdll name 'gdk_input_motion_events';
function  gdk_im_ready:gint;cdecl;external gdkdll name 'gdk_im_ready';
procedure gdk_im_begin(ic:TGdkIC; window:PGdkWindow);cdecl;external gdkdll name 'gdk_im_begin';
procedure gdk_im_end;cdecl;external gdkdll name 'gdk_im_end';
function  gdk_im_decide_style(supported_style:TGdkIMStyle):TGdkIMStyle;cdecl;external gdkdll name 'gdk_im_decide_style';
function  gdk_im_set_best_style(best_allowed_style:TGdkIMStyle):TGdkIMStyle;cdecl;external gdkdll name 'gdk_im_set_best_style';
function  gdk_ic_new(attr:PGdkICAttr; mask:TGdkICAttributesType):TGdkIC;cdecl;external gdkdll name 'gdk_ic_new';
procedure gdk_ic_destroy(ic:TGdkIC);cdecl;external gdkdll name 'gdk_ic_destroy';
function  gdk_ic_get_style(ic:TGdkIC):TGdkIMStyle;cdecl;external gdkdll name 'gdk_ic_get_style';
function  gdk_ic_get_events(ic:TGdkIC):TGdkEventMask;cdecl;external gdkdll name 'gdk_ic_get_events';
function  gdk_ic_attr_new:PGdkICAttr;cdecl;external gdkdll name 'gdk_ic_attr_new';
procedure gdk_ic_attr_destroy(attr:PGdkICAttr);cdecl;external gdkdll name 'gdk_ic_attr_destroy';
function  gdk_ic_set_attr(ic:TGdkIC; attr:PGdkICAttr; mask:TGdkICAttributesType):TGdkICAttributesType;cdecl;external gdkdll name 'gdk_ic_set_attr';
function  gdk_ic_get_attr(ic:TGdkIC; attr:PGdkICAttr; mask:TGdkICAttributesType):TGdkICAttributesType;cdecl;external gdkdll name 'gdk_ic_get_attr';
function  gdk_wcstombs(src:PGdkWChar):Pgchar;cdecl;external gdkdll name 'gdk_wcstombs';
function  gdk_mbstowcs(dest:PGdkWChar; src:Pgchar; dest_max:gint):gint;cdecl;external gdkdll name 'gdk_mbstowcs';
function  gdk_color_context_new(visual:PGdkVisual; colormap:PGdkColormap):PGdkColorContext;cdecl;external gdkdll name 'gdk_color_context_new';
function  gdk_color_context_new_mono(visual:PGdkVisual; colormap:PGdkColormap):PGdkColorContext;cdecl;external gdkdll name 'gdk_color_context_new_mono';
procedure gdk_color_context_free(cc:PGdkColorContext);cdecl;external gdkdll name 'gdk_color_context_free';
function  gdk_color_context_get_pixel(cc:PGdkColorContext; red:gushort; green:gushort; blue:gushort; failed:Pgint):gulong;cdecl;external gdkdll name 'gdk_color_context_get_pixel';
procedure gdk_color_context_get_pixels(cc:PGdkColorContext; reds:Pgushort; greens:Pgushort; blues:Pgushort; ncolors:gint; colors:Pgulong; nallocated:Pgint);cdecl;external gdkdll name 'gdk_color_context_get_pixels';
procedure gdk_color_context_get_pixels_incremental(cc:PGdkColorContext; reds:Pgushort; greens:Pgushort; blues:Pgushort; ncolors:gint; used:Pgint; colors:Pgulong; nallocated:Pgint);cdecl;external gdkdll name 'gdk_color_context_get_pixels_incremental';
function  gdk_color_context_query_color(cc:PGdkColorContext; color:PGdkColor):gint;cdecl;external gdkdll name 'gdk_color_context_query_color';
function  gdk_color_context_query_colors(cc:PGdkColorContext; colors:PGdkColor; num_colors:gint):gint;cdecl;external gdkdll name 'gdk_color_context_query_colors';
function  gdk_color_context_add_palette(cc:PGdkColorContext; palette:PGdkColor; num_palette:gint):gint;cdecl;external gdkdll name 'gdk_color_context_add_palette';
procedure gdk_color_context_init_dither(cc:PGdkColorContext);cdecl;external gdkdll name 'gdk_color_context_init_dither';
procedure gdk_color_context_free_dither(cc:PGdkColorContext);cdecl;external gdkdll name 'gdk_color_context_free_dither';
function  gdk_color_context_get_pixel_from_palette(cc:PGdkColorContext; red:Pgushort; green:Pgushort; blue:Pgushort; failed:Pgint):gulong;cdecl;external gdkdll name 'gdk_color_context_get_pixel_from_palette';
function  gdk_color_context_get_index_from_palette(cc:PGdkColorContext; red:Pgint; green:Pgint; blue:Pgint; failed:Pgint):guchar;cdecl;external gdkdll name 'gdk_color_context_get_index_from_palette';
function  gdk_region_new:PGdkRegion;cdecl;external gdkdll name 'gdk_region_new';
procedure gdk_region_destroy(region:PGdkRegion);cdecl;external gdkdll name 'gdk_region_destroy';
procedure gdk_region_get_clipbox(region:PGdkRegion; rectangle:PGdkRectangle);cdecl;external gdkdll name 'gdk_region_get_clipbox';
function  gdk_region_empty(region:PGdkRegion):gboolean;cdecl;external gdkdll name 'gdk_region_empty';
function  gdk_region_equal(region1:PGdkRegion; region2:PGdkRegion):gboolean;cdecl;external gdkdll name 'gdk_region_equal';
function  gdk_region_point_in(region:PGdkRegion; x:longint; y:longint):gboolean;cdecl;external gdkdll name 'gdk_region_point_in';
function  gdk_region_rect_in(region:PGdkRegion; rect:PGdkRectangle):TGdkOverlapType;cdecl;external gdkdll name 'gdk_region_rect_in';
function  gdk_region_polygon(points:PGdkPoint; npoints:gint; fill_rule:TGdkFillRule):PGdkRegion;cdecl;external gdkdll name 'gdk_region_polygon';
procedure gdk_region_offset(region:PGdkRegion; dx:gint; dy:gint);cdecl;external gdkdll name 'gdk_region_offset';
procedure gdk_region_shrink(region:PGdkRegion; dx:gint; dy:gint);cdecl;external gdkdll name 'gdk_region_shrink';
function  gdk_region_union_with_rect(region:PGdkRegion; rect:PGdkRectangle):PGdkRegion;cdecl;external gdkdll name 'gdk_region_union_with_rect';
function  gdk_regions_intersect(source1:PGdkRegion; source2:PGdkRegion):PGdkRegion;cdecl;external gdkdll name 'gdk_regions_intersect';
function  gdk_regions_union(source1:PGdkRegion; source2:PGdkRegion):PGdkRegion;cdecl;external gdkdll name 'gdk_regions_union';
function  gdk_regions_subtract(source1:PGdkRegion; source2:PGdkRegion):PGdkRegion;cdecl;external gdkdll name 'gdk_regions_subtract';
function  gdk_regions_xor(source1:PGdkRegion; source2:PGdkRegion):PGdkRegion;cdecl;external gdkdll name 'gdk_regions_xor';
procedure gdk_event_send_clientmessage_toall(event:PGdkEvent);cdecl;external gdkdll name 'gdk_event_send_clientmessage_toall';
function  gdk_event_send_client_message(event:PGdkEvent; xid:guint32):gboolean;cdecl;external gdkdll name 'gdk_event_send_client_message';
function  gdk_keyval_name(keyval:guint):Pgchar;cdecl;external gdkdll name 'gdk_keyval_name';
function  gdk_keyval_from_name(keyval_name:Pgchar):guint;cdecl;external gdkdll name 'gdk_keyval_from_name';
function  gdk_keyval_to_upper(keyval:guint):guint;cdecl;external gdkdll name 'gdk_keyval_to_upper';
function  gdk_keyval_to_lower(keyval:guint):guint;cdecl;external gdkdll name 'gdk_keyval_to_lower';
function  gdk_keyval_is_upper(keyval:guint):gboolean;cdecl;external gdkdll name 'gdk_keyval_is_upper';
function  gdk_keyval_is_lower(keyval:guint):gboolean;cdecl;external gdkdll name 'gdk_keyval_is_lower';
{$ifndef gtkos2}
var
  gdk_threads_mutex : PGMutex;external gdkdll name 'gdk_threads_mutex';
{$endif}
procedure gdk_threads_enter;cdecl;external gdkdll name 'gdk_threads_enter';
procedure gdk_threads_leave;cdecl;external gdkdll name 'gdk_threads_leave';

{$endif read_interface}

{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}

