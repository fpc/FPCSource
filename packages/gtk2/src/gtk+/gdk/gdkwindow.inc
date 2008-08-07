// included by gdk2.pp
{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Classes of windows.
     InputOutput: Almost every window should be of this type. Such windows
                  receive events and are also displayed on screen.
     InputOnly: Used only in special circumstances when events need to be
                stolen from another window or windows. Input only windows
                have no visible output, so they are handy for placing over
                top of a group of windows in order to grab the events (or
                filter the events) from those windows.
  }
   PGdkWindowClass = ^TGdkWindowClass;
   TGdkWindowClass = (
     GDK_INPUT_OUTPUT,
     GDK_INPUT_ONLY
   );

{ Types of windows.
     Root: There is only 1 root window and it is initialized
           at startup. Creating a window of type GDK_WINDOW_ROOT
           is an error.
     Toplevel: Windows which interact with the window manager.
     Child: Windows which are children of some other type of window.
            (Any other type of window). Most windows are child windows.
     Dialog: A special kind of toplevel window which interacts with
             the window manager slightly differently than a regular
             toplevel window. Dialog windows should be used for any
             transient window.
     Foreign: A window that actually belongs to another application
  }
   PGdkWindowType = ^TGdkWindowType;
   TGdkWindowType = (
     GDK_WINDOW_ROOT,
     GDK_WINDOW_TOPLEVEL,
     GDK_WINDOW_CHILD,
     GDK_WINDOW_DIALOG,
     GDK_WINDOW_TEMP,
     GDK_WINDOW_FOREIGN
   );

{ Window attribute mask values.
     GDK_WA_TITLE: The "title" field is valid.
     GDK_WA_X: The "x" field is valid.
     GDK_WA_Y: The "y" field is valid.
     GDK_WA_CURSOR: The "cursor" field is valid.
     GDK_WA_COLORMAP: The "colormap" field is valid.
     GDK_WA_VISUAL: The "visual" field is valid.
  }
   PGdkWindowAttributesType = ^TGdkWindowAttributesType;
   TGdkWindowAttributesType = longint; // see below for possible values

{ Size restriction enumeration. }

   PGdkWindowHints = ^TGdkWindowHints;
   TGdkWindowHints = longint; // see below for possible values

{ Window type hints.
   These are hints for the window manager that indicate
   what type of function the window has. The window manager
   can use this when determining decoration and behaviour
   of the window. The hint must be set before mapping the
   window.

     Normal: Normal toplevel window
     Dialog: Dialog window
     Menu: Window used to implement a menu.
     Toolbar: Window used to implement toolbars.
  }
   PGdkWindowTypeHint = ^TGdkWindowTypeHint;
   TGdkWindowTypeHint = (
     GDK_WINDOW_TYPE_HINT_NORMAL,
     GDK_WINDOW_TYPE_HINT_DIALOG,
     GDK_WINDOW_TYPE_HINT_MENU,           // Torn off menu
     GDK_WINDOW_TYPE_HINT_TOOLBAR,
     GDK_WINDOW_TYPE_HINT_SPLASHSCREEN,
     GDK_WINDOW_TYPE_HINT_UTILITY,
     GDK_WINDOW_TYPE_HINT_DOCK,
     GDK_WINDOW_TYPE_HINT_DESKTOP,
     GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU, // A drop down menu (from a menubar)
     GDK_WINDOW_TYPE_HINT_POPUP_MENU,    // A popup menu (from right-click)
     GDK_WINDOW_TYPE_HINT_TOOLTIP,
     GDK_WINDOW_TYPE_HINT_NOTIFICATION,
     GDK_WINDOW_TYPE_HINT_COMBO,
     GDK_WINDOW_TYPE_HINT_DND
   );

{ The next two enumeration values current match the
   Motif constants. If this is changed, the implementation
   of gdk_window_set_decorations/gdk_window_set_functions
   will need to change as well.
  }

   PGdkWMDecoration = ^TGdkWMDecoration;
   TGdkWMDecoration =  longint; // see below for possible values

   PGdkWMFunction = ^TGdkWMFunction;
   TGdkWMFunction =  longint; // see below for possible values

{ Currently, these are the same values numerically as in the
   X protocol. If you change that, gdkwindow-x11.c/gdk_window_set_geometry_hints()
   will need fixing.
  }
   PGdkGravity = ^TGdkGravity;
   TGdkGravity = integer; // see below for possible values

   PGdkWindowEdge = ^TGdkWindowEdge;
   TGdkWindowEdge = (
     GDK_WINDOW_EDGE_NORTH_WEST,
     GDK_WINDOW_EDGE_NORTH,
     GDK_WINDOW_EDGE_NORTH_EAST,
     GDK_WINDOW_EDGE_WEST,
     GDK_WINDOW_EDGE_EAST,
     GDK_WINDOW_EDGE_SOUTH_WEST,
     GDK_WINDOW_EDGE_SOUTH,
     GDK_WINDOW_EDGE_SOUTH_EAST
   );

   PGdkWindowAttr = ^TGdkWindowAttr;
   TGdkWindowAttr = record
        title : Pgchar;
        event_mask : gint;
        x : gint;
        y : gint;
        width : gint;
        height : gint;
        wclass : TGdkWindowClass;
        visual : PGdkVisual;
        colormap : PGdkColormap;
        window_type : TGdkWindowType;
        cursor : PGdkCursor;
        wmclass_name : Pgchar;
        wmclass_class : Pgchar;
        override_redirect : gboolean;
     end;

   PGdkGeometry = ^TGdkGeometry;
   TGdkGeometry = record
        min_width : gint;
        min_height : gint;
        max_width : gint;
        max_height : gint;
        base_width : gint;
        base_height : gint;
        width_inc : gint;
        height_inc : gint;
        min_aspect : gdouble;
        max_aspect : gdouble;
        win_gravity : TGdkGravity;
     end;

{ unused for now }
   PGdkPointerHooks = ^TGdkPointerHooks;
   TGdkPointerHooks = record
        get_pointer : function (window:PGdkWindow; x:Pgint; y:Pgint; mask:PGdkModifierType):PGdkWindow; cdecl;
        window_at_pointer : function (screen:PGdkScreen; win_x:Pgint; win_y:Pgint):PGdkWindow; cdecl;
     end;

{ window-system-specific delegate anObject  }
   PGdkWindowObject = ^TGdkWindowObject;
   TGdkWindowObject = record
        parent_instance : TGdkDrawable;
        impl : PGdkDrawable;
        parent : PGdkWindowObject;
        user_data : gpointer;
        x : gint;
        y : gint;
        extension_events : gint;
        filters : PGList;
        children : PGList;
        bg_color : TGdkColor;
        bg_pixmap : PGdkPixmap;
        paint_stack : PGSList;
        update_area : PGdkRegion;
        update_freeze_count : guint;
        window_type : guint8;
        depth : guint8;
        resize_count : guint8;
        state : TGdkWindowState;
        flag0 : word; { this is a bit combined flag, combining the following:
                         guint guffaw_gravity : 1;
                         guint input_only : 1;
                         guint modal_hint : 1;
                         guint destroyed : 2;
                        use the following functions to access them:
                         GdkWindowObject_guffaw_gravity
                         GdkWindowObject_set_guffaw_gravity
                         GdkWindowObject_input_only
                         GdkWindowObject_set_input_only
                         GdkWindowObject_modal_hint
                         GdkWindowObject_set_modal_hint
                         GdkWindowObject_destroyed
                         GdkWindowObject_set_destroyed
                       }
        event_mask : TGdkEventMask;
     end;

   PGdkWindowObjectClass = ^TGdkWindowObjectClass;
   TGdkWindowObjectClass = record
        parent_class : TGdkDrawableClass;
     end;

   gdk_window_invalidate_maybe_recurse_child_func =
     function (para1:PGdkWindow; para2:gpointer):gboolean;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   // values for TGdkWindowAttributesType
   GDK_WA_TITLE = 1 shl 1;
   GDK_WA_X = 1 shl 2;
   GDK_WA_Y = 1 shl 3;
   GDK_WA_CURSOR = 1 shl 4;
   GDK_WA_COLORMAP = 1 shl 5;
   GDK_WA_VISUAL = 1 shl 6;
   GDK_WA_WMCLASS = 1 shl 7;
   GDK_WA_NOREDIR = 1 shl 8;

   // values for TGdkWindowHints
   GDK_HINT_POS = 1 shl 0;
   GDK_HINT_MIN_SIZE = 1 shl 1;
   GDK_HINT_MAX_SIZE = 1 shl 2;
   GDK_HINT_BASE_SIZE = 1 shl 3;
   GDK_HINT_ASPECT = 1 shl 4;
   GDK_HINT_RESIZE_INC = 1 shl 5;
   GDK_HINT_WIN_GRAVITY = 1 shl 6;
   GDK_HINT_USER_POS = 1 shl 7;
   GDK_HINT_USER_SIZE = 1 shl 8;

   // values for TGdkWMDecoration
   GDK_DECOR_ALL = 1 shl 0;
   GDK_DECOR_BORDER = 1 shl 1;
   GDK_DECOR_RESIZEH = 1 shl 2;
   GDK_DECOR_TITLE = 1 shl 3;
   GDK_DECOR_MENU = 1 shl 4;
   GDK_DECOR_MINIMIZE = 1 shl 5;
   GDK_DECOR_MAXIMIZE = 1 shl 6;

   // values for TGdkWMFunction
   GDK_FUNC_ALL = 1 shl 0;
   GDK_FUNC_RESIZE = 1 shl 1;
   GDK_FUNC_MOVE = 1 shl 2;
   GDK_FUNC_MINIMIZE = 1 shl 3;
   GDK_FUNC_MAXIMIZE = 1 shl 4;
   GDK_FUNC_CLOSE = 1 shl 5;

   //  values for TGdkGravity
   GDK_GRAVITY_NORTH_WEST = 1;
   GDK_GRAVITY_NORTH = 2;
   GDK_GRAVITY_NORTH_EAST = 3;
   GDK_GRAVITY_WEST = 4;
   GDK_GRAVITY_CENTER = 5;
   GDK_GRAVITY_EAST = 6;
   GDK_GRAVITY_SOUTH_WEST = 7;
   GDK_GRAVITY_SOUTH = 8;
   GDK_GRAVITY_SOUTH_EAST = 9;
   GDK_GRAVITY_STATIC = 10;


function GDK_TYPE_WINDOW : GType;
function GDK_WINDOW(anObject : Pointer) : PGdkWindow;
function GDK_WINDOW_CLASS(klass : Pointer) : PGdkWindowObjectClass;
function GDK_IS_WINDOW(anObject : Pointer) : boolean;
function GDK_IS_WINDOW_CLASS(klass : Pointer) : boolean;
function GDK_WINDOW_GET_CLASS(obj : Pointer) : PGdkWindowObjectClass;

function GDK_WINDOW_OBJECT(anObject : Pointer) : PGdkWindowObject;


// constants and functions to access the bits in a TGdkWindowObject
const
   bm_TGdkWindowObject_guffaw_gravity = $1;
   bp_TGdkWindowObject_guffaw_gravity = 0;
   bm_TGdkWindowObject_input_only = $2;
   bp_TGdkWindowObject_input_only = 1;
   bm_TGdkWindowObject_modal_hint = $4;
   bp_TGdkWindowObject_modal_hint = 2;
   bm_TGdkWindowObject_destroyed = $18;
   bp_TGdkWindowObject_destroyed = 3;

function GdkWindowObject_guffaw_gravity(var a : TGdkWindowObject) : guint;
procedure GdkWindowObject_set_guffaw_gravity(var a : TGdkWindowObject; __guffaw_gravity : guint);
function GdkWindowObject_input_only(var a : TGdkWindowObject) : guint;
procedure GdkWindowObject_set_input_only(var a : TGdkWindowObject; __input_only : guint);
function GdkWindowObject_modal_hint(var a : TGdkWindowObject) : guint;
procedure GdkWindowObject_set_modal_hint(var a : TGdkWindowObject; __modal_hint : guint);
function GdkWindowObject_destroyed(var a : TGdkWindowObject) : guint;
procedure GdkWindowObject_set_destroyed(var a : TGdkWindowObject; __destroyed : guint);


{ Windows }

function gdk_window_object_get_type:GType; cdecl; external gdklib;
function gdk_window_new(parent:PGdkWindow; attributes:PGdkWindowAttr; attributes_mask:gint):PGdkWindow; cdecl; external gdklib;
procedure gdk_window_destroy(window:PGdkWindow); cdecl; external gdklib;
function gdk_window_get_window_type(window:PGdkWindow):TGdkWindowType; cdecl; external gdklib;
function gdk_window_at_pointer(win_x:Pgint; win_y:Pgint):PGdkWindow; cdecl; external gdklib;
procedure gdk_window_show(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_hide(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_withdraw(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_show_unraised(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_move(window:PGdkWindow; x:gint; y:gint); cdecl; external gdklib;
procedure gdk_window_resize(window:PGdkWindow; width:gint; height:gint); cdecl; external gdklib;
procedure gdk_window_move_resize(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint); cdecl; external gdklib;
procedure gdk_window_reparent(window:PGdkWindow; new_parent:PGdkWindow; x:gint; y:gint); cdecl; external gdklib;
procedure gdk_window_clear(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_clear_area(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint); cdecl; external gdklib;
procedure gdk_window_clear_area_e(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint); cdecl; external gdklib;
procedure gdk_window_raise(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_lower(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_focus(window:PGdkWindow; timestamp:guint32); cdecl; external gdklib;
procedure gdk_window_set_user_data(window:PGdkWindow; user_data:gpointer); cdecl; external gdklib;
procedure gdk_window_set_override_redirect(window:PGdkWindow; override_redirect:gboolean); cdecl; external gdklib;
procedure gdk_window_add_filter(window:PGdkWindow; _function:TGdkFilterFunc; data:gpointer); cdecl; external gdklib;
procedure gdk_window_remove_filter(window:PGdkWindow; _function:TGdkFilterFunc; data:gpointer); cdecl; external gdklib;
procedure gdk_window_scroll(window:PGdkWindow; dx:gint; dy:gint); cdecl; external gdklib;

{
   This allows for making shaped (partially transparent) windows
   - cool feature, needed for Drag and Drag for example.
    The shape_mask can be the mask
    from gdk_pixmap_create_from_xpm.   Stefan Wille
  }
procedure gdk_window_shape_combine_mask(window:PGdkWindow; mask:PGdkBitmap; x:gint; y:gint); cdecl; external gdklib;
procedure gdk_window_shape_combine_region(window:PGdkWindow; shape_region:PGdkRegion; offset_x:gint; offset_y:gint); cdecl; external gdklib;

{
   This routine allows you to quickly take the shapes of all the child windows
   of a window and use their shapes as the shape mask for this window - useful
   for container windows that dont want to look like a big box

   - Raster
  }
procedure gdk_window_set_child_shapes(window:PGdkWindow); cdecl; external gdklib;

{
   This routine allows you to merge (ie ADD) child shapes to your
   own window's shape keeping its current shape and ADDING the child
   shapes to it.

   - Raster
  }
procedure gdk_window_merge_child_shapes(window:PGdkWindow); cdecl; external gdklib;

{
   Check if a window has been shown, and whether all its
   parents up to a toplevel have been shown, respectively.
   Note that a window that is_viewable below is not necessarily
   viewable in the X sense.
  }
function gdk_window_is_visible(window:PGdkWindow):gboolean; cdecl; external gdklib;
function gdk_window_is_viewable(window:PGdkWindow):gboolean; cdecl; external gdklib;
function gdk_window_get_state(window:PGdkWindow):TGdkWindowState; cdecl; external gdklib;

{ Set static bit gravity on the parent, and static
   window gravity on all children.
  }
function gdk_window_set_static_gravities(window:PGdkWindow; use_static:gboolean):gboolean; cdecl; external gdklib;


{ Functions to create/lookup windows from their native equivalents  }

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_window_foreign_new(anid:TGdkNativeWindow):PGdkWindow; cdecl; external gdklib;
function gdk_window_lookup(anid:TGdkNativeWindow):PGdkWindow; cdecl; external gdklib;
{$endif}

function gdk_window_foreign_new_for_display(display:PGdkDisplay; anid:TGdkNativeWindow):PGdkWindow; cdecl; external gdklib;
function gdk_window_lookup_for_display(display:PGdkDisplay; anid:TGdkNativeWindow):PGdkWindow; cdecl; external gdklib;


{ GdkWindow  }

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_window_set_hints(window:PGdkWindow; x:gint; y:gint; min_width:gint; min_height:gint;
            max_width:gint; max_height:gint; flags:gint); cdecl; external gdklib;
{$endif}
procedure gdk_window_set_type_hint(window:PGdkWindow; hint:TGdkWindowTypeHint); cdecl; external gdklib;
procedure gdk_window_set_modal_hint(window:PGdkWindow; modal:gboolean); cdecl; external gdklib;
procedure gdk_window_set_geometry_hints(window:PGdkWindow; geometry:PGdkGeometry; geom_mask:TGdkWindowHints); cdecl; external gdklib;
procedure gdk_set_sm_client_id(sm_client_id:Pgchar); cdecl; external gdklib;
procedure gdk_window_begin_paint_rect(window:PGdkWindow; rectangle:PGdkRectangle); cdecl; external gdklib;
procedure gdk_window_begin_paint_region(window:PGdkWindow; region:PGdkRegion); cdecl; external gdklib;
procedure gdk_window_end_paint(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_set_title(window:PGdkWindow; title:Pgchar); cdecl; external gdklib;
procedure gdk_window_set_role(window:PGdkWindow; role:Pgchar); cdecl; external gdklib;
procedure gdk_window_set_transient_for(window:PGdkWindow; parent:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_set_background(window:PGdkWindow; color:PGdkColor); cdecl; external gdklib;
procedure gdk_window_set_back_pixmap(window:PGdkWindow; pixmap:PGdkPixmap; parent_relative:gboolean); cdecl; external gdklib;
procedure gdk_window_set_cursor(window:PGdkWindow; cursor:PGdkCursor); cdecl; external gdklib;
procedure gdk_window_get_user_data(window:PGdkWindow; data:gpointer); cdecl; external gdklib;
procedure gdk_window_get_geometry(window:PGdkWindow; x:Pgint; y:Pgint; width:Pgint; height:Pgint;
            depth:Pgint); cdecl; external gdklib;
procedure gdk_window_get_position(window:PGdkWindow; x:Pgint; y:Pgint); cdecl; external gdklib;
function gdk_window_get_origin(window:PGdkWindow; x:Pgint; y:Pgint):gint; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_window_get_deskrelative_origin(window:PGdkWindow; x:Pgint; y:Pgint):gboolean; cdecl; external gdklib;
{$endif}

procedure gdk_window_get_root_origin(window:PGdkWindow; x:Pgint; y:Pgint); cdecl; external gdklib;
procedure gdk_window_get_frame_extents(window:PGdkWindow; rect:PGdkRectangle); cdecl; external gdklib;
function gdk_window_get_pointer(window:PGdkWindow; x:Pgint; y:Pgint; mask:PGdkModifierType):PGdkWindow; cdecl; external gdklib;
function gdk_window_get_parent(window:PGdkWindow):PGdkWindow; cdecl; external gdklib;
function gdk_window_get_toplevel(window:PGdkWindow):PGdkWindow; cdecl; external gdklib;
function gdk_window_get_children(window:PGdkWindow):PGList; cdecl; external gdklib;
function gdk_window_peek_children(window:PGdkWindow):PGList; cdecl; external gdklib;
function gdk_window_get_events(window:PGdkWindow):TGdkEventMask; cdecl; external gdklib;
procedure gdk_window_set_events(window:PGdkWindow; event_mask:TGdkEventMask); cdecl; external gdklib;
procedure gdk_window_set_icon_list(window:PGdkWindow; pixbufs:PGList); cdecl; external gdklib;
procedure gdk_window_set_icon(window:PGdkWindow; icon_window:PGdkWindow; pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gdklib;
procedure gdk_window_set_icon_name(window:PGdkWindow; name:Pgchar); cdecl; external gdklib;
procedure gdk_window_set_group(window:PGdkWindow; leader:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_set_decorations(window:PGdkWindow; decorations:TGdkWMDecoration); cdecl; external gdklib;
function gdk_window_get_decorations(window:PGdkWindow; decorations:PGdkWMDecoration):gboolean; cdecl; external gdklib;
procedure gdk_window_set_functions(window:PGdkWindow; functions:TGdkWMFunction); cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_window_get_toplevels:PGList; cdecl; external gdklib;
// Already defined: procedure gdk_set_sm_client_id(sm_client_id:Pgchar); cdecl; external gdklib;
{$endif}

procedure gdk_window_iconify(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_deiconify(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_stick(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_unstick(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_maximize(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_unmaximize(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_register_dnd(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_begin_resize_drag(window:PGdkWindow; edge:TGdkWindowEdge; button:gint; root_x:gint; root_y:gint;
            timestamp:guint32); cdecl; external gdklib;
procedure gdk_window_begin_move_drag(window:PGdkWindow; button:gint; root_x:gint; root_y:gint; timestamp:guint32); cdecl; external gdklib;


{ Interface for dirty-region queueing  }

procedure gdk_window_invalidate_rect(window:PGdkWindow; rect:PGdkRectangle; invalidate_children:gboolean); cdecl; external gdklib;
procedure gdk_window_invalidate_region(window:PGdkWindow; region:PGdkRegion; invalidate_children:gboolean); cdecl; external gdklib;
procedure gdk_window_invalidate_maybe_recurse(window:PGdkWindow; region:PGdkRegion; child_func:gdk_window_invalidate_maybe_recurse_child_func; user_data:gpointer); cdecl; external gdklib;
function gdk_window_get_update_area(window:PGdkWindow):PGdkRegion; cdecl; external gdklib;
procedure gdk_window_freeze_updates(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_thaw_updates(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_window_process_all_updates; cdecl; external gdklib;
procedure gdk_window_process_updates(window:PGdkWindow; update_children:gboolean); cdecl; external gdklib;


{ Enable/disable flicker, so you can tell if your code is inefficient.  }

procedure gdk_window_set_debug_updates(setting:gboolean); cdecl; external gdklib;
procedure gdk_window_constrain_size(geometry:PGdkGeometry; flags:guint; width:gint; height:gint; new_width:Pgint;
            new_height:Pgint); cdecl; external gdklib;
procedure gdk_window_get_internal_paint_info(window:PGdkWindow; var real_drawable:PGdkDrawable; x_offset:Pgint; y_offset:Pgint); cdecl; external gdklib;
function gdk_set_pointer_hooks(new_hooks:PGdkPointerHooks):PGdkPointerHooks; cdecl; external gdklib;
function gdk_get_default_root_window:PGdkWindow; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function GDK_ROOT_PARENT : PGdkWindow;

procedure gdk_window_get_size(drawable:PGdkDrawable; width:Pgint; height:Pgint);
function gdk_window_get_type(window:PGdkWindow):TGdkWindowType;
function gdk_window_get_colormap(drawable:PGdkDrawable):PGdkColormap;
procedure gdk_window_set_colormap(drawable:PGdkDrawable; colormap:PGdkColormap);
function gdk_window_get_visual(drawable:PGdkDrawable):PGdkVisual;
function gdk_window_ref(drawable:PGdkDrawable):PGdkDrawable;
procedure gdk_window_unref(drawable:PGdkDrawable);

procedure gdk_window_copy_area(drawable:PGdkDrawable; gc:PGdkGC;
  x,y: gint; source_drawable:PGdkDrawable; source_x,source_y: gint;
  width,height : gint);
{$endif}
{ GDK_DISABLE_DEPRECATED  }

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_WINDOW : GType;
begin
  GDK_TYPE_WINDOW:=gdk_window_object_get_type;
end;

function GDK_WINDOW(anObject : Pointer) : PGdkWindow;
begin
  GDK_WINDOW:=PGdkWindow(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_WINDOW));
end;

function GDK_WINDOW_CLASS(klass : Pointer) : PGdkWindowObjectClass;
begin
  GDK_WINDOW_CLASS:=PGdkWindowObjectClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                              GDK_TYPE_WINDOW));
end;

function GDK_IS_WINDOW(anObject : Pointer) : boolean;
begin
  GDK_IS_WINDOW:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_WINDOW);
end;

function GDK_IS_WINDOW_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_WINDOW_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_WINDOW);
end;

function GDK_WINDOW_GET_CLASS(obj : Pointer) : PGdkWindowObjectClass;
begin
  GDK_WINDOW_GET_CLASS:=PGdkWindowObjectClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                              GDK_TYPE_WINDOW));
end;

function GDK_WINDOW_OBJECT(anObject : Pointer) : PGdkWindowObject;
begin
  GDK_WINDOW_OBJECT:=PGdkWindowObject(GDK_WINDOW(anObject));
end;

function GdkWindowObject_guffaw_gravity(var a : TGdkWindowObject) : guint;
begin
  GdkWindowObject_guffaw_gravity:=
    (a.flag0 and bm_TGdkWindowObject_guffaw_gravity)
      shr bp_TGdkWindowObject_guffaw_gravity;
end;

procedure GdkWindowObject_set_guffaw_gravity(var a : TGdkWindowObject; __guffaw_gravity : guint);
begin
  a.flag0:=
    a.flag0 or ((__guffaw_gravity shl bp_TGdkWindowObject_guffaw_gravity)
      and bm_TGdkWindowObject_guffaw_gravity);
end;

function GdkWindowObject_input_only(var a : TGdkWindowObject) : guint;
begin
  GdkWindowObject_input_only:=
    (a.flag0 and bm_TGdkWindowObject_input_only) shr bp_TGdkWindowObject_input_only;
end;

procedure GdkWindowObject_set_input_only(var a : TGdkWindowObject; __input_only : guint);
begin
  a.flag0:=a.flag0 or ((__input_only shl bp_TGdkWindowObject_input_only)
    and bm_TGdkWindowObject_input_only);
end;

function GdkWindowObject_modal_hint(var a : TGdkWindowObject) : guint;
begin
  GdkWindowObject_modal_hint:=(a.flag0 and bm_TGdkWindowObject_modal_hint)
    shr bp_TGdkWindowObject_modal_hint;
end;

procedure GdkWindowObject_set_modal_hint(var a : TGdkWindowObject; __modal_hint : guint);
begin
  a.flag0:=a.flag0 or ((__modal_hint shl bp_TGdkWindowObject_modal_hint)
    and bm_TGdkWindowObject_modal_hint);
end;

function GdkWindowObject_destroyed(var a : TGdkWindowObject) : guint;
begin
  GdkWindowObject_destroyed:=(a.flag0 and bm_TGdkWindowObject_destroyed)
    shr bp_TGdkWindowObject_destroyed;
end;

procedure GdkWindowObject_set_destroyed(var a : TGdkWindowObject; __destroyed : guint);
begin
  a.flag0:=a.flag0 or ((__destroyed shl bp_TGdkWindowObject_destroyed)
    and bm_TGdkWindowObject_destroyed);
end;

function GDK_ROOT_PARENT : PGdkWindow;
begin
  GDK_ROOT_PARENT:=gdk_get_default_root_window;
end;

procedure gdk_window_get_size(drawable:PGdkDrawable; width:Pgint; height:Pgint);
begin
  gdk_drawable_get_size(drawable,width,height);
end;

function gdk_window_get_type(window:PGdkWindow):TGdkWindowType;
begin
  gdk_window_get_type:=gdk_window_get_window_type(window);
end;

function gdk_window_get_colormap(drawable:PGdkDrawable):PGdkColormap;
begin
  gdk_window_get_colormap:=gdk_drawable_get_colormap(drawable);
end;

procedure gdk_window_set_colormap(drawable:PGdkDrawable; colormap:PGdkColormap);
begin
  gdk_drawable_set_colormap(drawable,colormap);
end;

function gdk_window_get_visual(drawable:PGdkDrawable):PGdkVisual;
begin
  gdk_window_get_visual:=gdk_drawable_get_visual(drawable);
end;

function gdk_window_ref(drawable:PGdkDrawable):PGdkDrawable;
begin
  gdk_window_ref:=GDK_DRAWABLE(g_object_ref(G_OBJECT(drawable)));
end;

procedure gdk_window_unref(drawable:PGdkDrawable);
begin
  g_object_unref(G_OBJECT(drawable));
end;

procedure gdk_window_copy_area(drawable:PGdkDrawable; gc:PGdkGC;
  x,y: gint; source_drawable:PGdkDrawable; source_x,source_y: gint;
  width,height : gint);
begin
  gdk_draw_pixmap(drawable,gc,source_drawable,source_x,source_y,
                  x,y,width,height);
end;
{$ENDIF}

