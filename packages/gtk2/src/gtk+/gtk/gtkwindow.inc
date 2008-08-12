// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type:
   PGtkWindowGeometryInfo = pointer;

   PGtkWindowGroup = ^TGtkWindowGroup;

{ The following flags are initially TRUE (before a window is mapped).
     They cause us to compute a configure request that involves
     default-only parameters. Once mapped, we set them to FALSE.
     Then we set them to TRUE again on unmap (for position)
     and on unrealize (for size).
    }
{ GtkWindowType  }
{ gtk_window_iconify() called before realization  }
{ GdkWindowTypeHint  }
{ GdkGravity  }
   PGtkWindow = ^TGtkWindow;
   TGtkWindow = record
        bin : TGtkBin;
        title : Pgchar;
        wmclass_name : Pgchar;
        wmclass_class : Pgchar;
        wm_role : Pgchar;
        focus_widget : PGtkWidget;
        default_widget : PGtkWidget;
        transient_parent : PGtkWindow;
        geometry_info : PGtkWindowGeometryInfo;
        frame : PGdkWindow;
        group : PGtkWindowGroup;
        configure_request_count : guint16;
        flag0 : longint;
        frame_left : guint;
        frame_top : guint;
        frame_right : guint;
        frame_bottom : guint;
        keys_changed_handler : guint;
        mnemonic_modifier : TGdkModifierType;
        screen : PGdkScreen;
     end;


{ G_SIGNAL_ACTION signals for keybindings  }
{ Padding for future expansion  }
   PGtkWindowClass = ^TGtkWindowClass;
   TGtkWindowClass = record
        parent_class : TGtkBinClass;
        set_focus : procedure (window:PGtkWindow; focus:PGtkWidget); cdecl;
        frame_event : function (window:PGtkWindow; event:PGdkEvent):gboolean; cdecl;
        activate_focus : procedure (window:PGtkWindow); cdecl;
        activate_default : procedure (window:PGtkWindow); cdecl;
        move_focus : procedure (window:PGtkWindow; direction:TGtkDirectionType); cdecl;
        keys_changed : procedure (window:PGtkWindow); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;


   TGtkWindowGroup = record
        parent_instance : TGObject;
        grabs : PGSList;
     end;

{ Padding for future expansion  }
   PGtkWindowGroupClass = ^TGtkWindowGroupClass;
   TGtkWindowGroupClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

   TGtkWindowKeysForeachFunc = procedure (window:PGtkWindow; keyval:guint; modifiers:TGdkModifierType; is_mnemonic:gboolean; data:gpointer); cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkWindow_allow_shrink = $1;
   bp_TGtkWindow_allow_shrink = 0;
   bm_TGtkWindow_allow_grow = $2;
   bp_TGtkWindow_allow_grow = 1;
   bm_TGtkWindow_configure_notify_received = $4;
   bp_TGtkWindow_configure_notify_received = 2;
   bm_TGtkWindow_need_default_position = $8;
   bp_TGtkWindow_need_default_position = 3;
   bm_TGtkWindow_need_default_size = $10;
   bp_TGtkWindow_need_default_size = 4;
   bm_TGtkWindow_position = $E0;
   bp_TGtkWindow_position = 5;
   bm_TGtkWindow_type = $F00;
   bp_TGtkWindow_type = 8;
   bm_TGtkWindow_has_user_ref_count = $1000;
   bp_TGtkWindow_has_user_ref_count = 12;
   bm_TGtkWindow_has_focus = $2000;
   bp_TGtkWindow_has_focus = 13;
   bm_TGtkWindow_modal = $4000;
   bp_TGtkWindow_modal = 14;
   bm_TGtkWindow_destroy_with_parent = $8000;
   bp_TGtkWindow_destroy_with_parent = 15;
   bm_TGtkWindow_has_frame = $10000;
   bp_TGtkWindow_has_frame = 16;
   bm_TGtkWindow_iconify_initially = $20000;
   bp_TGtkWindow_iconify_initially = 17;
   bm_TGtkWindow_stick_initially = $40000;
   bp_TGtkWindow_stick_initially = 18;
   bm_TGtkWindow_maximize_initially = $80000;
   bp_TGtkWindow_maximize_initially = 19;
   bm_TGtkWindow_decorated = $100000;
   bp_TGtkWindow_decorated = 20;
   bm_TGtkWindow_type_hint = $E00000;
   bp_TGtkWindow_type_hint = 21;
   bm_TGtkWindow_gravity = $1F000000;
   bp_TGtkWindow_gravity = 24;


function GTK_TYPE_WINDOW : GType;
function GTK_WINDOW(obj: pointer) : PGtkWindow;
function GTK_WINDOW_CLASS(klass: pointer) : PGtkWindowClass;
function GTK_IS_WINDOW(obj: pointer) : boolean;
function GTK_IS_WINDOW_CLASS(klass: pointer) : boolean;
function GTK_WINDOW_GET_CLASS(obj: pointer) : PGtkWindowClass;


function allow_shrink(var a : TGtkWindow) : guint;
procedure set_allow_shrink(var a : TGtkWindow; __allow_shrink : guint);
function allow_grow(var a : TGtkWindow) : guint;
procedure set_allow_grow(var a : TGtkWindow; __allow_grow : guint);
function configure_notify_received(var a : TGtkWindow) : guint;
procedure set_configure_notify_received(var a : TGtkWindow; __configure_notify_received : guint);
function need_default_position(var a : TGtkWindow) : guint;
procedure set_need_default_position(var a : TGtkWindow; __need_default_position : guint);
function need_default_size(var a : TGtkWindow) : guint;
procedure set_need_default_size(var a : TGtkWindow; __need_default_size : guint);
function position(var a : TGtkWindow) : guint;
procedure set_position(var a : TGtkWindow; __position : guint);
function get_type(var a : TGtkWindow) : guint; overload;
procedure set_type(var a : TGtkWindow; __type : guint); overload;
function has_user_ref_count(var a : TGtkWindow) : guint;
procedure set_has_user_ref_count(var a : TGtkWindow; __has_user_ref_count : guint);
function has_focus(var a : TGtkWindow) : guint;
procedure set_has_focus(var a : TGtkWindow; __has_focus : guint);
function modal(var a : TGtkWindow) : guint;
procedure set_modal(var a : TGtkWindow; __modal : guint);
function destroy_with_parent(var a : TGtkWindow) : guint;
procedure set_destroy_with_parent(var a : TGtkWindow; __destroy_with_parent : guint);
function has_frame(var a : TGtkWindow) : guint; overload;
procedure set_has_frame(var a : TGtkWindow; __has_frame : guint); overload;
function iconify_initially(var a : TGtkWindow) : guint;
procedure set_iconify_initially(var a : TGtkWindow; __iconify_initially : guint);
function stick_initially(var a : TGtkWindow) : guint;
procedure set_stick_initially(var a : TGtkWindow; __stick_initially : guint);
function maximize_initially(var a : TGtkWindow) : guint;
procedure set_maximize_initially(var a : TGtkWindow; __maximize_initially : guint);
function decorated(var a : TGtkWindow) : guint;
procedure set_decorated(var a : TGtkWindow; __decorated : guint);
function type_hint(var a : TGtkWindow) : guint;
procedure set_type_hint(var a : TGtkWindow; __type_hint : guint);
function gravity(var a : TGtkWindow) : guint;
procedure set_gravity(var a : TGtkWindow; __gravity : guint);
function GTK_TYPE_WINDOW_GROUP : GType;
function GTK_WINDOW_GROUP(anObject: pointer) : PGtkWindowGroup;
function GTK_WINDOW_GROUP_CLASS(klass: pointer) : PGtkWindowGroupClass;
function GTK_IS_WINDOW_GROUP(anObject: pointer) : boolean;
function GTK_IS_WINDOW_GROUP_CLASS(klass: pointer) : boolean;
function GTK_WINDOW_GROUP_GET_CLASS(obj: pointer) : PGtkWindowGroupClass;


function gtk_window_get_type:TGtkType; cdecl; external gtklib;
function gtk_window_new(_type:TGtkWindowType):PGtkWidget; cdecl; external gtklib;
procedure gtk_window_set_title(window:PGtkWindow; title:Pgchar); cdecl; external gtklib;
function gtk_window_get_title(window:PGtkWindow):Pgchar; cdecl; external gtklib;
procedure gtk_window_set_wmclass(window:PGtkWindow; wmclass_name:Pgchar; wmclass_class:Pgchar); cdecl; external gtklib;
procedure gtk_window_set_role(window:PGtkWindow; role:Pgchar); cdecl; external gtklib;
function gtk_window_get_role(window:PGtkWindow):Pgchar; cdecl; external gtklib;
procedure gtk_window_add_accel_group(window:PGtkWindow; accel_group:PGtkAccelGroup); cdecl; external gtklib;
procedure gtk_window_remove_accel_group(window:PGtkWindow; accel_group:PGtkAccelGroup); cdecl; external gtklib;
procedure gtk_window_set_position(window:PGtkWindow; position:TGtkWindowPosition); cdecl; external gtklib;
function gtk_window_activate_focus(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_focus(window:PGtkWindow; focus:PGtkWidget); cdecl; external gtklib;
function gtk_window_get_focus(window:PGtkWindow):PGtkWidget; cdecl; external gtklib;
procedure gtk_window_set_default(window:PGtkWindow; default_widget:PGtkWidget); cdecl; external gtklib;
function gtk_window_activate_default(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_transient_for(window:PGtkWindow; parent:PGtkWindow); cdecl; external gtklib;
function gtk_window_get_transient_for(window:PGtkWindow):PGtkWindow; cdecl; external gtklib;
procedure gtk_window_set_type_hint(window:PGtkWindow; hint:TGdkWindowTypeHint); cdecl; external gtklib;
function gtk_window_get_type_hint(window:PGtkWindow):TGdkWindowTypeHint; cdecl; external gtklib;
procedure gtk_window_set_destroy_with_parent(window:PGtkWindow; setting:gboolean); cdecl; external gtklib;
function gtk_window_get_destroy_with_parent(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_resizable(window:PGtkWindow; resizable:gboolean); cdecl; external gtklib;
function gtk_window_get_resizable(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_gravity(window:PGtkWindow; gravity:TGdkGravity); cdecl; external gtklib;
function gtk_window_get_gravity(window:PGtkWindow):TGdkGravity; cdecl; external gtklib;
procedure gtk_window_set_geometry_hints(window:PGtkWindow; geometry_widget:PGtkWidget; geometry:PGdkGeometry; geom_mask:TGdkWindowHints); cdecl; external gtklib;
{ gtk_window_set_has_frame () must be called before realizing the window_ }
procedure gtk_window_set_has_frame(window:PGtkWindow; setting:gboolean); cdecl; external gtklib;
function gtk_window_get_has_frame(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_frame_dimensions(window:PGtkWindow; left:gint; top:gint; right:gint; bottom:gint); cdecl; external gtklib;
procedure gtk_window_get_frame_dimensions(window:PGtkWindow; left:Pgint; top:Pgint; right:Pgint; bottom:Pgint); cdecl; external gtklib;
procedure gtk_window_set_decorated(window:PGtkWindow; setting:gboolean); cdecl; external gtklib;
function gtk_window_get_decorated(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_icon_list(window:PGtkWindow; list:PGList); cdecl; external gtklib;
function gtk_window_get_icon_list(window:PGtkWindow):PGList; cdecl; external gtklib;
procedure gtk_window_set_icon(window:PGtkWindow; icon:PGdkPixbuf); cdecl; external gtklib;
function gtk_window_get_icon(window:PGtkWindow):PGdkPixbuf; cdecl; external gtklib;
procedure gtk_window_set_default_icon_list(list:PGList); cdecl; external gtklib;
function gtk_window_get_default_icon_list:PGList; cdecl; external gtklib;
{ If window is set modal, input will be grabbed when show and released when hide  }
procedure gtk_window_set_modal(window:PGtkWindow; modal:gboolean); cdecl; external gtklib;
function gtk_window_get_modal(window:PGtkWindow):gboolean; cdecl; external gtklib;
function gtk_window_list_toplevels:PGList; cdecl; external gtklib;
procedure gtk_window_add_mnemonic(window:PGtkWindow; keyval:guint; target:PGtkWidget); cdecl; external gtklib;
procedure gtk_window_remove_mnemonic(window:PGtkWindow; keyval:guint; target:PGtkWidget); cdecl; external gtklib;
function gtk_window_mnemonic_activate(window:PGtkWindow; keyval:guint; modifier:TGdkModifierType):gboolean; cdecl; external gtklib;
procedure gtk_window_set_mnemonic_modifier(window:PGtkWindow; modifier:TGdkModifierType); cdecl; external gtklib;
function gtk_window_get_mnemonic_modifier(window:PGtkWindow):TGdkModifierType; cdecl; external gtklib;
procedure gtk_window_present(window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_iconify(window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_deiconify(window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_stick(window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_unstick(window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_maximize(window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_unmaximize(window:PGtkWindow); cdecl; external gtklib;

{$ifdef HasGTK2_2}
procedure gtk_window_fullscreen(window: PGtkWindow); cdecl; external gtklib;
procedure gtk_window_unfullscreen(window: PGtkWindow); cdecl; external gtklib;
procedure gtk_window_set_keep_above(window: PGtkWindow; setting: gboolean); cdecl; external gtklib;
procedure gtk_window_set_keep_below(window: PGtkWindow; setting: gboolean); cdecl; external gtklib;
procedure gtk_window_set_screen(window:PGtkWindow; screen:PGdkScreen); cdecl; external gtklib;
function gtk_window_get_screen(window:PGtkWindow):PGdkScreen; cdecl; external gtklib;
function gtk_window_get_skip_pager_hint(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_skip_pager_hint(window:PGtkWindow; setting:gboolean); cdecl; external gtklib;
function gtk_window_get_skip_taskbar_hint(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_skip_taskbar_hint(window:PGtkWindow; setting:gboolean); cdecl; external gtklib;
procedure gtk_window_set_auto_startup_notification(setting:gboolean); cdecl; external gtklib;
function gtk_window_set_default_icon_from_file(filename: Pgchar; err: PPGError):gboolean; cdecl; external gtklib;
function gtk_window_set_icon_from_file(window: PGtkWindow; filename: Pgchar; err: PPGError):gboolean; cdecl; external gtklib;
{$endif HasGTK2_2}

procedure gtk_window_begin_resize_drag(window:PGtkWindow; edge:TGdkWindowEdge; button:gint; root_x:gint; root_y:gint;
            timestamp:guint32); cdecl; external gtklib;
procedure gtk_window_begin_move_drag(window:PGtkWindow; button:gint; root_x:gint; root_y:gint; timestamp:guint32); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_window_set_policy(window:PGtkWindow; allow_shrink:gint; allow_grow:gint; auto_shrink:gint); cdecl; external gtklib;

procedure gtk_window_position(window:PGtkWindow; position:TGtkWindowPosition);
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{ Set initial default size of the window (does not constrain user
   resize operations)
  }

procedure gtk_window_set_default_size(window:PGtkWindow; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_window_get_default_size(window:PGtkWindow; width:Pgint; height:Pgint); cdecl; external gtklib;
procedure gtk_window_resize(window:PGtkWindow; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_window_get_size(window:PGtkWindow; width:Pgint; height:Pgint); cdecl; external gtklib;
procedure gtk_window_move(window:PGtkWindow; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_window_get_position(window:PGtkWindow; root_x:Pgint; root_y:Pgint); cdecl; external gtklib;
function gtk_window_parse_geometry(window:PGtkWindow; geometry:Pgchar):gboolean; cdecl; external gtklib;
{ Ignore this unless you are writing a GUI builder  }
procedure gtk_window_reshow_with_initial_size(window:PGtkWindow); cdecl; external gtklib;
{ Window groups
  }
function gtk_window_group_get_type:GType; cdecl; external gtklib;
function gtk_window_group_new:PGtkWindowGroup; cdecl; external gtklib;
procedure gtk_window_group_add_window(window_group:PGtkWindowGroup; window:PGtkWindow); cdecl; external gtklib;
procedure gtk_window_group_remove_window(window_group:PGtkWindowGroup; window:PGtkWindow); cdecl; external gtklib;
{ --- internal functions ---  }
procedure _gtk_window_internal_set_focus(window:PGtkWindow; focus:PGtkWidget); cdecl; external gtklib;
procedure gtk_window_remove_embedded_xid(window:PGtkWindow; xid:guint); cdecl; external gtklib;
procedure gtk_window_add_embedded_xid(window:PGtkWindow; xid:guint); cdecl; external gtklib;
procedure _gtk_window_reposition(window:PGtkWindow; x:gint; y:gint); cdecl; external gtklib;
procedure _gtk_window_constrain_size(window:PGtkWindow; width:gint; height:gint; new_width:Pgint; new_height:Pgint); cdecl; external gtklib;
function _gtk_window_get_group(window:PGtkWindow):PGtkWindowGroup; cdecl; external gtklib;
function _gtk_window_activate_key(window:PGtkWindow; event:PGdkEventKey):gboolean; cdecl; external gtklib;

procedure _gtk_window_keys_foreach(window:PGtkWindow; func:TGtkWindowKeysForeachFunc; func_data:gpointer); cdecl; external gtklib;
{ --- internal (GtkAcceleratable) ---  }
function _gtk_window_query_nonaccels(window:PGtkWindow; accel_key:guint; accel_mods:TGdkModifierType):gboolean; cdecl; external gtklib;

{$IFDEF HasGTK2_4}
function gtk_window_get_accept_focus(window:PGtkWindow):gboolean; cdecl; external gtklib;
procedure gtk_window_set_accept_focus(window:PGtkWindow; setting:gboolean); cdecl; external gtklib;
function gtk_window_has_toplevel_focus(window: PGtkWindow):gboolean;cdecl; external gtklib;
function gtk_window_is_active(window: PGtkWindow):gboolean; cdecl; external gtklib;
{$ENDIF HasGTK2_4}


{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_WINDOW : GType;
begin
   GTK_TYPE_WINDOW:=gtk_window_get_type;
end;

function GTK_WINDOW(obj: pointer) : PGtkWindow;
begin
   GTK_WINDOW:=PGtkWindow(GTK_CHECK_CAST(obj,GTK_TYPE_WINDOW));
end;

function GTK_WINDOW_CLASS(klass: pointer) : PGtkWindowClass;
begin
   GTK_WINDOW_CLASS:=PGtkWindowClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_WINDOW));
end;

function GTK_IS_WINDOW(obj: pointer) : boolean;
begin
   GTK_IS_WINDOW:=GTK_CHECK_TYPE(obj,GTK_TYPE_WINDOW);
end;

function GTK_IS_WINDOW_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_WINDOW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_WINDOW);
end;

function GTK_WINDOW_GET_CLASS(obj: pointer) : PGtkWindowClass;
begin
   GTK_WINDOW_GET_CLASS:=PGtkWindowClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_WINDOW));
end;

function allow_shrink(var a : TGtkWindow) : guint;
begin
   allow_shrink:=(a.flag0 and bm_TGtkWindow_allow_shrink) shr bp_TGtkWindow_allow_shrink;
end;

procedure set_allow_shrink(var a : TGtkWindow; __allow_shrink : guint);
begin
   a.flag0:=a.flag0 or ((__allow_shrink shl bp_TGtkWindow_allow_shrink) and bm_TGtkWindow_allow_shrink);
end;

function allow_grow(var a : TGtkWindow) : guint;
begin
   allow_grow:=(a.flag0 and bm_TGtkWindow_allow_grow) shr bp_TGtkWindow_allow_grow;
end;

procedure set_allow_grow(var a : TGtkWindow; __allow_grow : guint);
begin
   a.flag0:=a.flag0 or ((__allow_grow shl bp_TGtkWindow_allow_grow) and bm_TGtkWindow_allow_grow);
end;

function configure_notify_received(var a : TGtkWindow) : guint;
begin
   configure_notify_received:=(a.flag0 and bm_TGtkWindow_configure_notify_received) shr bp_TGtkWindow_configure_notify_received;
end;

procedure set_configure_notify_received(var a : TGtkWindow; __configure_notify_received : guint);
begin
   a.flag0:=a.flag0 or ((__configure_notify_received shl bp_TGtkWindow_configure_notify_received) and bm_TGtkWindow_configure_notify_received);
end;

function need_default_position(var a : TGtkWindow) : guint;
begin
   need_default_position:=(a.flag0 and bm_TGtkWindow_need_default_position) shr bp_TGtkWindow_need_default_position;
end;

procedure set_need_default_position(var a : TGtkWindow; __need_default_position : guint);
begin
   a.flag0:=a.flag0 or ((__need_default_position shl bp_TGtkWindow_need_default_position) and bm_TGtkWindow_need_default_position);
end;

function need_default_size(var a : TGtkWindow) : guint;
begin
   need_default_size:=(a.flag0 and bm_TGtkWindow_need_default_size) shr bp_TGtkWindow_need_default_size;
end;

procedure set_need_default_size(var a : TGtkWindow; __need_default_size : guint);
begin
   a.flag0:=a.flag0 or ((__need_default_size shl bp_TGtkWindow_need_default_size) and bm_TGtkWindow_need_default_size);
end;

function position(var a : TGtkWindow) : guint;
begin
   position:=(a.flag0 and bm_TGtkWindow_position) shr bp_TGtkWindow_position;
end;

procedure set_position(var a : TGtkWindow; __position : guint);
begin
   a.flag0:=a.flag0 or ((__position shl bp_TGtkWindow_position) and bm_TGtkWindow_position);
end;

function get_type(var a : TGtkWindow) : guint;
begin
   get_type:=(a.flag0 and bm_TGtkWindow_type) shr bp_TGtkWindow_type;
end;

procedure set_type(var a : TGtkWindow; __type : guint);
begin
   a.flag0:=a.flag0 or ((__type shl bp_TGtkWindow_type) and bm_TGtkWindow_type);
end;

function has_user_ref_count(var a : TGtkWindow) : guint;
begin
   has_user_ref_count:=(a.flag0 and bm_TGtkWindow_has_user_ref_count) shr bp_TGtkWindow_has_user_ref_count;
end;

procedure set_has_user_ref_count(var a : TGtkWindow; __has_user_ref_count : guint);
begin
   a.flag0:=a.flag0 or ((__has_user_ref_count shl bp_TGtkWindow_has_user_ref_count) and bm_TGtkWindow_has_user_ref_count);
end;

function has_focus(var a : TGtkWindow) : guint;
begin
   has_focus:=(a.flag0 and bm_TGtkWindow_has_focus) shr bp_TGtkWindow_has_focus;
end;

procedure set_has_focus(var a : TGtkWindow; __has_focus : guint);
begin
   a.flag0:=a.flag0 or ((__has_focus shl bp_TGtkWindow_has_focus) and bm_TGtkWindow_has_focus);
end;

function modal(var a : TGtkWindow) : guint;
begin
   modal:=(a.flag0 and bm_TGtkWindow_modal) shr bp_TGtkWindow_modal;
end;

procedure set_modal(var a : TGtkWindow; __modal : guint);
begin
   a.flag0:=a.flag0 or ((__modal shl bp_TGtkWindow_modal) and bm_TGtkWindow_modal);
end;

function destroy_with_parent(var a : TGtkWindow) : guint;
begin
   destroy_with_parent:=(a.flag0 and bm_TGtkWindow_destroy_with_parent) shr bp_TGtkWindow_destroy_with_parent;
end;

procedure set_destroy_with_parent(var a : TGtkWindow; __destroy_with_parent : guint);
begin
   a.flag0:=a.flag0 or ((__destroy_with_parent shl bp_TGtkWindow_destroy_with_parent) and bm_TGtkWindow_destroy_with_parent);
end;

function has_frame(var a : TGtkWindow) : guint;
begin
   has_frame:=(a.flag0 and bm_TGtkWindow_has_frame) shr bp_TGtkWindow_has_frame;
end;

procedure set_has_frame(var a : TGtkWindow; __has_frame : guint);
begin
   a.flag0:=a.flag0 or ((__has_frame shl bp_TGtkWindow_has_frame) and bm_TGtkWindow_has_frame);
end;

function iconify_initially(var a : TGtkWindow) : guint;
begin
   iconify_initially:=(a.flag0 and bm_TGtkWindow_iconify_initially) shr bp_TGtkWindow_iconify_initially;
end;

procedure set_iconify_initially(var a : TGtkWindow; __iconify_initially : guint);
begin
   a.flag0:=a.flag0 or ((__iconify_initially shl bp_TGtkWindow_iconify_initially) and bm_TGtkWindow_iconify_initially);
end;

function stick_initially(var a : TGtkWindow) : guint;
begin
   stick_initially:=(a.flag0 and bm_TGtkWindow_stick_initially) shr bp_TGtkWindow_stick_initially;
end;

procedure set_stick_initially(var a : TGtkWindow; __stick_initially : guint);
begin
   a.flag0:=a.flag0 or ((__stick_initially shl bp_TGtkWindow_stick_initially) and bm_TGtkWindow_stick_initially);
end;

function maximize_initially(var a : TGtkWindow) : guint;
begin
   maximize_initially:=(a.flag0 and bm_TGtkWindow_maximize_initially) shr bp_TGtkWindow_maximize_initially;
end;

procedure set_maximize_initially(var a : TGtkWindow; __maximize_initially : guint);
begin
   a.flag0:=a.flag0 or ((__maximize_initially shl bp_TGtkWindow_maximize_initially) and bm_TGtkWindow_maximize_initially);
end;

function decorated(var a : TGtkWindow) : guint;
begin
   decorated:=(a.flag0 and bm_TGtkWindow_decorated) shr bp_TGtkWindow_decorated;
end;

procedure set_decorated(var a : TGtkWindow; __decorated : guint);
begin
   a.flag0:=a.flag0 or ((__decorated shl bp_TGtkWindow_decorated) and bm_TGtkWindow_decorated);
end;

function type_hint(var a : TGtkWindow) : guint;
begin
   type_hint:=(a.flag0 and bm_TGtkWindow_type_hint) shr bp_TGtkWindow_type_hint;
end;

procedure set_type_hint(var a : TGtkWindow; __type_hint : guint);
begin
   a.flag0:=a.flag0 or ((__type_hint shl bp_TGtkWindow_type_hint) and bm_TGtkWindow_type_hint);
end;

function gravity(var a : TGtkWindow) : guint;
begin
   gravity:=(a.flag0 and bm_TGtkWindow_gravity) shr bp_TGtkWindow_gravity;
end;

procedure set_gravity(var a : TGtkWindow; __gravity : guint);
begin
   a.flag0:=a.flag0 or ((__gravity shl bp_TGtkWindow_gravity) and bm_TGtkWindow_gravity);
end;

function GTK_TYPE_WINDOW_GROUP : GType;
begin
   GTK_TYPE_WINDOW_GROUP:=gtk_window_group_get_type;
end;

function GTK_WINDOW_GROUP(anObject: pointer) : PGtkWindowGroup;
begin
   GTK_WINDOW_GROUP:=PGtkWindowGroup(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_WINDOW_GROUP));
end;

function GTK_WINDOW_GROUP_CLASS(klass: pointer) : PGtkWindowGroupClass;
begin
   GTK_WINDOW_GROUP_CLASS:=PGtkWindowGroupClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_WINDOW_GROUP));
end;

function GTK_IS_WINDOW_GROUP(anObject: pointer) : boolean;
begin
   GTK_IS_WINDOW_GROUP:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_WINDOW_GROUP);
end;

function GTK_IS_WINDOW_GROUP_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_WINDOW_GROUP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_WINDOW_GROUP);
end;

function GTK_WINDOW_GROUP_GET_CLASS(obj: pointer) : PGtkWindowGroupClass;
begin
   GTK_WINDOW_GROUP_GET_CLASS:=PGtkWindowGroupClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_WINDOW_GROUP));
end;

procedure gtk_window_position(window:PGtkWindow; position:TGtkWindowPosition);
begin
  gtk_window_set_position(window,position);
end;
{$ENDIF read_implementation}
// included by gtk2.pas

