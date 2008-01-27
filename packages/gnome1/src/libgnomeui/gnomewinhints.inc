{$IfDef read_interface}

const
   XA_WIN_PROTOCOLS = '_WIN_PROTOCOLS';
   XA_WIN_ICONS = '_WIN_ICONS';
   XA_WIN_WORKSPACE = '_WIN_WORKSPACE';
   XA_WIN_WORKSPACE_COUNT = '_WIN_WORKSPACE_COUNT';
   XA_WIN_WORKSPACE_NAMES = '_WIN_WORKSPACE_NAMES';
   XA_WIN_LAYER = '_WIN_LAYER';
   XA_WIN_STATE = '_WIN_STATE';
   XA_WIN_HINTS = '_WIN_HINTS';
   XA_WIN_WORKAREA = '_WIN_WORKAREA';
   XA_WIN_CLIENT_LIST = '_WIN_CLIENT_LIST';
   XA_WIN_APP_STATE = '_WIN_APP_STATE';
   XA_WIN_EXPANDED_SIZE = '_WIN_EXPANDED_SIZE';
   XA_WIN_CLIENT_MOVING = '_WIN_CLIENT_MOVING';
   XA_WIN_SUPPORTING_WM_CHECK = '_WIN_SUPPORTING_WM_CHECK';
type
   TGnomeWinLayer = (WIN_LAYER_DESKTOP := 0,WIN_LAYER_BELOW := 2,
     WIN_LAYER_NORMAL := 4,WIN_LAYER_ONTOP := 6,
     WIN_LAYER_DOCK := 8,WIN_LAYER_ABOVE_DOCK := 10
     );

   TGnomeWinState = (WIN_STATE_STICKY := 1 shl 0,WIN_STATE_MINIMIZED := 1 shl 1,
     WIN_STATE_MAXIMIZED_VERT := 1 shl 2,WIN_STATE_MAXIMIZED_HORIZ := 1 shl 3,
     WIN_STATE_HIDDEN := 1 shl 4,WIN_STATE_SHADED := 1 shl 5,
     WIN_STATE_HID_WORKSPACE := 1 shl 6,WIN_STATE_HID_TRANSIENT := 1 shl 7,
     WIN_STATE_FIXED_POSITION := 1 shl 8,WIN_STATE_ARRANGE_IGNORE := 1 shl 9
     );

   TGnomeWinHints = (WIN_HINTS_SKIP_FOCUS := 1 shl 0,WIN_HINTS_SKIP_WINLIST := 1 shl 1,
     WIN_HINTS_SKIP_TASKBAR := 1 shl 2,WIN_HINTS_GROUP_TRANSIENT := 1 shl 3,
     WIN_HINTS_FOCUS_ON_CLICK := 1 shl 4,WIN_HINTS_DO_NOT_COVER := 1 shl 5
     );

   TGnomeWinAppState = (WIN_APP_STATE_NONE,WIN_APP_STATE_ACTIVE1,
     WIN_APP_STATE_ACTIVE2,WIN_APP_STATE_ERROR1,
     WIN_APP_STATE_ERROR2,WIN_APP_STATE_FATAL_ERROR1,
     WIN_APP_STATE_FATAL_ERROR2,WIN_APP_STATE_IDLE1,
     WIN_APP_STATE_IDLE2,WIN_APP_STATE_WAITING1,
     WIN_APP_STATE_WAITING2,WIN_APP_STATE_WORKING1,
     WIN_APP_STATE_WORKING2,WIN_APP_STATE_NEED_USER_INPUT1,
     WIN_APP_STATE_NEED_USER_INPUT2,WIN_APP_STATE_STRUGGLING1,
     WIN_APP_STATE_STRUGGLING2,WIN_APP_STATE_DISK_TRAFFIC1,
     WIN_APP_STATE_DISK_TRAFFIC2,WIN_APP_STATE_NETWORK_TRAFFIC1,
     WIN_APP_STATE_NETWORK_TRAFFIC2,WIN_APP_STATE_OVERLOADED1,
     WIN_APP_STATE_OVERLOADED2,WIN_APP_STATE_PERCENT000_1,
     WIN_APP_STATE_PERCENT000_2,WIN_APP_STATE_PERCENT010_1,
     WIN_APP_STATE_PERCENT010_2,WIN_APP_STATE_PERCENT020_1,
     WIN_APP_STATE_PERCENT020_2,WIN_APP_STATE_PERCENT030_1,
     WIN_APP_STATE_PERCENT030_2,WIN_APP_STATE_PERCENT040_1,
     WIN_APP_STATE_PERCENT040_2,WIN_APP_STATE_PERCENT050_1,
     WIN_APP_STATE_PERCENT050_2,WIN_APP_STATE_PERCENT060_1,
     WIN_APP_STATE_PERCENT060_2,WIN_APP_STATE_PERCENT070_1,
     WIN_APP_STATE_PERCENT070_2,WIN_APP_STATE_PERCENT080_1,
     WIN_APP_STATE_PERCENT080_2,WIN_APP_STATE_PERCENT090_1,
     WIN_APP_STATE_PERCENT090_2,WIN_APP_STATE_PERCENT100_1,
     WIN_APP_STATE_PERCENT100_2);

procedure gnome_win_hints_init;cdecl;external libgnomeuidll name 'gnome_win_hints_init';
procedure gnome_win_hints_set_layer(window:PGtkWidget; layer:TGnomeWinLayer);cdecl;external libgnomeuidll name 'gnome_win_hints_set_layer';
function gnome_win_hints_get_layer(window:PGtkWidget):TGnomeWinLayer;cdecl;external libgnomeuidll name 'gnome_win_hints_get_layer';
procedure gnome_win_hints_set_state(window:PGtkWidget; state:TGnomeWinState);cdecl;external libgnomeuidll name 'gnome_win_hints_set_state';
function gnome_win_hints_get_state(window:PGtkWidget):TGnomeWinState;cdecl;external libgnomeuidll name 'gnome_win_hints_get_state';
procedure gnome_win_hints_set_hints(window:PGtkWidget; skip:TGnomeWinHints);cdecl;external libgnomeuidll name 'gnome_win_hints_set_hints';
function gnome_win_hints_get_hints(window:PGtkWidget):TGnomeWinHints;cdecl;external libgnomeuidll name 'gnome_win_hints_get_hints';
procedure gnome_win_hints_set_workspace(window:PGtkWidget; workspace:gint);cdecl;external libgnomeuidll name 'gnome_win_hints_set_workspace';
function gnome_win_hints_get_workspace(window:PGtkWidget):gint;cdecl;external libgnomeuidll name 'gnome_win_hints_get_workspace';
procedure gnome_win_hints_set_current_workspace(workspace:gint);cdecl;external libgnomeuidll name 'gnome_win_hints_set_current_workspace';
function gnome_win_hints_get_current_workspace:gint;cdecl;external libgnomeuidll name 'gnome_win_hints_get_current_workspace';
function gnome_win_hints_get_workspace_names:PGList;cdecl;external libgnomeuidll name 'gnome_win_hints_get_workspace_names';
function gnome_win_hints_get_workspace_count:gint;cdecl;external libgnomeuidll name 'gnome_win_hints_get_workspace_count';
procedure gnome_win_hints_set_expanded_size(window:PGtkWidget; x:gint; y:gint; width:gint; height:gint);cdecl;external libgnomeuidll name 'gnome_win_hints_set_expanded_size';
function gnome_win_hints_get_expanded_size(window:PGtkWidget; x:Pgint; y:Pgint; width:Pgint; height:Pgint):gboolean;cdecl;external libgnomeuidll name 'gnome_win_hints_get_expanded_size';
procedure gnome_win_hints_set_moving(window:PGtkWidget; moving:gboolean);cdecl;external libgnomeuidll name 'gnome_win_hints_set_moving';
procedure gnome_win_hints_set_app_state(window:PGtkWidget; state:TGnomeWinAppState);cdecl;external libgnomeuidll name 'gnome_win_hints_set_app_state';
function gnome_win_hints_get_app_state(window:PGtkWidget):TGnomeWinAppState;cdecl;external libgnomeuidll name 'gnome_win_hints_get_app_state';
function gnome_win_hints_wm_exists:gboolean;cdecl;external libgnomeuidll name 'gnome_win_hints_wm_exists';
function gnome_win_hints_get_client_window_ids:PGList;cdecl;external libgnomeuidll name 'gnome_win_hints_get_client_window_ids';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
