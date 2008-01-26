// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ --- notifier ---  }
   TGtkAccelMapForeach = procedure (data:gpointer; accel_path:Pgchar; accel_key:guint; accel_mods:TGdkModifierType; changed:gboolean); cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ --- public API ---  }
procedure gtk_accel_map_add_entry(accel_path:Pgchar; accel_key:guint; accel_mods:TGdkModifierType); cdecl; external gtklib;
function gtk_accel_map_lookup_entry(accel_path:Pgchar; key:PGtkAccelKey):gboolean; cdecl; external gtklib;
function gtk_accel_map_change_entry(accel_path:Pgchar; accel_key:guint; accel_mods:TGdkModifierType; replace:gboolean):gboolean; cdecl; external gtklib;
procedure gtk_accel_map_load(file_name:Pgchar); cdecl; external gtklib;
procedure gtk_accel_map_save(file_name:Pgchar); cdecl; external gtklib;
procedure gtk_accel_map_foreach(data:gpointer; foreach_func:TGtkAccelMapForeach); cdecl; external gtklib;
procedure gtk_accel_map_load_fd(fd:gint); cdecl; external gtklib;
procedure gtk_accel_map_load_scanner(scanner:PGScanner); cdecl; external gtklib;
procedure gtk_accel_map_save_fd(fd:gint); cdecl; external gtklib;
{ --- filter functions ---  }
procedure gtk_accel_map_add_filter(filter_pattern:Pgchar); cdecl; external gtklib;
procedure gtk_accel_map_foreach_unfiltered(data:gpointer; foreach_func:TGtkAccelMapForeach); cdecl; external gtklib;
{ --- internal API ---  }
procedure _gtk_accel_map_init; cdecl; external gtklib;
procedure _gtk_accel_map_add_group(accel_path:Pgchar; accel_group:PGtkAccelGroup); cdecl; external gtklib;
procedure _gtk_accel_map_remove_group(accel_path:Pgchar; accel_group:PGtkAccelGroup); cdecl; external gtklib;
function _gtk_accel_path_is_valid(accel_path:Pgchar):gboolean; cdecl; external gtklib;
{$endif read_interface_rest}
// included by gtk2.pas

