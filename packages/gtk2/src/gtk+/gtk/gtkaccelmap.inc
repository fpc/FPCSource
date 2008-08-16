// included by gtk2.pas

{$IFDEF read_forward_definitions}
  PGtkAccelMap = ^TGtkAccelMap;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  TGtkAccelMap = record
    parent_instance: TGObject;
  end;

  PGtkAccelMapClass = ^TGtkAccelMapClass;
  TGtkAccelMapClass = record
    parent_class: TGObjectClass;
  end;

{ --- notifier ---  }
   TGtkAccelMapForeach = procedure (data:gpointer; accel_path:Pgchar; accel_key:guint; accel_mods:TGdkModifierType; changed:gboolean); cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ACCEL_MAP : GType;
function GTK_ACCEL_MAP(obj: pointer) : PGtkAccelMap;
function GTK_ACCEL_MAP_CLASS(klass : pointer) : PGtkAccelMapClass;
function GTK_IS_ACCEL_MAP(obj: pointer) : boolean;
function GTK_IS_ACCEL_MAP_CLASS(klass : pointer) : boolean;
function GTK_ACCEL_MAP_GET_CLASS(obj: pointer) : PGtkAccelMapClass;

{ --- public API ---  }
function gtk_accel_map_get_type: GType; cdecl; external gtklib;

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

{$ifdef HasGTK2_4}
function gtk_accel_map_get: PGtkAccelMap; cdecl; external gtklib;
procedure gtk_accel_map_lock_path(accel_path: Pgchar); cdecl; external gtklib;
procedure gtk_accel_map_unlock_path(accel_path: Pgchar); cdecl; external gtklib;
{$endif}

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ACCEL_MAP : GType;
begin
   GTK_TYPE_ACCEL_MAP:=gtk_accel_map_get_type;
end;

function GTK_ACCEL_MAP(obj: pointer) : PGtkAccelMap;
begin
   GTK_ACCEL_MAP:=PGtkAccelMap(GTK_CHECK_CAST(obj,GTK_TYPE_ACCEL_MAP));
end;

function GTK_ACCEL_MAP_CLASS(klass: pointer) : PGtkAccelMapClass;
begin
   GTK_ACCEL_MAP_CLASS:=PGtkAccelMapClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ACCEL_MAP));
end;

function GTK_IS_ACCEL_MAP(obj: pointer) : boolean;
begin
   GTK_IS_ACCEL_MAP:=GTK_CHECK_TYPE(obj,GTK_TYPE_ACCEL_MAP);
end;

function GTK_IS_ACCEL_MAP_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ACCEL_MAP_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ACCEL_MAP);
end;

function GTK_ACCEL_MAP_GET_CLASS(obj: pointer) : PGtkAccelMapClass;
begin
   GTK_ACCEL_MAP_GET_CLASS:=PGtkAccelMapClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ACCEL_MAP));
end;
{$ENDIF read_implementation}
