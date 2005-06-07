{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkAccelFlags = (
       GTK_ACCEL_VISIBLE := 1 shl 0,
       GTK_ACCEL_SIGNAL_VISIBLE := 1 shl 1,
       GTK_ACCEL_LOCKED := 1 shl 2,
       GTK_ACCEL_MASK := $07
       );

     PGtkAccelGroup = ^TGtkAccelGroup;
     TGtkAccelGroup = record
          ref_count : guint;
          lock_count : guint;
          modifier_mask : TGdkModifierType;
          attach_objects : PGSList;
       end;

     PGtkAccelEntry = ^TGtkAccelEntry;
     TGtkAccelEntry = record
          accel_group : PGtkAccelGroup;
          accelerator_key : guint;
          accelerator_mods : TGdkModifierType;
          accel_flags : TGtkAccelFlags;
          _object : PGtkObject;
          signal_id : guint;
       end;

function  gtk_accel_group_new:PGtkAccelGroup;cdecl;external gtkdll name 'gtk_accel_group_new';
function  gtk_accel_group_get_default:PGtkAccelGroup;cdecl;external gtkdll name 'gtk_accel_group_get_default';
function  gtk_accel_group_ref(accel_group:PGtkAccelGroup):PGtkAccelGroup;cdecl;external gtkdll name 'gtk_accel_group_ref';
procedure gtk_accel_group_unref(accel_group:PGtkAccelGroup);cdecl;external gtkdll name 'gtk_accel_group_unref';
function  gtk_accel_group_activate(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType):gboolean;cdecl;external gtkdll name 'gtk_accel_group_activate';
function  gtk_accel_groups_activate(_object:PGtkObject; accel_key:guint; accel_mods:TGdkModifierType):gboolean;cdecl;external gtkdll name 'gtk_accel_groups_activate';
procedure gtk_accel_group_attach(accel_group:PGtkAccelGroup; _object:PGtkObject);cdecl;external gtkdll name 'gtk_accel_group_attach';
procedure gtk_accel_group_detach(accel_group:PGtkAccelGroup; _object:PGtkObject);cdecl;external gtkdll name 'gtk_accel_group_detach';
procedure gtk_accel_group_lock(accel_group:PGtkAccelGroup);cdecl;external gtkdll name 'gtk_accel_group_lock';
procedure gtk_accel_group_unlock(accel_group:PGtkAccelGroup);cdecl;external gtkdll name 'gtk_accel_group_unlock';
function  gtk_accel_group_get_entry(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType):PGtkAccelEntry;cdecl;external gtkdll name 'gtk_accel_group_get_entry';
procedure gtk_accel_group_lock_entry(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType);cdecl;external gtkdll name 'gtk_accel_group_lock_entry';
procedure gtk_accel_group_unlock_entry(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType);cdecl;external gtkdll name 'gtk_accel_group_unlock_entry';
procedure gtk_accel_group_add(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType; accel_flags:TGtkAccelFlags; _object:PGtkObject; accel_signal:Pgchar);cdecl;external gtkdll name 'gtk_accel_group_add';
procedure gtk_accel_group_remove(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType; _object:PGtkObject);cdecl;external gtkdll name 'gtk_accel_group_remove';
procedure gtk_accel_group_handle_add(_object:PGtkObject; accel_signal_id:guint; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType; accel_flags:TGtkAccelFlags);cdecl;external gtkdll name 'gtk_accel_group_handle_add';
procedure gtk_accel_group_handle_remove(_object:PGtkObject; accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType);cdecl;external gtkdll name 'gtk_accel_group_handle_remove';
function  gtk_accel_group_create_add(class_type:TGtkType; signal_flags:TGtkSignalRunType; handler_offset:guint):guint;cdecl;external gtkdll name 'gtk_accel_group_create_add';
function  gtk_accel_group_create_remove(class_type:TGtkType; signal_flags:TGtkSignalRunType; handler_offset:guint):guint;cdecl;external gtkdll name 'gtk_accel_group_create_remove';
{$ifndef gtkwin}
{$ifndef darwin}
procedure gtk_accel_group_marshal_add(_object:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_accel_group_marshal_add';
procedure gtk_accel_group_marshal_remove(_object:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;external gtkdll name 'gtk_accel_group_marshal_remove';
{$endif}
{$endif}
function  gtk_accel_groups_from_object(_object:PGtkObject):PGSList;cdecl;external gtkdll name 'gtk_accel_groups_from_object';
function  gtk_accel_group_entries_from_object(_object:PGtkObject):PGSList;cdecl;external gtkdll name 'gtk_accel_group_entries_from_object';
function  gtk_accelerator_valid(keyval:guint; modifiers:TGdkModifierType):gboolean;cdecl;external gtkdll name 'gtk_accelerator_valid';
procedure gtk_accelerator_parse(accelerator:Pgchar; accelerator_key:Pguint; accelerator_mods:PGdkModifierType);cdecl;external gtkdll name 'gtk_accelerator_parse';
function  gtk_accelerator_name(accelerator_key:guint; accelerator_mods:TGdkModifierType):Pgchar;cdecl;external gtkdll name 'gtk_accelerator_name';
procedure gtk_accelerator_set_default_mod_mask(default_mod_mask:TGdkModifierType);cdecl;external gtkdll name 'gtk_accelerator_set_default_mod_mask';
function  gtk_accelerator_get_default_mod_mask:guint;cdecl;external gtkdll name 'gtk_accelerator_get_default_mod_mask';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


