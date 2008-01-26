// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ --- accel flags ---  }
{ display in GtkAccelLabel?  }
{ is it removable?  }
   PGtkAccelFlags = ^TGtkAccelFlags;
   TGtkAccelFlags = longint;

{ --- typedefs & structures ---  }
   PGtkAccelGroup = ^TGtkAccelGroup;
   PGtkAccelGroupEntry = ^TGtkAccelGroupEntry;

   TGtkAccelGroupActivate = function (accel_group:PGtkAccelGroup; acceleratable:PGObject; keyval:guint; modifier:TGdkModifierType):gboolean; cdecl;
   TGtkAccelGroup = record
        parent : TGObject;
        lock_count : guint;
        modifier_mask : TGdkModifierType;
        acceleratables : PGSList;
        n_accels : guint;
        priv_accels : PGtkAccelGroupEntry;
     end;

{ Padding for future expansion  }
   PGtkAccelGroupClass = ^TGtkAccelGroupClass;
   TGtkAccelGroupClass = record
        parent_class : TGObjectClass;
        accel_changed : procedure (accel_group:PGtkAccelGroup; keyval:guint; modifier:TGdkModifierType; accel_closure:PGClosure); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

   PGtkAccelKey = ^TGtkAccelKey;
   TGtkAccelKey = record
        accel_key : guint;
        accel_mods : TGdkModifierType;
        flag0 : word;
     end;

   TGtkAccelGroupEntry = record
        key : TGtkAccelKey;
        closure : PGClosure;
        accel_path_quark : TGQuark;
     end;

  Tgtk_accel_group_find_func = function (key:PGtkAccelKey; closure:PGClosure; data:gpointer):gboolean; cdecl;
  TGtkAccelGroupFindFunc = Tgtk_accel_group_find_func;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_ACCEL_VISIBLE = 1 shl 0;
   GTK_ACCEL_LOCKED = 1 shl 1;
   GTK_ACCEL_MASK = $07;

   bm_TGtkAccelKey_accel_flags = $FFFF;
   bp_TGtkAccelKey_accel_flags = 0;


{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_accel_group_ref(AccelGroup: PGtkAccelGroup);
procedure gtk_accel_group_unref(AccelGroup: PGtkAccelGroup);
{$endif}

{ --- type macros ---  }
function GTK_TYPE_ACCEL_GROUP : GType;
function GTK_ACCEL_GROUP(anObject: pointer) : PGtkAccelGroup;
function GTK_ACCEL_GROUP_CLASS(klass: pointer) : PGtkAccelGroupClass;
function GTK_IS_ACCEL_GROUP(anObject: pointer) : boolean;
function GTK_IS_ACCEL_GROUP_CLASS(klass: pointer) : boolean;
function GTK_ACCEL_GROUP_GET_CLASS(obj: pointer) : PGtkAccelGroupClass;

function accel_flags(var a : TGtkAccelKey) : guint;
procedure set_accel_flags(var a : TGtkAccelKey; __accel_flags : guint);
{ -- Accelerator Groups ---  }

function gtk_accel_group_get_type:GType; cdecl; external gtklib;
function gtk_accel_group_new:PGtkAccelGroup; cdecl; external gtklib;
procedure gtk_accel_group_lock(accel_group:PGtkAccelGroup); cdecl; external gtklib;
procedure gtk_accel_group_unlock(accel_group:PGtkAccelGroup); cdecl; external gtklib;
procedure gtk_accel_group_connect(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType; accel_flags:TGtkAccelFlags; closure:PGClosure); cdecl; external gtklib;
procedure gtk_accel_group_connect_by_path(accel_group:PGtkAccelGroup; accel_path:Pgchar; closure:PGClosure); cdecl; external gtklib;
function gtk_accel_group_disconnect(accel_group:PGtkAccelGroup; closure:PGClosure):gboolean; cdecl; external gtklib;
function gtk_accel_group_disconnect_key(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType):gboolean; cdecl; external gtklib;
{ --- GtkActivatable glue ---  }
procedure _gtk_accel_group_attach(accel_group:PGtkAccelGroup; anObject:PGObject); cdecl; external gtklib;
procedure _gtk_accel_group_detach(accel_group:PGtkAccelGroup; anObject:PGObject); cdecl; external gtklib;
function gtk_accel_groups_activate(anObject:PGObject; accel_key:guint; accel_mods:TGdkModifierType):gboolean; cdecl; external gtklib;
function gtk_accel_groups_from_object(anObject:PGObject):PGSList; cdecl; external gtklib;
function gtk_accel_group_find(accel_group:PGtkAccelGroup; find_func:Tgtk_accel_group_find_func; data:gpointer):PGtkAccelKey; cdecl; external gtklib;
function gtk_accel_group_from_accel_closure(closure:PGClosure):PGtkAccelGroup; cdecl; external gtklib;
{ --- Accelerators---  }
function gtk_accelerator_valid(keyval:guint; modifiers:TGdkModifierType):gboolean; cdecl; external gtklib;
procedure gtk_accelerator_parse(accelerator:Pgchar; accelerator_key:Pguint; accelerator_mods:PGdkModifierType); cdecl; external gtklib;
function gtk_accelerator_name(accelerator_key:guint; accelerator_mods:TGdkModifierType):Pgchar; cdecl; external gtklib;
procedure gtk_accelerator_set_default_mod_mask(default_mod_mask:TGdkModifierType); cdecl; external gtklib;
function gtk_accelerator_get_default_mod_mask:guint; cdecl; external gtklib;

{ --- internal ---  }
function gtk_accel_group_query(accel_group:PGtkAccelGroup; accel_key:guint; accel_mods:TGdkModifierType; n_entries:Pguint):PGtkAccelGroupEntry; cdecl; external gtklib;
procedure _gtk_accel_group_reconnect(accel_group:PGtkAccelGroup; accel_path_quark:TGQuark); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ACCEL_GROUP : GType;
begin
   GTK_TYPE_ACCEL_GROUP:=gtk_accel_group_get_type;
end;

function GTK_ACCEL_GROUP(anObject: pointer) : PGtkAccelGroup;
begin
   GTK_ACCEL_GROUP:=PGtkAccelGroup(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_ACCEL_GROUP));
end;

function GTK_ACCEL_GROUP_CLASS(klass: pointer) : PGtkAccelGroupClass;
begin
   GTK_ACCEL_GROUP_CLASS:=PGtkAccelGroupClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_ACCEL_GROUP));
end;

function GTK_IS_ACCEL_GROUP(anObject: pointer) : boolean;
begin
   GTK_IS_ACCEL_GROUP:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_ACCEL_GROUP);
end;

function GTK_IS_ACCEL_GROUP_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ACCEL_GROUP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_ACCEL_GROUP);
end;

function GTK_ACCEL_GROUP_GET_CLASS(obj: pointer) : PGtkAccelGroupClass;
begin
   GTK_ACCEL_GROUP_GET_CLASS:=PGtkAccelGroupClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_ACCEL_GROUP));
end;

function accel_flags(var a : TGtkAccelKey) : guint;
begin
   accel_flags:=(a.flag0 and bm_TGtkAccelKey_accel_flags) shr bp_TGtkAccelKey_accel_flags;
end;

procedure set_accel_flags(var a : TGtkAccelKey; __accel_flags : guint);
begin
   a.flag0:=a.flag0 or ((__accel_flags shl bp_TGtkAccelKey_accel_flags) and bm_TGtkAccelKey_accel_flags);
end;

procedure gtk_accel_group_ref(AccelGroup: PGtkAccelGroup);
begin
  g_object_ref(AccelGroup);
end;

procedure gtk_accel_group_unref(AccelGroup: PGtkAccelGroup);
begin
  g_object_unref(AccelGroup);
end;
{$ENDIF read_implementation}
// included by gtk2.pas

