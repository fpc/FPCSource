{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkActionGroupPrivate = pointer;

   PGtkActionGroup = ^TGtkActionGroup;
   TGtkActionGroup = record
        parent : TGObject;
        private_data : PGtkActionGroupPrivate;
     end;

{ Padding for future expansion  }
   PGtkActionGroupClass = ^TGtkActionGroupClass;
   TGtkActionGroupClass = record
        parent_class : TGObjectClass;
        get_action : function (action_group:PGtkActionGroup; action_name:Pgchar):PGtkAction;cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

  (* prefixed pgchar types  with sz due name conflict with label *)
   PGtkActionEntry = ^TGtkActionEntry;
   TGtkActionEntry = record
        szName : Pgchar;
        szStock_id : Pgchar;
        szLabel : Pgchar;
        szAccelerator : Pgchar;
        szTooltip : Pgchar;
        callback : TGCallback;
     end;

   PGtkToggleActionEntry = ^TGtkToggleActionEntry;
   TGtkToggleActionEntry = record
        szName : Pgchar;
        szStock_id : Pgchar;
        szLabel : Pgchar;
        szAccelerator : Pgchar;
        szTooltip : Pgchar;
        callback : TGCallback;
        is_active : gboolean;
     end;

   PGtkRadioActionEntry = ^TGtkRadioActionEntry;
   TGtkRadioActionEntry = record
        szName : Pgchar;
        szStock_id : Pgchar;
        szLabel : Pgchar;
        szAccelerator : Pgchar;
        szTooltip : Pgchar;
        value : gint;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_ACTION_GROUP : GType;
function GTK_ACTION_GROUP(obj : pointer) : PGtkActionGroup;
function GTK_ACTION_GROUP_CLASS(vtable : pointer) : PGtkActionGroupClass;
function GTK_IS_ACTION_GROUP(obj : pointer) : gboolean;
function GTK_IS_ACTION_GROUP_CLASS(vtable : pointer) : gboolean;
function GTK_ACTION_GROUP_GET_CLASS(inst : pointer) : PGtkActionGroupClass;



function gtk_action_group_get_type:GType;cdecl;external gtklib name 'gtk_action_group_get_type';

function gtk_action_group_new(name:Pgchar):PGtkActionGroup;cdecl;external gtklib name 'gtk_action_group_new';

function gtk_action_group_get_name(action_group:PGtkActionGroup):Pgchar;cdecl;external gtklib name 'gtk_action_group_get_name';
function gtk_action_group_get_sensitive(action_group:PGtkActionGroup):gboolean;cdecl;external gtklib name 'gtk_action_group_get_sensitive';
procedure gtk_action_group_set_sensitive(action_group:PGtkActionGroup; sensitive:gboolean);cdecl;external gtklib name 'gtk_action_group_set_sensitive';
function gtk_action_group_get_visible(action_group:PGtkActionGroup):gboolean;cdecl;external gtklib name 'gtk_action_group_get_visible';
procedure gtk_action_group_set_visible(action_group:PGtkActionGroup; visible:gboolean);cdecl;external gtklib name 'gtk_action_group_set_visible';

function gtk_action_group_get_action(action_group:PGtkActionGroup; action_name:Pgchar):PGtkAction;cdecl;external gtklib name 'gtk_action_group_get_action';
function gtk_action_group_list_actions(action_group:PGtkActionGroup):PGList;cdecl;external gtklib name 'gtk_action_group_list_actions';
procedure gtk_action_group_add_action(action_group:PGtkActionGroup; action:PGtkAction);cdecl;external gtklib name 'gtk_action_group_add_action';

procedure gtk_action_group_add_action_with_accel(action_group:PGtkActionGroup; action:PGtkAction; accelerator:Pgchar);cdecl;external gtklib name 'gtk_action_group_add_action_with_accel';
procedure gtk_action_group_remove_action(action_group:PGtkActionGroup; action:PGtkAction);cdecl;external gtklib name 'gtk_action_group_remove_action';
procedure gtk_action_group_add_actions(action_group:PGtkActionGroup; entries:PGtkActionEntry; n_entries:guint; user_data:gpointer);cdecl;external gtklib name 'gtk_action_group_add_actions';
procedure gtk_action_group_add_toggle_actions(action_group:PGtkActionGroup; entries:PGtkToggleActionEntry; n_entries:guint; user_data:gpointer);cdecl;external gtklib name 'gtk_action_group_add_toggle_actions';
procedure gtk_action_group_add_radio_actions(action_group:PGtkActionGroup; entries:PGtkRadioActionEntry; n_entries:guint; value:gint; on_change:TGCallback;
            user_data:gpointer);cdecl;external gtklib name 'gtk_action_group_add_radio_actions';
procedure gtk_action_group_add_actions_full(action_group:PGtkActionGroup; entries:PGtkActionEntry; n_entries:guint; user_data:gpointer; destroy:TGDestroyNotify);cdecl;external gtklib name 'gtk_action_group_add_actions_full';
procedure gtk_action_group_add_toggle_actions_full(action_group:PGtkActionGroup; entries:PGtkToggleActionEntry; n_entries:guint; user_data:gpointer; destroy:TGDestroyNotify);cdecl;external gtklib name 'gtk_action_group_add_toggle_actions_full';
procedure gtk_action_group_add_radio_actions_full(action_group:PGtkActionGroup; entries:PGtkRadioActionEntry; n_entries:guint; value:gint; on_change:TGCallback;
            user_data:gpointer; destroy:TGDestroyNotify);cdecl;external gtklib name 'gtk_action_group_add_radio_actions_full';

procedure gtk_action_group_set_translate_func(action_group:PGtkActionGroup; func:TGtkTranslateFunc; data:gpointer; notify:TGtkDestroyNotify);cdecl;external gtklib name 'gtk_action_group_set_translate_func';

procedure gtk_action_group_set_translation_domain(action_group:PGtkActionGroup; domain:Pgchar);cdecl;external gtklib name 'gtk_action_group_set_translation_domain';


{ Protected for use by GtkAction  }
procedure _gtk_action_group_emit_connect_proxy(action_group:PGtkActionGroup; action:PGtkAction; proxy:PGtkWidget);cdecl;external gtklib name '_gtk_action_group_emit_connect_proxy';
procedure _gtk_action_group_emit_disconnect_proxy(action_group:PGtkActionGroup; action:PGtkAction; proxy:PGtkWidget);cdecl;external gtklib name '_gtk_action_group_emit_disconnect_proxy';
procedure _gtk_action_group_emit_pre_activate(action_group:PGtkActionGroup; action:PGtkAction);cdecl;external gtklib name '_gtk_action_group_emit_pre_activate';
procedure _gtk_action_group_emit_post_activate(action_group:PGtkActionGroup; action:PGtkAction);cdecl;external gtklib name '_gtk_action_group_emit_post_activate';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_ACTION_GROUP : GType;
begin
   GTK_TYPE_ACTION_GROUP:=gtk_action_group_get_type;
end;

function GTK_ACTION_GROUP(obj : pointer) : PGtkActionGroup;
begin
   GTK_ACTION_GROUP:=PGtkActionGroup(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_ACTION_GROUP));
end;

function GTK_ACTION_GROUP_CLASS(vtable : pointer) : PGtkActionGroupClass;
begin
   GTK_ACTION_GROUP_CLASS:=PGtkActionGroupClass(G_TYPE_CHECK_CLASS_CAST(vtable,GTK_TYPE_ACTION_GROUP));
end;

function GTK_IS_ACTION_GROUP(obj : pointer) : gboolean;
begin
   GTK_IS_ACTION_GROUP:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_ACTION_GROUP);
end;

function GTK_IS_ACTION_GROUP_CLASS(vtable : pointer) : gboolean;
begin
   GTK_IS_ACTION_GROUP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(vtable,GTK_TYPE_ACTION_GROUP);
end;

function GTK_ACTION_GROUP_GET_CLASS(inst : pointer) : PGtkActionGroupClass;
begin
   GTK_ACTION_GROUP_GET_CLASS:=PGtkActionGroupClass(G_TYPE_INSTANCE_GET_CLASS(inst,GTK_TYPE_ACTION_GROUP));
end;


{$endif read_implementation}
