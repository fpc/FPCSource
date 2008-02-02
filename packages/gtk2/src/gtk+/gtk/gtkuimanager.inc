// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkUIManagerPrivate = pointer;
   PGtkUIManager = ^TGtkUIManager;
   TGtkUIManager = record
        parent : TGObject;
        private_data : PGtkUIManagerPrivate;
     end;

{ Signals  }
{ Padding for future expansion  }
   PGtkUIManagerClass = ^TGtkUIManagerClass;
   TGtkUIManagerClass = record
        parent_class : TGObjectClass;
        add_widget : procedure (merge:PGtkUIManager; widget:PGtkWidget);cdecl;
        actions_changed : procedure (merge:PGtkUIManager);
        connect_proxy : procedure (merge:PGtkUIManager; action:PGtkAction; proxy:PGtkWidget);
        disconnect_proxy : procedure (merge:PGtkUIManager; action:PGtkAction; proxy:PGtkWidget);
        pre_activate : procedure (merge:PGtkUIManager; action:PGtkAction);
        post_activate : procedure (merge:PGtkUIManager; action:PGtkAction);
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;


   PGtkUIManagerItemType = ^TGtkUIManagerItemType;
   TGtkUIManagerItemType = (GTK_UI_MANAGER_AUTO := 0,
                            GTK_UI_MANAGER_MENUBAR := 1 shl 0,
                            GTK_UI_MANAGER_MENU := 1 shl 1,
                            GTK_UI_MANAGER_TOOLBAR := 1 shl 2,
                            GTK_UI_MANAGER_PLACEHOLDER := 1 shl 3,
                            GTK_UI_MANAGER_POPUP := 1 shl 4,
                            GTK_UI_MANAGER_MENUITEM := 1 shl 5,
                            GTK_UI_MANAGER_TOOLITEM := 1 shl 6,
                            GTK_UI_MANAGER_SEPARATOR := 1 shl 7,
                            GTK_UI_MANAGER_ACCELERATOR := 1 shl 8);

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_UI_MANAGER : GType;
function GTK_UI_MANAGER(obj : pointer) : PGtkUIManager;
function GTK_UI_MANAGER_CLASS(klass : pointer) : PGtkUIManagerClass;
function GTK_IS_UI_MANAGER(obj : pointer) : gboolean;
function GTK_IS_UI_MANAGER_CLASS(klass : pointer) : gboolean;
function GTK_UI_MANAGER_GET_CLASS(obj : pointer) : PGtkUIManagerClass;


function gtk_ui_manager_get_type:GType;cdecl;external gtklib name 'gtk_ui_manager_get_type';

function gtk_ui_manager_new:PGtkUIManager;cdecl;external gtklib name 'gtk_ui_manager_new';

procedure gtk_ui_manager_set_add_tearoffs(self:PGtkUIManager; add_tearoffs:gboolean);cdecl;external gtklib name 'gtk_ui_manager_set_add_tearoffs';
function gtk_ui_manager_get_add_tearoffs(self:PGtkUIManager):gboolean;cdecl;external gtklib name 'gtk_ui_manager_get_add_tearoffs';

procedure gtk_ui_manager_insert_action_group(self:PGtkUIManager; action_group:PGtkActionGroup; pos:gint);cdecl;external gtklib name 'gtk_ui_manager_insert_action_group';
procedure gtk_ui_manager_remove_action_group(self:PGtkUIManager; action_group:PGtkActionGroup);cdecl;external gtklib name 'gtk_ui_manager_remove_action_group';
function gtk_ui_manager_get_action_groups(self:PGtkUIManager):PGList;cdecl;external gtklib name 'gtk_ui_manager_get_action_groups';

function gtk_ui_manager_get_accel_group(self:PGtkUIManager):PGtkAccelGroup;cdecl;external gtklib name 'gtk_ui_manager_get_accel_group';

function gtk_ui_manager_get_widget(self:PGtkUIManager; path:Pgchar):PGtkWidget;cdecl;external gtklib name 'gtk_ui_manager_get_widget';

function gtk_ui_manager_get_toplevels(self:PGtkUIManager; types:TGtkUIManagerItemType):PGSList;cdecl;external gtklib name 'gtk_ui_manager_get_toplevels';

function gtk_ui_manager_get_action(self:PGtkUIManager; path:Pgchar):PGtkAction;cdecl;external gtklib name 'gtk_ui_manager_get_action';

function gtk_ui_manager_add_ui_from_string(self:PGtkUIManager; buffer:Pgchar; length:gssize; error:PPGError):guint;cdecl;external gtklib name 'gtk_ui_manager_add_ui_from_string';


function gtk_ui_manager_add_ui_from_file(self:PGtkUIManager; filename:Pgchar; error:PPGError):guint;cdecl;external gtklib name 'gtk_ui_manager_add_ui_from_file';

procedure gtk_ui_manager_add_ui(self:PGtkUIManager; merge_id:guint; path:Pgchar; name:Pgchar; action:Pgchar;
            _type:TGtkUIManagerItemType; top:gboolean);cdecl;external gtklib name 'gtk_ui_manager_add_ui';
procedure gtk_ui_manager_remove_ui(self:PGtkUIManager; merge_id:guint);cdecl;external gtklib name 'gtk_ui_manager_remove_ui';

function gtk_ui_manager_get_ui(self:PGtkUIManager):Pgchar;cdecl;external gtklib name 'gtk_ui_manager_get_ui';
procedure gtk_ui_manager_ensure_update(self:PGtkUIManager);cdecl;external gtklib name 'gtk_ui_manager_ensure_update';
function gtk_ui_manager_new_merge_id(self:PGtkUIManager):guint;cdecl;external gtklib name 'gtk_ui_manager_new_merge_id';

{$endif read_interface_rest}

{$ifdef read_implementation}
function GTK_TYPE_UI_MANAGER : GType;
begin
   GTK_TYPE_UI_MANAGER:=gtk_ui_manager_get_type;
end;

function GTK_UI_MANAGER(obj : pointer) : PGtkUIManager;
begin
   GTK_UI_MANAGER:=PGtkUIManager(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_UI_MANAGER));
end;

function GTK_UI_MANAGER_CLASS(klass : pointer) : PGtkUIManagerClass;
begin
   GTK_UI_MANAGER_CLASS:=PGtkUIManagerClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_UI_MANAGER));
end;

function GTK_IS_UI_MANAGER(obj : pointer) : gboolean;
begin
   GTK_IS_UI_MANAGER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_UI_MANAGER);
end;

function GTK_IS_UI_MANAGER_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_UI_MANAGER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_UI_MANAGER);
end;

function GTK_UI_MANAGER_GET_CLASS(obj : pointer) : PGtkUIManagerClass;
begin
   GTK_UI_MANAGER_GET_CLASS:=PGtkUIManagerClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_UI_MANAGER));
end;


{$endif read_implementation}
