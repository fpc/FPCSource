// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkActionPrivate = pointer;

   PGtkAction = ^TGtkAction;
   TGtkAction = record
        _object : TGObject;
        private_data : PGtkActionPrivate;
     end;

{ activation signal  }
{ widget creation routines (not signals)  }
{ Padding for future expansion  }
   PGtkActionClass = ^TGtkActionClass;
   TGtkActionClass = record
        parent_class : TGObjectClass;
        activate : procedure (action:PGtkAction);cdecl;
        menu_item_type : GType;
        toolbar_item_type : GType;
        create_menu_item : function (action:PGtkAction):PGtkWidget;
        create_tool_item : function (action:PGtkAction):PGtkWidget;
        connect_proxy : procedure (action:PGtkAction; proxy:PGtkWidget);
        disconnect_proxy : procedure (action:PGtkAction; proxy:PGtkWidget);
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}


function GTK_TYPE_ACTION : GType;

function GTK_ACTION(obj : pointer) : PGtkAction;

function GTK_ACTION_CLASS(klass : pointer) : PGtkActionClass;

function GTK_IS_ACTION(obj : pointer) : gboolean;

function GTK_IS_ACTION_CLASS(klass : pointer) : gboolean;

function GTK_ACTION_GET_CLASS(obj : pointer) : PGtkActionClass;



function gtk_action_get_type:GType;cdecl;external gtklib name 'gtk_action_get_type';
function gtk_action_new(name:Pgchar; szLabel:Pgchar; tooltip:Pgchar; stock_id:Pgchar):PGtkAction;cdecl;external gtklib name 'gtk_action_new';

function gtk_action_get_name(action:PGtkAction):Pgchar;cdecl;external gtklib name 'gtk_action_get_name';
function gtk_action_is_sensitive(action:PGtkAction):gboolean;cdecl;external gtklib name 'gtk_action_is_sensitive';
function gtk_action_get_sensitive(action:PGtkAction):gboolean;cdecl;external gtklib name 'gtk_action_get_sensitive';
function gtk_action_is_visible(action:PGtkAction):gboolean;cdecl;external gtklib name 'gtk_action_is_visible';
function gtk_action_get_visible(action:PGtkAction):gboolean;cdecl;external gtklib name 'gtk_action_get_visible';
procedure gtk_action_activate(action:PGtkAction);cdecl;external gtklib name 'gtk_action_activate';
function gtk_action_create_icon(action:PGtkAction; icon_size:TGtkIconSize):PGtkWidget;cdecl;external gtklib name 'gtk_action_create_icon';
function gtk_action_create_menu_item(action:PGtkAction):PGtkWidget;cdecl;external gtklib name 'gtk_action_create_menu_item';
function gtk_action_create_tool_item(action:PGtkAction):PGtkWidget;cdecl;external gtklib name 'gtk_action_create_tool_item';
procedure gtk_action_connect_proxy(action:PGtkAction; proxy:PGtkWidget);cdecl;external gtklib name 'gtk_action_connect_proxy';
procedure gtk_action_disconnect_proxy(action:PGtkAction; proxy:PGtkWidget);cdecl;external gtklib name 'gtk_action_disconnect_proxy';
function gtk_action_get_proxies(action:PGtkAction):PGSList;cdecl;external gtklib name 'gtk_action_get_proxies';
procedure gtk_action_connect_accelerator(action:PGtkAction);cdecl;external gtklib name 'gtk_action_connect_accelerator';
procedure gtk_action_disconnect_accelerator(action:PGtkAction);cdecl;external gtklib name 'gtk_action_disconnect_accelerator';

{ protected ... for use by child actions  }
procedure gtk_action_block_activate_from(action:PGtkAction; proxy:PGtkWidget);cdecl;external gtklib name 'gtk_action_block_activate_from';
procedure gtk_action_unblock_activate_from(action:PGtkAction; proxy:PGtkWidget);cdecl;external gtklib name 'gtk_action_unblock_activate_from';
procedure _gtk_action_emit_activate(action:PGtkAction);cdecl;external gtklib name '_gtk_action_emit_activate';

{ protected ... for use by action groups  }

procedure gtk_action_set_accel_path(action:PGtkAction; accel_path:Pgchar);cdecl;external gtklib name 'gtk_action_set_accel_path';
procedure gtk_action_set_accel_group(action:PGtkAction; accel_group:PGtkAccelGroup);cdecl;external gtklib name 'gtk_action_set_accel_group';
{$endif read_interface_rest}


{$ifdef read_implementation}
function GTK_TYPE_ACTION : GType;
begin
   GTK_TYPE_ACTION:=gtk_action_get_type;
end;

function GTK_ACTION(obj : pointer) : PGtkAction;
begin
   GTK_ACTION:=PGtkAction(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_ACTION));
end;

function GTK_ACTION_CLASS(klass : pointer) : PGtkActionClass;
begin
   GTK_ACTION_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_ACTION);
end;

function GTK_IS_ACTION(obj : pointer) : gboolean;
begin
   GTK_IS_ACTION:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_ACTION);
end;

function GTK_IS_ACTION_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_ACTION_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_ACTION);
end;

function GTK_ACTION_GET_CLASS(obj : pointer) : PGtkActionClass;
begin
   GTK_ACTION_GET_CLASS:=PGtkActionClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_ACTION));
end;

{$endif read_implementation}
