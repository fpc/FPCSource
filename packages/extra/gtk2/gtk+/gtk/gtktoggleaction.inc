// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkToggleActionPrivate = pointer;

   PGtkToggleAction = ^TGtkToggleAction;
   TGtkToggleAction = record
        parent : TGtkAction;
        private_data : PGtkToggleActionPrivate;
     end;

{ Padding for future expansion  }
   PGtkToggleActionClass = ^TGtkToggleActionClass;
   TGtkToggleActionClass = record
        parent_class : TGtkActionClass;
        toggled : procedure (action:PGtkToggleAction);cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;


{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_TOGGLE_ACTION : GType;
function GTK_TOGGLE_ACTION(obj : pointer) : PGtkToggleAction;
function GTK_TOGGLE_ACTION_CLASS(klass : pointer) : PGtkToggleActionClass;
function GTK_IS_TOGGLE_ACTION(obj : pointer) : gboolean;
function GTK_IS_TOGGLE_ACTION_CLASS(klass : pointer) : gboolean;
function GTK_TOGGLE_ACTION_GET_CLASS(obj : pointer) : PGtkToggleActionClass;


function gtk_toggle_action_get_type:GType;cdecl;external gtklib name 'gtk_toggle_action_get_type';

function gtk_toggle_action_new(name:Pgchar; szLabel:Pgchar; tooltip:Pgchar; stock_id:Pgchar):PGtkToggleAction;cdecl;external gtklib name 'gtk_toggle_action_new';
procedure gtk_toggle_action_toggled(action:PGtkToggleAction);cdecl;external gtklib name 'gtk_toggle_action_toggled';
procedure gtk_toggle_action_set_active(action:PGtkToggleAction; is_active:gboolean);cdecl;external gtklib name 'gtk_toggle_action_set_active';
function gtk_toggle_action_get_active(action:PGtkToggleAction):gboolean;cdecl;external gtklib name 'gtk_toggle_action_get_active';
procedure gtk_toggle_action_set_draw_as_radio(action:PGtkToggleAction; draw_as_radio:gboolean);cdecl;external gtklib name 'gtk_toggle_action_set_draw_as_radio';
function gtk_toggle_action_get_draw_as_radio(action:PGtkToggleAction):gboolean;cdecl;external gtklib name 'gtk_toggle_action_get_draw_as_radio';
{$ENDIF read_interface_rest}

{$IFDEF read_implementation}
function GTK_TYPE_TOGGLE_ACTION : GType;
begin
   Result:=gtk_toggle_action_get_type;
end;

function GTK_TOGGLE_ACTION(obj : pointer) : PGtkToggleAction;
begin
   Result:=PGtkToggleAction(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TOGGLE_ACTION));
end;

function GTK_TOGGLE_ACTION_CLASS(klass : pointer) : PGtkToggleActionClass;
begin
   Result:=PGtkToggleActionClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TOGGLE_ACTION));
end;

function GTK_IS_TOGGLE_ACTION(obj : pointer) : gboolean;
begin
   Result:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TOGGLE_ACTION);
end;

function GTK_IS_TOGGLE_ACTION_CLASS(klass : pointer) : gboolean;
begin
   Result:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOGGLE_ACTION);
end;

function GTK_TOGGLE_ACTION_GET_CLASS(obj : pointer) : PGtkToggleActionClass;
begin
   Result:=PGtkToggleActionClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TOGGLE_ACTION));
end;

{$ENDIF read_implementation}
