
// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkRadioActionPrivate = pointer;

   PGtkRadioAction = ^TGtkRadioAction;
   TGtkRadioAction = record
        parent : TGtkToggleAction;
        private_data : PGtkRadioActionPrivate;
     end;

{ Padding for future expansion  }
   PGtkRadioActionClass = ^TGtkRadioActionClass;
   TGtkRadioActionClass = record
        parent_class : TGtkToggleActionClass;
        changed : procedure (action:PGtkRadioAction; current:PGtkRadioAction);cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_RADIO_ACTION : GType;
function GTK_RADIO_ACTION(obj : pointer) : PGtkRadioAction;
function GTK_RADIO_ACTION_CLASS(klass : pointer) : PGtkRadioActionClass;
function GTK_IS_RADIO_ACTION(obj : pointer) : gboolean;
function GTK_IS_RADIO_ACTION_CLASS(klass : pointer) : gboolean;
function GTK_RADIO_ACTION_GET_CLASS(obj : pointer) : PGtkRadioActionClass;

function gtk_radio_action_get_type:GType;cdecl;external gtklib name 'gtk_radio_action_get_type';
function gtk_radio_action_new(name:Pgchar; szLabel:Pgchar; tooltip:Pgchar; stock_id:Pgchar; value:gint):PGtkRadioAction;cdecl;external gtklib name 'gtk_radio_action_new';
function gtk_radio_action_get_group(action:PGtkRadioAction):PGSList;cdecl;external gtklib name 'gtk_radio_action_get_group';
procedure gtk_radio_action_set_group(action:PGtkRadioAction; group:PGSList);cdecl;external gtklib name 'gtk_radio_action_set_group';
function gtk_radio_action_get_current_value(action:PGtkRadioAction):gint;cdecl;external gtklib name 'gtk_radio_action_get_current_value';

{$ENDIF read_interface_rest}

{$IFDEF read_implementation}

function GTK_TYPE_RADIO_ACTION : GType;
begin
   GTK_TYPE_RADIO_ACTION:=gtk_radio_action_get_type;
end;

function GTK_RADIO_ACTION(obj : pointer) : PGtkRadioAction;
begin
   GTK_RADIO_ACTION:=PGtkRadioAction(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_RADIO_ACTION));
end;

function GTK_RADIO_ACTION_CLASS(klass : pointer) : PGtkRadioActionClass;
begin
   GTK_RADIO_ACTION_CLASS:=PGtkRadioActionClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_RADIO_ACTION));
end;

function GTK_IS_RADIO_ACTION(obj : pointer) : gboolean;
begin
   GTK_IS_RADIO_ACTION:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_RADIO_ACTION);
end;

function GTK_IS_RADIO_ACTION_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_RADIO_ACTION_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_RADIO_ACTION);
end;

function GTK_RADIO_ACTION_GET_CLASS(obj : pointer) : PGtkRadioActionClass;
begin
   GTK_RADIO_ACTION_GET_CLASS:=PGtkRadioActionClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_RADIO_ACTION));
end;

{$ENDIF read_implementation}
