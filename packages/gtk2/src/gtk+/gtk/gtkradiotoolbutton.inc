

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkRadioToolButton = ^TGtkRadioToolButton;
   TGtkRadioToolButton = record
        parent : TGtkToggleToolButton;
     end;

{ Padding for future expansion  }
   PGtkRadioToolButtonClass = ^TGtkRadioToolButtonClass;
   TGtkRadioToolButtonClass = record
        parent_class : TGtkToggleToolButtonClass;
        _gtk_reserved1 : procedure ;cdecl;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;
{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}
function GTK_TYPE_RADIO_TOOL_BUTTON : GType;
function GTK_RADIO_TOOL_BUTTON(obj : pointer) : PGtkRadioToolButton;
function GTK_RADIO_TOOL_BUTTON_CLASS(klass : pointer) : PGtkRadioToolButtonClass;
function GTK_IS_RADIO_TOOL_BUTTON(obj : pointer) : gboolean;
function GTK_IS_RADIO_TOOL_BUTTON_CLASS(klass : pointer) : gboolean;
function GTK_RADIO_TOOL_BUTTON_GET_CLASS(obj : pointer) : PGtkRadioToolButtonClass;


function gtk_radio_tool_button_get_type:GType;cdecl;external gtklib name 'gtk_radio_tool_button_get_type';
function gtk_radio_tool_button_new(group:PGSList):PGtkToolItem;cdecl;external gtklib name 'gtk_radio_tool_button_new';

function gtk_radio_tool_button_new_from_stock(group:PGSList; stock_id:Pgchar):PGtkToolItem;cdecl;external gtklib name 'gtk_radio_tool_button_new_from_stock';
function gtk_radio_tool_button_new_from_widget(group:PGtkRadioToolButton):PGtkToolItem;cdecl;external gtklib name 'gtk_radio_tool_button_new_from_widget';

function gtk_radio_tool_button_new_with_stock_from_widget(group:PGtkRadioToolButton; stock_id:Pgchar):PGtkToolItem;cdecl;external gtklib name 'gtk_radio_tool_button_new_with_stock_from_widget';
function gtk_radio_tool_button_get_group(button:PGtkRadioToolButton):PGSList;cdecl;external gtklib name 'gtk_radio_tool_button_get_group';
procedure gtk_radio_tool_button_set_group(button:PGtkRadioToolButton; group:PGSList);cdecl;external gtklib name 'gtk_radio_tool_button_set_group';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_RADIO_TOOL_BUTTON : GType;
begin
   GTK_TYPE_RADIO_TOOL_BUTTON:=gtk_radio_tool_button_get_type;
end;

function GTK_RADIO_TOOL_BUTTON(obj : pointer) : PGtkRadioToolButton;
begin
   GTK_RADIO_TOOL_BUTTON:=PGtkRadioToolButton(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_RADIO_TOOL_BUTTON));
end;

function GTK_RADIO_TOOL_BUTTON_CLASS(klass : pointer) : PGtkRadioToolButtonClass;
begin
   GTK_RADIO_TOOL_BUTTON_CLASS:=PGtkRadioToolButtonClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_RADIO_TOOL_BUTTON));
end;

function GTK_IS_RADIO_TOOL_BUTTON(obj : pointer) : gboolean;
begin
   GTK_IS_RADIO_TOOL_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_RADIO_TOOL_BUTTON);
end;

function GTK_IS_RADIO_TOOL_BUTTON_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_RADIO_TOOL_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_RADIO_TOOL_BUTTON);
end;


function GTK_RADIO_TOOL_BUTTON_GET_CLASS(obj : pointer) : PGtkRadioToolButtonClass;
begin
   GTK_RADIO_TOOL_BUTTON_GET_CLASS:=PGtkRadioToolButtonClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_RADIO_TOOL_BUTTON));
end;


{$endif read_implementation}
