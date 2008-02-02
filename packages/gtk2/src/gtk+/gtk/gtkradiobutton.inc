// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkRadioButton = ^TGtkRadioButton;
   TGtkRadioButton = record
        check_button : TGtkCheckButton;
        group : PGSList;
     end;

{ Padding for future expansion  }
   PGtkRadioButtonClass = ^TGtkRadioButtonClass;
   TGtkRadioButtonClass = record
        parent_class : TGtkCheckButtonClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_RADIO_BUTTON : GType;
function GTK_RADIO_BUTTON(obj: pointer) : PGtkRadioButton;
function GTK_RADIO_BUTTON_CLASS(klass: pointer) : PGtkRadioButtonClass;
function GTK_IS_RADIO_BUTTON(obj: pointer) : boolean;
function GTK_IS_RADIO_BUTTON_CLASS(klass: pointer) : boolean;
function GTK_RADIO_BUTTON_GET_CLASS(obj: pointer) : PGtkRadioButtonClass;


function gtk_radio_button_get_type:TGtkType; cdecl; external gtklib;
function gtk_radio_button_new(group:PGSList):PGtkWidget; cdecl; external gtklib;
function gtk_radio_button_new_from_widget(group:PGtkRadioButton):PGtkWidget; cdecl; external gtklib;
function gtk_radio_button_new_with_label(group:PGSList; _label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_radio_button_new_with_label_from_widget(group:PGtkRadioButton; _label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_radio_button_new_with_mnemonic(group:PGSList; _label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_radio_button_new_with_mnemonic_from_widget(group:PGtkRadioButton; _label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_radio_button_get_group(radio_button:PGtkRadioButton):PGSList; cdecl; external gtklib;
procedure gtk_radio_button_set_group(radio_button:PGtkRadioButton; group:PGSList); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_radio_button_group(radio_button:PGtkRadioButton):PGSList;
{$endif}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_RADIO_BUTTON : GType;
begin
   GTK_TYPE_RADIO_BUTTON:=gtk_radio_button_get_type;
end;

function GTK_RADIO_BUTTON(obj: pointer) : PGtkRadioButton;
begin
   GTK_RADIO_BUTTON:=PGtkRadioButton(GTK_CHECK_CAST(obj,GTK_TYPE_RADIO_BUTTON));
end;

function GTK_RADIO_BUTTON_CLASS(klass: pointer) : PGtkRadioButtonClass;
begin
   GTK_RADIO_BUTTON_CLASS:=PGtkRadioButtonClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_RADIO_BUTTON));
end;

function GTK_IS_RADIO_BUTTON(obj: pointer) : boolean;
begin
   GTK_IS_RADIO_BUTTON:=GTK_CHECK_TYPE(obj,GTK_TYPE_RADIO_BUTTON);
end;

function GTK_IS_RADIO_BUTTON_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_RADIO_BUTTON_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_RADIO_BUTTON);
end;

function GTK_RADIO_BUTTON_GET_CLASS(obj: pointer) : PGtkRadioButtonClass;
begin
   GTK_RADIO_BUTTON_GET_CLASS:=PGtkRadioButtonClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_RADIO_BUTTON));
end;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_radio_button_group(radio_button:PGtkRadioButton):PGSList;
begin
   gtk_radio_button_group:=gtk_radio_button_get_group(radio_button);
end;
{$endif}

{$ENDIF read_implementation}
// included by gtk2.pas

