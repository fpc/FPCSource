// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCheckButton = ^TGtkCheckButton;
   TGtkCheckButton = record
        toggle_button : TGtkToggleButton;
     end;

{ Padding for future expansion  }
   PGtkCheckButtonClass = ^TGtkCheckButtonClass;
   TGtkCheckButtonClass = record
        parent_class : TGtkToggleButtonClass;
        draw_indicator : procedure (check_button:PGtkCheckButton; area:PGdkRectangle); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_CHECK_BUTTON : GType;
function GTK_CHECK_BUTTON(obj: pointer) : PGtkCheckButton;
function GTK_CHECK_BUTTON_CLASS(klass: pointer) : PGtkCheckButtonClass;
function GTK_IS_CHECK_BUTTON(obj: pointer) : boolean;
function GTK_IS_CHECK_BUTTON_CLASS(klass: pointer) : boolean;
function GTK_CHECK_BUTTON_GET_CLASS(obj: pointer) : PGtkCheckButtonClass;


function gtk_check_button_get_type:TGtkType; cdecl; external gtklib;
function gtk_check_button_new:PGtkWidget; cdecl; external gtklib;
function gtk_check_button_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_check_button_new_with_mnemonic(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure _gtk_check_button_get_props(check_button:PGtkCheckButton; indicator_size:Pgint; indicator_spacing:Pgint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CHECK_BUTTON : GType;
begin
   GTK_TYPE_CHECK_BUTTON:=gtk_check_button_get_type;
end;

function GTK_CHECK_BUTTON(obj: pointer) : PGtkCheckButton;
begin
   GTK_CHECK_BUTTON:=PGtkCheckButton(GTK_CHECK_CAST(obj,GTK_TYPE_CHECK_BUTTON));
end;

function GTK_CHECK_BUTTON_CLASS(klass: pointer) : PGtkCheckButtonClass;
begin
   GTK_CHECK_BUTTON_CLASS:=PGtkCheckButtonClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CHECK_BUTTON));
end;

function GTK_IS_CHECK_BUTTON(obj: pointer) : boolean;
begin
   GTK_IS_CHECK_BUTTON:=GTK_CHECK_TYPE(obj,GTK_TYPE_CHECK_BUTTON);
end;

function GTK_IS_CHECK_BUTTON_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CHECK_BUTTON_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CHECK_BUTTON);
end;

function GTK_CHECK_BUTTON_GET_CLASS(obj: pointer) : PGtkCheckButtonClass;
begin
   GTK_CHECK_BUTTON_GET_CLASS:=PGtkCheckButtonClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CHECK_BUTTON));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

