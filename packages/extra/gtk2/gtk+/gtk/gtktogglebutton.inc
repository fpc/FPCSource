// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkToggleButton = ^TGtkToggleButton;
   TGtkToggleButton = record
        button : TGtkButton;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkToggleButtonClass = ^TGtkToggleButtonClass;
   TGtkToggleButtonClass = record
        parent_class : TGtkButtonClass;
        toggled : procedure (toggle_button:PGtkToggleButton); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkToggleButton_active = $1;
   bp_TGtkToggleButton_active = 0;
   bm_TGtkToggleButton_draw_indicator = $2;
   bp_TGtkToggleButton_draw_indicator = 1;
   bm_TGtkToggleButton_inconsistent = $4;
   bp_TGtkToggleButton_inconsistent = 2;

function GTK_TYPE_TOGGLE_BUTTON : GType;
function GTK_TOGGLE_BUTTON(obj: pointer) : PGtkToggleButton;
function GTK_TOGGLE_BUTTON_CLASS(klass: pointer) : PGtkToggleButtonClass;
function GTK_IS_TOGGLE_BUTTON(obj: pointer) : boolean;
function GTK_IS_TOGGLE_BUTTON_CLASS(klass: pointer) : boolean;
function GTK_TOGGLE_BUTTON_GET_CLASS(obj: pointer) : PGtkToggleButtonClass;

function active(var a : TGtkToggleButton) : guint; overload;
procedure set_active(var a : TGtkToggleButton; __active : guint); overload;
function draw_indicator(var a : TGtkToggleButton) : guint;
procedure set_draw_indicator(var a : TGtkToggleButton; __draw_indicator : guint);
function inconsistent(var a : TGtkToggleButton) : guint; overload;
procedure set_inconsistent(var a : TGtkToggleButton; __inconsistent : guint); overload;


function gtk_toggle_button_get_type:TGtkType; cdecl; external gtklib;
function gtk_toggle_button_new:PGtkWidget; cdecl; external gtklib;
function gtk_toggle_button_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_toggle_button_new_with_mnemonic(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_toggle_button_set_mode(toggle_button:PGtkToggleButton; draw_indicator:gboolean); cdecl; external gtklib;
function gtk_toggle_button_get_mode(toggle_button:PGtkToggleButton):gboolean; cdecl; external gtklib;
procedure gtk_toggle_button_set_active(toggle_button:PGtkToggleButton; is_active:gboolean); cdecl; external gtklib;
function gtk_toggle_button_get_active(toggle_button:PGtkToggleButton):gboolean; cdecl; external gtklib;
procedure gtk_toggle_button_toggled(toggle_button:PGtkToggleButton); cdecl; external gtklib;
procedure gtk_toggle_button_set_inconsistent(toggle_button:PGtkToggleButton; setting:gboolean); cdecl; external gtklib;
function gtk_toggle_button_get_inconsistent(toggle_button:PGtkToggleButton):gboolean; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_toggle_button_set_state(toggle_button:PGtkToggleButton; is_active:gboolean);
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TOGGLE_BUTTON : GType;
begin
   GTK_TYPE_TOGGLE_BUTTON:=gtk_toggle_button_get_type;
end;

function GTK_TOGGLE_BUTTON(obj: pointer) : PGtkToggleButton;
begin
   GTK_TOGGLE_BUTTON:=PGtkToggleButton(GTK_CHECK_CAST(obj,GTK_TYPE_TOGGLE_BUTTON));
end;

function GTK_TOGGLE_BUTTON_CLASS(klass: pointer) : PGtkToggleButtonClass;
begin
   GTK_TOGGLE_BUTTON_CLASS:=PGtkToggleButtonClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TOGGLE_BUTTON));
end;

function GTK_IS_TOGGLE_BUTTON(obj: pointer) : boolean;
begin
   GTK_IS_TOGGLE_BUTTON:=GTK_CHECK_TYPE(obj,GTK_TYPE_TOGGLE_BUTTON);
end;

function GTK_IS_TOGGLE_BUTTON_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TOGGLE_BUTTON_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOGGLE_BUTTON);
end;

function GTK_TOGGLE_BUTTON_GET_CLASS(obj: pointer) : PGtkToggleButtonClass;
begin
   GTK_TOGGLE_BUTTON_GET_CLASS:=PGtkToggleButtonClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TOGGLE_BUTTON));
end;

function active(var a : TGtkToggleButton) : guint;
begin
   active:=(a.flag0 and bm_TGtkToggleButton_active) shr bp_TGtkToggleButton_active;
end;

procedure set_active(var a : TGtkToggleButton; __active : guint);
begin
   a.flag0:=a.flag0 or ((__active shl bp_TGtkToggleButton_active) and bm_TGtkToggleButton_active);
end;

function draw_indicator(var a : TGtkToggleButton) : guint;
begin
   draw_indicator:=(a.flag0 and bm_TGtkToggleButton_draw_indicator) shr bp_TGtkToggleButton_draw_indicator;
end;

procedure set_draw_indicator(var a : TGtkToggleButton; __draw_indicator : guint);
begin
   a.flag0:=a.flag0 or ((__draw_indicator shl bp_TGtkToggleButton_draw_indicator) and bm_TGtkToggleButton_draw_indicator);
end;

function inconsistent(var a : TGtkToggleButton) : guint;
begin
   inconsistent:=(a.flag0 and bm_TGtkToggleButton_inconsistent) shr bp_TGtkToggleButton_inconsistent;
end;

procedure set_inconsistent(var a : TGtkToggleButton; __inconsistent : guint);
begin
   a.flag0:=a.flag0 or ((__inconsistent shl bp_TGtkToggleButton_inconsistent) and bm_TGtkToggleButton_inconsistent);
end;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_toggle_button_set_state(toggle_button:PGtkToggleButton; is_active:gboolean);
begin
   gtk_toggle_button_set_active(toggle_button,is_active);
end;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_implementation}
// included by gtk2.pas

