// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkSpinButtonUpdatePolicy = ^TGtkSpinButtonUpdatePolicy;
   TGtkSpinButtonUpdatePolicy = (
     GTK_UPDATE_ALWAYS,
     GTK_UPDATE_IF_VALID
   );

   PGtkSpinType = ^TGtkSpinType;
   TGtkSpinType = (
     GTK_SPIN_STEP_FORWARD,
     GTK_SPIN_STEP_BACKWARD,
     GTK_SPIN_PAGE_FORWARD,
     GTK_SPIN_PAGE_BACKWARD,
     GTK_SPIN_HOME,
     GTK_SPIN_END,
     GTK_SPIN_USER_DEFINED
   );

{ valid: GTK_ARROW_UP=0, GTK_ARROW_DOWN=1 or 2=NONE/BOTH  }
   PGtkSpinButton = ^TGtkSpinButton;
   TGtkSpinButton = record
        entry : TGtkEntry;
        adjustment : PGtkAdjustment;
        panel : PGdkWindow;
        timer : guint32;
        climb_rate : gdouble;
        timer_step : gdouble;
        update_policy : TGtkSpinButtonUpdatePolicy;
        flag0 : longint;
     end;

{ Action signals for keybindings, do not connect to these  }
{ Padding for future expansion  }
   PGtkSpinButtonClass = ^TGtkSpinButtonClass;
   TGtkSpinButtonClass = record
        parent_class : TGtkEntryClass;
        input : function (spin_button:PGtkSpinButton; new_value:Pgdouble):gint; cdecl;
        output : function (spin_button:PGtkSpinButton):gint; cdecl;
        value_changed : procedure (spin_button:PGtkSpinButton); cdecl;
        change_value : procedure (spin_button:PGtkSpinButton; scroll:TGtkScrollType); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_INPUT_ERROR = -(1);

   bm_TGtkSpinButton_in_child = $3;
   bp_TGtkSpinButton_in_child = 0;
   bm_TGtkSpinButton_click_child = $C;
   bp_TGtkSpinButton_click_child = 2;
   bm_TGtkSpinButton_button = $30;
   bp_TGtkSpinButton_button = 4;
   bm_TGtkSpinButton_need_timer = $40;
   bp_TGtkSpinButton_need_timer = 6;
   bm_TGtkSpinButton_timer_calls = $380;
   bp_TGtkSpinButton_timer_calls = 7;
   bm_TGtkSpinButton_digits = $FFC00;
   bp_TGtkSpinButton_digits = 10;
   bm_TGtkSpinButton_numeric = $100000;
   bp_TGtkSpinButton_numeric = 20;
   bm_TGtkSpinButton_wrap = $200000;
   bp_TGtkSpinButton_wrap = 21;
   bm_TGtkSpinButton_snap_to_ticks = $400000;
   bp_TGtkSpinButton_snap_to_ticks = 22;


function GTK_TYPE_SPIN_BUTTON : GType;
function GTK_SPIN_BUTTON(obj: pointer) : PGtkSpinButton;
function GTK_SPIN_BUTTON_CLASS(klass: pointer) : PGtkSpinButtonClass;
function GTK_IS_SPIN_BUTTON(obj: pointer) : boolean;
function GTK_IS_SPIN_BUTTON_CLASS(klass: pointer) : boolean;
function GTK_SPIN_BUTTON_GET_CLASS(obj: pointer) : PGtkSpinButtonClass;


function in_child(var a : TGtkSpinButton) : guint; overload;
procedure set_in_child(var a : TGtkSpinButton; __in_child : guint); overload;
function click_child(var a : TGtkSpinButton) : guint; overload;
procedure set_click_child(var a : TGtkSpinButton; __click_child : guint); overload;
function button(var a : TGtkSpinButton) : guint; overload;
procedure set_button(var a : TGtkSpinButton; __button : guint); overload;
function need_timer(var a : TGtkSpinButton) : guint; overload;
procedure set_need_timer(var a : TGtkSpinButton; __need_timer : guint); overload;
function timer_calls(var a : TGtkSpinButton) : guint;
procedure set_timer_calls(var a : TGtkSpinButton; __timer_calls : guint);
function digits(var a : TGtkSpinButton) : guint;
procedure set_digits(var a : TGtkSpinButton; __digits : guint);
function numeric(var a : TGtkSpinButton) : guint;
procedure set_numeric(var a : TGtkSpinButton; __numeric : guint);
function wrap(var a : TGtkSpinButton) : guint; overload;
procedure set_wrap(var a : TGtkSpinButton; __wrap : guint); overload;
function snap_to_ticks(var a : TGtkSpinButton) : guint;
procedure set_snap_to_ticks(var a : TGtkSpinButton; __snap_to_ticks : guint);

function gtk_spin_button_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_spin_button_configure(spin_button:PGtkSpinButton; adjustment:PGtkAdjustment; climb_rate:gdouble; digits:guint); cdecl; external gtklib;
function gtk_spin_button_new(adjustment:PGtkAdjustment; climb_rate:gdouble; digits:guint):PGtkWidget; cdecl; external gtklib;
function gtk_spin_button_new_with_range(min:gdouble; max:gdouble; step:gdouble):PGtkWidget; cdecl; external gtklib;
procedure gtk_spin_button_set_adjustment(spin_button:PGtkSpinButton; adjustment:PGtkAdjustment); cdecl; external gtklib;
function gtk_spin_button_get_adjustment(spin_button:PGtkSpinButton):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_spin_button_set_digits(spin_button:PGtkSpinButton; digits:guint); cdecl; external gtklib;
function gtk_spin_button_get_digits(spin_button:PGtkSpinButton):guint; cdecl; external gtklib;
procedure gtk_spin_button_set_increments(spin_button:PGtkSpinButton; step:gdouble; page:gdouble); cdecl; external gtklib;
procedure gtk_spin_button_get_increments(spin_button:PGtkSpinButton; step:Pgdouble; page:Pgdouble); cdecl; external gtklib;
procedure gtk_spin_button_set_range(spin_button:PGtkSpinButton; min:gdouble; max:gdouble); cdecl; external gtklib;
procedure gtk_spin_button_get_range(spin_button:PGtkSpinButton; min:Pgdouble; max:Pgdouble); cdecl; external gtklib;
function gtk_spin_button_get_value(spin_button:PGtkSpinButton):gdouble; cdecl; external gtklib;
function gtk_spin_button_get_value_as_int(spin_button:PGtkSpinButton):gint; cdecl; external gtklib;
procedure gtk_spin_button_set_value(spin_button:PGtkSpinButton; value:gdouble); cdecl; external gtklib;
procedure gtk_spin_button_set_update_policy(spin_button:PGtkSpinButton; policy:TGtkSpinButtonUpdatePolicy); cdecl; external gtklib;
function gtk_spin_button_get_update_policy(spin_button:PGtkSpinButton):TGtkSpinButtonUpdatePolicy; cdecl; external gtklib;
procedure gtk_spin_button_set_numeric(spin_button:PGtkSpinButton; numeric:gboolean); cdecl; external gtklib;
function gtk_spin_button_get_numeric(spin_button:PGtkSpinButton):gboolean; cdecl; external gtklib;
procedure gtk_spin_button_spin(spin_button:PGtkSpinButton; direction:TGtkSpinType; increment:gdouble); cdecl; external gtklib;
procedure gtk_spin_button_set_wrap(spin_button:PGtkSpinButton; wrap:gboolean); cdecl; external gtklib;
function gtk_spin_button_get_wrap(spin_button:PGtkSpinButton):gboolean; cdecl; external gtklib;
procedure gtk_spin_button_set_snap_to_ticks(spin_button:PGtkSpinButton; snap_to_ticks:gboolean); cdecl; external gtklib;
function gtk_spin_button_get_snap_to_ticks(spin_button:PGtkSpinButton):gboolean; cdecl; external gtklib;
procedure gtk_spin_button_update(spin_button:PGtkSpinButton); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_spin_button_get_value_as_float(spin_button:PGtkSpinButton):gdouble;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SPIN_BUTTON : GType;
begin
   GTK_TYPE_SPIN_BUTTON:=gtk_spin_button_get_type;
end;

function GTK_SPIN_BUTTON(obj: pointer) : PGtkSpinButton;
begin
   GTK_SPIN_BUTTON:=PGtkSpinButton(GTK_CHECK_CAST(obj,GTK_TYPE_SPIN_BUTTON));
end;

function GTK_SPIN_BUTTON_CLASS(klass: pointer) : PGtkSpinButtonClass;
begin
   GTK_SPIN_BUTTON_CLASS:=PGtkSpinButtonClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SPIN_BUTTON));
end;

function GTK_IS_SPIN_BUTTON(obj: pointer) : boolean;
begin
   GTK_IS_SPIN_BUTTON:=GTK_CHECK_TYPE(obj,GTK_TYPE_SPIN_BUTTON);
end;

function GTK_IS_SPIN_BUTTON_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SPIN_BUTTON_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SPIN_BUTTON);
end;

function GTK_SPIN_BUTTON_GET_CLASS(obj: pointer) : PGtkSpinButtonClass;
begin
   GTK_SPIN_BUTTON_GET_CLASS:=PGtkSpinButtonClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SPIN_BUTTON));
end;

function in_child(var a : TGtkSpinButton) : guint;
begin
   in_child:=(a.flag0 and bm_TGtkSpinButton_in_child) shr bp_TGtkSpinButton_in_child;
end;

procedure set_in_child(var a : TGtkSpinButton; __in_child : guint);
begin
   a.flag0:=a.flag0 or ((__in_child shl bp_TGtkSpinButton_in_child) and bm_TGtkSpinButton_in_child);
end;

function click_child(var a : TGtkSpinButton) : guint;
begin
   click_child:=(a.flag0 and bm_TGtkSpinButton_click_child) shr bp_TGtkSpinButton_click_child;
end;

procedure set_click_child(var a : TGtkSpinButton; __click_child : guint);
begin
   a.flag0:=a.flag0 or ((__click_child shl bp_TGtkSpinButton_click_child) and bm_TGtkSpinButton_click_child);
end;

function button(var a : TGtkSpinButton) : guint;
begin
   button:=(a.flag0 and bm_TGtkSpinButton_button) shr bp_TGtkSpinButton_button;
end;

procedure set_button(var a : TGtkSpinButton; __button : guint);
begin
   a.flag0:=a.flag0 or ((__button shl bp_TGtkSpinButton_button) and bm_TGtkSpinButton_button);
end;

function need_timer(var a : TGtkSpinButton) : guint;
begin
   need_timer:=(a.flag0 and bm_TGtkSpinButton_need_timer) shr bp_TGtkSpinButton_need_timer;
end;

procedure set_need_timer(var a : TGtkSpinButton; __need_timer : guint);
begin
   a.flag0:=a.flag0 or ((__need_timer shl bp_TGtkSpinButton_need_timer) and bm_TGtkSpinButton_need_timer);
end;

function timer_calls(var a : TGtkSpinButton) : guint;
begin
   timer_calls:=(a.flag0 and bm_TGtkSpinButton_timer_calls) shr bp_TGtkSpinButton_timer_calls;
end;

procedure set_timer_calls(var a : TGtkSpinButton; __timer_calls : guint);
begin
   a.flag0:=a.flag0 or ((__timer_calls shl bp_TGtkSpinButton_timer_calls) and bm_TGtkSpinButton_timer_calls);
end;

function digits(var a : TGtkSpinButton) : guint;
begin
   digits:=(a.flag0 and bm_TGtkSpinButton_digits) shr bp_TGtkSpinButton_digits;
end;

procedure set_digits(var a : TGtkSpinButton; __digits : guint);
begin
   a.flag0:=a.flag0 or ((__digits shl bp_TGtkSpinButton_digits) and bm_TGtkSpinButton_digits);
end;

function numeric(var a : TGtkSpinButton) : guint;
begin
   numeric:=(a.flag0 and bm_TGtkSpinButton_numeric) shr bp_TGtkSpinButton_numeric;
end;

procedure set_numeric(var a : TGtkSpinButton; __numeric : guint);
begin
   a.flag0:=a.flag0 or ((__numeric shl bp_TGtkSpinButton_numeric) and bm_TGtkSpinButton_numeric);
end;

function wrap(var a : TGtkSpinButton) : guint;
begin
   wrap:=(a.flag0 and bm_TGtkSpinButton_wrap) shr bp_TGtkSpinButton_wrap;
end;

procedure set_wrap(var a : TGtkSpinButton; __wrap : guint);
begin
   a.flag0:=a.flag0 or ((__wrap shl bp_TGtkSpinButton_wrap) and bm_TGtkSpinButton_wrap);
end;

function snap_to_ticks(var a : TGtkSpinButton) : guint;
begin
   snap_to_ticks:=(a.flag0 and bm_TGtkSpinButton_snap_to_ticks) shr bp_TGtkSpinButton_snap_to_ticks;
end;

procedure set_snap_to_ticks(var a : TGtkSpinButton; __snap_to_ticks : guint);
begin
   a.flag0:=a.flag0 or ((__snap_to_ticks shl bp_TGtkSpinButton_snap_to_ticks) and bm_TGtkSpinButton_snap_to_ticks);
end;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_spin_button_get_value_as_float(spin_button:PGtkSpinButton):gdouble;
begin
   gtk_spin_button_get_value_as_float:=gtk_spin_button_get_value(spin_button);
end;
{$endif}

{$ENDIF read_implementation}
// included by gtk2.pas

