{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}


  type
     TGtkSpinButtonUpdatePolicy = (GTK_UPDATE_ALWAYS,GTK_UPDATE_IF_VALID
       );

     TGtkSpinType = (GTK_SPIN_STEP_FORWARD,GTK_SPIN_STEP_BACKWARD,
       GTK_SPIN_PAGE_FORWARD,GTK_SPIN_PAGE_BACKWARD,
       GTK_SPIN_HOME,GTK_SPIN_END,GTK_SPIN_USER_DEFINED
       );

     PGtkSpinButton = ^TGtkSpinButton;
     TGtkSpinButton = record
          entry : TGtkEntry;
          adjustment : PGtkAdjustment;
          panel : PGdkWindow;
          shadow_type : TGtkShadowType;
          timer : guint32;
          ev_time : guint32;
          climb_rate : gfloat;
          timer_step : gfloat;
          update_policy : TGtkSpinButtonUpdatePolicy;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
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
     bm_TGtkSpinButton_digits = $1C00;
     bp_TGtkSpinButton_digits = 10;
     bm_TGtkSpinButton_numeric = $2000;
     bp_TGtkSpinButton_numeric = 13;
     bm_TGtkSpinButton_wrap = $4000;
     bp_TGtkSpinButton_wrap = 14;
     bm_TGtkSpinButton_snap_to_ticks = $8000;
     bp_TGtkSpinButton_snap_to_ticks = 15;
function  in_child(var a : TGtkSpinButton) : guint;
procedure set_in_child(var a : TGtkSpinButton; __in_child : guint);
function  click_child(var a : TGtkSpinButton) : guint;
procedure set_click_child(var a : TGtkSpinButton; __click_child : guint);
function  button(var a : TGtkSpinButton) : guint;
procedure set_button(var a : TGtkSpinButton; __button : guint);
function  need_timer(var a : TGtkSpinButton) : guint;
procedure set_need_timer(var a : TGtkSpinButton; __need_timer : guint);
function  timer_calls(var a : TGtkSpinButton) : guint;
procedure set_timer_calls(var a : TGtkSpinButton; __timer_calls : guint);
function  digits(var a : TGtkSpinButton) : guint;
procedure set_digits(var a : TGtkSpinButton; __digits : guint);
function  numeric(var a : TGtkSpinButton) : guint;
procedure set_numeric(var a : TGtkSpinButton; __numeric : guint);
function  wrap(var a : TGtkSpinButton) : guint;
procedure set_wrap(var a : TGtkSpinButton; __wrap : guint);
function  snap_to_ticks(var a : TGtkSpinButton) : guint;
procedure set_snap_to_ticks(var a : TGtkSpinButton; __snap_to_ticks : guint);

  type
     PGtkSpinButtonClass = ^TGtkSpinButtonClass;
     TGtkSpinButtonClass = record
          parent_class : TGtkEntryClass;
       end;

Type
  GTK_SPIN_BUTTON = PGtkSpinButton;
  GTK_SPIN_BUTTON_CLASS = PGtkSpinButtonClass;

function  GTK_SPIN_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_spin_button_get_type';
function  GTK_IS_SPIN_BUTTON(obj:pointer):boolean;
function  GTK_IS_SPIN_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_spin_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_spin_button_get_type';
procedure gtk_spin_button_configure(spin_button:PGtkSpinButton; adjustment:PGtkAdjustment; climb_rate:gfloat; digits:guint);cdecl;external gtkdll name 'gtk_spin_button_configure';
function  gtk_spin_button_new(adjustment:PGtkAdjustment; climb_rate:gfloat; digits:guint):PGtkWidget;cdecl;external gtkdll name 'gtk_spin_button_new';
procedure gtk_spin_button_set_adjustment(spin_button:PGtkSpinButton; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_spin_button_set_adjustment';
function  gtk_spin_button_get_adjustment(spin_button:PGtkSpinButton):PGtkAdjustment;cdecl;external gtkdll name 'gtk_spin_button_get_adjustment';
procedure gtk_spin_button_set_digits(spin_button:PGtkSpinButton; digits:guint);cdecl;external gtkdll name 'gtk_spin_button_set_digits';
function  gtk_spin_button_get_value_as_float(spin_button:PGtkSpinButton):gfloat;cdecl;external gtkdll name 'gtk_spin_button_get_value_as_float';
function  gtk_spin_button_get_value_as_int(spin_button:PGtkSpinButton):gint;cdecl;external gtkdll name 'gtk_spin_button_get_value_as_int';
procedure gtk_spin_button_set_value(spin_button:PGtkSpinButton; value:gfloat);cdecl;external gtkdll name 'gtk_spin_button_set_value';
procedure gtk_spin_button_set_update_policy(spin_button:PGtkSpinButton; policy:TGtkSpinButtonUpdatePolicy);cdecl;external gtkdll name 'gtk_spin_button_set_update_policy';
procedure gtk_spin_button_set_numeric(spin_button:PGtkSpinButton; numeric:gboolean);cdecl;external gtkdll name 'gtk_spin_button_set_numeric';
procedure gtk_spin_button_spin(spin_button:PGtkSpinButton; direction:TGtkSpinType; increment:gfloat);cdecl;external gtkdll name 'gtk_spin_button_spin';
procedure gtk_spin_button_set_wrap(spin_button:PGtkSpinButton; wrap:gboolean);cdecl;external gtkdll name 'gtk_spin_button_set_wrap';
procedure gtk_spin_button_set_shadow_type(spin_button:PGtkSpinButton; shadow_type:TGtkShadowType);cdecl;external gtkdll name 'gtk_spin_button_set_shadow_type';
procedure gtk_spin_button_set_snap_to_ticks(spin_button:PGtkSpinButton; snap_to_ticks:gboolean);cdecl;external gtkdll name 'gtk_spin_button_set_snap_to_ticks';
procedure gtk_spin_button_update(spin_button:PGtkSpinButton);cdecl;external gtkdll name 'gtk_spin_button_update';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  in_child(var a : TGtkSpinButton) : guint;
    begin
       in_child:=(a.flag0 and bm_TGtkSpinButton_in_child) shr bp_TGtkSpinButton_in_child;
    end;

procedure set_in_child(var a : TGtkSpinButton; __in_child : guint);
    begin
       a.flag0:=a.flag0 or ((__in_child shl bp_TGtkSpinButton_in_child) and bm_TGtkSpinButton_in_child);
    end;

function  click_child(var a : TGtkSpinButton) : guint;
    begin
       click_child:=(a.flag0 and bm_TGtkSpinButton_click_child) shr bp_TGtkSpinButton_click_child;
    end;

procedure set_click_child(var a : TGtkSpinButton; __click_child : guint);
    begin
       a.flag0:=a.flag0 or ((__click_child shl bp_TGtkSpinButton_click_child) and bm_TGtkSpinButton_click_child);
    end;

function  button(var a : TGtkSpinButton) : guint;
    begin
       button:=(a.flag0 and bm_TGtkSpinButton_button) shr bp_TGtkSpinButton_button;
    end;

procedure set_button(var a : TGtkSpinButton; __button : guint);
    begin
       a.flag0:=a.flag0 or ((__button shl bp_TGtkSpinButton_button) and bm_TGtkSpinButton_button);
    end;

function  need_timer(var a : TGtkSpinButton) : guint;
    begin
       need_timer:=(a.flag0 and bm_TGtkSpinButton_need_timer) shr bp_TGtkSpinButton_need_timer;
    end;

procedure set_need_timer(var a : TGtkSpinButton; __need_timer : guint);
    begin
       a.flag0:=a.flag0 or ((__need_timer shl bp_TGtkSpinButton_need_timer) and bm_TGtkSpinButton_need_timer);
    end;

function  timer_calls(var a : TGtkSpinButton) : guint;
    begin
       timer_calls:=(a.flag0 and bm_TGtkSpinButton_timer_calls) shr bp_TGtkSpinButton_timer_calls;
    end;

procedure set_timer_calls(var a : TGtkSpinButton; __timer_calls : guint);
    begin
       a.flag0:=a.flag0 or ((__timer_calls shl bp_TGtkSpinButton_timer_calls) and bm_TGtkSpinButton_timer_calls);
    end;

function  digits(var a : TGtkSpinButton) : guint;
    begin
       digits:=(a.flag0 and bm_TGtkSpinButton_digits) shr bp_TGtkSpinButton_digits;
    end;

procedure set_digits(var a : TGtkSpinButton; __digits : guint);
    begin
       a.flag0:=a.flag0 or ((__digits shl bp_TGtkSpinButton_digits) and bm_TGtkSpinButton_digits);
    end;

function  numeric(var a : TGtkSpinButton) : guint;
    begin
       numeric:=(a.flag0 and bm_TGtkSpinButton_numeric) shr bp_TGtkSpinButton_numeric;
    end;

procedure set_numeric(var a : TGtkSpinButton; __numeric : guint);
    begin
       a.flag0:=a.flag0 or ((__numeric shl bp_TGtkSpinButton_numeric) and bm_TGtkSpinButton_numeric);
    end;

function  wrap(var a : TGtkSpinButton) : guint;
    begin
       wrap:=(a.flag0 and bm_TGtkSpinButton_wrap) shr bp_TGtkSpinButton_wrap;
    end;

procedure set_wrap(var a : TGtkSpinButton; __wrap : guint);
    begin
       a.flag0:=a.flag0 or ((__wrap shl bp_TGtkSpinButton_wrap) and bm_TGtkSpinButton_wrap);
    end;

function  snap_to_ticks(var a : TGtkSpinButton) : guint;
    begin
       snap_to_ticks:=(a.flag0 and bm_TGtkSpinButton_snap_to_ticks) shr bp_TGtkSpinButton_snap_to_ticks;
    end;

procedure set_snap_to_ticks(var a : TGtkSpinButton; __snap_to_ticks : guint);
    begin
       a.flag0:=a.flag0 or ((__snap_to_ticks shl bp_TGtkSpinButton_snap_to_ticks) and bm_TGtkSpinButton_snap_to_ticks);
    end;

function  GTK_IS_SPIN_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_SPIN_BUTTON:=(obj<>nil) and GTK_IS_SPIN_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_SPIN_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_SPIN_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_SPIN_BUTTON_TYPE);
end;

{$endif read_implementation}


