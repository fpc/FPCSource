// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  // internal type
  PGtkRangeLayout = pointer;
  // internal type
  PGtkRangeStepTimer = pointer;

{ These two are private/opaque types, ignore  }
{< protected > }
{ Steppers are: < > ---- < >
                a b      c d    }
{ Area of entire stepper + trough assembly in widget->window coords  }
{ Slider range along the long dimension, in widget->window coords  }
{ Round off value to this many digits, -1 for no rounding  }
{< private > }
{ trough click was on the forward side of slider  }
{ need to emit value_changed  }
   PGtkRange = ^TGtkRange;
   TGtkRange = record
        widget : TGtkWidget;
        adjustment : PGtkAdjustment;
        update_policy : TGtkUpdateType;
        flag0 : word;
        min_slider_size : gint;
        orientation : TGtkOrientation;
        range_rect : TGdkRectangle;
        slider_start : gint;
        slider_end : gint;
        round_digits : gint;
        flag1 : word;
        layout : PGtkRangeLayout;
        timer : PGtkRangeStepTimer;
        slide_initial_slider_position : gint;
        slide_initial_coordinate : gint;
        update_timeout_id : guint;
        event_window : PGdkWindow;
     end;

{ what detail to pass to GTK drawing functions  }
{ action signals for keybindings  }
{ Virtual functions  }
{ Padding for future expansion  }
   PGtkRangeClass = ^TGtkRangeClass;
   TGtkRangeClass = record
        parent_class : TGtkWidgetClass;
        slider_detail : Pgchar;
        stepper_detail : Pgchar;
        value_changed : procedure (range:PGtkRange); cdecl;
        adjust_bounds : procedure (range:PGtkRange; new_value:gdouble); cdecl;
        move_slider : procedure (range:PGtkRange; scroll:TGtkScrollType); cdecl;
        get_range_border : procedure (range:PGtkRange; border:PGtkBorder); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkRange_inverted = $1;
   bp_TGtkRange_inverted = 0;
   bm_TGtkRange_flippable = $2;
   bp_TGtkRange_flippable = 1;
   bm_TGtkRange_has_stepper_a = $4;
   bp_TGtkRange_has_stepper_a = 2;
   bm_TGtkRange_has_stepper_b = $8;
   bp_TGtkRange_has_stepper_b = 3;
   bm_TGtkRange_has_stepper_c = $10;
   bp_TGtkRange_has_stepper_c = 4;
   bm_TGtkRange_has_stepper_d = $20;
   bp_TGtkRange_has_stepper_d = 5;
   bm_TGtkRange_need_recalc = $40;
   bp_TGtkRange_need_recalc = 6;
   bm_TGtkRange_slider_size_fixed = $80;
   bp_TGtkRange_slider_size_fixed = 7;
   bm_TGtkRange_trough_click_forward = $1;
   bp_TGtkRange_trough_click_forward = 0;
   bm_TGtkRange_update_pending = $2;
   bp_TGtkRange_update_pending = 1;


function GTK_TYPE_RANGE : GType;
function GTK_RANGE(obj: pointer) : PGtkRange;
function GTK_RANGE_CLASS(klass: pointer) : PGtkRangeClass;
function GTK_IS_RANGE(obj: pointer) : boolean;
function GTK_IS_RANGE_CLASS(klass: pointer) : boolean;
function GTK_RANGE_GET_CLASS(obj: pointer) : PGtkRangeClass;

function inverted(var a : TGtkRange) : guint;
procedure set_inverted(var a : TGtkRange; __inverted : guint);
function flippable(var a : TGtkRange) : guint;
procedure set_flippable(var a : TGtkRange; __flippable : guint);
function has_stepper_a(var a : TGtkRange) : guint;
procedure set_has_stepper_a(var a : TGtkRange; __has_stepper_a : guint);
function has_stepper_b(var a : TGtkRange) : guint;
procedure set_has_stepper_b(var a : TGtkRange; __has_stepper_b : guint);
function has_stepper_c(var a : TGtkRange) : guint;
procedure set_has_stepper_c(var a : TGtkRange; __has_stepper_c : guint);
function has_stepper_d(var a : TGtkRange) : guint;
procedure set_has_stepper_d(var a : TGtkRange; __has_stepper_d : guint);
function need_recalc(var a : TGtkRange) : guint;
procedure set_need_recalc(var a : TGtkRange; __need_recalc : guint);
function slider_size_fixed(var a : TGtkRange) : guint;
procedure set_slider_size_fixed(var a : TGtkRange; __slider_size_fixed : guint);
function trough_click_forward(var a : TGtkRange) : guint;
procedure set_trough_click_forward(var a : TGtkRange; __trough_click_forward : guint);
function update_pending(var a : TGtkRange) : guint;
procedure set_update_pending(var a : TGtkRange; __update_pending : guint);

function gtk_range_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_range_set_update_policy(range:PGtkRange; policy:TGtkUpdateType); cdecl; external gtklib;
function gtk_range_get_update_policy(range:PGtkRange):TGtkUpdateType; cdecl; external gtklib;
procedure gtk_range_set_adjustment(range:PGtkRange; adjustment:PGtkAdjustment); cdecl; external gtklib;
function gtk_range_get_adjustment(range:PGtkRange):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_range_set_inverted(range:PGtkRange; setting:gboolean); cdecl; external gtklib;
function gtk_range_get_inverted(range:PGtkRange):gboolean; cdecl; external gtklib;
procedure gtk_range_set_increments(range:PGtkRange; step:gdouble; page:gdouble); cdecl; external gtklib;
procedure gtk_range_set_range(range:PGtkRange; min:gdouble; max:gdouble); cdecl; external gtklib;
procedure gtk_range_set_value(range:PGtkRange; value:gdouble); cdecl; external gtklib;
function gtk_range_get_value(range:PGtkRange):gdouble; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_RANGE : GType;
begin
   GTK_TYPE_RANGE:=gtk_range_get_type;
end;

function GTK_RANGE(obj: pointer) : PGtkRange;
begin
   GTK_RANGE:=PGtkRange(GTK_CHECK_CAST(obj,GTK_TYPE_RANGE));
end;

function GTK_RANGE_CLASS(klass: pointer) : PGtkRangeClass;
begin
   GTK_RANGE_CLASS:=PGtkRangeClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_RANGE));
end;

function GTK_IS_RANGE(obj: pointer) : boolean;
begin
   GTK_IS_RANGE:=GTK_CHECK_TYPE(obj,GTK_TYPE_RANGE);
end;

function GTK_IS_RANGE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_RANGE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_RANGE);
end;

function GTK_RANGE_GET_CLASS(obj: pointer) : PGtkRangeClass;
begin
   GTK_RANGE_GET_CLASS:=PGtkRangeClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_RANGE));
end;

function inverted(var a : TGtkRange) : guint;
begin
   inverted:=(a.flag0 and bm_TGtkRange_inverted) shr bp_TGtkRange_inverted;
end;

procedure set_inverted(var a : TGtkRange; __inverted : guint);
begin
   a.flag0:=a.flag0 or ((__inverted shl bp_TGtkRange_inverted) and bm_TGtkRange_inverted);
end;

function flippable(var a : TGtkRange) : guint;
begin
   flippable:=(a.flag0 and bm_TGtkRange_flippable) shr bp_TGtkRange_flippable;
end;

procedure set_flippable(var a : TGtkRange; __flippable : guint);
begin
   a.flag0:=a.flag0 or ((__flippable shl bp_TGtkRange_flippable) and bm_TGtkRange_flippable);
end;

function has_stepper_a(var a : TGtkRange) : guint;
begin
   has_stepper_a:=(a.flag0 and bm_TGtkRange_has_stepper_a) shr bp_TGtkRange_has_stepper_a;
end;

procedure set_has_stepper_a(var a : TGtkRange; __has_stepper_a : guint);
begin
   a.flag0:=a.flag0 or ((__has_stepper_a shl bp_TGtkRange_has_stepper_a) and bm_TGtkRange_has_stepper_a);
end;

function has_stepper_b(var a : TGtkRange) : guint;
begin
   has_stepper_b:=(a.flag0 and bm_TGtkRange_has_stepper_b) shr bp_TGtkRange_has_stepper_b;
end;

procedure set_has_stepper_b(var a : TGtkRange; __has_stepper_b : guint);
begin
   a.flag0:=a.flag0 or ((__has_stepper_b shl bp_TGtkRange_has_stepper_b) and bm_TGtkRange_has_stepper_b);
end;

function has_stepper_c(var a : TGtkRange) : guint;
begin
   has_stepper_c:=(a.flag0 and bm_TGtkRange_has_stepper_c) shr bp_TGtkRange_has_stepper_c;
end;

procedure set_has_stepper_c(var a : TGtkRange; __has_stepper_c : guint);
begin
   a.flag0:=a.flag0 or ((__has_stepper_c shl bp_TGtkRange_has_stepper_c) and bm_TGtkRange_has_stepper_c);
end;

function has_stepper_d(var a : TGtkRange) : guint;
begin
   has_stepper_d:=(a.flag0 and bm_TGtkRange_has_stepper_d) shr bp_TGtkRange_has_stepper_d;
end;

procedure set_has_stepper_d(var a : TGtkRange; __has_stepper_d : guint);
begin
   a.flag0:=a.flag0 or ((__has_stepper_d shl bp_TGtkRange_has_stepper_d) and bm_TGtkRange_has_stepper_d);
end;

function need_recalc(var a : TGtkRange) : guint;
begin
   need_recalc:=(a.flag0 and bm_TGtkRange_need_recalc) shr bp_TGtkRange_need_recalc;
end;

procedure set_need_recalc(var a : TGtkRange; __need_recalc : guint);
begin
   a.flag0:=a.flag0 or ((__need_recalc shl bp_TGtkRange_need_recalc) and bm_TGtkRange_need_recalc);
end;

function slider_size_fixed(var a : TGtkRange) : guint;
begin
   slider_size_fixed:=(a.flag0 and bm_TGtkRange_slider_size_fixed) shr bp_TGtkRange_slider_size_fixed;
end;

procedure set_slider_size_fixed(var a : TGtkRange; __slider_size_fixed : guint);
begin
   a.flag0:=a.flag0 or ((__slider_size_fixed shl bp_TGtkRange_slider_size_fixed) and bm_TGtkRange_slider_size_fixed);
end;

function trough_click_forward(var a : TGtkRange) : guint;
begin
   trough_click_forward:=(a.flag1 and bm_TGtkRange_trough_click_forward) shr bp_TGtkRange_trough_click_forward;
end;

procedure set_trough_click_forward(var a : TGtkRange; __trough_click_forward : guint);
begin
   a.flag1:=a.flag1 or ((__trough_click_forward shl bp_TGtkRange_trough_click_forward) and bm_TGtkRange_trough_click_forward);
end;

function update_pending(var a : TGtkRange) : guint;
begin
   update_pending:=(a.flag1 and bm_TGtkRange_update_pending) shr bp_TGtkRange_update_pending;
end;

procedure set_update_pending(var a : TGtkRange; __update_pending : guint);
begin
   a.flag1:=a.flag1 or ((__update_pending shl bp_TGtkRange_update_pending) and bm_TGtkRange_update_pending);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

