// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkProgressBarStyle = ^TGtkProgressBarStyle;
   TGtkProgressBarStyle = (
     GTK_PROGRESS_CONTINUOUS,
     GTK_PROGRESS_DISCRETE
   );

   PGtkProgressBarOrientation = ^TGtkProgressBarOrientation;
   TGtkProgressBarOrientation = (
     GTK_PROGRESS_LEFT_TO_RIGHT,
     GTK_PROGRESS_RIGHT_TO_LEFT,
     GTK_PROGRESS_BOTTOM_TO_TOP,
     GTK_PROGRESS_TOP_TO_BOTTOM
   );

   PGtkProgressBar = ^TGtkProgressBar;
   TGtkProgressBar = record
        progress : TGtkProgress;
        bar_style : TGtkProgressBarStyle;
        orientation : TGtkProgressBarOrientation;
        blocks : guint;
        in_block : gint;
        activity_pos : gint;
        activity_step : guint;
        activity_blocks : guint;
        pulse_fraction : gdouble;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkProgressBarClass = ^TGtkProgressBarClass;
   TGtkProgressBarClass = record
        parent_class : TGtkProgressClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkProgressBar_activity_dir = $1;
   bp_TGtkProgressBar_activity_dir = 0;


function GTK_TYPE_PROGRESS_BAR : GType;
function GTK_PROGRESS_BAR(obj: pointer) : PGtkProgressBar;
function GTK_PROGRESS_BAR_CLASS(klass: pointer) : PGtkProgressBarClass;
function GTK_IS_PROGRESS_BAR(obj: pointer) : boolean;
function GTK_IS_PROGRESS_BAR_CLASS(klass: pointer) : boolean;
function GTK_PROGRESS_BAR_GET_CLASS(obj: pointer) : PGtkProgressBarClass;

function activity_dir(var a : TGtkProgressBar) : guint;
procedure set_activity_dir(var a : TGtkProgressBar; __activity_dir : guint);

function gtk_progress_bar_get_type:TGtkType; cdecl; external gtklib;
function gtk_progress_bar_new:PGtkWidget; cdecl; external gtklib;
{
   GtkProgress/GtkProgressBar had serious problems in GTK 1.2.

    - Only 3 or 4 functions are really needed for 95% of progress
      interfaces; GtkProgress[Bar] had about 25 functions, and
      didn't even include these 3 or 4.
    - In activity mode, the API involves setting the adjustment
      to any random value, just to have the side effect of
      calling the progress bar update function - the adjustment
      is totally ignored in activity mode
    - You set the activity step as a pixel value, which means to
      set the activity step you basically need to connect to
      size_allocate
    - There are ctree_set_expander_style()-functions, to randomly
      change look-and-feel for no good reason
    - The split between GtkProgress and GtkProgressBar makes no sense
      to me whatsoever.

   This was a big wart on GTK and made people waste lots of time,
   both learning and using the interface.

   So, I have added what I feel is the correct API, and marked all the
   rest deprecated. However, the changes are 100% backward-compatible and
   should break no existing code.

   The following 9 functions are the new programming interface.
  }
procedure gtk_progress_bar_pulse(pbar:PGtkProgressBar); cdecl; external gtklib;
procedure gtk_progress_bar_set_text(pbar:PGtkProgressBar; text:Pgchar); cdecl; external gtklib;
procedure gtk_progress_bar_set_fraction(pbar:PGtkProgressBar; fraction:gdouble); cdecl; external gtklib;
procedure gtk_progress_bar_set_pulse_step(pbar:PGtkProgressBar; fraction:gdouble); cdecl; external gtklib;
procedure gtk_progress_bar_set_orientation(pbar:PGtkProgressBar; orientation:TGtkProgressBarOrientation); cdecl; external gtklib;
function gtk_progress_bar_get_text(pbar:PGtkProgressBar):Pgchar; cdecl; external gtklib;
function gtk_progress_bar_get_fraction(pbar:PGtkProgressBar):gdouble; cdecl; external gtklib;
function gtk_progress_bar_get_pulse_step(pbar:PGtkProgressBar):gdouble; cdecl; external gtklib;
function gtk_progress_bar_get_orientation(pbar:PGtkProgressBar):TGtkProgressBarOrientation; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
{ Everything below here is deprecated  }
function gtk_progress_bar_new_with_adjustment(adjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
procedure gtk_progress_bar_set_bar_style(pbar:PGtkProgressBar; style:TGtkProgressBarStyle); cdecl; external gtklib;
procedure gtk_progress_bar_set_discrete_blocks(pbar:PGtkProgressBar; blocks:guint); cdecl; external gtklib;
{ set_activity_step() is not only deprecated, it doesn't even work.
   (Of course, it wasn't usable anyway, you had to set it from a size_allocate
   handler or something)
  }
procedure gtk_progress_bar_set_activity_step(pbar:PGtkProgressBar; step:guint); cdecl; external gtklib;
procedure gtk_progress_bar_set_activity_blocks(pbar:PGtkProgressBar; blocks:guint); cdecl; external gtklib;
procedure gtk_progress_bar_update(pbar:PGtkProgressBar; percentage:gdouble); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_PROGRESS_BAR : GType;
begin
   GTK_TYPE_PROGRESS_BAR:=gtk_progress_bar_get_type;
end;

function GTK_PROGRESS_BAR(obj: pointer) : PGtkProgressBar;
begin
   GTK_PROGRESS_BAR:=PGtkProgressBar(GTK_CHECK_CAST(obj,GTK_TYPE_PROGRESS_BAR));
end;

function GTK_PROGRESS_BAR_CLASS(klass: pointer) : PGtkProgressBarClass;
begin
   GTK_PROGRESS_BAR_CLASS:=PGtkProgressBarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_PROGRESS_BAR));
end;

function GTK_IS_PROGRESS_BAR(obj: pointer) : boolean;
begin
   GTK_IS_PROGRESS_BAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_PROGRESS_BAR);
end;

function GTK_IS_PROGRESS_BAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_PROGRESS_BAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_PROGRESS_BAR);
end;

function GTK_PROGRESS_BAR_GET_CLASS(obj: pointer) : PGtkProgressBarClass;
begin
   GTK_PROGRESS_BAR_GET_CLASS:=PGtkProgressBarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_PROGRESS_BAR));
end;

function activity_dir(var a : TGtkProgressBar) : guint;
begin
   activity_dir:=(a.flag0 and bm_TGtkProgressBar_activity_dir) shr bp_TGtkProgressBar_activity_dir;
end;

procedure set_activity_dir(var a : TGtkProgressBar; __activity_dir : guint);
begin
   a.flag0:=a.flag0 or ((__activity_dir shl bp_TGtkProgressBar_activity_dir) and bm_TGtkProgressBar_activity_dir);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

