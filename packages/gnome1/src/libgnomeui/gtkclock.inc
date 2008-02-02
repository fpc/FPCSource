{$IfDef read_interface}

type
   TGtkClockType = (GTK_CLOCK_INCREASING,GTK_CLOCK_DECREASING,
     GTK_CLOCK_REALTIME);

   PGtkClock = ^TGtkClock;
   TGtkClock = record
        widget : TGtkLabel;
        thetype : TGtkClockType;
        timer_id : gint;
        update_interval : gint;
        seconds : time_t;
        stopped : time_t;
        fmt : Pgchar;
        thetm : Ptm;
     end;
   GTK_CLOCK = PGTKClock;

   PGtkClockClass = ^TGtkClockClass;
   TGtkClockClass = record
        parent_class : TGtkLabelClass;
     end;
   GTK_CLOCK_CLASS = PGTKClockClass;

function GTK_TYPE_CLOCK : TGTKType;
function GTK_IS_CLOCK(obj : Pointer) : gboolean;
function GTK_IS_CLOCK_CLASS(klass : Pointer) : gboolean;

function gtk_clock_get_type:TGTKType;cdecl;external libgnomeuidll name 'gtk_clock_get_type';
function gtk_clock_new(thetype:TGtkClockType):PGtkWidget;cdecl;external libgnomeuidll name 'gtk_clock_new';
procedure gtk_clock_set_format(gclock:PGtkClock; fmt:Pgchar);cdecl;external libgnomeuidll name 'gtk_clock_set_format';
procedure gtk_clock_set_seconds(gclock:PGtkClock; seconds:time_t);cdecl;external libgnomeuidll name 'gtk_clock_set_seconds';
procedure gtk_clock_set_update_interval(gclock:PGtkClock; seconds:gint);cdecl;external libgnomeuidll name 'gtk_clock_set_update_interval';
procedure gtk_clock_start(gclock:PGtkClock);cdecl;external libgnomeuidll name 'gtk_clock_start';
procedure gtk_clock_stop(gclock:PGtkClock);cdecl;external libgnomeuidll name 'gtk_clock_stop';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GTK_TYPE_CLOCK : TGTKType;
begin
  GTK_TYPE_CLOCK:=gtk_clock_get_type;
end;

function GTK_IS_CLOCK(obj : Pointer) : gboolean;
begin
   GTK_IS_CLOCK:=(obj<>nil) and GTK_IS_CLOCK_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GTK_IS_CLOCK_CLASS(klass : Pointer) : gboolean;
begin
   GTK_IS_CLOCK_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TYPE_CLOCK);
end;

{$Endif read_implementation}
