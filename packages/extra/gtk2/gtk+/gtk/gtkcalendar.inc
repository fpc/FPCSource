// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCalendarDisplayOptions = ^TGtkCalendarDisplayOptions;
   TGtkCalendarDisplayOptions = longint;

{ Padding for future expansion  }
   PGtkCalendar = ^TGtkCalendar;
   TGtkCalendar = record
        widget : TGtkWidget;
        header_style : PGtkStyle;
        label_style : PGtkStyle;
        month : gint;
        year : gint;
        selected_day : gint;
        day_month : array[0..5] of array[0..6] of gint;
        day : array[0..5] of array[0..6] of gint;
        num_marked_dates : gint;
        marked_date : array[0..30] of gint;
        display_flags : TGtkCalendarDisplayOptions;
        marked_date_color : array[0..30] of TGdkColor;
        gc : PGdkGC;
        xor_gc : PGdkGC;
        focus_row : gint;
        focus_col : gint;
        highlight_row : gint;
        highlight_col : gint;
        private_data : gpointer;
        grow_space : array[0..31] of gchar;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{ Signal handlers  }
   PGtkCalendarClass = ^TGtkCalendarClass;
   TGtkCalendarClass = record
        parent_class : TGtkWidgetClass;
        month_changed : procedure (calendar:PGtkCalendar); cdecl;
        day_selected : procedure (calendar:PGtkCalendar); cdecl;
        day_selected_double_click : procedure (calendar:PGtkCalendar); cdecl;
        prev_month : procedure (calendar:PGtkCalendar); cdecl;
        next_month : procedure (calendar:PGtkCalendar); cdecl;
        prev_year : procedure (calendar:PGtkCalendar); cdecl;
        next_year : procedure (calendar:PGtkCalendar); cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_CALENDAR_SHOW_HEADING = 1 shl 0;
   GTK_CALENDAR_SHOW_DAY_NAMES = 1 shl 1;
   GTK_CALENDAR_NO_MONTH_CHANGE = 1 shl 2;
   GTK_CALENDAR_SHOW_WEEK_NUMBERS = 1 shl 3;
   GTK_CALENDAR_WEEK_START_MONDAY = 1 shl 4;


function GTK_TYPE_CALENDAR : GType;
function GTK_CALENDAR(obj: pointer) : PGtkCalendar;
function GTK_CALENDAR_CLASS(klass: pointer) : PGtkCalendarClass;
function GTK_IS_CALENDAR(obj: pointer) : boolean;
function GTK_IS_CALENDAR_CLASS(klass: pointer) : boolean;
function GTK_CALENDAR_GET_CLASS(obj: pointer) : PGtkCalendarClass;


function gtk_calendar_get_type:TGtkType; cdecl; external gtklib;
function gtk_calendar_new:PGtkWidget; cdecl; external gtklib;
function gtk_calendar_select_month(calendar:PGtkCalendar; month:guint; year:guint):gboolean; cdecl; external gtklib;
procedure gtk_calendar_select_day(calendar:PGtkCalendar; day:guint); cdecl; external gtklib;
function gtk_calendar_mark_day(calendar:PGtkCalendar; day:guint):gboolean; cdecl; external gtklib;
function gtk_calendar_unmark_day(calendar:PGtkCalendar; day:guint):gboolean; cdecl; external gtklib;
procedure gtk_calendar_clear_marks(calendar:PGtkCalendar); cdecl; external gtklib;
procedure gtk_calendar_display_options(calendar:PGtkCalendar; flags:TGtkCalendarDisplayOptions); cdecl; external gtklib;
procedure gtk_calendar_get_date(calendar:PGtkCalendar; year:Pguint; month:Pguint; day:Pguint); cdecl; external gtklib;
procedure gtk_calendar_freeze(calendar:PGtkCalendar); cdecl; external gtklib;
procedure gtk_calendar_thaw(calendar:PGtkCalendar); cdecl; external gtklib;

{$IFDEF HasGTK2_4}
function  gtk_calendar_get_display_options(calendar:PGtkCalendar): TGtkCalendarDisplayOptions;  cdecl; external gtklib;
procedure gtk_calendar_set_display_options(calendar:PGtkCalendar; flags:TGtkCalendarDisplayOptions); cdecl; external gtklib;
{$ENDIF HasGTK2_4}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CALENDAR : GType;
begin
   GTK_TYPE_CALENDAR:=gtk_calendar_get_type;
end;

function GTK_CALENDAR(obj: pointer) : PGtkCalendar;
begin
   GTK_CALENDAR:=PGtkCalendar(GTK_CHECK_CAST(obj,GTK_TYPE_CALENDAR));
end;

function GTK_CALENDAR_CLASS(klass: pointer) : PGtkCalendarClass;
begin
   GTK_CALENDAR_CLASS:=PGtkCalendarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CALENDAR));
end;

function GTK_IS_CALENDAR(obj: pointer) : boolean;
begin
   GTK_IS_CALENDAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_CALENDAR);
end;

function GTK_IS_CALENDAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CALENDAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CALENDAR);
end;

function GTK_CALENDAR_GET_CLASS(obj: pointer) : PGtkCalendarClass;
begin
   GTK_CALENDAR_GET_CLASS:=PGtkCalendarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CALENDAR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

