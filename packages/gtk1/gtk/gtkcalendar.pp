{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkCalendarDisplayOptions = (
       GTK_CALENDAR_SHOW_HEADING := 1 shl 0,
       GTK_CALENDAR_SHOW_DAY_NAMES := 1 shl 1,
       GTK_CALENDAR_NO_MONTH_CHANGE := 1 shl 2,
       GTK_CALENDAR_SHOW_WEEK_NUMBERS := 1 shl 3,
       GTK_CALENDAR_WEEK_START_MONDAY := 1 shl 4
       );

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
       end;

     PGtkCalendarClass = ^TGtkCalendarClass;
     TGtkCalendarClass = record
          parent_class : TGtkWidgetClass;
          month_changed : procedure (calendar:PGtkCalendar);cdecl;
          day_selected : procedure (calendar:PGtkCalendar);cdecl;
          day_selected_double_click : procedure (calendar:PGtkCalendar);cdecl;
          prev_month : procedure (calendar:PGtkCalendar);cdecl;
          next_month : procedure (calendar:PGtkCalendar);cdecl;
          prev_year : procedure (calendar:PGtkCalendar);cdecl;
          next_year : procedure (calendar:PGtkCalendar);cdecl;
       end;

type
  GTK_CALENDAR=PGtkCalendar;
  GTK_CALENDAR_CLASS=PGtkCalendarClass;

function  GTK_CALENDAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_calendar_get_type';
function  GTK_IS_CALENDAR(obj:pointer):boolean;
function  GTK_IS_CALENDAR_CLASS(klass:pointer):boolean;

function  gtk_calendar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_calendar_get_type';
function  gtk_calendar_new:PGtkWidget;cdecl;external gtkdll name 'gtk_calendar_new';
function  gtk_calendar_select_month(calendar:PGtkCalendar; month:guint; year:guint):gint;cdecl;external gtkdll name 'gtk_calendar_select_month';
procedure gtk_calendar_select_day(calendar:PGtkCalendar; day:guint);cdecl;external gtkdll name 'gtk_calendar_select_day';
function  gtk_calendar_mark_day(calendar:PGtkCalendar; day:guint):gint;cdecl;external gtkdll name 'gtk_calendar_mark_day';
function  gtk_calendar_unmark_day(calendar:PGtkCalendar; day:guint):gint;cdecl;external gtkdll name 'gtk_calendar_unmark_day';
procedure gtk_calendar_clear_marks(calendar:PGtkCalendar);cdecl;external gtkdll name 'gtk_calendar_clear_marks';
procedure gtk_calendar_display_options(calendar:PGtkCalendar; flags:TGtkCalendarDisplayOptions);cdecl;external gtkdll name 'gtk_calendar_display_options';
procedure gtk_calendar_get_date(calendar:PGtkCalendar; year:Pguint; month:Pguint; day:Pguint);cdecl;external gtkdll name 'gtk_calendar_get_date';
procedure gtk_calendar_freeze(calendar:PGtkCalendar);cdecl;external gtkdll name 'gtk_calendar_freeze';
procedure gtk_calendar_thaw(calendar:PGtkCalendar);cdecl;external gtkdll name 'gtk_calendar_thaw';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_CALENDAR(obj:pointer):boolean;
begin
  GTK_IS_CALENDAR:=(obj<>nil) and GTK_IS_CALENDAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CALENDAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CALENDAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CALENDAR_TYPE);
end;

{$endif read_implementation}


