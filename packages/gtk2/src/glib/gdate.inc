// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGTime      = ^TGTime;
  TGTime      = gint32;

  PGDateYear  = ^TGDateYear;
  TGDateYear  = guint16;

  PGDateDay   = ^TGDateDay;
  TGDateDay   = guint8;   // day of the month

  Ptm         = ^Ttm;
  Ttm         = record
              tm_sec   : gint;                     // Seconds.     [0-60] (1 leap second)
              tm_min   : gint;                     // Minutes.     [0-59]
              tm_hour  : gint;                     // Hours.       [0-23]
              tm_mday  : gint;                     // Day.         [1-31]
              tm_mon   : gint;                     // Month.       [0-11]
              tm_year  : gint;                     // Year - 1900.
              tm_wday  : gint;                     // Day of week. [0-6]
              tm_yday  : gint;                     // Days in year.[0-365]
              tm_isdst : gint;                     // DST.         [-1/0/1]
              tm_gmtoff: glong;        // Seconds east of UTC.
              tm_zone  : pgchar;       // Timezone abbreviation.
       end;

  PGDateDMY = ^TGDateDMY;
  TGDateDMY = integer;

  PGDateWeekday = ^TGDateWeekday;
  TGDateWeekday = integer;

  PGDateMonth = ^TGDateMonth;
  TGDateMonth = integer;

  PGDate = ^TGDate;
  TGDate = record
    flag0 : longint;
    flag1 : longint;
  end;
{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}
const 
  G_DATE_DAY = 0;
  G_DATE_MONTH = 1;
  G_DATE_YEAR = 2;

const 
  G_DATE_BAD_WEEKDAY = 0;
  G_DATE_MONDAY = 1;
  G_DATE_TUESDAY = 2;
  G_DATE_WEDNESDAY = 3;
  G_DATE_THURSDAY = 4;
  G_DATE_FRIDAY = 5;
  G_DATE_SATURDAY = 6;
  G_DATE_SUNDAY = 7;

const
  G_DATE_BAD_MONTH = 0;
  G_DATE_JANUARY = 1;
  G_DATE_FEBRUARY = 2;
  G_DATE_MARCH = 3;
  G_DATE_APRIL = 4;
  G_DATE_MAY = 5;
  G_DATE_JUNE = 6;
  G_DATE_JULY = 7;
  G_DATE_AUGUST = 8;
  G_DATE_SEPTEMBER = 9;
  G_DATE_OCTOBER = 10;
  G_DATE_NOVEMBER = 11;
  G_DATE_DECEMBER = 12;

const
  G_DATE_BAD_JULIAN = 0;
  G_DATE_BAD_DAY    = 0;
  G_DATE_BAD_YEAR   = 0;


function  g_date_new:PGDate; cdecl;external gliblib name 'g_date_new';
function  g_date_new_dmy(day:TGDateDay; month:TGDateMonth; year:TGDateYear):PGDate;cdecl;external gliblib name 'g_date_new_dmy';
function  g_date_new_julian(julian_day:guint32):PGDate;cdecl;external gliblib name 'g_date_new_julian';
procedure g_date_free(date:PGDate);cdecl;external gliblib name 'g_date_free';

function g_date_valid         (date:PGDate):      gboolean;cdecl;external gliblib name 'g_date_valid';
function g_date_valid_month   (month:TGDateMonth):gboolean;cdecl;external gliblib name 'g_date_valid_month';
function g_date_valid_year    (year:TGDateYear):  gboolean;cdecl;external gliblib name 'g_date_valid_year';
function g_date_valid_weekday (weekday:TGDateWeekday):  gboolean;cdecl;external gliblib name 'g_date_valid_weekday';
function g_date_valid_julian  (julian_date:guint32):    gboolean;cdecl;external gliblib name 'g_date_valid_julian';


function g_date_get_weekday(date:PGDate):TGDateWeekday;cdecl;external gliblib name 'g_date_get_weekday';
function g_date_get_month(date:PGDate):TGDateMonth;cdecl;external gliblib name 'g_date_get_month';
function g_date_get_year(date:PGDate):TGDateYear;cdecl;external gliblib name 'g_date_get_year';
function g_date_get_day(date:PGDate):TGDateDay;cdecl;external gliblib name 'g_date_get_day';
function g_date_get_julian(date:PGDate):guint32;cdecl;external gliblib name 'g_date_get_julian';
function g_date_get_day_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_day_of_year';

function g_date_get_monday_week_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_monday_week_of_year';
function g_date_get_sunday_week_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_sunday_week_of_year';

procedure g_date_clear(date:PGDate; n_dates:guint);cdecl;external gliblib name 'g_date_clear';

procedure g_date_set_parse(date:PGDate; str:Pgchar);cdecl;external gliblib name 'g_date_set_parse';
procedure g_date_set_time(date:PGDate; time:TGTime);cdecl;external gliblib name 'g_date_set_time';
procedure g_date_set_month(date:PGDate; month:TGDateMonth);cdecl;external gliblib name 'g_date_set_month';
procedure g_date_set_day(date:PGDate; day:TGDateDay);cdecl;external gliblib name 'g_date_set_day';
procedure g_date_set_year(date:PGDate; year:TGDateYear);cdecl;external gliblib name 'g_date_set_year';
procedure g_date_set_dmy(date:PGDate; day:TGDateDay; month:TGDateMonth; y:TGDateYear);cdecl;external gliblib name 'g_date_set_dmy';
procedure g_date_set_julian(date:PGDate; julian_date:guint32);cdecl;external gliblib name 'g_date_set_julian';
function g_date_is_first_of_month(date:PGDate):gboolean;cdecl;external gliblib name 'g_date_is_first_of_month';
function g_date_is_last_of_month(date:PGDate):gboolean;cdecl;external gliblib name 'g_date_is_last_of_month';


procedure g_date_add_days(date:PGDate; n_days:guint);cdecl;external gliblib name 'g_date_add_days';
procedure g_date_subtract_days(date:PGDate; n_days:guint);cdecl;external gliblib name 'g_date_subtract_days';

procedure g_date_add_months(date:PGDate; n_months:guint);cdecl;external gliblib name 'g_date_add_months';
procedure g_date_subtract_months(date:PGDate; n_months:guint);cdecl;external gliblib name 'g_date_subtract_months';

procedure g_date_add_years(date:PGDate; n_years:guint);cdecl;external gliblib name 'g_date_add_years';
procedure g_date_subtract_years(date:PGDate; n_years:guint);cdecl;external gliblib name 'g_date_subtract_years';

function  g_date_is_leap_year (year:TGDateYear): gboolean; cdecl;external gliblib name 'g_date_is_leap_year';
function  g_date_get_days_in_month   (month: TGDateMonth; year: TGDateYear):guint8;cdecl;external gliblib name 'g_date_get_days_in_month';
function  g_date_get_monday_weeks_in_year (year:TGDateYear): guint8; cdecl;external gliblib name 'g_date_get_monday_weeks_in_year';
function  g_date_get_sunday_weeks_in_year (year:TGDateYear): guint8; cdecl;external gliblib name 'g_date_get_sunday_weeks_in_year';

function g_date_days_between(date1:PGDate; date2:PGDate):gint;cdecl;external gliblib name 'g_date_days_between';

function g_date_compare(lhs:PGDate; rhs:PGDate):gint;cdecl;external gliblib name 'g_date_compare';

procedure g_date_to_struct_tm(date:PGDate; tm:Ptm);cdecl;external gliblib name 'g_date_to_struct_tm';
procedure g_date_clamp(date:PGDate; min_date:PGDate; max_date:PGDate);cdecl;external gliblib name 'g_date_clamp';

procedure g_date_order(date1:PGDate; date2:PGDate);cdecl;external gliblib name 'g_date_order';

function g_date_strftime(s:Pgchar; slen:gsize; format:Pgchar; date:PGDate):gsize;cdecl;external gliblib name 'g_date_strftime';

{ DEPRECATED functions are maped to their newer versions as the arguments and the return value are the same}

{ DEPRECATED functions are disabled because a
  name conflict with G_DATE_MONTH, G_DATE_YEAR and G_DATE_DAY

    function g_date_weekday(date:PGDate):TGDateWeekday;cdecl;external gliblib name 'g_date_get_weekday';
    function g_date_month(date:PGDate):TGDateMonth;cdecl;external gliblib name 'g_date_get_month';
    function g_date_year(date:PGDate):TGDateYear;cdecl;external gliblib name 'g_date_get_year';
    function g_date_day(date:PGDate):TGDateDay;cdecl;external gliblib name 'g_date_get_day';
    function g_date_julian(date:PGDate):guint32;cdecl;external gliblib name 'g_date_get_julian';
    function g_date_day_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_day_of_year';
    function g_date_monday_week_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_monday_week_of_year';
    function g_date_sunday_week_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_sunday_week_of_year';
    function g_date_days_in_month   (month: TGDateMonth; year: TGDateYear):guint8;cdecl;external gliblib name 'g_date_get_days_in_month';
    function g_date_monday_weeks_in_year (year:TGDateYear): guint8; cdecl;external gliblib name 'g_date_get_monday_weeks_in_year';
    function g_date_sunday_weeks_in_year (year:TGDateYear): guint8; cdecl;external gliblib name 'g_date_get_sunday_weeks_in_year';
}
{$ENDIF read_interface_rest}
