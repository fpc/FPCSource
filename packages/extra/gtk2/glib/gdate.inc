// {$include <glib/gquark.h>}
{ GDate

  Date calculations (not time for now, to be resolved). These are a
  mutant combination of Steffen Beyer's DateCalc routines
 (http://www.perl.com/CPAN/authors/id/STBEY/) and Jon Trowbridge's
  date routines (written for in-house software).  Written by Havoc
  Pennington <hp@pobox.com>
}

  type
       PGTime      = ^TGTime;
       TGTime      = gint32;

       PGDateYear  = ^TGDateYear;
       TGDateYear  = guint16;

       PGDateDay   = ^TGDateDay;
       TGDateDay   = guint8;   // day of the month

    { day of the month  }
    { make struct tm known without having to include time.h  }

       { from time.h }
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


    { enum used to specify order of appearance in parsed date strings  }

     type
       PGDateDMY = ^TGDateDMY;
       TGDateDMY = integer;
     const G_DATE_DAY = 0;
           G_DATE_MONTH = 1;
           G_DATE_YEAR = 2;

    { actual week and month values  }

     type
       PGDateWeekday = ^TGDateWeekday;
       TGDateWeekday = integer;
     const G_DATE_BAD_WEEKDAY = 0;
           G_DATE_MONDAY = 1;
           G_DATE_TUESDAY = 2;
           G_DATE_WEDNESDAY = 3;
           G_DATE_THURSDAY = 4;
           G_DATE_FRIDAY = 5;
           G_DATE_SATURDAY = 6;
           G_DATE_SUNDAY = 7;

     type
       PGDateMonth = ^TGDateMonth;
       TGDateMonth = integer;
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

    { Note: directly manipulating structs is generally a bad idea, but
       in this case it's an  incredibly  bad idea, because all or part
       of this struct can be invalid at any given time. Use the functions,
       or you will get hosed, I promise.
      }

    { julian days representation - we use a
                                  bitfield hoping that 64 bit platforms
                                  will pack this whole struct in one big
                                  int
                                }
    { julian is valid  }
    { dmy is valid  }
    { DMY representation  }

    type
       PGDate = ^TGDate;
       TGDate = record
            flag0 : longint;
            flag1 : longint;
         end;

    { g_date_new() returns an invalid date, you then have to _set() stuff
       to get a usable anObject. You can also allocate a GDate statically,
       then call g_date_clear() to initialize.
      }

    function  g_date_new:PGDate; cdecl;external gliblib name 'g_date_new';

    function  g_date_new_dmy(day:TGDateDay; month:TGDateMonth; year:TGDateYear):PGDate;cdecl;external gliblib name 'g_date_new_dmy';

    function  g_date_new_julian(julian_day:guint32):PGDate;cdecl;external gliblib name 'g_date_new_julian';

    procedure g_date_free(date:PGDate);cdecl;external gliblib name 'g_date_free';

    { check g_date_valid() after doing an operation that might fail, like
       _parse.  Almost all g_date operations are undefined on invalid
       dates (the exceptions are the mutators, since you need those to
       return to validity).
      }

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

    { First monday/sunday is the start of week 1; if we haven't reached
       that day, return 0. These are not ISO weeks of the year; that
       routine needs to be added.
       these functions return the number of weeks, starting on the
       corrsponding day
      }

    function g_date_get_monday_week_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_monday_week_of_year';

    function g_date_get_sunday_week_of_year(date:PGDate):guint;cdecl;external gliblib name 'g_date_get_sunday_week_of_year';

    { If you create a static date struct you need to clear it to get it
       in a sane state before use. You can clear a whole array at
       once with the ndates argument.
      }
    procedure g_date_clear(date:PGDate; n_dates:guint);cdecl;external gliblib name 'g_date_clear';

    { The parse routine is meant for dates typed in by a user, so it
       permits many formats but tries to catch common typos. If your data
       needs to be strictly validated, it is not an appropriate function.
      }

    procedure g_date_set_parse(date:PGDate; str:Pgchar);cdecl;external gliblib name 'g_date_set_parse';

    procedure g_date_set_time(date:PGDate; time:TGTime);cdecl;external gliblib name 'g_date_set_time';

    procedure g_date_set_month(date:PGDate; month:TGDateMonth);cdecl;external gliblib name 'g_date_set_month';

    procedure g_date_set_day(date:PGDate; day:TGDateDay);cdecl;external gliblib name 'g_date_set_day';

    procedure g_date_set_year(date:PGDate; year:TGDateYear);cdecl;external gliblib name 'g_date_set_year';

    procedure g_date_set_dmy(date:PGDate; day:TGDateDay; month:TGDateMonth; y:TGDateYear);cdecl;external gliblib name 'g_date_set_dmy';

    procedure g_date_set_julian(date:PGDate; julian_date:guint32);cdecl;external gliblib name 'g_date_set_julian';

    function g_date_is_first_of_month(date:PGDate):gboolean;cdecl;external gliblib name 'g_date_is_first_of_month';

    function g_date_is_last_of_month(date:PGDate):gboolean;cdecl;external gliblib name 'g_date_is_last_of_month';


    { To go forward by some number of weeks just go forward weeks 7 days  }
    procedure g_date_add_days(date:PGDate; n_days:guint);cdecl;external gliblib name 'g_date_add_days';

    procedure g_date_subtract_days(date:PGDate; n_days:guint);cdecl;external gliblib name 'g_date_subtract_days';


    { If you add/sub months while day > 28, the day might change  }
    procedure g_date_add_months(date:PGDate; n_months:guint);cdecl;external gliblib name 'g_date_add_months';

    procedure g_date_subtract_months(date:PGDate; n_months:guint);cdecl;external gliblib name 'g_date_subtract_months';


    { If it's feb 29, changing years can move you to the 28th  }
    procedure g_date_add_years(date:PGDate; n_years:guint);cdecl;external gliblib name 'g_date_add_years';

    procedure g_date_subtract_years(date:PGDate; n_years:guint);cdecl;external gliblib name 'g_date_subtract_years';


    function  g_date_is_leap_year (year:TGDateYear): gboolean; cdecl;external gliblib name 'g_date_is_leap_year';

    function  g_date_get_days_in_month   (month: TGDateMonth; year: TGDateYear):guint8;cdecl;external gliblib name 'g_date_get_days_in_month';

    function  g_date_get_monday_weeks_in_year (year:TGDateYear): guint8; cdecl;external gliblib name 'g_date_get_monday_weeks_in_year';

    function  g_date_get_sunday_weeks_in_year (year:TGDateYear): guint8; cdecl;external gliblib name 'g_date_get_sunday_weeks_in_year';

    { Returns the number of days between the two dates.  If date2 comes
       before date1, a negative value is return.  }

    function g_date_days_between(date1:PGDate; date2:PGDate):gint;cdecl;external gliblib name 'g_date_days_between';

    { qsort-friendly (with a cast...)  }
    function g_date_compare(lhs:PGDate; rhs:PGDate):gint;cdecl;external gliblib name 'g_date_compare';


    procedure g_date_to_struct_tm(date:PGDate; tm:Ptm);cdecl;external gliblib name 'g_date_to_struct_tm';

    procedure g_date_clamp(date:PGDate; min_date:PGDate; max_date:PGDate);cdecl;external gliblib name 'g_date_clamp';

    { Swap date1 and date2's values if date1 > date2.  }
    procedure g_date_order(date1:PGDate; date2:PGDate);cdecl;external gliblib name 'g_date_order';

    { Just like strftime() except you can only use date-related formats.
         Using a time format is undefined.
      }
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
