{ Parsed from Foundation.framework NSCalendarDate.h }


{$ifdef TYPES}
type
  NSCalendarDatePtr = ^NSCalendarDate;
{$endif}

{$ifdef CLASSES}

type
  NSCalendarDate = objcclass external (NSDate)
  private
    refCount: NSUInteger;
    _timeIntervalSinceReferenceDate: NSTimeInterval;
    _timeZone: NSTimeZone;
    _formatString: NSString;
    _reserved: pointer;
  public
    class function calendarDate: id; message 'calendarDate'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    class function dateWithString_calendarFormat_locale (description: NSString; format: NSString; locale: id): id; message 'dateWithString:calendarFormat:locale:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    class function dateWithString_calendarFormat (description: NSString; format: NSString): id; message 'dateWithString:calendarFormat:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    class function dateWithYear_month_day_hour_minute_second_timeZone (year: NSInteger; month: NSUInteger; day: NSUInteger; hour: NSUInteger; minute: NSUInteger; second: NSUInteger; aTimeZone: NSTimeZone): id; message 'dateWithYear:month:day:hour:minute:second:timeZone:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function dateByAddingYears_months_days_hours_minutes_seconds (year: NSInteger; month: NSInteger; day: NSInteger; hour: NSInteger; minute: NSInteger; second: NSInteger): NSCalendarDate; message 'dateByAddingYears:months:days:hours:minutes:seconds:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function dayOfCommonEra: NSInteger; message 'dayOfCommonEra'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function dayOfMonth: NSInteger; message 'dayOfMonth'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function dayOfWeek: NSInteger; message 'dayOfWeek'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function dayOfYear: NSInteger; message 'dayOfYear'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function hourOfDay: NSInteger; message 'hourOfDay'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function minuteOfHour: NSInteger; message 'minuteOfHour'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function monthOfYear: NSInteger; message 'monthOfYear'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function secondOfMinute: NSInteger; message 'secondOfMinute'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function yearOfCommonEra: NSInteger; message 'yearOfCommonEra'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function calendarFormat: NSString; message 'calendarFormat'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function descriptionWithCalendarFormat_locale (format: NSString; locale: id): NSString; message 'descriptionWithCalendarFormat:locale:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function descriptionWithCalendarFormat (format: NSString): NSString; message 'descriptionWithCalendarFormat:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function descriptionWithLocale (locale: id): NSString; message 'descriptionWithLocale:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function timeZone: NSTimeZone; message 'timeZone'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function initWithString_calendarFormat_locale (description: NSString; format: NSString; locale: id): id; message 'initWithString:calendarFormat:locale:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function initWithString_calendarFormat (description: NSString; format: NSString): id; message 'initWithString:calendarFormat:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function initWithString (description: NSString): id; message 'initWithString:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function initWithYear_month_day_hour_minute_second_timeZone (year: NSInteger; month: NSUInteger; day: NSUInteger; hour: NSUInteger; minute: NSUInteger; second: NSUInteger; aTimeZone: NSTimeZone): id; message 'initWithYear:month:day:hour:minute:second:timeZone:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    procedure setCalendarFormat (format: NSString); message 'setCalendarFormat:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    procedure setTimeZone (aTimeZone: NSTimeZone); message 'setTimeZone:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    procedure years_months_days_hours_minutes_seconds_sinceDate (yp: NSIntegerPtr; mop: NSIntegerPtr; dp: NSIntegerPtr; hp: NSIntegerPtr; mip: NSIntegerPtr; sp: NSIntegerPtr; date: NSCalendarDate); message 'years:months:days:hours:minutes:seconds:sinceDate:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
  end;


type
  NSCalendarDateExtras = objccategory external (NSDate)
    class function dateWithNaturalLanguageString_locale (string_: NSString; locale: id): id; message 'dateWithNaturalLanguageString:locale:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    class function dateWithNaturalLanguageString (string_: NSString): id; message 'dateWithNaturalLanguageString:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    class function dateWithString (aString: NSString): id; message 'dateWithString:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function dateWithCalendarFormat_timeZone (format: NSString; aTimeZone: NSTimeZone): NSCalendarDate; message 'dateWithCalendarFormat:timeZone:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function descriptionWithCalendarFormat_timeZone_locale (format: NSString; aTimeZone: NSTimeZone; locale: id): NSString; message 'descriptionWithCalendarFormat:timeZone:locale:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
    function initWithString (description: NSString): id; message 'initWithString:'; deprecated 'in 10_4, 10_10, 2_0, 8_0';
  end;
{$endif}

