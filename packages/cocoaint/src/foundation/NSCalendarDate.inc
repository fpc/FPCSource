{ Parsed from Foundation.framework NSCalendarDate.h }

{$ifdef TYPES}
{$ifndef NSCALENDARDATE_PAS_T}
{$define NSCALENDARDATE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCALENDARDATE_PAS_R}
{$define NSCALENDARDATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCALENDARDATE_PAS_F}
{$define NSCALENDARDATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCALENDARDATE_PAS_S}
{$define NSCALENDARDATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCalendarDate = objcclass;
  NSCalendarDatePointer = ^NSCalendarDate;
  NSCalendarDatePtr = NSCalendarDatePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCALENDARDATE_PAS_C}
{$define NSCALENDARDATE_PAS_C}

{ NSCalendarDate }
  NSCalendarDate = objcclass external (NSDate)
  private
    refCount: NSUInteger;
    _timeIntervalSinceReferenceDate: NSTimeInterval;
    _timeZone: NSTimeZone;
    _formatString: NSString;
    _reserved: Pointer;
    
  public
    class function calendarDate: id; message 'calendarDate';
    class function dateWithString_calendarFormat_locale(description_: NSString; format: NSString; locale: id): id; message 'dateWithString:calendarFormat:locale:';
    class function dateWithString_calendarFormat(description_: NSString; format: NSString): id; message 'dateWithString:calendarFormat:';
    class function dateWithYear_month_day_hour_minute_second_timeZone(year: NSInteger; month: NSUInteger; day: NSUInteger; hour: NSUInteger; minute: NSUInteger; second: NSUInteger; aTimeZone: NSTimeZone): id; message 'dateWithYear:month:day:hour:minute:second:timeZone:';
    function dateByAddingYears_months_days_hours_minutes_seconds(year: NSInteger; month: NSInteger; day: NSInteger; hour: NSInteger; minute: NSInteger; second: NSInteger): NSCalendarDate; message 'dateByAddingYears:months:days:hours:minutes:seconds:';
    function dayOfCommonEra: NSInteger; message 'dayOfCommonEra';
    function dayOfMonth: NSInteger; message 'dayOfMonth';
    function dayOfWeek: NSInteger; message 'dayOfWeek';
    function dayOfYear: NSInteger; message 'dayOfYear';
    function hourOfDay: NSInteger; message 'hourOfDay';
    function minuteOfHour: NSInteger; message 'minuteOfHour';
    function monthOfYear: NSInteger; message 'monthOfYear';
    function secondOfMinute: NSInteger; message 'secondOfMinute';
    function yearOfCommonEra: NSInteger; message 'yearOfCommonEra';
    function calendarFormat: NSString; message 'calendarFormat';
    function descriptionWithCalendarFormat_locale(format: NSString; locale: id): NSString; message 'descriptionWithCalendarFormat:locale:';
    function descriptionWithCalendarFormat(format: NSString): NSString; message 'descriptionWithCalendarFormat:';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function timeZone: NSTimeZone; message 'timeZone';
    function initWithString_calendarFormat_locale(description_: NSString; format: NSString; locale: id): id; message 'initWithString:calendarFormat:locale:';
    function initWithString_calendarFormat(description_: NSString; format: NSString): id; message 'initWithString:calendarFormat:';
    function initWithString(description_: NSString): id; message 'initWithString:';
    function initWithYear_month_day_hour_minute_second_timeZone(year: NSInteger; month: NSUInteger; day: NSUInteger; hour: NSUInteger; minute: NSUInteger; second: NSUInteger; aTimeZone: NSTimeZone): id; message 'initWithYear:month:day:hour:minute:second:timeZone:';
    procedure setCalendarFormat(format: NSString); message 'setCalendarFormat:';
    procedure setTimeZone(aTimeZone: NSTimeZone); message 'setTimeZone:';
    procedure years_months_days_hours_minutes_seconds_sinceDate(yp: NSIntegerPtr; mop: NSIntegerPtr; dp: NSIntegerPtr; hp: NSIntegerPtr; mip: NSIntegerPtr; sp: NSIntegerPtr; date_: NSCalendarDate); message 'years:months:days:hours:minutes:seconds:sinceDate:';
  end;

{ NSCalendarDateExtrasCategory }
  NSCalendarDateExtrasCategory = objccategory external (NSDate)
    class function dateWithNaturalLanguageString_locale(string_: NSString; locale: id): id; message 'dateWithNaturalLanguageString:locale:';
    class function dateWithNaturalLanguageString(string_: NSString): id; message 'dateWithNaturalLanguageString:';
    class function dateWithString(aString: NSString): id; message 'dateWithString:';
    function dateWithCalendarFormat_timeZone(format: NSString; aTimeZone: NSTimeZone): NSCalendarDate; message 'dateWithCalendarFormat:timeZone:';
    function descriptionWithCalendarFormat_timeZone_locale(format: NSString; aTimeZone: NSTimeZone; locale: id): NSString; message 'descriptionWithCalendarFormat:timeZone:locale:';
    function initWithString(description_: NSString): id; message 'initWithString:';
  end;

{$endif}
{$endif}
