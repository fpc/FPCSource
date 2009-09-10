{ Parsed from Foundation.framework NSCalendarDate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCALENDARDATE_PAS_H}
{$define NSCALENDARDATE_PAS_H}
type
  NSCalendarDatePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSCALENDARDATE_PAS_T}
{$define NSCALENDARDATE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCalendarDate = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCALENDARDATE_PAS_C}
{$define NSCALENDARDATE_PAS_C}

{ NSCalendarDate }
  NSCalendarDate = objcclass(NSDate)
  private
    _refCount: culong;
    __timeIntervalSinceReferenceDate: NSTimeInterval;
    __timeZone: NSTimeZone;
    __formatString: NSString;
    __reserved: Pointer;
    
  public
    class function alloc: NSCalendarDate; message 'alloc';

    class function dateWithYear_month_day_hour_minute_second_timeZone(year: clong; month: culong; day: culong; hour: culong; minute: culong; second: culong; aTimeZone: NSTimeZone): id; message 'dateWithYear:month:day:hour:minute:second:timeZone:';
    class function dateWithString_calendarFormat(description_: NSString; format: NSString): id; message 'dateWithString:calendarFormat:';
    class function dateWithString_calendarFormat_locale(description_: NSString; format: NSString; locale: id): id; message 'dateWithString:calendarFormat:locale:';
    class function calendarDate: id; message 'calendarDate';
    function initWithYear_month_day_hour_minute_second_timeZone(year: clong; month: culong; day: culong; hour: culong; minute: culong; second: culong; aTimeZone: NSTimeZone): id; message 'initWithYear:month:day:hour:minute:second:timeZone:';
    function initWithString(description_: NSString): id; message 'initWithString:';
    function initWithString_calendarFormat(description_: NSString; format: NSString): id; message 'initWithString:calendarFormat:';
    function initWithString_calendarFormat_locale(description_: NSString; format: NSString; locale: id): id; message 'initWithString:calendarFormat:locale:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setTimeZone(aTimeZone: NSTimeZone); message 'setTimeZone:';
    function calendarFormat: NSString; message 'calendarFormat';
    procedure setCalendarFormat(format: NSString); message 'setCalendarFormat:';
    function yearOfCommonEra: clong; message 'yearOfCommonEra';
    function monthOfYear: clong; message 'monthOfYear';
    function dayOfMonth: clong; message 'dayOfMonth';
    function dayOfWeek: clong; message 'dayOfWeek';
    function dayOfYear: clong; message 'dayOfYear';
    function dayOfCommonEra: clong; message 'dayOfCommonEra';
    function hourOfDay: clong; message 'hourOfDay';
    function minuteOfHour: clong; message 'minuteOfHour';
    function secondOfMinute: clong; message 'secondOfMinute';
    function dateByAddingYears_months_days_hours_minutes_seconds(year: clong; month: clong; day: clong; hour: clong; minute: clong; second: clong): NSCalendarDate; message 'dateByAddingYears:months:days:hours:minutes:seconds:';
    procedure years_months_days_hours_minutes_seconds_sinceDate(var yp: clong; var mop: clong; var dp: clong; var hp: clong; var mip: clong; var sp: clong; date_: NSCalendarDate); message 'years:months:days:hours:minutes:seconds:sinceDate:';
    function description: NSString; message 'description';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithCalendarFormat(format: NSString): NSString; message 'descriptionWithCalendarFormat:';
    function descriptionWithCalendarFormat_locale(format: NSString; locale: id): NSString; message 'descriptionWithCalendarFormat:locale:';
  end; external;

{$endif}
{$endif}
