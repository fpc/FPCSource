{ Parsed from Foundation.framework NSCalendarDate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

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
{$ifndef NSCALENDARDATE_PAS_S}
{$define NSCALENDARDATE_PAS_S}

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
    _refCount: NSUInteger;
    __timeIntervalSinceReferenceDate: NSTimeInterval;
    __timeZone: NSTimeZone;
    __formatString: NSString;
    __reserved: Pointer;
    
  public
    class function alloc: NSCalendarDate; message 'alloc';

    class function dateWithYear_month_day_hour_minute_second_timeZone(year: NSInteger; month: NSUInteger; day: NSUInteger; hour: NSUInteger; minute: NSUInteger; second: NSUInteger; aTimeZone: NSTimeZone): id; message 'dateWithYear:month:day:hour:minute:second:timeZone:';
    class function dateWithString_calendarFormat(description_: NSString; format: NSString): id; message 'dateWithString:calendarFormat:';
    class function dateWithString_calendarFormat_locale(description_: NSString; format: NSString; locale: id): id; message 'dateWithString:calendarFormat:locale:';
    class function calendarDate: id; message 'calendarDate';
    function initWithYear_month_day_hour_minute_second_timeZone(year: NSInteger; month: NSUInteger; day: NSUInteger; hour: NSUInteger; minute: NSUInteger; second: NSUInteger; aTimeZone: NSTimeZone): id; message 'initWithYear:month:day:hour:minute:second:timeZone:';
    function initWithString(description_: NSString): id; message 'initWithString:';
    function initWithString_calendarFormat(description_: NSString; format: NSString): id; message 'initWithString:calendarFormat:';
    function initWithString_calendarFormat_locale(description_: NSString; format: NSString; locale: id): id; message 'initWithString:calendarFormat:locale:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setTimeZone(aTimeZone: NSTimeZone); message 'setTimeZone:';
    function calendarFormat: NSString; message 'calendarFormat';
    procedure setCalendarFormat(format: NSString); message 'setCalendarFormat:';
    function yearOfCommonEra: NSInteger; message 'yearOfCommonEra';
    function monthOfYear: NSInteger; message 'monthOfYear';
    function dayOfMonth: NSInteger; message 'dayOfMonth';
    function dayOfWeek: NSInteger; message 'dayOfWeek';
    function dayOfYear: NSInteger; message 'dayOfYear';
    function dayOfCommonEra: NSInteger; message 'dayOfCommonEra';
    function hourOfDay: NSInteger; message 'hourOfDay';
    function minuteOfHour: NSInteger; message 'minuteOfHour';
    function secondOfMinute: NSInteger; message 'secondOfMinute';
    function dateByAddingYears_months_days_hours_minutes_seconds(year: NSInteger; month: NSInteger; day: NSInteger; hour: NSInteger; minute: NSInteger; second: NSInteger): NSCalendarDate; message 'dateByAddingYears:months:days:hours:minutes:seconds:';
    procedure years_months_days_hours_minutes_seconds_sinceDate(yp: NSIntegerPointer; mop: NSIntegerPointer; dp: NSIntegerPointer; hp: NSIntegerPointer; mip: NSIntegerPointer; sp: NSIntegerPointer; date_: NSCalendarDate); message 'years:months:days:hours:minutes:seconds:sinceDate:';
    function description: NSString; message 'description';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithCalendarFormat(format: NSString): NSString; message 'descriptionWithCalendarFormat:';
    function descriptionWithCalendarFormat_locale(format: NSString; locale: id): NSString; message 'descriptionWithCalendarFormat:locale:';
  end; external;

{$endif}
{$endif}
