{ Parsed from Foundation.framework NSCalendar.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCALENDAR_PAS_H}
{$define NSCALENDAR_PAS_H}
type
  NSCalendarPointer = Pointer;
  NSDateComponentsPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCALENDAR_PAS_T}
{$define NSCALENDAR_PAS_T}

{ Constants }

const
  NSEraCalendarUnit = kCFCalendarUnitEra;
  NSYearCalendarUnit = kCFCalendarUnitYear;
  NSMonthCalendarUnit = kCFCalendarUnitMonth;
  NSDayCalendarUnit = kCFCalendarUnitDay;
  NSHourCalendarUnit = kCFCalendarUnitHour;
  NSMinuteCalendarUnit = kCFCalendarUnitMinute;
  NSSecondCalendarUnit = kCFCalendarUnitSecond;
  NSWeekCalendarUnit = kCFCalendarUnitWeek;
  NSWeekdayCalendarUnit = kCFCalendarUnitWeekday;
  NSWeekdayOrdinalCalendarUnit = kCFCalendarUnitWeekdayOrdinal;

const
  NSUndefinedDateComponent = NSIntegerMax;

{ Types }
type
  NSCalendarUnit = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCALENDAR_PAS_R}
{$define NSCALENDAR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCALENDAR_PAS_F}
{$define NSCALENDAR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCALENDAR_PAS_T}
{$define NSCALENDAR_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCalendar = objcclass;
  NSDateComponents = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCALENDAR_PAS_C}
{$define NSCALENDAR_PAS_C}

{ NSCalendar }
  NSCalendar = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSCalendar; message 'alloc';

    class function currentCalendar: id; message 'currentCalendar';
    function initWithCalendarIdentifier(ident: NSString): id; message 'initWithCalendarIdentifier:';
    function calendarIdentifier: NSString; message 'calendarIdentifier';
    procedure setLocale(locale_: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setTimeZone(tz: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setFirstWeekday(weekday: culong); message 'setFirstWeekday:';
    function firstWeekday: culong; message 'firstWeekday';
    procedure setMinimumDaysInFirstWeek(mdw: culong); message 'setMinimumDaysInFirstWeek:';
    function minimumDaysInFirstWeek: culong; message 'minimumDaysInFirstWeek';
    function minimumRangeOfUnit(unit_: NSCalendarUnit): NSRange; message 'minimumRangeOfUnit:';
    function maximumRangeOfUnit(unit_: NSCalendarUnit): NSRange; message 'maximumRangeOfUnit:';
    function rangeOfUnit_inUnit_forDate(smaller: NSCalendarUnit; larger: NSCalendarUnit; date: NSDate): NSRange; message 'rangeOfUnit:inUnit:forDate:';
    function ordinalityOfUnit_inUnit_forDate(smaller: NSCalendarUnit; larger: NSCalendarUnit; date: NSDate): culong; message 'ordinalityOfUnit:inUnit:forDate:';
    function rangeOfUnit_startDate_interval_forDate(unit_: NSCalendarUnit; var datep: NSDate; var tip: NSTimeInterval; date: NSDate): Boolean; message 'rangeOfUnit:startDate:interval:forDate:';
    function dateFromComponents(comps: NSDateComponents): NSDate; message 'dateFromComponents:';
    function components_fromDate(unitFlags: culong; date: NSDate): NSDateComponents; message 'components:fromDate:';
    function dateByAddingComponents_toDate_options(comps: NSDateComponents; date: NSDate; opts: culong): NSDate; message 'dateByAddingComponents:toDate:options:';
    function components_fromDate_toDate_options(unitFlags: culong; startingDate: NSDate; resultDate: NSDate; opts: culong): NSDateComponents; message 'components:fromDate:toDate:options:';
  end; external;

{ NSDateComponents }
  NSDateComponents = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSDateComponents; message 'alloc';

    function era: clong; message 'era';
    function year: clong; message 'year';
    function month: clong; message 'month';
    function day: clong; message 'day';
    function hour: clong; message 'hour';
    function minute: clong; message 'minute';
    function second: clong; message 'second';
    function week: clong; message 'week';
    function weekday: clong; message 'weekday';
    function weekdayOrdinal: clong; message 'weekdayOrdinal';
    procedure setEra(v: clong); message 'setEra:';
    procedure setYear(v: clong); message 'setYear:';
    procedure setMonth(v: clong); message 'setMonth:';
    procedure setDay(v: clong); message 'setDay:';
    procedure setHour(v: clong); message 'setHour:';
    procedure setMinute(v: clong); message 'setMinute:';
    procedure setSecond(v: clong); message 'setSecond:';
    procedure setWeek(v: clong); message 'setWeek:';
    procedure setWeekday(v: clong); message 'setWeekday:';
    procedure setWeekdayOrdinal(v: clong); message 'setWeekdayOrdinal:';
  end; external;

{$endif}
{$endif}
