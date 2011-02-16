{ Parsed from Foundation.framework NSCalendar.h }

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
  NSQuarterCalendarUnit = kCFCalendarUnitQuarter;

const
  NSWrapCalendarComponents = kCFCalendarComponentsWrap;

const
  NSUndefinedDateComponent = NSIntegerMax;

{ Types }
type
  NSCalendarUnit = NSUInteger;
  NSCalendarUnitPtr = ^NSCalendarUnit;

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
{$ifndef NSCALENDAR_PAS_S}
{$define NSCALENDAR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCalendar = objcclass;
  NSCalendarPointer = ^NSCalendar;
  NSCalendarPtr = NSCalendarPointer;
  NSDateComponents = objcclass;
  NSDateComponentsPointer = ^NSDateComponents;
  NSDateComponentsPtr = NSDateComponentsPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCALENDAR_PAS_C}
{$define NSCALENDAR_PAS_C}

{ NSCalendar }
  NSCalendar = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function currentCalendar: id; message 'currentCalendar';
    class function autoupdatingCurrentCalendar: id; message 'autoupdatingCurrentCalendar';
    function initWithCalendarIdentifier(ident: NSString): id; message 'initWithCalendarIdentifier:';
    function calendarIdentifier: NSString; message 'calendarIdentifier';
    procedure setLocale(locale_: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setTimeZone(tz: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setFirstWeekday(weekday: NSUInteger); message 'setFirstWeekday:';
    function firstWeekday: NSUInteger; message 'firstWeekday';
    procedure setMinimumDaysInFirstWeek(mdw: NSUInteger); message 'setMinimumDaysInFirstWeek:';
    function minimumDaysInFirstWeek: NSUInteger; message 'minimumDaysInFirstWeek';
    function minimumRangeOfUnit(unit_: NSCalendarUnit): NSRange; message 'minimumRangeOfUnit:';
    function maximumRangeOfUnit(unit_: NSCalendarUnit): NSRange; message 'maximumRangeOfUnit:';
    function rangeOfUnit_inUnit_forDate(smaller: NSCalendarUnit; larger: NSCalendarUnit; date: NSDate): NSRange; message 'rangeOfUnit:inUnit:forDate:';
    function ordinalityOfUnit_inUnit_forDate(smaller: NSCalendarUnit; larger: NSCalendarUnit; date: NSDate): NSUInteger; message 'ordinalityOfUnit:inUnit:forDate:';
    function rangeOfUnit_startDate_interval_forDate(unit_: NSCalendarUnit; datep: NSDatePointer; tip: NSTimeIntervalPtr; date: NSDate): Boolean; message 'rangeOfUnit:startDate:interval:forDate:';
    function dateFromComponents(comps: NSDateComponents): NSDate; message 'dateFromComponents:';
    function components_fromDate(unitFlags: NSUInteger; date: NSDate): NSDateComponents; message 'components:fromDate:';
    function dateByAddingComponents_toDate_options(comps: NSDateComponents; date: NSDate; opts: NSUInteger): NSDate; message 'dateByAddingComponents:toDate:options:';
    function components_fromDate_toDate_options(unitFlags: NSUInteger; startingDate: NSDate; resultDate: NSDate; opts: NSUInteger): NSDateComponents; message 'components:fromDate:toDate:options:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSDateComponents }
  NSDateComponents = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function era: NSInteger; message 'era';
    function year: NSInteger; message 'year';
    function month: NSInteger; message 'month';
    function day: NSInteger; message 'day';
    function hour: NSInteger; message 'hour';
    function minute: NSInteger; message 'minute';
    function second: NSInteger; message 'second';
    function week: NSInteger; message 'week';
    function weekday: NSInteger; message 'weekday';
    function weekdayOrdinal: NSInteger; message 'weekdayOrdinal';
    function quarter: NSInteger; message 'quarter';
    procedure setEra(v: NSInteger); message 'setEra:';
    procedure setYear(v: NSInteger); message 'setYear:';
    procedure setMonth(v: NSInteger); message 'setMonth:';
    procedure setDay(v: NSInteger); message 'setDay:';
    procedure setHour(v: NSInteger); message 'setHour:';
    procedure setMinute(v: NSInteger); message 'setMinute:';
    procedure setSecond(v: NSInteger); message 'setSecond:';
    procedure setWeek(v: NSInteger); message 'setWeek:';
    procedure setWeekday(v: NSInteger); message 'setWeekday:';
    procedure setWeekdayOrdinal(v: NSInteger); message 'setWeekdayOrdinal:';
    procedure setQuarter(v: NSInteger); message 'setQuarter:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
