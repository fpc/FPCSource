{ Parsed from Foundation.framework NSCalendar.h }


{$ifdef TYPES}
type
  NSCalendarPtr = ^NSCalendar;
  NSDateComponentsPtr = ^NSDateComponents;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSCalendarIdentifierGregorian: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierBuddhist: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierChinese: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierCoptic: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierEthiopicAmeteMihret: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierEthiopicAmeteAlem: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierHebrew: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierISO8601: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierIndian: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierIslamic: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierIslamicCivil: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierJapanese: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierPersian: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierRepublicOfChina: NSString { available in 10_6, 4_0 }; cvar; external;
  NSCalendarIdentifierIslamicTabular: NSString { available in 10_10, 8_0 }; cvar; external;
  NSCalendarIdentifierIslamicUmmAlQura: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSCalendarUnit = NSUInteger;
  NSCalendarUnitPtr = ^NSCalendarUnit;

const
  NSCalendarUnitEra = kCFCalendarUnitEra;
  NSCalendarUnitYear = kCFCalendarUnitYear;
  NSCalendarUnitMonth = kCFCalendarUnitMonth;
  NSCalendarUnitDay = kCFCalendarUnitDay;
  NSCalendarUnitHour = kCFCalendarUnitHour;
  NSCalendarUnitMinute = kCFCalendarUnitMinute;
  NSCalendarUnitSecond = kCFCalendarUnitSecond;
  NSCalendarUnitWeekday = kCFCalendarUnitWeekday;
  NSCalendarUnitWeekdayOrdinal = kCFCalendarUnitWeekdayOrdinal;
  NSCalendarUnitQuarter = kCFCalendarUnitQuarter { available in 10_6, 4_0 };
  NSCalendarUnitWeekOfMonth = kCFCalendarUnitWeekOfMonth { available in 10_7, 5_0 };
  NSCalendarUnitWeekOfYear = kCFCalendarUnitWeekOfYear { available in 10_7, 5_0 };
  NSCalendarUnitYearForWeekOfYear = kCFCalendarUnitYearForWeekOfYear { available in 10_7, 5_0 };
  NSCalendarUnitNanosecond = 1 shl 15 { available in 10_7, 5_0 };
  NSCalendarUnitCalendar = 1 shl 20 { available in 10_7, 4_0 };
  NSCalendarUnitTimeZone = 1 shl 21 { available in 10_7, 4_0 };
  NSEraCalendarUnit = NSCalendarUnitEra deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitEra instead"';
  NSYearCalendarUnit = NSCalendarUnitYear deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitYear instead"';
  NSMonthCalendarUnit = NSCalendarUnitMonth deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitMonth instead"';
  NSDayCalendarUnit = NSCalendarUnitDay deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitDay instead"';
  NSHourCalendarUnit = NSCalendarUnitHour deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitHour instead"';
  NSMinuteCalendarUnit = NSCalendarUnitMinute deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitMinute instead"';
  NSSecondCalendarUnit = NSCalendarUnitSecond deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitSecond instead"';
  NSWeekCalendarUnit = kCFCalendarUnitWeek deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitWeekOfMonth or NSCalendarUnitWeekOfYear, depending on which you mean"';
  NSWeekdayCalendarUnit = NSCalendarUnitWeekday deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitWeekday instead"';
  NSWeekdayOrdinalCalendarUnit = NSCalendarUnitWeekdayOrdinal deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarUnitWeekdayOrdinal instead"';
  NSQuarterCalendarUnit = NSCalendarUnitQuarter deprecated 'in 10_6, 10_10, 4_0, 8_0, "Use NSCalendarUnitQuarter instead"';
  NSWeekOfMonthCalendarUnit = NSCalendarUnitWeekOfMonth deprecated 'in 10_7, 10_10, 5_0, 8_0, "Use NSCalendarUnitWeekOfMonth instead"';
  NSWeekOfYearCalendarUnit = NSCalendarUnitWeekOfYear deprecated 'in 10_7, 10_10, 5_0, 8_0, "Use NSCalendarUnitWeekOfYear instead"';
  NSYearForWeekOfYearCalendarUnit = NSCalendarUnitYearForWeekOfYear deprecated 'in 10_7, 10_10, 5_0, 8_0, "Use NSCalendarUnitYearForWeekOfYear instead"';
  NSCalendarCalendarUnit = NSCalendarUnitCalendar deprecated 'in 10_7, 10_10, 4_0, 8_0, "Use NSCalendarUnitCalendar instead"';
  NSTimeZoneCalendarUnit = NSCalendarUnitTimeZone deprecated 'in 10_7, 10_10, 4_0, 8_0, "Use NSCalendarUnitTimeZone instead"';

type
  NSCalendarOptions = NSUInteger;
  NSCalendarOptionsPtr = ^NSCalendarOptions;

const
  NSCalendarWrapComponents = 1 shl 0;
  NSCalendarMatchStrictly = 1 shl 1 { available in 10_9, 7_0 };
  NSCalendarSearchBackwards = 1 shl 2 { available in 10_9, 7_0 };
  NSCalendarMatchPreviousTimePreservingSmallerUnits = 1 shl 8 { available in 10_9, 7_0 };
  NSCalendarMatchNextTimePreservingSmallerUnits = 1 shl 9 { available in 10_9, 7_0 };
  NSCalendarMatchNextTime = 1 shl 10 { available in 10_9, 7_0 };
  NSCalendarMatchFirst = 1 shl 12 { available in 10_9, 7_0 };
  NSCalendarMatchLast = 1 shl 13 { available in 10_9, 7_0 };
const
  NSWrapCalendarComponents = NSCalendarWrapComponents deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarWrapComponents instead"';
{$endif}

{$ifdef CLASSES}

type
  NSCalendar = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    class function currentCalendar: NSCalendar; message 'currentCalendar';
    class function autoupdatingCurrentCalendar: NSCalendar; message 'autoupdatingCurrentCalendar'; { available in 10_5, 2_0 }
    class function calendarWithIdentifier (calendarIdentifierConstant: NSString): NSCalendar; message 'calendarWithIdentifier:'; { available in 10_9, 8_0 }
    function initWithCalendarIdentifier (ident: NSString): id; message 'initWithCalendarIdentifier:'; { NS_DESIGNATED_INITIALIZER }
    function calendarIdentifier: NSString; message 'calendarIdentifier';
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setTimeZone(newValue: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setFirstWeekday(newValue: NSUInteger); message 'setFirstWeekday:';
    function firstWeekday: NSUInteger; message 'firstWeekday';
    procedure setMinimumDaysInFirstWeek(newValue: NSUInteger); message 'setMinimumDaysInFirstWeek:';
    function minimumDaysInFirstWeek: NSUInteger; message 'minimumDaysInFirstWeek';
    function eraSymbols: NSArray; message 'eraSymbols';
    function longEraSymbols: NSArray; message 'longEraSymbols';
    function monthSymbols: NSArray; message 'monthSymbols';
    function shortMonthSymbols: NSArray; message 'shortMonthSymbols';
    function veryShortMonthSymbols: NSArray; message 'veryShortMonthSymbols';
    function standaloneMonthSymbols: NSArray; message 'standaloneMonthSymbols';
    function shortStandaloneMonthSymbols: NSArray; message 'shortStandaloneMonthSymbols';
    function veryShortStandaloneMonthSymbols: NSArray; message 'veryShortStandaloneMonthSymbols';
    function weekdaySymbols: NSArray; message 'weekdaySymbols';
    function shortWeekdaySymbols: NSArray; message 'shortWeekdaySymbols';
    function veryShortWeekdaySymbols: NSArray; message 'veryShortWeekdaySymbols';
    function standaloneWeekdaySymbols: NSArray; message 'standaloneWeekdaySymbols';
    function shortStandaloneWeekdaySymbols: NSArray; message 'shortStandaloneWeekdaySymbols';
    function veryShortStandaloneWeekdaySymbols: NSArray; message 'veryShortStandaloneWeekdaySymbols';
    function quarterSymbols: NSArray; message 'quarterSymbols';
    function shortQuarterSymbols: NSArray; message 'shortQuarterSymbols';
    function standaloneQuarterSymbols: NSArray; message 'standaloneQuarterSymbols';
    function shortStandaloneQuarterSymbols: NSArray; message 'shortStandaloneQuarterSymbols';
    function AMSymbol: NSString; message 'AMSymbol';
    function PMSymbol: NSString; message 'PMSymbol';
    function minimumRangeOfUnit (unit_: NSCalendarUnit): NSRange; message 'minimumRangeOfUnit:';
    function maximumRangeOfUnit (unit_: NSCalendarUnit): NSRange; message 'maximumRangeOfUnit:';
    function rangeOfUnit_inUnit_forDate (smaller: NSCalendarUnit; larger: NSCalendarUnit; date: NSDate): NSRange; message 'rangeOfUnit:inUnit:forDate:';
    function ordinalityOfUnit_inUnit_forDate (smaller: NSCalendarUnit; larger: NSCalendarUnit; date: NSDate): NSUInteger; message 'ordinalityOfUnit:inUnit:forDate:';
    function rangeOfUnit_startDate_interval_forDate (unit_: NSCalendarUnit; datep: NSDatePtr; tip: NSTimeIntervalPtr; date: NSDate): ObjCBOOL; message 'rangeOfUnit:startDate:interval:forDate:'; { available in 10_5, 2_0 }
    function dateFromComponents (comps: NSDateComponents): NSDate; message 'dateFromComponents:';
    function components_fromDate (unitFlags: NSCalendarUnit; date: NSDate): NSDateComponents; message 'components:fromDate:';
    function dateByAddingComponents_toDate_options (comps: NSDateComponents; date: NSDate; opts: NSCalendarOptions): NSDate; message 'dateByAddingComponents:toDate:options:';
    function components_fromDate_toDate_options (unitFlags: NSCalendarUnit; startingDate: NSDate; resultDate: NSDate; opts: NSCalendarOptions): NSDateComponents; message 'components:fromDate:toDate:options:';
    procedure getEra_year_month_day_fromDate (eraValuePointer: NSIntegerPtr; yearValuePointer: NSIntegerPtr; monthValuePointer: NSIntegerPtr; dayValuePointer: NSIntegerPtr; date: NSDate); message 'getEra:year:month:day:fromDate:'; { available in 10_9, 8_0 }
    procedure getEra_yearForWeekOfYear_weekOfYear_weekday_fromDate (eraValuePointer: NSIntegerPtr; yearValuePointer: NSIntegerPtr; weekValuePointer: NSIntegerPtr; weekdayValuePointer: NSIntegerPtr; date: NSDate); message 'getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:'; { available in 10_9, 8_0 }
    procedure getHour_minute_second_nanosecond_fromDate (hourValuePointer: NSIntegerPtr; minuteValuePointer: NSIntegerPtr; secondValuePointer: NSIntegerPtr; nanosecondValuePointer: NSIntegerPtr; date: NSDate); message 'getHour:minute:second:nanosecond:fromDate:'; { available in 10_9, 8_0 }
    function component_fromDate (unit_: NSCalendarUnit; date: NSDate): NSInteger; message 'component:fromDate:'; { available in 10_9, 8_0 }
    function dateWithEra_year_month_day_hour_minute_second_nanosecond (eraValue: NSInteger; yearValue: NSInteger; monthValue: NSInteger; dayValue: NSInteger; hourValue: NSInteger; minuteValue: NSInteger; secondValue: NSInteger; nanosecondValue: NSInteger): NSDate; message 'dateWithEra:year:month:day:hour:minute:second:nanosecond:'; { available in 10_9, 8_0 }
    function dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecond (eraValue: NSInteger; yearValue: NSInteger; weekValue: NSInteger; weekdayValue: NSInteger; hourValue: NSInteger; minuteValue: NSInteger; secondValue: NSInteger; nanosecondValue: NSInteger): NSDate; message 'dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:'; { available in 10_9, 8_0 }
    function startOfDayForDate (date: NSDate): NSDate; message 'startOfDayForDate:'; { available in 10_9, 8_0 }
    function componentsInTimeZone_fromDate (timezone_: NSTimeZone; date: NSDate): NSDateComponents; message 'componentsInTimeZone:fromDate:'; { available in 10_9, 8_0 }
    function compareDate_toDate_toUnitGranularity (date1: NSDate; date2: NSDate; unit_: NSCalendarUnit): NSComparisonResult; message 'compareDate:toDate:toUnitGranularity:'; { available in 10_9, 8_0 }
    function isDate_equalToDate_toUnitGranularity (date1: NSDate; date2: NSDate; unit_: NSCalendarUnit): ObjCBOOL; message 'isDate:equalToDate:toUnitGranularity:'; { available in 10_9, 8_0 }
    function isDate_inSameDayAsDate (date1: NSDate; date2: NSDate): ObjCBOOL; message 'isDate:inSameDayAsDate:'; { available in 10_9, 8_0 }
    function isDateInToday (date: NSDate): ObjCBOOL; message 'isDateInToday:'; { available in 10_9, 8_0 }
    function isDateInYesterday (date: NSDate): ObjCBOOL; message 'isDateInYesterday:'; { available in 10_9, 8_0 }
    function isDateInTomorrow (date: NSDate): ObjCBOOL; message 'isDateInTomorrow:'; { available in 10_9, 8_0 }
    function isDateInWeekend (date: NSDate): ObjCBOOL; message 'isDateInWeekend:'; { available in 10_9, 8_0 }
    function rangeOfWeekendStartDate_interval_containingDate (datep: NSDatePtr; tip: NSTimeIntervalPtr; date: NSDate): ObjCBOOL; message 'rangeOfWeekendStartDate:interval:containingDate:'; { available in 10_9, 8_0 }
    function nextWeekendStartDate_interval_options_afterDate (datep: NSDatePtr; tip: NSTimeIntervalPtr; options: NSCalendarOptions; date: NSDate): ObjCBOOL; message 'nextWeekendStartDate:interval:options:afterDate:'; { available in 10_9, 8_0 }
    function components_fromDateComponents_toDateComponents_options (unitFlags: NSCalendarUnit; startingDateComp: NSDateComponents; resultDateComp: NSDateComponents; options: NSCalendarOptions): NSDateComponents; message 'components:fromDateComponents:toDateComponents:options:'; { available in 10_9, 8_0 }
    function dateByAddingUnit_value_toDate_options (unit_: NSCalendarUnit; value: NSInteger; date: NSDate; options: NSCalendarOptions): NSDate; message 'dateByAddingUnit:value:toDate:options:'; { available in 10_9, 8_0 }
    procedure enumerateDatesStartingAfterDate_matchingComponents_options_usingBlock (start: NSDate; comps: NSDateComponents; opts: NSCalendarOptions; block: OpaqueCBlock); message 'enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:'; { available in 10_9, 8_0 }
    function nextDateAfterDate_matchingComponents_options (date: NSDate; comps: NSDateComponents; options: NSCalendarOptions): NSDate; message 'nextDateAfterDate:matchingComponents:options:'; { available in 10_9, 8_0 }
    function nextDateAfterDate_matchingUnit_value_options (date: NSDate; unit_: NSCalendarUnit; value: NSInteger; options: NSCalendarOptions): NSDate; message 'nextDateAfterDate:matchingUnit:value:options:'; { available in 10_9, 8_0 }
    function nextDateAfterDate_matchingHour_minute_second_options (date: NSDate; hourValue: NSInteger; minuteValue: NSInteger; secondValue: NSInteger; options: NSCalendarOptions): NSDate; message 'nextDateAfterDate:matchingHour:minute:second:options:'; { available in 10_9, 8_0 }
    function dateBySettingUnit_value_ofDate_options (unit_: NSCalendarUnit; v: NSInteger; date: NSDate; opts: NSCalendarOptions): NSDate; message 'dateBySettingUnit:value:ofDate:options:'; { available in 10_9, 8_0 }
    function dateBySettingHour_minute_second_ofDate_options (h: NSInteger; m: NSInteger; s: NSInteger; date: NSDate; opts: NSCalendarOptions): NSDate; message 'dateBySettingHour:minute:second:ofDate:options:'; { available in 10_9, 8_0 }
    function date_matchesComponents (date: NSDate; components: NSDateComponents): ObjCBOOL; message 'date:matchesComponents:'; { available in 10_9, 8_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSCalendarDayChangedNotification: NSString { available in 10_9, 8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSDateComponentUndefined = NSIntegerMax;
  NSUndefinedDateComponent = NSDateComponentUndefined deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSDateComponentUndefined instead"';
{$endif}

{$ifdef CLASSES}

type
  NSDateComponents = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    procedure setCalendar(newValue: NSCalendar); message 'setCalendar:';
    function calendar: NSCalendar; message 'calendar';
    procedure setTimeZone(newValue: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setEra(newValue: NSInteger); message 'setEra:';
    function era: NSInteger; message 'era';
    procedure setYear(newValue: NSInteger); message 'setYear:';
    function year: NSInteger; message 'year';
    procedure setMonth(newValue: NSInteger); message 'setMonth:';
    function month: NSInteger; message 'month';
    procedure setDay(newValue: NSInteger); message 'setDay:';
    function day: NSInteger; message 'day';
    procedure setHour(newValue: NSInteger); message 'setHour:';
    function hour: NSInteger; message 'hour';
    procedure setMinute(newValue: NSInteger); message 'setMinute:';
    function minute: NSInteger; message 'minute';
    procedure setSecond(newValue: NSInteger); message 'setSecond:';
    function second: NSInteger; message 'second';
    procedure setNanosecond(newValue: NSInteger); message 'setNanosecond:';
    function nanosecond: NSInteger; message 'nanosecond';
    procedure setWeekday(newValue: NSInteger); message 'setWeekday:';
    function weekday: NSInteger; message 'weekday';
    procedure setWeekdayOrdinal(newValue: NSInteger); message 'setWeekdayOrdinal:';
    function weekdayOrdinal: NSInteger; message 'weekdayOrdinal';
    procedure setQuarter(newValue: NSInteger); message 'setQuarter:';
    function quarter: NSInteger; message 'quarter';
    procedure setWeekOfMonth(newValue: NSInteger); message 'setWeekOfMonth:';
    function weekOfMonth: NSInteger; message 'weekOfMonth';
    procedure setWeekOfYear(newValue: NSInteger); message 'setWeekOfYear:';
    function weekOfYear: NSInteger; message 'weekOfYear';
    procedure setYearForWeekOfYear(newValue: NSInteger); message 'setYearForWeekOfYear:';
    function yearForWeekOfYear: NSInteger; message 'yearForWeekOfYear';
    procedure setLeapMonth(newValue: ObjCBOOL); message 'setLeapMonth:';
    function isLeapMonth: ObjCBOOL; message 'isLeapMonth';
    function date: NSDate; message 'date';
    function week: NSInteger; message 'week'; deprecated 'in 10_4, 10_9, 2_0, 7_0, "Use weekOfMonth or weekOfYear, depending on which you mean"';
    procedure setWeek (v: NSInteger); message 'setWeek:'; deprecated 'in 10_4, 10_9, 2_0, 7_0, "Use setWeekOfMonth: or setWeekOfYear:, depending on which you mean"';
    procedure setValue_forComponent (value: NSInteger; unit_: NSCalendarUnit); message 'setValue:forComponent:'; { available in 10_9, 8_0 }
    function valueForComponent (unit_: NSCalendarUnit): NSInteger; message 'valueForComponent:'; { available in 10_9, 8_0 }
    function isValidDate: ObjCBOOL; message 'isValidDate';
    function isValidDateInCalendar (calendar_: NSCalendar): ObjCBOOL; message 'isValidDateInCalendar:'; { available in 10_9, 8_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

