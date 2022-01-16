{ Parsed from CalendarStore.framework CalRecurrenceRule.h }


{$ifdef TYPES}
type
  CalRecurrenceEndPtr = ^CalRecurrenceEnd;
  CalNthWeekDayPtr = ^CalNthWeekDay;
  CalRecurrenceRulePtr = ^CalRecurrenceRule;
{$endif}

{$ifdef CLASSES}

type
  CalRecurrenceEnd = objcclass external (NSObject, NSCopyingProtocol)
  private
    _endDate: NSDate;
    _occurrenceCount: NSUInteger;
  public
    class function recurrenceEndWithEndDate (endDate: NSDate): id; message 'recurrenceEndWithEndDate:'; deprecated 'in 10_5, 10_8';
    class function recurrenceEndWithOccurrenceCount (occurrenceCount: NSUInteger): id; message 'recurrenceEndWithOccurrenceCount:'; deprecated 'in 10_5, 10_8';
    function usesEndDate: ObjCBOOL; message 'usesEndDate';
    function endDate: NSDate; message 'endDate';
    function occurrenceCount: NSUInteger; message 'occurrenceCount';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;

type
  CalNthWeekDay = objcclass external (NSObject, NSCopyingProtocol)
  private
    _dayOfTheWeek: NSUInteger;
    _weekNumber: NSInteger;
  public
    function dayOfTheWeek: NSUInteger; message 'dayOfTheWeek';
    function weekNumber: NSInteger; message 'weekNumber';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef TYPES}
const
  CalRecurrenceDaily = 0;
  CalRecurrenceWeekly = 1;
  CalRecurrenceMonthly = 2;
  CalRecurrenceYearly = 3;
type
  CalRecurrenceType = clong;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CalDefaultRecurrenceInterval: NSUInteger deprecated 'in 10_5, 10_8'; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CalRecurrenceRule = objcclass external (NSObject, NSCopyingProtocol)
  private
    _objectID: id;
    _reserved: pointer;
    _monthsOfTheYear: NSArray;
    _nthWeekDaysOfTheMonth: NSArray;
    _daysOfTheMonth: NSArray;
    _daysOfTheWeek: NSArray;
    _firstDayOfTheWeek: NSUInteger;
    _recurrenceInterval: NSUInteger;
    _recurrenceType: CalRecurrenceType;
    _recurrenceEnd: CalRecurrenceEnd;
  public
    function initDailyRecurrenceWithInterval_end (interval: NSUInteger; end_: CalRecurrenceEnd): id; message 'initDailyRecurrenceWithInterval:end:'; deprecated 'in 10_5, 10_8';
    function initWeeklyRecurrenceWithInterval_end (interval: NSUInteger; end_: CalRecurrenceEnd): id; message 'initWeeklyRecurrenceWithInterval:end:'; deprecated 'in 10_5, 10_8';
    function initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_end (interval: NSUInteger; days: NSArray; end_: CalRecurrenceEnd): id; message 'initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:'; deprecated 'in 10_5, 10_8';
    function initMonthlyRecurrenceWithInterval_end (interval: NSUInteger; end_: CalRecurrenceEnd): id; message 'initMonthlyRecurrenceWithInterval:end:'; deprecated 'in 10_5, 10_8';
    function initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_end (interval: NSUInteger; monthDays: NSArray; end_: CalRecurrenceEnd): id; message 'initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:'; deprecated 'in 10_5, 10_8';
    function initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_end (interval: NSUInteger; weekDay: NSUInteger; monthWeek: NSInteger; end_: CalRecurrenceEnd): id; message 'initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:'; deprecated 'in 10_5, 10_8';
    function initYearlyRecurrenceWithInterval_end (interval: NSUInteger; end_: CalRecurrenceEnd): id; message 'initYearlyRecurrenceWithInterval:end:'; deprecated 'in 10_5, 10_8';
    function initYearlyRecurrenceWithInterval_forMonthsOfTheYear_end (interval: NSUInteger; months: NSArray; end_: CalRecurrenceEnd): id; message 'initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:'; deprecated 'in 10_5, 10_8';
    function initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_end (interval: NSUInteger; weekDay: NSUInteger; monthWeek: NSInteger; months: NSArray; end_: CalRecurrenceEnd): id; message 'initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:'; deprecated 'in 10_5, 10_8';
    function recurrenceEnd: CalRecurrenceEnd; message 'recurrenceEnd';
    function recurrenceType: CalRecurrenceType; message 'recurrenceType';
    function recurrenceInterval: NSUInteger; message 'recurrenceInterval';
    function firstDayOfTheWeek: NSUInteger; message 'firstDayOfTheWeek';
    function daysOfTheWeek: NSArray; message 'daysOfTheWeek';
    function daysOfTheMonth: NSArray; message 'daysOfTheMonth';
    function nthWeekDaysOfTheMonth: NSArray; message 'nthWeekDaysOfTheMonth';
    function monthsOfTheYear: NSArray; message 'monthsOfTheYear';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

