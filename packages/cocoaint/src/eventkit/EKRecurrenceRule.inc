{ Parsed from EventKit.framework EKRecurrenceRule.h }


{$ifdef TYPES}
type
  EKRecurrenceRulePtr = ^EKRecurrenceRule;
{$endif}

{$ifdef CLASSES}

type
  EKRecurrenceRule = objcclass external (EKObject, NSCopyingProtocol)
  private
  public
    function initRecurrenceWithFrequency_interval_end (type_: EKRecurrenceFrequency; interval: NSInteger; end_: EKRecurrenceEnd): id; message 'initRecurrenceWithFrequency:interval:end:';
    function initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_end (type_: EKRecurrenceFrequency; interval: NSInteger; days: NSArray; monthDays: NSArray; months: NSArray; weeksOfTheYear: NSArray; daysOfTheYear: NSArray; setPositions: NSArray; end_: EKRecurrenceEnd): id; message 'initRecurrenceWithFrequency:interval:daysOfTheWeek:daysOfTheMonth:monthsOfTheYear:weeksOfTheYear:daysOfTheYear:setPositions:end:';
    function calendarIdentifier: NSString; message 'calendarIdentifier';
    procedure setRecurrenceEnd(newValue: EKRecurrenceEnd); message 'setRecurrenceEnd:';
    function recurrenceEnd: EKRecurrenceEnd; message 'recurrenceEnd';
    function frequency: EKRecurrenceFrequency; message 'frequency';
    function interval: NSInteger; message 'interval';
    function firstDayOfTheWeek: NSInteger; message 'firstDayOfTheWeek';
    function daysOfTheWeek: NSArray; message 'daysOfTheWeek';
    function daysOfTheMonth: NSArray; message 'daysOfTheMonth';
    function daysOfTheYear: NSArray; message 'daysOfTheYear';
    function weeksOfTheYear: NSArray; message 'weeksOfTheYear';
    function monthsOfTheYear: NSArray; message 'monthsOfTheYear';
    function setPositions: NSArray; message 'setPositions';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

