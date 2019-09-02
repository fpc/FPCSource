{ Parsed from CalendarStore.framework CalCalendarItem.h }


{$ifdef TYPES}
type
  CalCalendarItemPtr = ^CalCalendarItem;
{$endif}

{$ifdef CLASSES}

type
  CalCalendarItem = objcclass external (NSObject, NSCopyingProtocol)
  private
    _objectID: id;
    _reserved: pointer;
    _calendar: CalCalendar;
    _notes: NSString;
    _url: NSURL;
    _title: NSString;
    _uid: NSString;
    _dateStamp: NSDate;
    _alarms: NSArray;
  public
    function hasAlarm: ObjCBOOL; message 'hasAlarm'; deprecated 'in 10_5, 10_8';
    function nextAlarmDate: NSDate; message 'nextAlarmDate'; deprecated 'in 10_5, 10_8';
    procedure setCalendar(newValue: CalCalendar); message 'setCalendar:';
    function calendar: CalCalendar; message 'calendar';
    procedure setNotes(newValue: NSString); message 'setNotes:';
    function notes: NSString; message 'notes';
    procedure setUrl(newValue: NSURL); message 'setUrl:';
    function url: NSURL; message 'url';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function uid: NSString; message 'uid';
    function dateStamp: NSDate; message 'dateStamp';
    procedure setAlarms(newValue: NSArray); message 'setAlarms:';
    function alarms: NSArray; message 'alarms';
    procedure addAlarm (alarm: CalAlarm); message 'addAlarm:'; deprecated 'in 10_5, 10_8';
    procedure addAlarms (alarms_: NSArray); message 'addAlarms:'; deprecated 'in 10_5, 10_8';
    procedure removeAlarm (alarm: CalAlarm); message 'removeAlarm:'; deprecated 'in 10_5, 10_8';
    procedure removeAlarms (alarms_: NSArray); message 'removeAlarms:'; deprecated 'in 10_5, 10_8';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

