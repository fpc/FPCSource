{ Parsed from EventKit.framework EKCalendarItem.h }


{$ifdef TYPES}
type
  EKCalendarItemPtr = ^EKCalendarItem;
{$endif}

{$ifdef CLASSES}

type
  EKCalendarItem = objcclass external (EKObject)
  private
  public
    procedure setCalendar(newValue: EKCalendar); message 'setCalendar:';
    function calendar: EKCalendar; message 'calendar';
    function calendarItemIdentifier: NSString; message 'calendarItemIdentifier';
    function calendarItemExternalIdentifier: NSString; message 'calendarItemExternalIdentifier';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setLocation(newValue: NSString); message 'setLocation:';
    function location: NSString; message 'location';
    procedure setNotes(newValue: NSString); message 'setNotes:';
    function notes: NSString; message 'notes';
    procedure setURL(newValue: NSURL); message 'setURL:';
    function URL: NSURL; message 'URL';
    function lastModifiedDate: NSDate; message 'lastModifiedDate';
    function creationDate: NSDate; message 'creationDate';
    procedure setTimeZone(newValue: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    function hasAlarms: ObjCBOOL; message 'hasAlarms';
    function hasRecurrenceRules: ObjCBOOL; message 'hasRecurrenceRules';
    function hasAttendees: ObjCBOOL; message 'hasAttendees';
    function hasNotes: ObjCBOOL; message 'hasNotes';
    function attendees: NSArray; message 'attendees';
    procedure setAlarms(newValue: NSArray); message 'setAlarms:';
    function alarms: NSArray; message 'alarms';
    procedure addAlarm (alarm: EKAlarm); message 'addAlarm:';
    procedure removeAlarm (alarm: EKAlarm); message 'removeAlarm:';
    procedure setRecurrenceRules(newValue: NSArray); message 'setRecurrenceRules:';
    function recurrenceRules: NSArray; message 'recurrenceRules';
    procedure addRecurrenceRule (rule: EKRecurrenceRule); message 'addRecurrenceRule:';
    procedure removeRecurrenceRule (rule: EKRecurrenceRule); message 'removeRecurrenceRule:';
  end;
{$endif}

