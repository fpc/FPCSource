{ Parsed from CalendarStore.framework CalCalendar.h }


{$ifdef TYPES}
type
  CalCalendarPtr = ^CalCalendar;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CalCalendarTypeBirthday: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalCalendarTypeCalDAV: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalCalendarTypeLocal: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalCalendarTypeSubscription: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalCalendarTypeIMAP: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalCalendarTypeExchange: NSString deprecated 'in 10_6, 10_8'; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CalCalendar = objcclass external (NSObject, NSCopyingProtocol)
  private
    _objectID: id;
    _group: id;
    _reserved: pointer;
    _color: NSColor;
    _notes: NSString;
    _title: NSString;
    _type: NSString;
    _uid: NSString;
    _isEditable: ObjCBOOL;
  public
    class function calendar: id; message 'calendar'; deprecated 'in 10_5, 10_8';
    procedure setColor(newValue: NSColor); message 'setColor:';
    function color: NSColor; message 'color';
    procedure setNotes(newValue: NSString); message 'setNotes:';
    function notes: NSString; message 'notes';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function type_: NSString; message 'type';
    function uid: NSString; message 'uid';
    function isEditable: ObjCBOOL; message 'isEditable';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

