{ Parsed from EventKit.framework EKCalendar.h }


{$ifdef TYPES}
type
  EKCalendarPtr = ^EKCalendar;
{$endif}

{$ifdef CLASSES}

type
  EKCalendar = objcclass external (EKObject)
  public
    class function calendarForEntityType_eventStore (entityType: EKEntityType; eventStore: EKEventStore): EKCalendar; message 'calendarForEntityType:eventStore:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    procedure setSource(newValue: EKSource); message 'setSource:';
    function source: EKSource; message 'source';
    function calendarIdentifier: NSString; message 'calendarIdentifier';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function type_: EKCalendarType; message 'type';
    function allowsContentModifications: ObjCBOOL; message 'allowsContentModifications';
    function isSubscribed: ObjCBOOL; message 'isSubscribed';
    function isImmutable: ObjCBOOL; message 'isImmutable';
    procedure setColor(newValue: NSColor); message 'setColor:';
    function color: NSColor; message 'color';
    function supportedEventAvailabilities: EKCalendarEventAvailabilityMask; message 'supportedEventAvailabilities';
    function allowedEntityTypes: EKEntityMask; message 'allowedEntityTypes';
  end;
{$endif}

