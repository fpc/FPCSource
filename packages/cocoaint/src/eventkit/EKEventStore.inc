{ Parsed from EventKit.framework EKEventStore.h }


{ Types from EKEventStore }
{$ifdef TYPES}

type
  EKEventStoreRequestAccessCompletionHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  EKEventStorePtr = ^EKEventStore;
{$endif}

{$ifdef TYPES}
const
  EKSpanThisEvent = 0;
  EKSpanFutureEvents = 1;
type
  EKSpan = clong;

type
  EKAuthorizationStatus = NSInteger;
  EKAuthorizationStatusPtr = ^EKAuthorizationStatus;

const
  EKAuthorizationStatusNotDetermined = 0;
  EKAuthorizationStatusRestricted = 1;
  EKAuthorizationStatusDenied = 2;
  EKAuthorizationStatusAuthorized = 3;

type
  EKEventSearchCallback = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  EKEventStore = objcclass external (NSObject)
  public
    class function authorizationStatusForEntityType (entityType: EKEntityType): EKAuthorizationStatus; message 'authorizationStatusForEntityType:'; { available starting in __MAC_10_9,__IPHONE_6_0 }
    function initWithAccessToEntityTypes (entityTypes: EKEntityMask): id; message 'initWithAccessToEntityTypes:'; deprecated 'in __MAC_10_8,__MAC_10_9,__IPHONE_NA,__IPHONE_NA';
    function init: id; message 'init'; { available starting in __MAC_10_9,__IPHONE_4_0 }
    procedure requestAccessToEntityType_completion (entityType: EKEntityType; completion: EKEventStoreRequestAccessCompletionHandler); message 'requestAccessToEntityType:completion:'; { available starting in __MAC_10_9,__IPHONE_6_0 }
    function eventStoreIdentifier: NSString; message 'eventStoreIdentifier';
    function sources: NSArray; message 'sources'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function sourceWithIdentifier (identifier: NSString): EKSource; message 'sourceWithIdentifier:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function calendarsForEntityType (entityType: EKEntityType): NSArray; message 'calendarsForEntityType:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function defaultCalendarForNewEvents: EKCalendar; message 'defaultCalendarForNewEvents';
    function defaultCalendarForNewReminders: EKCalendar; message 'defaultCalendarForNewReminders'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function calendarWithIdentifier (identifier: NSString): EKCalendar; message 'calendarWithIdentifier:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function saveCalendar_commit_error (calendar: EKCalendar; commit: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'saveCalendar:commit:error:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function removeCalendar_commit_error (calendar: EKCalendar; commit: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'removeCalendar:commit:error:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function calendarItemWithIdentifier (identifier: NSString): EKCalendarItem; message 'calendarItemWithIdentifier:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function calendarItemsWithExternalIdentifier (externalIdentifier: NSString): NSArray; message 'calendarItemsWithExternalIdentifier:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function saveEvent_span_commit_error (event: EKEvent; span: EKSpan; commit: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'saveEvent:span:commit:error:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function removeEvent_span_commit_error (event: EKEvent; span: EKSpan; commit: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'removeEvent:span:commit:error:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    function eventWithIdentifier (identifier: NSString): EKEvent; message 'eventWithIdentifier:';
    function eventsMatchingPredicate (predicate: NSPredicate): NSArray; message 'eventsMatchingPredicate:';
    procedure enumerateEventsMatchingPredicate_usingBlock (predicate: NSPredicate; block: EKEventSearchCallback); message 'enumerateEventsMatchingPredicate:usingBlock:';
    function predicateForEventsWithStartDate_endDate_calendars (startDate: NSDate; endDate: NSDate; calendars: NSArray): NSPredicate; message 'predicateForEventsWithStartDate:endDate:calendars:';
    function saveReminder_commit_error (reminder: EKReminder; commit: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'saveReminder:commit:error:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function removeReminder_commit_error (reminder: EKReminder; commit: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'removeReminder:commit:error:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function fetchRemindersMatchingPredicate_completion (predicate: NSPredicate; completion: OpaqueCBlock): id; message 'fetchRemindersMatchingPredicate:completion:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    procedure cancelFetchRequest (fetchIdentifier: id); message 'cancelFetchRequest:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function predicateForRemindersInCalendars (calendars: NSArray): NSPredicate; message 'predicateForRemindersInCalendars:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function predicateForIncompleteRemindersWithDueDateStarting_ending_calendars (startDate: NSDate; endDate: NSDate; calendars: NSArray): NSPredicate; message 'predicateForIncompleteRemindersWithDueDateStarting:ending:calendars:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function predicateForCompletedRemindersWithCompletionDateStarting_ending_calendars (startDate: NSDate; endDate: NSDate; calendars: NSArray): NSPredicate; message 'predicateForCompletedRemindersWithCompletionDateStarting:ending:calendars:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
    function commit (error: NSErrorPtr): ObjCBOOL; message 'commit:'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    procedure reset; message 'reset'; { available starting in __MAC_10_8,__IPHONE_5_0 }
    procedure refreshSourcesIfNecessary; message 'refreshSourcesIfNecessary'; { available starting in __MAC_10_8,__IPHONE_5_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  EKEventStoreChangedNotification: NSString { available starting in __MAC_10_8,__IPHONE_4_0 }; cvar; external;
{$endif}

