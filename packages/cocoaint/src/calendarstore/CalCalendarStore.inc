{ Parsed from CalendarStore.framework CalCalendarStore.h }


{$ifdef TYPES}
type
  CalCalendarStorePtr = ^CalCalendarStore;
{$endif}

{$ifdef TYPES}
const
  CalSpanThisEvent = 0;
  CalSpanFutureEvents = 1;
  CalSpanAllEvents = 2;
type
  CalSpan = clong;
{$endif}

{$ifdef CLASSES}

type
  CalCalendarStore = objcclass external (NSObject)
  private
    _reserved: pointer;
  public
    class function defaultCalendarStore: CalCalendarStore; message 'defaultCalendarStore'; deprecated 'in 10_5, 10_8';
    function calendars: NSArray; message 'calendars'; deprecated 'in 10_5, 10_8';
    function calendarWithUID (UID: NSString): CalCalendar; message 'calendarWithUID:'; deprecated 'in 10_5, 10_8';
    function saveCalendar_error (calendar: CalCalendar; error: NSErrorPtr): ObjCBOOL; message 'saveCalendar:error:'; deprecated 'in 10_5, 10_8';
    function removeCalendar_error (calendar: CalCalendar; error: NSErrorPtr): ObjCBOOL; message 'removeCalendar:error:'; deprecated 'in 10_5, 10_8';
    function eventsWithPredicate (predicate: NSPredicate): NSArray; message 'eventsWithPredicate:'; deprecated 'in 10_5, 10_8';
    function eventWithUID_occurrence (uid: NSString; date: NSDate): CalEvent; message 'eventWithUID:occurrence:'; deprecated 'in 10_5, 10_8';
    function tasksWithPredicate (predicate: NSPredicate): NSArray; message 'tasksWithPredicate:'; deprecated 'in 10_5, 10_8';
    function taskWithUID (uid: NSString): CalTask; message 'taskWithUID:'; deprecated 'in 10_5, 10_8';
    function saveEvent_span_error (event: CalEvent; span: CalSpan; error: NSErrorPtr): ObjCBOOL; message 'saveEvent:span:error:'; deprecated 'in 10_5, 10_8';
    function removeEvent_span_error (event: CalEvent; span: CalSpan; error: NSErrorPtr): ObjCBOOL; message 'removeEvent:span:error:'; deprecated 'in 10_5, 10_8';
    function saveTask_error (task: CalTask; error: NSErrorPtr): ObjCBOOL; message 'saveTask:error:'; deprecated 'in 10_5, 10_8';
    function removeTask_error (task: CalTask; error: NSErrorPtr): ObjCBOOL; message 'removeTask:error:'; deprecated 'in 10_5, 10_8';
    class function eventPredicateWithStartDate_endDate_calendars (startDate: NSDate; endDate: NSDate; calendars_: NSArray): NSPredicate; message 'eventPredicateWithStartDate:endDate:calendars:'; deprecated 'in 10_5, 10_8';
    class function eventPredicateWithStartDate_endDate_UID_calendars (startDate: NSDate; endDate: NSDate; UID: NSString; calendars_: NSArray): NSPredicate; message 'eventPredicateWithStartDate:endDate:UID:calendars:'; deprecated 'in 10_5, 10_8';
    class function taskPredicateWithCalendars (calendars_: NSArray): NSPredicate; message 'taskPredicateWithCalendars:'; deprecated 'in 10_5, 10_8';
    class function taskPredicateWithUncompletedTasks (calendars_: NSArray): NSPredicate; message 'taskPredicateWithUncompletedTasks:'; deprecated 'in 10_5, 10_8';
    class function taskPredicateWithUncompletedTasksDueBefore_calendars (dueDate: NSDate; calendars_: NSArray): NSPredicate; message 'taskPredicateWithUncompletedTasksDueBefore:calendars:'; deprecated 'in 10_5, 10_8';
    class function taskPredicateWithTasksCompletedSince_calendars (completedSince: NSDate; calendars_: NSArray): NSPredicate; message 'taskPredicateWithTasksCompletedSince:calendars:'; deprecated 'in 10_5, 10_8';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CalCalendarsChangedNotification: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalEventsChangedNotification: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalTasksChangedNotification: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalCalendarsChangedExternallyNotification: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalEventsChangedExternallyNotification: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalTasksChangedExternallyNotification: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalInsertedRecordsKey: NSString; cvar; external;
  CalUpdatedRecordsKey: NSString; cvar; external;
  CalDeletedRecordsKey: NSString; cvar; external;
  CalSenderProcessIDKey: NSString deprecated 'in 10_5, 10_8'; cvar; external;
  CalUserUIDKey: NSString deprecated 'in 10_5, 10_8'; cvar; external;
{$endif}

