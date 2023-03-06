{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesEventKit;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  EKAlarm = objcclass external;
  EKCalendar = objcclass external;
  EKCalendarItem = objcclass external;
  EKEvent = objcclass external;
  EKEventStore = objcclass external;
  EKObject = objcclass external;
  EKParticipant = objcclass external;
  EKRecurrenceDayOfWeek = objcclass external;
  EKRecurrenceEnd = objcclass external;
  EKRecurrenceRule = objcclass external;
  EKReminder = objcclass external;
  EKSource = objcclass external;
  EKStructuredLocation = objcclass external;

type
  ABAddressBook = objcclass external;
  ABPerson = objcclass external;
  NSColor = objcclass external;

implementation
end.
