{ Parsed from EventKit.framework EKEvent.h }


{$ifdef TYPES}
type
  EKEventPtr = ^EKEvent;
{$endif}

{$ifdef TYPES}
const
  EKEventAvailabilityNotSupported = -1;
  EKEventAvailabilityBusy = 0;
  EKEventAvailabilityFree = 1;
  EKEventAvailabilityTentative = 2;
  EKEventAvailabilityUnavailable = 3;
type
  EKEventAvailability = clong;
const
  EKEventStatusNone = 0;
  EKEventStatusConfirmed = 1;
  EKEventStatusTentative = 2;
  EKEventStatusCanceled = 3;
type
  EKEventStatus = clong;
{$endif}

{$ifdef CLASSES}

type
  EKEvent = objcclass external (EKCalendarItem)
  private
  public
    class function eventWithEventStore (eventStore: EKEventStore): EKEvent; message 'eventWithEventStore:';
    function eventIdentifier: NSString; message 'eventIdentifier';
    procedure setAllDay(newValue: ObjCBOOL); message 'setAllDay:';
    function isAllDay: ObjCBOOL; message 'isAllDay';
    procedure setStartDate(newValue: NSDate); message 'setStartDate:';
    function startDate: NSDate; message 'startDate';
    procedure setEndDate(newValue: NSDate); message 'setEndDate:';
    function endDate: NSDate; message 'endDate';
    function compareStartDateWithEvent (other: EKEvent): NSComparisonResult; message 'compareStartDateWithEvent:';
    function organizer: EKParticipant; message 'organizer';
    procedure setAvailability(newValue: EKEventAvailability); message 'setAvailability:';
    function availability: EKEventAvailability; message 'availability';
    function status: EKEventStatus; message 'status';
    function isDetached: ObjCBOOL; message 'isDetached';
    function occurrenceDate: NSDate; message 'occurrenceDate';
    function refresh: ObjCBOOL; message 'refresh';
    function birthdayPersonUniqueID: NSString; message 'birthdayPersonUniqueID';
  end;
{$endif}

