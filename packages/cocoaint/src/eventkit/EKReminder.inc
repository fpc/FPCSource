{ Parsed from EventKit.framework EKReminder.h }


{$ifdef TYPES}
type
  EKReminderPtr = ^EKReminder;
{$endif}

{$ifdef CLASSES}

type
  EKReminder = objcclass external (EKCalendarItem)
  private
  public
    class function reminderWithEventStore (eventStore: EKEventStore): EKReminder; message 'reminderWithEventStore:';
    procedure setStartDateComponents(newValue: NSDateComponents); message 'setStartDateComponents:';
    function startDateComponents: NSDateComponents; message 'startDateComponents';
    procedure setDueDateComponents(newValue: NSDateComponents); message 'setDueDateComponents:';
    function dueDateComponents: NSDateComponents; message 'dueDateComponents';
    procedure setCompleted(newValue: ObjCBOOL); message 'setCompleted:';
    function isCompleted: ObjCBOOL; message 'isCompleted';
    procedure setCompletionDate(newValue: NSDate); message 'setCompletionDate:';
    function completionDate: NSDate; message 'completionDate';
    procedure setPriority(newValue: NSUInteger); message 'setPriority:';
    function priority: NSUInteger; message 'priority';
  end;
{$endif}

