{ Parsed from CalendarStore.framework CalTask.h }


{$ifdef TYPES}
type
  CalTaskPtr = ^CalTask;
{$endif}

{$ifdef TYPES}
const
  CalPriorityNone = 0;
  CalPriorityHigh = 1;
  CalPriorityMedium = 5;
  CalPriorityLow = 9;

type
  CalPriority = NSUInteger;
  CalPriorityPtr = ^CalPriority;
{$endif}

{$ifdef CLASSES}

type
  CalTask = objcclass external (CalCalendarItem)
  private
    _dueDate: NSDate;
    _priority: CalPriority;
    _completedDate: NSDate;
  public
    class function task: id; message 'task';
    procedure setDueDate(newValue: NSDate); message 'setDueDate:';
    function dueDate: NSDate; message 'dueDate';
    procedure setPriority(newValue: CalPriority); message 'setPriority:';
    function priority: CalPriority; message 'priority';
    procedure setIsCompleted(newValue: ObjCBOOL); message 'setIsCompleted:';
    function isCompleted: ObjCBOOL; message 'isCompleted';
    procedure setCompletedDate(newValue: NSDate); message 'setCompletedDate:';
    function completedDate: NSDate; message 'completedDate';
  end;
{$endif}

