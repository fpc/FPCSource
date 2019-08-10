{ Parsed from Foundation.framework NSNotificationQueue.h }


{$ifdef TYPES}
type
  NSNotificationQueuePtr = ^NSNotificationQueue;
{$endif}

{$ifdef TYPES}
type
  NSPostingStyle = NSUInteger;
  NSPostingStylePtr = ^NSPostingStyle;

const
  NSPostWhenIdle = 1;
  NSPostASAP = 2;
  NSPostNow = 3;

type
  NSNotificationCoalescing = NSUInteger;
  NSNotificationCoalescingPtr = ^NSNotificationCoalescing;

const
  NSNotificationNoCoalescing = 0;
  NSNotificationCoalescingOnName = 1;
  NSNotificationCoalescingOnSender = 2;
{$endif}

{$ifdef CLASSES}

type
  NSNotificationQueue = objcclass external (NSObject)
  private
    _notificationCenter: id;
    _asapQueue: id;
    _asapObs: id;
    _idleQueue: id;
    _idleObs: id;
  public
    class function defaultQueue: NSNotificationQueue; message 'defaultQueue';
    function initWithNotificationCenter (notificationCenter: NSNotificationCenter): instancetype; message 'initWithNotificationCenter:'; { NS_DESIGNATED_INITIALIZER }
    procedure enqueueNotification_postingStyle (notification: NSNotification; postingStyle: NSPostingStyle); message 'enqueueNotification:postingStyle:';
    procedure enqueueNotification_postingStyle_coalesceMask_forModes (notification: NSNotification; postingStyle: NSPostingStyle; coalesceMask: NSUInteger; modes: NSArray); message 'enqueueNotification:postingStyle:coalesceMask:forModes:';
    procedure dequeueNotificationsMatching_coalesceMask (notification: NSNotification; coalesceMask: NSUInteger); message 'dequeueNotificationsMatching:coalesceMask:';
  end;
{$endif}

