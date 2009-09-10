{ Parsed from Foundation.framework NSNotificationQueue.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSNOTIFICATIONQUEUE_PAS_H}
{$define NSNOTIFICATIONQUEUE_PAS_H}
type
  NSNotificationQueuePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSNOTIFICATIONQUEUE_PAS_T}
{$define NSNOTIFICATIONQUEUE_PAS_T}

{ Constants }

const
  NSPostWhenIdle = 1;
  NSPostASAP = 2;
  NSPostNow = 3;

const
  NSNotificationNoCoalescing = 0;
  NSNotificationCoalescingOnName = 1;
  NSNotificationCoalescingOnSender = 2;

{ Types }
type
  NSPostingStyle = culong;
  NSNotificationCoalescing = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSNOTIFICATIONQUEUE_PAS_R}
{$define NSNOTIFICATIONQUEUE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSNOTIFICATIONQUEUE_PAS_F}
{$define NSNOTIFICATIONQUEUE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSNOTIFICATIONQUEUE_PAS_T}
{$define NSNOTIFICATIONQUEUE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNotificationQueue = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNOTIFICATIONQUEUE_PAS_C}
{$define NSNOTIFICATIONQUEUE_PAS_C}

{ NSNotificationQueue }
  NSNotificationQueue = objcclass(NSObject)
  private
    __notificationCenter: id;
    __asapQueue: id;
    __asapObs: id;
    __idleQueue: id;
    __idleObs: id;
    
  public
    class function alloc: NSNotificationQueue; message 'alloc';

    class function defaultQueue: id; message 'defaultQueue';
    function initWithNotificationCenter(notificationCenter: NSNotificationCenter): id; message 'initWithNotificationCenter:';
    procedure enqueueNotification_postingStyle(notification: NSNotification; postingStyle: NSPostingStyle); message 'enqueueNotification:postingStyle:';
    procedure enqueueNotification_postingStyle_coalesceMask_forModes(notification: NSNotification; postingStyle: NSPostingStyle; coalesceMask: culong; modes: NSArray); message 'enqueueNotification:postingStyle:coalesceMask:forModes:';
    procedure dequeueNotificationsMatching_coalesceMask(notification: NSNotification; coalesceMask: culong); message 'dequeueNotificationsMatching:coalesceMask:';
  end; external;

{$endif}
{$endif}
