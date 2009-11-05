{ Parsed from Foundation.framework NSThread.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTHREAD_PAS_T}
{$define NSTHREAD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTHREAD_PAS_R}
{$define NSTHREAD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTHREAD_PAS_F}
{$define NSTHREAD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTHREAD_PAS_S}
{$define NSTHREAD_PAS_S}

{ External string constants }
var
  NSWillBecomeMultiThreadedNotification: NSString; external name '_NSWillBecomeMultiThreadedNotification';
  NSDidBecomeSingleThreadedNotification: NSString; external name '_NSDidBecomeSingleThreadedNotification';
  NSThreadWillExitNotification: NSString; external name '_NSThreadWillExitNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSThread = objcclass;
  NSThreadPointer = ^NSThread;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTHREAD_PAS_C}
{$define NSTHREAD_PAS_C}

{ NSThread }
  NSThread = objcclass(NSObject)
  private
    __private: id;
        _bytes: array[0..43] of byte;
    
  public
    class function alloc: NSThread; message 'alloc';

    class function currentThread: NSThread; message 'currentThread';
    class procedure detachNewThreadSelector_toTarget_withObject(selector: SEL; target: id; argument: id); message 'detachNewThreadSelector:toTarget:withObject:';
    class function isMultiThreaded: Boolean; message 'isMultiThreaded';
    function threadDictionary: NSMutableDictionary; message 'threadDictionary';
    class procedure sleepUntilDate(date: NSDate); message 'sleepUntilDate:';
    class procedure sleepForTimeInterval(ti: NSTimeInterval); message 'sleepForTimeInterval:';
    class procedure exit; message 'exit';
    class function threadPriority: double; message 'threadPriority';
    class function setThreadPriority(p: double): Boolean; message 'setThreadPriority:';
    class function callStackReturnAddresses: NSArray; message 'callStackReturnAddresses';
    procedure setName(n: NSString); message 'setName:';
    function name: NSString; message 'name';
    function stackSize: NSUInteger; message 'stackSize';
    procedure setStackSize(s: NSUInteger); message 'setStackSize:';
    function isMainThread: Boolean; message 'isMainThread';
    class function mainThread: NSThread; message 'mainThread';
    function init: id; message 'init';
    function initWithTarget_selector_object(target: id; selector: SEL; argument: id): id; message 'initWithTarget:selector:object:';
    function isExecuting: Boolean; message 'isExecuting';
    function isFinished: Boolean; message 'isFinished';
    function isCancelled: Boolean; message 'isCancelled';
    procedure cancel; message 'cancel';
    procedure start; message 'start';
    procedure main; message 'main';
  end; external;

{$endif}
{$endif}
