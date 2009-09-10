{ Parsed from Foundation.framework NSThread.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTHREAD_PAS_H}
{$define NSTHREAD_PAS_H}
type
  NSThreadPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTHREAD_PAS_T}
{$define NSTHREAD_PAS_T}

{ CFString constants }
var
  NSWillBecomeMultiThreadedNotification: CFStringRef; external name '_NSWillBecomeMultiThreadedNotification';
  NSDidBecomeSingleThreadedNotification: CFStringRef; external name '_NSDidBecomeSingleThreadedNotification';
  NSThreadWillExitNotification: CFStringRef; external name '_NSThreadWillExitNotification';

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
{$ifndef NSTHREAD_PAS_T}
{$define NSTHREAD_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSThread = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTHREAD_PAS_C}
{$define NSTHREAD_PAS_C}

{ NSThread }
  NSThread = objcclass(NSObject)
  private
    __private: id;
    __bytes: byte;
    
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
    function stackSize: culong; message 'stackSize';
    procedure setStackSize(s: culong); message 'setStackSize:';
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
