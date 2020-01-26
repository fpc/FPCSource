{ Parsed from Foundation.framework NSThread.h }


{$ifdef TYPES}
type
  NSThreadPtr = ^NSThread;
{$endif}

{$ifdef CLASSES}

type
  NSThread = objcclass external (NSObject)
  private
    _private: id;
    _bytes:array[0..43] of cuint8;
  public
    class function currentThread: NSThread; message 'currentThread';
    class procedure detachNewThreadSelector_toTarget_withObject (selector: SEL; target: id; argument: id); message 'detachNewThreadSelector:toTarget:withObject:';
    class function isMultiThreaded: ObjCBOOL; message 'isMultiThreaded';
    function threadDictionary: NSMutableDictionary; message 'threadDictionary';
    class procedure sleepUntilDate (date: NSDate); message 'sleepUntilDate:';
    class procedure sleepForTimeInterval (ti: NSTimeInterval); message 'sleepForTimeInterval:';
    class procedure exit; message 'exit';
    class function classThreadPriority: double; message 'threadPriority';
    class function setThreadPriority (p: double): ObjCBOOL; message 'setThreadPriority:';
    procedure setThreadPriority_(newValue: double); message 'setThreadPriority:';
    function threadPriority: double; message 'threadPriority';
    procedure setQualityOfService(newValue: NSQualityOfService); message 'setQualityOfService:';
    function qualityOfService: NSQualityOfService; message 'qualityOfService';
    class function callStackReturnAddresses: NSArray; message 'callStackReturnAddresses'; { available in 10_5, 2_0 }
    class function callStackSymbols: NSArray; message 'callStackSymbols'; { available in 10_6, 4_0 }
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setStackSize(newValue: NSUInteger); message 'setStackSize:';
    function stackSize: NSUInteger; message 'stackSize';
    function isMainThread: ObjCBOOL; message 'isMainThread';
    class function classIsMainThread: ObjCBOOL; message 'isMainThread'; { available in 10_5, 2_0 }
    class function mainThread: NSThread; message 'mainThread'; { available in 10_5, 2_0 }
    function init: instancetype; message 'init'; { available in 10_5, 2_0 }
    function initWithTarget_selector_object (target: id; selector: SEL; argument: id): instancetype; message 'initWithTarget:selector:object:'; { available in 10_5, 2_0 }
    function isExecuting: ObjCBOOL; message 'isExecuting';
    function isFinished: ObjCBOOL; message 'isFinished';
    function isCancelled: ObjCBOOL; message 'isCancelled';
    procedure cancel; message 'cancel'; { available in 10_5, 2_0 }
    procedure start; message 'start'; { available in 10_5, 2_0 }
    procedure main; message 'main'; { available in 10_5, 2_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSWillBecomeMultiThreadedNotification: NSString; cvar; external;
  NSDidBecomeSingleThreadedNotification: NSString; cvar; external;
  NSThreadWillExitNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSThreadPerformAdditions = objccategory external (NSObject)
    procedure performSelectorOnMainThread_withObject_waitUntilDone_modes (aSelector: SEL; arg: id; wait: ObjCBOOL; array_: NSArray); message 'performSelectorOnMainThread:withObject:waitUntilDone:modes:';
    procedure performSelectorOnMainThread_withObject_waitUntilDone (aSelector: SEL; arg: id; wait: ObjCBOOL); message 'performSelectorOnMainThread:withObject:waitUntilDone:';
    procedure performSelector_onThread_withObject_waitUntilDone_modes (aSelector: SEL; thr: NSThread; arg: id; wait: ObjCBOOL; array_: NSArray); message 'performSelector:onThread:withObject:waitUntilDone:modes:'; { available in 10_5, 2_0 }
    procedure performSelector_onThread_withObject_waitUntilDone (aSelector: SEL; thr: NSThread; arg: id; wait: ObjCBOOL); message 'performSelector:onThread:withObject:waitUntilDone:'; { available in 10_5, 2_0 }
    procedure performSelectorInBackground_withObject (aSelector: SEL; arg: id); message 'performSelectorInBackground:withObject:'; { available in 10_5, 2_0 }
  end;
{$endif}

