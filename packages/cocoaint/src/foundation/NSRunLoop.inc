{ Parsed from Foundation.framework NSRunLoop.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSRUNLOOP_PAS_T}
{$define NSRUNLOOP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRUNLOOP_PAS_R}
{$define NSRUNLOOP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRUNLOOP_PAS_F}
{$define NSRUNLOOP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRUNLOOP_PAS_S}
{$define NSRUNLOOP_PAS_S}

{ External string constants }
var
  NSDefaultRunLoopMode: NSString; cvar; external;
  NSRunLoopCommonModes: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRunLoop = objcclass;
  NSRunLoopPointer = ^NSRunLoop;
  NSRunLoopPtr = NSRunLoopPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRUNLOOP_PAS_C}
{$define NSRUNLOOP_PAS_C}

{ NSRunLoop }
  NSRunLoop = objcclass(NSObject)
  private
    _rl: id;
    _dperf: id;
    _perft: id;
    _info: id;
    _ports: id;
        _reserved: array[0..5] of Pointer;
    
  public
    class function alloc: NSRunLoop; message 'alloc';

    class function currentRunLoop: NSRunLoop; message 'currentRunLoop';
    class function mainRunLoop: NSRunLoop; message 'mainRunLoop';
    function currentMode: NSString; message 'currentMode';
    function getCFRunLoop: CFRunLoopRef; message 'getCFRunLoop';
    procedure addTimer_forMode(timer: NSTimer; mode: NSString); message 'addTimer:forMode:';
    procedure addPort_forMode(aPort: NSPort; mode: NSString); message 'addPort:forMode:';
    procedure removePort_forMode(aPort: NSPort; mode: NSString); message 'removePort:forMode:';
    function limitDateForMode(mode: NSString): NSDate; message 'limitDateForMode:';
    procedure acceptInputForMode_beforeDate(mode: NSString; limitDate: NSDate); message 'acceptInputForMode:beforeDate:';
  end; external;

{ NSRunLoopConveniencesCategory }
  NSRunLoopConveniencesCategory = objccategory(NSRunLoop)
    procedure run; message 'run';
    procedure runUntilDate(limitDate: NSDate); message 'runUntilDate:';
    function runMode_beforeDate(mode: NSString; limitDate: NSDate): Boolean; message 'runMode:beforeDate:';
    procedure configureAsServer; message 'configureAsServer'; deprecated 'in Mac OS X 10.5 and later';
  end; external;

{ NSDelayedPerformingCategory }
  NSDelayedPerformingCategory = objccategory(NSObject)
    procedure performSelector_withObject_afterDelay_inModes(aSelector: SEL; anArgument: id; delay: NSTimeInterval; modes: NSArray); message 'performSelector:withObject:afterDelay:inModes:';
    procedure performSelector_withObject_afterDelay(aSelector: SEL; anArgument: id; delay: NSTimeInterval); message 'performSelector:withObject:afterDelay:';
    class procedure cancelPreviousPerformRequestsWithTarget_selector_object(aTarget: id; aSelector: SEL; anArgument: id); message 'cancelPreviousPerformRequestsWithTarget:selector:object:';
    class procedure cancelPreviousPerformRequestsWithTarget(aTarget: id); message 'cancelPreviousPerformRequestsWithTarget:';
  end; external;

{ NSOrderedPerformCategory }
  NSOrderedPerformCategory = objccategory(NSRunLoop)
    procedure performSelector_target_argument_order_modes(aSelector: SEL; target: id; arg: id; order: NSUInteger; modes: NSArray); message 'performSelector:target:argument:order:modes:';
    procedure cancelPerformSelector_target_argument(aSelector: SEL; target: id; arg: id); message 'cancelPerformSelector:target:argument:';
    procedure cancelPerformSelectorsWithTarget(target: id); message 'cancelPerformSelectorsWithTarget:';
  end; external;

{$endif}
{$endif}
