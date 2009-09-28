{ Parsed from Foundation.framework NSRunLoop.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSRUNLOOP_PAS_H}
{$define NSRUNLOOP_PAS_H}
type
  NSRunLoopPointer = Pointer;

{$endif}
{$endif}

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
  NSDefaultRunLoopMode: NSString; external name '_NSDefaultRunLoopMode';
  NSRunLoopCommonModes: NSString; external name '_NSRunLoopCommonModes';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRunLoop = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRUNLOOP_PAS_C}
{$define NSRUNLOOP_PAS_C}

{ NSRunLoop }
  NSRunLoop = objcclass(NSObject)
  private
    __rl: id;
    __dperf: id;
    __perft: id;
        _reserved: array[0..7] of Pointer;
    
  public
    class function alloc: NSRunLoop; message 'alloc';

    class function currentRunLoop: NSRunLoop; message 'currentRunLoop';
    function currentMode: NSString; message 'currentMode';
    function getCFRunLoop: CFRunLoopRef; message 'getCFRunLoop';
    procedure addTimer_forMode(timer: NSTimer; mode: NSString); message 'addTimer:forMode:';
    procedure addPort_forMode(aPort: NSPort; mode: NSString); message 'addPort:forMode:';
    procedure removePort_forMode(aPort: NSPort; mode: NSString); message 'removePort:forMode:';
    function limitDateForMode(mode: NSString): NSDate; message 'limitDateForMode:';
    procedure acceptInputForMode_beforeDate(mode: NSString; limitDate: NSDate); message 'acceptInputForMode:beforeDate:';

    { Category: NSRunLoopConveniences }
    procedure run; message 'run';
    procedure runUntilDate(limitDate: NSDate); message 'runUntilDate:';
    function runMode_beforeDate(mode: NSString; limitDate: NSDate): Boolean; message 'runMode:beforeDate:';

    { Category: NSOrderedPerform }
    procedure performSelector_target_argument_order_modes(aSelector: SEL; target: id; arg: id; order: NSUInteger; modes: NSArray); message 'performSelector:target:argument:order:modes:';
    procedure cancelPerformSelector_target_argument(aSelector: SEL; target: id; arg: id); message 'cancelPerformSelector:target:argument:';
    procedure cancelPerformSelectorsWithTarget(target: id); message 'cancelPerformSelectorsWithTarget:';
  end; external;

{$endif}
{$endif}
