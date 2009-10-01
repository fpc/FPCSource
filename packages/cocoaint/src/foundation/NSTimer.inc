{ Parsed from Foundation.framework NSTimer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTIMER_PAS_T}
{$define NSTIMER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTIMER_PAS_R}
{$define NSTIMER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTIMER_PAS_F}
{$define NSTIMER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTIMER_PAS_S}
{$define NSTIMER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTimer = objcclass;
  NSTimerPointer = ^NSTimer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTIMER_PAS_C}
{$define NSTIMER_PAS_C}

{ NSTimer }
  NSTimer = objcclass(NSObject)
    
  public
    class function alloc: NSTimer; message 'alloc';

    class function timerWithTimeInterval_invocation_repeats(ti: NSTimeInterval; invocation: NSInvocation; yesOrNo: Boolean): NSTimer; message 'timerWithTimeInterval:invocation:repeats:';
    class function scheduledTimerWithTimeInterval_invocation_repeats(ti: NSTimeInterval; invocation: NSInvocation; yesOrNo: Boolean): NSTimer; message 'scheduledTimerWithTimeInterval:invocation:repeats:';
    class function timerWithTimeInterval_target_selector_userInfo_repeats(ti: NSTimeInterval; aTarget: id; aSelector: SEL; userInfo_: id; yesOrNo: Boolean): NSTimer; message 'timerWithTimeInterval:target:selector:userInfo:repeats:';
    class function scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(ti: NSTimeInterval; aTarget: id; aSelector: SEL; userInfo_: id; yesOrNo: Boolean): NSTimer; message 'scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:';
    function initWithFireDate_interval_target_selector_userInfo_repeats(date: NSDate; ti: NSTimeInterval; t: id; s: SEL; ui: id; rep: Boolean): id; message 'initWithFireDate:interval:target:selector:userInfo:repeats:';
    procedure fire; message 'fire';
    function fireDate: NSDate; message 'fireDate';
    procedure setFireDate(date: NSDate); message 'setFireDate:';
    function timeInterval: NSTimeInterval; message 'timeInterval';
    procedure invalidate; message 'invalidate';
    function isValid: Boolean; message 'isValid';
    function userInfo: id; message 'userInfo';
  end; external;

{$endif}
{$endif}
