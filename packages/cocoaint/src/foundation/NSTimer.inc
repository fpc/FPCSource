{ Parsed from Foundation.framework NSTimer.h }

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
  NSTimerPtr = NSTimerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTIMER_PAS_C}
{$define NSTIMER_PAS_C}

{ NSTimer }
  NSTimer = objcclass external (NSObject)
    
  public
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
  end;

{$endif}
{$endif}
