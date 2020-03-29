{ Parsed from Foundation.framework NSTimer.h }


{$ifdef TYPES}
type
  NSTimerPtr = ^NSTimer;
{$endif}

{$ifdef CLASSES}

type
  NSTimer = objcclass external (NSObject)
  public
    class function timerWithTimeInterval_invocation_repeats (ti: NSTimeInterval; invocation: NSInvocation; yesOrNo: ObjCBOOL): NSTimer; message 'timerWithTimeInterval:invocation:repeats:';
    class function scheduledTimerWithTimeInterval_invocation_repeats (ti: NSTimeInterval; invocation: NSInvocation; yesOrNo: ObjCBOOL): NSTimer; message 'scheduledTimerWithTimeInterval:invocation:repeats:';
    class function timerWithTimeInterval_target_selector_userInfo_repeats (ti: NSTimeInterval; aTarget: id; aSelector: SEL; userInfo: id; yesOrNo: ObjCBOOL): NSTimer; message 'timerWithTimeInterval:target:selector:userInfo:repeats:';
    class function scheduledTimerWithTimeInterval_target_selector_userInfo_repeats (ti: NSTimeInterval; aTarget: id; aSelector: SEL; userInfo: id; yesOrNo: ObjCBOOL): NSTimer; message 'scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:';
    function initWithFireDate_interval_target_selector_userInfo_repeats (date: NSDate; ti: NSTimeInterval; t: id; s: SEL; ui: id; rep: ObjCBOOL): instancetype; message 'initWithFireDate:interval:target:selector:userInfo:repeats:'; { NS_DESIGNATED_INITIALIZER }
    procedure fire; message 'fire';
    procedure setFireDate(newValue: NSDate); message 'setFireDate:';
    function fireDate: NSDate; message 'fireDate';
    function timeInterval: NSTimeInterval; message 'timeInterval';
    procedure setTolerance(newValue: NSTimeInterval); message 'setTolerance:';
    function tolerance: NSTimeInterval; message 'tolerance';
    procedure invalidate; message 'invalidate';
    function isValid: ObjCBOOL; message 'isValid';
    function userInfo: id; message 'userInfo';
  end;
{$endif}

