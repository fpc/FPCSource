{ Parsed from AppKit.framework NSAnimation.h }


{$ifdef TYPES}
type
  NSAnimationPtr = ^NSAnimation;
  NSViewAnimationPtr = ^NSViewAnimation;
  NSAnimationDelegateProtocolPtr = ^NSAnimationDelegateProtocol;
  NSAnimatablePropertyContainerProtocolPtr = ^NSAnimatablePropertyContainerProtocol;
{$endif}

{$ifdef TYPES}
type
  NSAnimationCurve = NSUInteger;
  NSAnimationCurvePtr = ^NSAnimationCurve;

const
  NSAnimationEaseInOut = 0;
  NSAnimationEaseIn = 1;
  NSAnimationEaseOut = 2;
  NSAnimationLinear = 3;

type
  NSAnimationBlockingMode = NSUInteger;
  NSAnimationBlockingModePtr = ^NSAnimationBlockingMode;

const
  NSAnimationBlocking = 0;
  NSAnimationNonblocking = 1;
  NSAnimationNonblockingThreaded = 2;

type
  NSAnimationProgress = single;
  NSAnimationProgressPtr = ^NSAnimationProgress;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAnimationProgressMarkNotification: NSString; cvar; external;
  NSAnimationProgressMark: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSAnimation = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _duration: NSTimeInterval;
    _currentProgress: NSAnimationProgress;
    _framesPerSecond: single;
    _delegate: id;
    _displayLink: NSDisplayLink;
    _startTime: NSTimeInterval;
    _progressMarks: NSMutableArray;
    _startAnimation: NSAnimation;
    _stopAnimation: NSAnimation;
    _nextProgressMark: cint;
    _aFlags: bitpacked record
      case byte of
        0: (_anonBitField__aFlags0: cuint);
        1: (
          delegateAnimationShouldStart: 0..1;
          delegateAnimationDidStop: 0..1;
          delegateAnimationDidEnd: 0..1;
          delegateAnimationValueForProgress: 0..1;
          delegateAnimationDidReachProgressMark: 0..1;
          animating: 0..1;
          blocking: 0..1;
          reserved: 0..((1 shl 25)-1);
        );
      end;
    _aSettings: bitpacked record
      case byte of
        0: (_anonBitField__aSettings0: cuint);
        1: (
          animationCurve: 0..((1 shl 8)-1);
          animationBlockingMode: 0..((1 shl 2)-1);
          reserved: 0..((1 shl 22)-1);
        );
      end;
    _scheduledRunLoop: NSRunLoop;
    _reserved2: NSInteger;
    _reserved3: NSInteger;
    _reserved4: NSInteger;
  public
    function initWithDuration_animationCurve (duration: NSTimeInterval; animationCurve: NSAnimationCurve): instancetype; message 'initWithDuration:animationCurve:';
    procedure startAnimation; message 'startAnimation';
    procedure stopAnimation; message 'stopAnimation';
    function isAnimating: ObjCBOOL; message 'isAnimating';
    procedure setCurrentProgress(newValue: NSAnimationProgress); message 'setCurrentProgress:';
    function currentProgress: NSAnimationProgress; message 'currentProgress';
    procedure setDuration(newValue: NSTimeInterval); message 'setDuration:';
    function duration: NSTimeInterval; message 'duration';
    procedure setAnimationBlockingMode(newValue: NSAnimationBlockingMode); message 'setAnimationBlockingMode:';
    function animationBlockingMode: NSAnimationBlockingMode; message 'animationBlockingMode';
    procedure setFrameRate(newValue: single); message 'setFrameRate:';
    function frameRate: single; message 'frameRate';
    procedure setAnimationCurve(newValue: NSAnimationCurve); message 'setAnimationCurve:';
    function animationCurve: NSAnimationCurve; message 'animationCurve';
    function currentValue: single; message 'currentValue';
    procedure setDelegate(newValue: NSAnimationDelegateProtocol); message 'setDelegate:';
    function delegate: NSAnimationDelegateProtocol; message 'delegate';
    procedure setProgressMarks(newValue: NSArray); message 'setProgressMarks:';
    function progressMarks: NSArray; message 'progressMarks';
    procedure addProgressMark (progressMark: NSAnimationProgress); message 'addProgressMark:';
    procedure removeProgressMark (progressMark: NSAnimationProgress); message 'removeProgressMark:';
    procedure startWhenAnimation_reachesProgress (animation: NSAnimation; startProgress: NSAnimationProgress); message 'startWhenAnimation:reachesProgress:';
    procedure stopWhenAnimation_reachesProgress (animation: NSAnimation; stopProgress: NSAnimationProgress); message 'stopWhenAnimation:reachesProgress:';
    procedure clearStartAnimation; message 'clearStartAnimation';
    procedure clearStopAnimation; message 'clearStopAnimation';
    function runLoopModesForAnimating: NSArray; message 'runLoopModesForAnimating';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSAnimationDelegateProtocol = objcprotocol external name 'NSAnimationDelegate' (NSObjectProtocol)
  optional
    function animationShouldStart (animation: NSAnimation): ObjCBOOL; message 'animationShouldStart:';
    procedure animationDidStop (animation: NSAnimation); message 'animationDidStop:';
    procedure animationDidEnd (animation: NSAnimation); message 'animationDidEnd:';
    function animation_valueForProgress (animation: NSAnimation; progress: NSAnimationProgress): single; message 'animation:valueForProgress:';
    procedure animation_didReachProgressMark (animation: NSAnimation; progress: NSAnimationProgress); message 'animation:didReachProgressMark:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSViewAnimationTargetKey: NSString; cvar; external;
  NSViewAnimationStartFrameKey: NSString; cvar; external;
  NSViewAnimationEndFrameKey: NSString; cvar; external;
  NSViewAnimationEffectKey: NSString; cvar; external;
  NSViewAnimationFadeInEffect: NSString; cvar; external;
  NSViewAnimationFadeOutEffect: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSViewAnimation = objcclass external (NSAnimation)
  private
    _viewAnimations: NSArray;
    _viewAnimationInfo: id;
    _windowAnimationInfo: id;
    _reserved4a: NSUInteger;
    _reserved4b: NSUInteger;
    _reserved4c: NSUInteger;
    _vaFlags: bitpacked record
      case byte of
        0: (_anonBitField__vaFlags0: cuint);
        1: (
          reserved: 0..((1 shl 32)-1);
        );
      end;
    _reserved5: NSUInteger;
    _reserved6: NSUInteger;
    _reserved7: NSUInteger;
    _reserved8: NSUInteger;
  public
    function initWithViewAnimations (viewAnimations: NSArray): instancetype; message 'initWithViewAnimations:';
    procedure setViewAnimations(newValue: NSArray); message 'setViewAnimations:';
    function viewAnimations: NSArray; message 'viewAnimations';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSAnimatablePropertyContainerProtocol = objcprotocol external name 'NSAnimatablePropertyContainer'
    function animator: instancetype; message 'animator'; { available in 10_5 }
    function animations: NSDictionary; message 'animations'; { available in 10_5 }
    procedure setAnimations (animations_: NSDictionary); message 'setAnimations:'; { available in 10_5 }
    function animationForKey (key: NSString): id; message 'animationForKey:'; { available in 10_5 }
    class function defaultAnimationForKey (key: NSString): id; message 'defaultAnimationForKey:'; { available in 10_5 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAnimationTriggerOrderIn: NSString { available in 10_5 }; cvar; external;
  NSAnimationTriggerOrderOut: NSString { available in 10_5 }; cvar; external;
{$endif}

