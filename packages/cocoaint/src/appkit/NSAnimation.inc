{ Parsed from Appkit.framework NSAnimation.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSANIMATION_PAS_H}
{$define NSANIMATION_PAS_H}
type
  NSAnimationPointer = Pointer;
  NSViewAnimationPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSANIMATION_PAS_T}
{$define NSANIMATION_PAS_T}

{ Constants }

const
  NSAnimationEaseIn = 0;
  NSAnimationEaseOut = 1;
  NSAnimationLinear = 2;

const
  NSAnimationBlocking = 0;
  NSAnimationNonblocking = 1;
  NSAnimationNonblockingThreaded = 2;

{ Types }
type
  NSAnimationCurve = NSUInteger;
  NSAnimationBlockingMode = NSUInteger;
  NSAnimationProgress = single;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSANIMATION_PAS_R}
{$define NSANIMATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSANIMATION_PAS_F}
{$define NSANIMATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSANIMATION_PAS_T}
{$define NSANIMATION_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAnimatablePropertyContainerProtocol = objcprotocol;
  NSAnimation = objcclass;
  NSViewAnimation = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSANIMATION_PAS_C}
{$define NSANIMATION_PAS_C}

{ NSAnimation }
  NSAnimation = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __duration: NSTimeInterval;
    __currentProgress: NSAnimationProgress;
    __framesPerSecond: single;
    __delegate: id;
    __timer: NSTimer;
    __startTime: NSTimeInterval;
    __progressMarks: NSMutableArray;
    __startAnimation: NSAnimation;
    __stopAnimation: NSAnimation;
    __nextProgressMark: cint;
    __aFlags: bitpacked record
        delegateAnimationShouldStart: 0..1;
        delegateAnimationDidStop: 0..1;
        delegateAnimationDidEnd: 0..1;
        delegateAnimationValueForProgress: 0..1;
        delegateAnimationDidReachProgressMark: 0..1;
        animating: 0..1;
        blocking: 0..1;
        reserved: 0..((1 shl 25)-1);
      end;
    __aSettings: bitpacked record
        animationCurve: 0..((1 shl 8)-1);
        animationBlockingMode: 0..((1 shl 2)-1);
        reserved: 0..((1 shl 22)-1);
      end;
    __reserved1: NSInteger;
    __reserved2: NSInteger;
    __reserved3: NSInteger;
    __reserved4: NSInteger;
    
  public
    class function alloc: NSAnimation; message 'alloc';

    function initWithDuration_animationCurve(duration_: NSTimeInterval; animationCurve_: NSAnimationCurve): id; message 'initWithDuration:animationCurve:';
    procedure startAnimation; message 'startAnimation';
    procedure stopAnimation; message 'stopAnimation';
    function isAnimating: Boolean; message 'isAnimating';
    function currentProgress: NSAnimationProgress; message 'currentProgress';
    procedure setCurrentProgress(progress: NSAnimationProgress); message 'setCurrentProgress:';
    procedure setDuration(duration_: NSTimeInterval); message 'setDuration:';
    function duration: NSTimeInterval; message 'duration';
    function animationBlockingMode: NSAnimationBlockingMode; message 'animationBlockingMode';
    procedure setAnimationBlockingMode(animationBlockingMode_: NSAnimationBlockingMode); message 'setAnimationBlockingMode:';
    procedure setFrameRate(framesPerSecond: single); message 'setFrameRate:';
    function frameRate: single; message 'frameRate';
    procedure setAnimationCurve(curve: NSAnimationCurve); message 'setAnimationCurve:';
    function animationCurve: NSAnimationCurve; message 'animationCurve';
    function currentValue: single; message 'currentValue';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function progressMarks: NSArray; message 'progressMarks';
    procedure setProgressMarks(progressMarks_: NSArray); message 'setProgressMarks:';
    procedure addProgressMark(progressMark: NSAnimationProgress); message 'addProgressMark:';
    procedure removeProgressMark(progressMark: NSAnimationProgress); message 'removeProgressMark:';
    procedure startWhenAnimation_reachesProgress(animation: NSAnimation; startProgress: NSAnimationProgress); message 'startWhenAnimation:reachesProgress:';
    procedure stopWhenAnimation_reachesProgress(animation: NSAnimation; stopProgress: NSAnimationProgress); message 'stopWhenAnimation:reachesProgress:';
    procedure clearStartAnimation; message 'clearStartAnimation';
    procedure clearStopAnimation; message 'clearStopAnimation';
    function runLoopModesForAnimating: NSArray; message 'runLoopModesForAnimating';
  end; external;

{ NSViewAnimation }
  NSViewAnimation = objcclass(NSAnimation)
  private
    __viewAnimations: NSArray;
    __viewAnimationInfo: CFMutableDictionaryRef;
    __windowAnimationInfo: CFMutableDictionaryRef;
    __reserved4a: NSUInteger;
    __reserved4b: NSUInteger;
    __reserved4c: NSUInteger;
    __vaFlags: bitpacked record
        reserved: 0..((1 shl 32)-1);
      end;
    __reserved5: NSUInteger;
    __reserved6: NSUInteger;
    __reserved7: NSUInteger;
    __reserved8: NSUInteger;
    
  public
    class function alloc: NSViewAnimation; message 'alloc';

    function initWithViewAnimations(viewAnimations_: NSArray): id; message 'initWithViewAnimations:';
    function viewAnimations: NSArray; message 'viewAnimations';
    procedure setViewAnimations(viewAnimations_: NSArray); message 'setViewAnimations:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSANIMATION_PAS_P}
{$define NSANIMATION_PAS_P}
  
{ NSAnimatablePropertyContainer Protocol }
  NSAnimatablePropertyContainerProtocol = objcprotocol
    function animator: id; message 'animator';
    function animations: NSDictionary; message 'animations';
    procedure setAnimations(dict: NSDictionary); message 'setAnimations:';
    function animationForKey(key: NSString): id; message 'animationForKey:';
  end; external name 'NSAnimatablePropertyContainer';
{$endif}
{$endif}
