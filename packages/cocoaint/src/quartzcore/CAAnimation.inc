{ Parsed from Quartzcore.framework CAAnimation.h }

{$ifdef TYPES}
{$ifndef CAANIMATION_PAS_T}
{$define CAANIMATION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAANIMATION_PAS_R}
{$define CAANIMATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAANIMATION_PAS_F}
{$define CAANIMATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAANIMATION_PAS_S}
{$define CAANIMATION_PAS_S}

{ External string constants }
var
  kCAAnimationLinear: NSString; cvar; external;
  kCAAnimationDiscrete: NSString; cvar; external;
  kCAAnimationPaced: NSString; cvar; external;
  kCAAnimationRotateAuto: NSString; cvar; external;
  kCAAnimationRotateAutoReverse: NSString; cvar; external;
  kCATransitionFade: NSString; cvar; external;
  kCATransitionMoveIn: NSString; cvar; external;
  kCATransitionPush: NSString; cvar; external;
  kCATransitionReveal: NSString; cvar; external;
  kCATransitionFromRight: NSString; cvar; external;
  kCATransitionFromLeft: NSString; cvar; external;
  kCATransitionFromTop: NSString; cvar; external;
  kCATransitionFromBottom: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAAnimation = objcclass;
  CAAnimationPointer = ^CAAnimation;
  CAAnimationPtr = CAAnimationPointer;
  CAPropertyAnimation = objcclass;
  CAPropertyAnimationPointer = ^CAPropertyAnimation;
  CAPropertyAnimationPtr = CAPropertyAnimationPointer;
  CABasicAnimation = objcclass;
  CABasicAnimationPointer = ^CABasicAnimation;
  CABasicAnimationPtr = CABasicAnimationPointer;
  CAKeyframeAnimation = objcclass;
  CAKeyframeAnimationPointer = ^CAKeyframeAnimation;
  CAKeyframeAnimationPtr = CAKeyframeAnimationPointer;
  CATransition = objcclass;
  CATransitionPointer = ^CATransition;
  CATransitionPtr = CATransitionPointer;
  CAAnimationGroup = objcclass;
  CAAnimationGroupPointer = ^CAAnimationGroup;
  CAAnimationGroupPtr = CAAnimationGroupPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAANIMATION_PAS_C}
{$define CAANIMATION_PAS_C}

{ CAAnimation }
  CAAnimation = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol, CAMediaTimingProtocol, CAActionProtocol)
  private
    __attr: Pointer;
    _flags: cuint32;
    
  public
    class function animation: id; message 'animation';
    class function defaultValueForKey(key: NSString): id; message 'defaultValueForKey:';
    function shouldArchiveValueForKey(key: NSString): Boolean; message 'shouldArchiveValueForKey:';
    procedure setTimingFunction (newValue: CAMediaTimingFunction); message 'setTimingFunction:';
    function timingFunction: CAMediaTimingFunction; message 'timingFunction';
    procedure setDelegate (newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setRemovedOnCompletion (newValue: Boolean); message 'setRemovedOnCompletion:';
    function isRemovedOnCompletion: Boolean; message 'isRemovedOnCompletion';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
    procedure setBeginTime (newValue: CFTimeInterval);
    function beginTime: CFTimeInterval;
    procedure setDuration (newValue: CFTimeInterval);
    function duration: CFTimeInterval;
    procedure setSpeed (newValue: single);
    function speed: single;
    procedure setTimeOffset (newValue: CFTimeInterval);
    function timeOffset: CFTimeInterval;
    procedure setRepeatCount (newValue: single);
    function repeatCount: single;
    procedure setRepeatDuration (newValue: CFTimeInterval);
    function repeatDuration: CFTimeInterval;
    procedure setAutoreverses (newValue: Boolean);
    function autoreverses: Boolean;
    procedure setFillMode (newValue: NSString);
    function fillMode: NSString;
  end;

{ CAPropertyAnimation }
  CAPropertyAnimation = objcclass external (CAAnimation)
    
  public
    class function animationWithKeyPath(path: NSString): id; message 'animationWithKeyPath:';
    procedure setKeyPath (newValue: NSString); message 'setKeyPath:';
    function keyPath: NSString; message 'keyPath';
    procedure setAdditive (newValue: Boolean); message 'setAdditive:';
    function isAdditive: Boolean; message 'isAdditive';
    procedure setCumulative (newValue: Boolean); message 'setCumulative:';
    function isCumulative: Boolean; message 'isCumulative';
    procedure setValueFunction (newValue: CAValueFunction); message 'setValueFunction:';
    function valueFunction: CAValueFunction; message 'valueFunction';
  end;

{ CABasicAnimation }
  CABasicAnimation = objcclass external (CAPropertyAnimation)
    
  public
    procedure setFromValue (newValue: id); message 'setFromValue:';
    function fromValue: id; message 'fromValue';
    procedure setToValue (newValue: id); message 'setToValue:';
    function toValue: id; message 'toValue';
    procedure setByValue (newValue: id); message 'setByValue:';
    function byValue: id; message 'byValue';
  end;

{ CAKeyframeAnimation }
  CAKeyframeAnimation = objcclass external (CAPropertyAnimation)
    
  public
    procedure setValues (newValue: NSArray); message 'setValues:';
    function values: NSArray; message 'values';
    procedure setPath (newValue: CGPathRef); message 'setPath:';
    function path: CGPathRef; message 'path';
    procedure setKeyTimes (newValue: NSArray); message 'setKeyTimes:';
    function keyTimes: NSArray; message 'keyTimes';
    procedure setTimingFunctions (newValue: NSArray); message 'setTimingFunctions:';
    function timingFunctions: NSArray; message 'timingFunctions';
    procedure setCalculationMode (newValue: NSString); message 'setCalculationMode:';
    function calculationMode: NSString; message 'calculationMode';
    procedure setRotationMode (newValue: NSString); message 'setRotationMode:';
    function rotationMode: NSString; message 'rotationMode';
  end;

{ CATransition }
  CATransition = objcclass external (CAAnimation)
    
  public
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setSubtype (newValue: NSString); message 'setSubtype:';
    function subtype: NSString; message 'subtype';
    procedure setStartProgress (newValue: single); message 'setStartProgress:';
    function startProgress: single; message 'startProgress';
    procedure setEndProgress (newValue: single); message 'setEndProgress:';
    function endProgress: single; message 'endProgress';
    procedure setFilter (newValue: id); message 'setFilter:';
    function filter: id; message 'filter';
  end;

{ CAAnimationGroup }
  CAAnimationGroup = objcclass external (CAAnimation)
    
  public
    procedure setAnimations (newValue: NSArray); message 'setAnimations:';
    function animations: NSArray; message 'animations';
  end;

{ CAAnimationDelegateCategory }
  CAAnimationDelegateCategory = objccategory external (NSObject)
    procedure animationDidStart(anim: CAAnimation); message 'animationDidStart:';
    procedure animationDidStop_finished(anim: CAAnimation; flag: Boolean); message 'animationDidStop:finished:';
  end;

{$endif}
{$endif}
