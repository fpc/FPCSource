{ Parsed from QuartzCore.framework CAAnimation.h }


{$ifdef TYPES}
type
  CAAnimationPtr = ^CAAnimation;
  CAPropertyAnimationPtr = ^CAPropertyAnimation;
  CABasicAnimationPtr = ^CABasicAnimation;
  CAKeyframeAnimationPtr = ^CAKeyframeAnimation;
  CATransitionPtr = ^CATransition;
  CAAnimationGroupPtr = ^CAAnimationGroup;
{$endif}

{$ifdef CLASSES}

type
  CAAnimation = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol, CAMediaTimingProtocol, CAActionProtocol)
  private
    _attr: pointer;
    _flags: cuint32;
  public
    class function animation: instancetype; message 'animation';
    class function defaultValueForKey (key: NSString): id; message 'defaultValueForKey:';
    function shouldArchiveValueForKey (key: NSString): ObjCBOOL; message 'shouldArchiveValueForKey:';
    procedure setTimingFunction(newValue: CAMediaTimingFunction); message 'setTimingFunction:';
    function timingFunction: CAMediaTimingFunction; message 'timingFunction';
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setRemovedOnCompletion(newValue: ObjCBOOL); message 'setRemovedOnCompletion:';
    function isRemovedOnCompletion: ObjCBOOL; message 'isRemovedOnCompletion';

    { Adopted protocols }
    procedure setAutoreverses(newValue: ObjCBOOL); message 'setAutoreverses:';
    function autoreverses: ObjCBOOL; message 'autoreverses';
    procedure setBeginTime(newValue: CFTimeInterval); message 'setBeginTime:';
    function beginTime: CFTimeInterval; message 'beginTime';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure setDuration(newValue: CFTimeInterval); message 'setDuration:';
    function duration: CFTimeInterval; message 'duration';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    procedure setFillMode(newValue: NSString); message 'setFillMode:';
    function fillMode: NSString; message 'fillMode';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    procedure setRepeatCount(newValue: single); message 'setRepeatCount:';
    function repeatCount: single; message 'repeatCount';
    procedure setRepeatDuration(newValue: CFTimeInterval); message 'setRepeatDuration:';
    function repeatDuration: CFTimeInterval; message 'repeatDuration';
    procedure runActionForKey_object_arguments (event: NSString; anObject: id; dict: NSDictionary); message 'runActionForKey:object:arguments:';
    procedure setSpeed(newValue: single); message 'setSpeed:';
    function speed: single; message 'speed';
    procedure setTimeOffset(newValue: CFTimeInterval); message 'setTimeOffset:';
    function timeOffset: CFTimeInterval; message 'timeOffset';
  end;


type
  CAAnimationDelegate = objccategory external (NSObject)
    procedure animationDidStart (anim: CAAnimation); message 'animationDidStart:';
    procedure animationDidStop_finished (anim: CAAnimation; flag: ObjCBOOL); message 'animationDidStop:finished:';
  end;


type
  CAPropertyAnimation = objcclass external (CAAnimation)
  public
    class function animationWithKeyPath (path: NSString): instancetype; message 'animationWithKeyPath:';
    procedure setKeyPath(newValue: NSString); message 'setKeyPath:';
    function keyPath: NSString; message 'keyPath';
    procedure setAdditive(newValue: ObjCBOOL); message 'setAdditive:';
    function isAdditive: ObjCBOOL; message 'isAdditive';
    procedure setCumulative(newValue: ObjCBOOL); message 'setCumulative:';
    function isCumulative: ObjCBOOL; message 'isCumulative';
    procedure setValueFunction(newValue: CAValueFunction); message 'setValueFunction:';
    function valueFunction: CAValueFunction; message 'valueFunction';
  end;

type
  CABasicAnimation = objcclass external (CAPropertyAnimation)
  public
    procedure setFromValue(newValue: id); message 'setFromValue:';
    function fromValue: id; message 'fromValue';
    procedure setToValue(newValue: id); message 'setToValue:';
    function toValue: id; message 'toValue';

    procedure setByValue(newValue: id); message 'setByValue:';
    function byValue: id; message 'byValue';
  end;

type
  CAKeyframeAnimation = objcclass external (CAPropertyAnimation)
  public
    procedure setValues(newValue: NSArray); message 'setValues:';
    function values: NSArray; message 'values';
    procedure setPath(newValue: CGPathRef); message 'setPath:';
    function path: CGPathRef; message 'path';
    procedure setKeyTimes(newValue: NSArray); message 'setKeyTimes:';
    function keyTimes: NSArray; message 'keyTimes';
    procedure setTimingFunctions(newValue: NSArray); message 'setTimingFunctions:';
    function timingFunctions: NSArray; message 'timingFunctions';
    procedure setCalculationMode(newValue: NSString); message 'setCalculationMode:';
    function calculationMode: NSString; message 'calculationMode';
    procedure setTensionValues(newValue: NSArray); message 'setTensionValues:';
    function tensionValues: NSArray; message 'tensionValues';
    procedure setContinuityValues(newValue: NSArray); message 'setContinuityValues:';
    function continuityValues: NSArray; message 'continuityValues';

    procedure setBiasValues(newValue: NSArray); message 'setBiasValues:';
    function biasValues: NSArray; message 'biasValues';
    procedure setRotationMode(newValue: NSString); message 'setRotationMode:';
    function rotationMode: NSString; message 'rotationMode';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAAnimationLinear: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAAnimationDiscrete: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAAnimationPaced: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAAnimationCubic: NSString { available starting in __MAC_10_7, __IPHONE_4_0 }; cvar; external;
  kCAAnimationCubicPaced: NSString { available starting in __MAC_10_7, __IPHONE_4_0 }; cvar; external;
  kCAAnimationRotateAuto: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAAnimationRotateAutoReverse: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CATransition = objcclass external (CAAnimation)
  public
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
    procedure setSubtype(newValue: NSString); message 'setSubtype:';
    function subtype: NSString; message 'subtype';
    procedure setStartProgress(newValue: single); message 'setStartProgress:';
    function startProgress: single; message 'startProgress';
    procedure setEndProgress(newValue: single); message 'setEndProgress:';
    function endProgress: single; message 'endProgress';
    procedure setFilter(newValue: id); message 'setFilter:';
    function filter: id; message 'filter';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCATransitionFade: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionMoveIn: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionPush: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionReveal: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionFromRight: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionFromLeft: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionFromTop: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransitionFromBottom: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CAAnimationGroup = objcclass external (CAAnimation)
  public
    procedure setAnimations(newValue: NSArray); message 'setAnimations:';
    function animations: NSArray; message 'animations';
  end;
{$endif}

