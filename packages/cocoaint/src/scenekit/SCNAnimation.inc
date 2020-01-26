{ Parsed from SceneKit.framework SCNAnimation.h }


{$ifdef TYPES}
type
  SCNAnimationEventPtr = ^SCNAnimationEvent;
  SCNAnimatableProtocolPtr = ^SCNAnimatableProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  SCNAnimatableProtocol = objcprotocol external name 'SCNAnimatable' (NSObjectProtocol)
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
  end;
{$endif}

{$ifdef CLASSES}

type
  SceneKitAdditions = objccategory external (CAAnimation)
    procedure setUsesSceneTimeBase(newValue: ObjCBOOL); message 'setUsesSceneTimeBase:';
    function usesSceneTimeBase: ObjCBOOL; message 'usesSceneTimeBase';
    procedure setFadeInDuration(newValue: CGFloat); message 'setFadeInDuration:';
    function fadeInDuration: CGFloat; message 'fadeInDuration';
    procedure setFadeOutDuration(newValue: CGFloat); message 'setFadeOutDuration:';
    function fadeOutDuration: CGFloat; message 'fadeOutDuration';
    procedure setAnimationEvents(newValue: NSArray); message 'setAnimationEvents:';
    function animationEvents: NSArray; message 'animationEvents';
  end;
{$endif}

{$ifdef TYPES}
type
  SCNAnimationEventBlock = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  SCNAnimationEvent = objcclass external (NSObject)
  private
    _reserved: id;
  public
    class function animationEventWithKeyTime_block (time: CGFloat; eventBlock: SCNAnimationEventBlock): instancetype; message 'animationEventWithKeyTime:block:';
  end;
{$endif}

