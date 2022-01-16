{ Parsed from SceneKit.framework SCNLight.h }


{$ifdef TYPES}
type
  SCNLightPtr = ^SCNLight;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNLightTypeAmbient: NSString; cvar; external;
  SCNLightTypeOmni: NSString; cvar; external;
  SCNLightTypeDirectional: NSString; cvar; external;
  SCNLightTypeSpot: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  SCNShadowMode = NSInteger;
  SCNShadowModePtr = ^SCNShadowMode;

const
  SCNShadowModeForward = 0;
  SCNShadowModeDeferred = 1;
  SCNShadowModeModulated = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNLightAttenuationStartKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
  SCNLightAttenuationEndKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
  SCNLightAttenuationFalloffExponentKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
  SCNLightSpotInnerAngleKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
  SCNLightSpotOuterAngleKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
  SCNLightShadowNearClippingKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
  SCNLightShadowFarClippingKey: NSString deprecated 'in 10_8, 10_10, NA, NA'; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  SCNLight = objcclass external (NSObject, SCNAnimatableProtocol, SCNTechniqueSupportProtocol, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function light: instancetype; message 'light';
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
    procedure setColor(newValue: id); message 'setColor:';
    function color: id; message 'color';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setCastsShadow(newValue: ObjCBOOL); message 'setCastsShadow:';
    function castsShadow: ObjCBOOL; message 'castsShadow';
    procedure setShadowColor(newValue: id); message 'setShadowColor:';
    function shadowColor: id; message 'shadowColor';
    procedure setShadowRadius(newValue: CGFloat); message 'setShadowRadius:';
    function shadowRadius: CGFloat; message 'shadowRadius';
    procedure setShadowMapSize(newValue: CGSize); message 'setShadowMapSize:';
    function shadowMapSize: CGSize; message 'shadowMapSize';
    procedure setShadowSampleCount(newValue: NSUInteger); message 'setShadowSampleCount:';
    function shadowSampleCount: NSUInteger; message 'shadowSampleCount';
    procedure setShadowMode(newValue: SCNShadowMode); message 'setShadowMode:';
    function shadowMode: SCNShadowMode; message 'shadowMode';
    procedure setShadowBias(newValue: CGFloat); message 'setShadowBias:';
    function shadowBias: CGFloat; message 'shadowBias';
    procedure setOrthographicScale(newValue: CGFloat); message 'setOrthographicScale:';
    function orthographicScale: CGFloat; message 'orthographicScale';
    procedure setZNear(newValue: CGFloat); message 'setZNear:';
    function zNear: CGFloat; message 'zNear';
    procedure setZFar(newValue: CGFloat); message 'setZFar:';
    function zFar: CGFloat; message 'zFar';
    procedure setAttenuationStartDistance(newValue: CGFloat); message 'setAttenuationStartDistance:';
    function attenuationStartDistance: CGFloat; message 'attenuationStartDistance';
    procedure setAttenuationEndDistance(newValue: CGFloat); message 'setAttenuationEndDistance:';
    function attenuationEndDistance: CGFloat; message 'attenuationEndDistance';
    procedure setAttenuationFalloffExponent(newValue: CGFloat); message 'setAttenuationFalloffExponent:';
    function attenuationFalloffExponent: CGFloat; message 'attenuationFalloffExponent';
    procedure setSpotInnerAngle(newValue: CGFloat); message 'setSpotInnerAngle:';
    function spotInnerAngle: CGFloat; message 'spotInnerAngle';
    procedure setSpotOuterAngle(newValue: CGFloat); message 'setSpotOuterAngle:';
    function spotOuterAngle: CGFloat; message 'spotOuterAngle';
    function gobo: SCNMaterialProperty; message 'gobo';
    procedure setCategoryBitMask(newValue: NSUInteger); message 'setCategoryBitMask:';
    function categoryBitMask: NSUInteger; message 'categoryBitMask';
    function attributeForKey (key: NSString): id; message 'attributeForKey:'; deprecated 'in 10_8, 10_10, NA, NA';
    procedure setAttribute_forKey (attribute: id; key: NSString); message 'setAttribute:forKey:'; deprecated 'in 10_8, 10_10, NA, NA';

    { Adopted protocols }
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
    procedure setTechnique(newValue: SCNTechnique); message 'setTechnique:';
    function technique: SCNTechnique; message 'technique';
  end;
{$endif}

