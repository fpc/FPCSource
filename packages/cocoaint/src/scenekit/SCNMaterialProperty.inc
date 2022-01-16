{ Parsed from SceneKit.framework SCNMaterialProperty.h }


{$ifdef TYPES}
type
  SCNMaterialPropertyPtr = ^SCNMaterialProperty;
{$endif}

{$ifdef TYPES}
type
  SCNFilterMode = NSInteger;
  SCNFilterModePtr = ^SCNFilterMode;

const
  SCNFilterModeNone = 0 { available in 10_9, 8_0 };
  SCNFilterModeNearest = 1 { available in 10_9, 8_0 };
  SCNFilterModeLinear = 2 { available in 10_9, 8_0 };
  SCNNoFiltering = SCNFilterModeNone;
  SCNNearestFiltering = SCNFilterModeNearest;
  SCNLinearFiltering = SCNFilterModeLinear;

type
  SCNWrapMode = NSInteger;
  SCNWrapModePtr = ^SCNWrapMode;

const
  SCNWrapModeClamp = 1 { available in 10_9, 8_0 };
  SCNWrapModeRepeat = 2 { available in 10_9, 8_0 };
  SCNWrapModeClampToBorder = 3 { available in 10_9, NA };
  SCNWrapModeMirror = 4 { available in 10_9, 8_0 };
  SCNClamp = SCNWrapModeClamp;
  SCNRepeat = SCNWrapModeRepeat;
  SCNClampToBorder = SCNWrapModeClampToBorder;
  SCNMirror = SCNWrapModeMirror;
{$endif}

{$ifdef CLASSES}

type
  SCNMaterialProperty = objcclass external (NSObject, SCNAnimatableProtocol, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function materialPropertyWithContents (contents: id): instancetype; message 'materialPropertyWithContents:'; { available in 10_9, 8_0 }
    procedure setContents(newValue: id); message 'setContents:';
    function contents: id; message 'contents';
    procedure setIntensity(newValue: CGFloat); message 'setIntensity:';
    function intensity: CGFloat; message 'intensity';
    procedure setMinificationFilter(newValue: SCNFilterMode); message 'setMinificationFilter:';
    function minificationFilter: SCNFilterMode; message 'minificationFilter';
    procedure setMagnificationFilter(newValue: SCNFilterMode); message 'setMagnificationFilter:';
    function magnificationFilter: SCNFilterMode; message 'magnificationFilter';
    procedure setMipFilter(newValue: SCNFilterMode); message 'setMipFilter:';
    function mipFilter: SCNFilterMode; message 'mipFilter';
    procedure setContentsTransform(newValue: SCNMatrix4); message 'setContentsTransform:';
    function contentsTransform: SCNMatrix4; message 'contentsTransform';
    procedure setWrapS(newValue: SCNWrapMode); message 'setWrapS:';
    function wrapS: SCNWrapMode; message 'wrapS';
    procedure setWrapT(newValue: SCNWrapMode); message 'setWrapT:';
    function wrapT: SCNWrapMode; message 'wrapT';
    procedure setBorderColor(newValue: id); message 'setBorderColor:';
    function borderColor: id; message 'borderColor';
    procedure setMappingChannel(newValue: NSInteger); message 'setMappingChannel:';
    function mappingChannel: NSInteger; message 'mappingChannel';
    procedure setMaxAnisotropy(newValue: CGFloat); message 'setMaxAnisotropy:';
    function maxAnisotropy: CGFloat; message 'maxAnisotropy';

    { Adopted protocols }
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

