{ Parsed from SceneKit.framework SCNCamera.h }


{$ifdef TYPES}
type
  SCNCameraPtr = ^SCNCamera;
{$endif}

{$ifdef CLASSES}

type
  SCNCamera = objcclass external (NSObject, SCNAnimatableProtocol, SCNTechniqueSupportProtocol, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function camera: instancetype; message 'camera';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setXFov(newValue: double); message 'setXFov:';
    function xFov: double; message 'xFov';
    procedure setYFov(newValue: double); message 'setYFov:';
    function yFov: double; message 'yFov';
    procedure setZNear(newValue: double); message 'setZNear:';
    function zNear: double; message 'zNear';
    procedure setZFar(newValue: double); message 'setZFar:';
    function zFar: double; message 'zFar';
    procedure setAutomaticallyAdjustsZRange(newValue: ObjCBOOL); message 'setAutomaticallyAdjustsZRange:';
    function automaticallyAdjustsZRange: ObjCBOOL; message 'automaticallyAdjustsZRange';
    procedure setUsesOrthographicProjection(newValue: ObjCBOOL); message 'setUsesOrthographicProjection:';
    function usesOrthographicProjection: ObjCBOOL; message 'usesOrthographicProjection';
    procedure setOrthographicScale(newValue: double); message 'setOrthographicScale:';
    function orthographicScale: double; message 'orthographicScale';
    function projectionTransform: SCNMatrix4; message 'projectionTransform';
    procedure setProjectionTransform (projectionTransform_: SCNMatrix4); message 'setProjectionTransform:'; { available in 10_9, 8_0 }
    procedure setFocalDistance(newValue: CGFloat); message 'setFocalDistance:';
    function focalDistance: CGFloat; message 'focalDistance';
    procedure setFocalSize(newValue: CGFloat); message 'setFocalSize:';
    function focalSize: CGFloat; message 'focalSize';
    procedure setFocalBlurRadius(newValue: CGFloat); message 'setFocalBlurRadius:';
    function focalBlurRadius: CGFloat; message 'focalBlurRadius';
    procedure setAperture(newValue: CGFloat); message 'setAperture:';
    function aperture: CGFloat; message 'aperture';
    procedure setCategoryBitMask(newValue: NSUInteger); message 'setCategoryBitMask:';
    function categoryBitMask: NSUInteger; message 'categoryBitMask';

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

