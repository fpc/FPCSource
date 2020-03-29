{ Parsed from SceneKit.framework SCNConstraint.h }


{$ifdef TYPES}
type
  SCNConstraintPtr = ^SCNConstraint;
  SCNLookAtConstraintPtr = ^SCNLookAtConstraint;
  SCNTransformConstraintPtr = ^SCNTransformConstraint;
  SCNIKConstraintPtr = ^SCNIKConstraint;
{$endif}

{$ifdef CLASSES}

type
  SCNConstraint = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol, SCNAnimatableProtocol)
  private
    _constraintReserved: id;
  public
    procedure setInfluenceFactor(newValue: CGFloat); message 'setInfluenceFactor:';
    function influenceFactor: CGFloat; message 'influenceFactor';

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
  end;

type
  SCNLookAtConstraint = objcclass external (SCNConstraint)
  private
    _reserved: id;
  public
    class function lookAtConstraintWithTarget (target: SCNNode): instancetype; message 'lookAtConstraintWithTarget:';
    function target: SCNNode; message 'target';
    procedure setGimbalLockEnabled(newValue: ObjCBOOL); message 'setGimbalLockEnabled:';
    function gimbalLockEnabled: ObjCBOOL; message 'gimbalLockEnabled';
  end;

type
  SCNTransformConstraint = objcclass external (SCNConstraint)
  private
    _reserved: id;
  public
    class function transformConstraintInWorldSpace_withBlock (world: ObjCBOOL; block: OpaqueCBlock): instancetype; message 'transformConstraintInWorldSpace:withBlock:';
  end;

type
  SCNIKConstraint = objcclass external (SCNConstraint)
  private
    _reserved: id;
  public
    class function inverseKinematicsConstraintWithChainRootNode (chainRootNode: SCNNode): instancetype; message 'inverseKinematicsConstraintWithChainRootNode:';
    function chainRootNode: SCNNode; message 'chainRootNode';
    procedure setTargetPosition(newValue: SCNVector3); message 'setTargetPosition:';
    function targetPosition: SCNVector3; message 'targetPosition';
    procedure setMaxAllowedRotationAngle_forJoint (angle: CGFloat; node: SCNNode); message 'setMaxAllowedRotationAngle:forJoint:';
    function maxAllowedRotationAngleForJoint (node: SCNNode): CGFloat; message 'maxAllowedRotationAngleForJoint:';
  end;
{$endif}

