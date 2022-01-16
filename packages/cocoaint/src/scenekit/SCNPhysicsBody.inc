{ Parsed from SceneKit.framework SCNPhysicsBody.h }


{$ifdef TYPES}
type
  SCNPhysicsBodyPtr = ^SCNPhysicsBody;
{$endif}

{$ifdef TYPES}
type
  SCNPhysicsBodyType = NSInteger;
  SCNPhysicsBodyTypePtr = ^SCNPhysicsBodyType;

const
  SCNPhysicsBodyTypeStatic = 0;
  SCNPhysicsBodyTypeDynamic = 1;
  SCNPhysicsBodyTypeKinematic = 2;

type
  SCNPhysicsCollisionCategory = NSUInteger;
  SCNPhysicsCollisionCategoryPtr = ^SCNPhysicsCollisionCategory;

const
  SCNPhysicsCollisionCategoryDefault = 1 shl 0;
  SCNPhysicsCollisionCategoryStatic = 1 shl 1;
  SCNPhysicsCollisionCategoryAll = 0;
{$endif}

{$ifdef CLASSES}

type
  SCNPhysicsBody = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function staticBody: instancetype; message 'staticBody';
    class function dynamicBody: instancetype; message 'dynamicBody';
    class function kinematicBody: instancetype; message 'kinematicBody';
    class function bodyWithType_shape (type_: SCNPhysicsBodyType; shape: SCNPhysicsShape): instancetype; message 'bodyWithType:shape:';
    procedure setType(newValue: SCNPhysicsBodyType); message 'setType:';
    function type_: SCNPhysicsBodyType; message 'type';
    procedure setMass(newValue: CGFloat); message 'setMass:';
    function mass: CGFloat; message 'mass';
    procedure setCharge(newValue: CGFloat); message 'setCharge:';
    function charge: CGFloat; message 'charge';
    procedure setFriction(newValue: CGFloat); message 'setFriction:';
    function friction: CGFloat; message 'friction';
    procedure setRestitution(newValue: CGFloat); message 'setRestitution:';
    function restitution: CGFloat; message 'restitution';
    procedure setRollingFriction(newValue: CGFloat); message 'setRollingFriction:';
    function rollingFriction: CGFloat; message 'rollingFriction';
    procedure setPhysicsShape(newValue: SCNPhysicsShape); message 'setPhysicsShape:';
    function physicsShape: SCNPhysicsShape; message 'physicsShape';
    function isResting: ObjCBOOL; message 'isResting';
    procedure setAllowsResting(newValue: ObjCBOOL); message 'setAllowsResting:';
    function allowsResting: ObjCBOOL; message 'allowsResting';
    procedure setVelocity(newValue: SCNVector3); message 'setVelocity:';
    function velocity: SCNVector3; message 'velocity';
    procedure setAngularVelocity(newValue: SCNVector4); message 'setAngularVelocity:';
    function angularVelocity: SCNVector4; message 'angularVelocity';
    procedure setDamping(newValue: CGFloat); message 'setDamping:';
    function damping: CGFloat; message 'damping';
    procedure setAngularDamping(newValue: CGFloat); message 'setAngularDamping:';
    function angularDamping: CGFloat; message 'angularDamping';
    procedure setVelocityFactor(newValue: SCNVector3); message 'setVelocityFactor:';
    function velocityFactor: SCNVector3; message 'velocityFactor';
    procedure setAngularVelocityFactor(newValue: SCNVector3); message 'setAngularVelocityFactor:';
    function angularVelocityFactor: SCNVector3; message 'angularVelocityFactor';
    procedure setCategoryBitMask(newValue: NSUInteger); message 'setCategoryBitMask:';
    function categoryBitMask: NSUInteger; message 'categoryBitMask';
    procedure setCollisionBitMask(newValue: NSUInteger); message 'setCollisionBitMask:';
    function collisionBitMask: NSUInteger; message 'collisionBitMask';
    procedure applyForce_impulse (direction: SCNVector3; impulse: ObjCBOOL); message 'applyForce:impulse:';
    procedure applyForce_atPosition_impulse (direction: SCNVector3; position: SCNVector3; impulse: ObjCBOOL); message 'applyForce:atPosition:impulse:';
    procedure applyTorque_impulse (torque: SCNVector4; impulse: ObjCBOOL); message 'applyTorque:impulse:';
    procedure clearAllForces; message 'clearAllForces';
    procedure resetTransform; message 'resetTransform';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

