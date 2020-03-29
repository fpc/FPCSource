{ Parsed from SceneKit.framework SCNPhysicsWorld.h }


{$ifdef TYPES}
type
  SCNPhysicsWorldPtr = ^SCNPhysicsWorld;
  SCNPhysicsContactDelegateProtocolPtr = ^SCNPhysicsContactDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNPhysicsTestCollisionBitMaskKey: NSString; cvar; external;
  SCNPhysicsTestSearchModeKey: NSString; cvar; external;
  SCNPhysicsTestBackfaceCullingKey: NSString; cvar; external;
  SCNPhysicsTestSearchModeAny: NSString; cvar; external;
  SCNPhysicsTestSearchModeClosest: NSString; cvar; external;
  SCNPhysicsTestSearchModeAll: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  SCNPhysicsContactDelegateProtocol = objcprotocol external name 'SCNPhysicsContactDelegate' (NSObjectProtocol)
  optional
    procedure physicsWorld_didBeginContact (world: SCNPhysicsWorld; contact: SCNPhysicsContact); message 'physicsWorld:didBeginContact:';
    procedure physicsWorld_didUpdateContact (world: SCNPhysicsWorld; contact: SCNPhysicsContact); message 'physicsWorld:didUpdateContact:';
    procedure physicsWorld_didEndContact (world: SCNPhysicsWorld; contact: SCNPhysicsContact); message 'physicsWorld:didEndContact:';
  end;
{$endif}

{$ifdef CLASSES}

type
  SCNPhysicsWorld = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    procedure setGravity(newValue: SCNVector3); message 'setGravity:';
    function gravity: SCNVector3; message 'gravity';
    procedure setSpeed(newValue: CGFloat); message 'setSpeed:';
    function speed: CGFloat; message 'speed';
    procedure setTimeStep(newValue: NSTimeInterval); message 'setTimeStep:';
    function timeStep: NSTimeInterval; message 'timeStep';
    procedure setContactDelegate(newValue: SCNPhysicsContactDelegateProtocol); message 'setContactDelegate:';
    function contactDelegate: SCNPhysicsContactDelegateProtocol; message 'contactDelegate';
    procedure addBehavior (behavior: SCNPhysicsBehavior); message 'addBehavior:';
    procedure removeBehavior (behavior: SCNPhysicsBehavior); message 'removeBehavior:';
    procedure removeAllBehaviors; message 'removeAllBehaviors';
    function allBehaviors: NSArray; message 'allBehaviors';
    function rayTestWithSegmentFromPoint_toPoint_options (origin: SCNVector3; dest: SCNVector3; options: NSDictionary): NSArray; message 'rayTestWithSegmentFromPoint:toPoint:options:';
    function contactTestBetweenBody_andBody_options (bodyA: SCNPhysicsBody; bodyB: SCNPhysicsBody; options: NSDictionary): NSArray; message 'contactTestBetweenBody:andBody:options:';
    function contactTestWithBody_options (body: SCNPhysicsBody; options: NSDictionary): NSArray; message 'contactTestWithBody:options:';
    function convexSweepTestWithShape_fromTransform_toTransform_options (shape: SCNPhysicsShape; from: SCNMatrix4; to_: SCNMatrix4; options: NSDictionary): NSArray; message 'convexSweepTestWithShape:fromTransform:toTransform:options:';
    procedure updateCollisionPairs; message 'updateCollisionPairs';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

