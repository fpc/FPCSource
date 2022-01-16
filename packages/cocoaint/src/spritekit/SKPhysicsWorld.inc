{ Parsed from SpriteKit.framework SKPhysicsWorld.h }


{$ifdef TYPES}
type
  SKPhysicsWorldPtr = ^SKPhysicsWorld;
  SKPhysicsContactDelegateProtocolPtr = ^SKPhysicsContactDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  SKPhysicsContactDelegateProtocol = objcprotocol external name 'SKPhysicsContactDelegate' (NSObjectProtocol)
  optional
    procedure didBeginContact (contact: SKPhysicsContact); message 'didBeginContact:';
    procedure didEndContact (contact: SKPhysicsContact); message 'didEndContact:';
  end;
{$endif}

{$ifdef CLASSES}

type
  SKPhysicsWorld = objcclass external (NSObject, NSCodingProtocol)
  public
    procedure setGravity(newValue: CGVector); message 'setGravity:';
    function gravity: CGVector; message 'gravity';
    procedure setSpeed(newValue: CGFloat); message 'setSpeed:';
    function speed: CGFloat; message 'speed';
    procedure setContactDelegate(newValue: SKPhysicsContactDelegateProtocol); message 'setContactDelegate:';
    function contactDelegate: SKPhysicsContactDelegateProtocol; message 'contactDelegate';
    procedure addJoint (joint: SKPhysicsJoint); message 'addJoint:';
    procedure removeJoint (joint: SKPhysicsJoint); message 'removeJoint:';
    procedure removeAllJoints; message 'removeAllJoints';
    function sampleFieldsAt (position: vector_float3): vector_float3; message 'sampleFieldsAt:'; { available in 10_10, 8_0 }
    function bodyAtPoint (point: CGPoint): SKPhysicsBody; message 'bodyAtPoint:';
    function bodyInRect (rect: CGRect): SKPhysicsBody; message 'bodyInRect:';
    function bodyAlongRayStart_end (start: CGPoint; end_: CGPoint): SKPhysicsBody; message 'bodyAlongRayStart:end:';
    procedure enumerateBodiesAtPoint_usingBlock (point: CGPoint; block: OpaqueCBlock); message 'enumerateBodiesAtPoint:usingBlock:';
    procedure enumerateBodiesInRect_usingBlock (rect: CGRect; block: OpaqueCBlock); message 'enumerateBodiesInRect:usingBlock:';
    procedure enumerateBodiesAlongRayStart_end_usingBlock (start: CGPoint; end_: CGPoint; block: OpaqueCBlock); message 'enumerateBodiesAlongRayStart:end:usingBlock:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

