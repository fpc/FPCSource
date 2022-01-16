{ Parsed from SpriteKit.framework SKPhysicsContact.h }


{$ifdef TYPES}
type
  SKPhysicsContactPtr = ^SKPhysicsContact;
{$endif}

{$ifdef CLASSES}

type
  SKPhysicsContact = objcclass external (NSObject)
  public
    function bodyA: SKPhysicsBody; message 'bodyA';
    function bodyB: SKPhysicsBody; message 'bodyB';
    function contactPoint: CGPoint; message 'contactPoint';
    function contactNormal: CGVector; message 'contactNormal';
    function collisionImpulse: CGFloat; message 'collisionImpulse';
  end;
{$endif}

