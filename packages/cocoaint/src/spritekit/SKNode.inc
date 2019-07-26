{ Parsed from SpriteKit.framework SKNode.h }


{ Types from SKNode }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  SKNodePtr = ^SKNode;
{$endif}

{$ifdef TYPES}
type
  SKBlendMode = NSInteger;
  SKBlendModePtr = ^SKBlendMode;

const
  SKBlendModeAlpha = 0;
  SKBlendModeAdd = 1;
  SKBlendModeSubtract = 2;
  SKBlendModeMultiply = 3;
  SKBlendModeMultiplyX2 = 4;
  SKBlendModeScreen = 5;
  SKBlendModeReplace = 6;
{$endif}

{$ifdef CLASSES}

type
  SKNode = objcclass external (NSResponder, NSCopyingProtocol, NSCodingProtocol)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    class function node: instancetype; message 'node';
    class function nodeWithFileNamed (filename: NSString): instancetype; message 'nodeWithFileNamed:';
    function frame: CGRect; message 'frame';
    function calculateAccumulatedFrame: CGRect; message 'calculateAccumulatedFrame';
    procedure setPosition(newValue: CGPoint); message 'setPosition:';
    function position: CGPoint; message 'position';
    procedure setZPosition(newValue: CGFloat); message 'setZPosition:';
    function zPosition: CGFloat; message 'zPosition';
    procedure setZRotation(newValue: CGFloat); message 'setZRotation:';
    function zRotation: CGFloat; message 'zRotation';
    procedure setXScale(newValue: CGFloat); message 'setXScale:';
    function xScale: CGFloat; message 'xScale';
    procedure setYScale(newValue: CGFloat); message 'setYScale:';
    function yScale: CGFloat; message 'yScale';
    procedure setSpeed(newValue: CGFloat); message 'setSpeed:';
    function speed: CGFloat; message 'speed';
    procedure setAlpha(newValue: CGFloat); message 'setAlpha:';
    function alpha: CGFloat; message 'alpha';
    procedure setPaused(newValue: ObjCBOOL); message 'setPaused:';
    function isPaused: ObjCBOOL; message 'isPaused';
    procedure setHidden(newValue: ObjCBOOL); message 'setHidden:';
    function isHidden: ObjCBOOL; message 'isHidden';
    procedure setUserInteractionEnabled(newValue: ObjCBOOL); message 'setUserInteractionEnabled:';
    function isUserInteractionEnabled: ObjCBOOL; message 'isUserInteractionEnabled';
    function parent: SKNode; message 'parent';
    function children: NSArray; message 'children';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    function scene: SKScene; message 'scene';
    procedure setPhysicsBody(newValue: SKPhysicsBody); message 'setPhysicsBody:';
    function physicsBody: SKPhysicsBody; message 'physicsBody';
    procedure setUserData(newValue: NSMutableDictionary); message 'setUserData:';
    function userData: NSMutableDictionary; message 'userData';
    procedure setReachConstraints(newValue: SKReachConstraints); message 'setReachConstraints:';
    function reachConstraints: SKReachConstraints; message 'reachConstraints';
    procedure setConstraints(newValue: NSArray); message 'setConstraints:';
    function constraints: NSArray; message 'constraints';
    procedure setScale (scale: CGFloat); message 'setScale:';
    procedure addChild (node_: SKNode); message 'addChild:';
    procedure insertChild_atIndex (node_: SKNode; index: NSInteger); message 'insertChild:atIndex:';
    procedure removeChildrenInArray (nodes: NSArray); message 'removeChildrenInArray:';
    procedure removeAllChildren; message 'removeAllChildren';
    procedure removeFromParent; message 'removeFromParent';
    function childNodeWithName (name_: NSString): SKNode; message 'childNodeWithName:';
    procedure enumerateChildNodesWithName_usingBlock (name_: NSString; block: OpaqueCBlock); message 'enumerateChildNodesWithName:usingBlock:';
    function objectForKeyedSubscript (name_: NSString): NSArray; message 'objectForKeyedSubscript:'; { available in 10_10, 8_0 }
    function inParentHierarchy (parent_: SKNode): ObjCBOOL; message 'inParentHierarchy:';
    procedure runAction (action: SKAction); message 'runAction:';
    procedure runAction_completion (action: SKAction; block: OpaqueCBlock); message 'runAction:completion:';
    procedure runAction_withKey (action: SKAction; key: NSString); message 'runAction:withKey:';
    function hasActions: ObjCBOOL; message 'hasActions';
    function actionForKey (key: NSString): SKAction; message 'actionForKey:';
    procedure removeActionForKey (key: NSString); message 'removeActionForKey:';
    procedure removeAllActions; message 'removeAllActions';
    function containsPoint (p: CGPoint): ObjCBOOL; message 'containsPoint:';
    function nodeAtPoint (p: CGPoint): SKNode; message 'nodeAtPoint:';
    function nodesAtPoint (p: CGPoint): NSArray; message 'nodesAtPoint:';
    function convertPoint_fromNode (point: CGPoint; node_: SKNode): CGPoint; message 'convertPoint:fromNode:';
    function convertPoint_toNode (point: CGPoint; node_: SKNode): CGPoint; message 'convertPoint:toNode:';
    function intersectsNode (node_: SKNode): ObjCBOOL; message 'intersectsNode:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;
{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef CLASSES}

type
  SKNodeTouches = objccategory external (UITouch)
    function locationInNode (node: SKNode): CGPoint; message 'locationInNode:';
    function previousLocationInNode (node: SKNode): CGPoint; message 'previousLocationInNode:';
  end;
{$endif}

{$else}
{$ifdef CLASSES}

type
  SKNodeEvent = objccategory external (NSEvent)
    function locationInNode (node: SKNode): CGPoint; message 'locationInNode:';
  end;
{$endif}

{$endif}
