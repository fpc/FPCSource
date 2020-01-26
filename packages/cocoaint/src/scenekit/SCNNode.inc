{ Parsed from SceneKit.framework SCNNode.h }


{$ifdef TYPES}
type
  SCNNodePtr = ^SCNNode;
  SCNNodeRendererDelegateProtocolPtr = ^SCNNodeRendererDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNModelTransform: NSString; cvar; external;
  SCNViewTransform: NSString; cvar; external;
  SCNProjectionTransform: NSString; cvar; external;
  SCNNormalTransform: NSString; cvar; external;
  SCNModelViewTransform: NSString; cvar; external;
  SCNModelViewProjectionTransform: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  SCNNode = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol, SCNAnimatableProtocol, SCNActionableProtocol, SCNBoundingVolumeProtocol)
  private
    _reserved: id;
  public
    class function node: instancetype; message 'node';
    class function nodeWithGeometry (geometry: SCNGeometry): SCNNode; message 'nodeWithGeometry:';
    function clone: id; message 'clone';
    function flattenedClone: SCNNode; message 'flattenedClone'; { available in 10_9, 8_0 }
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setLight(newValue: SCNLight); message 'setLight:';
    function light: SCNLight; message 'light';
    procedure setCamera(newValue: SCNCamera); message 'setCamera:';
    function camera: SCNCamera; message 'camera';
    procedure setGeometry(newValue: SCNGeometry); message 'setGeometry:';
    function geometry: SCNGeometry; message 'geometry';
    procedure setSkinner(newValue: SCNSkinner); message 'setSkinner:';
    function skinner: SCNSkinner; message 'skinner';
    procedure setMorpher(newValue: SCNMorpher); message 'setMorpher:';
    function morpher: SCNMorpher; message 'morpher';
    procedure setTransform(newValue: SCNMatrix4); message 'setTransform:';
    function transform: SCNMatrix4; message 'transform';
    procedure setPosition(newValue: SCNVector3); message 'setPosition:';
    function position: SCNVector3; message 'position';
    procedure setRotation(newValue: SCNVector4); message 'setRotation:';
    function rotation: SCNVector4; message 'rotation';
    procedure setOrientation(newValue: SCNQuaternion); message 'setOrientation:';
    function orientation: SCNQuaternion; message 'orientation';
    procedure setEulerAngles(newValue: SCNVector3); message 'setEulerAngles:';
    function eulerAngles: SCNVector3; message 'eulerAngles';
    procedure setScale(newValue: SCNVector3); message 'setScale:';
    function scale: SCNVector3; message 'scale';
    procedure setPivot(newValue: SCNMatrix4); message 'setPivot:';
    function pivot: SCNMatrix4; message 'pivot';
    function worldTransform: SCNMatrix4; message 'worldTransform';
    procedure setHidden(newValue: ObjCBOOL); message 'setHidden:';
    function isHidden: ObjCBOOL; message 'isHidden';
    procedure setOpacity(newValue: CGFloat); message 'setOpacity:';
    function opacity: CGFloat; message 'opacity';
    procedure setRenderingOrder(newValue: NSInteger); message 'setRenderingOrder:';
    function renderingOrder: NSInteger; message 'renderingOrder';
    procedure setCastsShadow(newValue: ObjCBOOL); message 'setCastsShadow:';
    function castsShadow: ObjCBOOL; message 'castsShadow';
    function parentNode: SCNNode; message 'parentNode';
    function childNodes: NSArray; message 'childNodes';
    procedure addChildNode (child: SCNNode); message 'addChildNode:';
    procedure insertChildNode_atIndex (child: SCNNode; index: NSUInteger); message 'insertChildNode:atIndex:';
    procedure removeFromParentNode; message 'removeFromParentNode';
    procedure replaceChildNode_with (oldChild: SCNNode; newChild: SCNNode); message 'replaceChildNode:with:';
    function childNodeWithName_recursively (name_: NSString; recursively: ObjCBOOL): SCNNode; message 'childNodeWithName:recursively:';
    function childNodesPassingTest (predicate: OpaqueCBlock): NSArray; message 'childNodesPassingTest:';
    procedure enumerateChildNodesUsingBlock (block: OpaqueCBlock); message 'enumerateChildNodesUsingBlock:'; { available in 10_10, 8_0 }
    function convertPosition_toNode (position_: SCNVector3; node_: SCNNode): SCNVector3; message 'convertPosition:toNode:'; { available in 10_9, 8_0 }
    function convertPosition_fromNode (position_: SCNVector3; node_: SCNNode): SCNVector3; message 'convertPosition:fromNode:'; { available in 10_9, 8_0 }
    function convertTransform_toNode (transform_: SCNMatrix4; node_: SCNNode): SCNMatrix4; message 'convertTransform:toNode:'; { available in 10_9, 8_0 }
    function convertTransform_fromNode (transform_: SCNMatrix4; node_: SCNNode): SCNMatrix4; message 'convertTransform:fromNode:'; { available in 10_9, 8_0 }
    procedure setPhysicsBody(newValue: SCNPhysicsBody); message 'setPhysicsBody:';
    function physicsBody: SCNPhysicsBody; message 'physicsBody';
    procedure setPhysicsField(newValue: SCNPhysicsField); message 'setPhysicsField:';
    function physicsField: SCNPhysicsField; message 'physicsField';
    procedure setConstraints(newValue: NSArray); message 'setConstraints:';
    function constraints: NSArray; message 'constraints';
    procedure setFilters(newValue: NSArray); message 'setFilters:';
    function filters: NSArray; message 'filters';
    function presentationNode: SCNNode; message 'presentationNode';
    procedure setPaused(newValue: ObjCBOOL); message 'setPaused:';
    function isPaused: ObjCBOOL; message 'isPaused';
    procedure setRendererDelegate(newValue: SCNNodeRendererDelegateProtocol); message 'setRendererDelegate:';
    function rendererDelegate: SCNNodeRendererDelegateProtocol; message 'rendererDelegate';
    function hitTestWithSegmentFromPoint_toPoint_options (pointA: SCNVector3; pointB: SCNVector3; options: NSDictionary): NSArray; message 'hitTestWithSegmentFromPoint:toPoint:options:'; { available in 10_9, 8_0 }
    procedure setCategoryBitMask(newValue: NSUInteger); message 'setCategoryBitMask:';
    function categoryBitMask: NSUInteger; message 'categoryBitMask';

    { Adopted protocols }
    function actionForKey (key: NSString): SCNAction; message 'actionForKey:'; { available in 10_10, 8_0 }
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function getBoundingBoxMin_max (min: SCNVector3Ptr; max: SCNVector3Ptr): ObjCBOOL; message 'getBoundingBoxMin:max:';
    function getBoundingSphereCenter_radius (center: SCNVector3Ptr; radius: CGFloatPtr): ObjCBOOL; message 'getBoundingSphereCenter:radius:';
    function hasActions: ObjCBOOL; message 'hasActions'; { available in 10_10, 8_0 }
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure removeActionForKey (key: NSString); message 'removeActionForKey:'; { available in 10_10, 8_0 }
    procedure removeAllActions; message 'removeAllActions'; { available in 10_10, 8_0 }
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    procedure runAction (action: SCNAction); message 'runAction:'; { available in 10_10, 8_0 }
    procedure runAction_completionHandler (action: SCNAction; block: OpaqueCBlock); message 'runAction:completionHandler:'; { available in 10_10, 8_0 }
    procedure runAction_forKey (action: SCNAction; key: NSString); message 'runAction:forKey:'; { available in 10_10, 8_0 }
    procedure runAction_forKey_completionHandler (action: SCNAction; key: NSString; block: OpaqueCBlock); message 'runAction:forKey:completionHandler:'; { available in 10_10, 8_0 }
    procedure setBoundingBoxMin_max (min: SCNVector3Ptr; max: SCNVector3Ptr); message 'setBoundingBoxMin:max:'; { available in 10_9, 8_0 }
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  SCNNodeRendererDelegateProtocol = objcprotocol external name 'SCNNodeRendererDelegate' (NSObjectProtocol)
  optional
    procedure renderNode_renderer_arguments (node: SCNNode; renderer: SCNRenderer; arguments: NSDictionary); message 'renderNode:renderer:arguments:';
  end;
{$endif}

