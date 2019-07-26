{ Parsed from SpriteKit.framework SKAction.h }


{$ifdef TYPES}
type
  SKActionPtr = ^SKAction;
{$endif}

{$ifdef TYPES}
type
  SKActionTimingMode = NSInteger;
  SKActionTimingModePtr = ^SKActionTimingMode;

const
  SKActionTimingLinear = 0;
  SKActionTimingEaseIn = 1;
  SKActionTimingEaseOut = 2;
  SKActionTimingEaseInEaseOut = 3;

type
  SKActionTimingFunction = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  SKAction = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    procedure setDuration(newValue: NSTimeInterval); message 'setDuration:';
    function duration: NSTimeInterval; message 'duration';
    procedure setTimingMode(newValue: SKActionTimingMode); message 'setTimingMode:';
    function timingMode: SKActionTimingMode; message 'timingMode';
    procedure setTimingFunction(newValue: SKActionTimingFunction); message 'setTimingFunction:';
    function timingFunction: SKActionTimingFunction; message 'timingFunction';
    procedure setSpeed(newValue: CGFloat); message 'setSpeed:';
    function speed: CGFloat; message 'speed';
    function reversedAction: SKAction; message 'reversedAction';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  SKActions = objccategory external (SKAction)
    class function moveBy_duration (delta: CGVector; sec: NSTimeInterval): SKAction; message 'moveBy:duration:';
    class function moveByX_y_duration (deltaX: CGFloat; deltaY: CGFloat; sec: NSTimeInterval): SKAction; message 'moveByX:y:duration:';
    class function moveTo_duration (location: CGPoint; sec: NSTimeInterval): SKAction; message 'moveTo:duration:';
    class function moveToX_duration (x: CGFloat; sec: NSTimeInterval): SKAction; message 'moveToX:duration:';
    class function moveToY_duration (y: CGFloat; sec: NSTimeInterval): SKAction; message 'moveToY:duration:';
    class function rotateByAngle_duration (radians: CGFloat; sec: NSTimeInterval): SKAction; message 'rotateByAngle:duration:';
    class function rotateToAngle_duration (radians: CGFloat; sec: NSTimeInterval): SKAction; message 'rotateToAngle:duration:';
    class function rotateToAngle_duration_shortestUnitArc (radians: CGFloat; sec: NSTimeInterval; shortestUnitArc: ObjCBOOL): SKAction; message 'rotateToAngle:duration:shortestUnitArc:';
    class function resizeByWidth_height_duration (width: CGFloat; height: CGFloat; duration_: NSTimeInterval): SKAction; message 'resizeByWidth:height:duration:';
    class function resizeToWidth_height_duration (width: CGFloat; height: CGFloat; duration_: NSTimeInterval): SKAction; message 'resizeToWidth:height:duration:';
    class function resizeToWidth_duration (width: CGFloat; duration_: NSTimeInterval): SKAction; message 'resizeToWidth:duration:';
    class function resizeToHeight_duration (height: CGFloat; duration_: NSTimeInterval): SKAction; message 'resizeToHeight:duration:';
    class function scaleBy_duration (scale: CGFloat; sec: NSTimeInterval): SKAction; message 'scaleBy:duration:';
    class function scaleXBy_y_duration (xScale: CGFloat; yScale: CGFloat; sec: NSTimeInterval): SKAction; message 'scaleXBy:y:duration:';
    class function scaleTo_duration (scale: CGFloat; sec: NSTimeInterval): SKAction; message 'scaleTo:duration:';
    class function scaleXTo_y_duration (xScale: CGFloat; yScale: CGFloat; sec: NSTimeInterval): SKAction; message 'scaleXTo:y:duration:';
    class function scaleXTo_duration (scale: CGFloat; sec: NSTimeInterval): SKAction; message 'scaleXTo:duration:';
    class function scaleYTo_duration (scale: CGFloat; sec: NSTimeInterval): SKAction; message 'scaleYTo:duration:';
    class function sequence (actions: NSArray): SKAction; message 'sequence:';
    class function group (actions: NSArray): SKAction; message 'group:';
    class function repeatAction_count (action: SKAction; count: NSUInteger): SKAction; message 'repeatAction:count:';
    class function repeatActionForever (action: SKAction): SKAction; message 'repeatActionForever:';
    class function fadeInWithDuration (sec: NSTimeInterval): SKAction; message 'fadeInWithDuration:';
    class function fadeOutWithDuration (sec: NSTimeInterval): SKAction; message 'fadeOutWithDuration:';
    class function fadeAlphaBy_duration (factor: CGFloat; sec: NSTimeInterval): SKAction; message 'fadeAlphaBy:duration:';
    class function fadeAlphaTo_duration (alpha: CGFloat; sec: NSTimeInterval): SKAction; message 'fadeAlphaTo:duration:';
    class function hide: SKAction; message 'hide'; { available in 10_10, 8_0 }
    class function unhide: SKAction; message 'unhide'; { available in 10_10, 8_0 }
    class function setTexture (texture: SKTexture): SKAction; message 'setTexture:';
    class function setTexture_resize (texture: SKTexture; resize: ObjCBOOL): SKAction; message 'setTexture:resize:';
    class function animateWithTextures_timePerFrame (textures: NSArray; sec: NSTimeInterval): SKAction; message 'animateWithTextures:timePerFrame:';
    class function animateWithTextures_timePerFrame_resize_restore (textures: NSArray; sec: NSTimeInterval; resize: ObjCBOOL; restore: ObjCBOOL): SKAction; message 'animateWithTextures:timePerFrame:resize:restore:';
    class function playSoundFileNamed_waitForCompletion (soundFile: NSString; wait: ObjCBOOL): SKAction; message 'playSoundFileNamed:waitForCompletion:';
    class function colorizeWithColor_colorBlendFactor_duration (color: SKColorPtr; colorBlendFactor: CGFloat; sec: NSTimeInterval): SKAction; message 'colorizeWithColor:colorBlendFactor:duration:';
    class function colorizeWithColorBlendFactor_duration (colorBlendFactor: CGFloat; sec: NSTimeInterval): SKAction; message 'colorizeWithColorBlendFactor:duration:';
    class function falloffTo_duration (falloff: single; sec: NSTimeInterval): SKAction; message 'falloffTo:duration:'; { available in 10_10, 8_0 }
    class function falloffBy_duration (falloff: single; sec: NSTimeInterval): SKAction; message 'falloffBy:duration:'; { available in 10_10, 8_0 }
    class function followPath_duration (path: CGPathRef; sec: NSTimeInterval): SKAction; message 'followPath:duration:';
    class function followPath_asOffset_orientToPath_duration (path: CGPathRef; offset: ObjCBOOL; orient: ObjCBOOL; sec: NSTimeInterval): SKAction; message 'followPath:asOffset:orientToPath:duration:';
    class function followPath_speed (path: CGPathRef; speed_: CGFloat): SKAction; message 'followPath:speed:';
    class function followPath_asOffset_orientToPath_speed (path: CGPathRef; offset: ObjCBOOL; orient: ObjCBOOL; speed_: CGFloat): SKAction; message 'followPath:asOffset:orientToPath:speed:';
    class function speedBy_duration (speed_: CGFloat; sec: NSTimeInterval): SKAction; message 'speedBy:duration:';
    class function speedTo_duration (speed_: CGFloat; sec: NSTimeInterval): SKAction; message 'speedTo:duration:';
    class function reachTo_rootNode_duration (position: CGPoint; root: SKNode; sec: NSTimeInterval): SKAction; message 'reachTo:rootNode:duration:'; { available in 10_10, 8_0 }
    class function reachTo_rootNode_velocity (position: CGPoint; root: SKNode; velocity: CGFloat): SKAction; message 'reachTo:rootNode:velocity:'; { available in 10_10, 8_0 }
    class function reachToNode_rootNode_duration (node: SKNode; root: SKNode; sec: NSTimeInterval): SKAction; message 'reachToNode:rootNode:duration:'; { available in 10_10, 8_0 }
    class function reachToNode_rootNode_velocity (node: SKNode; root: SKNode; velocity: CGFloat): SKAction; message 'reachToNode:rootNode:velocity:'; { available in 10_10, 8_0 }
    class function strengthTo_duration (strength: single; sec: NSTimeInterval): SKAction; message 'strengthTo:duration:';
    class function strengthBy_duration (strength: single; sec: NSTimeInterval): SKAction; message 'strengthBy:duration:';
    class function waitForDuration (sec: NSTimeInterval): SKAction; message 'waitForDuration:';
    class function waitForDuration_withRange (sec: NSTimeInterval; durationRange: NSTimeInterval): SKAction; message 'waitForDuration:withRange:';
    class function removeFromParent: SKAction; message 'removeFromParent';
    class function performSelector_onTarget (selector: SEL; target: id): SKAction; message 'performSelector:onTarget:';
    class function runBlock (block: dispatch_block_t): SKAction; message 'runBlock:';
    class function runBlock_queue (block: dispatch_block_t; queue: dispatch_queue_t): SKAction; message 'runBlock:queue:';
    class function runAction_onChildWithName (action: SKAction; name: NSString): SKAction; message 'runAction:onChildWithName:';
    class function customActionWithDuration_actionBlock (seconds: NSTimeInterval; block: OpaqueCBlock): SKAction; message 'customActionWithDuration:actionBlock:';
  end;
{$endif}

