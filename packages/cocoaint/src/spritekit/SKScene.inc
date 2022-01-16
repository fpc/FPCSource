{ Parsed from SpriteKit.framework SKScene.h }


{$ifdef TYPES}
type
  SKScenePtr = ^SKScene;
  SKSceneDelegateProtocolPtr = ^SKSceneDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  SKSceneScaleMode = NSInteger;
  SKSceneScaleModePtr = ^SKSceneScaleMode;

const
  SKSceneScaleModeFill = 0;
  SKSceneScaleModeAspectFill = 1;
  SKSceneScaleModeAspectFit = 2;
  SKSceneScaleModeResizeFill = 3;
{$endif}

{$ifdef PROTOCOLS}

type
  SKSceneDelegateProtocol = objcprotocol external name 'SKSceneDelegate' (NSObjectProtocol)
  optional
    procedure update_forScene (currentTime: NSTimeInterval; scene: SKScene); message 'update:forScene:';
    procedure didEvaluateActionsForScene (scene: SKScene); message 'didEvaluateActionsForScene:';
    procedure didSimulatePhysicsForScene (scene: SKScene); message 'didSimulatePhysicsForScene:';
    procedure didApplyConstraintsForScene (scene: SKScene); message 'didApplyConstraintsForScene:';
    procedure didFinishUpdateForScene (scene: SKScene); message 'didFinishUpdateForScene:';
  end;
{$endif}

{$ifdef CLASSES}

type
  SKScene = objcclass external (SKEffectNode)
  public
    function initWithSize (size: CGSize): instancetype; message 'initWithSize:';
    class function sceneWithSize (size: CGSize): instancetype; message 'sceneWithSize:';
    procedure setSize(newValue: CGSize); message 'setSize:';
    function size: CGSize; message 'size';
    procedure setScaleMode(newValue: SKSceneScaleMode); message 'setScaleMode:';
    function scaleMode: SKSceneScaleMode; message 'scaleMode';
    procedure setBackgroundColor(newValue: SKColorPtr); message 'setBackgroundColor:';
    function backgroundColor: SKColorPtr; message 'backgroundColor';
    procedure setDelegate(newValue: SKSceneDelegateProtocol); message 'setDelegate:';
    function delegate: SKSceneDelegateProtocol; message 'delegate';
    procedure setAnchorPoint(newValue: CGPoint); message 'setAnchorPoint:';
    function anchorPoint: CGPoint; message 'anchorPoint';
    function physicsWorld: SKPhysicsWorld; message 'physicsWorld';
    function convertPointFromView (point: CGPoint): CGPoint; message 'convertPointFromView:';
    function convertPointToView (point: CGPoint): CGPoint; message 'convertPointToView:';
    function view: SKView; message 'view';
    procedure update (currentTime: NSTimeInterval); message 'update:';
    procedure didEvaluateActions; message 'didEvaluateActions';
    procedure didSimulatePhysics; message 'didSimulatePhysics';
    procedure didApplyConstraints; message 'didApplyConstraints'; { available in 10_10, 8_0 }
    procedure didFinishUpdate; message 'didFinishUpdate'; { available in 10_10, 8_0 }
    procedure didMoveToView (view_: SKView); message 'didMoveToView:';
    procedure willMoveFromView (view_: SKView); message 'willMoveFromView:';
    procedure didChangeSize (oldSize: CGSize); message 'didChangeSize:';
  end;
{$endif}

