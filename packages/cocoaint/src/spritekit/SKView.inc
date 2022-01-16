{ Parsed from SpriteKit.framework SKView.h }


{ Types from SKView }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  SKViewPtr = ^SKView;
{$endif}

{$ifdef CLASSES}

type
  SKView = objcclass external (NSView)
  public
    procedure setPaused(newValue: ObjCBOOL); message 'setPaused:';
    function isPaused: ObjCBOOL; message 'isPaused';
    procedure setShowsFPS(newValue: ObjCBOOL); message 'setShowsFPS:';
    function showsFPS: ObjCBOOL; message 'showsFPS';
    procedure setShowsDrawCount(newValue: ObjCBOOL); message 'setShowsDrawCount:';
    function showsDrawCount: ObjCBOOL; message 'showsDrawCount';
    procedure setShowsNodeCount(newValue: ObjCBOOL); message 'setShowsNodeCount:';
    function showsNodeCount: ObjCBOOL; message 'showsNodeCount';
    procedure setShowsQuadCount(newValue: ObjCBOOL); message 'setShowsQuadCount:';
    function showsQuadCount: ObjCBOOL; message 'showsQuadCount';
    procedure setShowsPhysics(newValue: ObjCBOOL); message 'setShowsPhysics:';
    function showsPhysics: ObjCBOOL; message 'showsPhysics';
    procedure setShowsFields(newValue: ObjCBOOL); message 'setShowsFields:';
    function showsFields: ObjCBOOL; message 'showsFields';
    procedure setAsynchronous(newValue: ObjCBOOL); message 'setAsynchronous:';
    function isAsynchronous: ObjCBOOL; message 'isAsynchronous';
    procedure setAllowsTransparency(newValue: ObjCBOOL); message 'setAllowsTransparency:';
    function allowsTransparency: ObjCBOOL; message 'allowsTransparency';
    procedure setIgnoresSiblingOrder(newValue: ObjCBOOL); message 'setIgnoresSiblingOrder:';
    function ignoresSiblingOrder: ObjCBOOL; message 'ignoresSiblingOrder';
    procedure setShouldCullNonVisibleNodes(newValue: ObjCBOOL); message 'setShouldCullNonVisibleNodes:';
    function shouldCullNonVisibleNodes: ObjCBOOL; message 'shouldCullNonVisibleNodes';
    procedure setFrameInterval(newValue: NSInteger); message 'setFrameInterval:';
    function frameInterval: NSInteger; message 'frameInterval';
    procedure presentScene (scene: SKScene); message 'presentScene:';
    procedure presentScene_transition (scene: SKScene; transition: SKTransition); message 'presentScene:transition:';
    function scene: SKScene; message 'scene';
    function textureFromNode (node: SKNode): SKTexture; message 'textureFromNode:';
    function textureFromNode_crop (node: SKNode; crop: CGRect): SKTexture; message 'textureFromNode:crop:';
    function convertPoint_toScene (point: CGPoint; scene_: SKScene): CGPoint; message 'convertPoint:toScene:';
    function convertPoint_fromScene (point: CGPoint; scene_: SKScene): CGPoint; message 'convertPoint:fromScene:';
  end;
{$endif}

