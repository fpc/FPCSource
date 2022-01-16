{ Parsed from SpriteKit.framework SK3DNode.h }


{$ifdef TYPES}
type
  SK3DNodePtr = ^SK3DNode;
{$endif}

{$ifdef CLASSES}

type
  SK3DNode = objcclass external (SKNode)
  public
    function initWithViewportSize (viewportSize: CGSize): instancetype; message 'initWithViewportSize:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    class function nodeWithViewportSize (viewportSize: CGSize): instancetype; message 'nodeWithViewportSize:';
    procedure setViewportSize(newValue: CGSize); message 'setViewportSize:';
    function viewportSize: CGSize; message 'viewportSize';
    procedure setScnScene(newValue: SCNScene); message 'setScnScene:';
    function scnScene: SCNScene; message 'scnScene';
    procedure setSceneTime(newValue: NSTimeInterval); message 'setSceneTime:';
    function sceneTime: NSTimeInterval; message 'sceneTime';
    function hitTest_options (thePoint: CGPoint; options: NSDictionary): NSArray; message 'hitTest:options:';
    function projectPoint (point: vector_float3): vector_float3; message 'projectPoint:';
    function unprojectPoint (point: vector_float3): vector_float3; message 'unprojectPoint:';
    procedure setPlaying(newValue: ObjCBOOL); message 'setPlaying:';
    function isPlaying: ObjCBOOL; message 'isPlaying';
    procedure setLoops(newValue: ObjCBOOL); message 'setLoops:';
    function loops: ObjCBOOL; message 'loops';
    procedure setPointOfView(newValue: SCNNode); message 'setPointOfView:';
    function pointOfView: SCNNode; message 'pointOfView';
    procedure setAutoenablesDefaultLighting(newValue: ObjCBOOL); message 'setAutoenablesDefaultLighting:';
    function autoenablesDefaultLighting: ObjCBOOL; message 'autoenablesDefaultLighting';
  end;
{$endif}

