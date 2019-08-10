{ Parsed from SceneKit.framework SCNLayer.h }


{$ifdef TYPES}
type
  SCNLayerPtr = ^SCNLayer;
{$endif}

{$ifdef CLASSES}

type
  SCNLayer = objcclass external (CAOpenGLLayer, SCNSceneRendererProtocol, SCNTechniqueSupportProtocol)
  private
    _reserved: id;
  public
    procedure setScene(newValue: SCNScene); message 'setScene:';
    function scene: SCNScene; message 'scene';

    { Adopted protocols }
    procedure setDelegate(newValue: SCNSceneRendererDelegateProtocol); message 'setDelegate:';
    function delegate: SCNSceneRendererDelegateProtocol; message 'delegate';
    procedure setAutoenablesDefaultLighting(newValue: ObjCBOOL); message 'setAutoenablesDefaultLighting:';
    function autoenablesDefaultLighting: ObjCBOOL; message 'autoenablesDefaultLighting';
    function context: pointer; message 'context';
    procedure setCurrentTime(newValue: NSTimeInterval); message 'setCurrentTime:';
    function currentTime: NSTimeInterval; message 'currentTime';
    function hitTest_options (thePoint: CGPoint; options: NSDictionary): NSArray; message 'hitTest:options:';
    function isNodeInsideFrustum_withPointOfView (node: SCNNode; pointOfView: SCNNode): ObjCBOOL; message 'isNodeInsideFrustum:withPointOfView:'; { available in 10_9, 8_0 }
    procedure setJitteringEnabled(newValue: ObjCBOOL); message 'setJitteringEnabled:';
    function isJitteringEnabled: ObjCBOOL; message 'isJitteringEnabled';
    procedure setLoops(newValue: ObjCBOOL); message 'setLoops:';
    function loops: ObjCBOOL; message 'loops';
    procedure setOverlaySKScene(newValue: SKScene); message 'setOverlaySKScene:';
    function overlaySKScene: SKScene; message 'overlaySKScene';
    procedure setPlaying(newValue: ObjCBOOL); message 'setPlaying:';
    function isPlaying: ObjCBOOL; message 'isPlaying';
    procedure setPointOfView(newValue: SCNNode); message 'setPointOfView:';
    function pointOfView: SCNNode; message 'pointOfView';
    function prepareObject_shouldAbortBlock (object_: id; block: OpaqueCBlock): ObjCBOOL; message 'prepareObject:shouldAbortBlock:'; { available in 10_9, 8_0 }
    procedure prepareObjects_withCompletionHandler (objects: NSArray; completionHandler: OpaqueCBlock); message 'prepareObjects:withCompletionHandler:'; { available in 10_10, 8_0 }
    function projectPoint (point: SCNVector3): SCNVector3; message 'projectPoint:'; { available in 10_9, 8_0 }
    procedure setSceneTime(newValue: NSTimeInterval); message 'setSceneTime:';
    function sceneTime: NSTimeInterval; message 'sceneTime';
    procedure setShowsStatistics(newValue: ObjCBOOL); message 'setShowsStatistics:';
    function showsStatistics: ObjCBOOL; message 'showsStatistics';
    procedure setTechnique(newValue: SCNTechnique); message 'setTechnique:';
    function technique: SCNTechnique; message 'technique';
    function unprojectPoint (point: SCNVector3): SCNVector3; message 'unprojectPoint:'; { available in 10_9, 8_0 }
  end;
{$endif}

