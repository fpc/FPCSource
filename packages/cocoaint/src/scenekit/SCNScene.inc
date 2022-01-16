{ Parsed from SceneKit.framework SCNScene.h }


{$ifdef TYPES}
type
  SCNScenePtr = ^SCNScene;
  SCNSceneExportDelegateProtocolPtr = ^SCNSceneExportDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  SCNSceneExportProgressHandler = OpaqueCBlock;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNSceneExportDestinationURL: NSString { available in 10_9, 8_0 }; cvar; external;
  SCNSceneStartTimeAttributeKey: NSString; cvar; external;
  SCNSceneEndTimeAttributeKey: NSString; cvar; external;
  SCNSceneFrameRateAttributeKey: NSString; cvar; external;
  SCNSceneUpAxisAttributeKey: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  SCNScene = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function scene: instancetype; message 'scene';
    function rootNode: SCNNode; message 'rootNode';
    function physicsWorld: SCNPhysicsWorld; message 'physicsWorld';
    function attributeForKey (key: NSString): id; message 'attributeForKey:';
    procedure setAttribute_forKey (attribute: id; key: NSString); message 'setAttribute:forKey:';
    function background: SCNMaterialProperty; message 'background';
    class function sceneNamed (name: NSString): instancetype; message 'sceneNamed:'; { available in 10_9, 8_0 }
    class function sceneNamed_inDirectory_options (name: NSString; directory: NSString; options: NSDictionary): instancetype; message 'sceneNamed:inDirectory:options:'; { available in 10_10, 8_0 }
    class function sceneWithURL_options_error (url: NSURL; options: NSDictionary; error: NSErrorPtr): instancetype; message 'sceneWithURL:options:error:';
    function writeToURL_options_delegate_progressHandler (url: NSURL; options: NSDictionary; delegate: SCNSceneExportDelegateProtocol; progressHandler: SCNSceneExportProgressHandler): ObjCBOOL; message 'writeToURL:options:delegate:progressHandler:'; { available in 10_9, NA }
    procedure setFogStartDistance(newValue: CGFloat); message 'setFogStartDistance:';
    function fogStartDistance: CGFloat; message 'fogStartDistance';
    procedure setFogEndDistance(newValue: CGFloat); message 'setFogEndDistance:';
    function fogEndDistance: CGFloat; message 'fogEndDistance';
    procedure setFogDensityExponent(newValue: CGFloat); message 'setFogDensityExponent:';
    function fogDensityExponent: CGFloat; message 'fogDensityExponent';
    procedure setFogColor(newValue: id); message 'setFogColor:';
    function fogColor: id; message 'fogColor';
    procedure setPaused(newValue: ObjCBOOL); message 'setPaused:';
    function isPaused: ObjCBOOL; message 'isPaused';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  SCNSceneExportDelegateProtocol = objcprotocol external name 'SCNSceneExportDelegate' (NSObjectProtocol)
  optional
    function writeImage_withSceneDocumentURL_originalImageURL (image: NSImage; documentURL: NSURL; originalImageURL: NSURL): NSURL; message 'writeImage:withSceneDocumentURL:originalImageURL:'; { available in 10_9, NA }
  end;
{$endif}

