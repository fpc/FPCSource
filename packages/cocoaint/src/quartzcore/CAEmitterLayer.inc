{ Parsed from QuartzCore.framework CAEmitterLayer.h }


{$ifdef TYPES}
type
  CAEmitterLayerPtr = ^CAEmitterLayer;
{$endif}

{$ifdef CLASSES}

type
  CAEmitterLayer = objcclass external (CALayer)
  public
    procedure setEmitterCells(newValue: NSArray); message 'setEmitterCells:';
    function emitterCells: NSArray; message 'emitterCells';
    procedure setBirthRate(newValue: single); message 'setBirthRate:';
    function birthRate: single; message 'birthRate';
    procedure setLifetime(newValue: single); message 'setLifetime:';
    function lifetime: single; message 'lifetime';
    procedure setEmitterPosition(newValue: CGPoint); message 'setEmitterPosition:';
    function emitterPosition: CGPoint; message 'emitterPosition';
    procedure setEmitterZPosition(newValue: CGFloat); message 'setEmitterZPosition:';
    function emitterZPosition: CGFloat; message 'emitterZPosition';
    procedure setEmitterSize(newValue: CGSize); message 'setEmitterSize:';
    function emitterSize: CGSize; message 'emitterSize';
    procedure setEmitterDepth(newValue: CGFloat); message 'setEmitterDepth:';
    function emitterDepth: CGFloat; message 'emitterDepth';
    procedure setEmitterShape(newValue: NSString); message 'setEmitterShape:';
    function emitterShape: NSString; message 'emitterShape';
    procedure setEmitterMode(newValue: NSString); message 'setEmitterMode:';
    function emitterMode: NSString; message 'emitterMode';
    procedure setRenderMode(newValue: NSString); message 'setRenderMode:';
    function renderMode: NSString; message 'renderMode';
    procedure setPreservesDepth(newValue: ObjCBOOL); message 'setPreservesDepth:';
    function preservesDepth: ObjCBOOL; message 'preservesDepth';
    procedure setVelocity(newValue: single); message 'setVelocity:';
    function velocity: single; message 'velocity';
    procedure setScale(newValue: single); message 'setScale:';
    function scale: single; message 'scale';
    procedure setSpin(newValue: single); message 'setSpin:';
    function spin: single; message 'spin';
    procedure setSeed(newValue: cuint); message 'setSeed:';
    function seed: cuint; message 'seed';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAEmitterLayerPoint: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerLine: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerRectangle: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerCuboid: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerCircle: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerSphere: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerPoints: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerOutline: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerSurface: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerVolume: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerUnordered: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerOldestFirst: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerOldestLast: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerBackToFront: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
  kCAEmitterLayerAdditive: NSString { available starting in __MAC_10_6, __IPHONE_5_0 }; cvar; external;
{$endif}

