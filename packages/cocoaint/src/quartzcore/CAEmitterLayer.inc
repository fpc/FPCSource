{ Parsed from Quartzcore.framework CAEmitterLayer.h }

{$ifdef TYPES}
{$ifndef CAEMITTERLAYER_PAS_T}
{$define CAEMITTERLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAEMITTERLAYER_PAS_R}
{$define CAEMITTERLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAEMITTERLAYER_PAS_F}
{$define CAEMITTERLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAEMITTERLAYER_PAS_S}
{$define CAEMITTERLAYER_PAS_S}

{ External string constants }
var
  kCAEmitterLayerPoint: NSString; cvar; external;
  kCAEmitterLayerLine: NSString; cvar; external;
  kCAEmitterLayerRectangle: NSString; cvar; external;
  kCAEmitterLayerCuboid: NSString; cvar; external;
  kCAEmitterLayerCircle: NSString; cvar; external;
  kCAEmitterLayerSphere: NSString; cvar; external;
  kCAEmitterLayerPoints: NSString; cvar; external;
  kCAEmitterLayerOutline: NSString; cvar; external;
  kCAEmitterLayerSurface: NSString; cvar; external;
  kCAEmitterLayerVolume: NSString; cvar; external;
  kCAEmitterLayerUnordered: NSString; cvar; external;
  kCAEmitterLayerOldestFirst: NSString; cvar; external;
  kCAEmitterLayerOldestLast: NSString; cvar; external;
  kCAEmitterLayerBackToFront: NSString; cvar; external;
  kCAEmitterLayerAdditive: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAEmitterLayer = objcclass;
  CAEmitterLayerPointer = ^CAEmitterLayer;
  CAEmitterLayerPtr = CAEmitterLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAEMITTERLAYER_PAS_C}
{$define CAEMITTERLAYER_PAS_C}

{ CAEmitterLayer }
  CAEmitterLayer = objcclass external (CALayer)
    
  public
    procedure setEmitterCells (newValue: NSArray); message 'setEmitterCells:';
    function emitterCells: NSArray; message 'emitterCells';
    procedure setBirthRate (newValue: single); message 'setBirthRate:';
    function birthRate: single; message 'birthRate';
    procedure setLifetime (newValue: single); message 'setLifetime:';
    function lifetime: single; message 'lifetime';
    procedure setEmitterPosition (newValue: CGPoint); message 'setEmitterPosition:';
    function emitterPosition: CGPoint; message 'emitterPosition';
    procedure setEmitterZPosition (newValue: CGFloat); message 'setEmitterZPosition:';
    function emitterZPosition: CGFloat; message 'emitterZPosition';
    procedure setEmitterSize (newValue: CGSize); message 'setEmitterSize:';
    function emitterSize: CGSize; message 'emitterSize';
    procedure setEmitterDepth (newValue: CGFloat); message 'setEmitterDepth:';
    function emitterDepth: CGFloat; message 'emitterDepth';
    procedure setEmitterShape (newValue: NSString); message 'setEmitterShape:';
    function emitterShape: NSString; message 'emitterShape';
    procedure setEmitterMode (newValue: NSString); message 'setEmitterMode:';
    function emitterMode: NSString; message 'emitterMode';
    procedure setRenderMode (newValue: NSString); message 'setRenderMode:';
    function renderMode: NSString; message 'renderMode';
    procedure setPreservesDepth (newValue: Boolean); message 'setPreservesDepth:';
    function preservesDepth: Boolean; message 'preservesDepth';
    procedure setVelocity (newValue: single); message 'setVelocity:';
    function velocity: single; message 'velocity';
    procedure setScale (newValue: single); message 'setScale:';
    function scale: single; message 'scale';
    procedure setSpin (newValue: single); message 'setSpin:';
    function spin: single; message 'spin';
    procedure setSeed (newValue: cuint); message 'setSeed:';
    function seed: cuint; message 'seed';
  end;

{$endif}
{$endif}
