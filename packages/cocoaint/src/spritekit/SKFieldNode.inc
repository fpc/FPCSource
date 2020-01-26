{ Parsed from SpriteKit.framework SKFieldNode.h }


{ Types from SKFieldNode }
{$ifdef TYPES}

type
  SKFieldForceEvaluator = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  SKFieldNodePtr = ^SKFieldNode;
{$endif}

{$ifdef CLASSES}

type
  SKFieldNode = objcclass external (SKNode)
  public
    procedure setRegion(newValue: SKRegion); message 'setRegion:';
    function region: SKRegion; message 'region';
    procedure setStrength(newValue: single); message 'setStrength:';
    function strength: single; message 'strength';
    procedure setFalloff(newValue: single); message 'setFalloff:';
    function falloff: single; message 'falloff';
    procedure setMinimumRadius(newValue: single); message 'setMinimumRadius:';
    function minimumRadius: single; message 'minimumRadius';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setExclusive(newValue: ObjCBOOL); message 'setExclusive:';
    function isExclusive: ObjCBOOL; message 'isExclusive';
    procedure setCategoryBitMask(newValue: cuint32); message 'setCategoryBitMask:';
    function categoryBitMask: cuint32; message 'categoryBitMask';
    procedure setDirection(newValue: vector_float3); message 'setDirection:';
    function direction: vector_float3; message 'direction';
    procedure setSmoothness(newValue: single); message 'setSmoothness:';
    function smoothness: single; message 'smoothness';
    procedure setAnimationSpeed(newValue: single); message 'setAnimationSpeed:';
    function animationSpeed: single; message 'animationSpeed';
    procedure setTexture(newValue: SKTexture); message 'setTexture:';
    function texture: SKTexture; message 'texture';
    class function dragField: SKFieldNode; message 'dragField';
    class function vortexField: SKFieldNode; message 'vortexField';
    class function radialGravityField: SKFieldNode; message 'radialGravityField';
    class function linearGravityFieldWithVector (direction_: vector_float3): SKFieldNode; message 'linearGravityFieldWithVector:';
    class function velocityFieldWithVector (direction_: vector_float3): SKFieldNode; message 'velocityFieldWithVector:';
    class function velocityFieldWithTexture (velocityTexture: SKTexture): SKFieldNode; message 'velocityFieldWithTexture:';
    class function noiseFieldWithSmoothness_animationSpeed (smoothness_: CGFloat; speed_: CGFloat): SKFieldNode; message 'noiseFieldWithSmoothness:animationSpeed:';
    class function turbulenceFieldWithSmoothness_animationSpeed (smoothness_: CGFloat; speed_: CGFloat): SKFieldNode; message 'turbulenceFieldWithSmoothness:animationSpeed:';
    class function springField: SKFieldNode; message 'springField';
    class function electricField: SKFieldNode; message 'electricField';
    class function magneticField: SKFieldNode; message 'magneticField';
    class function customFieldWithEvaluationBlock (block: SKFieldForceEvaluator): SKFieldNode; message 'customFieldWithEvaluationBlock:';
  end;
{$endif}

