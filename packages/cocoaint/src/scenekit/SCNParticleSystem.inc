{ Parsed from SceneKit.framework SCNParticleSystem.h }


{$ifdef TYPES}
type
  SCNParticlePropertyControllerPtr = ^SCNParticlePropertyController;
  SCNParticleSystemPtr = ^SCNParticleSystem;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNParticlePropertyPosition: NSString; cvar; external;
  SCNParticlePropertyAngle: NSString; cvar; external;
  SCNParticlePropertyRotationAxis: NSString; cvar; external;
  SCNParticlePropertyVelocity: NSString; cvar; external;
  SCNParticlePropertyAngularVelocity: NSString; cvar; external;
  SCNParticlePropertyLife: NSString; cvar; external;
  SCNParticlePropertyColor: NSString; cvar; external;
  SCNParticlePropertyOpacity: NSString; cvar; external;
  SCNParticlePropertySize: NSString; cvar; external;
  SCNParticlePropertyFrame: NSString; cvar; external;
  SCNParticlePropertyFrameRate: NSString; cvar; external;
  SCNParticlePropertyBounce: NSString; cvar; external;
  SCNParticlePropertyCharge: NSString; cvar; external;
  SCNParticlePropertyFriction: NSString; cvar; external;
  SCNParticlePropertyContactPoint: NSString; cvar; external;
  SCNParticlePropertyContactNormal: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  SCNParticleEventBlock = OpaqueCBlock;
  SCNParticleModifierBlock = OpaqueCBlock;
  SCNParticleSortingMode = NSInteger;
  SCNParticleSortingModePtr = ^SCNParticleSortingMode;

const
  SCNParticleSortingModeNone = 0;
  SCNParticleSortingModeProjectedDepth = 1;
  SCNParticleSortingModeDistance = 2;
  SCNParticleSortingModeOldestFirst = 3;
  SCNParticleSortingModeYoungestFirst = 4;

type
  SCNParticleBlendMode = NSInteger;
  SCNParticleBlendModePtr = ^SCNParticleBlendMode;

const
  SCNParticleBlendModeAdditive = 0;
  SCNParticleBlendModeSubtract = 1;
  SCNParticleBlendModeMultiply = 2;
  SCNParticleBlendModeScreen = 3;
  SCNParticleBlendModeAlpha = 4;
  SCNParticleBlendModeReplace = 5;

type
  SCNParticleOrientationMode = NSInteger;
  SCNParticleOrientationModePtr = ^SCNParticleOrientationMode;

const
  SCNParticleOrientationModeBillboardScreenAligned = 0;
  SCNParticleOrientationModeBillboardViewAligned = 1;
  SCNParticleOrientationModeFree = 2;
  SCNParticleOrientationModeBillboardYAligned = 3;

type
  SCNParticleBirthLocation = NSInteger;
  SCNParticleBirthLocationPtr = ^SCNParticleBirthLocation;

const
  SCNParticleBirthLocationSurface = 0;
  SCNParticleBirthLocationVolume = 1;
  SCNParticleBirthLocationVertex = 2;

type
  SCNParticleBirthDirection = NSInteger;
  SCNParticleBirthDirectionPtr = ^SCNParticleBirthDirection;

const
  SCNParticleBirthDirectionConstant = 0;
  SCNParticleBirthDirectionSurfaceNormal = 1;
  SCNParticleBirthDirectionRandom = 2;

type
  SCNParticleImageSequenceAnimationMode = NSInteger;
  SCNParticleImageSequenceAnimationModePtr = ^SCNParticleImageSequenceAnimationMode;

const
  SCNParticleImageSequenceAnimationModeRepeat = 0;
  SCNParticleImageSequenceAnimationModeClamp = 1;
  SCNParticleImageSequenceAnimationModeAutoReverse = 2;

type
  SCNParticleInputMode = NSInteger;
  SCNParticleInputModePtr = ^SCNParticleInputMode;

const
  SCNParticleInputModeOverLife = 0;
  SCNParticleInputModeOverDistance = 1;
  SCNParticleInputModeOverOtherProperty = 2;

type
  SCNParticleModifierStage = NSInteger;
  SCNParticleModifierStagePtr = ^SCNParticleModifierStage;

const
  SCNParticleModifierStagePreDynamics = 0;
  SCNParticleModifierStagePostDynamics = 1;
  SCNParticleModifierStagePreCollision = 2;
  SCNParticleModifierStagePostCollision = 3;

type
  SCNParticleEvent = NSInteger;
  SCNParticleEventPtr = ^SCNParticleEvent;

const
  SCNParticleEventBirth = 0;
  SCNParticleEventDeath = 1;
  SCNParticleEventCollision = 2;
{$endif}

{$ifdef CLASSES}

type
  SCNParticlePropertyController = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _reserved: id;
  public
    class function controllerWithAnimation (animation: CAAnimation): instancetype; message 'controllerWithAnimation:';
    procedure setAnimation(newValue: CAAnimation); message 'setAnimation:';
    function animation: CAAnimation; message 'animation';
    procedure setInputMode(newValue: SCNParticleInputMode); message 'setInputMode:';
    function inputMode: SCNParticleInputMode; message 'inputMode';
    procedure setInputScale(newValue: CGFloat); message 'setInputScale:';
    function inputScale: CGFloat; message 'inputScale';
    procedure setInputBias(newValue: CGFloat); message 'setInputBias:';
    function inputBias: CGFloat; message 'inputBias';
    procedure setInputOrigin(newValue: SCNNode); message 'setInputOrigin:';
    function inputOrigin: SCNNode; message 'inputOrigin';
    procedure setInputProperty(newValue: NSString); message 'setInputProperty:';
    function inputProperty: NSString; message 'inputProperty';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  SCNParticleSystem = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol, SCNAnimatableProtocol)
  private
    _reserved: id;
  public
    class function particleSystem: instancetype; message 'particleSystem';
    class function particleSystemNamed_inDirectory (name: NSString; directory: NSString): instancetype; message 'particleSystemNamed:inDirectory:';
    procedure setEmissionDuration(newValue: CGFloat); message 'setEmissionDuration:';
    function emissionDuration: CGFloat; message 'emissionDuration';
    procedure setEmissionDurationVariation(newValue: CGFloat); message 'setEmissionDurationVariation:';
    function emissionDurationVariation: CGFloat; message 'emissionDurationVariation';
    procedure setIdleDuration(newValue: CGFloat); message 'setIdleDuration:';
    function idleDuration: CGFloat; message 'idleDuration';
    procedure setIdleDurationVariation(newValue: CGFloat); message 'setIdleDurationVariation:';
    function idleDurationVariation: CGFloat; message 'idleDurationVariation';
    procedure setLoops(newValue: ObjCBOOL); message 'setLoops:';
    function loops: ObjCBOOL; message 'loops';
    procedure setBirthRate(newValue: CGFloat); message 'setBirthRate:';
    function birthRate: CGFloat; message 'birthRate';
    procedure setBirthRateVariation(newValue: CGFloat); message 'setBirthRateVariation:';
    function birthRateVariation: CGFloat; message 'birthRateVariation';
    procedure setWarmupDuration(newValue: CGFloat); message 'setWarmupDuration:';
    function warmupDuration: CGFloat; message 'warmupDuration';
    procedure setEmitterShape(newValue: SCNGeometry); message 'setEmitterShape:';
    function emitterShape: SCNGeometry; message 'emitterShape';
    procedure setBirthLocation(newValue: SCNParticleBirthLocation); message 'setBirthLocation:';
    function birthLocation: SCNParticleBirthLocation; message 'birthLocation';
    procedure setBirthDirection(newValue: SCNParticleBirthDirection); message 'setBirthDirection:';
    function birthDirection: SCNParticleBirthDirection; message 'birthDirection';
    procedure setSpreadingAngle(newValue: CGFloat); message 'setSpreadingAngle:';
    function spreadingAngle: CGFloat; message 'spreadingAngle';
    procedure setEmittingDirection(newValue: SCNVector3); message 'setEmittingDirection:';
    function emittingDirection: SCNVector3; message 'emittingDirection';
    procedure setAcceleration(newValue: SCNVector3); message 'setAcceleration:';
    function acceleration: SCNVector3; message 'acceleration';
    procedure setLocal(newValue: ObjCBOOL); message 'setLocal:';
    function isLocal: ObjCBOOL; message 'isLocal';
    procedure setParticleAngle(newValue: CGFloat); message 'setParticleAngle:';
    function particleAngle: CGFloat; message 'particleAngle';
    procedure setParticleAngleVariation(newValue: CGFloat); message 'setParticleAngleVariation:';
    function particleAngleVariation: CGFloat; message 'particleAngleVariation';
    procedure setParticleVelocity(newValue: CGFloat); message 'setParticleVelocity:';
    function particleVelocity: CGFloat; message 'particleVelocity';
    procedure setParticleVelocityVariation(newValue: CGFloat); message 'setParticleVelocityVariation:';
    function particleVelocityVariation: CGFloat; message 'particleVelocityVariation';
    procedure setParticleAngularVelocity(newValue: CGFloat); message 'setParticleAngularVelocity:';
    function particleAngularVelocity: CGFloat; message 'particleAngularVelocity';
    procedure setParticleAngularVelocityVariation(newValue: CGFloat); message 'setParticleAngularVelocityVariation:';
    function particleAngularVelocityVariation: CGFloat; message 'particleAngularVelocityVariation';
    procedure setParticleLifeSpan(newValue: CGFloat); message 'setParticleLifeSpan:';
    function particleLifeSpan: CGFloat; message 'particleLifeSpan';
    procedure setParticleLifeSpanVariation(newValue: CGFloat); message 'setParticleLifeSpanVariation:';
    function particleLifeSpanVariation: CGFloat; message 'particleLifeSpanVariation';
    procedure setSystemSpawnedOnDying(newValue: SCNParticleSystem); message 'setSystemSpawnedOnDying:';
    function systemSpawnedOnDying: SCNParticleSystem; message 'systemSpawnedOnDying';
    procedure setSystemSpawnedOnCollision(newValue: SCNParticleSystem); message 'setSystemSpawnedOnCollision:';
    function systemSpawnedOnCollision: SCNParticleSystem; message 'systemSpawnedOnCollision';
    procedure setSystemSpawnedOnLiving(newValue: SCNParticleSystem); message 'setSystemSpawnedOnLiving:';
    function systemSpawnedOnLiving: SCNParticleSystem; message 'systemSpawnedOnLiving';
    procedure setParticleImage(newValue: id); message 'setParticleImage:';
    function particleImage: id; message 'particleImage';
    procedure setImageSequenceColumnCount(newValue: NSUInteger); message 'setImageSequenceColumnCount:';
    function imageSequenceColumnCount: NSUInteger; message 'imageSequenceColumnCount';
    procedure setImageSequenceRowCount(newValue: NSUInteger); message 'setImageSequenceRowCount:';
    function imageSequenceRowCount: NSUInteger; message 'imageSequenceRowCount';
    procedure setImageSequenceInitialFrame(newValue: CGFloat); message 'setImageSequenceInitialFrame:';
    function imageSequenceInitialFrame: CGFloat; message 'imageSequenceInitialFrame';
    procedure setImageSequenceInitialFrameVariation(newValue: CGFloat); message 'setImageSequenceInitialFrameVariation:';
    function imageSequenceInitialFrameVariation: CGFloat; message 'imageSequenceInitialFrameVariation';
    procedure setImageSequenceFrameRate(newValue: CGFloat); message 'setImageSequenceFrameRate:';
    function imageSequenceFrameRate: CGFloat; message 'imageSequenceFrameRate';
    procedure setImageSequenceFrameRateVariation(newValue: CGFloat); message 'setImageSequenceFrameRateVariation:';
    function imageSequenceFrameRateVariation: CGFloat; message 'imageSequenceFrameRateVariation';
    procedure setImageSequenceAnimationMode(newValue: SCNParticleImageSequenceAnimationMode); message 'setImageSequenceAnimationMode:';
    function imageSequenceAnimationMode: SCNParticleImageSequenceAnimationMode; message 'imageSequenceAnimationMode';
    procedure setParticleColor(newValue: NSColor); message 'setParticleColor:';
    function particleColor: NSColor; message 'particleColor';
    procedure setParticleColorVariation(newValue: SCNVector4); message 'setParticleColorVariation:';
    function particleColorVariation: SCNVector4; message 'particleColorVariation';
    procedure setParticleSize(newValue: CGFloat); message 'setParticleSize:';
    function particleSize: CGFloat; message 'particleSize';
    procedure setParticleSizeVariation(newValue: CGFloat); message 'setParticleSizeVariation:';
    function particleSizeVariation: CGFloat; message 'particleSizeVariation';
    procedure setBlendMode(newValue: SCNParticleBlendMode); message 'setBlendMode:';
    function blendMode: SCNParticleBlendMode; message 'blendMode';
    procedure setBlackPassEnabled(newValue: ObjCBOOL); message 'setBlackPassEnabled:';
    function isBlackPassEnabled: ObjCBOOL; message 'isBlackPassEnabled';
    procedure setOrientationMode(newValue: SCNParticleOrientationMode); message 'setOrientationMode:';
    function orientationMode: SCNParticleOrientationMode; message 'orientationMode';
    procedure setSortingMode(newValue: SCNParticleSortingMode); message 'setSortingMode:';
    function sortingMode: SCNParticleSortingMode; message 'sortingMode';
    procedure setLightingEnabled(newValue: ObjCBOOL); message 'setLightingEnabled:';
    function isLightingEnabled: ObjCBOOL; message 'isLightingEnabled';
    procedure setAffectedByGravity(newValue: ObjCBOOL); message 'setAffectedByGravity:';
    function affectedByGravity: ObjCBOOL; message 'affectedByGravity';
    procedure setAffectedByPhysicsFields(newValue: ObjCBOOL); message 'setAffectedByPhysicsFields:';
    function affectedByPhysicsFields: ObjCBOOL; message 'affectedByPhysicsFields';
    procedure setParticleDiesOnCollision(newValue: ObjCBOOL); message 'setParticleDiesOnCollision:';
    function particleDiesOnCollision: ObjCBOOL; message 'particleDiesOnCollision';
    procedure setColliderNodes(newValue: NSArray); message 'setColliderNodes:';
    function colliderNodes: NSArray; message 'colliderNodes';
    procedure setParticleMass(newValue: CGFloat); message 'setParticleMass:';
    function particleMass: CGFloat; message 'particleMass';
    procedure setParticleMassVariation(newValue: CGFloat); message 'setParticleMassVariation:';
    function particleMassVariation: CGFloat; message 'particleMassVariation';
    procedure setParticleBounce(newValue: CGFloat); message 'setParticleBounce:';
    function particleBounce: CGFloat; message 'particleBounce';
    procedure setParticleBounceVariation(newValue: CGFloat); message 'setParticleBounceVariation:';
    function particleBounceVariation: CGFloat; message 'particleBounceVariation';
    procedure setParticleFriction(newValue: CGFloat); message 'setParticleFriction:';
    function particleFriction: CGFloat; message 'particleFriction';
    procedure setParticleFrictionVariation(newValue: CGFloat); message 'setParticleFrictionVariation:';
    function particleFrictionVariation: CGFloat; message 'particleFrictionVariation';
    procedure setParticleCharge(newValue: CGFloat); message 'setParticleCharge:';
    function particleCharge: CGFloat; message 'particleCharge';
    procedure setParticleChargeVariation(newValue: CGFloat); message 'setParticleChargeVariation:';
    function particleChargeVariation: CGFloat; message 'particleChargeVariation';
    procedure setDampingFactor(newValue: CGFloat); message 'setDampingFactor:';
    function dampingFactor: CGFloat; message 'dampingFactor';
    procedure setSpeedFactor(newValue: CGFloat); message 'setSpeedFactor:';
    function speedFactor: CGFloat; message 'speedFactor';
    procedure setStretchFactor(newValue: CGFloat); message 'setStretchFactor:';
    function stretchFactor: CGFloat; message 'stretchFactor';
    procedure setFresnelExponent(newValue: CGFloat); message 'setFresnelExponent:';
    function fresnelExponent: CGFloat; message 'fresnelExponent';
    procedure setPropertyControllers(newValue: NSDictionary); message 'setPropertyControllers:';
    function propertyControllers: NSDictionary; message 'propertyControllers';
    procedure reset; message 'reset';
    procedure handleEvent_forProperties_withBlock (event: SCNParticleEvent; properties: NSArray; block: SCNParticleEventBlock); message 'handleEvent:forProperties:withBlock:';
    procedure addModifierForProperties_atStage_withBlock (properties: NSArray; stage: SCNParticleModifierStage; block: SCNParticleModifierBlock); message 'addModifierForProperties:atStage:withBlock:';
    procedure removeModifiersOfStage (stage: SCNParticleModifierStage); message 'removeModifiersOfStage:';
    procedure removeAllModifiers; message 'removeAllModifiers';

    { Adopted protocols }
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  SCNNode_SCNParticleSystemSupport = objccategory external name 'SCNParticleSystemSupport' (SCNNode)
    procedure addParticleSystem (system: SCNParticleSystem); message 'addParticleSystem:';
    procedure removeAllParticleSystems; message 'removeAllParticleSystems';
    procedure removeParticleSystem (system: SCNParticleSystem); message 'removeParticleSystem:';
    function particleSystems: NSArray; message 'particleSystems';
  end;

type
  SCNParticleSystemSupport = objccategory external (SCNScene)
    procedure addParticleSystem_withTransform (system: SCNParticleSystem; transform: SCNMatrix4); message 'addParticleSystem:withTransform:';
    procedure removeAllParticleSystems; message 'removeAllParticleSystems';
    procedure removeParticleSystem (system: SCNParticleSystem); message 'removeParticleSystem:';
    function particleSystems: NSArray; message 'particleSystems';
  end;
{$endif}

