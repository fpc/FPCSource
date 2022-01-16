{ Parsed from SpriteKit.framework SKEmitterNode.h }


{$ifdef TYPES}
type
  SKEmitterNodePtr = ^SKEmitterNode;
{$endif}

{$ifdef CLASSES}

type
  SKEmitterNode = objcclass external (SKNode)
  public
    procedure advanceSimulationTime (sec: NSTimeInterval); message 'advanceSimulationTime:';
    procedure resetSimulation; message 'resetSimulation';
    procedure setParticleTexture(newValue: SKTexture); message 'setParticleTexture:';
    function particleTexture: SKTexture; message 'particleTexture';
    procedure setParticleZPosition(newValue: CGFloat); message 'setParticleZPosition:';
    function particleZPosition: CGFloat; message 'particleZPosition';
    procedure setParticleZPositionRange(newValue: CGFloat); message 'setParticleZPositionRange:';
    function particleZPositionRange: CGFloat; message 'particleZPositionRange';
    procedure setParticleZPositionSpeed(newValue: CGFloat); message 'setParticleZPositionSpeed:';
    function particleZPositionSpeed: CGFloat; message 'particleZPositionSpeed';
    procedure setParticleBlendMode(newValue: SKBlendMode); message 'setParticleBlendMode:';
    function particleBlendMode: SKBlendMode; message 'particleBlendMode';
    procedure setParticleColor(newValue: SKColorPtr); message 'setParticleColor:';
    function particleColor: SKColorPtr; message 'particleColor';
    procedure setParticleColorRedRange(newValue: CGFloat); message 'setParticleColorRedRange:';
    function particleColorRedRange: CGFloat; message 'particleColorRedRange';
    procedure setParticleColorGreenRange(newValue: CGFloat); message 'setParticleColorGreenRange:';
    function particleColorGreenRange: CGFloat; message 'particleColorGreenRange';
    procedure setParticleColorBlueRange(newValue: CGFloat); message 'setParticleColorBlueRange:';
    function particleColorBlueRange: CGFloat; message 'particleColorBlueRange';
    procedure setParticleColorAlphaRange(newValue: CGFloat); message 'setParticleColorAlphaRange:';
    function particleColorAlphaRange: CGFloat; message 'particleColorAlphaRange';
    procedure setParticleColorRedSpeed(newValue: CGFloat); message 'setParticleColorRedSpeed:';
    function particleColorRedSpeed: CGFloat; message 'particleColorRedSpeed';
    procedure setParticleColorGreenSpeed(newValue: CGFloat); message 'setParticleColorGreenSpeed:';
    function particleColorGreenSpeed: CGFloat; message 'particleColorGreenSpeed';
    procedure setParticleColorBlueSpeed(newValue: CGFloat); message 'setParticleColorBlueSpeed:';
    function particleColorBlueSpeed: CGFloat; message 'particleColorBlueSpeed';
    procedure setParticleColorAlphaSpeed(newValue: CGFloat); message 'setParticleColorAlphaSpeed:';
    function particleColorAlphaSpeed: CGFloat; message 'particleColorAlphaSpeed';
    procedure setParticleColorSequence(newValue: SKKeyframeSequence); message 'setParticleColorSequence:';
    function particleColorSequence: SKKeyframeSequence; message 'particleColorSequence';
    procedure setParticleColorBlendFactor(newValue: CGFloat); message 'setParticleColorBlendFactor:';
    function particleColorBlendFactor: CGFloat; message 'particleColorBlendFactor';
    procedure setParticleColorBlendFactorRange(newValue: CGFloat); message 'setParticleColorBlendFactorRange:';
    function particleColorBlendFactorRange: CGFloat; message 'particleColorBlendFactorRange';
    procedure setParticleColorBlendFactorSpeed(newValue: CGFloat); message 'setParticleColorBlendFactorSpeed:';
    function particleColorBlendFactorSpeed: CGFloat; message 'particleColorBlendFactorSpeed';
    procedure setParticleColorBlendFactorSequence(newValue: SKKeyframeSequence); message 'setParticleColorBlendFactorSequence:';
    function particleColorBlendFactorSequence: SKKeyframeSequence; message 'particleColorBlendFactorSequence';
    procedure setParticlePosition(newValue: CGPoint); message 'setParticlePosition:';
    function particlePosition: CGPoint; message 'particlePosition';
    procedure setParticlePositionRange(newValue: CGVector); message 'setParticlePositionRange:';
    function particlePositionRange: CGVector; message 'particlePositionRange';
    procedure setParticleSpeed(newValue: CGFloat); message 'setParticleSpeed:';
    function particleSpeed: CGFloat; message 'particleSpeed';
    procedure setParticleSpeedRange(newValue: CGFloat); message 'setParticleSpeedRange:';
    function particleSpeedRange: CGFloat; message 'particleSpeedRange';
    procedure setEmissionAngle(newValue: CGFloat); message 'setEmissionAngle:';
    function emissionAngle: CGFloat; message 'emissionAngle';
    procedure setEmissionAngleRange(newValue: CGFloat); message 'setEmissionAngleRange:';
    function emissionAngleRange: CGFloat; message 'emissionAngleRange';
    procedure setXAcceleration(newValue: CGFloat); message 'setXAcceleration:';
    function xAcceleration: CGFloat; message 'xAcceleration';
    procedure setYAcceleration(newValue: CGFloat); message 'setYAcceleration:';
    function yAcceleration: CGFloat; message 'yAcceleration';
    procedure setParticleBirthRate(newValue: CGFloat); message 'setParticleBirthRate:';
    function particleBirthRate: CGFloat; message 'particleBirthRate';
    procedure setNumParticlesToEmit(newValue: NSUInteger); message 'setNumParticlesToEmit:';
    function numParticlesToEmit: NSUInteger; message 'numParticlesToEmit';
    procedure setParticleLifetime(newValue: CGFloat); message 'setParticleLifetime:';
    function particleLifetime: CGFloat; message 'particleLifetime';
    procedure setParticleLifetimeRange(newValue: CGFloat); message 'setParticleLifetimeRange:';
    function particleLifetimeRange: CGFloat; message 'particleLifetimeRange';
    procedure setParticleRotation(newValue: CGFloat); message 'setParticleRotation:';
    function particleRotation: CGFloat; message 'particleRotation';
    procedure setParticleRotationRange(newValue: CGFloat); message 'setParticleRotationRange:';
    function particleRotationRange: CGFloat; message 'particleRotationRange';
    procedure setParticleRotationSpeed(newValue: CGFloat); message 'setParticleRotationSpeed:';
    function particleRotationSpeed: CGFloat; message 'particleRotationSpeed';
    procedure setParticleSize(newValue: CGSize); message 'setParticleSize:';
    function particleSize: CGSize; message 'particleSize';
    procedure setParticleScale(newValue: CGFloat); message 'setParticleScale:';
    function particleScale: CGFloat; message 'particleScale';
    procedure setParticleScaleRange(newValue: CGFloat); message 'setParticleScaleRange:';
    function particleScaleRange: CGFloat; message 'particleScaleRange';
    procedure setParticleScaleSpeed(newValue: CGFloat); message 'setParticleScaleSpeed:';
    function particleScaleSpeed: CGFloat; message 'particleScaleSpeed';
    procedure setParticleScaleSequence(newValue: SKKeyframeSequence); message 'setParticleScaleSequence:';
    function particleScaleSequence: SKKeyframeSequence; message 'particleScaleSequence';
    procedure setParticleAlpha(newValue: CGFloat); message 'setParticleAlpha:';
    function particleAlpha: CGFloat; message 'particleAlpha';
    procedure setParticleAlphaRange(newValue: CGFloat); message 'setParticleAlphaRange:';
    function particleAlphaRange: CGFloat; message 'particleAlphaRange';
    procedure setParticleAlphaSpeed(newValue: CGFloat); message 'setParticleAlphaSpeed:';
    function particleAlphaSpeed: CGFloat; message 'particleAlphaSpeed';
    procedure setParticleAlphaSequence(newValue: SKKeyframeSequence); message 'setParticleAlphaSequence:';
    function particleAlphaSequence: SKKeyframeSequence; message 'particleAlphaSequence';
    procedure setParticleAction(newValue: SKAction); message 'setParticleAction:';
    function particleAction: SKAction; message 'particleAction';
    procedure setFieldBitMask(newValue: cuint32); message 'setFieldBitMask:';
    function fieldBitMask: cuint32; message 'fieldBitMask';
    procedure setTargetNode(newValue: SKNode); message 'setTargetNode:';
    function targetNode: SKNode; message 'targetNode';
    procedure setShader(newValue: SKShader); message 'setShader:';
    function shader: SKShader; message 'shader';
  end;
{$endif}

