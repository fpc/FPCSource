{ Parsed from SceneKit.framework SCNMaterial.h }


{$ifdef TYPES}
type
  SCNMaterialPtr = ^SCNMaterial;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNLightingModelPhong: NSString; cvar; external;
  SCNLightingModelBlinn: NSString; cvar; external;
  SCNLightingModelLambert: NSString; cvar; external;
  SCNLightingModelConstant: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  SCNCullMode = NSInteger;
  SCNCullModePtr = ^SCNCullMode;

const
  SCNCullBack = 0;
  SCNCullFront = 1;

type
  SCNTransparencyMode = NSInteger;
  SCNTransparencyModePtr = ^SCNTransparencyMode;

const
  SCNTransparencyModeAOne = 0;
  SCNTransparencyModeRGBZero = 1;
{$endif}

{$ifdef CLASSES}

type
  SCNMaterial = objcclass external (NSObject, SCNAnimatableProtocol, SCNShadableProtocol, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function material: instancetype; message 'material';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    function diffuse: SCNMaterialProperty; message 'diffuse';
    function ambient: SCNMaterialProperty; message 'ambient';
    function specular: SCNMaterialProperty; message 'specular';
    function emission: SCNMaterialProperty; message 'emission';
    function transparent: SCNMaterialProperty; message 'transparent';
    function reflective: SCNMaterialProperty; message 'reflective';
    function multiply: SCNMaterialProperty; message 'multiply';
    function normal: SCNMaterialProperty; message 'normal';
    procedure setShininess(newValue: CGFloat); message 'setShininess:';
    function shininess: CGFloat; message 'shininess';
    procedure setTransparency(newValue: CGFloat); message 'setTransparency:';
    function transparency: CGFloat; message 'transparency';
    procedure setLightingModelName(newValue: NSString); message 'setLightingModelName:';
    function lightingModelName: NSString; message 'lightingModelName';
    procedure setLitPerPixel(newValue: ObjCBOOL); message 'setLitPerPixel:';
    function isLitPerPixel: ObjCBOOL; message 'isLitPerPixel';
    procedure setDoubleSided(newValue: ObjCBOOL); message 'setDoubleSided:';
    function isDoubleSided: ObjCBOOL; message 'isDoubleSided';
    procedure setCullMode(newValue: SCNCullMode); message 'setCullMode:';
    function cullMode: SCNCullMode; message 'cullMode';
    procedure setTransparencyMode(newValue: SCNTransparencyMode); message 'setTransparencyMode:';
    function transparencyMode: SCNTransparencyMode; message 'transparencyMode';
    procedure setLocksAmbientWithDiffuse(newValue: ObjCBOOL); message 'setLocksAmbientWithDiffuse:';
    function locksAmbientWithDiffuse: ObjCBOOL; message 'locksAmbientWithDiffuse';
    procedure setWritesToDepthBuffer(newValue: ObjCBOOL); message 'setWritesToDepthBuffer:';
    function writesToDepthBuffer: ObjCBOOL; message 'writesToDepthBuffer';
    procedure setReadsFromDepthBuffer(newValue: ObjCBOOL); message 'setReadsFromDepthBuffer:';
    function readsFromDepthBuffer: ObjCBOOL; message 'readsFromDepthBuffer';
    procedure setFresnelExponent(newValue: CGFloat); message 'setFresnelExponent:';
    function fresnelExponent: CGFloat; message 'fresnelExponent';

    { Adopted protocols }
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    procedure handleBindingOfSymbol_usingBlock (symbol: NSString; block: SCNBindingBlock); message 'handleBindingOfSymbol:usingBlock:'; { available in 10_9, 8_0 }
    procedure handleUnbindingOfSymbol_usingBlock (symbol: NSString; block: SCNBindingBlock); message 'handleUnbindingOfSymbol:usingBlock:'; { available in 10_9, 8_0 }
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure setProgram(newValue: SCNProgram); message 'setProgram:';
    function program_: SCNProgram; message 'program';
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    procedure setShaderModifiers(newValue: NSDictionary); message 'setShaderModifiers:';
    function shaderModifiers: NSDictionary; message 'shaderModifiers';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

